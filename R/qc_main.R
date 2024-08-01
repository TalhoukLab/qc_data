#' Quality Control Process
#'
#' @param data_file Name of the data file to be validated
#' @param rules_file_path Path to the rules Excel file
#' @param yml_path Directory path where YAML and HTML reports will be saved
#' @param log_file Path to the log file where logs will be stored
#'
#' @return agent Object containing the results and configuration of the validation process
#' @export
#'

qc_main <- function(registry_data, rules, yml_path = getwd(), log_file) {

  ################################
  # Load package
  library(pointblank)
  library(dplyr)
  library(stringr)
  library(log4r)
  library(tidyr)
  library(sqldf)

  my_logfile <- file.path(getwd(), "log", log_file)
  my_console_appender <- console_appender(layout = default_log_layout())
  my_file_appender <- file_appender(my_logfile, append = TRUE, layout = default_log_layout())
  my_logger <- logger(threshold = "INFO", appenders = list(my_console_appender, my_file_appender))

  # Log functions
  log4r_info_start <- function(now, data_file) {
    log4r::info(my_logger, paste("QC Registry data file:", data_file, "on", now))
  }

  log4r_info_complete <- function(now, yml) {
    log4r::info(my_logger, paste("Registry data QC is done. Results are saved to", yml, "at", now))
  }

  log4r_info_variables_verified <- function() {
    log4r::info(my_logger, paste("All variables checked by QC are included in the Registry data file."))
  }

  log4r_info_variables_not_found <- function(variables) {
    log4r::info(my_logger, paste("Variables:", variables, "are not included in the Registry data file."))
  }

  # Read in data file
  # registry_data <- read.csv(data, check.names = FALSE, stringsAsFactors = FALSE)

  # Read in rule list
  options(scipen = 999)
  # rules <- read.csv(paste0(rule_path, "rules_Q2024.csv"), check.names = FALSE, stringsAsFactors = FALSE) %>% mutate_if(is.logical, as.character)
  rules$pre_condition_filter <- ifelse(is.na(rules$pre_condition_filter), "", rules$pre_condition_filter)
  rules$precondition_filter_variable <- ifelse(is.na(rules$precondition_filter_variable), "", rules$precondition_filter_variable)

  check_date_format <- function(column) {
    # Check if the date is in "yyyy-mm-dd" format
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", column)) {
      return("%Y-%m-%d")
    }

    # Check if the date is in "dd/mm/yyyy hh:mm" format
    if (grepl("^\\d{2}/\\d{2}/\\d{4} \\d{1,2}:\\d{2}(:\\d{2})?$", column)) {
      return("%d/%m/%Y %H:%M")
    }

    # Check if the date is in "dd-mm-yyyy hh:mm" format
    if (grepl("^\\d{1}/\\d{2}/\\d{4} \\d{1,2}:\\d{2}(:\\d{2})?$", column)) {
      return("%d/%m/%Y %H:%M")
    }

    # Check if the date is in "y-m-d" format
    if (grepl("^\\d{1}/\\d{1}/\\d{4} \\d{1,2}:\\d{2}(:\\d{2})?$", column)) {
      return("%d/%m/%Y %H:%M")
    }

    # Check if the date is in "y-m-d" format
    if (grepl("^\\d{2}/\\d{1}/\\d{4} \\d{1,2}:\\d{2}(:\\d{2})?$", column)) {
      return("%d/%m/%Y %H:%M")
    }

    # If none of the formats match, return "Unknown"
    return("Unknown")
  }


  variables_to_check <- unique(c(rules$variable1, rules$variable2)[which(c(rules$variable1, rules$variable2) != "")])
  #
  # Log when starts .........
  log4r_info_start(Sys.time(), registry_data)
  # Check if variables from the rule lists are contained in the data file
  if(all(variables_to_check %in% colnames(registry_data))) {
    log4r_info_variables_verified()
    # Data prepare
    date_column <- grep("date", names(registry_data), value = TRUE)[1]
    date_format <- check_date_format(registry_data[[date_column]][1])
    # Processes date-related columns in the registry_data table, converts their values to the desired format, and stores the transformed data in a YAML file

    tbl_store(
      rlang::inject(
      registry_data ~ registry_data %>%
        mutate(across(
          matches("date"),
          ~ as.Date(., format = !!date_format)
        ))
      %>%
        mutate(across(
          matches("last_attended_appt"),
          ~ as.Date(., format = !!date_format)
        )))
      ,
      .init = ~ library(tidyverse)
    ) %>%
      yaml_write(filename = "QCRegistry.yml", path = yml_path)

    agent <- create_agent(
      tbl = rlang::inject(~ tbl_source("registry_data", file.path(!!yml_path, "QCRegistry.yml"))),
      tbl_name = "registry_data",
      label = "Registry data validation",
    )

    # Iterate over each row in the 'rules' data frame
    for (i in 1:nrow(rules)) {
      if (rules$rule_category[i] == "exist check") {
        if (rules$pre_condition_filter[i] == ""){
          agent <- agent %>%
            col_vals_in_set(
              columns = rules$variable1[i],
              set = c(ALIVE,DECEASED),
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        } else{
          agent <- agent %>%
            col_vals_in_set(
              columns = rules$variable1[i],
              preconditions = rlang::inject(~ . %>%
                                              dplyr::filter(!!rlang::sym(rules$precondition_filter_variable[i]) == rules$pre_condition_filter[!!i])),
              set = EMPTY,
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        }

      } else if (rules$rule_category[i] == "null check") {
        if (rules$pre_condition_filter[i] != ""){
          agent <- agent %>%
            col_vals_null(
              columns = rules$variable1[i],
              preconditions = rlang::inject(~ . %>%
                                              dplyr::filter(!!rlang::sym(rules$precondition_filter_variable[i]) == rules$pre_condition_filter[!!i])),
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        } else{
          agent <- agent %>%
            col_vals_null(
              columns = rules$variable1[i],
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        }


      } else if (rules$rule_category[i] == "not null check") {
        if(rules$pre_condition_filter[i] != ""){
          agent <- agent %>%
            col_vals_not_null(
              columns = rules$variable1[i],
              preconditions = rlang::inject(~ . %>%
                                              dplyr::filter(!!rlang::sym(rules$precondition_filter_variable[i]) == rules$pre_condition_filter[!!i])),
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        }else{
          agent <- agent %>%
            col_vals_not_null(
              columns = rules$variable1[i],
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        }

      } else if (rules$rule_category[i] == "greater than") {
        if(rules$pre_condition_filter[i] == ""){
          agent <- agent %>%
            col_vals_gt(
              columns = rules$variable1[i],
              preconditions = rlang::inject(~ . %>% tidyr::drop_na(!!rlang::sym(rules$variable1[i]),
                                                                   !!rlang::sym(rules$variable2[i]))),
              value = vars(!!rlang::sym(rules$variable2[i])),
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        }
        else{
          agent <- agent %>%
            col_vals_gt(
              columns = rules$variable1[i],
              preconditions = rlang::inject(~ . %>% tidyr::drop_na(!!rlang::sym(rules$variable1[i]),
                                                                   !!rlang::sym(rules$variable2[i])) %>%
                                              dplyr::filter(!!rlang::sym(rules$precondition_filter_variable[i]) == rules$pre_condition_filter[!!i])),
              value = vars(!!rlang::sym(rules$variable2[i])),
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        }


      } else if (rules$rule_category[i] == "less than") {
        if(rules$pre_condition_filter[i] != ""){
          agent <- agent %>%
            col_vals_lt(
              columns = rules$variable1[i],
              preconditions = rlang::inject(~ . %>%
                                              tidyr::drop_na(!!rlang::sym(rules$variable1[i])) %>%
                                              dplyr::filter(!!rlang::sym(rules$precondition_filter_variable[i]) == rules$pre_condition_filter[!!i])),
              value = Sys.Date(),
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        }
        else {
          agent <- agent %>%
            col_vals_lt(
              columns = rules$variable1[i],
              preconditions = rlang::inject(~ . %>% tidyr::drop_na(!!rlang::sym(rules$variable1[i]))),
              value = Sys.Date(),
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        }
      } else if (rules$rule_category[i] == "regex") {
        if(rules$pre_condition_filter[i] == "Null Filter"){
          agent <- agent %>%
            col_vals_regex(
              columns = rules$variable1[i],
              preconditions = rlang::inject(~ . %>% tidyr::drop_na(!!rlang::sym(rules$variable2[i]))),
              regex = rules$value[i],
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        } else if(rules$pre_condition_filter[i] != ""){
          agent <- agent %>%
            col_vals_regex(
              columns = rules$variable1[i],
              preconditions = rlang::inject(~ . %>%
                                              dplyr::filter(!!rlang::sym(rules$precondition_filter_variable[i]) == rules$pre_condition_filter[!!i])),
              regex = rules$value[i],
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        } else {
          agent <- agent %>%
            col_vals_regex(
              columns = rules$variable1[i],
              preconditions = rlang::inject(~ . %>% dplyr::filter(grepl("[a-zA-Z0-9]+", !!rlang::sym(rules$variable2[i])))),
              regex = rules$value[i],
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        }
      }
    }


    agent <- agent%>%interrogate()

    all_passed(agent)
    report <- get_agent_report(agent)

    yaml_write(agent, path = yml_path)
    yaml_agent_string(filename = "agent-registry_data.yml", path = yml_path)
    export_report(agent,
                  filename = affix_datetime("validation_report.html"),
                  path = yml_path)

    # Log when completes .......
    log4r_info_complete(Sys.time(), yml_path)

    return(agent)

  } else{
    log4r_info_variables_not_found(unique(variables_to_check[which(!variables_to_check%in%colnames(registry_data))]))
    stop()
  }



  return()

}





