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
qc_main <- function(data_file, rules_file_path, yml_path, log_file) {

  # Load necessary libraries
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(yaml)
  library(log4r)
  library(tbl2yaml)  # Assuming this is used for converting data to YAML (not fully detailed in original code)

  # Configure paths and constants
  config <- function(data_file) {
    ALIVE <- "A"
    DECEASED <- "D"
    EMPTY <- c("", " ", "  ", NULL)
    yml_path <- file.path(yml_path, "yml")
    data_path <- file.path(getwd(), "data")
    data <- file.path(data_path, data_file)
    rule_path <- file.path(getwd(), "rules")

    list(
      ALIVE = ALIVE,
      DECEASED = DECEASED,
      EMPTY = EMPTY,
      yml_path = yml_path,
      data_path = data_path,
      data_file = data_file,
      data = data,
      rule_path = rule_path
    )
  }

  # Logging functions
  my_logfile <- log_file
  my_console_appender <- console_appender(layout = default_log_layout())
  my_file_appender <- file_appender(my_logfile, append = TRUE, layout = default_log_layout())
  my_logger <- logger(threshold = "INFO", appenders = list(my_console_appender, my_file_appender))

  log4r_info_start <- function(now, data_file) {
    log4r::info(my_logger, paste("QC Registry data file:", data_file, "on", now))
  }

  log4r_info_complete <- function(now, yml) {
    log4r::info(my_logger, paste("Registry data QC is done. Results are saved to", yml, "at", now))
  }

  log4r_info_variables_verified <- function() {
    log4r::info(my_logger, "All variables checked by QC are included in the Registry data file.")
  }

  log4r_info_variables_not_found <- function(variables) {
    log4r::info(my_logger, paste("Variables:", variables, "are not included in the Registry data file."))
  }

  # Main QC process

  # Setup paths and constants
  paths_and_values <- config(data_file)
  ALIVE <- paths_and_values$ALIVE
  DECEASED <- paths_and_values$DECEASED
  EMPTY <- paths_and_values$EMPTY
  yml_path <- paths_and_values$yml_path
  rule_path <- paths_and_values$rule_path
  data <- paths_and_values$data

  # Read in data file
  registry_data <- read.csv(data, check.names = FALSE, stringsAsFactors = FALSE)

  # Read in rule list from Excel file
  rules <- read_excel(rules_file_path) %>%
    mutate_if(is.logical, as.character)
  rules$pre_condition_filter <- rules$pre_condition_filter %>% replace_na("")
  rules$precondition_filter_variable <- rules$precondition_filter_variable %>% replace_na("")

  # Log when starts .........
  log4r_info_start(Sys.time(), data_file)

  # Check if variables from the rule lists are contained in the data file
  variables_to_check <- unique(c(rules$variable1, rules$variable2)[which(c(rules$variable1, rules$variable2) != "")])
  if (all(variables_to_check %in% colnames(registry_data))) {
    log4r_info_variables_verified()

    # Determine date format function
    check_date_format <- function(column) {
      if (grepl("^\\d{4}-\\d{2}-\\d{2}$", column)) {
        return("%Y-%m-%d")
      }
      if (grepl("^\\d{2}/\\d{2}/\\d{4} \\d{1,2}:\\d{2}(:\\d{2})?$", column)) {
        return("%d/%m/%Y %H:%M")
      }
      return("Unknown")
    }

    # Determine date format from the data
    date_column <- grep("date", names(registry_data), value = TRUE)[1]
    date_format <- check_date_format(registry_data[[date_column]][1])

    # Processes date-related columns in the registry_data table, converts their values to the desired format, and stores the transformed data in a YAML file
    tbl_store(
      registry_data ~ registry_data %>%
        mutate(across(
          matches("date"),
          ~ as.Date(., format = date_format)
        )),
      .init = ~ library(tidyverse)
    ) %>%
      yaml_write(filename = "QCRegistry.yml", path = yml_path)

    # Create agent and apply rules
    agent <- create_agent(
      tbl = ~ tbl_source("registry_data", file.path(yml_path, "QCRegistry.yml")),
      tbl_name = "registry_data",
      label = "Registry data validation"
    )

    # Iterate over each row in the 'rules' data frame
    for (i in 1:nrow(rules)) {
      if (rules$rule_category[i] == "exist check") {
        if (rules$pre_condition_filter[i] == "") {
          agent <- agent %>%
            col_vals_in_set(
              columns = rules$variable1[i],
              set = c(ALIVE, DECEASED),
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        } else {
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
        if (rules$pre_condition_filter[i] != "") {
          agent <- agent %>%
            col_vals_null(
              columns = rules$variable1[i],
              preconditions = rlang::inject(~ . %>%
                                              dplyr::filter(!!rlang::sym(rules$precondition_filter_variable[i]) == rules$pre_condition_filter[!!i])),
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        } else {
          agent <- agent %>%
            col_vals_null(
              columns = rules$variable1[i],
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        }

      } else if (rules$rule_category[i] == "not null check") {
        if (rules$pre_condition_filter[i] != "") {
          agent <- agent %>%
            col_vals_not_null(
              columns = rules$variable1[i],
              preconditions = rlang::inject(~ . %>%
                                              dplyr::filter(!!rlang::sym(rules$precondition_filter_variable[i]) == rules$pre_condition_filter[!!i])),
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        } else {
          agent <- agent %>%
            col_vals_not_null(
              columns = rules$variable1[i],
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        }

      } else if (rules$rule_category[i] == "greater than") {
        if (rules$pre_condition_filter[i] == "") {
          agent <- agent %>%
            col_vals_gt(
              columns = rules$variable1[i],
              preconditions = rlang::inject(~ . %>% tidyr::drop_na(!!rlang::sym(rules$variable1[i]),
                                                                   !!rlang::sym(rules$variable2[i]))),
              value = vars(!!rlang::sym(rules$variable2[i])),
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        } else {
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
        if (rules$pre_condition_filter[i] != "") {
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
        } else {
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
        if (rules$pre_condition_filter[i] == "Null Filter") {
          agent <- agent %>%
            col_vals_regex(
              columns = rules$variable1[i],
              preconditions = rlang::inject(~ . %>% tidyr::drop_na(!!rlang::sym(rules$variable2[i]))),
              regex = rules$value[i],
              actions = action_levels(warn_at = rules$warning[i], stop_at = rules$stop[i]),
              label = rules$rule_description[i]
            )
        } else if (rules$pre_condition_filter[i] != "") {
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

    # Interrogate agent and check if all passed
    agent <- agent %>% interrogate()
    all_passed(agent)
    report <- get_agent_report(agent)

    # Write YAML agent configuration
    yaml_write(agent, path = yml_path)
    yaml_agent_string(filename = "agent-registry_data.yml", path = yml_path)

    # Export HTML report
    export_report(agent,
                  filename = affix_datetime("validation_report.html"),
                  path = yml_path)

    # Log when completes .......
    log4r_info_complete(Sys.time(), yml_path)

    return(agent)  # Return the agent object

  } else {
    # Log variables not found and stop execution
    log4r_info_variables_not_found(unique(variables_to_check[which(!variables_to_check %in% colnames(registry_data))]))
    stop()
  }
}
