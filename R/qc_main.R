#' Main QC check file
#'
#' @param data_file takes in data set
#' @param rules_file_path takes in the rules file
#'
#' @return agent
#' @export
#'
#' @examples
qc_main <- function(data_file, rules_file_path) {

  # Source necessary scripts
  source("../qc_data/R/config.R")
  source("../qc_data/R/logs.R")

  # Setup paths and constants
  paths_and_values <- setup_paths(data_file)
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
      # Check if the date is in "yyyy-mm-dd" format
      if (grepl("^\\d{4}-\\d{2}-\\d{2}$", column)) {
        return("%Y-%m-%d")
      }

      # Check if the date is in "dd/mm/yyyy hh:mm" format
      if (grepl("^\\d{2}/\\d{2}/\\d{4} \\d{1,2}:\\d{2}(:\\d{2})?$", column)) {
        return("%d/%m/%Y %H:%M")
      }

      # Add more date formats as needed...

      # If none of the formats match, return "Unknown"
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
