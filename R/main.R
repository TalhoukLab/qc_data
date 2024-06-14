# please make this into a function

#' [description of your function]
#'
#' @field [input parameter]
#' @field [input parameter]
#' @field [input parameter ... etc]
#'
#' @export [function name, I think you need this so that when you build the package, this function will be visible to the user using this package]
#' @examples



################################
# Load package
library(pointblank)
library(dplyr)
library(stringr)
library(log4r)
library(tidyr)

# Source script
source(paste0(getwd(), "/config.R"))
source(paste0(getwd(), "/logs.R"))

# Read in data file
registry_data <- read.csv(data, check.names = FALSE, stringsAsFactors = FALSE)

# Read in rule list
options(scipen = 999)
rules <- read.csv(paste0(rule_path, "rules_Q2024.csv"), check.names = FALSE, stringsAsFactors = FALSE) %>% mutate_if(is.logical, as.character)
rules$pre_condition_filter <- rules$pre_condition_filter %>% replace_na("")
rules$precondition_filter_variable <- rules$precondition_filter_variable %>% replace_na("")

date_format <- ""

check_date_format <- function(column) {

  date_parts <- strsplit(column, " ")[[1]]

  # Check if the first part contains four digits (for year) and the second part contains a time
  if (length(date_parts) > 1 && grepl("^\\d{2}/\\d{2}/\\d{4}$", date_parts[1])) {
    date_format = "%d/%m/%Y %H:%M"
  } else if (length(date_parts) == 1 && grepl("^\\d{4}$", date_parts[1])) {
    # Check if the first part contains four digits (for year)
    date_format = "%Y-%m-%d"
  } else if (length(date_parts) > 1 && grepl("^\\d{2}-\\d{2}-\\d{4}$", date_parts[1])) {
    # Check if the second part (after splitting by whitespace) has the format "dd-mm-yyyy hh:mm"
    date_format = "%d-%m-%Y %H:%M"
  } else {
    return("Unknown")
  }
}

check_date_format(registry_data$diagnosis_date)

variables_to_check <- unique(c(rules$variable1, rules$variable2)[which(c(rules$variable1, rules$variable2) != "")])
#
# Log when starts .........
log4r_info_start(Sys.time(), data_file)
# Check if variables from the rule lists are contained in the data file
if(all(variables_to_check %in% colnames(registry_data))) {
  log4r_info_variables_verified()
  # Data prepare

  # Processes date-related columns in the registry_data table, converts their values to the desired format, and stores the transformed data in a YAML file
  tbl_store(
    registry_data ~ registry_data %>%
      mutate(across(
        matches("date"),
        ~ as.Date(., format = date_format)
      ))
    %>%
      mutate(across(
        matches("last_attended_appt"),
        ~ as.Date(., format = date_format)
      ))
    ,
    .init = ~ library(tidyverse)
  ) %>%
    yaml_write(filename = "QCRegistry.yml", path = yml_path)

  agent <- create_agent(
          tbl = ~ tbl_source("registry_data", file.path(yml_path, "QCRegistry.yml")),
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

} else{
  log4r_info_variables_not_found(unique(variables_to_check[which(!variables_to_check%in%colnames(registry_data))]))
  stop()
}
