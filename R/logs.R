#' Initialize logging configuration
#'
#' Initializes logging configuration with log file setup and logging functions
#'
#' @param log_file The name of the log file to use
#'
#' @return list of logging functions
#' @export
#'
#' @examples
logs <- function(log_file = "logs.txt") {

  # Initialize log file path
  log_path <- file.path(getwd(), "log", log_file)

  # Initialize append-ers
  my_console_appender <- console_appender(layout = default_log_layout())
  my_file_appender <- file_appender(log_path, append = TRUE, layout = default_log_layout())

  # Create logger
  my_logger <- logger(threshold = "INFO", appenders = list(my_console_appender, my_file_appender))

  # Log functions
  log_info_start <- function(now, data_file) {
    log4r::info(my_logger, paste("QC Registry data file:", data_file, "on", now))
  }

  log_info_complete <- function(now, yml) {
    log4r::info(my_logger, paste("Registry data QC is done. Results are saved to", yml, "at", now))
  }

  log_info_variables_verified <- function() {
    log4r::info(my_logger, "All variables checked by QC are included in the Registry data file.")
  }

  log_info_variables_not_found <- function(variables) {
    log4r::info(my_logger, paste("Variables:", variables, "are not included in the Registry data file."))
  }

  # Return list of logging functions
  list(
    log_info_start = log_info_start,
    log_info_complete = log_info_complete,
    log_info_variables_verified = log_info_variables_verified,
    log_info_variables_not_found = log_info_variables_not_found
  )
}
