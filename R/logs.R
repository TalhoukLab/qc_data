# My logs
my_logfile = paste0(getwd(), "/log/logs.txt")

my_console_appender = console_appender(layout = default_log_layout())
my_file_appender = file_appender(my_logfile, append = TRUE, layout = default_log_layout())

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
