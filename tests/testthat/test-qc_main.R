library(testthat)
library(QcData)
library(yaml)
library(log4r)
library(dplyr)

# FAIL
test_that("qc_main function works correctly", {

  # Setup temporary directories and files
  temp_dir <- tempdir()
  temp_log_file <- "test_log.txt"
  # temp_yml_path <- file.path(temp_dir, "test_yml")

  # dir.create(temp_yml_path, showWarnings = FALSE)

  # Sample registry data and rules
  registry_data <- data.frame(
    date = c("2024-07-01", "2024-07-02", "2024-07-03", NA, "2024-07-05"),
    last_attended_appt = c("2024-07-01", NA, "2024-07-03", "2024-07-04", "2024-07-05"),
    status = c("ALIVE", "DECEASED", "ALIVE", "ALIVE", "DECEASED")
  )

  rules <- data.frame(
    variable1 = c("status", "last_attended_appt"),
    variable2 = c("", ""),
    rule_category = c("exist check", "greater than"),
    rule_description = c("Status should be valid", "Date should be before today"),
    warning = c(1, 1),
    stop = c(2, 2),
    pre_condition_filter = c("", ""),
    precondition_filter_variable = c("", ""),
    value = c("", Sys.Date() - 1)
  )

  # Run qc_main function
  qc_main(
    registry_data = registry_data,
    rules = rules,,
    log_file = temp_log_file
  )

  # Check if log file is created
  # expect_true(file.exists(temp_log_file))

  # Check if YAML report is created
  # yaml_file <- file.path(temp_yml_path, "QCRegistry.yml")
  # expect_true(file.exists(yaml_file))

  # Read and validate YAML content
  yaml_content <- yaml::read_yaml(yaml_file)
  expect_true(is.list(yaml_content))

  # Check if the log file contains the expected entries
  # log_content <- readLines(temp_log_file)
  # expect_true(any(grepl("QC Registry data file:", log_content)))
  # expect_true(any(grepl("Registry data QC is done", log_content)))

  # Check for specific log entries
  # expect_true(any(grepl("All variables checked by QC are included", log_content)))
  # expect_true(any(grepl("Variables:", log_content)))
})

# PASS
test_that("qc_main handles missing variables correctly", {

  # Setup temporary directories and files
  temp_dir <- tempdir()
  temp_log_file <- file.path(temp_dir, "test_log.txt")
  temp_yml_path <- file.path(temp_dir, "test_yml")

  dir.create(temp_yml_path, showWarnings = FALSE)

  # Sample registry data and rules
  registry_data <- data.frame(
    date = c("2024-07-01", "2024-07-02", "2024-07-03", NA, "2024-07-05"),
    last_attended_appt = c("2024-07-01", NA, "2024-07-03", "2024-07-04", "2024-07-05")
  )

  rules <- data.frame(
    variable1 = c("status"),
    variable2 = c(""),
    rule_category = c("exist check"),
    rule_description = c("Status should be valid"),
    warning = c(1),
    stop = c(2),
    pre_condition_filter = c(""),
    precondition_filter_variable = c(""),
    value = c("")
  )

  # Expect error due to missing variable
  expect_error(qc_main(
    registry_data = registry_data,
    rules = rules,
    yml_path = temp_yml_path,
    log_file = temp_log_file
  ))
})

# FAIL
test_that("qc_main correctly logs information", {

  # Setup temporary directories and files
  temp_dir <- tempdir()
  temp_log_file <- file.path(temp_dir, "test_log.txt")
  temp_yml_path <- file.path(temp_dir, "test_yml")

  dir.create(temp_yml_path, showWarnings = FALSE)

  # Sample registry data and rules
  registry_data <- data.frame(
    date = c("2024-07-01", "2024-07-02", "2024-07-03", NA, "2024-07-05"),
    last_attended_appt = c("2024-07-01", NA, "2024-07-03", "2024-07-04", "2024-07-05"),
    status = c("ALIVE", "DECEASED", "ALIVE", "ALIVE", "DECEASED")
  )

  rules <- data.frame(
    variable1 = c("status"),
    variable2 = c(""),
    rule_category = c("exist check"),
    rule_description = c("Status should be valid"),
    warning = c(1),
    stop = c(2),
    pre_condition_filter = c(""),
    precondition_filter_variable = c(""),
    value = c("")
  )

  # Run qc_main function
  expect_silent(qc_main(
    registry_data = registry_data,
    rules = rules,
    yml_path = temp_yml_path,
    log_file = temp_log_file
  ))

  # Check if log file is created
  log_content <- readLines(temp_log_file)
  expect_true(any(grepl("QC Registry data file:", log_content)))
  expect_true(any(grepl("Registry data QC is done", log_content)))
})

# FAIL
test_that("qc_main creates expected YAML report structure", {

  # Setup temporary directories and files
  temp_dir <- tempdir()
  temp_log_file <- file.path(temp_dir, "test_log.txt")
  temp_yml_path <- file.path(temp_dir, "test_yml")

  dir.create(temp_yml_path, showWarnings = FALSE)

  # Sample registry data and rules
  registry_data <- data.frame(
    date = c("2024-07-01", "2024-07-02", "2024-07-03", NA, "2024-07-05"),
    last_attended_appt = c("2024-07-01", NA, "2024-07-03", "2024-07-04", "2024-07-05"),
    status = c("ALIVE", "DECEASED", "ALIVE", "ALIVE", "DECEASED")
  )

  rules <- data.frame(
    variable1 = c("status"),
    variable2 = c(""),
    rule_category = c("exist check"),
    rule_description = c("Status should be valid"),
    warning = c(1),
    stop = c(2),
    pre_condition_filter = c(""),
    precondition_filter_variable = c(""),
    value = c("")
  )

  # Run qc_main function
  expect_silent(qc_main(
    registry_data = registry_data,
    rules = rules,
    yml_path = temp_yml_path,
    log_file = temp_log_file
  ))

  # Check if YAML report is created
  yaml_file <- file.path(temp_yml_path, "QCRegistry.yml")
  expect_true(file.exists(yaml_file))

  # Read and validate YAML content
  yaml_content <- yaml::read_yaml(yaml_file)
  expect_true(is.list(yaml_content))
})
