library(testthat)
library(QcData)
library(yaml)
library(log4r)
library(dplyr)

test_that("qc_main handles missing variables correctly", {

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
  expect_error(qc_main(registry_data,rules,,))
})
