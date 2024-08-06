library(testthat)
library(mockery)  # For function mocking

# Mock function
mock_get_data_extracts <- function(agent, rule) {
  data.frame(study_id = c(2, 4, 6))
}

test_that("remove_failed removes the correct rows", {
  # Use the mock function
  mockery::stub(remove_failed, "get_data_extracts", mock_get_data_extracts)

  # Create example data
  data <- data.frame(study_id = 1:6, value = letters[1:6])
  agent <- NULL
  rule <- NULL

  # Call the function
  remove_failed(agent, data, rule)

  # Load the output file and check its contents
  removed_data <- read.csv("removed_data.csv")

  # Expected result
  expected_data <- data.frame(study_id = c(1, 3, 5), value = letters[c(1, 3, 5)])

  expect_equal(removed_data, expected_data)

  # Clean up
  file.remove("removed_data.csv")
})

test_that("remove_failed stops if 'to_remove' is not a data frame", {
  # Use the mock function that returns a non-data frame
  mockery::stub(remove_failed, "get_data_extracts", function(agent, rule) NULL)

  data <- data.frame(study_id = 1:6, value = letters[1:6])
  agent <- NULL
  rule <- NULL

  expect_error(remove_failed(agent, data, rule),
               "Both 'to_remove' and 'data' should be data frames.")
})

test_that("remove_failed stops if 'data' is not a data frame", {
  # Use the mock function
  mockery::stub(remove_failed, "get_data_extracts", mock_get_data_extracts)

  data <- list()  # Not a data frame
  agent <- NULL
  rule <- NULL

  expect_error(remove_failed(agent, data, rule),
               "Both 'to_remove' and 'data' should be data frames.")
})

test_that("remove_failed stops if 'study_id' column is missing", {
  # Use the mock function
  mockery::stub(remove_failed, "get_data_extracts", function(agent, rule) {
    data.frame(non_study_id = 2:4)
  })

  data <- data.frame(value = letters[1:6])
  agent <- NULL
  rule <- NULL

  expect_error(remove_failed(agent, data, rule),
               "'study_id' column is missing from one of the data frames.")
})
