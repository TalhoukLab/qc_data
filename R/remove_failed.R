#' Remove failed data
#'
#' @param agent Run qc_main to retrieve agent
#' @param data Registry data
#' @param rule Rule #
#'
#' @export
#'
remove_failed <- function(agent, data, rule) {
  library(sqldf)

  # Retrieve the data to be removed
  to_remove <- get_data_extracts(agent, rule)

  # Ensure 'to_remove' and 'data' are data frames and contain 'study_id'
  if (!is.data.frame(to_remove) || !is.data.frame(data)) {
    stop("Both 'to_remove' and 'data' should be data frames.")
  }
  if (!("study_id" %in% names(to_remove)) || !("study_id" %in% names(data))) {
    stop("'study_id' column is missing from one of the data frames.")
  }

  # Perform the SQL operation
  removed_df <- sqldf("SELECT * FROM data WHERE study_id NOT IN (SELECT study_id FROM to_remove)")

  # Define the output file path (you can customize this path as needed)
  output_file <- "removed_data.csv"

  # Write the CSV file
  write.csv(removed_df, file = output_file, row.names = FALSE)

  # Confirm file creation
  message("File written to ", output_file)
}

