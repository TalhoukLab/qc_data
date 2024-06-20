#' Configuration Function
#'
#' @param data_file the name (in quotes "") of the data frame undergoing QC
#'
#' @return List of paths and values
#' @export
#'
#' @examples
config <- function(data_file) {
  # Values
  ALIVE <- "A"
  DECEASED <- "D"
  EMPTY <- c("", " ", "  ", NULL)

  # Paths
  yml_path <- file.path(getwd(), "yml")
  data_path <- file.path(getwd(), "data")
  data <- file.path(data_path, data_file)
  rule_path <- file.path(getwd(), "rules")

  # Return a list of paths and values
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
