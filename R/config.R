# values
ALIVE = "A"
DECEASED = "D"
EMPTY = c("", " ", "  ", NULL)

# path
yml_path <- paste0(getwd(), "/yml/")
data_path <- paste0(getwd(), "/data/")
# data_file <- "q20-00031_disease_details.csv"
data_file <- "Q2024-4250936_disease_details.csv"
data <- paste0(data_path, "/", data_file)
rule_path <- paste0(getwd(), "/rules/")
