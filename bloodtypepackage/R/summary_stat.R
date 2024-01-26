#' Calculate summary statistics for each blood type
#'
#' This function calculates summary statistics (e.g., mean, median, etc.) for each blood type.
#'
#' @param filepath Path to the CSV file containing blood type data.
#' @export
#' @examples
#' calculate_blood_type_summary_stats("/path/to/your/file.csv")
#'

calculate_blood_type_summary_stats <- function(filepath) {
  # Read the data from the CSV file
  data <- read.csv(filepath, header = TRUE, check.names = FALSE)

  # Calculate summary statistics
  summary_stats <- summary(data[, -c(1, 2)]) # Exclude first two columns (Country, Population)
  print(summary_stats)
}
