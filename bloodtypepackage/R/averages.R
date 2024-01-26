#' Calculate the average of each blood type, including Country and Population
#'
#' This function calculates the average frequency for each blood type.
#'
#' @param filepath Path to the CSV file containing blood type data.
#' @return A tibble with BloodType and AverageFrequency columns sorted by descending AverageFrequency.
#' @export
#' @examples
#' calculate_blood_type_averages("/path/to/your/file.csv")

calculate_blood_type_averages <- function(filepath) {
  # Read the data from the CSV file
  data <- read.csv(filepath, header = TRUE, check.names = FALSE)

  # Exclude the first two columns (Country and Population) for analysis and convert to a tibble
  data <- as_tibble(data) %>%
    select(-Country, -Population)

  # Calculate the total frequency for each blood type
  blood_type_frequencies <- colSums(data)

  # Find the most common blood type
  most_common_blood_type <- names(blood_type_frequencies)[which.max(blood_type_frequencies)]

  cat("The most common blood type worldwide is:", most_common_blood_type, "\n")

  # Calculate the averages
  averages <- data %>%
    summarise(across(starts_with("O"), mean, na.rm = TRUE),
              across(starts_with("A"), mean, na.rm = TRUE),
              across(starts_with("B"), mean, na.rm = TRUE),
              across(starts_with("AB"), mean, na.rm = TRUE))

  # Reshape the data for better presentation
  averages <- gather(averages, BloodType, AverageFrequency, starts_with(c("O", "A", "B", "AB")))

  # Arrange by descending AverageFrequency
  averages <- averages %>%
    arrange(desc(AverageFrequency))

  return(averages)
}

