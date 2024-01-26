# Load the tidyverse package
library(tidyverse)

# Read the data with backticks around column names
blood_type_data <- read.csv("/Users/sahil_sewani/Documents/VCU/Fall 2023/Biostatistical Computing (BIOS 524)/HW/Final R Project/bloodtypes.csv", header = TRUE, check.names = FALSE)

# Exclude the first two columns (Country and Population) for analysis and convert to a tibble
blood_type_data <- as_tibble(blood_type_data) %>%
  select(-Country, -Population)

# Calculate the total frequency for each blood type
blood_type_frequencies <- colSums(blood_type_data)

### Find the most common blood type
most_common_blood_type <- names(blood_type_frequencies)[which.max(blood_type_frequencies)]

cat("The most common blood type worldwide is:", most_common_blood_type, "\n")



### Calculate the average of each blood type, including Country and Population
calculate_blood_type_averages <- function(data) {
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

calculate_blood_type_averages(blood_type_data)

### Calculate summary statistics for each blood type
calculate_blood_type_summary_stats <- function(data) {
  summary_stats <- summary(data[, -c(1, 2)]) # Exclude first two columns (Country, Population)
  print(summary_stats)
}

calculate_blood_type_summary_stats(blood_type_data)



# Load necessary libraries
library(tidyverse)

# Load the data from the CSV file
blood_type_data <- read.csv("/Users/sahil_sewani/Documents/VCU/Fall 2023/Biostatistical Computing (BIOS 524)/HW/Final R Project/bloodtypes.csv", header = TRUE, fileEncoding = "UTF-8")

# Manually set the column names
colnames(blood_type_data) <- c("Country", "Population", "O+", "A+", "B+", "AB+", "O-", "A-", "B-", "AB-")

### Create box plots for each blood type (function)
create_blood_type_boxplots <- function(data) {
  data %>%
    pivot_longer(cols = -c(Country, Population), names_to = "Blood_Type", values_to = "Frequency") %>%
    ggplot(aes(x = Blood_Type, y = Frequency)) +
    geom_boxplot(fill = "skyblue", alpha = 0.7) +
    labs(title = "Box Plot of Blood Type Frequencies",
         x = "Blood Type",
         y = "Frequency") +
    theme_minimal()
}

### initialize box-plot function
create_blood_type_boxplots(blood_type_data)


### Create histograms for each blood type with colors (function)
create_blood_type_histogram <- function(blood_type_data) {
  blood_type_data %>%
    pivot_longer(cols = -c(Country, Population), names_to = "Blood_Type", values_to = "Frequency") %>%
    ggplot(aes(x = Blood_Type, y = Frequency, fill = Blood_Type)) +
    geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.7) +
    scale_fill_manual(values = c(
      "O+" = "blue",
      "A+" = "green",
      "B+" = "red",
      "AB+" = "purple",
      "O-" = "lightblue",
      "A-" = "lightgreen",
      "B-" = "lightcoral",
      "AB-" = "orchid"
    )) +
    labs(title = "Histogram of Blood Type Frequencies",
         x = "Blood Type",
         y = "Frequency") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

### initialize histogram function
create_blood_type_histogram(blood_type_data)


create_blood_type_bar_plot <- function(data) {
  data[1:10]
  data %>%
    pivot_longer(cols = -c(Country, Population), names_to = "Blood_Type", values_to = "Frequency") %>%
    ggplot(aes(x = Country, y = Frequency, fill = Blood_Type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), alpha = 0.7) +  # Adjust width here
    scale_fill_manual(values = c(
      "O+" = "blue",
      "A+" = "green",
      "B+" = "red",
      "AB+" = "purple",
      "O-" = "lightblue",
      "A-" = "lightgreen",
      "B-" = "lightcoral",
      "AB-" = "orchid"
    )) +
    labs(title = "Blood Type Frequencies by Country",
         x = "Country",
         y = "Frequency") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.margin = margin(2, 2, 4, 2, "cm"))  # Adjust plot margin for more space
}

create_blood_type_bar_plot(blood_type_data)










