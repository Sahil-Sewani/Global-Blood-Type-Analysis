# Global-Blood-Type-Analysis

## Description of data
The "Global Blood Type Distribution Dataset" compiles worldwide blood type information. It offers a broad view of ABO blood group prevalence across diverse populations and regions. This dataset is a valuable tool for researchers, healthcare professionals, and data analysts to study global blood type diversity. It can support a range of studies, including cross-cultural research and medical analysis. Researchers can use this raw dataset to uncover significant trends and correlations related to blood types in different countries. The data will be explored in a quantitative manner and displayed through bar-plots and histograms. 

Link to dataset: <https://www.kaggle.com/datasets/kamilenovaes/global-blood-type-distribution>

Data visualization of results: <https://github.com/Sahil-Sewani/Global-Blood-Type-Analysis/blob/main/summary_stat_visualization.pdf>

Raw code of analysis: <[https://github.com/Sahil-Sewani/bloodtypes](https://github.com/Sahil-Sewani/Global-Blood-Type-Analysis/blob/main/globalbloodtypeanalysis_code.R)>

### Finding the most common blood type 

This code reads blood type data from a CSV file, processes it to exclude certain columns, calculates the total frequency of each blood type, and then identifies and prints the most common blood type.

```{r}
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
```

### Calculate the average of each blood type

This code loads necessary R packages, installs and loads the 'bloodtypepackage', uses it to calculate the average frequency of each blood type from a CSV file, and then prints the result to the console.

```{r}
# Load the tidyverse package
library(tidyverse)

# Load the 'bloodtypepackage'
install.packages('bloodtypepackage')
library(bloodtypepackage)

averages_of_types <- bloodtypepackage::calculate_blood_type_averages("/Users/sahil_sewani/Documents/VCU/Fall 2023/Biostatistical Computing (BIOS 524)/HW/Final R Project/bloodtypes.csv")
print(averages_of_types)
```

## Calculate the summary statistics of each blood type

this code loads necessary R packages, installs and loads the 'bloodtypepackage', uses it to calculate summary statistics for each blood type from a CSV file, and then prints the result to the console.

```{r}
# Load the tidyverse package
library(tidyverse)

# Load the 'bloodtypepackage'
install.packages('bloodtypepackage')
library(bloodtypepackage)

summary_statistics_types <- bloodtypepackage::calculate_blood_type_summary_stats("/Users/sahil_sewani/Documents/VCU/Fall 2023/Biostatistical Computing (BIOS 524)/HW/Final R Project/bloodtypes.csv")
```

![Screen Shot 2024-01-26 at 1 04 16 PM](https://github.com/Sahil-Sewani/Global-Blood-Type-Analysis/assets/97994997/37cddaa5-376e-4f92-a249-6e60bc45df8e)


Analysis of results:
For O+ blood type, the frequencies range from a minimum of 25.50 to a maximum of 75.00. The data is spread across the quartiles as follows: the first quartile (25th percentile) lies at 32.08, the median (50th percentile) is 38.17, and the third quartile (75th percentile) is 46.82. The mean frequency, representing the average, is approximately 40.35.

A+ blood type shows frequencies ranging from a minimum of 14.00 to a maximum of 46.30. The first quartile is 25.82, the median is 30.00, and the third quartile is 34.85. The mean frequency is around 29.67.

B+ blood type exhibits frequencies with a minimum of 4.72 and a maximum of 36.80. The first quartile is 10.00, the median is 15.00, and the third quartile is 21.23. The mean frequency is approximately 16.40.

AB+ blood type has frequencies ranging from a minimum of 0.500 to a maximum of 14.700. The first quartile is 2.925, the median is 4.295, and the third quartile is 6.300. The mean frequency is about 4.827.

For O- blood type, frequencies range from a minimum of 0.060 to a maximum of 13.000. The first quartile is 1.790, the median is 4.000, and the third quartile is 6.000. The mean frequency is approximately 3.917.

A- blood type displays frequencies with a minimum of 0.040 and a maximum of 8.000. The first quartile is 1.000, the median is 2.700, and the third quartile is 6.000. The mean frequency is around 3.277.

B- blood type exhibits frequencies ranging from a minimum of 0.010 to a maximum of 3.130. The first quartile is 0.540, the median is 1.250, and the third quartile is 2.000. The mean frequency is approximately 1.334.

Finally, AB- blood type shows frequencies with a minimum of 0.010 and a maximum of 1.200. The first quartile is 0.150, the median is 0.400, and the third quartile is 0.910. The mean frequency is about 0.4963. Please note that there may be missing values in the dataset, as indicated by the presence of "NA's" in the summary.

## Create box-plots for each blood type

This code defines a function named create_blood_type_boxplots which generates box plots for blood type frequencies.

```{r}
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
```

![Screen Shot 2024-01-26 at 1 06 10 PM](https://github.com/Sahil-Sewani/Global-Blood-Type-Analysis/assets/97994997/68d11f63-80e7-452e-b736-f7c75e53670c)


The box-plot reflects the frequency data

## Create a histogram for the blood type frequencies

This code defines a function named create_blood_type_histogram which creates histograms for blood type frequencies, with each blood type represented by a different color.

```{r}
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
```

![Screen Shot 2024-01-26 at 1 07 27 PM](https://github.com/Sahil-Sewani/Global-Blood-Type-Analysis/assets/97994997/73a20025-8b50-4b27-a325-25b8fff22670)


The histogram reflects the frequency data

