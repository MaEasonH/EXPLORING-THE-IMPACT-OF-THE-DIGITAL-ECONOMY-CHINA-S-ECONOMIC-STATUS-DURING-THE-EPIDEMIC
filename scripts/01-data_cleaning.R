#### Preamble ####
# Purpose: Make data more efficient
# Author: HengMa
# Date: 17/04/2024
# Contact: heng.ma@mail.utoronto.ca



#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(lubridate)
#### Clean data ####

data <- read.csv("/cloud/project/data/analysis_data/Analized_data.csv")

# Convert 'Month' to date format assuming the format is DD/MM/YYYY
data$Month <- dmy(data$Month)
q25 <- quantile(data$Cumulative_Growth_of_Fixed_Asset_Investment, 0.25)
q75 <- quantile(data$Cumulative_Growth_of_Fixed_Asset_Investment, 0.75)
iqr <- q75 - q25

lower_bound <- q25 - 1.5 * iqr
upper_bound <- q75 + 1.5 * iqr

# Replace outliers with NA (or handle as needed)
data$Cumulative_Growth_of_Fixed_Asset_Investment[data$Cumulative_Growth_of_Fixed_Asset_Investment < lower_bound |
                                                   data$Cumulative_Growth_of_Fixed_Asset_Investment > upper_bound] <- NA

# Remove any rows with NA if needed (depends on how you want to handle missing data)
data <- na.omit(data)
max_index <- max(data$Consumer_Price_Index, na.rm = TRUE)
min_index <- min(data$Consumer_Price_Index, na.rm = TRUE)
data$Consumer_Price_Index_scaled <- (data$Consumer_Price_Index - min_index) / (max_index - min_index)

# Save the cleaned data
write.csv(data, "/cloud/project/data/analysis_data/Cleaned_data.csv", row.names = FALSE)


