#### Preamble ####
# Purpose: Conveniently browse models
# Author: HengMa
# Date: 17/04/2024
# Contact: heng.ma@mail.utoronto.ca



#### Workspace setup ####
library(tidyverse)
library(rstanarm)

#### Read data ####
data <- read_csv("/cloud/project/data/analysis_data/Cleaned_data.csv")

### Model data ####
first_model <- lm(Consumer_Price_Index ~ Mobile_Internet_Access_Traffic, data = data)
options(show.signif.stars = FALSE)

second_model <- lm(Consumer_Price_Index ~ Mobile_Internet_Access_Traffic + Month + National_Fiscal_Expenditure + Money_and_Quasi_Money_Supply + Cumulative_Growth_of_Fixed_Asset_Investment, data = data)
options(show.signif.stars = FALSE)



#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)
saveRDS(
  second_model,
  file = "models/second_model.rds"
)

