#### Preamble ####
# Purpose: Use simulated data to test the performance of statistical models or algorithms to ensure they work reliably under different circumstances
# Author: HengMa
# Date: 17/04/2024
# Contact: heng.ma@mail.utoronto.ca




#### Workspace setup ####
library(tidyverse)


#### Simulate data ####
library(boot)


data <- read.csv("/cloud/project/data/analysis_data/Analized_data.csv")

run_regression <- function(data) {
  model <- lm(`Consumer_Price_Index` ~ 
                `Mobile_Internet_Access_Traffic` + 
                `National_Fiscal_Expenditure` + `Money_and_Quasi_Money_Supply` + 
                `Cumulative_Growth_of_Fixed_Asset_Investment`, data = data)
  list(coefficients = coef(model), r_squared = summary(model)$r.squared)
  summary((model))
}
set.seed(123)  
results <- replicate(100, run_regression(data), simplify = FALSE)

results[[1]]



