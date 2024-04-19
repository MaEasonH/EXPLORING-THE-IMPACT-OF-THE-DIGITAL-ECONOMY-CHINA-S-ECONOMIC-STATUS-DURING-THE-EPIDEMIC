#### Preamble ####
# Purpose: replications
# Author: HengMa
# Date: 17/04/2024
# Contact: heng.ma@mail.utoronto.ca

#### Workspace setup ####
library(ggplot2)
library(readr)
library(beepr)
library(broom)
library(broom.mixed)
library(knitr)
library(modelsummary)
library(purrr)
library(rstanarm)
library(testthat)
library(tidyverse)
library(kableExtra)
library(xtable)
library(flextable)
library(MASS)

#### Load data ####

data <- read.csv("/cloud/project/data/analysis_data/Cleaned_data.csv")


data$Month <- as.Date(data$Month, format = "%Y/%m/%d")

# Create a scatter plot for each independent variable against CPI
variables <- c("Mobile_Internet_Access_Traffic" 
)

plots <- lapply(variables, function(var) {
  ggplot(data, aes(x = !!sym(var), y = Consumer_Price_Index)) +
    geom_point(alpha = 0.6) +  # Scatter points with slight transparency
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +  # Explicit formula
    labs(title = paste("Relationship between CPI and", var),
         x = var, 
         y = "Consumer Price Index") +
    theme_minimal()
})

# Print all plots
for (plot in plots) {
  print(plot)
}

data$Month <- as.Date(data$Month, format = "%Y/%m/%d")


variables <- c("National_Fiscal_Expenditure" 
)

plots <- lapply(variables, function(var) {
  ggplot(data, aes(x = !!sym(var), y = Consumer_Price_Index)) +
    geom_point(alpha = 0.6) +  # Scatter points with slight transparency
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +  # Explicit formula
    labs(title = paste("Relationship between CPI and", "National Fiscal Expenditure"),
         x = var, 
         y = "Consumer Price Index") +
    theme_minimal()
})

# Print all plots
for (plot in plots) {
  print(plot)
}

model1 <- lm(Consumer_Price_Index ~ Mobile_Internet_Access_Traffic, data = data)
model1

ggplot(data, aes(x = Mobile_Internet_Access_Traffic, y = Consumer_Price_Index)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "black")


correlation_result <- cor(data$`National_Fiscal_Expenditure`, data$`Money_and_Quasi_Money_Supply`, use = "complete.obs")
correlation_result

ggplot(data, aes(x = National_Fiscal_Expenditure, y = Money_and_Quasi_Money_Supply)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "black")


