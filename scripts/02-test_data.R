#### Preamble ####
# Purpose: Test data reliability
# Author: HengMa
# Date: 17/04/2024
# Contact: heng.ma@mail.utoronto.ca


#### Workspace setup ####
library(testthat)
library(dplyr)
library(readr)
library(lubridate)

#### Test data ####


data <- read_csv("/cloud/project/data/analysis_data/Analized_data.csv")

# Test 1: Data has the correct columns
test_that("Data has the correct columns", {
  expected_columns <- c("Month", "Consumer_Price_Index", "Mobile_Internet_Access_Traffic",
                        "National_Fiscal_Expenditure", "Money_and_Quasi_Money_Supply",
                        "Cumulative_Growth_of_Fixed_Asset_Investment")
  expect_true(all(expected_columns %in% colnames(data)))
})

# Test 2: No missing values
test_that("There are no missing values", {
  expect_true(!any(is.na(data)))
})

# Test 3: Correct data types
test_that("Correct data types for each column", {
  expect_is(data$Month, "character")
  expect_is(data$Consumer_Price_Index, "numeric")
  expect_is(data$Mobile_Internet_Access_Traffic, "numeric")
  expect_is(data$National_Fiscal_Expenditure, "numeric")
  expect_is(data$Money_and_Quasi_Money_Supply, "numeric")
  expect_is(data$Cumulative_Growth_of_Fixed_Asset_Investment, "numeric")
})

# Test 4: Month is in the correct date format
test_that("Month is in the correct date format", {
  expect_true(all(!is.na(dmy(data$Month))))
})

# Test 5: Numeric columns within expected ranges
test_that("Numeric columns within expected ranges", {
  expect_true(all(data$Consumer_Price_Index >= 95 & data$Consumer_Price_Index <= 110))
  expect_true(all(data$Mobile_Internet_Access_Traffic > 0))
  expect_true(all(data$National_Fiscal_Expenditure > 30000))
  expect_true(all(data$Money_and_Quasi_Money_Supply > 1900000))
  expect_true(all(data$Cumulative_Growth_of_Fixed_Asset_Investment > -30 & 
                    data$Cumulative_Growth_of_Fixed_Asset_Investment < 40))
})

# Test 6: Data is sorted by month

test_that("Data is sorted by Month", {
  
  data$Month <- dmy(data$Month)
  sorted_data <- data[order(data$Month),]
  sorted_dates <- sorted_data$Month
  
  # Check if the sorted dates are identical to the original dates (they should be since we sorted them)
  expect_true(identical(sorted_dates, sort(sorted_dates)))
})