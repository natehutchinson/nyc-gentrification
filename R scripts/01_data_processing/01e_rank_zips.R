###################################################################################################
## File name: Rank ZIPs
## Description: Create weighted percentile ranks for house prices and income
## Created: July 7, 2023
## Last edited: July 7, 2023
###################################################################################################

## Load libraries
library(tidyverse)
library(janitor)
library(Hmisc)

#### Create weighted percentile ranks for ZHVI data
## load zhvi data
zhvi <- read_csv('data/processed_data/nyc_metro_zhvi.csv') %>%
  select(-c(avg_zhvi_2000, avg_zhvi_2001, avg_zhvi_2002, avg_zhvi_2003, avg_zhvi_2004, avg_zhvi_2005, avg_zhvi_2006,
            avg_zhvi_2007, avg_zhvi_2008, avg_zhvi_2009, avg_zhvi_2010, avg_zhvi_2022, avg_zhvi_2023))

## load housing units data
housing_units <- read_csv('data/processed_data/population_housing_units_by_zip.csv') %>%
  select(zip_code, starts_with('housing_units'))

## join house prices with housing units
zhvi_with_units <- zhvi %>%
  inner_join(housing_units) ## we lose one ZIP code in Brick, NJ- not very important

## define df to iterate over
numeric_cols_housing <- zhvi_with_units %>%
  select(-zip_code)

## create empty output df without columns for housing units (as those are just weights)
output_housing <- data.frame(matrix(nrow = nrow(numeric_cols_housing), ncol = ncol(numeric_cols_housing)/2))
new_cols_housing <- list()

## calculate percentile rank for each column and append to output, create new column name for each percentile column 
for (i in seq_along(output_housing)) {
  name <- colnames(numeric_cols_housing)[i]
  new_name <- paste(name, "_pct", sep = "")
  new_cols_housing <- append(new_cols_housing, new_name)
  weights <- numeric_cols_housing[[i + 11]]
  rank <- wtd.rank(numeric_cols_housing[[i]], weights = weights) 
  total <- sum(weights, na.rm = TRUE)
  pct <- rank/total
  output_housing[[i]] <- pct
}

## append column names to output df
colnames(output_housing) <- new_cols_housing

#### Create weighted percentile ranks for IRS data
## load IRS data
irs <- read_csv('data/processed_data/irs_data_clean.csv')

## add in missing ZIP
## a new ZIP in Williamsburg is missing from the IRS data
## I will assign it the same values as the ZIP it was split from
missing_zip_irs <- irs %>%
  filter(zipcode == '11211') %>%
  mutate(zipcode = if_else(zipcode == '11211', '11249', NA))

irs_final <- rbind(irs, missing_zip_irs)

## load population data
population <- read_csv('data/processed_data/population_housing_units_by_zip.csv') %>%
  select(zip_code, starts_with('total_pop')) %>%
  select(-total_pop_2021) ## income data only goes up to 2020

## join incomes with population
agi_with_pop <- irs_final %>%
  inner_join(population, by = c('zipcode' = 'zip_code'))

## define df to iterate over
numeric_cols_income <- agi_with_pop %>%
  select(-zipcode)

## create empty output df without columns for population (as those are just weights)
output_income <- data.frame(matrix(nrow = nrow(numeric_cols_income), ncol = ncol(numeric_cols_income)/2))
new_cols_income <- list()

## calculate percentile rank for each column and append to output, create new column name for each percentile column 
for (i in seq_along(output_income)) {
  name <- colnames(numeric_cols_income)[i]
  new_name <- paste(name, "_pct", sep = "")
  new_cols_income <- append(new_cols_income, new_name)
  weights <- numeric_cols_income[[i + 10]]
  rank <- wtd.rank(numeric_cols_income[[i]], weights = weights) 
  total <- sum(weights, na.rm = TRUE)
  pct <- rank/total
  output_income[[i]] <- pct
}

## append column names to output df
colnames(output_income) <- new_cols_income

## combine all data back together
combined_data <- cbind(zhvi_with_units, agi_with_pop, output_housing, output_income) %>%
  select(-c(starts_with('housing_units'), starts_with('total_pop'), zipcode)) %>%
  write_csv('data/processed_data/weigted_percentile_ranks.csv')
  