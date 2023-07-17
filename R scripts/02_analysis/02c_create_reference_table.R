#######################################################################################################
## File name: Create reference table
## Description: Summarize analysis variables for different zip types 
## Created: July 12, 2023
## Last edited: July 12, 2023
#######################################################################################################

## Load libraries
library(tidyverse)
library(janitor)

## load data
df_classified <- read_csv('data/processed_data/classified_zips.csv') %>%
  mutate(zip_code = as.character(zip_code))

## summarize analysis variables for below 25th percentile zips
below_25 <- df_classified %>%
  filter(zip_type == "Income below 25th percentile") %>%
  group_by(year, zip_type) %>%
  summarise(avg_rent = weighted.mean(avg_rent, housing_units, na.rm = TRUE),
            evictions = weighted.mean(evictions, housing_units, na.rm = TRUE),
            eviction_filings = weighted.mean(eviction_filings, housing_units, na.rm = TRUE),
            total_violations = weighted.mean(total_violations, housing_units, na.rm = TRUE),
            violations_c = weighted.mean(violations_c, housing_units, na.rm = TRUE),
            dob_calls_311 = weighted.mean(dob_calls_311, housing_units, na.rm = TRUE),
            permits_alteration = weighted.mean(permits_alteration, housing_units, na.rm = TRUE),
            permits_demolition = weighted.mean(permits_demolition, housing_units, na.rm = TRUE),
            nypd_calls_311 = weighted.mean(nypd_calls_311, total_pop, na.rm = TRUE),
            arrests = weighted.mean(arrests, total_pop, na.rm = TRUE))

## summarize analysis variables for below 50th percentile zips
below_50 <- df_classified %>%
  filter(zip_type == "Income below 50th percentile") %>%
  group_by(year, zip_type) %>%
  summarise(avg_rent = weighted.mean(avg_rent, housing_units, na.rm = TRUE),
            evictions = weighted.mean(evictions, housing_units, na.rm = TRUE),
            eviction_filings = weighted.mean(eviction_filings, housing_units, na.rm = TRUE),
            total_violations = weighted.mean(total_violations, housing_units, na.rm = TRUE),
            violations_c = weighted.mean(violations_c, housing_units, na.rm = TRUE),
            dob_calls_311 = weighted.mean(dob_calls_311, housing_units, na.rm = TRUE),
            permits_alteration = weighted.mean(permits_alteration, housing_units, na.rm = TRUE),
            permits_demolition = weighted.mean(permits_demolition, housing_units, na.rm = TRUE),
            nypd_calls_311 = weighted.mean(nypd_calls_311, total_pop, na.rm = TRUE),
            arrests = weighted.mean(arrests, total_pop, na.rm = TRUE))

## summarize analysis variables for below 50th percentile zips
high_income <- df_classified %>%
  filter(zip_type == "High income") %>%
  group_by(year, zip_type) %>%
  summarise(avg_rent = weighted.mean(avg_rent, housing_units, na.rm = TRUE),
            evictions = weighted.mean(evictions, housing_units, na.rm = TRUE),
            eviction_filings = weighted.mean(eviction_filings, housing_units, na.rm = TRUE),
            total_violations = weighted.mean(total_violations, housing_units, na.rm = TRUE),
            violations_c = weighted.mean(violations_c, housing_units, na.rm = TRUE),
            dob_calls_311 = weighted.mean(dob_calls_311, housing_units, na.rm = TRUE),
            permits_alteration = weighted.mean(permits_alteration, housing_units, na.rm = TRUE),
            permits_demolition = weighted.mean(permits_demolition, housing_units, na.rm = TRUE),
            nypd_calls_311 = weighted.mean(nypd_calls_311, total_pop, na.rm = TRUE),
            arrests = weighted.mean(arrests, total_pop, na.rm = TRUE))

## combine summaries with individual gentrifying zips
gentrifying_zips <- df_classified %>%
  filter(zip_type == "Newly gentrifying") %>%
  mutate(zip_type = zip_code) %>%
  select(colnames(high_income))

gentrification_year <- df_classified %>%
  filter(gentrification_signal == 1) %>%
  select(zip_code, gentrification_year = year) 
  

reference_table <- rbind(gentrifying_zips, below_25, below_50, high_income) %>%
  left_join(gentrification_year, by = c('zip_type' = 'zip_code')) %>%
  relocate(gentrification_year, .after = zip_type) %>%
  write_csv('data/processed_data/reference_table.csv')
