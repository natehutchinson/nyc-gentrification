#######################################################################################################
## File name: Classify ZIP codes
## Description: Classify ZIPs as gentrifying, always low-income, always high-income, etc. 
## Created: July 11, 2023
## Last edited: July 11, 2023
#######################################################################################################

## Load libraries
library(tidyverse)
library(janitor)

## load data
df <- read_csv('data/processed_data/analysis_df_additional_cols.csv') %>%
  mutate(zip_code = as.character(zip_code))

#### classify zips
## gentrifying zips
gentrifying_zips <- df %>%
  filter(gentrification_signal == 1) %>%
  select(zip_code) %>%
  mutate(zip_type = "Newly gentrifying")

## identify zips that were already gentrifying at start of data period
already_gent_zips <- df %>%
  filter(year == 2011,
         gap_25_pct == 1) %>%
  select(zip_code) %>%
  mutate(zip_type = "Already gentrifying")

## identify zips that were never in the bottom income quartile
highest_income_pct <- df %>%
  group_by(zip_code) %>%
  summarise(max_income_pct = max(agi_pct, na.rm = TRUE)) %>%
  filter(!zip_code %in% (already_gent_zips$zip_code),
         !zip_code %in% (gentrifying_zips$zip_code)) %>%
  mutate(zip_type = case_when(
    max_income_pct <= 0.25 ~ "Income below 25th percentile",
    max_income_pct <= 0.50 ~ "Income below 50th percentile",
    TRUE ~ "High income"
  )) %>%
  select(-max_income_pct)

## combine into one file
zip_classifications <- rbind(already_gent_zips, gentrifying_zips, highest_income_pct)

## append ZIP classifications 
df_classified <- df %>%
  left_join(zip_classifications) %>%
  write_csv('data/processed_data/classified_zips.csv')


