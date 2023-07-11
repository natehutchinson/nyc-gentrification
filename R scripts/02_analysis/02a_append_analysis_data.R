#######################################################################################################
## File name: Append analysis data
## Description: Add data from NYC Open Data & ZORI to analysis df to analyze effects of gentrification 
## Created: July 11, 2023
## Last edited: July 11, 2023
#######################################################################################################

## Load libraries
library(tidyverse)
library(janitor)

## load data
# analysis df
df <- read_csv('data/processed_data/final_analysis_df.csv')
# zori (zillow rent data)
zori <- read_csv('data/processed_data/nyc_zori.csv') %>%
  mutate(zip_code = as.character(zip_code))
# acs gross rent 
acs_rent <- read_csv('data/processed_data/acs_gross_rent_by_zip.csv')
# evictions
evictions <- read_csv('data/raw_data/nyc_open_data/evictions.csv') %>%
  mutate(zip_code = as.character(zip_code))
# code violations
code_violations <- read_csv('data/raw_data/nyc_open_data/code_violations.csv') %>%
  mutate(year = year(year)) %>%
  group_by(zip, year, class) %>%
  summarise(count = sum(count)) %>%
  pivot_wider(names_from = class, values_from = count, names_prefix = 'violations_') %>%
  clean_names() %>%
  mutate(total_violations = violations_a + violations_b + violations_c,
         zip = as.character(zip)) %>%
  rename(zip_code = zip)
# dob complaints
dob_complaints <- read_csv('data/raw_data/nyc_open_data/dob_311_calls.csv') %>%
  mutate(zip_code = as.character(zip_code))
# permits
permits <- read_csv('data/raw_data/nyc_open_data/housing_permits.csv') %>%
  rename(year = permit_year) %>%
  mutate(zip_code = as.character(zip_code))
# 311 calls to nypd
nypd_calls <- read_csv('data/raw_data/nyc_open_data/nypd_311_calls.csv') %>%
  mutate(zip_code = as.character(zip_code))

## filter for NYC ZIPs  
nyc <- df %>%
  filter(nyc_flag == 1)

## append analysis data
nyc_df <- nyc %>%
  ## zillow rent data
  left_join(zori, by = c("zip_code", "year")) %>%
  ## acs rent data
  left_join(acs_rent, by = c("zip_code", "year")) %>%
  ## evictions 
  left_join(evictions, by = c("zip_code", "year")) %>%
  ## code violations
  left_join(code_violations, by = c("zip_code", "year")) %>%
  ## dob complaints
  left_join(dob_complaints, by = c("zip_code", "year")) %>%
  ## permits
  left_join(permits, by = c("zip_code", "year")) %>%
  ## nypd calls
  left_join(nypd_calls, by = c("zip_code", "year")) %>%
  ## save output
  write_csv('data/processed_data/analysis_df_additional_cols.csv')

