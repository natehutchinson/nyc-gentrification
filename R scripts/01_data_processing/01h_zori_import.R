##################################################################################
## File name: ZORI import
## Description: Import and clean Zillow Observed Rent Index data, prepare for merge
## Created: July 10, 2023
## Last edited: July 10, 2023
##################################################################################

## Load libraries
library(tidyverse)
library(janitor)
library(stringr)

zori <- read_csv('data/raw_data/Zip_zori_sm_month.csv') %>%
  clean_names() %>%
  filter(city == "New York") %>%
  select(zip_code = region_name, starts_with('x')) %>%
  pivot_longer(-c(zip_code)) %>%
  mutate(year = str_sub(name, 2, 5)) %>%
  group_by(zip_code, year) %>%
  summarise(avg_rent = mean(value, na.rm = TRUE)) %>%
  filter(year != 2011) %>%
  write_csv('data/processed_data/nyc_zori.csv')
