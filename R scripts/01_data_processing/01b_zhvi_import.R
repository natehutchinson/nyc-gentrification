##################################################################################
## File name: ZHVI import
## Description: Import and clean Zillow Home Value Index data, prepare for merge
## Created: June 29, 2023
## Last edited: June 29, 2023
##################################################################################

## import libraries
library(tidyverse)
library(janitor)
library(stringr)

## Import data (replace this with an API call)
zhvi <- read_csv('data/raw_data/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv') %>%
  clean_names()

## Filter on NYC metro area
nyc_metro <- zhvi %>%
  filter(metro == 'New York-Newark-Jersey City, NY-NJ-PA') # 830 ZIPs

## Convert to yearly average
nyc_metro_yearly <- nyc_metro %>%
  select(-c(region_id, size_rank, region_type, state_name, state, city, metro, county_name)) %>%
  pivot_longer(-c(region_name)) %>%
  mutate(year = paste("20", str_sub(name, -2), sep = "")) %>%
  group_by(region_name, year) %>%
  summarise(avg_zhvi = mean(value, na.rm = TRUE)) %>%
  rename(zip_code = region_name) %>%
  mutate(zip_code = str_pad(zip_code, 5, pad = "0")) %>%
  pivot_wider(names_from = year, values_from = avg_zhvi, names_prefix = 'avg_zhvi_') %>%
  write_csv('data/processed_data/nyc_metro_zhvi.csv')

## define list of zips for nyc metro area
nyc_metro_zips <- nyc_metro_yearly %>%
  select(zip_code) %>%
  write_csv('data/processed_data/nyc_metro_zips.csv')

## get list of zips for nyc
nyc_zips <- nyc_metro %>%
  filter(city == "New York") %>%
  select(zip_code = region_name) %>%
  write_csv('data/processed_data/nyc_zips.csv')
