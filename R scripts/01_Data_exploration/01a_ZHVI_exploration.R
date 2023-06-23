#########################################################################
## File name: ZHVI exploration
## Description: Import and explore the Zillow Home Value Index data
## Last edited: June 20, 2023
#########################################################################

## Load libraries
library(tidyverse)
library(janitor)
library(skimr)

## Import data (replace this with an API call)
zhvi <- read_csv('data/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv') %>%
  clean_names()

## Filter on NYC metro area
# 830 ZIPs
nyc_metro <- zhvi %>%
  filter(metro == 'New York-Newark-Jersey City, NY-NJ-PA')

skim(nyc_metro)

## Some ZIPs have missing values at the beginning of the series, then have consistent data later
## Others have small gaps, which may be due to boundary changes
## Will need to determine the best way to create yearly averages

find_gaps <- nyc_metro %>%
  select(-c(region_id, size_rank, region_type, state_name, state, city, metro, county_name)) %>%
  pivot_longer(-c(region_name)) %>%
  group_by(region_name) %>%
  mutate(gap = if_else((is.na(value) & !is.na(lag(value))), 1, 0))

gap_zips <- find_gaps %>% 
  filter(gap == 1)

missing_start <- nyc_metro %>%
  filter(is.na(x2000_01_31))

length(missing_start$region_id) ## 58 ZIPs have missing data at the start of the series
length(gap_zips$region_name) ## 105 ZIPs have a gap in the data
length(unique(gap_zips$region_name)) ## 96 of these are unique (meaning 9 have multiple gaps)

## Some gaps are just one month, and appear to just be normal missing data
## Some gaps are a few months, and appear to be the result of changing ZIP boundaries
## Can we find out when ZIP boundaries changed?

## Need to convert to yearly averages:
## Make sure not to include missing values in averages
## Need to figure out how to average time series
## For boundary changes, will be an issue if gaps come in the middle of the year