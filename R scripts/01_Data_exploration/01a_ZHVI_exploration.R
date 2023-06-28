#########################################################################
## File name: ZHVI exploration
## Description: Import and explore the Zillow Home Value Index data
## Created: June 20, 2023
## Last edited: June 27, 2023
#########################################################################

## Load libraries
library(tidyverse)
library(janitor)
library(skimr)
library(stringr)
library(ggplot2)
library(sf)
library(scales)

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

## Convert to yearly average
nyc_metro_yearly <- nyc_metro %>%
  select(-c(region_id, size_rank, region_type, state_name, state, city, metro, county_name)) %>%
  pivot_longer(-c(region_name)) %>%
  mutate(year = str_sub(name, 2, 5)) %>%
  group_by(region_name, year) %>%
  summarise(avg_zhvi = mean(value, na.rm = TRUE)) %>%
  rename(zip_code = region_name) %>%
  pivot_wider(names_from = year, values_from = avg_zhvi, names_prefix = 'avg_zhvi_')


#### map ZHVI data
## define list of zips for nyc metro area
nyc_metro_zips <- nyc_metro_yearly %>%
  select(zip_code)

## get geometry for nyc metro area
zctas <- read_sf('data/cb_2019_us_zcta510_500k/cb_2019_us_zcta510_500k.shp') %>%
  clean_names() %>%
  rename(zip_code = geoid10) %>%
  inner_join(nyc_metro_yearly, by = 'zip_code')

nyc_metro_zips <- nyc_metro_yearly %>%
  select(zip_code) %>%
  write_csv('data/nyc_metro_zips.csv')

## map 2022 zhvi nyc metro area
ggplot(zctas) +
  aes(fill = avg_zhvi_2022) + 
  geom_sf(color = "white", size = 0.05) +
  theme_void() +
  scale_fill_viridis_c(labels = scales::comma)

## get list of zips for nyc
nyc_zips <- nyc_metro %>%
  filter(city == "New York") %>%
  select(zip_code = region_name) %>%
  write_csv('data/nyc_zips.csv')

## filter spatial df for nyc only
zctas_nyc <- zctas %>%
  inner_join(nyc_zips, by = 'zip_code')

## map nyc only 
ggplot(zctas_nyc) +
  aes(fill = avg_zhvi_2022) + 
  geom_sf(color = "white", size = 0.05) +
  theme_void() +
  scale_fill_viridis_c(labels = scales::comma)

## ZHVI data is missing for ZIP code 10038 (lower Manhattan) and a few others

