###################################################################################################
## File name: Census tract ZIP mapping
## Description: Create mapping between census tracts and ZCTAs and summarize ACS data at ZIP level
## Created: July 6, 2023
## Last edited: July 11, 2023
###################################################################################################

## Load libraries
library(tidyverse)
library(janitor)

## load datasets
nj <- read_csv('data/processed_data/geo_mapping_files/nj_tract_zcta_mapping.csv') %>%
  clean_names() %>%
  select(geoid, zip_code = m_zcta5ce10)

ny <- read_csv('data/processed_data/geo_mapping_files/ny_tract_zcta_mapping.csv') %>%
  clean_names() %>%
  select(geoid, zip_code = m_zcta5ce10)

pa <- read_csv('data/processed_data/geo_mapping_files/pa_tract_zcta_mapping.csv') %>%
  clean_names() %>%
  select(geoid, zip_code = m_zcta5ce10)

## combine into one df
all_states <- rbind(nj, ny, pa)

## filter for nyc metro
nyc_metro_zips <- read_csv('data/processed_data/nyc_metro_zips.csv')

census_tract_zip_mapping <- all_states %>%
  inner_join(nyc_metro_zips) %>%
  write_csv('data/processed_data/geo_mapping_files/census_tract_zcta_mapping.csv')

#### summarize ACS data at ZIP level
## import ACS data
acs <- read_csv('data/processed_data/census_data_ready.csv')

## append ZIPs
acs_with_zips <- acs %>%
  left_join(census_tract_zip_mapping) %>%
  filter(!is.na(zip_code)) ## missing zip code means not in nyc metro area or unmapped zip

## sumarize at ZIP-year level
mapped_zips_acs_data <- acs_with_zips %>%
  mutate(total_rent = housing_units * median_gross_rent) %>%
  group_by(zip_code, year) %>%
  summarise(total_pop = sum(total_pop), housing_units = sum(housing_units), 
            gross_rent_per_unit = sum(total_rent)/sum(housing_units)) 


#### get ACS data for zips not mapped by original process
## identify zips that are not mapped to any tract
mapped_zips <- census_tract_zip_mapping %>%
  select(zip_code) %>%
  distinct()

unmapped_zips <- nyc_metro_zips %>%
  filter(!(zip_code %in% mapped_zips$zip_code))

## load one-to-many zip-tract mapping
nj_one_to_many <- read_csv('data/processed_data/geo_mapping_files/nj_zip_tract_1_to_many.csv') %>%
  clean_names() %>%
  select(zip_code = zcta5ce10, geoid)

ny_one_to_many <- read_csv('data/processed_data/geo_mapping_files/ny_zip_tract_1_to_many.csv') %>%
  clean_names() %>%
  select(zip_code = zcta5ce10, geoid)

pa_one_to_many <- read_csv('data/processed_data/geo_mapping_files/pa_zip_tract_1_to_many.csv') %>%
  clean_names() %>%
  select(zip_code = zcta5ce10, geoid)

## combine into one df
all_states_one_to_many <- rbind(nj_one_to_many, ny_one_to_many, pa_one_to_many) %>%
  filter(!is.na(geoid))

## filter for unmapped zips
unmapped_zips_one_to_many <- all_states_one_to_many %>%
  inner_join(unmapped_zips)

## join with acs data
acs_unmapped_zips <- acs %>%
  inner_join(unmapped_zips_one_to_many, relationship = 'many-to-many')

## summarize at ZIP-year level
unmapped_zips_acs_data <- acs_unmapped_zips %>%
  mutate(total_rent = housing_units * median_gross_rent) %>%
  group_by(zip_code, year) %>%
  summarise(total_pop = mean(total_pop), housing_units = mean(housing_units),
            gross_rent_per_unit = mean(total_rent)/mean(housing_units)) 

#### combine mapped and unmapped zips
## append mapped and unmapped zips and pivot wide
acs_data_zip_level <- rbind(mapped_zips_acs_data, unmapped_zips_acs_data) %>%
  select(-gross_rent_per_unit) %>%
  mutate(total_pop = round(total_pop),
         housing_units = round(housing_units)) %>%
  pivot_wider(names_from = year, values_from = c(total_pop, housing_units))

## save rent data for later
acs_gross_rent <- rbind(mapped_zips_acs_data, unmapped_zips_acs_data) %>%
  select(zip_code, year, gross_rent_per_unit) %>%
  write_csv('data/processed_data/acs_gross_rent_by_zip.csv')

## ensure that all years are present for every zip
total_pop_all_years <- acs_data_zip_level %>%
  select(zip_code, starts_with('total_pop')) %>%
  pivot_longer(-zip_code, names_to = 'year', values_to = 'total_pop', names_prefix = 'total_pop_') 

housing_units_all_years <- acs_data_zip_level %>%
  select(zip_code, starts_with('housing')) %>%
  pivot_longer(-zip_code, names_to = 'year', values_to = 'housing_units', names_prefix = 'housing_units_') %>%
  mutate(housing_units = if_else(zip_code == '10577', 855, housing_units)) 
  ## census data had housing_units = 0 for this zip, so I filled in values with data from https://www.unitedstateszipcodes.org/10577/ 
  
zip_data_all <- total_pop_all_years %>%
  left_join(housing_units_all_years) %>%
  group_by(zip_code) %>%
  fill(total_pop, .direction = 'downup') %>% ## fill NAs with previous year's data or next year's data
  fill(housing_units, .direction = 'downup') %>%
  pivot_wider(names_from = year, values_from = c(total_pop, housing_units)) 

## add in missing ZIP
## a new ZIP in Williamsburg is missing from the ZCTA shapefile I used
## I will assign it the same values as the ZIP it was split from
missing_zip <- zip_data_all %>%
  filter(zip_code == '11211') %>%
  mutate(zip_code = if_else(zip_code == '11211', '11249', NA))

## finalize zip-level ACS data
zip_data_ready <- rbind(zip_data_all, missing_zip) %>%
  write_csv('data/processed_data/population_housing_units_by_zip.csv')
