##################################################################################
## File name: ACS Download
## Description: Pull ACS data by census tract for # of housing units and population
## Created: June 30, 2023
## Last edited: June 30, 2023
##################################################################################

## import libraries
library(tidyverse)
library(janitor)
library(tidycensus)

## replace with your own key
census_api_key("8524147f6edf7fe4b7c85681397fe5acd6993d62"
               #               , install = TRUE 
)


#### pull acs data for each year and combine into one df
## create blank df
census_data_raw <- data.frame(matrix(nrow = 0, ncol = 6))
columns <- c("GEOID", "NAME", "variable", "estimate", "moe", "year")
colnames(census_data_raw) <- columns

## pull estimates for each year in the data
for (i in 2011:2021) {
  df <- get_acs(
    geography = "tract",
    variables = c(
      housing_units = "B25001_001",
      total_pop = "B01003_001"
    ),
    state =  c("New York" , "New Jersey", "Pennsylvania"), 
    year = i,
    survey = "acs5"
  ) %>%
    mutate(year = i)
  census_data_raw <- rbind(census_data_raw, df)
}

## pivot to get data at tract-year level
census_data_ready <- census_data_raw %>%
  select(-moe) %>%
  clean_names() %>%
  pivot_wider(id_cols = c(geoid, name, year), names_from = variable, values_from = estimate) %>%
  write_csv('data/processed_data/census_data_ready.csv')










