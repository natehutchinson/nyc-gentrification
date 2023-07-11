###################################################################################################
## File name: Download NYC Open data
## Description: Set up API calls to pull relevant data sets from NYC Open Data
## Created: July 10, 2023
## Last edited: July 11, 2023
###################################################################################################

## Load libraries
library(tidyverse)
library(janitor)
library(fs)
library(stringr)
library(geoclient)
library(lubridate)

## Download evictions data
url_evictions <- URLencode('https://data.cityofnewyork.us/resource/6z8x-wfk4.csv?$query= SELECT * LIMIT 100000')

evictions_data <- read_csv(url_evictions) %>%
  clean_names() %>%
  select(zip_code = eviction_zip, ejectment, executed_date) %>%
  mutate(year = year(executed_date)) %>%
  group_by(zip_code, year, ejectment) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = ejectment, values_from = count) %>%
  clean_names() %>%
  mutate(ejectment = if_else(is.na(ejectment), 0, ejectment)) %>%
  mutate(eviction_filings = ejectment + not_an_ejectment) %>%
  select(zip_code, year, evictions = ejectment, eviction_filings)

write_csv(evictions_data, 'data/raw_data/nyc_open_data/evictions.csv')

## Download code violations data
# create empty data frame
code_violations <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(code_violations) <- c("zip", "year", "class", "count")

# loop over year to keep API calls smaller
for (year in 2011:2022){
  query <- paste("https://data.cityofnewyork.us/resource/wvxf-dwi5.csv?$query= SELECT zip, class, date_trunc_y(inspectiondate) as year WHERE date_extract_y(inspectiondate)=" , year, " AND class!='I' LIMIT 2000000", sep = "")
  url <- URLencode(query)
  df <- read_csv(url) %>%
    clean_names() %>%
    group_by(zip, year, class) %>%
    summarise(count = n())
  code_violations <- rbind(code_violations, df)
}

write_csv(code_violations, 'data/raw_data/nyc_open_data/code_violations.csv')

## Housing code complaint problems
# create empty data frame
housing_complaints <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(housing_complaints) <- c("zip", "year", "count")

# loop over year to keep API calls smaller
for (year in 2011:2022){
  query <- paste("https://data.cityofnewyork.us/resource/uwyv-629c.csv?$query= SELECT zip, date_trunc_y(receiveddate) as year WHERE date_extract_y(receiveddate)=" , year, " LIMIT 2000000", sep = "")
  url <- URLencode(query)
  df <- read_csv(url) %>%
    clean_names() %>%
    group_by(zip, year) %>%
    summarise(count = n())
  housing_complaints <- rbind(housing_complaints, df)
}

write_csv(housing_complaints, 'data/raw_data/nyc_open_data/housing_complaints.csv')

## 311 calls to NYPD
# load data- API call took too long, data was downloaded directly from Open Data using 'View Data' function
cops_311 <- read_csv("data/raw_data/nyc_open_data/311_Service_Requests_nypd.csv") %>%
  clean_names() %>%
  mutate(year = str_sub(created_date, 7, 11)) %>%
  group_by(incident_zip, year) %>%
  summarise(count = n()) %>%
  select(zip_code = incident_zip, year, nypd_calls_311 = count) %>%
  filter(year >= 2011) %>%
  ## write csv
  write_csv("data/raw_data/nyc_open_data/nypd_311_calls.csv")

## arrests
arrests_url <- URLencode('https://data.cityofnewyork.us/resource/8h9b-rp9u.csv?$query= SELECT arrest_precinct, date_extract_y(arrest_date) as year  LIMIT 100000')

arrests <- read_csv(arrests_url) %>%
  clean_names()

# create empty data frame
arrests_by_precinct <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(arrests_by_precinct) <- c("arrest_precinct", "year", "count")

# loop over year to keep API calls smaller
for (year in 2011:2022){
  query <- paste("https://data.cityofnewyork.us/resource/8h9b-rp9u.csv?$query= SELECT arrest_precinct, date_extract_y(arrest_date) as year WHERE date_extract_y(arrest_date)=", year, " LIMIT 2000000", sep = "")
  url <- URLencode(query)
  df <- read_csv(url) %>%
    clean_names() %>%
    group_by(arrest_precinct, year) %>%
    summarise(count = n())
  arrests_by_precinct <- rbind(arrests_by_precinct, df)
}

write_csv(arrests_by_precinct, 'data/raw_data/nyc_open_data/arrests_by_precinct.csv')

## DOB complaints (from 311)
# load data
dob_calls <- read_csv("data/raw_data/nyc_open_data/311_Service_Requests_DOB.csv") %>%
  clean_names() %>%
  mutate(year = str_sub(created_date, 7, 11)) %>%
  group_by(incident_zip, year) %>%
  summarise(count = n()) %>%
  select(zip_code = incident_zip, year, dob_calls_311 = count) %>%
  filter(year >= 2011) %>%
  ## write csv
  write_csv("data/raw_data/nyc_open_data/dob_311_calls.csv")

## DCP Housing DB
# load data 
housing_db <- read_csv("data/raw_data/nyc_open_data/HousingDB_post2010.csv") %>%
  clean_names() %>%
  filter(job_type %in% c("Alteration", "Demolition"),
         permit_year >= 2011) %>%
  select(job_type, permit_year, bbl, address_num, address_st, boro) %>%
  mutate(borough = case_when(
    boro == 1 ~ "Manhattan",
    boro == 2 ~ "Bronx",
    boro == 3 ~ "Brooklyn",
    boro == 4 ~ "Queens",
    boro == 5 ~ "Staten Island"
  )) %>%
  mutate(address_search = paste(address_num, address_st, borough))

housing_db_with_zip <- housing_db %>%
  mutate(zip_code = geo_search(address_search)[["zipCode"]])

housing_db_summary <- housing_db_with_zip %>%
  group_by(zip_code, permit_year, job_type) %>%
  summarise(permits = n()) %>%
  pivot_wider(names_from = job_type, values_from = permits, names_prefix = 'permits_') %>%
  clean_names() %>%
  write_csv("data/raw_data/nyc_open_data/housing_permits.csv")

  

