#########################################################################
## File name: DOF rolling sales data exploration
## Description: Import and explore the DOF rolling sales data
## Created: June 26, 2023
## Last edited: June 28, 2023
#########################################################################

## Load libraries
library(tidyverse)
library(janitor)
library(skimr)
library(stringr)
library(sf)
library(scales)
options(scipen = 999)

#### Import data
## Bulk DOF file from nycdb, which has sales from 2003-2020
dof_bulk <- read_csv('data/raw_data/dof_raw_data/dof_annual_sales.csv') %>%
  clean_names()

## Import data from 2021-2022 downloaded from DOF website
## read in files from directory
files <- list.files('data/dof_2021_2022')

dof_additional <- read_csv('data/raw_data/dof_raw_data/dof_2021_2022/2021_bronx.csv') %>%
  clean_names()

for (file in files[2:length(files)]) {
  temp <- read_csv(paste('data/raw_data/dof_raw_data/dof_2021_2022/', file, sep = "")) %>% clean_names
  dof_additional = rbind(dof_additional, temp)
}

## filter for multifamily residential buildings
mfr <- dof_bulk %>%
  ## keep only multi-family buildings and drop any building with commercial space
  filter(substring(building_class_category,0,2) %in% c('02','03','07','08','14'), 
         commercial_units == 0) %>%
  ## keep only relevant columns
  select(building_class_category, address, zip_code, residential_units, sale_price, year)

## do same thing for additional data
mfr_additional <- dof_additional %>%
  ## create column for year
  mutate(year = paste('20', str_sub(sale_date, -2), sep = "")) %>%
  ## keep only multi-family buildings and drop any building with commercial space
  filter(str_sub(building_class_category,0,2) %in% c('02','03','07','08','14'), 
         commercial_units == 0) %>%
  ## keep only relevant columns
  select(building_class_category, address, zip_code, residential_units, sale_price, year)

## append two datasets
mfr_all <- rbind(mfr, mfr_additional)
  
  
## check for potentially problematic values        
skim(mfr_all)

unique(mfr_all$zip_code)
nrow(mfr_all[mfr_all$zip_code == "00000", ]) ## 53 records with ZIP = "00000" --> drop these

nrow(mfr_all[mfr_all$residential_units == 0, ]) ## 6,510 (1.3%) have 0 residential units --> keep, but exclude if making calculations based on building size

nrow(mfr_all[mfr_all$sale_price == 0, ]) ## 197,035 (40.2% of buildings have sale price = 0) --> have to drop these

nrow(mfr_all[mfr_all$sale_price == 1, ])
nrow(mfr_all[mfr_all$sale_price == 2, ])
nrow(mfr_all[mfr_all$sale_price == 3, ])
nrow(mfr_all[mfr_all$sale_price == 4, ])

look <- mfr_all %>%
  filter(sale_price > 0,
         sale_price < 1000)

non_zero_sales <- mfr_all %>%
  filter(#sale_price > 0,
         sale_price < 200000) %>%
  mutate(sales_bkt = case_when(
    sale_price == 0 ~ "0",
    between(sale_price, 1, 10) ~ "a.1-10",
    between(sale_price, 11, 100) ~ "b.11-100",
    between(sale_price, 101, 1000) ~ "c.101-1000",
    between(sale_price, 1001, 10000) ~ "d.1001-10000",
    between(sale_price, 10001, 100000) ~ "e.10001-100000",
    between(sale_price, 100001, 200000) ~ "f.100001-200000",
  ))

summary_tbl <- non_zero_sales %>%
  group_by(sales_bkt) %>%
  summarise(count = n())

hist(non_zero_sales$sale_price, breaks = 30)


## what is the minimum sale price that I should consider "legitimate"?

## summarize yearly sales by ZIP
sales_by_zip <- mfr_all %>%
  filter(sale_price > 100,
         zip_code != '00000') %>%
  group_by(zip_code, year) %>%
  summarise(median_sale_price = median(sale_price), count_sales = n())



## some of these ZIPs may not have enough sales in particular years to be usable
hist(sales_by_zip$count_sales, breaks = 100)
nrow(sales_by_zip[sales_by_zip$count_sales <= 30, ])/length(sales_by_zip$count_sales)
# we lose 31% of observations (ZIP-years) if we require at least 30 sales w/ price > 100
nrow(sales_by_zip[sales_by_zip$count_sales <= 20, ])/length(sales_by_zip$count_sales)
# we lose 23% of observations (ZIP-years) if we require at least 30 sales w/ price > 100

## pivot sales by zip to wide
sales_by_zip_wide <- sales_by_zip %>%
  pivot_wider(names_from = year, values_from = c(median_sale_price, count_sales))

#### map building sales data
## get geometry for nyc 
zctas <- read_sf('data/raw_data/cb_2019_us_zcta510_500k/cb_2019_us_zcta510_500k.shp') %>%
  clean_names() %>%
  rename(zip_code = geoid10) %>%
  inner_join(sales_by_zip_wide, by = 'zip_code')

## map building sale prices
ggplot(zctas) +
  aes(fill = avg_sale_price_2013) + 
  geom_sf(color = "white", size = 0.05) +
  theme_void() +
  scale_fill_viridis_c(labels = scales::comma)


hist(sales_by_zip_wide$median_sale_price_2022, breaks = 100)

temp <- sales_by_zip_wide %>%
  filter(count_sales_2022 >= 30)

hist(temp$median_sale_price_2022, breaks = 100)

