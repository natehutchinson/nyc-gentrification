##############################################################################################################
## File name: Create gentrification index- second attempt
## Description: Create a continuous gentrification index and map it 
## Created: July 13, 2023
## Last edited: July 14, 2023
##############################################################################################################

## Load libraries
library(tidyverse)
library(janitor)
library(ggplot2)
library(scales)
library(sf)

## load data
df_classified <- read_csv('data/processed_data/classified_zips.csv') %>%
  mutate(zip_code = as.character(zip_code))

zips_to_graph <- df_classified %>%
  filter(zip_type %in% c('Already gentrifying', 'Newly gentrifying', 
                         'Income below 25th percentile')) %>%
  group_by(zip_code) %>%
  summarise(max_income = max(agi_pct, na.rm = TRUE)) %>%
  filter(max_income <= 0.25) %>%
  select(zip_code) %>%
  mutate(graph = 1)

## map
data_for_maps <- df_classified %>%
  filter(year <= 2019)%>% 
  left_join(zips_to_graph) %>%
  mutate(percentile_diff = if_else(graph == 1, percentile_diff, NA),
         percentile_diff = if_else(percentile_diff < 0, 0, percentile_diff))

zctas <- read_sf('data/raw_data/cb_2019_us_zcta510_500k/cb_2019_us_zcta510_500k.shp') %>%
  clean_names() %>%
  rename(zip_code = geoid10) %>%
  inner_join(data_for_maps) 

ggplot(zctas) +
  aes(fill = percentile_diff) + 
  geom_sf(color = "white", size = 0.05) +
  facet_wrap(~year) +
  theme_void() +
  scale_fill_viridis_c(option = "C", end = 0.9)

  


## use ZIPs below 25th percentile
## ensure that you only map zips that stay below that threshold the entire time
## figure out a good scale to use

## use this color palette throughout    
# show_col(viridis_pal(option = "C")(6))
  




