#########################################################################
## File name: IRS data exploration
## Description: Import and explore the IRS Tax Statistics data
## Created: June 21, 2023
## Last edited: June 28, 2023
#########################################################################

## The IRS data is stored in yearly csv files going back to 2011
## Prior to that it is stored at the state-year level
## There does not appear to be an API that allows us to pull this data, so I downloaded the yearly files going back to 2011

## Load libraries
library(tidyverse)
library(janitor)
library(skimr)
library(stringr)
library(sf)
library(scales)

## read in files from directory
files <- list.files('data/irs_data')

## define empty dataframe to append each year to
irs_data = data.frame(matrix(nrow = 0, ncol = 3))
columns = c("zipcode", "avg_agi", "year")
colnames(irs_data) = columns

## Loop over each file in the directory, summarize it, and append to our dataframe
for (file in files) {
  yr = substr(file,0,2)
  data <- read_csv(paste('data/irs_data/', file, sep = ""), col_types = cols(zipcode = "c")) %>%
    clean_names() %>%
    mutate(zipcode = str_pad(zipcode, width = 5, pad = "0")) %>% ## ensure all ZIPs have 5 digits
    filter(state %in% c('NJ', 'NY', 'PA'), ## only states in NYC metro area
           zipcode != '00000') %>%
    select(zipcode, agi_stub, returns = n1, agi = a00100) %>% ## rename variables to be more intuitive
    
    ## calculate yearly average AGI for each ZIP 
    mutate(total_agi = returns * agi) %>% 
    group_by(zipcode) %>%
    summarise(zip_total_agi = sum(total_agi),
              zip_total_returns = sum(returns)) %>%
    mutate(avg_agi = zip_total_agi/zip_total_returns) %>%
    
    ## output summary table of average AGI by ZIP
    select(zipcode, avg_agi) %>%
    
    ## add variable to be used when pivoting data
    mutate(year = paste("agi", yr, sep = ""))
  
  ## append each summary table to our dataframe
  irs_data = rbind(irs_data, data)
}

## pivot data
irs_data_wide <- pivot_wider(irs_data, id_cols = zipcode, names_from = year, values_from = avg_agi) %>%
  write_csv('data/irs_data/irs_data.csv')


skim(irs_data_wide) 

## about 10% missing values for each year
## note that not all of these ZIPs are in the NYC MSA, and those with missing values are likely to be more rural
## of missing values will probably drop after joining with MSA ZIPs
## for missing values that remain, probably best to impute

#### MAP AGI data
### get geometry for nyc metro area
## load nyc metro zips
nyc_metro_zips <- read_csv('data/nyc_metro_zips.csv')

## combine IRS data w/ nyc metro zips and shapefile
zctas <- read_sf('data/cb_2019_us_zcta510_500k/cb_2019_us_zcta510_500k.shp') %>%
  clean_names() %>%
  rename(zip_code = geoid10) %>%
  inner_join(irs_data_wide, by = c('zip_code' = 'zipcode')) %>%
  inner_join(nyc_metro_zips, by = 'zip_code')

### plot AGI for NYC metro
ggplot(zctas) +
  aes(fill = agi20) + 
  geom_sf(color = "white", size = 0.05) +
  theme_void() +
  scale_fill_viridis_c(labels = scales::comma)  

## load nyc zips
nyc_zips <- read_csv('data/nyc_zips.csv')

## filter for NYC ZIPs only
zctas_nyc <- zctas %>%
  inner_join(nyc_zips, by = 'zip_code')

## Plot NYC AGIs
ggplot(zctas_nyc) +
  aes(fill = agi20) + 
  geom_sf(color = "white", size = 0.05) +
  theme_void() +
  scale_fill_viridis_c(labels = scales::comma)


 