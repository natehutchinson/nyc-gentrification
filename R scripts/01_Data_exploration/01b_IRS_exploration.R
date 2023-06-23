#########################################################################
## File name: IRS data exploration
## Description: Import and explore the IRS Tax Statistics data
## Last edited: June 21, 2023
#########################################################################

## The IRS data is stored in yearly csv files going back to 2011
## Prior to that it is stored at the state-year level
## There does not appear to be an API that allows us to pull this data, so I downloaded the yearly files going back to 2011

## Load libraries
library(tidyverse)
library(janitor)
library(skimr)
library(stringr)

## read in files from directory
files = list.files('data/IRS data')

## define empty dataframe to append each year to
irs_data = data.frame(matrix(nrow = 0, ncol = 3))
columns = c("zipcode", "avg_agi", "year")
colnames(irs_data) = columns

## Loop over each file in the directory, summarize it, and append to our dataframe
for (file in files) {
  yr = substr(file,0,2)
  data <- read_csv(paste('data/IRS data/', file, sep = ""), col_types = cols(zipcode = "c")) %>%
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
  write_csv('data/IRS data/irs_data.csv')


skim(irs_data_wide) 

## about 10% missing values for each year
## note that not all of these ZIPs are in the NYC MSA, and those with missing values are likely to be more rural
## of missing values will probably drop after joining with MSA ZIPs
## for missing values that remain, probably best to impute
  
 