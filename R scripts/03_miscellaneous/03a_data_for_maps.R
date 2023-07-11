##################################################################################
## File name: Data for maps
## Description: Create data for QGIS to show location of gentrifying ZIPs
## Created: June 29, 2023
## Last edited: June 29, 2023
##################################################################################

## Load libraries
library(tidyverse)
library(janitor)

## load data
df <- read_csv('data/processed_data/final_analysis_df.csv')


data_for_map <- df %>%
  group_by(zip_code) %>%
  summarise(gentrification_signal = max(gentrification_signal, na.rm = TRUE),
            inner_third = max(inner_third_flag, na.rm = TRUE)) %>%
  mutate(gentrification_signal = if_else(gentrification_signal == -Inf, 0, gentrification_signal),
         nyc_msa = 1) %>%
  write_csv('data/processed_data/data_for_qgis2.csv')


test <- df %>%
  select(zip_code, inner_third_flag) %>%
  distinct()
