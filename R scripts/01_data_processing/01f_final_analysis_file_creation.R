###################################################################################################
## File name: Final analysis file creation
## Description: Calculate percentile differences and add relevant flags
## Created: July 7, 2023
## Last edited: July 7, 2023
###################################################################################################

## Load libraries
library(tidyverse)
library(janitor)

## load data
percentile_ranks <- read_csv('data/processed_data/weigted_percentile_ranks.csv')

#### calculate differences between percentiles for each ZIP and year
## create df of cols to be used for differences (drop 2021 because not in IRS data)
diff_cols <- percentile_ranks %>%
  select(ends_with('pct'), -avg_zhvi_2021_pct)

## create empty df for differences
diff_output <- data.frame(matrix(nrow = nrow(percentile_ranks), ncol = 10))
diff_name <- list()
year <- 2011 ## initialize year

## calculate differences in percentile ranks for each year
for (i in 1:10) {
  name <- paste("diff", year, sep = "_")
  diff_name <- append(diff_name, name)
  year <- year + 1
  diff_output[[i]] <- diff_cols[[i]] - diff_cols[[i + 10]]
}

## append column names to difference df
colnames(diff_output) <- diff_name

## add differences to original df
combined_diffs <- cbind(percentile_ranks, diff_output) %>%
  write_csv('data/processed_data/weighted_percentile_diffs.csv')

## add demographics
zip_data_ready <- read_csv('data/processed_data/population_housing_units_by_zip.csv') %>%
  select(-total_pop_2021)

combined_diffs_with_acs <- combined_diffs %>%
  left_join(zip_data_ready)

#### pivot data to long format
## pivot zhvi
zhvi_long <- combined_diffs_with_acs %>%
  select(zip_code, avg_zhvi_2011:avg_zhvi_2021) %>%
  pivot_longer(-zip_code, names_to = 'year', values_to = 'avg_zhvi', names_prefix = 'avg_zhvi_') 

## pivot housing units
housing_units_long <- combined_diffs_with_acs %>%
  select(zip_code, housing_units_2011:housing_units_2021) %>%
  pivot_longer(-zip_code, names_to = 'year', values_to = 'housing_units', names_prefix = 'housing_units_') 

## pivot agi
agi_long <- combined_diffs_with_acs %>%
  select(zip_code, agi11:agi20) %>%
  pivot_longer(-zip_code, names_to = 'year', values_to = 'agi', names_prefix = 'agi') %>%
  mutate(year = paste('20', year, sep = ""))

## pivot population
total_pop_long <- combined_diffs_with_acs %>%
  select(zip_code, total_pop_2011:total_pop_2020) %>%
  pivot_longer(-zip_code, names_to = 'year', values_to = 'total_pop', names_prefix = 'total_pop_')

## pivot zhvi percentile
zhvi_pct_long <- combined_diffs_with_acs %>%
  select(zip_code, avg_zhvi_2011_pct:avg_zhvi_2021_pct) %>%
  pivot_longer(-zip_code, names_to = 'year', values_to = 'avg_zhvi_pct', names_prefix = 'avg_zhvi_') %>%
  mutate(year = str_sub(year, 1, 4))

## pivot agi percentile 
agi_pct_long <- combined_diffs_with_acs %>%
  select(zip_code, agi11_pct:agi20_pct) %>%
  pivot_longer(-zip_code, names_to = 'year', values_to = 'agi_pct', names_prefix = 'agi') %>%
  mutate(year = paste('20', str_sub(year, 1, 2), sep = ""))

## pivot percentile differences
diffs_long <- combined_diffs_with_acs %>%
  select(zip_code, diff_2011:diff_2020) %>%
  pivot_longer(-zip_code, names_to = 'year', values_to = 'percentile_diff', names_prefix = 'diff_')

## create nyc flag
nyc_zips <- read.csv('data/processed_data/nyc_zips.csv', colClasses = c('zip_code' = 'character')) %>%
  mutate(nyc_flag = 1)

## create flag for inner third of MSA
nyc_metro_zips <- read.csv('data/processed_data/nyc_metro_zips.csv', colClasses = c('zip_code' = 'character'))

zip_distances <- read_csv('data/processed_data/geo_mapping_files/zip_distance_to_cbd.csv') %>%
  clean_names() %>%
  select(zip_code = id, distance = x10036) %>%
  inner_join(nyc_metro_zips) %>%
  group_by(zip_code) %>%
  summarise(distance = min(distance))

missing_zip <- zip_distances %>%
  filter(zip_code == '11211') %>%
  mutate(zip_code = if_else(zip_code == '11211', '11249', NA))

zip_distances <- rbind(zip_distances, missing_zip)

inner_third_flag <- zip_distances %>%
  mutate(dist_rank = rank(distance),
         dist_pct = dist_rank/length(distance),
         inner_third_flag = if_else(dist_pct <= 0.33, 1, 0)) %>%
  select(zip_code, inner_third_flag)

## join everything together
combined_diffs_long <- zhvi_long %>%
  left_join(housing_units_long, by = c('zip_code', 'year')) %>%
  left_join(agi_long, by = c('zip_code', 'year')) %>%
  left_join(total_pop_long, by = c('zip_code', 'year')) %>%
  left_join(zhvi_pct_long, by = c('zip_code', 'year')) %>%
  left_join(agi_pct_long, by = c('zip_code', 'year')) %>%
  left_join(diffs_long, by = c('zip_code', 'year')) %>%
  left_join(nyc_zips, by = 'zip_code') %>%
  left_join(inner_third_flag, by = 'zip_code') %>%
  ## create flags for NYC ZIPs, a difference of 25 percentile points, and ZIPs in the bottom 25 percentile of income
  mutate(nyc_flag = if_else(is.na(nyc_flag), 0, nyc_flag),
         gap_25_pct = if_else(percentile_diff >= 0.25, 1, 0),
         bottom_income_quartile = if_else(agi_pct <= 0.25, 1, 0)) %>%
  ## identify the first year that a ZIP has a 25 percentile point gap between home value and income
  group_by(zip_code) %>%
  mutate(gap_years = cumsum(gap_25_pct),
         first_gap = if_else(gap_years == 1, 
                             if_else(lag(gap_years == 1), 0, 1)
                             , 0)) %>%
  ungroup() %>%
  mutate(gentrification_signal = if_else((first_gap == 1 & bottom_income_quartile == 1 & inner_third_flag == 1), 1, 0)) %>%
  select(zip_code, nyc_flag, year, avg_zhvi:gentrification_signal) %>%
  write.csv('data/processed_data/final_analysis_df.csv', row.names = FALSE)
  




