##############################################################################################################
## File name: Create charts
## Description: Create charts of analysis variables for gentrifying zips against reference groups 
## Created: July 12, 2023
## Last edited: July 13, 2023
##############################################################################################################

## Load libraries
library(tidyverse)
library(janitor)
library(ggplot2)
library(scales)

#### trends for all analysis variables
## load data
df_classified <- read_csv('data/processed_data/classified_zips.csv') %>%
  mutate(zip_code = as.character(zip_code))

citywide_metrics <- df_classified %>%
  select(year, violations_c, dob_calls_311, permits_alteration:arrests) %>%
  group_by(year) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  pivot_longer(violations_c:arrests)

ggplot(citywide_metrics) +
  geom_line(aes(x = year, y = value, group = name)) +
  facet_wrap(~name, scales = "free_y") +
  scale_y_continuous(labels = comma, 
                     limits = c(0, NA))

#### charts for all gentrifying zips combined
## load data
df_classified <- read_csv('data/processed_data/classified_zips.csv') %>%
  mutate(zip_code = as.character(zip_code))

## calculate averages for low-income zips
low_income_zips_summary <- df_classified %>%
  filter(zip_type %in% c('Newly gentrifying', 'Income below 25th percentile')) %>%
  group_by(year) %>%
  summarise(avg_avg_rent = weighted.mean(avg_rent, housing_units, na.rm = TRUE),
            avg_evictions = weighted.mean(evictions, housing_units, na.rm = TRUE),
            avg_eviction_filings = weighted.mean(eviction_filings, housing_units, na.rm = TRUE),
            avg_total_violations = weighted.mean(total_violations, housing_units, na.rm = TRUE),
            avg_violations_c = weighted.mean(violations_c, housing_units, na.rm = TRUE),
            avg_dob_calls_311 = weighted.mean(dob_calls_311, housing_units, na.rm = TRUE),
            avg_permits_alteration = weighted.mean(permits_alteration, housing_units, na.rm = TRUE),
            avg_permits_demolition = weighted.mean(permits_demolition, housing_units, na.rm = TRUE),
            avg_nypd_calls_311 = weighted.mean(nypd_calls_311, total_pop, na.rm = TRUE),
            avg_arrests = weighted.mean(arrests, total_pop, na.rm = TRUE))

## index zips to year of gentrification signal
gentrifying_zips <- df_classified %>%
  filter(zip_type == 'Newly gentrifying')

# normalize values
gentrifying_zips_norm <- gentrifying_zips %>%
  left_join(low_income_zips_summary) %>%
  mutate(avg_rent_norm = 100 * (avg_rent/avg_avg_rent),
         evictions_norm = 100 * (evictions/avg_evictions),
         eviction_filings_norm = 100 * (eviction_filings/avg_eviction_filings),
         total_violations_norm = 100 * (total_violations/avg_total_violations),
         violations_c_norm = 100 * (violations_c/avg_violations_c),
         dob_calls_311_norm = 100 * (dob_calls_311/avg_dob_calls_311),
         permits_alteration_norm = 100 * (permits_alteration/avg_permits_alteration),
         permits_demolition_norm = 100 * (permits_demolition/avg_permits_demolition),
         nypd_calls_311_norm = 100 * (nypd_calls_311/avg_nypd_calls_311),
         arrests_norm = 100 * (arrests/avg_arrests)
         ) %>%
  select(-c(avg_avg_rent:avg_arrests))

g_year <- gentrifying_zips %>%
  filter(gentrification_signal == 1) %>%
  select(zip_code, gentrification_year = year) 

zips_indexed <- gentrifying_zips_norm %>%
  left_join(g_year) %>%
  mutate(years_since_signal = year - gentrification_year) %>%
  group_by(years_since_signal) %>%
  summarise(avg_rent = weighted.mean(avg_rent_norm, housing_units, na.rm = TRUE),
            evictions = weighted.mean(evictions_norm, housing_units, na.rm = TRUE),
            eviction_filings = weighted.mean(eviction_filings_norm, housing_units, na.rm = TRUE),
            total_violations = weighted.mean(total_violations_norm, housing_units, na.rm = TRUE),
            violations_c = weighted.mean(violations_c_norm, housing_units, na.rm = TRUE),
            dob_calls_311 = weighted.mean(dob_calls_311_norm, housing_units, na.rm = TRUE),
            permits_alteration = weighted.mean(permits_alteration_norm, housing_units, na.rm = TRUE),
            permits_demolition = weighted.mean(permits_demolition_norm, housing_units, na.rm = TRUE),
            nypd_calls_311 = weighted.mean(nypd_calls_311_norm, total_pop, na.rm = TRUE),
            arrests = weighted.mean(arrests_norm, total_pop, na.rm = TRUE),
            total_zips = n(),
            total_housing_units = sum(housing_units, na.rm = TRUE),
            total_pop = sum(total_pop, na.rm = TRUE)) %>%
  relocate(total_zips, total_housing_units, total_pop, .after = years_since_signal) %>%
  ## remove rows with too little data
  filter(between(years_since_signal, -3, 3))

#### graph analysis variables
## Class C code violations
ggplot(zips_indexed) +
  geom_line(aes(x = years_since_signal, y = violations_c), color = 'blue') + 
  geom_hline(yintercept = 100, linetype = 'dashed') +
  geom_vline(xintercept = 0, linetype = 'dotted', color = 'gray') +
  theme_minimal() + 
  scale_y_continuous(labels = comma, 
                     limits = c(60, 140)) +
  labs(
    title = "Class C Housing Code Violations",
    x = "Years from gentrification signal",
    y = "Index of code violations (100 = average for low-income ZIPs)", 
    caption = "Source: NYC Open Data"
  )

ggplot(zips_indexed) +
  geom_line(aes(x = years_since_signal, y = permits_alteration), color = 'blue') + 
  geom_hline(yintercept = 100, linetype = 'dashed') +
  geom_vline(xintercept = 0, linetype = 'dotted', color = 'gray') +
  theme_minimal() + 
  scale_y_continuous(labels = comma, 
                     limits = c(80, 120)) +
  labs(
    title = "DOB permits for alterations",
    x = "Years from gentrification signal",
    y = "Index of alteration permits (100 = average for low-income ZIPs)", 
    caption = "Source: NYC Open Data"
  )

ggplot(zips_indexed) +
  geom_line(aes(x = years_since_signal, y = permits_demolition), color = 'blue') + 
  geom_hline(yintercept = 100, linetype = 'dashed') +
  geom_vline(xintercept = 0, linetype = 'dotted', color = 'gray') +
  theme_minimal() + 
  scale_y_continuous(labels = comma, 
                     limits = c(50, 150)) +
  labs(
    title = "DOB permits for demolition",
    x = "Years from gentrification signal",
    y = "Index of demolition permits (100 = average for low-income ZIPs)", ## maybe no y label
    caption = "Source: NYC Open Data"
  )

ggplot(zips_indexed) +
  geom_line(aes(x = years_since_signal, y = dob_calls_311), color = 'blue') + 
  geom_hline(yintercept = 100, linetype = 'dashed') +
  geom_vline(xintercept = 0, linetype = 'dotted', color = 'gray') +
  theme_minimal() + 
  scale_y_continuous(labels = comma, 
                     limits = c(90, 110)) +
  labs(
    title = "311 Calls to DOB",
    x = "Years from gentrification signal",
    y = "DOB Complaints (100 = average for low-income ZIPs)", ## maybe no y label
    caption = "Source: NYC Open Data"
  )

## NYPD 311 calls
ggplot(zips_indexed) +
  geom_line(aes(x = years_since_signal, y = nypd_calls_311), color = 'blue') + 
  geom_hline(yintercept = 100, linetype = 'dashed') +
  geom_vline(xintercept = 0, linetype = 'dotted', color = 'gray') +
  theme_minimal() + 
  scale_y_continuous(labels = comma, 
                     limits = c(80, 120)) +
  labs(
    title = "311 Calls routed to NYPD",
    x = "Years from gentrification signal",
    y = "Index of 311 calls (100 = average for low-income ZIPs)",
    caption = "Source: NYC Open Data"
  )

# arrests
ggplot(zips_indexed) +
  geom_line(aes(x = years_since_signal, y = arrests), color = 'blue') + 
  geom_hline(yintercept = 100, linetype = 'dashed') +
  geom_vline(xintercept = 0, linetype = 'dotted', color = 'gray') +
  theme_minimal() + 
  scale_y_continuous(labels = comma, 
                     limits = c(79, 120)) +
  labs(
    title = "Arrests",
    x = "Years from gentrification signal",
    y = "Index of arrests (100 = average for low-income ZIPs)",
    caption = "Source: NYC Open Data"
  )
   
#### charts for individual zips
## load data 
data <- read_csv('data/processed_data/reference_table.csv')

## get data into graphing format
data_zips <- data %>%
  filter(!zip_type %in% c('Income below 25th percentile', 'Income below 50th percentile',
                          'High income')) %>%
  rename(zip_code = zip_type) %>%
  mutate(zip_type = 'ZIP of interest') %>%
  relocate(zip_type, .after = zip_code)

zip_codes <- data_zips %>%
  select(zip_code) %>%
  unique()

data_reference <- data %>%
  filter(zip_type %in% c('Income below 25th percentile', 'Income below 50th percentile',
                         'High income')) %>%
  cross_join(zip_codes) %>%
  relocate(zip_code, .before = year) %>%
  select(-gentrification_year) %>%
  left_join(g_year)

data_for_graphs <- rbind(data_reference, data_zips) %>%
  arrange(zip_code, year)


#### graphs
## rent- only a few ZIPs with decent data
rent_graphs <- data_for_graphs %>%
  filter(zip_code %in% c('10031', '10033', '10457', '11375'),
         !zip_type %in% c('High income', 'Income below 50th percentile'))

g_year_rent_graphs <- rent_graphs %>%
  select(zip_code, gentrification_year) %>%
  unique()

ggplot(rent_graphs) +
  geom_line(aes(x = year, y = avg_rent, color = zip_type)) +
  facet_wrap(~zip_code) +
  geom_vline(data = g_year_rent_graphs,
             aes(xintercept = gentrification_year)) +
  theme(legend.position = 'bottom',
        legend.title = element_blank())

## indexed version
rent_graphs_indexed <- rent_graphs %>%
  select(zip_code, year, zip_type, avg_rent, gentrification_year) %>%
  pivot_wider(names_from = zip_type, values_from = avg_rent) %>%
  clean_names() %>%
  rename(avg_rent = zip_of_interest,
         reference_rent = income_below_25th_percentile) %>%
  mutate(years_from_signal = year - gentrification_year,
         rent_indexed = 100 * avg_rent/reference_rent) %>%
  filter(between(years_from_signal, -4, 4))

ggplot(rent_graphs_indexed) +
  geom_line(aes(x = years_from_signal, y = rent_indexed)) +
  facet_wrap(~zip_code) + 
  scale_y_continuous(labels = comma, 
                     limits = c(70, 130))


























