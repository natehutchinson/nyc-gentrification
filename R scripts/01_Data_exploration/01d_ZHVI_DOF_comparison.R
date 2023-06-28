#########################################################################################################
## File name: ZHVI-DOF comparison
## Description: Analyze correlation between ZHVI and DOF data to determine if they are interchangeable
## Created: June 28, 2023
## Last edited: June 28, 2023
#########################################################################################################

library(ggplot2)

## Approach: compare ZHVI values with median building sale prices for 4 years: 2011, 2015, 2019, 2022
## Rationale for choosing these years: analysis period starts in 2011 and ends in 2022. 2019 is the last pre-covid year,
## and 2015 is the midpoint between 2011 and 2019

## create df of ZHVI values for relevant years
zhvi_for_comparison <- nyc_metro_yearly %>%
  select(zip_code, avg_zhvi_2011, avg_zhvi_2015, avg_zhvi_2019, avg_zhvi_2022)

## create df of building sales values for relevant years
sales_for_comparison <- sales_by_zip_wide %>%
  select(zip_code, median_sale_price_2011, median_sale_price_2015, median_sale_price_2019, median_sale_price_2022,
         count_sales_2011, count_sales_2015, count_sales_2019, count_sales_2022)

## join dfs
combined <- zhvi_for_comparison %>%
  inner_join(sales_for_comparison, by = 'zip_code')

## 2011
combined_2011 <- combined %>%
  filter(count_sales_2011 >= 20,
         !is.na(avg_zhvi_2011)) %>%
  select(zip_code, avg_zhvi_2011, median_sale_price_2011)

ggplot(combined_2011) + 
  aes(x = avg_zhvi_2011, y = median_sale_price_2011) +
  geom_point() 

cor(combined_2011$avg_zhvi_2011, combined_2011$median_sale_price_2011)

## 2015
combined_2015 <- combined %>%
  filter(count_sales_2015 >= 20,
         !is.na(avg_zhvi_2015)) %>%
  select(zip_code, avg_zhvi_2015, median_sale_price_2015)

ggplot(combined_2015) + 
  aes(x = avg_zhvi_2015, y = median_sale_price_2015) +
  geom_point() 

cor(combined_2015$avg_zhvi_2015, combined_2015$median_sale_price_2015)

## 2019
combined_2019 <- combined %>%
  filter(count_sales_2019 >= 20,
         !is.na(avg_zhvi_2019)) %>%
  select(zip_code, avg_zhvi_2019, median_sale_price_2019)

ggplot(combined_2019) + 
  aes(x = avg_zhvi_2019, y = median_sale_price_2019) +
  geom_point() 

cor(combined_2019$avg_zhvi_2019, combined_2019$median_sale_price_2019)

## 2022
combined_2022 <- combined %>%
  filter(count_sales_2022 >= 20,
         !is.na(avg_zhvi_2022)) %>%
  select(zip_code, avg_zhvi_2022, median_sale_price_2022)

ggplot(combined_2022) + 
  aes(x = avg_zhvi_2022, y = median_sale_price_2022) +
  geom_point() 

cor(combined_2022$avg_zhvi_2022, combined_2022$median_sale_price_2022)
