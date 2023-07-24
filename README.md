# nyc-gentrification

## Overview

This readme document will walk through the process used to download, clean, and visualize the data used to write [this article](https://medium.com/@natehutchinson92/there-goes-the-neighborhood-a-data-driven-approach-to-mitigating-the-effects-of-gentrification-482338a764e). The analysis was done entirely using R and QGIS. All R scripts can be found in the `R scripts` folder of this Github repository, and the components using QGIS are detailed below.

The next section will discuss the data sources used for this project. The final section will discuss data processing and visualization.

## Data sources

This project makes use of the following data sources:

* [The Census Bureau's American Community Survey](https://www.census.gov/programs-surveys/acs/data.html)
  - Housing units
  - Total population
  - Both metrics are 5-year estimates collected at the census tract level from 2011-2021
  - This data was downloaded via API using R's `tidycensus` package
  - The API call can be found in the script titled `01a_download_acs`
* Home price data from Zillow
  - [Zillow Home Value Index (ZHVI)](https://www.zillow.com/research/data/)
  - Both metrics are available at the ZIP code level on a monthly basis
* Adjusted Gross Income data from the IRS
  - [Individual income tax ZIP code data](https://www.irs.gov/statistics/soi-tax-stats-individual-income-tax-statistics-zip-code-data-soi)
  - This data is available at the ZIP code level on a yearly basis
* Neighborhood metrics from Zillow and NYC OpenData
  - [Zillow Observed Rent Index (ZORI)](https://www.zillow.com/research/data/)

  - [Housing maintenance code violations](https://data.cityofnewyork.us/Housing-Development/Housing-Maintenance-Code-Violations/wvxf-dwi5)
  - [Housing permits](https://www.nyc.gov/site/planning/data-maps/open-data/dwn-housing-database.page)
  - [311 calls (to NYPD and DOB)](https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/7ahn-ypff)
  - [Arrests](https://data.cityofnewyork.us/Public-Safety/NYPD-Arrests-Data-Historic-/8h9b-rp9u)
     - Arrests are the only neighborhood metric that do not have ZIP code in the data as downloaded
     - They do have the geographic coordinates of each arrest, which we can map to the ZIP code level in QGIS using the following process
        1. Download arrests data from NYC Open Data (important fields are arrest_key, x_coord_cd, and y_coord_cd)- I pulled the data one year at a time to avoid overloading QGIS
        2. Load ZIP code boundaries into QGIs
        3. Load arrests data into QGIS one year at a time
        4. Use the Count Points in Polygons function to count the number of arrests in each ZIP
        5. Export this file as a csv
        6. Repeat for each year 
  - [NYC ZIP code boundaries](https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u)
 
## Analysis approach

All analysis done for this article can be replicated by running all scripts in the `R Scripts` folder of this directory in order (you can ignore the sub-folder `00_data_exploration` and begin from `01_data_processing`). The primary analytical tasks of this project are to create the 'gentrification signal' that is the subject of this article and then to analyze how various neighborhood metrics change when the signal switches from zero to one. Both of these processes are outlined below:

#### Creating the 'gentrification signal'

The 'gentrification signal' is triggered for a ZIP code if the following criteria are met:

1. The difference between the ZIP code's weighted percentile rank for home values and its weighted percentile rank for income is more than 25 percentage points
2. The ZIP code is in the inner third of the NYC metropolitan statistical area (MSA)
3. The ZIP code is in the bottom income quartile for the MSA
4. The 25 percentile gap occurs for the first time during the observation period of 2011-2020 
 
Assessing criteria 1 requires doing the following:

1. Downloading home value data from the Zillow Home Value Index (ZHVI): this data is available at a month level, so we have to convert it to yearly data by taking yearly averages at the ZIP-year level- see `01_b_zhvi_import`
2. Downloading Adjusted Gross Income data from the IRS Statistics of Income: this data is already available at the ZIP-year level- see `01c_irs_import`
3. Obtaining the weights to be used to calculate weighted percentile rank
  - We use housing units as weights for the home value rankings and population for the income rankings
  - Both metrics are available from the American Community Survey (ACS)- see `01a_download_acs`
  - However, data is available at the census tract level, and we need it at the ZIP code level
  - To convert it to ZIP code level, we use a census tract-ZIP code mapping developed in QGIS, using the process outlined below
4. Process for creating census tract-ZIP code level:
     1. Download ZCTA shapefile from the [Census](https://www.census.gov/programs-surveys/geography/guidance/geo-areas/zctas.html)
     2. Download census tract shapefiles for NY, NJ, and PA from the [Census](https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2020&layergroup=Census+Tracts)
     3. Load all 4 shapefiles into QGIS
     4. Append the relevant ZIP code to each census tract GEOID using ‘Join Attributes by Location’
        - We have to do this 3 times- one for each census tract layer (3 states)
        - Join based on intersection, and take attributes of the feature with the largest overlap
     5. Step 4 will give us most of the mapping that we need, but it will miss some ZIPs
        - To account for these, we again join attributes by location, but instead we start with ZIPs and append GEOIDs
        - In this case we use one-to-many mapping
        - We will then take the average values for all census tracts mapped to a ZIP
        - The approach taken in step 4 makes more sense when there are multiple census tracts more or less contained within one ZIP
        - This is the case for much of NYC
        - The approach from step 5 is better when ZIPs are cover smaller sections of multiple tracts, as is the case in denser areas of the city
      6. Save outputs as CSVs
      7. Import CSVs to R for further processing- see `01d_census_tract_zip_mapping`
 5. Once we have the yearly values and weights for home values and incomes, we use the wtd.rank function of the [Hmisc package](https://www.rdocumentation.org/packages/Hmisc/versions/5.1-0) in R- see `01e_rank_zips`

Assessing criteria 2 requires calculating the distance of each ZIP code in the NYC MSA to the central business district (identified as ZIP code [10036](https://www.unitedstateszipcodes.org/10036/) for the purposes of this project). To calculate that, we do the following:

1. Load the ZCTA shapefile into QGIS
2. Filter to roughly the NYC metro area to reduce computation time
3. Add centroids to each ZCTA using Vector > Geometry Tools > Centroids
4. Select the centroid corresponding to the CBD (10036) and create a new layer made of just that centroid
5. Calculate distance between each centroid and the CBD centroid using the Distance Matrix tool
6. Export as CSV for further processing in R (see the script called `01f_final_analysis_file_creation`) 

Assessing criteria 3 & 4 is trivial once we have completed the analysis for 1 & 2- see `01f_final_analysis_file_creation`

#### Analyzing neighborhood change

1. All neighborhood metrics (see the 'Data sources' section) are downloaded, cleaned, and incorporated into the analysis data set using the scripts `01g_download_nyc_open_data`, `01h_zori_import`, and `02a_append_analysis_data`
2. ZIP codes are then classified into one of the following categories (see `02b_classify_zips`):
   - Already gentrifying- these are ZIP codes that already had a 25-percentile gap between home values and incomes at the start of the analysis period (2011). We cannot use these to assess the 'gentrification signal' because we do not know the first year they met the critera.
   - Newly gentrifying- these are the ZIP codes that meet the criteria for the 'gentrification signal'
   - Income below 25th percentile- these are ZIP codes that do not meet the criteria for the 'gentrification signal,' but are in the bottom quartile of the NYC MSA in terms of income
   - Income below 50th percentile- these are ZIP codes that do not meet the criteria for the 'gentrification signal,' but are in the bottom half of the NYC MSA in terms of income
   -  High income- these are ZIP codes that are in the top half of the NYC MSA in terms of income
   -  The latter two categories are not used for any of the analysis in this project, but are available for potential future analyses
   -  The process for doing so can be found in `02b_classify_zips`
 3. Weighted average values of each neighborhood metric are then calculated for each category
      - All metrics are weighted by the number of housing units in the ZIP code except for arrests and 311 calls to police, which are weighted by the total population of the ZIP code
      - See `02c_create_reference_table`
4. To analyze rent changes, we filter for the 4 ZIP codes that have decent rent data, and then plot their rents against the rents of ZIP codes in the 'Income below 25th percentile'
5. To analyze other neighborhood metrics, we do the following:
   1. Index all 'newly gentrifying' ZIP codes to the year that the 'gentrification signal' was triggered, so that the year of the signal is year zero, the year before is year -1, the year after is year 1, etc. This value is called 'years from signal'
   2. Similarly average the reference category (ZIP codes in the 25th income percentile or below) to this year
   3. For each year from signal (i.e. -2, -1, 0, 1, etc.), we divide each neighborhood metric for the 'newly gentrifying' ZIP codes by the same metric for the reference category
   4. This creates a set of indexed metrics where 100 is the average for ZIP codes in the 25th income percentile or below
   5. Taking code violations as an example, if the newly gentrifying ZIP codes have a value of 110 when years from signal is equal to 1, that means that, on average, newly gentrifying ZIP codes have 10% more code violations than the average low-income ZIP code in the year following the 'gentrification signal'
   6. Steps 4 and 5 are executed in the script called `02d_create_charts`
   7. Finally, the maps shown at the end of the article are created in the script `02e_gentrification_index_charts`
