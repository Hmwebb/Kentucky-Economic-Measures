### clear work space
rm(list = ls())

################################################
## Use this code to update KY measures in viz ##
################################################

library(readr)
library(fredr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(Rserve)

Rserve()

# Set API key
fredr_set_key("your_api_here")

# create a vector of the variables to pull from FRED
kentucky_vars <- c("MEHOINUSKYA646N", "LAUST210000000000003A", "PPAAKY21000A156NCEN", 
                   "GCT1501KY", "KYPOP")

## Create a loop to get all data from KY codes
kentucky_df <- data.frame()

for (item in kentucky_vars) {
  
  ky_output <- fredr_series_observations(item, observation_start = as.Date("2000-01-01"), observation_end = as.Date("9999-12-31"),
                                         frequency = "a", aggregation_method = "avg", output_type = 1)
  
  
  kentucky_df <- rbind(kentucky_df, ky_output)
  
}

# Create a Geo column and select values to keep
kentucky_df$Geo <- 'Kentucky'

# Select variables
kentucky_df <- kentucky_df %>% select(date, value, series_id, Geo)

# Convert to a wide data frame
kentucky_wide_df <- kentucky_df %>% pivot_wider(names_from = series_id, values_from = value)

# Create 10-yr population Change and annual percentage change for other variables, pulled above
kentucky_wide_df <- kentucky_wide_df %>% 
  mutate(
    ky_pop_change_10_yr = (KYPOP - lag(KYPOP, 9)) / lag(KYPOP, 9),
    ky_pop_change_10_yr_rounded = (round(ky_pop_change_10_yr, 4) * 100),
    ky_hhi_chg = round((MEHOINUSKYA646N-lag(MEHOINUSKYA646N))/lag(MEHOINUSKYA646N), 4) * 100,
    ky_ur_chg = round((LAUST210000000000003A - lag(LAUST210000000000003A)) / lag(LAUST210000000000003A), 4) * 100,
    ky_pov_chg = round((PPAAKY21000A156NCEN - lag(PPAAKY21000A156NCEN)) / lag(PPAAKY21000A156NCEN), 4) * 100,
    ky_hsg_chg = round((GCT1501KY - lag(GCT1501KY)) / lag(GCT1501KY), 4) * 100
  ) |>
  filter(date >= '2015-01-01') |> 
  select(-c(KYPOP, ky_pop_change_10_yr)) |> 
  rename("Median Household Income" = MEHOINUSKYA646N, "Unemployment Rate" = LAUST210000000000003A,
         "Poverty Rate" = PPAAKY21000A156NCEN, "High School Graduation Rate" = GCT1501KY,
         "10-yr Population Change" = ky_pop_change_10_yr_rounded)

### Load Kentucky Employment data
library(lubridate)

### pull from QCEW data
library(tidyverse)

qcewGetIndustryData <- function (year, qtr, industry) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/industry/INDUSTRY.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("INDUSTRY", industry, url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}

# Create a loop finding the employment data at the Kentucky level over your desired number of years
years <- 2014:2023

# Loop over years and bind results
kentucky_employment_data <- map_dfr(
  years,
  ~ qcewGetIndustryData(as.character(.x), "a", "10") %>%
    filter(area_fips == "21000", own_code == 0) 
)

# Create final data frame and variable for 5-yr employment change
# remember indexing for the lags!!
# manually add next year variable when time to update the code

kentucky_employment <- kentucky_employment_data|> 
  select(area_fips, year, annual_avg_emplvl) |> 
  mutate(Geography = case_when(area_fips == "21000" ~ "Kentucky"),
         ky_emp_change_5_yr = round((annual_avg_emplvl - lag(annual_avg_emplvl, 4)) / lag(annual_avg_emplvl, 4), 4) * 100
  ) |> 
  rename("Kentucky avg Employment" = annual_avg_emplvl, "Kentucky 5-yr Employment Change" = ky_emp_change_5_yr)

# convert year from numeric to date so it works in tableau
kentucky_employment$year <- ymd(kentucky_employment$year, truncated = 2L)

kentucky_employment <- kentucky_employment |> 
  rename(Geo = Geography, date = year)

final_ky_df <- full_join(kentucky_wide_df, kentucky_employment)

### Load KY GDP data
### Manually add next year when time to update code
library(bea.R)

beaKey <- "bea_key_here"

search <- beaSearch("GDP", "bea_key_here", asHtml = TRUE)

specs <- list(
  'UserID' = "bea_key_here",
  'Method' = 'GetData',
  'datasetname' = 'Regional', 
  'TableName' = 'CAGDP2',
  'LineCode' = '1',
  'GeoFips' = '21000',
  'Year' = '2017,2018,2019,2020,2021,2022,2023',
  'ResultFormat' = 'json'
)

df <- beaGet(specs)
ky_bea <- beaGet(specs, asWide = FALSE)
ky_bea$TimePeriod <- ymd(ky_bea$TimePeriod, truncated = 2L)

ky_gdp <- ky_bea |> 
  rename(State = GeoName, date = TimePeriod, Measure = CL_UNIT, Estimate = DataValue,
         FIPS = GeoFips) |> 
  select(-c(Code, UNIT_MULT)) |> 
  relocate(State, date, FIPS, Measure, Estimate) |> 
  mutate(GDP_Growth = round((Estimate-lag(Estimate))/lag(Estimate), 4) * 100) |> 
  rename("GDP in thousands" = Estimate, "GDP Growth" = GDP_Growth) |> 
  select(-c(Measure)) |> 
  rename(Geo = State)

# Merge data frames
final_ky_df <- full_join(final_ky_df, ky_gdp) |> 
  select(-c(area_fips, FIPS))

filtered_ky_df <- final_ky_df |> 
  filter(date >= '2017-01-01')

save(filtered_ky_df, file="updated_KY_measures.rdata")


