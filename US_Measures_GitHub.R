### clear work space
rm(list = ls())

#################################################
## Use this code to update US variables in viz ##
#################################################

library(readr)
library(fredr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(Rserve)
library(acs)
library(tidycensus)
library(tidyverse)

Rserve()

# Set API key
fredr_set_key("insert_key_here")

us_vars <- c("MEHOINUSA646N", "UNRATE", "PPAAUS00000A156NCEN", "POPTHM")

## Create a loop to get all data from US codes
us_df <- data.frame()

for (item in us_vars) {
  
  us_output <- fredr_series_observations(item, observation_start = as.Date("2000-01-01"), observation_end = as.Date("9999-12-31"),
                                         frequency = "a", aggregation_method = "avg", output_type = 1)
  
  
  us_df <- rbind(us_df, us_output)
  
}

# Create a Geo column and select values to keep
us_df$NAME <- 'United States'

# Select variables
us_df <- us_df %>% select(date, value, series_id, NAME)

# Convert to a wide df
us_wide_df <- us_df %>% pivot_wider(names_from = series_id, values_from = value)

# Create 10-yr population Change
us_wide_df <- us_wide_df %>% 
  mutate(
    us_pop_change_10_yr = (POPTHM - lag(POPTHM, 9)) / lag(POPTHM, 9),
    us_pop_change_10_yr_rounded = (round(us_pop_change_10_yr, 4) * 100),
    us_hhi_chg = round((MEHOINUSA646N - lag(MEHOINUSA646N))/ lag(MEHOINUSA646N), 4) * 100,
    us_ur_chg = round((UNRATE - lag(UNRATE)) / lag(UNRATE), 4) * 100,
    us_pov_chg = round((PPAAUS00000A156NCEN - lag(PPAAUS00000A156NCEN)) / lag(PPAAUS00000A156NCEN), 4) * 100
  ) |>
  filter(date >= '2015-01-01') |> 
  select(-c(POPTHM, us_pop_change_10_yr)) |> 
  rename("US Median Household Income" = MEHOINUSA646N, "US Unemployment Rate" = UNRATE,
         "US Poverty Rate" = PPAAUS00000A156NCEN,"US 10-yr Population Change" = us_pop_change_10_yr_rounded
  )

### Educational Attainment pull from ACS

## use this code outline to search for variables
#vars <- load_variables(year = 2019, dataset = "acs5/subject") %>%
#  separate(label, into = paste0("label", 1:9), sep = "!!", fill = "right", remove = FALSE) |> 
#  filter(grepl('S1501', name))

# Get US Educational Attainment data from the census
# Create a list of years to pull from to match other County Tableau data
# Manually add next year when time to update code
years <- lst(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023) 

# Create a vector of variable names and give good name
my_vars <- c(hsg_18_24 = "S1501_C01_003",
             sca_18_24 = "S1501_C01_004",
             ba_18_24 = "S1501_C01_005",
             pop_18_24 = "S1501_C01_001",
             hsg_25_up = "S1501_C01_009",
             sc_25_up = "S1501_C01_010",
             asso_25_up = "S1501_C01_011",
             bach_25_up = "S1501_C01_012",
             grad_25_up = "S1501_C01_013",
             pop_25_up = "S1501_C01_006")

# Create data frame by pulling desired variables from ACS 5-year
# And start cleaning data
county_edu_vars <- map_dfr(
  years,
  ~ get_acs(
    geography = "us",
    variables = my_vars,
    year = .x,
    survey = "acs5",
  ),
  .id = "year"
)

us_edu <- county_edu_vars |> 
  arrange(variable) |> 
  select(-c("GEOID", "moe")) |> 
  pivot_wider(names_from = variable,
              values_from = estimate) |> 
  group_by(year) |> 
  mutate(
    total_hsg = sum(hsg_18_24, sca_18_24, ba_18_24, hsg_25_up, sc_25_up, asso_25_up, bach_25_up, grad_25_up),
    total_18up_pop = sum(pop_18_24, pop_25_up)
  )

# change date format from character to date
us_edu$year <- as.numeric(us_edu$year)
us_edu$year <- ymd(us_edu$year, truncated = 2L)
us_edu$State <- 'KY'

us_edu <- us_edu |> 
  select(year, total_hsg, total_18up_pop) |> 
  rename(date = year) |> 
  mutate(us_hsg = (total_hsg / total_18up_pop) * 100)

# create final data frame by merging above data frames
final_us_df <- full_join(us_wide_df, us_edu)

### Start Employment pull
qcewGetIndustryData <- function (year, qtr, industry) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/industry/INDUSTRY.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("INDUSTRY", industry, url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}

# 2014 is as far back as the data goes
years <- 2014:2023
us_data_list <- list()

for (yr in years) {
  df <- qcewGetIndustryData(as.character(yr), "a", "10")
  df <- subset(df, area_fips == "US000" & own_code == 0)
  df$year <- yr
  us_data_list[[as.character(yr)]] <- df
}

all_us_employment_data <- do.call(rbind, us_data_list)

# Create final data frame and variable for 5-yr employment change
# remember indexing for the lags!!
# rename year to date to match other data frames for joining

us_employment <- all_us_employment_data |> 
  select(area_fips, year, annual_avg_emplvl) |> 
  mutate(Geography = case_when(area_fips == "US000" ~ "United States" ),
         us_emp_change_5_yr = (annual_avg_emplvl - lag(annual_avg_emplvl, 4)) / lag(annual_avg_emplvl, 4),
         rounded_us_emp_change = round(us_emp_change_5_yr, 4) * 100
  ) |> 
  select(c(year, rounded_us_emp_change, Geography)) |> 
  rename("date" = year, "US 5-yr Employment % Change" = rounded_us_emp_change,
         "NAME" = Geography) 

us_employment$date <- ymd(us_employment$date, truncated = 2L)

# Merge data frame with final data frame
final_us_df <- full_join(final_us_df, us_employment)

### Trying to pull GDP data from BEA
### Manually add year when time to update code
library(bea.R)

beaKey <- "bea_key_here"

specs <- list(
  'UserID' = "bea_key_here",
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'frequency' = 'A',
  'TableName' = 'T10105',
  'LineNumber' = '1',
  'Year' = '2017,2018,2019,2020,2021,2022,2023',
  'ResultFormat' = 'json'
)

df <- beaGet(specs)
us_bea <- beaGet(specs, asWide = FALSE)
us_bea$TimePeriod <- ymd(us_bea$TimePeriod, truncated = 2L)
us_bea$Geo <- "United States"

us_gdp <- us_bea |> 
  filter(LineNumber == "1") |> 
  rename(date = TimePeriod, Measure = METRIC_NAME, Estimate = DataValue) |> 
  select(c(date, Measure, Estimate, Geo,)) |> 
  relocate(Geo, date, Measure, Estimate) |> 
  mutate(GDP_Growth = round((Estimate-lag(Estimate))/lag(Estimate), 4) * 100) |> 
  rename(NAME = Geo, "US GDP in thousands" = Estimate, "US GDP Growth" = GDP_Growth) |> 
  select(-c(Measure))

# Merge data frame with final data frame
final_us_df <- full_join(final_us_df, us_gdp)

final_us_df_w_chg <- final_us_df |> 
  mutate(us_hsg_chg = round((us_hsg - lag(us_hsg)) / lag(us_hsg), 4) * 100)

filtered_us_df <- final_us_df_w_chg |> 
  filter(date >= '2017-01-01')

save(filtered_us_df, file="updated_US_measures.rdata")





