### clear work space
rm(list = ls())

############################################
## Use this one to update counties in viz ##
############################################

library(readr)
library(fredr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(devtools)
library(ggplot2)
library(Rserve)

Rserve()

# Set API key
fredr_set_key("Insert_your_API_here")

# Access county FIPS codes
CFIPS <- read_delim(url("https://github.com/robertwwalker/academic-mymod/raw/master/data/COUNTYFIPS.txt"),
                    "\t", escape_double = FALSE, trim_ws = TRUE)

cols(FIPS = col_character(), Name = col_character(), State = col_character())

### Access list of county abbreviations for when FRED doesn't use FIPS codes
Abbreviations <- read.table(file = "Insert where to find the CFIPS file on your own drive here", 
                            header = TRUE)

### First, pull data from FRED

## Wait a minute or two before running each of the following sections (until indicated)
## Because KY has so many counties, FRED is pulling 120 counties x 14 years of data for each variable,
## so to avoid pulling too much data at once, run these section by section.

# Pull data for KY counties estimate of median household income and create a data frame
CFIPS$series_id <- as.character(paste0("MHIKY",CFIPS$FIPS,"A052NCEN"))
household_income <- CFIPS %>% filter(State=="KY") %>% 
  select(series_id) %>% 
  unlist() %>% 
  map_dfr(., fredr) %>% 
  filter(date >= '2010-01-01') %>%
  rename(household_income = value) %>%
  select(date, household_income, series_id)

# Create "master" data frame called county measures and merge by series id
county_measures <- left_join(household_income, CFIPS, by="series_id") %>%
  rename(household_income.series_id = series_id) %>%
  select(date, Name, FIPS, State, household_income)

### Now wait a minute before running the next section.

# Pull data for KY Counties number of unemployed persons and make df
CFIPS$series_id <- as.character(paste0("LAUCN",CFIPS$FIPS,"0000000004A"))
num_unemployed <- CFIPS %>% filter(State=="KY") %>% 
  select(series_id) %>% 
  unlist() %>% 
  map_dfr(., fredr) %>% 
  filter(date >= '2010-01-01') %>%
  rename(num_unemployed = value) %>%
  select(date, num_unemployed, series_id)

# merge CFIPs with num_unemployed by series_id to match up counties
# Then merge with county_measures
num_unemployed <- left_join(num_unemployed, CFIPS, by="series_id")  
county_measures <- full_join(county_measures, num_unemployed) %>%
  select(date, Name, FIPS, State, household_income, num_unemployed)

### Wait a minute before running the next section

# Pull data for KY Counties civilian labor force
CFIPS$series_id <- as.character(paste0("LAUCN",CFIPS$FIPS,"0000000006A"))
clf <- CFIPS %>% filter(State=="KY") %>% 
  select(series_id) %>% 
  unlist() %>% 
  map_dfr(., fredr) %>% 
  filter(date >= '2010-01-01') %>%
  rename(clf = value) |> 
  select(date, clf, series_id)

# merge CFIPS with clf by series_id to match up counties
# Then merge with county_measures
clf <- left_join(clf, CFIPS, by="series_id")  
county_measures <- full_join(county_measures, clf) %>%
  select(date, Name, FIPS, State, household_income, num_unemployed, clf)

# Pull data for KY Counties number of people in poverty
CFIPS$series_id <- as.character(paste0("PEAAKY",CFIPS$FIPS,"A647NCEN"))
ppl_pov <- CFIPS %>% filter(State=="KY") %>% 
  select(series_id) %>% 
  unlist() %>% 
  map_dfr(., fredr) %>% 
  filter(date >= '2010-01-01') %>%
  rename(ppl_pov = value) |> 
  select(date, ppl_pov, series_id)

# merge CFIPs with ppl_pov by series-id to match up counties
# Then merge with county_measures
ppl_pov <- left_join(ppl_pov, CFIPS, by="series_id")  
county_measures <- full_join(county_measures, ppl_pov) %>%
  select(date, Name, FIPS, State, household_income, num_unemployed, clf, ppl_pov)

# Pull data for KY Counties population
Abbreviations$series_id <- as.character(paste0("KY", Abbreviations$Abbreviations ,"POP"))
population <- Abbreviations %>% select(series_id) %>%
  unlist() %>%
  map_dfr(., fredr) %>%
  filter(date >= '2000-01-01') %>%
  rename(population = value) |> 
  select(date, population, series_id)

#### Run as fast as needed now

# Create a population data frame so per capita variables can be calculated later.
population <- left_join(population, Abbreviations, by="series_id")

# To give a regional view of Kentucky, manually group the counties into their Area Development Districts
population <- population |> 
  mutate(ADD = case_when(Name %in% c("Ballard", "Calloway", "Carlisle", "Fulton", "Graves", "Hickman", "McCracken", "Marshall") ~ "Purchase",
                         Name %in% c("Caldwell", "Christian", "Crittenden", "Hopkins", "Livingston", "Lyon", "Muhlenberg", "Todd", "Trigg") ~ "Pennyrile",
                         Name %in% c("Daviess", "Hancock", "Henderson", "McLean", "Ohio", "Union", "Webster") ~ "Green River",
                         Name %in% c("Allen", "Barren", "Butler", "Edmonson", "Hart", "Logan", "Metcalfe", "Monroe", "Simpson", "Warren") ~ "Barren River",
                         Name %in% c("Breckinridge", "Grayson", "Hardin", "Larue", "Marion", "Meade", "Nelson", "Washington") ~ "Lincoln Trail",
                         Name %in% c("Bullitt", "Henry", "Jefferson", "Oldham", "Shelby", "Spencer", "Trimble") ~ "KIPDA",
                         Name %in% c("Boone", "Campbell", "Carroll", "Gallatin", "Grant", "Kenton", "Owen", "Pendleton") ~ "Northern Kentucky",
                         Name %in% c("Bracken", "Fleming", "Lewis", "Mason", "Robertson") ~ "Buffalo Trace",
                         Name %in% c("Bath", "Menifee", "Montgomery", "Morgan", "Rowan") ~ "Gateway",
                         Name %in% c("Boyd", "Carter", "Elliott", "Greenup", "Lawrence") ~ "FIVCO",
                         Name %in% c("Floyd", "Johnson", "Magoffin", "Martin", "Pike") ~ "Big Sandy",
                         Name %in% c("Breathitt", "Knott", "Lee", "Leslie", "Letcher", "Owsley", "Perry", "Wolfe") ~ "Kentucky River",
                         Name %in% c("Bell", "Clay", "Harlan", "Jackson", "Knox", "Laurel", "Rockcastle", "Whitley") ~ "Cumberland Valley",
                         Name %in% c("Adair", "Casey", "Clinton", "Cumberland", "Green", "McCreary", "Pulaski", "Russell", "Taylor", "Wayne") ~ "Lake Cumberland",
                         Name %in% c("Anderson", "Bourbon", "Boyle", "Clark", "Estill", "Fayette", "Franklin", "Garrard", "Harrison", "Jessamine", "Lincoln", "Madison", "Mercer", "Nicholas", "Powell", "Scott", "Woodford") ~ "Bluegrass"
  )
  )

# Create 10-yr population Change at both the county and area development district levels
counties_pop_change <- population %>% 
  group_by(Name) |> 
  mutate(
    county_population = population * 1000,
    county_pop_change_10_yr = (population - lag(population, 9)) / lag(population, 9),
    county_pop_change_10_yr_rounded = (round(county_pop_change_10_yr, 4) * 100),
    county_pop_lag_10yr = lag(county_population, 9)
  ) %>% 
  ungroup() |> 
  group_by(ADD, date) |> 
  mutate(
    add_pop = sum(population) * 1000
  ) |> 
  ungroup() |> 
  group_by(ADD) |> 
  mutate(
    add_pop_change_10_yr = (add_pop - lag(add_pop, 9)) / lag(add_pop, 9),
    add_pop_change_10_yr_rounded = (round(add_pop_change_10_yr, 4) * 100)
  ) |> 
  filter(date >= '2010-01-01') |> 
  select(-c(population, series_id, Abbreviations, county_pop_change_10_yr, add_pop_change_10_yr)
  )

# Join county measures and population data frames
county_measures_w_adds <- full_join(county_measures, counties_pop_change) |> 
  relocate(ADD, .after = Name)

##########################################################
# Now pull high school graduate attainment data from ACS #
##########################################################

library(acs)
library(tidycensus)
library(tidyverse)
library(lubridate)


# Run this to search for variables
#vars <- load_variables(year = 2019, dataset = "acs5/subject") %>%
#  separate(label, into = paste0("label", 1:9), sep = "!!", fill = "right", remove = FALSE) |> 
#  filter(grepl('S1501', name))

# Get US Educational Attainment data from the census
# Create a list of years to pull from to match other County Tableau data
# Manually add the next year when time to update code
years <- lst(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023) 

# Create a vector of variable names and give good name
# Population is 18+
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
    geography = "county",
    variables = my_vars,
    state = "KY",
    year = .x,
    survey = "acs5",
  ),
  .id = "year"
)

county_edu <- county_edu_vars |> 
  separate(col = NAME, into = c('County', 'State'), sep = ',') |> 
  mutate(County = str_remove_all(County, " County")) |> 
  arrange(variable) |> 
  select(-c("GEOID", "moe")) |> 
  pivot_wider(names_from = variable,
              values_from = estimate) |> 
  group_by(County, year) |> 
  mutate(
    total_hsg = sum(hsg_18_24, sca_18_24, ba_18_24, hsg_25_up, sc_25_up, asso_25_up, bach_25_up, grad_25_up),
    total_18up_pop = sum(pop_18_24, pop_25_up)
  )

# change date format from character to date
county_edu$year <- as.numeric(county_edu$year)
county_edu$year <- ymd(county_edu$year, truncated = 2L)
county_edu$State <- 'KY'

county_edu <- county_edu |> 
  select(year, County, State, total_hsg, total_18up_pop) |> 
  rename(Name = County, date = year)

# join education and county and ADD measures
county_measures_w_adds <- full_join(county_measures_w_adds, county_edu)

### Pull County Employment data from QCEW data
### Manually add next year chunk of code when time to update code

qcewGetIndustryData <- function (year, qtr, industry) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/industry/INDUSTRY.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("INDUSTRY", industry, url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}

years <- 2014:2023

# Loop over years and bind results
county_employment_data_all_years <- map_dfr(
  years,
  ~ qcewGetIndustryData(.x, "a", "10") %>%
    filter(
      as.numeric(area_fips) >= 21001,
      as.numeric(area_fips) <= 21239,
      own_code == 0
    ) 
)

# row bind to make one big data frame
# There are the same number of columns in the same order, so it's okay to do here
counties_employment <- county_employment_data_all_years|> 
  select(area_fips, year, annual_avg_emplvl) |> 
  group_by(area_fips) |> 
  mutate(county_emp_change_5_yr = (annual_avg_emplvl - lag(annual_avg_emplvl, 4)) / lag(annual_avg_emplvl, 4),
         county_emp_5_yr_rounded = (round(county_emp_change_5_yr, 4) * 100),
         county_emp_lag_5yr = lag(annual_avg_emplvl, 4),
         emp_chg = round((annual_avg_emplvl - lag(annual_avg_emplvl)), 0)
  ) |> 
  select(area_fips, year, county_emp_5_yr_rounded, annual_avg_emplvl, county_emp_lag_5yr) |> 
  rename(FIPS = area_fips, date = year) 

# Change date classification to date (from character)
counties_employment$date <- as.numeric(counties_employment$date)
counties_employment$date <- ymd(counties_employment$date, truncated = 2L)

# join employment and ADD county measures
county_employment_w_adds <- full_join(county_measures_w_adds, counties_employment) |>
  group_by(ADD, date) |> 
  mutate(add_emp_lvl = sum(annual_avg_emplvl)
  ) |> 
  ungroup() |> 
  group_by(ADD) |> 
  mutate(add_emp_chg_5_yr = round((add_emp_lvl - lag(add_emp_lvl, 4)) / lag(add_emp_lvl, 4), 4) * 100
  ) |> 
  filter(date >= '2016-01-01')

county_measures_w_adds <- full_join(county_employment_w_adds, county_employment_w_adds)


### Pull GDP data from BEA
library(bea.R)

# Manually add the next year when time to update code

beaKey <- "insert_bea_key_here"

# search <- beaSearch("GDP", "beaKey", asHtml = TRUE)

specs <- list(
  'UserID' = "beaKey",
  'Method' = 'GetData',
  'datasetname' = 'Regional', 
  'TableName' = 'CAGDP2',
  'LineCode' = '1',
  'GeoFips' = '21001,21003,21005,21007,21009,21011,21013,21015,21017,21019,21021,21023,21025,21027,21029,21031,21033,21035,21037,21039,21041,21043,21045,21047,21049,21051,21053,21055,21057,21059,21061,21063,21065,21067,21069,21071,21073,21075,21077,21079,21081,21083,21085,21087,21089,21091,21093,21095,21097,21099,21101,21103,21105,21107,21109,21111,21113,21115,21117,21119,21121,21123,21125,21127,21129,21131,21133,21135,21137,21139,21141,21143,21145,21147,21149,21151,21153,21155,21157,21159,21161,21163,21165,21167,21169,21171,21173,21175,21177,21179,21181,21183,21185,21187,21189,21191,21193,21195,21197,21199,21201,21203,21205,21207,21209,21211,21213,21215,21217,21219,21221,21223,21225,21227,21229,21231,21233,21235,21237,21239',
  'Year' = '2017,2018,2019,2020,2021,2022,2023',
  'ResultFormat' = 'json'
)

df <- beaGet(specs)
beaLong <- beaGet(specs, asWide = FALSE)
beaLong$State <- "Kentucky"
beaLong$TimePeriod <- ymd(beaLong$TimePeriod, truncated = 2L)

bea <- beaLong |> 
  rename(County = GeoName, date = TimePeriod, Measure = CL_UNIT, Estimate = DataValue,
         FIPS = GeoFips) |> 
  select(-c(Code, UNIT_MULT)) |> 
  relocate(State, date, FIPS, County, Measure, Estimate) |> 
  arrange(County, date) |> 
  group_by(County) |> 
  mutate(county_gdp_growth = round((Estimate-lag(Estimate))/lag(Estimate), 4) * 100,
         county_gdp_lag = lag(Estimate)) |> 
  rename("county_gdp_level" = Estimate, Name = County) |> 
  select(-c(Measure))

bea$State <- "KY"

county_measures_w_adds <- full_join(county_measures_w_adds, bea)

# Now that all of the data frames are joined into one, perform percent change calculations and calculations
# done at the ADD level
county_measures_w_calcs <- county_measures_w_adds |> 
  group_by(Name, date) |> 
  mutate(co_unemployment_rate = round(num_unemployed / clf, 4) * 100,
         co_percent_in_pov = round(ppl_pov / county_population, 4) * 100,
         co_hsg = round(total_hsg / total_18up_pop, 4) * 100
  ) |> 
  ungroup() |> 
  group_by(Name) |> 
  mutate(co_hhi_chg = round((household_income - lag(household_income)) / lag(household_income), 4) * 100,
         co_ur_chg = round((co_unemployment_rate - lag(co_unemployment_rate)) / lag(co_unemployment_rate), 4 ) * 100,
         co_pov_chg = round((co_percent_in_pov - lag(co_percent_in_pov)) / lag(co_percent_in_pov), 4) * 100,
         co_hsg_chg = round((co_hsg - lag(co_hsg)) / lag(co_hsg), 4) * 100
  ) |> 
  ungroup() |> 
  group_by(ADD, date) |> 
  mutate(add_hhi = round(mean(household_income)),
         add_ur = round(sum(num_unemployed) / sum(clf), 4) * 100,
         add_percent_in_pov = round(sum(ppl_pov) / sum(county_population), 4) * 100,
         add_total_hsg = sum(total_hsg),
         add_total_18up = sum(total_18up_pop),
         add_hsg = round(sum(total_hsg) / sum(total_18up_pop), 4) * 100,
         add_gdp_level = sum(county_gdp_level)
  ) |> 
  ungroup() |> 
  group_by(ADD) |> 
  mutate(add_hhi_chg = round((add_hhi - lag(add_hhi)) / lag(add_hhi), 4) * 100,
         add_ur_chg = round((add_ur - lag(add_ur)) / lag(add_ur), 4) * 100,
         add_pov_chg = round((add_percent_in_pov - lag(add_percent_in_pov)) / lag(add_percent_in_pov), 4) * 100,
         add_hsg_chg = round((add_hsg - lag(add_hsg)) / lag(add_hsg), 4) * 100,
         add_gdp_chg = round((add_gdp_level - lag(add_gdp_level)) / lag(add_gdp_level), 4) * 100
  )

# filter data do keep 2017 and above
filtered_county_measures <- county_measures_w_calcs |> 
  filter(date >= '2017-01-01')

save(filtered_county_measures, file="updated_county_measures.rdata")


