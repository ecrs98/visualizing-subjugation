########################################
## VISUALIZING SUBJUGATION - ATL
## Pull and Clean Data
## Author: Kwadwo Alfajiri Shah
## Date: 17 Mar 24
## Updated:
##

## ----------------- ##
## 0. housekeeping ----
## ----------------- ##

## libraries
packages <- c("dplyr", "tidycensus", "sf", "ggplot2", "stringr")
lapply(packages, require, character.only=T)

## setwd
root <- "~/visualizing-subjugation/atl/"
data <- paste0(root, "data/")
output <- paste0(root, "output/")
programs <- paste0(root, "programs/")
keys <- "~/utils/jingle_jangle/"
setwd(root)


## ---------------------------------------- ##
## 1. Pull data                           ----
## ---------------------------------------- ##

## enter api key
census_api_key(readLines(paste0(keys, "census_key.txt"))) %>% suppressWarnings()

## define variables to pull
## these are variable codes from the ACS - 
pull_vars <- c("B19013_001",    # median income
               
               ## occupation
               "C24010_001",   # occup total population
               "C24010_019",     # service - male
               "C24010_055",     # service - female
               
               ## employment status
               "B23025_001",    # employment total
               "B23025_005",    # unemployed
               "B23025_007",     # not in labor force
               
               ## race
               "B02009_001",   # black - alone or in combo
               "B02001_002",   # white alone
               "B01003_001",   # total pop
               
               ## commute
               "B08134_001",  # total
               "B08134_006",  # commute time - 25 to 29 mins
               "B08134_007",  # commute time - 30 to 34 mins
               "B08134_008",  # commute time - 25 to 44 mins
               "B08134_009",  # commute time - 45 to 59 mins
               "B08134_010",  # commute time - 60 or more mins
               "B08134_061",   # public transportation (excl taxi)
               
               ## poverty
               "B17017_001",   # total
               "B17017_002",   # below poverty level
               
               ## homeownership
               "DP04_0001",   # total housing units
               "DP04_0045",   # total occupied housing
               "DP04_0046"    # owner-occupied housing
)


## pull from api ----
atl.raw <- get_acs(
  state = "GA",
  county = c("Fulton", "DeKalb"),
  geography = "tract",
  variables = pull_vars,
  geometry = TRUE, # tells tidycensus to add tigris geometries
  year = 2021
)


## ---------------------------------------- ##
## 2. Clean data                           ----
## ---------------------------------------- ##

## clean
atl <- atl.raw %>% 
  ## reshape - long to wide
  select(-moe) %>% 
  tidyr::pivot_wider(names_from = "variable", values_from = "estimate") %>% 
  ## county name
  mutate(county = sub(".*,\\s(\\w+)\\sCounty.*", "\\1", NAME)) %>% 
  ## create percentages
  mutate(median_income = B19013_001,
         pct_service = (C24010_019 + C24010_055)/C24010_001,
         pct_unemp = B23025_005 / B23025_001,
         pct_not_labor = B23025_007 / B23025_001,
         pct_black = B02009_001 / B01003_001,
         pct_nonwhite = (B01003_001 - B02001_002) / B01003_001,
         pct_public_transit = B08134_061/B08134_001,
         pct_commute30 = rowSums(cbind(B08134_007,B08134_008,B08134_009,B08134_010))/B08134_001,
         # pct_commute25 = rowSums(cbind(B08134_006,B08134_007,B08134_008,B08134_009,B08134_010))/B08134_001,
         pct_poverty = B17017_002 / B17017_001,
         pct_homeowner = DP04_0046 / DP04_0045,
         pct_homeowner_total = DP04_0046 / DP04_0001
  ) %>% 
  ## create binned vars
  mutate(pct_service_bin = cut(pct_service, breaks=4),
         pct_unemp_bin = cut(pct_unemp, breaks=4),
         pct_not_labor_bin = cut(pct_not_labor, breaks=4),
         pct_black_bin = cut(pct_black, breaks = 4),
         pct_nonwhite_bin = cut(pct_nonwhite, breaks = 4),
         pct_public_transit_bin = cut(pct_public_transit, breaks = 4, na.rm=T),
         pct_poverty_bin = cut(pct_poverty, breaks = c(0,0.1,0.2,0.4,1)),
         pct_homeowner_bin = cut(pct_homeowner, breaks = 4),
         pct_homeowner_total_bin = cut(pct_homeowner_total, breaks = 4),) %>% 
  ## calculate block group centroids
  mutate(centroid = st_centroid(geometry),
         long_centroid = as.numeric(str_match(as.character(centroid), "\\((.*?),")[,2]),
         lat_centroid = as.numeric(str_match(as.character(centroid), ",(.*?)\\)")[,2])) %>% 
  ## check for missings in any/all column(s)
  rowwise() %>% 
  mutate(has_missing = any(is.na(across(everything()))),
         all_missing = all(is.na(across(median_income:pct_poverty_bin)))) %>% 
  ungroup() %>% 
  arrange(GEOID)
# rename(tot_occup = B24011_001E,
#        service = B24011_018E)

# Note: pct_service is service workers as percentage of civilian working pop >16yro


## look
str(atl)
summary(atl)


## standardize

atl_scale <- atl %>% 
  ## keep relevant columns
  select(GEOID, NAME, median_income,  
         matches("pct_.+[^(bin)]$"),
         long_centroid, lat_centroid, has_missing) %>% 
  ## standardize numeric columns
  mutate(                     
    across(c(median_income, starts_with("pct")),
           ~(scale(.) %>% as.vector()))
  )


## save ----
date <- format(Sys.Date(), "%Y%m%d")

## csv (no geometry)
atl %>% st_drop_geometry() %>% select(-centroid) %>% 
  write.csv(paste0(data, "atl_", date, ".csv"),
            row.names = F, na="")

atl_scale %>% st_drop_geometry() %>% 
  write.csv(paste0(data, "atl_standardized_", date, ".csv"),
            row.names = F, na="")

## shp
st_write(atl, paste0(data, "atl_", date, ".shp"))

## Rdata
save(atl, file = paste0(data, "atl_", date, ".Rdata"))
load(paste0(data, "atl_20240421.Rdata"))




