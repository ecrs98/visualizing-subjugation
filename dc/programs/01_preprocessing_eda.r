##############################
## DSIII FINAL PROJ
## PREPROCESSING AND EDA
## Author: Kwadwo Alfajiri Shah
## Date: 25 Oct 23
## Date Updated: 27 Jan 24
## 

## --------------- ##
## Housekeeping  ----
## --------------- ##

## libraries
packages <- c("dplyr", "tidycensus", "sf", "ggplot2",
              "stringr")
lapply(packages, require, character.only=T)

## setwd
root <- "~/visualizing-subjugation/dc/"
data <- paste0(root, "data/")
output <- paste0(root, "output/")
program <- paste0(root, "program/")
setwd(root)


## ---------------------------------------- ##
## 1. Pull data                           ----
## ---------------------------------------- ##

## enter api key
census_api_key(readLines(paste0(data, "census_key.txt"))) %>% suppressWarnings()


## define variables to pull
## these are variable codes from the ACS
 # for some reason occupation is only available at the block group
 # level through sexed cross-tabs... so that's what we're gonna work with

pull_vars <- c("B19013_001",    # median income
               
               ## occupation
               "C24010_001",   # occup total population
               # "C24010_002",    # occup male total
               # "C24010_003",   # mgmt, business, science, arts - male
               "C24010_019",     # service - male
               "C24010_055",     # service - female
               
               ## employment status
               "B23025_001",    # employment total
               "B23025_005",    # unemployed
               "B23025_007",     # not in labor force
               
               ## race
               "B02009_001",   # black - alone or in combo
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
               "B17017_002"   # below poverty level
               
               ## commute - subject tables
               # "S0801_C01_009",   # public transportation
               # "S0801_C01_046",   # mean travel time
               # "S0801_C01_026",   # worker pop - didnt work from home
               # "S0801_C01_001"    # worker pop - total
)


## pull from api
dc.raw <- get_acs(
    state = "DC",
    geography = "block group",
    variables = pull_vars,
    geometry = TRUE, # tells tidycensus to add tigris geometries
    year = 2021
)



## clean
dc <- dc.raw %>% 
    ## reshape - long to wide
    select(-moe) %>% 
    tidyr::pivot_wider(names_from = "variable", values_from = "estimate") %>% 
    ## create percentages
    mutate(median_income = B19013_001,
           pct_service = (C24010_019 + C24010_055)/C24010_001,
           pct_unemp = B23025_005 / B23025_001,
           pct_not_labor = B23025_007 / B23025_001,
           pct_black = B02009_001 / B01003_001,
           pct_public_transit = B08134_061/B08134_001,
           pct_commute30 = rowSums(cbind(B08134_007,B08134_008,B08134_009,B08134_010))/B08134_001,
           # pct_commute25 = rowSums(cbind(B08134_006,B08134_007,B08134_008,B08134_009,B08134_010))/B08134_001,
           pct_poverty = B17017_002 / B17017_001
    ) %>% 
    ## create binned vars
    mutate(pct_service_bin = cut(pct_service, breaks=4),
           pct_unemp_bin = cut(pct_unemp, breaks=4),
           pct_not_labor_bin = cut(pct_not_labor, breaks=4),
           pct_black_bin = cut(pct_black, breaks = 4),
           pct_public_transit_bin = cut(pct_public_transit, breaks = 4, na.rm=T),
           pct_poverty_bin = cut(pct_poverty, breaks = 4)) %>% 
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




## subset and standardize -- dropping completely missing block groups
dc_scale <- dc %>% 
  ## filter out missing groups    
    filter(all_missing==F) %>%     
  ## keep relevant columns
    select(GEOID, NAME, median_income,  
           matches("pct_.+[^(bin)]$"),
           long_centroid, lat_centroid, has_missing) %>% 
  ## standardize numeric columns
    mutate(                             
        across(c(median_income, starts_with("pct")),
               ~(scale(.) %>% as.vector()))
    )


## save
date <- format(Sys.Date(), "%Y%m%d")

dc %>% st_drop_geometry() %>% select(-centroid) %>% 
    write.csv(paste0(data, "dc_bg_characteristics_", date, ".csv"),
          row.names = F, na="")

dc_scale %>% st_drop_geometry() %>% 
    write.csv(paste0(data, "dc_bg_characteristics_scaled_", date, ".csv"),
              row.names = F, na="")



## ------------- ##
## summary stats

dc_sum <- dc %>% 
    select(pct_service, pct_unemp, pct_not_labor, pct_black) %>% 
    st_drop_geometry() %>% 
    as.data.frame()

stargazer::stargazer(dc_sum,type="text", summary=T)





## ---------------------------------------- ##
## 2. EDA                           ----
## ---------------------------------------- ##


## --------------- ##
##  map features ----

## block groups and centroids
block_groups <- dc %>% 
    ggplot()+
    geom_sf(aes(fill=factor(GEOID)))+
    geom_sf(data = dc$centroid, size = 0.5)+
    scale_fill_discrete(guide=NULL)+
    labs(title = "Block groups and centroids",
         caption = "Note: Distribution of colors are meaningless for this chart.")+
    theme_void()
block_groups

 # using long/lat
block_groups_longlat <- dc %>% 
    ggplot(aes(x = long_centroid, y = lat_centroid))+
    geom_point()
block_groups_longlat



## service workers
service_pct_bin <- dc %>% 
    ggplot(aes(fill=pct_service_bin))+
    geom_sf()+
    labs(title = "Service workers",
         subtitle = "Proportion of working population employed in service positions.",
         fill = "")+
    theme_void()
service_pct_bin



## unemp rate
unemp_rate_bin <- dc %>% 
    ggplot(aes(fill=pct_unemp_bin))+
    geom_sf()+
    labs(title = "Unemployment rate",
         fill = NULL)+
    theme_void()
unemp_rate_bin



## not in labor force
not_labor_bin <- dc %>% 
    ggplot(aes(fill=pct_not_labor_bin))+
    geom_sf()+
    labs(title = "Not in labor force",
         subtitle = "Proportion of working age population not in labor force",
         fill = NULL)+
    theme_void()
not_labor_bin



## black pop
pct_black <- dc %>% 
    ggplot(aes(fill = pct_black))+
    geom_sf()+
    labs(title = "Black population",
         subtitle = "Proportion of the population that is Black",
         fill = NULL)+
    theme_void()
pct_black

pct_black_bin <- dc %>%
    ggplot(aes(fill = pct_black_bin))+
    geom_sf()+
    labs(title = "Black population",
         subtitle = "Proportion of the population that is Black",
         fill = NULL)+
    theme_void()
pct_black_bin



## public transit
hist_transit <- hist(dc$pct_public_transit)

public_transit <- dc %>% 
    ggplot(aes(fill = pct_public_transit))+
    geom_sf()+
    labs(title = "Workers who take public transportation to work",
         fill = NULL)+
    theme_void()
public_transit

public_transit_bin <- dc %>% 
    ggplot(aes(fill = pct_public_transit_bin))+
    geom_sf()+
    labs(title = "Workers who take public transportation to work",
         fill = NULL)+
    theme_void()
public_transit_bin



## commute
 # 30 mins or more
commute30 <- dc %>% 
    ggplot(aes(fill = pct_commute30))+
    geom_sf()+
    labs(title = "Commute 30 mins or longer",
         fill = NULL)+
    theme_void()
commute30

commute30_bin <- dc %>% 
    ggplot(aes(fill = cut(pct_commute30, breaks = 4)))+
    geom_sf()+
    labs(title = "Commute 30 mins or longer",
         fill = NULL)+
    theme_void()
commute30_bin

 # 25 mins or more
commute25 <- dc %>% 
    ggplot(aes(fill = pct_commute25))+
    geom_sf()+
    labs(title = "Commute 25 mins or longer",
         fill = NULL)+
    theme_void()
commute25



## poverty rates
poverty <- dc %>% 
    ggplot(aes(fill = pct_poverty))+
    geom_sf()+
    labs(title = "Poverty rate",
         fill = NULL)+
    theme_void()
poverty

poverty_bin <- dc %>% 
    ggplot(aes(fill = pct_poverty_bin))+
    geom_sf()+
    labs(title = "Poverty rate",
         fill = NULL)+
    theme_void()
poverty_bin



## missing values - in any column
has_missing = dc %>% 
    ggplot(aes(fill = has_missing))+
    geom_sf()+
    labs(title = "Has a missing value in any column",
         fill = NULL)+
    theme_void()
has_missing


