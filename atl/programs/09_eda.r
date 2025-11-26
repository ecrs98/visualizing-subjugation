########################################
## VISUALIZING SUBJUGATION - ATL
## EDA
## Author: Kwadwo Alfajiri Shah
## Date: 17 Mar 24
## Updated:
##


## ----------------- ##
## 0. housekeeping ----
## ----------------- ##

## libraries
packages <- c("dplyr", "tidycensus", "sf", "ggplot2", "stringr", "patchwork")
lapply(packages, require, character.only=T)

## setwd
root <- "~/visualizing-subjugation/atl/"
data <- paste0(root, "data/")
output <- paste0(root, "output/")
programs <- paste0(root, "programs/")
setwd(root)

## load data ----
load(paste0(data, "atl_20240421.Rdata"))



## ---------------------------------------- ##
## 1. Map features                       ----
## ---------------------------------------- ##

## cop city shape
cc <- st_read(paste0(data, "cc_shapefile/cc_shapefile.shp"))


## bin colors
bin_colors <- c("#a6e3e3", "#5ee3e3", "#26aaaa", "#197171")

## map limits
xlim <- c(-84.42, -84.24)
ylim <- c(33.65, 33.79)

## cop city location
cc_long <- 33.69347
cc_lat <- -84.325

## map continuous vars
map <- atl %>% 
  ggplot() +
  geom_sf(aes(fill = median_income)) +
  geom_sf(data = cc, fill = NA, color = "black", lwd = 1) +
  # geom_point(x = cc_lat, y = cc_long,
  #            size = 3, shape = 23, fill = "yellow") +
  coord_sf(xlim = xlim, ylim = ylim) +
  scale_fill_gradient(low = bin_colors[4], high = bin_colors[1],
                      labels = scales::dollar) +
  labs(title = "median_income",
       subtitle = NULL,
       fill = "") +
  theme_void()
map



## mapping func -- binned vars
map_features <- function(feature_name, title = NULL, subtitle = NULL){
  map <- atl %>% 
    ggplot() +
    geom_sf(aes(fill = .data[[feature_name]])) +
    geom_sf(data = cc, fill = NA, color = "black", lwd = 1) +
    # geom_point(x = -84.325, y = 33.69347,
    #            size = 3, shape = 23, fill = "yellow") +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_manual(values = bin_colors, na.value = "white") +
    labs(title = feature_name,
         subtitle = subtitle,
         fill = "") +
    theme_void()
  return(map)
}


## create binned maps
bin_vars <- names(atl)[str_detect(names(atl), "bin")]
map_bin_list <- lapply(bin_vars, map_features)
names(map_bin_list) <- bin_vars

p1 <- map_bin_list[["pct_black_bin"]]
p2 <- map_bin_list[["pct_nonwhite_bin"]]
p3 <- map_bin_list[["pct_poverty_bin"]]
p4 <- map_bin_list[["pct_homeowner_bin"]]
p5 <- map_bin_list[["pct_homeowner_total_bin"]]


## View
(p1 + p2) / (p3 + p4)

p1 | (p4 / p5)

# map_bin_list[["pct_unemp_bin"]]    ## this map not best cuz we wanna know where high unemp rates are. create custom thresholds
# map_bin_list[["pct_service_bin"]]
# map_bin_list[["pct_public_transit_bin"]]
# map_bin_list[["pct_not_labor_bin"]]

## add hispanic pop
## add homeownership






## block groups and centroids
map_centroids <- atl %>% 
  ggplot()+
  geom_sf(aes(fill = county))+
  geom_sf(data = atl$centroid, size = 0.5)+
  # scale_fill_discrete(guide=NULL)+
  labs(title = "Census tract centroids")+
  theme_void()
map_centroids




