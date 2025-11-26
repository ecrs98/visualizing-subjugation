##############################
## VISUALIZING SUBJUGATION
## VISUALIZING RESULTS
## Author: Kwadwo Alfajiri Shah
## Date: 21 April 24
##

## ------
## Housekeeping
## -----
## libraries
packages <- c("dplyr", "tidycensus", "sf", "ggplot2",
              "stringr", "waffle", "ggridges")
lapply(packages, require, character.only=T)

## setwd
root <- "~/visualizing-subjugation/atl/"
data <- paste0(root, "data/")
output <- paste0(root, "output/")
programs <- paste0(root, "programs/")
# shiny_data <- "~/ppol5202/final/stg_viz_subj_dc/data/"
setwd(root)

## source custom funcs
source(paste0(programs, "999_custom_functions.R"))


## ---------------------------------------- ##
## 1. read data and postprocessing        ----
## ---------------------------------------- ##

## ----- ##
## read data

## read levels data
atl <- read.csv(paste0(data, "atl_20240421.csv")) %>% 
  mutate(GEOID = as.character(GEOID))

## read clustering results
cluster.raw <- read.csv(paste0(data, "20240421_atl_clustered.csv")) %>% 
  mutate(GEOID = as.character(GEOID))

## pull acs for geometries
geometries <- tigris::tracts(state = "GA", county = c("DeKalb", "Fulton"))


## ----- ##
## postprocessing

## merge geometries
cluster <- geometries %>% select(GEOID) %>% 
  left_join(cluster.raw, by = "GEOID")

## merge levels data
cluster <- cluster %>% 
  rename_with(~ paste0("std_", .), c(starts_with("pct"), contains("median_income"))) %>% 
  select(-NAME, -long_centroid, -lat_centroid, -has_missing) %>% 
  left_join(atl, by = "GEOID")

## factorize cluster var and create label col for plotly tooltip output
cluster <- cluster %>% 
  mutate(cluster2 = factor(cluster2, labels = c("Cluster 1", "Cluster 2")),
         cluster4 = factor(cluster4, levels = c(1,0,2,3), labels = paste0("Cluster ", 1:4)),
         label = paste("Pct Black: ", scales::percent(round(pct_black, 3)),
                       "\nMedian income: ", scales::dollar(median_income),
                       "\nPoverty rate: ", scales::percent(round(pct_poverty, 3)),
                       "\nHomeownership rate: ", scales::percent(round(pct_homeowner_total, 3))
                       )) 

## save shp file for shiny app
# date <- format(Sys.Date(), "%Y%m%d")
# st_write(cluster, paste0(shiny_data, date, "_dc_bg_cluster.shp"))


## long data for ridgeline plot

 # named indicators
indicators <- rev(c("std_pct_black", "std_pct_service", "std_pct_public_transit",
                    "std_pct_commute30",
                    "std_median_income", "std_pct_unemp",
                    "std_pct_poverty",
                    "std_pct_not_labor"))
names(indicators) <- rev(c("Percent Black", "Percent service workers", "Percent taking public transit\nto work", 
                       "Percent commuting to work\n30 minutes or longer",
                       "Median income", "Unemployment rate",
                       "Poverty rate",
                       "Percent not in the labor force"))

cluster_long <- cluster %>% 
  select(cluster2, starts_with("std_")) %>% 
  tidyr::pivot_longer(starts_with("std_"),
               names_to = "var", values_to = "st_dev") %>% 
  mutate(var = factor(var,
                      levels = indicators,
                      labels = names(indicators)))
head(cluster_long)






## ---------------------------------------- ##
## 2. Visualize clusters                  ----
## ---------------------------------------- ##

## colors
bin_colors <- c("#a6e3e3", "#5ee3e3", "#26aaaa", "#197171")
colors2 <- bin_colors[c(1,4)]

## map limits
xlim <- c(-84.42, -84.24)
ylim <- c(33.65, 33.79)

## cop city boundaries
cc <- st_read(paste0(data, "cc_shapefile/cc_shapefile.shp"))

## k=2
clusters_map2 <- cluster %>% 
  ggplot()+
  geom_sf(aes(fill = factor(cluster2)))+
  geom_sf(data = cc, fill = NA, color = "black", lwd = 1) +
  # coord_sf(xlim = xlim, ylim = ylim) +
  ggspatial::annotation_scale() +
  scale_fill_manual(values = colors2,
                    breaks = waiver(),
                    labels = c("Cluster 1", "Cluster 2"))+
  labs(title = "K-Means Clusters",
       subtitle = "K=2",
       fill = NULL)+
  theme_void()
clusters_map2

## k=4
clusters_map4 <- cluster %>% 
  ggplot()+
  geom_sf(aes(fill = factor(cluster4)))+
  geom_sf(data = cc, fill = NA, color = "black", lwd = 1) +
  # coord_sf(xlim = xlim, ylim = ylim) +
  ggspatial::annotation_scale() +
  scale_fill_manual(values = rev(bin_colors))+
  labs(title = "AI-classified map",
       # subtitle = "K=4",
       fill = NULL)+
  theme_void() 
clusters_map4




clusters_map2 | clusters_map4


## ---------------------------------------- ##
## 3. Visualize disparaties               ----
## ---------------------------------------- ##

## -----
## let's look at summary stats for each cluster to check for
## disparaties

## k = 2
stats2 <- cluster %>% 
  group_by(cluster2) %>% 
  summarise(across(where(is.numeric), mean, na.rm=T))

## ^ these are still standardized so let's do plots instead

## -----
## density plots

## k = 2
density_income2 <- cluster %>% 
  ggplot(aes(x=median_income, color = factor(cluster2),
             fill = factor(cluster2)))+
  geom_density(alpha = 0.2)+
  scale_fill_manual(values = colors2,
                    breaks = waiver())+
  scale_color_manual(values = colors2,
                     breaks = waiver())+
  theme_minimal()
density_income2

density_service2 <- cluster %>% 
  ggplot(aes(x=pct_service, color = factor(cluster2),
             fill = factor(cluster2)))+
  geom_density(alpha = 0.2)+
  theme_minimal()
density_service2

density_black2 <- cluster %>% 
  ggplot(aes(x=pct_black, color = factor(cluster2),
             fill = factor(cluster2)))+
  geom_density(alpha = 0.2)+
  theme_minimal()
density_black2

density_transit2 <- cluster %>% 
  ggplot(aes(x=pct_public_transit, color = factor(cluster2),
             fill = factor(cluster2)))+
  geom_density(alpha = 0.2)+
  theme_minimal()
density_transit2


## k = 4

plot_density <- function(var_, scale = "percent"){
  
  if(scale == "percent"){
    scale_x_labels = scale_x_continuous(labels = scales::percent)
  }else{
    scale_x_labels = scale_x_continuous(labels = scales::dollar)
  }
  
  density <- cluster %>% 
    ggplot(aes(x=.data[[var_]], color = factor(cluster4),
               fill = factor(cluster4)))+
    geom_density(alpha = 0.2)+
    scale_color_manual(values = rev(bin_colors)) +
    scale_fill_manual(values = rev(bin_colors)) + 
    scale_x_labels +
    labs(fill = NULL, color = NULL,
         x = NULL, title = var_) +
    theme_minimal()
  return(density)
}

p1 <- plot_density("median_income", scale = "dollar")
p2 <- plot_density("pct_black")
p3 <- plot_density("pct_poverty")
p4 <- plot_density("pct_homeowner_total")

(p1 | p2) / (p3 | p4)


density_income4 <- cluster %>% 
  ggplot(aes(x=median_income, color = factor(cluster4),
             fill = factor(cluster4)))+
  geom_density(alpha = 0.2)+
  theme_minimal()
density_income4

# density_service4 <- cluster %>% 
#   ggplot(aes(x=pct_service, color = factor(cluster4),
#              fill = factor(cluster4)))+
#   geom_density(alpha = 0.2)+
#   theme_minimal()
# density_service4

density_black4 <- cluster %>% 
  ggplot(aes(x=pct_black, color = factor(cluster4),
             fill = factor(cluster4)))+
  geom_density(alpha = 0.2)+
  theme_minimal()
density_black4

# density_transit4 <- cluster %>% 
#   ggplot(aes(x=pct_public_transit, color = factor(cluster4),
#              fill = factor(cluster4)))+
#   geom_density(alpha = 0.2)+
#   theme_minimal()
# density_transit4

density_poverty4 <- cluster %>% 
  ggplot(aes(x=pct_poverty, color = factor(cluster4),
             fill = factor(cluster4)))+
  geom_density(alpha = 0.2)+
  theme_minimal()
density_poverty4

density_homeowner4 <- cluster %>% 
  ggplot(aes(x=pct_homeowner_total, color = factor(cluster4),
             fill = factor(cluster4)))+
  geom_density(alpha = 0.2)+
  theme_minimal()
density_homeowner4


(density_income4 | density_black4) / (density_poverty4 | density_homeowner4)



## ----- ##
## ridgeline plot

## all characteristics distributions by cluster

ridgeline <- cluster_long %>% 
  ggplot(aes(x = st_dev, y = var, fill = cluster2,
             color = cluster2))+
  geom_density_ridges(alpha = 0.2,
                      rel_min_height = 0.01)+
  geom_vline(xintercept = 0, linetype = 2,
             color = "dimgrey")+
  scale_fill_manual(values = colors2,
                    breaks = waiver())+
  scale_color_manual(values = colors2,
                     breaks = waiver())+
  scale_x_continuous(breaks = seq(-2,6,2))+
  labs(title = "Block group characteristic distributions by cluster",
       x = "Std. Dev.",
       y = NULL,
       fill = NULL,
       color = NULL,
       caption = "Note: Values are standardized so that the mean of \neach variable is 0.")+
  theme_minimal()
ridgeline



## ---------------------------------------- ##
## 4. Waffle plots                        ----
## ---------------------------------------- ##

## -----
## cluster waffle

waffle_clusters <- cluster %>% 
  cluster_count_summarise() %>% 
  ggplot(aes(fill = factor(cluster2), values = clust_count))+
  geom_waffle(make_proportional = T,
              flip = F,
              color = "white",
              size = 3)+
  scale_fill_manual(values = colors2,
                    breaks = waiver(),
                    labels = c("Cluster 1", "Cluster 2"))+
  labs(title = NULL,
       fill = NULL)+
  theme_void()
waffle_clusters


## -----
## thresholded waffle plot

threshold <- .90

waffle_threshold <- cluster %>% 
  filter(pct_black >= threshold) %>% 
  cluster_count_summarise() %>% 
  ggplot(aes(fill = factor(cluster2), values = clust_count))+
  geom_waffle(make_proportional = T,
              flip = F,
              color = "white",
              size = 3)+
  scale_fill_manual(values = colors2,
                    breaks = waiver(),
                    labels = c("Cluster 1", "Cluster 2"))+
  labs(title = NULL,
       fill = NULL)+
  theme_void()
waffle_threshold







