##############################
## DSIII FINAL PROJ
## VISUALIZING RESULTS
## Author: Kwadwo Alfajiri Shah
## Date: 19 Nov 23
## Date Updated: 27 Jan 24

## ------
## Housekeeping
## -----
## libraries
packages <- c("dplyr", "tidycensus", "sf", "ggplot2",
              "stringr", "waffle", "ggridges")
lapply(packages, require, character.only=T)

## setwd
root <- "~/visualizing-subjugation/dc/"
data <- paste0(root, "data/")
output <- paste0(root, "output/")
programs <- paste0(root, "programs/")
# shiny_data <- "~/ppol5202/final/stg_viz_subj_dc/data/"
setwd(root)

## source custom funcs
source(paste0(programs, "99_custom_functions.R"))


## ---------------------------------------- ##
## 1. read data and postprocessing        ----
## ---------------------------------------- ##

## ----- ##
## read data

## read levels data
dc <- read.csv(paste0(data, "dc_bg_characteristics_20240127.csv")) %>% 
  mutate(GEOID = as.character(GEOID))

## read clustering results
cluster.raw <- read.csv(paste0(data, "dc_bg_clusters_20240127.csv")) %>% 
  mutate(GEOID = as.character(GEOID))

## pull acs for geometries
census_api_key(readLines(paste0(data, "census_key.txt"))) %>% suppressWarnings()

geometries <- get_acs(
  state = "DC",
  geography = "block group",
  variables = "B19013_001",
  geometry = TRUE, 
  year = 2021
)

## read loadings for output
loadings.raw <- read.csv(paste0(data, "pca_loadings.csv"))


## ----- ##
## postprocessing

## merge geometries
cluster <- geometries %>% select(GEOID) %>% 
  left_join(cluster.raw, by = "GEOID")

## merge levels data
cluster <- cluster %>% 
  rename_with(~ paste0("std_", .), c(starts_with("pct"), contains("median_income"))) %>% 
  select(-NAME, -long_centroid, -lat_centroid, -has_missing) %>% 
  left_join(dc, by = "GEOID")

## factorize cluster var and create label col for plotly tooltip output
cluster <- cluster %>% 
  mutate(cluster2 = factor(cluster2, labels = c("Cluster 1", "Cluster 2")),
         label = paste("Pct Black: ", scales::percent(round(pct_black, 3)),
                       "\nPct service: ", scales::percent(round(pct_service, 3)),
                       "\nPct. Metro to work: ", scales::percent(round(pct_public_transit, 3)),
                       "\nPct. Commuting >30 mins.:", scales::percent(round(pct_commute30, 3)),
                       "\nMedian income: ", scales::dollar(median_income),
                       "\nUnemp. rate: ", scales::percent(round(pct_unemp, 3)),
                       "\nPoverty rate: ", scales::percent(round(pct_poverty, 3))
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


## factorize loadings feature names for order + labels
loadings <- loadings.raw %>% 
  mutate(X = factor(X,
                    levels = str_remove(indicators, "std_"),
                    labels = str_replace(names(indicators), "\\n", " "))) %>% 
  ## keep only 5 components so table fits on page
  select(-Z6, -Z7, -Z8)
loadings


## ---------------------------------------- ##
## 2. Visualize clusters                  ----
## ---------------------------------------- ##

## colors
colors <- paletteer::paletteer_d("palettetown::pikachu")
colors2 <- colors[c(4,1)]

## k=2
clusters_map2 <- cluster %>% 
  ggplot(aes(fill = factor(cluster2)))+
  geom_sf()+
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
  ggplot(aes(fill = factor(cluster4)))+
  geom_sf()+
  labs(title = "Clusters",
       subtitle = "KMeans, K=4",
       fill = NULL)+
  theme_void()
clusters_map4


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
density_income4 <- cluster %>% 
  ggplot(aes(x=median_income, color = factor(cluster4),
             fill = factor(cluster4)))+
  geom_density(alpha = 0.2)+
  theme_minimal()
density_income4

density_service4 <- cluster %>% 
  ggplot(aes(x=pct_service, color = factor(cluster4),
             fill = factor(cluster4)))+
  geom_density(alpha = 0.2)+
  theme_minimal()
density_service4

density_black4 <- cluster %>% 
  ggplot(aes(x=pct_black, color = factor(cluster4),
             fill = factor(cluster4)))+
  geom_density(alpha = 0.2)+
  theme_minimal()
density_black4

density_transit4 <- cluster %>% 
  ggplot(aes(x=pct_public_transit, color = factor(cluster4),
             fill = factor(cluster4)))+
  geom_density(alpha = 0.2)+
  theme_minimal()
density_transit4


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


## ---------------------------------------- ##
## 5. PCA Loadings Table                  ----
## ---------------------------------------- ##

stargazer::stargazer(loadings, type = "text",
                     summary = F)





