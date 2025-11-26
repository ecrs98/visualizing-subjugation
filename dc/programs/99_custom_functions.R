##############################
## SUBJUGATION IN DC
## Custom Helper Functions
## Author: Alfa
## Date: 9 Dec 23
##

## --------------- ##
## housekeeping ----
## --------------- ##

packages <- c("dplyr", "tidycensus", "sf", "ggplot2",
              "stringr")
lapply(packages, require, character.only=T)


## ------------------------------ ##
## create waffle plot ----
## ------------------------------ ##

## cluster counts
cluster_count_summarise <- function(data){
  count_data <- data %>% 
    filter(!is.na(cluster2)) %>% 
    group_by(cluster2) %>% 
    summarise(clust_count = n()) %>% 
    mutate(total = sum(clust_count)) %>% 
    arrange(desc(cluster2))
  return(count_data)
}

## making the plot
plot_waffle <- function(data){
  
  waffle_data <- data %>% 
    cluster_count_summarise()
  
  waffle <- waffle_data %>% 
    ggplot(aes(fill = factor(cluster2), values = clust_count))+
    geom_waffle(make_proportional = T,
                flip = F,
                color = "white",
                size = 3)+
    scale_fill_manual(values = colors2,
                      breaks = waiver(),
                      labels = c("Cluster 1", "Cluster 2"))+
    labs(title = NULL,
         fill = NULL,
         caption = paste0("N = ", waffle_data$total))+
    theme_void()
  return(waffle)
}

## add thresholding capabilities
plot_waffle_threshold <- function(data, var_, threshold, a_b = "above"){
  
  if(a_b == "above"){ ## plot above threshold
    waffle_threshold <- data %>%
      filter(get(var_) >= threshold) %>% 
      plot_waffle()
  } else{ ## plot below threshold
    waffle_threshold <- data %>%
      filter(get(var_) <= threshold) %>% 
      plot_waffle()
  }
  
  return(waffle_threshold)
  
}


## test (var `cluster` comes from ./results_viz.r)
# cluster %>% plot_waffle()
# cluster %>% plot_waffle_threshold("pct_unemp", 0, a_b="below")





