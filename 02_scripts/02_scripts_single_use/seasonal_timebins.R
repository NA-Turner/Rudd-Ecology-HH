#altering the seasons to better capture rudd behavioural blocks


rudd_dets_P <- rudd_dets_P %>%
 mutate(
  month_num = as.numeric(format(detection_timestamp_EST, "%m")),
  day_num = as.numeric(format(detection_timestamp_EST, "%d")),
  season = case_when(
   (month_num == 4 & day_num >= 25) | month_num == 5 | (month_num == 6 & day_num <= 6) ~ "Spring",
   (month_num == 6 & day_num >= 7) | month_num %in% c(7, 8, 9) | (month_num == 10 & day_num <= 3) ~ "Summer",
   (month_num == 10 & day_num >= 4) | month_num == 11 & day_num <= 17 ~ "Fall",
   (month_num == 11 & day_num >= 18) | month_num %in% c(12, 1, 2, 3) | (month_num == 4 & day_num <= 24) ~ "Winter",
   TRUE ~ NA_character_
  )
 )

###################
#new seasonal blocks 

#look at monthly depth use or even weekly depth use boxplots
#movement rates, distnace travelled per unit of time




##num of fish detected on receiver plots
#using summarize funciton in GLATOS package 


##using this same dataframe above we can make seasonal bubble plots based on num_fish or num_detections
#do yearly or combine all years worth of data
#use script XXX to load in map plotting data 
library(rgdal)
library(ggplot2)
library(sf)

shorelinemap <- st_read("./01_data/02_processed_files/map data/HH_Poly_Mar2025/HH_WaterLinesToPoly_21Mar2025.shp")
shorelinemap <- st_transform(shorelinemap, crs = 4326)

#add in the other shapefile


HH<-ggplot(shorelinemap) +
 geom_sf(fill = "lightblue", color = "black", size = 0.5) +
 theme_minimal() +
 theme(axis.text = element_text(size = 8))
#use glatos rpackage to summarize detections by num_fish and num_dets

HH

#now split it up by season 
##pull out detection data based on seasons 
ruddsummer<-rudd_dets %>% filter(season=="Summer")
ruddfall<-rudd_dets %>% filter(season=="Fall")
ruddwinter<-rudd_dets %>% filter(season=="Winter")
ruddspring<-rudd_dets %>% filter(season=="Spring")

rudd_sum_summer<-summarize_detections(ruddsummer,location_col = "station", summ_type = "location")
all_ruddsummer_sf <- st_as_sf(
 rudd_sum_summer,
 coords = c("mean_lon", "mean_lat"),
 crs = 4326
) |> 
 st_transform(st_crs(shorelinemap))

bubsummer<-ggplot() +
 geom_sf(
  data = shorelinemap,
  fill = "grey96",
  color = "grey70",
  linewidth = 0.2
 ) +
 geom_sf(
  data = all_ruddsummer_sf,
  aes(color = num_fish),
  size = 2.8,
  alpha = 0.85
 ) +
 scale_color_viridis_c(
  option = "viridis",
  direction = -1,
  trans = "sqrt",
  name = "Number of fish"
 ) +
 coord_sf() +
 labs(title="Summer")
theme(
 legend.position = "right",
 plot.margin = margin(6, 6, 6, 6)
)


ggsave("summerbub.png", plot = bubsummer, width = 12, height = 8, dpi = 300)


###
rudd_sum_fall<-summarize_detections(ruddfall,location_col = "station", summ_type = "location")

all_ruddfall_sf <- st_as_sf(
 rudd_sum_fall,
 coords = c("mean_lon", "mean_lat"),
 crs = 4326
) |> 
 st_transform(st_crs(shorelinemap))

bubfall<-ggplot() +
 geom_sf(
  data = shorelinemap,
  fill = "grey96",
  color = "grey70",
  linewidth = 0.2
 ) +
 geom_sf(
  data = all_ruddfall_sf,
  aes(color = num_fish),
  size = 2.8,
  alpha = 0.85
 ) +
 scale_color_viridis_c(
  option = "viridis",
  direction = -1,
  trans = "sqrt",
  name = "Number of fish"
 ) +
 coord_sf() +
 labs(title="Fall")
theme(
 legend.position = "right",
 plot.margin = margin(6, 6, 6, 6), 
)


ggsave("fallbub.png", plot = bubfall, width = 12, height = 8, dpi = 300)

#################
rudd_sum_winter<-summarize_detections(ruddwinter,location_col = "station", summ_type = "location")

all_ruddwinter_sf <- st_as_sf(
 rudd_sum_winter,
 coords = c("mean_lon", "mean_lat"),
 crs = 4326
) |> 
 st_transform(st_crs(shorelinemap))

bubwinter<-ggplot() +
 geom_sf(
  data = shorelinemap,
  fill = "grey96",
  color = "grey70",
  linewidth = 0.2
 ) +
 geom_sf(
  data = all_ruddwinter_sf,
  aes(color = num_fish),
  size = 2.8,
  alpha = 0.85
 ) +
 scale_color_viridis_c(
  option = "viridis",
  direction = -1,
  trans = "sqrt",
  name = "Number of fish"
 ) +
 coord_sf() +
 labs(title="Winter")
theme(
 legend.position = "right",
 plot.margin = margin(6, 6, 6, 6), 
)
ggsave("winterbub.png", plot = bubwinter, width = 12, height = 8, dpi = 300)


#################
rudd_sum_spring<-summarize_detections(ruddspring,location_col = "station", summ_type = "location")

all_ruddspring_sf <- st_as_sf(
 rudd_sum_spring,
 coords = c("mean_lon", "mean_lat"),
 crs = 4326
) |> 
 st_transform(st_crs(shorelinemap))

bubspring<-ggplot() +
 geom_sf(
  data = shorelinemap,
  fill = "grey96",
  color = "grey70",
  linewidth = 0.2
 ) +
 geom_sf(
  data = all_ruddspring_sf,
  aes(color = num_fish),
  size = 2.8,
  alpha = 0.85
 ) +
 scale_color_viridis_c(
  option = "viridis",
  direction = -1,
  trans = "sqrt",
  name = "Number of fish"
 ) +
 coord_sf(expand=FALSE) +
 labs(title="Spring")
theme(
 legend.position = "right",
 plot.margin = margin(6, 6, 6, 6), 
)

ggsave("springbub.png", plot = bubspring, width = 12, height = 8, dpi = 300)


allbubs<-grid.arrange(bubwinter, bubspring, bubsummer, bubfall, ncol=2)
ggsave("seasonalbub.png", plot = allbubs, width = 12, height = 8, dpi = 300)
