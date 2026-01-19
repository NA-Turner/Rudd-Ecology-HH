# data vis 
#possibly delete later from larger github paper package 
######################
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

#seasonaggplot2#seasonal and monthly depth use to see patterns 
#assign month column and season column to filtered detection dataframe

rudd_dets <- readRDS("~/For Github/Rudd-Ecology-HH/01_data/03_large_files_LFS/02_processed_files/rudd_detections_CLEAN.rds")

#add column to DF for months 
rudd_dets <- rudd_dets %>%
 mutate(month = format(detection_timestamp_EST, "%b"))

#add season based on Larocque et al 2024 and 2020 papers
#that used water temperature do define seasons
rudd_dets <- rudd_dets %>%
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

##depth over time with season at top as rug plot

#add jsut a date column 
rudd_dets <- rudd_dets %>%
 mutate(
  date = format(detection_timestamp_EST, "%Y-%m-%d"))

#make sure pressure tags only go forward from here
rudd_dets_P<-rudd_dets %>% filter (SensorType == "1")

# Calculate daily mean depth (if you have multiple observations per day)
dailydepth_means <- rudd_dets_P %>%
 group_by(date) %>%
 summarise(mean_depth = mean(Sensor.Val, na.rm = TRUE), .groups = 'drop')

# Calculate daily min/max for the gray ranges
daily_ranges <- rudd_dets_P %>%
 group_by(date) %>%
 summarise(
  min_depth = min(Sensor.Val, na.rm = TRUE),
  max_depth = max(Sensor.Val, na.rm = TRUE),
  mean_depth = mean(Sensor.Val, na.rm = TRUE),
  .groups = 'drop'
 )

unique(rudd_dets_temp$animal_id)

#make sure all are in date format

rudd_dets_P <- rudd_dets_P %>%
 mutate(date = as.Date(date))

dailydepth_means <- dailydepth_means %>%
 mutate(date = as.Date(date))

daily_ranges <- daily_ranges %>%
 mutate(date = as.Date(date))


# Sample rugg data for performance (take every 10th or 100th detection)
rudd_dets_sampled <- rudd_dets_P %>%
 slice(seq(1, n(), by = 10))  # Take every 10th row



#this takes a while to run 

# Now plot with MUCH fewer lines
depthplot<-ggplot() +
 # Use daily ranges instead of individual detections
 geom_linerange(data = daily_ranges,
                aes(x = date, ymin = min_depth, ymax = max_depth),
                color = "gray80", alpha = 0.3) +
 geom_point(data = daily_ranges,
            aes(x = date, y = mean_depth),
            size = 2, alpha = 0.7) +
 geom_rug(data = rudd_dets_sampled,
          aes(x = date, color = season),
          sides = "t",
          length = unit(0.05, "npc"),
          alpha = 0.8)+
 scale_color_manual(values = c("Spring" = "#2ca25f", 
                              "Summer" = "#fee08b",
                              "Fall" = "#f46d43",
                              "Winter" = "skyblue2"),
                   name = "Thermal Season") +
 scale_y_reverse(breaks = seq(0, 30, 2)) +
 scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
 labs(x = "Date", y = "Mean Depth (m)") +
 theme_classic() +
 theme(legend.position = "right",
       axis.text = element_text(size = 10),
       axis.title = element_text(size = 12),
       plot.title = element_text(face = "bold", hjust = 0))

ggsave("depth_rugplot1.png", plot = depthplot, width = 12, height = 8, dpi = 300)


# Calculate mean and confidence intervals for each month
month_ci <- rudd_dets %>%
 group_by(month) %>%
 summarise(
  mean_depth = mean(Sensor.Val, na.rm = TRUE),
  se = sd(Sensor.Val, na.rm = TRUE) / sqrt(n()),
  ci_lower = mean_depth - 1.96 * se,
  ci_upper = mean_depth + 1.96 * se,
  .groups = 'drop'
 )

# Make sure month is ordered
rudd_dets <- rudd_dets %>%
 mutate(month = factor(month, levels = month))

month_ci <- month_ci %>%
 mutate(month = factor(month, levels = month))

# Create plot with boxplot and confidence intervals
ggplot(rudd_dets, aes(x = month, y = Sensor.Val)) +
 geom_boxplot(fill = "steelblue", alpha = 0.6) +
 geom_pointrange(data = month_ci, 
                 aes(x = month, y = mean_depth, ymin = ci_lower, ymax = ci_upper),
                 color = "red", size = 0.5, linewidth = 1) +
 labs(
  x = "Month",
  y = "Depth (m)"
 ) +
 theme_minimal() +
 scale_y_reverse(breaks = seq(0, 30, 2)) +
 theme(
  panel.grid.major.x = element_blank()
 )


###based on the grey min max values for minimum depths there is never a season where they seem different (except in winter 2023)
#could they be making diel movements during the winter???
ruddwinter_24_25<-rudd_dets_temp %>%filter(year=="2024" & "2025")





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


##########################
#####below was copied over from old code might just need to be deleted ?
########################
unique(dets_tagdeets$`Date tag dies`)

##more details on fish that left the harbour
#fish 1386
#fish 9159
#fish 508

###plotting rec locations single location only
#load in receiver 

library(dplyr)
library(ggplot2)
library(ggrepel)  # for nice labels that don't overlap
#filter by HAM and WLN
unique(GLATOS_receiverLocations_20260112_221824$glatos_array)

recs_hh_WLN <- GLATOS_receiverLocations_20260112_221824 %>% 
 filter(glatos_array %in% c("HAM", "WLN"))

#add deplopyment year to dataframe based on deployment date
recs_hh_WLN$year <- format(as.Date(recs_hh_WLN$deploy_date_time), "%Y")
#keep only years included in this study
recs_hh_WLN <- recs_hh_WLN %>%
 filter(year >= 2020 & year <= 2025)
unique(recs_hh_WLN$year)

##and just 2025 for next plot 
recs_hh_WLN2025 <- recs_hh_WLN %>%
 filter(year == 2025)

##################
LKOmap <- st_read("./01_data/02_processed_files/map data/LKO_shorelinemap/LakeOntShoreline_MajorWaters.shp")
LKOmap <- st_transform(LKOmap, crs = 4326)

#set coordiantes to show HH and western LKO (around where we had furtherest detection)
x_limits_lko<- c(-79.9, -79.5) 
y_limits_lko<- c(43.18, 43.5)
#
x_limits_HHall <- c(-79.94, -79.78)  # Define your desired range for x-axis
y_limits_HHall <- c(43.265, 43.315)  # Define your desired range for y-axis

HHmap<-ggplot(LKOmap) +
 geom_sf(fill = "lightblue", color = "black", size = 0.5) +
 theme_minimal() +
 coord_sf(xlim = x_limits_HHall, ylim = y_limits_HHall) +
 theme(axis.text = element_text(size = 8))
#use glatos rpackage to summarize detections by num_fish and num_dets
HHmap <- st_transform(HHmap, crs = 4326)


recs_hh_WLN$year<-as.factor(recs_hh_WLN$year)

library(ggrepel)

recmaps <- ggplot() +
 geom_sf(data = LKOmap, fill = "lightblue", color = "black", linewidth=0.5) +
 coord_sf(xlim = x_limits_HHall, ylim = y_limits_HHall) +
 # Main data points on all panels
 geom_point(data = recs_hh_WLN,
            aes(x = deploy_long, y = deploy_lat), 
            size = 1.2,
            color = "darkred") +
 facet_wrap(~year) +
 guides(fill = "none", shape = "none") +
 theme_minimal() +
 theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 13), 
       axis.text.y = element_text(size = 13),
       strip.text = element_text(size = 14, face = "bold"),  # face inside element_text()
       legend.position = "none")

recmaps

ggsave("recmaps-2020 to 2025.png", plot = recmaps, width = 12, height = 8, dpi = 300)
#############
#isolate for just the 2025 year to show all recs in HH and WLKO and add labels 
#########################
x_limits_lko<- c(-79.9, -79.5) 
y_limits_lko<- c(43.18, 43.5)

recs2025HHLKO <- ggplot() +
 geom_sf(data = LKOmap, fill = "lightblue", color = "black", linewidth=0.5) +
 coord_sf(xlim = x_limits_lko, ylim = y_limits_lko) +
 # Main data points on all panels
 geom_point(data = recs_hh_WLN2025,
            aes(x = deploy_long, y = deploy_lat), 
            size = 1.2,
            color = "darkred") +
 geom_text(
  data = recs_hh_WLN2025,
  aes(x = deploy_long,
      y = deploy_lat,
      label = station),
  size = 3.5,
  nudge_y=0.002, color="black"
 ) +
 facet_wrap(~year) +
 guides(fill = "none", shape = "none") +
 theme_minimal() +
 theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 13), 
       axis.text.y = element_text(size = 13),
       strip.text = element_text(size = 14, face = "bold"),  # face inside element_text()
       legend.position = "none")

#map is really messy 
#exporting for mapping in Arcgis 
write.csv(recs_hh_WLN, "reclocs_2020-2025.csv")

















#######################

unique(pos$animal_id)

rudd1386 <- subset(rudd_dets, animal_id == 'Rudd_1386')

library(leaflet)

library(ggplot2)
library(dplyr)

# Create movement transitions between receivers
movements1386 <- rudd1386 %>%
 arrange(animal_id, detection_timestamp_EST) %>%
 group_by(animal_id) %>%
 mutate(
  next_station = lead(station),
  next_lat = lead(deploy_lat),
  next_long = lead(deploy_long)
 ) %>%
 filter(!is.na(next_station))

# Plot movement network
ggplot() +
 geom_segment(data = movements1386,
              aes(x = deploy_long, y = deploy_lat,
                  xend = next_long, yend = next_lat),
              arrow = arrow(length = unit(0.2, "cm")),
              alpha = 0.3, color = "red") +
 labs(title = "Fish Movement Network",
      x = "Longitude", y = "Latitude") +
 theme_minimal()

###using gganimate                             
library(gifski)
library(gapminder)
library(gganimate)
library("av")

gc()

as.data.frame(clipped_plusremovals) 
clipped_plusremovals$detection_timestamp_utc <- as.POSIXct(clipped_plusremovals$detection_timestamp_utc, tz = "EDT")

head(clipped_plusremovals)
pos <- interpolate_path(clipped_plusremovals, int_time_stamp=86400/24)

head(pos)
#make each individual a diff colour
n<-unique(pos$animal_id)
col.rainbow<-rainbow(length(n))

pos3<-data.frame()

for (i in 1:length(n)){
 
 pos2<-pos[pos$animal_id==n[i],]
 pos2$Colour<-col.rainbow[i]
 
 
 pos3<-rbind(pos3,pos2)
 
}

pos<-pos3
unique(pos$animal_id)

##shows a full coloured point if a true detection and not interpolation
pos$fill<-ifelse(pos$record_type =="detection", 27,1)

# convert pos2 to data.table
setDT(pos)

# extract unique bin_timestamp from the interpolated data
int <- unique(pos, by = "bin_timestamp")

setDF(pos)

#one as an exmample 
unique(pos$animal_id)
head(pos)
library(ggplot2)
library(gganimate)

p<-ggplot(shorelinemap) +
 geom_sf(color = "black", fill = "lightgrey") +
 coord_sf(xlim = x_limits_lko, ylim = y_limits_lko) +
 #geom_point(data = receivers, aes(x = deploy_long, y = deploy_lat),
 # size = 2, color = "black", inherit.aes = FALSE) + #receivers in water are black points
 geom_point(data=pos, aes(x=longitude, y=latitude,
                          group=animal_id, color=animal_id,
                          size=record_type), inherit.aes=FALSE) +
 xlab("Longitude") +
 ylab("Latitude") +
 # scale_color_manual(values=c("red", "blue")) +
 # coord_sf(xlim = c(-79.95, -79.75), ylim = c(43.22, 43.35), expand = FALSE)+
 #scale_color_manual(values=c("red", "blue"))+
 # scale_size_manual(values=c(2,1)) +
 theme(legend.position = "none") +
 transition_time(bin_timestamp)    +
 ggtitle('{frame_time}')

p

##zoomed out 
animate(p, fps = 4, nframes = 200, renderer = av_renderer())
anim_save("ruddalllko.mp4")
#this plots it in the r plot viewer and you can pause, can we add to an Rmarkdown then ????
##zoomed in all HH
pp<-ggplot(shorelinemap) +
 geom_sf(color = "black", fill = "lightgrey") +
 coord_sf(xlim = x_limits_HHall, ylim = y_limits_HHall) +
 #geom_point(data = receivers, aes(x = deploy_long, y = deploy_lat),
 # size = 2, color = "black", inherit.aes = FALSE) + #receivers in water are black points
 geom_point(data=pos, aes(x=longitude, y=latitude,
                          group=animal_id, color=animal_id,
                          size=record_type), inherit.aes=FALSE) +
 xlab("Longitude") +
 ylab("Latitude") +
 # scale_color_manual(values=c("red", "blue")) +
 # coord_sf(xlim = c(-79.95, -79.75), ylim = c(43.22, 43.35), expand = FALSE)+
 #scale_color_manual(values=c("red", "blue"))+
 # scale_size_manual(values=c(2,1)) +
 theme(legend.position = "none") +
 transition_time(bin_timestamp)    +
 ggtitle('{frame_time}')

pp

##zoomed out 
animate(pp, fps = 4, nframes = 200, renderer = av_renderer())
anim_save("ruddallHH.mp4")
#############################explore fish tagged in 2023 as we had CPM and GC recs out then for the first year 


