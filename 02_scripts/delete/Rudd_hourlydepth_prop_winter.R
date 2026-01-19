##time of day 24 hours for winter data depth to look for diel movements in the water column 

##asign hour 1-24 to a new column in the dataframe 

head(rudd_dets_temp)
#just winter for now 
Rudd_depth_winter<-rudd_dets_temp %>% filter(season =="Winter")
Rudd_depth_winter$year <- format(as.Date(Rudd_depth_winter$detection_timestamp_EST), "%Y")

Rudd_depth_winter2324 <- Rudd_depth_winter %>% 
 filter(year %in% c(2023, 2024))

#assign hourly column 

Rudd_depth_winter2324 <- Rudd_depth_winter2324 %>%
 mutate(hour = hour(detection_timestamp_EST))

depth_proportions_hour <- Rudd_depth_winter2324 %>%
 group_by(hour, Sensor.Val) %>% 
 summarise(count = n(), .groups = "drop") %>%
 group_by(hour) %>%
 mutate(proportion = count / sum(count)) %>%
 ungroup()


library(ggplot2)
library(viridis)

# Create the heatmap
winterhourlydepth<-ggplot(depth_proportions_hour, aes(x = hour, y = Sensor.Val, fill = proportion)) +
 geom_tile() +
 scale_fill_viridis(option = "plasma", name = "Proportion") +
 scale_x_continuous(breaks = 0:23, labels = 0:23) +
 labs(
  x = "Hour of Day",
  y = "Depth (m)",
  title = "Proportion of Depth Detections by Hour"
 ) +
 theme_minimal() +
 scale_y_reverse()+
 theme(
  panel.grid = element_blank(),
  axis.text.x = element_text(angle = 0, hjust = 0.5)
 )

#only 2023 and 2024 data? 
ggsave("winter houlry depth_2023_2024.png", plot = winterhourlydepth, width = 12, height = 8, dpi = 300)



#####same as above but for east end in the fall 

