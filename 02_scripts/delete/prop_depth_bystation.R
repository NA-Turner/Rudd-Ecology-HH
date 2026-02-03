

daily_location <- rudd_dets %>%
 group_by(animal_id, date) %>%
 count(station, sort = TRUE) %>%
 slice(1) %>%   # Get the station with the highest count
 ungroup() %>%
 dplyr::select(animal_id, date,station)


#Create a sequence of all dates for each individual
full_dates <- daily_location %>%
 group_by(animal_id) %>%
 complete(date = seq(min(date), max(date), by = "day")) %>%  # Fill in missing dates
 ungroup()%>% 
 dplyr::select(animal_id, date)

#Merge full dates with daily location data and carry forward the last known location
final_locations <- full_dates %>%
 left_join(daily_location, by = c("animal_id", "date")) %>%
 group_by(animal_id) %>%
 arrange(date) %>%
 mutate(station = zoo::na.locf(station, na.rm = FALSE)) %>%  # Carry forward the last known location
 ungroup()

#data with all carried forward locations= final_locations

#daily proportional activity
#Count the total number of individuals detected per day
total_per_day <- final_locations %>%
 group_by(date) %>%
 summarise(total_individuals = n_distinct(animal_id))

#Count the number of individuals detected at each receiver per day
count_per_receiver <- final_locations %>%
 group_by(date, station) %>%
 summarise(individuals_at_receiver = n_distinct(animal_id)) %>%
 ungroup()

#Join the two dataframes and calculate the proportion
proportions <- count_per_receiver %>%
 left_join(total_per_day, by = "date") %>%
 mutate(proportion = (individuals_at_receiver / total_individuals)*100)

# View the result
print(proportions)

#this dataframe proportions has daily proportioanl activity 

#adding daily depth to the dataframe of proportional activity?

#keep all P sensor tags for this one
rudd_detsPonly <- rudd_dets %>%
 filter(SensorType == 1)

daily_depth <- rudd_detsPonly %>%
 group_by(animal_id, date) %>%
 summarise(avg_depth = mean(Sensor.Val, na.rm = TRUE), .groups = "drop")

#Create a sequence of all dates for each individual
full_dates_depth <- daily_depth %>%
 group_by(animal_id) %>%
 complete(date = seq(min(date), max(date), by = "day")) %>%  # Fill in missing dates
 ungroup()%>% 
 dplyr::select(animal_id, date)

#Merge full dates with daily avg depth data and carry forward the last known avg depth if not one avalibale
final_locations_depth <- full_dates_depth %>%
 left_join(daily_depth, by = c("animal_id", "date")) %>%
 group_by(animal_id) %>%
 arrange(date) %>%
 mutate(avg_depth = zoo::na.locf(avg_depth, na.rm = FALSE)) %>%  # Carry forward the last known location
 ungroup()

final_locations_depth
#merge these two dataframes 
# Merging avg_depth into final_locations
final_locations_withavgdepth <- final_locations %>%
 left_join(final_locations_depth %>% select(animal_id, date, avg_depth), by = c("animal_id", "date"))

summary_df_depth_recgroup <- final_locations_withavgdepth %>%
 group_by(date, station) %>%
 summarize(avg_depth = mean(avg_depth, na.rm = TRUE), .groups = "drop")

##now take the proportions dataframe and add the avg depth to it based on the date and rec group

final_locations_withavgdepth_Proprotions <- proportions %>%
 left_join(summary_df_depth_recgroup, by = c("date", "station"))


final_locations_withavgdepth_Proprotions_shallow<-final_locations_withavgdepth_Proprotions %>%
 filter(proportion >=60)

final_locations_withavgdepth_Proprotions_shallow <- final_locations_withavgdepth_Proprotions_shallow %>%
 filter(avg_depth <= 3.0)



all_props<-ggplot(final_locations_withavgdepth_Proprotions_shallow, aes(
 x = date, 
 y = avg_depth, 
 color = station, 
 size = proportion)) +
 geom_point(alpha = 0.7) +  
 scale_y_reverse() +  
 scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
 scale_size_area(max_size = 10, name = "Proportion") +  # Changed this
 theme_minimal() +
 labs(
  x = "Date",
  y = "Depth (m)"
 ) +
 theme(
  axis.text.x = element_text(angle = 45, hjust = 1, size=14),
  axis.text.y = element_text(size = 14),
  axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 14)
 )



ggplotly(all_props)

