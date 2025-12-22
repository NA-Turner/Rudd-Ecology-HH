#electrofishing data CPUE of Rudd
#add in lat lon locatiosn
library(lubridate)
library(dplyr)

#effort for efish per transect 
Rudd_HH_electrofishingdata$Effort_time <- 0.083 
## 


#from 2010 onwards
#GFham <- dplyr::filter(GFham, Year >=2016)
#unique(GFham$Year)

# GFham1 <- dplyr::filter(GFham, Season %in% c("Summer"))
# unique(GFham1$Season)
Rudd_HH_electrofishingdata <- Rudd_HH_electrofishingdata %>%
  mutate(year = year(Date))

#add year in if you want it year by year 
# Step 1: Summarize the Rudd_HH_electrofishingdata number of individuals captured per transect per year
abundance_summary <- Rudd_HH_electrofishingdata %>%
  group_by(Transect, year) %>%
  summarize(total_abundance = sum(`Number Individuals`, na.rm = TRUE), .groups = 'drop', 
            total_effort = sum(Effort_time, na.rm = TRUE))


# Step 3: Combine abundance and effort dataframes
cpue_data_rudd <- abundance_summary %>%
  mutate(CPUE = total_abundance / total_effort)

#need to bind back to transect locations then we can plot it up 

cpue_data_withtransectdetails <- cpue_data_rudd %>%
  left_join(HH_TransectDetails_25May2022, by = c("Transect"))

write.csv(cpue_data_withtransectdetails, "Rudd_efish_abundanceCPUE_all_HH.csv")

####make a table 
library(dplyr)
library(tidyr)

# Suppose your tibble is called "df"
# ---- Total abundance table ----
abundance_table <- cpue_data_withtransectdetails %>%
  select(Transect, year, total_abundance) %>%
  pivot_wider(names_from = year, values_from = total_abundance, 
              names_sort = TRUE)

# ---- CPUE table ----
cpue_table <- cpue_data_withtransectdetails %>%
  select(Transect, year, CPUE) %>%
  pivot_wider(names_from = year, values_from = CPUE, 
               names_sort = TRUE)

cpue_table_withlatlon <- cpue_table %>%
  left_join(HH_TransectDetails_25May2022, by = c("Transect"))

HHmap_with_cpue <- HHmap+
  # 2️⃣ Add CPUE points
  geom_point(
    data = cpue_data_withtransectdetails,
    aes(x = Longitude, y = Latitude, size = CPUE),
    color = "orange", alpha = 1
  ) +
  facet_wrap(~year)+  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude",
    size = "CPUE"
  )

#save the plot for later

ggsave("HHmap_with_cpue.png", plot = HHmap_with_cpue, width = 8, height = 6, dpi = 300, bg = "white")


##


cpue_data <- GFham1 %>% 
  # Group by year and Trip ID so that you can calculate CPUE for every trip in every year
  group_by(Year,Transect) %>% 
  # For each year and trip ID, calculate the CPUE for each trip by dividing the sum of the catch, converted from grams to kilograms, by the trip by the number of fishing hours
  summarize(Trip_CPUE = sum(Abundance) / mean(Effort_time)) %>% 
  # Next, just group by year so we can calculate median CPUE for each year across all trips in the year
  group_by(Transect) %>% 
  # Calculate median CPUE for each year
  summarize(Median_CPUE_kg_hour = median(Trip_CPUE))



