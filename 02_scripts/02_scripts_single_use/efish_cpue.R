#efish catch data 
#DFO long-term boat electrofishing monitoring surverys
#includes all avaliable data. Sampling occured during spring, summer, fall at night, day, and dusk
#goldfish, common carp, and rudd


#get year column 
#change names to common 
library(lubridate)
library(dplyr)

efishcatchdata$year<-year(efishcatchdata$Date)
efish_biomass$year<-year(efish_biomass$Date)

#change namges 
efishcatchdata <- efishcatchdata %>%
 mutate(
  Species = case_when(
   Species == "F0181" ~ "Goldfish",
   Species == "F0186" ~ "Common Carp",
   Species == "F0220" ~ "Rudd",
   TRUE ~ Species
  )
 )

#now need to add in zeros/ tranects that were sampled but no catch occured for any of the three species 
#join test 
#these dataframes are in the raw data file 
head(efishcatchdata)
head(efish_biomass)

library(dplyr)

# Step 1: Get all unique transect-date combinations from the complete sampling data
all_samples <- efish_biomass %>%
 select(Sample, Location, Transect, Date, Time, Estimate, `Time Period`, year) %>%
 distinct()

# Find which transect-date combinations are NOT in the catch data
#anti-join fnction will find transect-date combination that exist in the sampling data (biomass df) but not the catch df

zero_catch_events <- all_samples %>%
 anti_join(efishcatchdata, by = c("Transect", "Date")) %>%
 mutate(
  Species = NA,
  Biomass = 0,
  Numbers = 0,
  year = year(Date)
 )

# Combine with original catch data
complete_catch_data <- bind_rows(efishcatchdata, zero_catch_events)


library(dplyr)

# Calculate mean catch per transect by species and year
# This includes zeros where species wasn't caught but sampling occurred
summary_data <- complete_catch_data %>%
 # Get all unique transect-date-year combinations (all sampling events)
 distinct(Transect, Date, year) %>%
 # Cross join with all species to create every species x sampling event combination
 cross_join(
  complete_catch_data %>% 
   filter(!is.na(Species)) %>% 
   distinct(Species)
 ) %>%
 # Join back to actual catch data
 left_join(
  complete_catch_data %>% filter(!is.na(Species)),
  by = c("Transect", "Date", "year", "Species")
 ) %>%
 # Replace NAs with 0 (these are the true zeros where species wasn't caught)
 mutate(
  Numbers = ifelse(is.na(Numbers), 0, Numbers)
 ) %>%
 # Now calculate means
 group_by(Species, year) %>%
 summarise(
  mean_numbers_per_transect = mean(Numbers),
  n_transects = n(),
  .groups = "drop"
 )


#plot 
library(tidyr)
library(ggplot2)

# Reshape data to long format
summary_long <- summary_data %>%
 pivot_longer(
  cols = c(mean_numbers_per_transect),
  names_to = "metric",
  values_to = "value"
 ) %>%
 mutate(metric = recode(metric,
                        "mean_numbers_per_transect" = "Numbers"))

ggplot(summary_long, aes(x = year, y = value, color = Species)) +
 geom_line(linewidth = 1) +
 geom_point(size = 3) +
 geom_vline(xintercept = 1996, linetype = "dashed", color = "black", linewidth = 0.8) +
 facet_wrap(~metric, scales = "free_y") +
 labs(
  x = "Year",
  y = "Mean Catch per Transect",
  title = "Mean Fish Catch per Transect by Species Over Time"
 ) +
 theme_minimal()

##this figure looks diff than one on Boston et al. goldfish paper 
#but this isnt filtered out by a season or time period this is ALL avaliable sampling data


##going to have to assign each efish transect to west, north, east 
#then re-run for Rudd to show highest populaitons in the west end whihc has most SAV
