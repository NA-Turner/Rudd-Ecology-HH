#efish catch data 
#DFO long-term boat electrofishing monitoring surverys
#includes all avaliable data. Sampling occured during spring, summer, fall at night, day, and dusk
#goldfish, common carp, and rudd

#load in data 


#get year column 
#change names to common 
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

#file locations 
efish_biomass <- read_excel("01_data/01_raw_files/efish data/efish_biomass.xlsx")
efishcatchdata <- read_excel("01_data/01_raw_files/efish data/efishcatchdata.xlsx")

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


# Calculate mean catch per transect by species and year with confidence intervals
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
 # Now calculate means and confidence intervals
 group_by(Species, year) %>%
 summarise(
  mean_numbers_per_transect = mean(Numbers),
  sd = sd(Numbers),
  n_transects = n(),
  se = sd / sqrt(n_transects),
  ci_lower = mean_numbers_per_transect - qt(0.975, n_transects - 1) * se,
  ci_upper = mean_numbers_per_transect + qt(0.975, n_transects - 1) * se,
  .groups = "drop"
 )



# Reshape data to long format
summary_long <- summary_data %>%
 pivot_longer(
  cols = c(mean_numbers_per_transect),
  names_to = "metric",
  values_to = "value"
 ) %>%
 mutate(metric = recode(metric,
                        "mean_numbers_per_transect" = "Numbers"))


#ggplot of rudd, common carp, goldfish mean catch over sampling years 

ggplot(summary_long, aes(x = year, y = value, color = Species)) +
 geom_line(linewidth = 1) +
 geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
 geom_point(size = 3) +
 geom_vline(xintercept = 1996, linetype = "dashed", color = "black", linewidth = 0.8) +
 facet_wrap(~metric, scales = "free_y") +
 scale_x_continuous(breaks = unique(summary_long$year)) +
 labs(
  x = "Year",
  y = "Mean Catch per Transect",
  title = ""
 ) +
 theme_minimal() +
 theme(
  axis.text.x = element_text(angle = 75, hjust = 1),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), 
  axis.line = element_line(color = "black"),
 axis.ticks.x = element_line(color = "black"))

#assign zone and replot by zone 

Transect_details <- read.csv("01_data/01_raw_files/efish data/HH_TransectDetails.csv")

complete_catch_data_zone <- complete_catch_data %>%
 left_join(Transect_details %>% select(Transect, Zone), by = "Transect")

summary_data_byzone <- complete_catch_data_zone %>%
 # Get all unique transect-date-year-zone combinations (all sampling events)
 distinct(Transect, Date, year, Zone) %>%
 # Cross join with all species to create every species x sampling event combination
 cross_join(
  complete_catch_data_zone %>% 
   filter(!is.na(Species)) %>% 
   distinct(Species)
 ) %>%
 # Join back to actual catch data
 left_join(
  complete_catch_data_zone %>% filter(!is.na(Species)),
  by = c("Transect", "Date", "year", "Zone", "Species")
 ) %>%
 # Replace NAs with 0 (these are the true zeros where species wasn't caught)
 mutate(
  Numbers = ifelse(is.na(Numbers), 0, Numbers)
 ) %>%
 # Now calculate means and confidence intervals by zone and year
 group_by(Species, year, Zone) %>%
 summarise(
  mean_numbers_per_transect = mean(Numbers),
  sd = sd(Numbers),
  n_transects = n(),
  se = sd / sqrt(n_transects),
  ci_lower = mean_numbers_per_transect - qt(0.975, n_transects - 1) * se,
  ci_upper = mean_numbers_per_transect + qt(0.975, n_transects - 1) * se,
  .groups = "drop"
 )

#and plot the data

zoneplot <- ggplot(summary_data_byzone, aes(x = year, y = mean_numbers_per_transect, color = Species)) +
  geom_rect(data = summary_data_byzone %>% filter(is_middle) %>% distinct(Zone),
            xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
            fill = "grey95", alpha = 0.5, inherit.aes = FALSE) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3, alpha = 0.6) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 1996, linetype = "dashed", color = "darkgrey", linewidth = 0.6) +
  facet_wrap(~Zone, ncol = 3, scales="fixed") +  # Removed scales argument - defaults to "fixed"
  scale_x_continuous(breaks = unique(summary_data_byzone$year)) +
  labs(
    x = "Year",
    y = "Mean Catch per Transect",
    title = "Mean Fish Catch per Transect by Species and Zone Over Time"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 75, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom"
  )

zoneplot
