#basic proccess of GLATOS telemetry data 
#filter for false detections and min lag times 
gc()

###load packages 
library(dplyr)
library(tidyr)
library(beepr)
library(lubridate)
library(glatos)
library(readxl)
install.packages("glatos")

# First install remotes if you don't have it
install.packages("remotes")
# Then install glatos from GitHub
remotes::install_github("ocean-tracking-network/glatos")
getRversion()

#load in all data files
#raw detection file
detections <- read_csv("01_data/03_large_files_LFS/01_raw_files/hamlo_rudd_20260112.csv")
#All tagged Rudd workbook
tagspec <- read_excel("01_data/02_processed_files/Rudd_tagworkbook.xlsx")
#all receivers 
recs <- read.csv("01_data/02_processed_files/GLATOS_receiverLocations_20260112_221824.csv")


tagspec$transmitter_id<-as.character(tagspec$TAG_ID_CODE)
colnames(tagspec)
##due to temperature and depth, added SN and sensor type to the info to bring to detection dataset
fish.info <- dplyr::select(tagspec,transmitter_id,'Tag SN',SensorType,Slope,Intercept)
fish.info$Slope<-as.numeric(fish.info$Slope)
fish.info$Intercept<-as.numeric(fish.info$Intercept)

##remove NAs - only fish with slopes/intercepts
fish.info<-fish.info[!is.na(fish.info$Slope),]

#join the detections and depth info datasets
fish.info$transmitter_id<-as.numeric(fish.info$transmitter_id)
detections<-left_join(detections,fish.info, by = c("transmitter_id"))
beep(5)
## Get Final Sensor Value, in this case 'Depth' and 'temperature' 
detections$Sensor.Val <- (detections$sensor_value*detections$Slope)+detections$Intercept 
beep(5)

### check to see if all fish with sensor data are getting a corrected sensor value. It should have the same # of rows in deep and deep2
#deep<-detections %>% filter(!is.na(sensor_value))
#deep2<-detections %>% filter(!is.na(Sensor.Val))

#quick summary of min and max depths by fish species

depths <- detections %>% group_by(common_name_e, SensorType) %>% summarize(min=min(Sensor.Val,na.rm = TRUE), max=max(Sensor.Val,na.rm = TRUE), ids=n_distinct(transmitter_id))

##with temperature and depth values calculated, we will make the transmitter_id be the SN value to combine location dataset properly for each fish
#detections$transmitter_id<-detections$`Tag SN`
#difference is because of TP tags
n_distinct(detections$transmitter_id) #57
n_distinct(detections$`Tag SN`) #47

###individually change transmitter_ids for fish with temp and pressure sensors. It messes up so many other things otherwise. 
tempfish<-detections %>% group_by(common_name_e,transmitter_id, `Tag SN`) %>% filter(SensorType=="T") %>% summarize(n=n())
unique(tempfish$transmitter_id)
SNs<-unique(tempfish$`Tag SN`)



pressurefish<-detections %>% group_by(transmitter_id, `Tag SN`,common_name_e, SensorType) %>% filter(`Tag SN` %in% SNs)%>% summarize(n=n())

detections$transmitter_id[detections$transmitter_id == 1365] <- 1366
detections$transmitter_id[detections$transmitter_id == 1367] <- 1368
detections$transmitter_id[detections$transmitter_id == 1369] <- 1370
detections$transmitter_id[detections$transmitter_id == 1371] <- 1372
detections$transmitter_id[detections$transmitter_id == 1373] <- 1374
detections$transmitter_id[detections$transmitter_id == 1377] <- 1378
detections$transmitter_id[detections$transmitter_id == 1379] <- 1380
detections$transmitter_id[detections$transmitter_id == 1385] <- 1386
detections$transmitter_id[detections$transmitter_id == 1391] <- 1392
detections$transmitter_id[detections$transmitter_id == 1397] <- 1398

#verify that transmitter IDs were combined
pressurefish<-detections %>% group_by(transmitter_id, `Tag SN`,common_name_e, SensorType) %>% filter(`Tag SN` %in% SNs)%>% summarize(n=n())
n_distinct((pressurefish$transmitter_id))

### there is a difference since some fish were never provided with a SN or aren't in our fish workbook (e.g., Paul Bzonek's UoT carp)
n_distinct(detections$transmitter_id)
#47
n_distinct(detections$`Tag SN`)
#47
########################################################
###false filter detections

###tf = threshold time. Where the rule of thumb is to omit data when
###a detection is separated from others of the same tag ID on 
###a single receiver by 30 times the nominal delay of the transmitter 
###(e.g., 3600 seconds for tags with a 120 sec nominal delay) - basically removing a random blip of a detection
### in Hamilton Harbour with such the dense array, false filtering is recommended. 

###determine nominal delay 
tagspec$Min

tagspec$nominaldelay<-((tagspec$`Max-Delay`-tagspec$Min)/2)+tagspec$Min
nominaldelay<-tagspec %>% group_by(nominaldelay) %>% summarise(n=n())
mindelay<-tagspec %>% group_by(Min) %>% summarise(n=n())
fish.info2 <- select(tagspec,transmitter_id,nominaldelay,Min)
#join the detections and nominal delay info datasets
fish.info2$transmitter_id<-as.numeric(fish.info2$transmitter_id)
detections<-left_join(detections,fish.info2, by = c("transmitter_id"))

###assume Min is 130 for fish without info - used 130 as it was the most common Min Delay value 
detections$Min[is.na(detections$Min)] <- 130
detections$nominaldelay[is.na(detections$nominaldelay)] <- 200
unique(detections$nominaldelay)

# Nominal Delay is 200 sec in HH (for most fish)
##false detection filtered based on correct nominal delay for each fish 
#30 times the nominal delay
#adjusts for each tag specific threshold so more accurate than 

ls("package:glatos")

delays<-unique(detections$nominaldelay)
filtered_detections<-data.frame()
#i=1
for (i in 1:length(delays)) {
  
  temp<-detections %>% filter(nominaldelay==delays[i])
  
  false=delays[i]*30  
  
  filtered_detections_part<-false_detections(det=temp, tf = false, show_plot=TRUE)
  
  filtered_detections<-bind_rows(filtered_detections,filtered_detections_part)
  
}

#Total detections before filter: 5163601
#after removing detections that did not pass the filter 
#Detections passing filter: 5093856
#Percentage removed: 1.35%


### remove false detections for all dataset. Gets rid of many weird detection 
detections<-filtered_detections[filtered_detections$passed_filter==1,]
unique(detections$passed_filter)



#summary statistics 
# Summary before saving
message("=== False Detection Filter Summary ===")
message(paste("Total detections before filter:", nrow(filtered_detections)))
message(paste("Detections passing filter:", nrow(detections)))
message(paste("False detections removed:", 
              nrow(filtered_detections) - nrow(detections)))
message(paste("Percentage removed:", 
              round((1 - nrow(detections)/nrow(filtered_detections))*100, 2), "%"))

message()

#########################################
###save in case R crashes due to size of dataset (often happened to me...)
#delete at the end once you have final data file (files are very large and take up lots of space)
### This will take a while so run the 'beep' function together with the 'saveRDS' function

#saveRDS(detections, "./01_data/03_large_files_LFS/02_processed_files/Rudddetections_filtered_2017-2025.rds")
#beep(5)

#### see if fish are detected within minimal delay (130s) on the same receiver - i.e. being detected again on a receiver 
## earlier than what is actually possible. If this happens it is not a true detection. 

min(tagspec$Min, na.rm=T)
#60 only 1 

mindelay<-detections %>% group_by(Min) %>% summarise(n=n_distinct(transmitter_id))

# Most tags have minimal nominal delay of 130s, but using tag-specific Min values (60-130s)
# to ensure appropriate filtering for each individual tag
# recalculate min lag column as we removed some values from false filtering. 

detections<-min_lag(detections)

beep(5)
min(detections$min_lag, na.rm=T)

### this process needs to be repeated since when a detection that occurred too soon is removed, the minimum lag between 
# detections of the same individual on the same receiver could still be < the minimal nominal delay 
#(i.e., a detection could have occurred 5 times after the initial one that were too quick)
## this will help remove any biases with any position estimates later or detection #s. 

##R can't run dataset this large in one go. So attempting to split it
tags<-unique(detections$transmitter_id)
data_filtered<-list()
pb <- winProgressBar(title = "progress bar", min = 0,
                     max = length(tags), width = 300)

#this will take a while to run 

for (i in 1:length(tags)) {
  temp<-detections %>% filter(transmitter_id==tags[i])
  ##use the min delay for the specific tag - before ran it generically
  lag<-unique(temp$Min)
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, title=paste( round(i/length(tags)*100, 0),
                                        "% done"))
  ##to remove the detections that occurred too soon on the same receiver
  repeat {
    # do something
    short<-temp %>% filter(min_lag<lag) ### selects all data with short detections
    long<-temp %>% filter(!min_lag<lag) ### selects all good data with longer detections
    no<-temp %>% filter(is.na(min_lag)) ### keeps those with NA min lag in dataset (think this is alright - its if only one detection at a receiver or the last time detected)
    short2<-short[!duplicated(short[c("station_no","transmitter_id","min_lag")]),] ### of the short detections - removes duplicates - or detections after the first one that is occurring too quickly. 
    #Note, this is likley not perfect but its still removing detections that are occurring too early so probably is good
    #It removes those with the same min_lag value at the same station. Mostly due to a glitch at one receiver (station 34) with 8 sec min lags repeating in error. 
    temp<-bind_rows(long,short2, no)  #### combines dataset with short detections removed
    # detections2 <- detections2 %>% select(-c(min_lag)) ###removes min lag column - it recalculates over same column name
    temp <- min_lag(temp) ###recalculates min lag column
    min(temp$min_lag)
    # exit if the condition is met
    if(nrow(temp %>% filter(min_lag<lag))==0) {   ##### if there are any detections with min lag < 120, this keeps repeating
      break
    }
  }
  data_filtered<-bind_rows(data_filtered,temp)
  
}
beep(5)
close(pb)


############################################################
#change detections dataframe for time etc. 
##################################################################

##convert to appropriate time zone for analyses
data_filtered$detection_timestamp_EST<-as.POSIXct(format(data_filtered$detection_timestamp_utc, tz="America/Toronto",format="%Y-%m-%d %H:%M:%S"))
data_filtered$detection_timestamp_EST<-force_tz(data_filtered$detection_timestamp_EST,'America/Toronto')
beep(5)
###ensure data is in UTC for loading in data. Similarly all other data. 
attributes(data_filtered$detection_timestamp_utc)
attributes(data_filtered$detection_timestamp_EST)

##convert to appropriate time zone for analyses
data_filtered$EST_release_date_time<-as.POSIXct(data_filtered$utc_release_date_time)

#unique(data_filtered$EST_release_date_time)
attr(data_filtered$EST_release_date_time, "tzone") <- "America/Toronto"
data_filtered$EST_release_date_time <- format(as.POSIXct(data_filtered$EST_release_date_time, format = "%Y-%m-%d %H:%M:%S"))

data_filtered$EST_release_date_time <- (as.POSIXct(data_filtered$EST_release_date_time, format = "%Y-%m-%d %H:%M:%S"))
data_filtered$EST_release_date_time<-force_tz(data_filtered$EST_release_date_time,'America/Toronto')
beep(5)

###ensure data is in UTC for loading in data. Similarly all other data. 
attributes(data_filtered$utc_release_date_time)
attributes(data_filtered$EST_release_date_time)

#one fish has NA for transmitter id we can add back into dataframe (match up with tag sn)
data_filtered$tag_serial_number[is.na(data_filtered$tag_serial_number)] <- 1271930
data_filtered$`Tag SN`[is.na(data_filtered$`Tag SN`)] <- 1271930
unique(data_filtered$`Tag SN`)

data_filtered <- data_filtered %>%
 mutate(animal_id = paste(common_name_e, transmitter_id, sep = "_"))

###################################################################
saveRDS(data_filtered, "./01_data/03_large_files_LFS/01_raw_files/Rudddetections_filtered1B_2017-2025.rds")
beep(5)


### CHECK RECEIVERS FOR ERRORS in receiver_timeline_plot script.


#### QAQC'd receiver errors using the "Receiver_timeline_plot" script. No errors as of Feb 7, 2024 however...


##can also make a bubble plot to determine if fish were detected outside of weird areas
detection_bubble_plot(data_filtered, location_col= "station")
#and a close up of AOI
detection_bubble_plot(data_filtered, location_col= "station",background_ylim = c(43, 43.8), background_xlim = c(-80.2, -79.5))

###or see if histogram of lats and longs 
hist(data_filtered$deploy_lat)
hist(data_filtered$deploy_long)

########################
###can now move to creating and editing abacus plots for data clipping/ removing dead fish etc. 
##See dataclip_abacusplots
