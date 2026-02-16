##load in the filtered detection dataframe
data_filtered <- readRDS("~/01_data/03_large_files_LFS/01_raw_files/Rudddetections_filtered1B_2017-2025.rds")

library(ggplot2)
library(dplyr)
library(tidyr)
library(beepr)
library(lubridate)
library(ggpubr)
library(scales)
library(readxl)

data_filtered$SensorType<-as.character(data_filtered$SensorType)

ruddalldets <- data_filtered %>%
  mutate(SensorType = case_when(
    SensorType == "P" ~ 1,
    SensorType == "T" ~ 2
  ))

#
dir.create(file.path("./03_outputs/01_figures/Abacusplots_preclip"), recursive = TRUE)


ruddalldets$SensorType<-as.factor(ruddalldets$SensorType)
for(i in 1:length(tags)){
  #i<-1
  db<- subset(ruddalldets, ruddalldets$transmitter_id == paste0(tags[i]))
  #head(db)
  unique(db$sensor_unit)
  species<-db$common_name_e[1]
  id1<-unique(as.character(db$transmitter_id))
  id<-paste("Tag ID ",id1,sep="")
  
  first <- min(db$detection_timestamp_EST) # time of first detection
  last <- max(db$detection_timestamp_EST) # time of last detection
  
  db$station<-as.factor(db$station)
  db <- db[order(db$detection_timestamp_EST, decreasing=F),]
  
  p1<-ggplot(db, aes(x= detection_timestamp_EST,y=station,group=1))+
    geom_line()+
    geom_point(col="purple2",size=2)+
    # geom_line( aes(x= detection_timestamp_EST), col="firebrick1", size =1)+
    #geom_point(aes(x=end),shape= 4,col="black", size = 3)+
    # geom_segment(aes(x = min,yend=transmitter_id,xend = max),size=1)+
    labs(x="Date",y="Station")+
    scale_x_datetime(
      limits = c(first, last),   # Use limits here instead of xlim()
      #labels = date_format("%m-%Y"),
      date_breaks = "1 month"
    )+
    theme_bw()+
    theme(axis.text.x=element_text(angle=45, hjust=1))#+facet_wrap(~common_name_e, scales="free_y")
  p1  
  
  if( n_distinct(db$SensorType)<2){
    p2 <- ggplot(data=db, aes(x= detection_timestamp_EST,y=Sensor.Val)) + 
      #geom_line(aes(group=1))+#, alpha=0.8, col="black") + 
      geom_point(col="black")+
      
      labs(x="Date", y="Depth (m)")+#scale_y_reverse()+
      #  scale_x_datetime(labels = date_format("%m-%Y"))+
      theme_bw()+theme(text=element_text(size=14))
    
    #p2
  }
  if( n_distinct(db$SensorType)>1){
    
    #db$Sensor.Val[db$SensorType=="T"] <-*-1 
    p2 <- ggplot(data=db, aes(x= detection_timestamp_EST,y=Sensor.Val, col=SensorType)) + 
      geom_point()+
      #geom_line(aes(group=1))+#, alpha=0.8, col="black") + 
      labs(x="Date", y="Depth (m)", fill="Sensor Type")+
      #scale_y_reverse(sec.axis = sec_axis(~rev(.),name="Temperature (°C)"))+
      scale_y_continuous(sec.axis = dup_axis(name="Temperature (°C)"))+
      #  scale_x_datetime(labels = date_format("%m-%Y"))+
      
      theme_bw()+theme(text=element_text(size=14))+
      scale_colour_manual(labels=c("Depth","Temperature"), values=c("black","tomato"))
    
    #p2
  }
  #margin = theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))
  hmmm<-ggarrange(p1,p2,nrow=1)
  hmmm<-annotate_figure(hmmm,
                        top = text_grob(paste('Tag ID',id1, '-', species, ''), color = "black", face = "bold", size = 14))
  
  ggsave(plot=hmmm, paste0("./03_outputs/01_figures/Abacusplots_preclip/Individual_",species,"_ID",id1,"_abacus_depth_plotsrudd.png"),  width = 35, height = 20,units = "cm", dpi = 400, bg="white")
}

#################fish to be completely removed from the dataframe...
#tags didnt work, died post tag. 
#following filtering rules set out in Larocque et al. 2024 paper 
#fish with <1 month to be removed from analysis completely
#fish alive for >1month had detections clipped (died) plus one day before suspected death
#

#########
#####Fish removed
########

#fish id 500; died at rec HAM-096 CPM within first month
#fish id 503; doed at rec HAM-094/093 CPM
#fish id 504; died at rec HAM-032 (fishway)
#fish id 505; died at rec HAM-032 (fishway)
#fish id 506; died in cpm two week post tag
#fish id 511; died in cpm <1 month remove 
#fish id 513; died at rec HAM-031 (fishway) <1month
#fish id 1372; died at rec HAM-032 (fishway)
#fish id 1378; died at rec HAM-032 (fishway) <1month 
#fish id 1380; died at rec HAM-032 (fishway) <1month
#fish id 1392; died at HAM-032 (fishway)
#fish id 9153; died post tagging around HAM-009 (east end)
#fish id 15865; died at HAM-023 (tagged with a V13)
#fish id 25081; died at HAM-012




#filter and clip for fish tagged in 2024
#############
#497 clip @ 2024-07-12
#498 no clip data good until last detection 2025-12-01
#499 no clip data good until last detection 2025-12-01
#500 remove
#501 no clip data good until last detection 2025-09-01
#502 no clip data looks good until it goes offline @ fishway 2025-05-15
#503 remove
#504 remove
#505 remove
#506 remove
#507 no clip data good until last detection 2025-12-01
#508 no clip (left harbour) last detection around 2025-07-01 
#(will be off HH rec for RF analysis when in LKO from mid may 2024 to april 2025 )
#509 no clip last detected CPM end of August 2024
#510 clip @2024-08-20
#511 remove
#512 no clip data good until last detection ~2025-12-01
#513 remove
#514 no clip data good until last detection ~2025-12-01
#515 no clip - died in CPM no clip just goes offline ~2024-06-15
#519 clip @ 2024-06-02

############
#filter and clip fish tagged in 2023
##########
#1366 clip @ 2023-12-25 
#1368 no clip
#1370 clip @ 2023-06-20
#1372 clip @2023-06-01 (died at fishway)
#1374 no clip - goes until 2024-06-01 removed at fishway 
#1378 remove 
#1380 remove
#1386 no clip - 1 year worth of detections, goes offline at fishway. also leaves harbour mid July - mid Dec
#1392 remove
#1398 clip @ 2024-05-15

######
#filter and clip tagged in 2021 and 2020
#########
#9149 no clip 
#9150 no clip
#9151 clip @ 2021-12-15
#9152 clip @ 2022-01-07
#9153 remove
#9154 clip @ 2021-09-01
#9155 no clip
#9156 clip @ 2022-09-07
#9157 no clip
#9158 no clip 
#9159 no clip (left harbor between mid may to oct 2022)
#9160 clip @ 2022-07-20
#9162 clip @ 2021-04-16 #gets removed later anyways in RF model PA DF (just FYI)
#9163 clip @ 2021-05-15 #gets removed later anyways in RF model PA DF (just FYI)
#9164 no clip
#25081 remove

#tagged in 2017
#15865 remove

##summary of fish to be removed
###47 tagged fish, remove right away n=15... 32 remain.. 
#removed 7/20 from tagging done in 2024
#removed 4/10 from tagging done in 2023
#removed 2/16 from tagging done in 2020/2021
#removed 1/1 from 2017 

#if you need to check in on an indiviudal fish plot using plotly wrapper
library(plotly)

rudd9151<-filter(detectionsrudd_removals11, transmitter_id=="9151")

first <- min(rudd9151$detection_timestamp_EST) # time of first detection
last <- max(rudd9151$detection_timestamp_EST) # time of last detection

rudd9151$station<-as.factor(rudd9151$station)
rudd9151 <- rudd9158[order(rudd9151$detection_timestamp_EST, decreasing=F),]
colnames(rudd9151)

p1<-ggplot(rudd9151, aes(x= detection_timestamp_EST,y=station, group=1))+
 geom_line()+
 geom_point(col="purple2",size=2)+
 # geom_line( aes(x= detection_timestamp_EST), col="firebrick1", size =1)+
 #geom_point(aes(x=end),shape= 4,col="black", size = 3)+
 # geom_segment(aes(x = min,yend=transmitter_id,xend = max),size=1)+
 labs(x="Date",y="Station")+
 scale_x_datetime(
  limits = c(first, last),   # Use limits here instead of xlim()
  labels = date_format("%m-%Y"),
  date_breaks = "1 month")+
 theme_bw()+
 theme(axis.text.x=element_text(angle=45, hjust=1))#+facet_wrap(~common_name_e, scales="free_y")
p1  
ggplotly(p1)

p2 <- ggplot(data=rudd9158, aes(x= detection_timestamp_EST,y=Sensor.Val)) + 
 #geom_line(aes(group=1))+#, alpha=0.8, col="black") + 
 geom_point(col="black")+
 
 labs(x="Date", y="Depth (m)")+#scale_y_reverse()+
 # scale_x_datetime(labels = date_format("%m-%Y"))+
 theme_bw()+theme(text=element_text(size=14))

p2
ggplotly(p2)

# Convert each ggplot to plotly
p1_plotly <- ggplotly(p1)
p2_plotly <- ggplotly(p2)


##############################################################
#############filtering the detections dataframe ##############
##############################################################
##first pull out the ids you want to remove completely from the dataframe

id_to_remove<- c("500", "503", "504", "505",
              "506","511","513", "1372","1378", "1380","1392"
              ,"9153", "15865", "25081")

# Use filter to remove these individuals
detectionsrudd_removals1 <- data_filtered %>%
  filter(!transmitter_id %in% id_to_remove)

unique(detectionsrudd_removals1$transmitter_id)

detectionsrudd_removals1$detection_timestamp_EST <- as.POSIXct(detectionsrudd_removals1$detection_timestamp_EST, format="%Y-%m-%d %H:%M:%S")


#then pull out the ids you want to clip from the dataframe, clip with the below code and then bind the two back together

#now we want to clip the rest of the fish after we have removed the other indivdiuals we no longer want in the dataframe 
#belwo
#data clipped for 10 fish

#2024 fish 
detectionsrudd_removals1<-detectionsrudd_removals1[!(detectionsrudd_removals1$transmitter_id==497 & detectionsrudd_removals1$detection_timestamp_EST > "2024-07-12 00:00:00"),] 
detectionsrudd_removals1<-detectionsrudd_removals1[!(detectionsrudd_removals1$transmitter_id==510 & detectionsrudd_removals1$detection_timestamp_EST > "2024-08-20 00:00:00"),] 
#2023 fish 
detectionsrudd_removals1<-detectionsrudd_removals1[!(detectionsrudd_removals1$transmitter_id==1366 & detectionsrudd_removals1$detection_timestamp_EST > "2023-12-25 00:00:00"),] 
detectionsrudd_removals1<-detectionsrudd_removals1[!(detectionsrudd_removals1$transmitter_id==1370 & detectionsrudd_removals1$detection_timestamp_EST > "2023-06-20 00:00:00"),] 
detectionsrudd_removals1<-detectionsrudd_removals1[!(detectionsrudd_removals1$transmitter_id==1372 & detectionsrudd_removals1$detection_timestamp_EST > "2023-06-01 00:00:00"),] 
detectionsrudd_removals1<-detectionsrudd_removals1[!(detectionsrudd_removals1$transmitter_id==1398 & detectionsrudd_removals1$detection_timestamp_EST > "2024-05-15 00:00:00"),] 
#2020 and 2021 fish 
detectionsrudd_removals1<-detectionsrudd_removals1[!(detectionsrudd_removals1$transmitter_id==9151 & detectionsrudd_removals1$detection_timestamp_EST > "2021-12-15 00:00:00"),] 
detectionsrudd_removals1<-detectionsrudd_removals1[!(detectionsrudd_removals1$transmitter_id==9152 & detectionsrudd_removals1$detection_timestamp_EST > "2022-01-07 00:00:00"),] 
detectionsrudd_removals1<-detectionsrudd_removals1[!(detectionsrudd_removals1$transmitter_id==9154 & detectionsrudd_removals1$detection_timestamp_EST > "2021-09-01 00:00:00"),] 
detectionsrudd_removals1<-detectionsrudd_removals1[!(detectionsrudd_removals1$transmitter_id==9156 & detectionsrudd_removals1$detection_timestamp_EST > "2022-09-07 00:00:00"),] 
detectionsrudd_removals1<-detectionsrudd_removals1[!(detectionsrudd_removals1$transmitter_id==9160 & detectionsrudd_removals1$detection_timestamp_EST > "2022-07-20 00:00:00"),] 
detectionsrudd_removals1<-detectionsrudd_removals1[!(detectionsrudd_removals1$transmitter_id==9162 & detectionsrudd_removals1$detection_timestamp_EST > "2021-04-16 00:00:00"),] 
detectionsrudd_removals1<-detectionsrudd_removals1[!(detectionsrudd_removals1$transmitter_id==9163 & detectionsrudd_removals1$detection_timestamp_EST > "2021-05-15 00:00:00"),] 

#check make sure it worked first 

summary_clipped <- detectionsrudd_removals1 %>%
 group_by(transmitter_id) %>%
 summarize(
  first_detection = min(detection_timestamp_EST),
  last_detection = max(detection_timestamp_EST),
  .groups = "drop"
 )

#remove first 48 hours from tag online date 

detectionsrudd_removals_48hrs <- detectionsrudd_removals1 %>%
  group_by(transmitter_id) %>%
  mutate(tag_date = min(as.Date(EST_release_date_time), na.rm = TRUE)) %>%  # Get tagging date per animal
  filter(as.Date(detection_timestamp_EST) > tag_date + 2) %>%  # Keep only detections after 2 days 
  ungroup()

check <- detectionsrudd_removals_48hrs %>%
 group_by(transmitter_id) %>%
 summarize(
  tag_date = first(as.Date(EST_release_date_time)),
  first_detection = min(as.Date(detection_timestamp_EST)),
  days_diff = as.numeric(first_detection - tag_date)
 )




saveRDS(detectionsrudd_removals_48hrs, "./01_data/03_large_files_LFS/rudd_detections_QAQC.rds")
detectionsrudd_removals_48hrs <- readRDS("~/01_data/03_large_files_LFS/02_processed_files/rudd_detections_QAQC.rds")

########################
##########rerun abacus plots post removal and clipping to make sure it all looks right 
##########################
tags<-unique(detectionsrudd_removals_48hrs$transmitter_id)

detectionsrudd_removals_48hrs$SensorType<-as.factor(detectionsrudd_removals_48hrs$SensorType)

for(i in 1:length(tags)){
  #i<-1
  db<- subset(detectionsrudd_removals_48hrs, detectionsrudd_removals_48hrs$transmitter_id == paste0(tags[i]))
  #head(db)
  unique(db$sensor_unit)
  species<-db$common_name_e[1]
  id1<-unique(as.character(db$transmitter_id))
  id<-paste("Tag ID ",id1,sep="")
  
  first <- min(db$detection_timestamp_EST) # time of first detection
  last <- max(db$detection_timestamp_EST) # time of last detection
  
  db$station<-as.factor(db$station)
  db <- db[order(db$detection_timestamp_EST, decreasing=F),]
  
  p1<-ggplot(db, aes(x= detection_timestamp_EST,y=station,group=1))+
    geom_line()+
    geom_point(col="purple2",size=2)+
    # geom_line( aes(x= detection_timestamp_EST), col="firebrick1", size =1)+
    #geom_point(aes(x=end),shape= 4,col="black", size = 3)+
    # geom_segment(aes(x = min,yend=transmitter_id,xend = max),size=1)+
    labs(x="Date",y="Station")+
    scale_x_datetime(
      limits = c(first, last),   # Use limits here instead of xlim()
      #labels = date_format("%m-%Y"),
      date_breaks = "1 month"
    )+
    theme_bw()+
    theme(axis.text.x=element_text(angle=45, hjust=1))#+facet_wrap(~common_name_e, scales="free_y")
  p1  
  
  if( n_distinct(db$SensorType)<2){
    p2 <- ggplot(data=db, aes(x= detection_timestamp_EST,y=Sensor.Val)) + 
      #geom_line(aes(group=1))+#, alpha=0.8, col="black") + 
      geom_point(col="black")+
      
      labs(x="Date", y="Depth (m)")+#scale_y_reverse()+
      #  scale_x_datetime(labels = date_format("%m-%Y"))+
      theme_bw()+theme(text=element_text(size=14))
    
    #p2
  }
  if( n_distinct(db$SensorType)>1){
    
    #db$Sensor.Val[db$SensorType=="T"] <-*-1 
    p2 <- ggplot(data=db, aes(x= detection_timestamp_EST,y=Sensor.Val, col=SensorType)) + 
      geom_point()+
      #geom_line(aes(group=1))+#, alpha=0.8, col="black") + 
      labs(x="Date", y="Depth (m)", fill="Sensor Type")+
      #scale_y_reverse(sec.axis = sec_axis(~rev(.),name="Temperature (°C)"))+
      scale_y_continuous(sec.axis = dup_axis(name="Temperature (°C)"))+
      #  scale_x_datetime(labels = date_format("%m-%Y"))+
      
      theme_bw()+theme(text=element_text(size=14))+
      scale_colour_manual(labels=c("Depth","Temperature"), values=c("black","tomato"))
    
    #p2
  }
  #margin = theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))
  hmmm<-ggarrange(p1,p2,nrow=1)
  hmmm<-annotate_figure(hmmm,
                        top = text_grob(paste('Tag ID',id1, '-', species, ''), color = "black", face = "bold", size = 14))
  
  ggsave(plot=hmmm, paste0("./03_outputs/01_figures/Abacusplots_postclip/Individual_",species,"_ID",id1,"_abacus_depth_plotsCLEANED.png"),  width = 35, height = 20,units = "cm", dpi = 400, bg="white")
}


#####pull out a dataframe for Rudd fate to be sent over to excel
#after all data has been clipped 
#animalid
#FL
#weight
#location tagged
#tag type
#date tagged
#last detection 
#number of days detected
#fate detrmined through abacus plots
#last rec detected on 
#notes (for fish tagged in 2023 - released into CPM side. this is when array was expanded aswell
#from 2023 onwards any tagged fish that came through the fishway were put into CPM side if noticed that they were tagged)

#load in fish tagging workbook 
#add fish details to cleaned and filtered detection data file for overview summary 


#Rudd_tagworkbook LOAD
#load in cleaned detections file 
#rudd_detections_QAQC

Rudd_tagworkbook <- read_excel("01_data/02_processed_files/Rudd_tagworkbook.xlsx")

detectionsrudd_removals_48hrs$transmitter_id<-as.numeric(detectionsrudd_removals_48hrs$transmitter_id)
Rudd_tagworkbook$transmitter_id<-as.numeric(Rudd_tagworkbook$transmitter_id)


dets_tagdeets <- left_join(detectionsrudd_removals_48hrs, Rudd_tagworkbook, by = "transmitter_id")


# Get first and last detection for each fish
fish_timeline <- dets_tagdeets %>%
  group_by(animal_id) %>%
  summarise(
    first_detection = min(detection_timestamp_EST),
    last_detection = max(detection_timestamp_EST),
    Battery_length = first(`Date tag dies`),
    species = first(common_name_e)
  )


############
summary_df <- dets_tagdeets %>%
  # convert timestamps to POSIXct (adjust parser if your format is different)
  mutate(
    detection_timestamp_EST = parse_date_time(detection_timestamp_EST,
                                              orders = c("ymd HMS", "ymd HM", "ymd", "Ymd HMS"),
                                              tz = "America/Toronto")) %>%
  group_by(transmitter_id) %>%
  summarize(
    forklength      = first(`Fork (mm)`),
    totallength     = first(`Total (mm)`),
    weight          = first(`Mass (g)`),
    location_tagged = first(`Release Location`),
    # use min/max so results don't depend on row order
    release_date    = min(GLATOS_RELEASE_DATE_TIME, na.rm = TRUE),
    first_detection = min(detection_timestamp_EST,   na.rm = TRUE),
    last_detection  = max(detection_timestamp_EST,   na.rm = TRUE),
    date_tag_dies = (`Date tag dies`),
    days_at_large   = as.numeric(difftime(last_detection, first_detection, units = "days")),
    tag_type        = first(tag_model),
    .groups = "drop"
  )


write.csv(summary_df, "./01_data/02_processed_files/Summary_rudd_tagdata_detdata.csv")


library(ggplot2)
library(dplyr)

dets_tagdeets$transmitter_id<-as.factor(dets_tagdeets$transmitter_id)


# Count active fish per day
active_fish_per_day <- fish_timeline %>%
 rowwise() %>%
 mutate(date_seq = list(seq(first_detection, last_detection, by = "day"))) %>%
 unnest(date_seq) %>%
 group_by(date_only = as.Date(date_seq)) %>%
 summarise(n_active_fish = n_distinct(animal_id), .groups = 'drop')

# Identify periods with 5+ fish
periods_5plus <- active_fish_per_day %>%
 filter(n_active_fish >= 5)


fish_timeline$animal_id<-as.factor(fish_timeline$animal_id)

# Create timeline plot with highlighted periods
five<-ggplot(fish_timeline, aes(y = animal_id)) +
 # Add shaded rectangles for 5+ fish periods
 geom_rect(data = periods_5plus, 
           aes(xmin = date_only, xmax = date_only + 1, 
               ymin = -Inf, ymax = Inf),
           fill = "orange3", alpha = 0.3, inherit.aes = FALSE) +
 # Fish timelines
 geom_segment(aes(x = first_detection, xend = last_detection, 
                  yend = animal_id), 
              size = 2, color = "black") +
 geom_point(aes(x = first_detection), color = "darkgreen", size = 3) +
 geom_point(aes(x = last_detection), color = "darkred", size = 3) +
 geom_point(aes(x = Battery_length), color = "gray29", size = 1, shape=4 ) +
 labs(title = "Fish Detection Timelines (Yellow = 5+ Active Fish)",
      x = "Date",
      y = "Transmitter ID") +
 scale_x_date(date_labels = "%b %Y", breaks = "1 month") +
 theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1, size=14), 
       axis.text.y= element_text(size=14))
five

ggsave("5activetags.png", plot = five, width = 12, height = 8, dpi = 300)

###
#4+ fish 
periods_4plus <- active_fish_per_day %>%
 filter(n_active_fish >= 4)

#some of the tag end dates (battery) wrong
# Create timeline plot with highlighted periods
four<-ggplot(fish_timeline, aes(y = animal_id)) +
 # Add shaded rectangles for 5+ fish periods
 geom_rect(data = periods_4plus, 
           aes(xmin = date_only, xmax = date_only + 1, 
               ymin = -Inf, ymax = Inf),
           fill = "orange3", alpha=0.3, inherit.aes = FALSE) +
 # Fish timelines
 geom_segment(aes(x = first_detection, xend = Battery_length, 
                  yend = animal_id), 
              size = 2, color = "black") +
 geom_point(aes(x = first_detection), color = "darkgreen", size = 3) +
 geom_point(aes(x = last_detection), color = "magenta", size = 3) +
 geom_point(aes(x = Battery_length), color = "darkred", size = 3) +
 labs(title = "Fish Detection Timelines (Orange = 4+ Active Fish)",
      x = "Date",
      y = "Transmitter ID") +
 scale_x_date(date_labels = "%b %Y", breaks = "1 month") +
 theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1, size=14), 
       axis.text.y = element_text(size=14))

four

ggsave("4activetags.png", plot = four, width = 12, height = 8, dpi = 300)


#same code but not active fish as in was detected but ALIVE fish per day 

# Count active fish per day
alive_intervals <- dets_tagdeets %>%
 group_by(transmitter_id) %>%
 summarise(
  start_date = as.Date(min(detection_timestamp_EST)),
  end_date   = as.Date(max(detection_timestamp_EST))
 )

# Build daily sequence across entire range
all_days <- seq(
 min(alive_intervals$start_date ),
 max(alive_intervals$end_date  ),
 by = "day"
)

# Expand fish × days and count who is alive
active_fish_per_day <- alive_intervals %>%
 tidyr::crossing(date_only = all_days) %>% 
 filter(date_only >= start_date,
        date_only <= end_date) %>%
 group_by(date_only) %>%
 summarise(n_active_fish = n(), .groups = "drop")

###
#4+ fish 
periods_5_plus <- active_fish_per_day %>%
 filter(n_active_fish >= 5)

# Create timeline plot with highlighted periods
ggplot(fish_timeline, aes(y = transmitter_id)) +
 # Add shaded rectangles for 5+ fish periods
 geom_rect(data = periods_5_plus, 
           aes(xmin = date_only, xmax = date_only + 1, 
               ymin = -Inf, ymax = Inf),
           fill = "goldenrod", alpha = 0.2, inherit.aes = FALSE) +
 # Fish timelines
 geom_segment(aes(x = first_detection, xend = last_detection, 
                  yend = transmitter_id), 
              size = 2, color = "black") +
 geom_point(aes(x = first_detection), color = "darkgreen", size = 3) +
 geom_point(aes(x = last_detection), color = "darkred", size = 3) +
 labs(title = "Fish Detection Timelines (Orange = 5+ Active Fish)",
      x = "Date",
      y = "Transmitter ID") +
 scale_x_date(date_labels = "%b %Y", breaks = "1 month") +
 theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1))


#### 4 or more active tags 
###
#4+ fish 
periods_4_plus <- active_fish_per_day %>%
 filter(n_active_fish >= 4)

# Create timeline plot with highlighted periods
ggplot(fish_timeline, aes(y = transmitter_id)) +
 # Add shaded rectangles for 5+ fish periods
 geom_rect(data = periods_4_plus, 
           aes(xmin = date_only, xmax = date_only + 1, 
               ymin = -Inf, ymax = Inf),
           fill = "goldenrod", alpha = 0.2, inherit.aes = FALSE) +
 # Fish timelines
 geom_segment(aes(x = first_detection, xend = last_detection, 
                  yend = transmitter_id), 
              size = 2, color = "black") +
 geom_point(aes(x = first_detection), color = "darkgreen", size = 3) +
 geom_point(aes(x = last_detection), color = "darkred", size = 3) +
 labs(title = "Fish Detection Timelines (Orange = 4+ Active Fish)",
      x = "Date",
      y = "Transmitter ID") +
 scale_x_date(date_labels = "%b %Y", breaks = "1 month") +
 theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1))




###what fish were detected at the canal HAM-022??



Canalrec22 <- detectionsrudd_removals_48hrs %>%
 filter(station_no  == 22)




unique(Canalrec22$transmitter_id)


#all unique fish that were detected at the canal rec

#1386 1368  506  508  499 9164 9157 9149 9150 9159 9160

#get a summary like we did for the RBG data that summrizes the dates
#coud then further investigate the fish and do a double check 

canalrec_summary <- Canalrec22 %>%
 group_by(detection_timestamp_EST, transmitter_id)

canalrec_summary$animal_id

# Create summary
summary_df <- canalrec_summary %>%
 group_by(animal_id) %>%
 summarise(
  detection_periods = {
   dates <- sort(unique(as.Date(detection_timestamp_EST)))
   
   if(length(dates) == 0) {
    NA_character_
   } else if(length(dates) == 1) {
    format(dates[1], "%d-%m-%Y")
   } else {
    ranges <- c()
    start_date <- dates[1]
    end_date <- dates[1]
    
    for(i in 2:length(dates)) {
     # Check if next date is consecutive (1 day apart)
     if(as.numeric(dates[i] - end_date) == 1) {
      end_date <- dates[i]
     } else {
      # Save the range or single date
      if(start_date == end_date) {
       ranges <- c(ranges, format(start_date, "%d-%m-%Y"))
      } else {
       ranges <- c(ranges, paste(format(start_date, "%d-%m-%Y"), "to", 
                                 format(end_date, "%d-%m-%Y")))
      }
      # Start new range
      start_date <- dates[i]
      end_date <- dates[i]
     }
    }
    
    # Add the final range or single date
    if(start_date == end_date) {
     ranges <- c(ranges, format(start_date, "%d-%m-%Y"))
    } else {
     ranges <- c(ranges, paste(format(start_date, "%d-%m-%Y"), "to", 
                               format(end_date, "%d-%m-%Y")))
    }
    
    paste(ranges, collapse = ", ")
   }
  },
  .groups = 'drop'
 )

print(summary_df)



rudd9159<-filter(dets_rudd, animal_id=="Rudd_9159")

