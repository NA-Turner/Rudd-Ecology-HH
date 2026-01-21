#########################
### daily presence/absence of fish at receivers in HH ####################
###code N.Turner and S.Larocque
#########################

#libraries
memory.limit(840000000)
library(dplyr)
library(glatos)
library(data.table)
library(lubridate)
library(ggplot2)
library(rgdal)
###### load up QAQC detection dataset #########
detections<-readRDS( "./01_data/03_large_files_LFS/02_processed_files/rudd_detections_CLEAN.rds")
unique(detections$utc_release_date_time)
#see scripts "Process GLATOS detection data" (in 02_scripts/01_functions) for min lag/false detection filtering 
#see "dataclipping_abacus" script as wel 

#### select date range of study #####
#start = 2021-10-21
####for HH habitat analyses - only including data after spring 2016
detections1<- detections %>% filter(detection_timestamp_EST>"2021-10-20 00:00:00")

### filter to only HH detections
detections1HH<-detections1 %>% filter(glatos_array=="HAM")

### can group sites in a certain order in future. 
detections1HH$transmitter_id<-as.factor(as.numeric(detections1HH$transmitter_id))

################################################################################
### remove extra detections off multiple receivers on a single ping. Keep first ping only. See if changes dataset compared to not doing this. 


###### remove detections of same individual on different receivers within the minimum nominal delay to avoid multiple detections 
#from the same single ping.
###trying to avoid bias of array having different # of receivers in different locations. 
#mmm doesn't quite handle that perfectly as they can still be detected more frequently in an area with coverage vs no coverage

###split by individual and remove detections less than the min lag (i.e., a ping that was detected on more than one receiver)
#keeps the first detection from the ping tho
ind<-unique(detections$animal_id)
singleping<-data.frame()
unique(detections$Min.Delay)

#loop to go through all individuals
for (t in 1:length(ind)){
  
  temp <- subset(detections, detections$animal_id == paste0(ind[t]))
  temp <- temp[order(temp$detection_timestamp_EST, decreasing=F),]
  lag<-min(temp$Min.Delay)
  
  ###### calculate and remove values with min nominal delay (~<120 s) time gap
  first_date <- temp$detection_timestamp_EST[1:(length(temp$detection_timestamp_EST)-1)]
  second_date <- temp$detection_timestamp_EST[2:length(temp$detection_timestamp_EST)]
  second_gap <- difftime(second_date, first_date, units="s")
  
  dup_index <- second_gap>lag
  dup_index <- c(TRUE, dup_index)
  temp<-temp[dup_index, ]
  
  ##################################################
  singleping<-bind_rows(singleping,temp)
}

###from before....
###changes the # of detections from 265284 to 116828 --- wow!
##let's see if there is a difference - not really LOL

detections<-singleping
##all good
hmm<-subset(singleping, is.na(animal_id))
#saveRDS(single, paste0("./SimpleAnalyses/",spp[i],"/",spp[i],"_QAQC_detections_onercvrperping_2015-2020.rds"))



################################################################################
##### get daily presence #####
daily<-detections %>% group_by(transmitter_id, common_name_e, date, station) %>% summarise(n_detections=n_distinct(detection_timestamp_utc))
daily$date<-force_tz(daily$date,'America/Toronto')

daily$date2<-as.POSIXct(format(daily$date, format="%Y-%m-%d %H:%M:%S", tz=""))
daily$date2<-force_tz(daily$date2,'America/Toronto')

##saved based on if single ping run or not.
saveRDS(daily, "./Data/HH_daily_presence_2016-2022_no30_may2024.rds")
saveRDS(daily, "./Data/HH_daily_presence_2016-2022_no30_may2024_singleping.rds")

###if starting from here... 
daily<-readRDS("./Data/Telemetry/HH_daily_presence_2016-2022_no30_may2024.rds")
daily<-readRDS("./Data/Telemetry/HH_daily_presence_2016-2022_no30_may2024_singleping.rds")

#### get daily station presence #####
# convert pos2 to data.table
setDT(daily)
# extract unique bin_timestamp from the interpolated data
int <- unique(daily, by = "date2")

##################################################
### load receivers #####
####load receivers from GLATOS file. This is provided with GLATOS query
receivers<-read_glatos_receivers("./Data/Telemetry/GLATOS_receiverLocations_20221228_211803.csv")

HAM_rcvrs<-receivers %>% filter(glatos_array=="HAM")

##########################################################
### plot receiver map and abacus plot ###
#### Plot out location of receivers ####

###choose great lakes shoreline - best detailed shapefile I have found for HH details
#shorelinemap<- readOGR("./Data/GIS/Shapefiles", "LakeOntShoreline_MajorWaters")

###need to convert to lat long from UTMS or points
#map_NAD83 <- spTransform(shorelinemap, CRS("+proj=longlat +datum=NAD83"))

####load hamilton harbour shapefile
HHshoreline<- readOGR("./Data/GIS/Shapefiles", "HH_WaterPolygons_01June2023")
###need to convert to lat long from UTMS or points
HHmap_NAD83 <- spTransform(HHshoreline, CRS("+proj=longlat +datum=NAD83"))

####
# Subset receivers to include only receivers that were deployed during the detection interval.
### round it off
newfirst <-"2016-04-30 00:00:00"
last<-"2022-05-01 00:00:00"

# Subset receiver deployments oustide the detection period.
# !is.na(rec$recover_date_time) eliminates receivers that have been
# deployed but not yet recovered.
##lets just focus on Hamilton Harbour specifically for now. 
plot_rec <- receivers[receivers$deploy_date_time < last & receivers$recover_date_time > newfirst &
                        !is.na(receivers$recover_date_time) & receivers$glatos_ins_frequency==69 & 
                        receivers$glatos_array=="HAM",]


plot_rec$deploy_date<-as.Date(plot_rec$deploy_date_time,origin="1970-01-01")
plot_rec$deploy_date<-ifelse(plot_rec$deploy_date<as.Date("2016-04-29",origin="1970-01-01"),as.Date("2016-04-29",origin="1970-01-01"),(plot_rec$deploy_date))
plot_rec$deploy_date<-as.Date(plot_rec$deploy_date,origin="1970-01-01")
###filter deploy if after end date
plot_rec <- plot_rec %>% filter(!deploy_date>"2022-05-01")

plot_rec$recover_date<-as.Date(plot_rec$recover_date_time,origin="1970-01-01")

plot_rec$recover_date<-ifelse(plot_rec$recover_date>as.Date("2022-05-01",origin="1970-01-01"),as.Date("2022-05-01",origin="1970-01-01"),(plot_rec$recover_date))
plot_rec$recover_date<-as.Date(plot_rec$recover_date,origin="1970-01-01")
###filter recovery if before start date
plot_rec <- plot_rec %>% filter(!recover_date<"2016-04-29")

plot_rec$deploy_year<-as.numeric(format(plot_rec$deploy_date, format= "%Y") )
plot_rec$recover_year<-as.numeric(format(plot_rec$recover_date, format= "%Y") )

plot_rec<-plot_rec %>% filter(!station_no=="64")
plot_rec<-plot_rec %>% filter(!station_no=="68")
plot_rec<-plot_rec %>% filter(!station_no=="22")
plot_rec<-plot_rec %>% filter(!station_no=="43")
#### to map in Arcmap
map<- plot_rec %>% group_by(station_no ) %>%dplyr::summarize(lat=mean(deploy_lat),long=mean(deploy_long), first=min(deploy_year), last=max(recover_year))
write.csv(map, "./Data/GIS/HH_receiver_locations_2016-2022.csv")

##figure out what where receivers are
### some have different deploy lat/longs at same receiver. Make sure they are not on land and see if they can be averaged
where<- plot_rec %>% group_by(station_no) %>% summarise(min=min(deploy_year), lat=mean(deploy_lat), long=mean(deploy_long))
where$min<-as.factor(where$min)



p<-ggplot()+
  #geom_polygon(data=map_NAD83,aes(x=long, y = lat, group=group),col="black", fill="white")+ 
  #coord_fixed(1.3)+
  geom_sf() +
  geom_polygon(data = HHmap_NAD83, aes(x = long, y = lat, group = group),fill="grey", colour = "grey35")+
  theme(legend.position = "left") +
  geom_text(data=where, aes(label=station_no,x = long, y = lat),hjust = 0, nudge_x = 0.0015,fontface = "bold")+
  #geom_polygon(data=gl[!gl$id %in% gl[gl$hole,]$id,],aes(x = long, y = lat, group = group), fill="white")+
  geom_point(data = where,aes(x = long, y = lat, fill=min), size = 3,   shape=21)+ 
  #  geom_text(data = where,aes(x = deploy_long, y = deploy_lat,label=station_no),hjust = 0, nudge_x = 0.0025)+
  coord_sf(xlim= c(-79.94,-79.767), ylim=c(43.26,43.315))+
  labs(x="Latitude", y="Longitude", fill="Deploy Year")+
 # scale_fill_manual(values = c("grey","white"))+
  theme_bw()+theme(text = element_text(size=16))

p

##west end inset
p2<-ggplot()+
  #geom_polygon(data=map_NAD83,aes(x=long, y = lat, group=group),col="black", fill="white")+ 
  #coord_fixed(1.3)+
  geom_sf() +
  geom_polygon(data = HHmap_NAD83, aes(x = long, y = lat, group = group),fill="grey", colour = "grey35")+
 
  geom_text(data=where, aes(label=station_no,x = long, y = lat), size =5,hjust = 0, nudge_x = 0.0005,fontface = "bold")+
  #geom_polygon(data=gl[!gl$id %in% gl[gl$hole,]$id,],aes(x = long, y = lat, group = group), fill="white")+
  geom_point(data = where,aes(x = long, y = lat, fill=min), size = 4,   shape=21)+ 
  #  geom_text(data = where,aes(x = deploy_long, y = deploy_lat,label=station_no),hjust = 0, nudge_x = 0.0025)+
  coord_sf(xlim=c(-79.895,-79.858), ylim=c(43.269,43.296))+
  labs(x="Latitude", y="Longitude", fill="Deploy Year")+
  # scale_fill_manual(values = c("grey","white"))+
  theme_bw()+ theme(legend.position = "none",text = element_text(size=16)) 

p2

###########
##combine 2 plots
library(ggpubr)
ggarrange(p, p2, labels = c("A", "B"), heights = c(2, 1.5),
          ncol = 1, nrow = 2,
          common.legend = TRUE, legend = "right")

##### Abacus plot of duration of deployment for each Hamilton Harbour receiver

### can group sites in a certain order in future. 
plot_rec2<-plot_rec
plot_rec2$station_no<-as.factor(as.numeric(plot_rec2$station_no))

p1<-ggplot(data=plot_rec2, aes(y=reorder(station_no, desc(station_no))))+
  geom_point(aes(x=deploy_date), col="chartreuse3",size=3)+
  geom_point(aes(x=recover_date), col="firebrick1", size = 2)+
  geom_segment(aes(x = deploy_date,yend=station_no,xend = recover_date),size=1)+
  labs(x="Date",y="Station No.")+
  theme_bw()+
  theme(text = element_text(size=16))

p1  

ggsave(plot=p1, "./Results/General/HH_Receiver_Deployment_Duration_sp2016-sp2022_new.png",  width = 25, height = 35,units = "cm", dpi = 400)

### which receivers are present for long enough duration or consistency for analyses required?
p2<-ggplot(data=plot_rec2, aes(y=reorder(station_no, desc(station_no))))+
  geom_point(aes(x=deploy_date_time), col="chartreuse3",size=3)+
  geom_point(aes(x=recover_date_time), col="firebrick1", size = 2)+
  geom_segment(aes(x = deploy_date_time,yend=station_no,xend = recover_date_time),size=1)+
  labs(x="Date",y="Station No.")+
  theme_bw()+
  theme(text = element_text(size=16))

p2  



# Add bin_timestamp to receivers, based on deploy/recover timestamps.
# Removes unnecessary columns in output to simplify This is a data.table
# non-equi join...
setDT(HAM_rcvrs)
recs <- HAM_rcvrs[int, .(deploy_lat = x.deploy_lat, deploy_long = x.deploy_long,
                        station_no=x.station_no, station = x.station, deploy_date_time = x.deploy_date_time, recover_date_time = x.recover_date_time,
                         date = i.date), on = .(deploy_date_time <= date,
                                                                  recover_date_time >= date), allow.cartesian = TRUE]
### now have... 
###### receivers present during study period for each day #####

###keep location stagnant and not shift around to reduce artificial induced spatial autocorrelation
recs<-left_join(recs,where, by="station_no")

recs<-na.omit(recs)

#### want to add in habitat data to receivers
habitat<-read.csv("./Data/Habitat/habitat_rcvr_LoS_clusters_final_july2022.csv")

habitat2<-habitat %>% dplyr::select(station, WL_AVG_mean,WL_AVG_SD,SAV_mean, SLOPE_mean,Weighted.Fetch.m.Mean, Cluster_group2, Detailed_group)

hab_recs<-left_join(recs,habitat2, by="station")
hab_daily<-left_join(daily, habitat2, by="station")

saveRDS(hab_daily, "./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_may2024.rds")
saveRDS(hab_daily, "./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_may2024_singleping.rds")

saveRDS(hab_recs, "./Data/Telemetry/HH_daily_receiver_presence_2016-2022_habitat_static_latlon_may2024.rds")


######################## an addition
##want to add in some more habitat variables - 350 m range
recs<-readRDS("./Data/Telemetry/HH_daily_receiver_presence_2016-2022_habitat_static_latlon_may2024.rds")
daily<-readRDS("./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_may2024.rds")
daily<-readRDS("./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_may2024_singleping.rds")


habitat_more<-read.csv("./Data/Habitat/hh_hard_substrate_rcvrbuff_aug2023.csv")
habitat2_more<-habitat_more %>% dplyr::select(station, mean_prop_hard)

hab_recs<-left_join(recs,habitat2_more, by="station")
hab_daily<-left_join(daily, habitat2_more, by="station")

habitat_more2<-read.csv("./Data/Habitat/HH_Receivers_DistWetland_RiverMouth_EVPresence_wRedhIllMarsh_Aug2023.csv")
habitat2_more2<-habitat_more2 %>% dplyr::select(station, RM_DistFix,Emerg_Pres,WL_DistFix,ClosestWL)

hab_recs<-left_join(hab_recs,habitat2_more2, by="station")
hab_daily<-left_join(hab_daily, habitat2_more2, by="station")

habitat_more3<-read.csv("./Data/Habitat/habitat_rcvr_LoS350buffer_secchi.csv")
habitat2_more3<-habitat_more3 %>% dplyr::select(station, secchi)

hab_recs<-left_join(hab_recs,habitat2_more3, by="station")
hab_daily<-left_join(hab_daily, habitat2_more3, by="station")

##updated SAV from water level 75m
habitat_more4<-read.csv("./Data/Habitat/hh_rcvr_350buff_75SAV_june2024.csv")

hab_recs<-left_join(hab_recs,habitat_more4, by="station")
hab_daily<-left_join(hab_daily, habitat_more4, by="station")


## no detections on receiver 64 - fish can't access it. removing it from analyses.
hab_daily<-hab_daily %>% filter(!station=="HAM-064")
hab_recs<-hab_recs %>% filter(!station=="HAM-064")

## receiver 68 was only deployed for a short period of time (2 weeks). removing it from analyses.
hab_daily<-hab_daily %>% filter(!station=="HAM-068")
hab_recs<-hab_recs %>% filter(!station=="HAM-068")

##receiver is on Lake Ont side and skews fetch values severely
hab_daily<-hab_daily %>% filter(!station=="HAM-022")
hab_recs<-hab_recs %>% filter(!station=="HAM-022")

unique(hab_daily$station)
saveRDS(hab_daily, "./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_june2024.rds")
saveRDS(hab_daily, "./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_may2024_singleping.rds")

saveRDS(hab_recs, "./Data/Telemetry/HH_daily_receiver_presence_2016-2022_habitat_static_latlon_june2024.rds")

#################################################################

#recs<-readRDS("./Data/Telemetry/HH_daily_receiver_presence_2016-2022_habitat_static_latlon_may2024.rds")
#daily<-readRDS("./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_may2024.rds")

###remove old SAV
#recs<-recs %>% select(!SAV_mean & !SAV)
#daily<-daily %>% select(!SAV_mean & !SAV)

##updated SAV from water level 75m
#habitat_more4<-read.csv("./Data/Habitat/hh_rcvr_350buff_75SAV_june2024.csv")

#hab_recs<-left_join(recs,habitat_more4, by="station")
#hab_daily<-left_join(daily, habitat_more4, by="station")

##used code above to save. updated code above too. 

#################################################
##want to add in some more habitat variables - 300 m range

recs<-readRDS("./Data/Telemetry/HH_daily_receiver_presence_2016-2022_habitat_static_latlon_may2024.rds")
daily<-readRDS("./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_may2024.rds")
#daily<-readRDS("./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_may2024_singleping.rds")

###remove columns with 350 m habitat values. 
recs = subset(recs, select = -c(WL_AVG_mean, WL_AVG_SD,SAV_mean, SLOPE_mean,Weighted.Fetch.m.Mean, secchi, SAV, mean_prop_hard) )
daily = subset(daily, select = -c(WL_AVG_mean, WL_AVG_SD,SAV_mean, SLOPE_mean,Weighted.Fetch.m.Mean, secchi, SAV, mean_prop_hard) )

habitat_more<-read.csv("./Data/Habitat/DetectionSensitivity/hh_rcvr_300los_habitat.csv")

hab_recs<-left_join(recs,habitat_more, by="station")
hab_daily<-left_join(daily, habitat_more, by="station")

saveRDS(hab_daily, "./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_may2024_300m.rds")
#saveRDS(hab_daily, "./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_may2024_singleping.rds")

saveRDS(hab_recs, "./Data/Telemetry/HH_daily_receiver_presence_2016-2022_habitat_static_latlon_may2024_300m.rds")

###################################################################################################


### add secchi to 50m grid file - Completed

#grid<-read.csv("./Data/HH_50mGrid_Depth_SAV_Fetch_WetlandDist_06June2023_new.csv")
#secchi<-read.csv("./Data/Habitat/hh_50mgrid_avgsecchi.csv")

#secchi<-secchi %>% select(ID, secchi)

#grid<-left_join(grid,secchi, by="ID")

#write.csv(grid, "./Data/HH_50mGrid_Depth_SAV_Fetch_WetlandDist_06June2023_final.csv")

##################
### add new SAV to 50m grid file - Completed

#grid<-read.csv("./Data/HH_50mGrid_Depth_SAV_Fetch_WetlandDist_06June2023_final.csv")
#sav<-read.csv("./Data/Habitat/hh_50mgrid_75SAVbuff_scrubbed.csv")

#grid<-left_join(grid,sav, by="ID")

#write.csv(grid, "./Data/HH_50mGrid_Depth_SAV_Fetch_WetlandDist_4Oct2023.csv")

##############