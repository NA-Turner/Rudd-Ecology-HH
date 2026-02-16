#########################
### daily presence/absence of fish at receivers in HH ####################

#libraries
library(dplyr)
library(glatos)
library(data.table)
library(lubridate)
library(ggplot2)
library(rgdal)
###### load up QAQC detection dataset #########
dets_rudd <- readRDS("~/For Github/Rudd-Ecology-HH1/01_data/03_large_files_LFS/02_processed_files/rudd_dets_rudd_QAQC.rds")

#### select date range of study #####
#based on the timeline plots of when we have 4 or more fish detected
#filtering data to aling with that timeline
####for HH habitat analyses - only including data until spring 2022 
#dets_rudd<- dets_rudd %>% filter(detection_timestamp_EST<"2021-10-24 00:00:00")
####for HH habitat analyses - only including data after spring 2016
dets_rudd1<- dets_rudd %>% filter(detection_timestamp_EST>"2021-10-24 00:00:00")

dets_rudd2 <- dets_rudd1 %>%
 filter(
  !(as.Date(detection_timestamp_EST) >= as.Date("2023-04-15") & 
     as.Date(detection_timestamp_EST) <= as.Date("2023-04-21")) &
   !(as.Date(detection_timestamp_EST) >= as.Date("2023-12-24") & 
      as.Date(detection_timestamp_EST) <= as.Date("2024-04-19"))
 )

unique(dets_rudd2$glatos_array)

### filter to only HH dets_rudd
dets_rudd2<-dets_rudd2 %>% filter(glatos_array=="HAM")

########################################################
##for the boosted regression tree analysis round 1 
#going to use array from SL paper so will need to filter out all receivers that were not included in SL paper
## Using semi_join (recommended - cleanest method)
filtered_detections <- dets_rudd2 %>%
 semi_join(Recs_usedinSLpaper, by = "station")


#save this dataframe to be used in the RF model round 1 analysis 
saveRDS(filtered_detections, "./01_data/03_large_files_LFS/Rudd_preppedforRFmodelPADF.rds")
filtered_detections <- readRDS("~/For Github/Rudd-Ecology-HH1/01_data/03_large_files_LFS/02_processed_files/Rudd_preppedforRFmodelPADF.rds")
################################################################################
### remove extra detections off multiple receivers on a single ping. 
#Keep first ping only. See if changes dataset compared to not doing this. 

###### remove detections of same individual on different receivers within the minimum nominal delay to avoid multiple detections
#from the same single ping.
###trying to avoid bias of array having different # of receivers in different locations. 
#mmm doesn't quite handle that perfectly as they can still be detected more frequently in an area with coverage vs no coverage

###split by individual and remove dets_rudd less than the min lag (i.e., a ping that was detected on more than one receiver)
#keeps the first detection from the ping tho
ind<-unique(filtered_detections$animal_id)
singleping<-data.frame()
unique(filtered_detections$Min)


#loop to go through all individuals
for (t in 1:length(ind)){
  
  temp <- subset(filtered_detections, filtered_detections$animal_id == paste0(ind[t]))
  temp <- temp[order(temp$detection_timestamp_EST, decreasing=F),]
  lag<-min(temp$Min)
  
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

# removes 913k detections from original dataframe 
filtered_detections1<-singleping
##all good
hmm<-subset(filtered_detections, is.na(animal_id))
#saveRDS(single, paste0("./SimpleAnalyses/",spp[i],"/",spp[i],"_QAQC_dets_rudd_onercvrperping_2015-2020.rds"))



################################################################################
##### get daily presence #####
#what was kept as the single locaiton ? highest daiy detection freq or 
##### get daily presence #####


##edited form SL code 
#keeps station with the most detections per fish per day 
daily <- filtered_detections1 %>% 
 group_by(transmitter_id, common_name_e, date, station) %>% 
 summarise(n_detections = n_distinct(detection_timestamp_utc), .groups = 'drop') %>%
 group_by(transmitter_id, date) %>%
 slice_max(n_detections, n = 1, with_ties = FALSE) %>%
 ungroup()

saveRDS(daily, "./01_data/HH_daily_presence_Rudd_Feb11.rds")

###if starting from here... 
Daily_singleping1 <- readRDS("~/For Github/Rudd-Ecology-HH1/01_data/HH_daily_presence_Rudd_Feb11.rds")
#Daily_al <- readRDS("~/For Github/Rudd-Ecology-HH1/01_data/HH_daily_presence_Rudd.rds")
#### get daily station presence #####

# extract unique bin_timestamp from the interpolated data
int <- unique(Daily_singleping1, by = "date")

##################################################
### load receivers #####
####load receivers from GLATOS file. This is provided with GLATOS query

#Select only station, lat, lon from big dataframe and join to small one

Recs_usedinSLpaper <- Recs_usedinSLpaper %>%
 left_join(Daily_singleping1 %>% dplyr::select(station, deploy_lat, deploy_long), by = "station")

Rudd_preppedforRFmodelPADF$station
Recs_usedinSLpaper$station

receivers<-read_glatos_receivers("./01_data/03_large_files_LFS/01_raw_files/GLATOS_receiverLocations_20260106_154310.csv")
#keep only ones that were used in SL paper 
recs_filtered <- receivers %>%
 semi_join(Recs_usedinSLpaper, by = "station")


##########################################################
#### Plot out location of receivers ####
###load hamilton harbour shapefile ###

HHshoreline<- read_sf("./01_data/04_shapefiles/HH_FineShoreline/HH_WaterLines_02June2023.shp")

###need to convert to lat long from UTMS or points
HHshoreline <- st_transform(HHshoreline, crs = 4326)  # WGS84 example

plot(st_geometry(HHshoreline))

####
# Subset receivers to include only receivers that were included in SL paper
newfirst <-"2021-10-24 00:00:00"
last<-"2025-11-25 00:00:00"


#### to map in Arcmap

colnames(recs_filtered)
map<- recs_filtered %>% group_by(station) %>%dplyr::summarize(lat=mean(deploy_lat),long=mean(deploy_long), first=min(deploy_date_time), last=max(recover_date_time))

write.csv(map, "./01_data/02_processed_files/HH_reclocs_for_RFmodel1.csv")

##figure out  where receivers are
### some have different deploy lat/longs at same receiver. Make sure they are not on land and see if they can be averaged
map<-map %>% filter(!station=="HAM-064")
map<-map %>% filter(!station=="HAM-068")
map<-map %>% filter(!station=="HAM-022")
map<-map %>% filter(!station=="HAM-043")


where<- map %>% group_by(station) %>% summarise(min=min(first), lat=mean(lat), long=mean(long))
where$min<-as.factor(where$min)

#remove these recs as per SL methods 

p<-ggplot()+
  #geom_polygon(data=map_NAD83,aes(x=long, y = lat, group=group),col="black", fill="white")+ 
  #coord_fixed(1.3)+
  geom_sf(data = HHshoreline,fill="grey", colour = "grey35")+
  theme(legend.position = "left") +
  geom_text(data=where, aes(label=station,x = long, y = lat),hjust = 0, nudge_x = 0.0015,fontface = "bold")+
  #geom_polygon(data=gl[!gl$id %in% gl[gl$hole,]$id,],aes(x = long, y = lat, group = group), fill="white")+
  geom_point(data = where,aes(x = long, y = lat, fill=min), size = 3,   shape=21)+ 
  #  geom_text(data = where,aes(x = deploy_long, y = deploy_lat,label=station_no),hjust = 0, nudge_x = 0.0025)+
  coord_sf(xlim= c(-79.94,-79.767), ylim=c(43.26,43.315))+
  labs(x="Latitude", y="Longitude", fill="Deploy Year")+
 # scale_fill_manual(values = c("grey","white"))+
  theme_bw()+theme(text = element_text(size=16), 
                   legend.position = "none")

p

##west end inset
p2<-ggplot()+
  #geom_polygon(data=map_NAD83,aes(x=long, y = lat, group=group),col="black", fill="white")+ 
  #coord_fixed(1.3)+
  geom_sf(data = HHshoreline,fill="grey", colour = "grey35")+
  geom_text(data=where, aes(label=station,x = long, y = lat), size =5,hjust = 0, nudge_x = 0.0005,fontface = "bold")+
  #geom_polygon(data=gl[!gl$id %in% gl[gl$hole,]$id,],aes(x = long, y = lat, group = group), fill="white")+
  geom_point(data = where,aes(x = long, y = lat, fill=min), size = 4,   shape=21)+ 
   geom_text(data = where,aes(x = long, y = lat,label=station),hjust = 0, nudge_x = 0.0025)+
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
          common.legend = TRUE, legend = "none")

##### Abacus plot of duration of deployment for each Hamilton Harbour receiver

### can group sites in a certain order in future. 
recs_filtered2<-recs_filtered
recs_filtered2$station_no<-as.factor(as.numeric(recs_filtered2$station_no))


# View the data
head(plot_rec2[, c("station_no", "deploy_date_time", "recover_date_time")])

recs_filtered2<- recs_filtered2 %>% filter(deploy_date_time >"2021-10-01 00:00:00")

p1<-ggplot(data=recs_filtered2, aes(y=reorder(station_no, desc(station_no))))+
  geom_point(aes(x=deploy_date_time), col="chartreuse3",size=3)+
  geom_point(aes(x=recover_date_time), col="firebrick1", size = 2)+
  geom_segment(aes(x = deploy_date_time,yend=station_no,xend = recover_date_time),size=1)+
  labs(x="Date",y="Station No.")+
  theme_bw()+
  theme(text = element_text(size=16))

p1  

#not critically important right now but recs 13 and 14 having some issues in the plot 

ggsave(plot=p1, "./03_outputs/01_figures/HH_Receiver_Deployment_Duration_Oct2021-endof2025_new.png",  width = 25, height = 35,units = "cm", dpi = 400)

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
recs_filtered_DT<-recs_filtered
head(recs_filtered_DT)
setDT(recs_filtered_DT)

int[, date := as.POSIXct(date)]

recs <- int[recs_filtered_DT, .(deploy_lat = i.deploy_lat, 
                                deploy_long = i.deploy_long,
                                station_no = i.station_no, 
                                station = i.station, 
                                deploy_date_time = i.deploy_date_time, 
                                recover_date_time = i.recover_date_time,
                                date = x.date), 
            on = .(date >= deploy_date_time,
                   date <= recover_date_time), 
            allow.cartesian = TRUE, 
            nomatch = NULL]
head(recs)


### now have... 
###### receivers present during study period for each day #####

###keep location stagnant and not shift around to reduce artificial induced spatial autocorrelation
recs_1<-left_join(recs,where, by="station")



#recs file 
saveRDS(recs_1, "./01_data/03_large_files_LFS/daily_recslocs_meanrecloc.rds")
daily_recslocs_meanrecloc <- readRDS("~/For Github/Rudd-Ecology-HH1/01_data/03_large_files_LFS/daily_recslocs_meanrecloc.rds")

daily_recslocs_meanrecloc_unique <- daily_recslocs_meanrecloc %>%
 distinct(date, station, .keep_all = TRUE)

#### want to add in habitat data to receivers
habitat<-read.csv("./01_data/03_large_files_LFS/02_processed_files/fromSL/habitat_rcvr_LoS_clusters_final_july2022.csv")

habitat2<-habitat %>% dplyr::select(station, WL_AVG_mean,WL_AVG_SD,SAV_mean, SLOPE_mean,Weighted.Fetch.m.Mean, Cluster_group2, Detailed_group)

hab_recs<-left_join(daily_recslocs_meanrecloc_unique,habitat2, by="station")
hab_daily<-left_join(Daily_singleping1, habitat2, by="station")

#saveRDS(hab_daily, "./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_may2024.rds")
#saveRDS(hab_daily, "./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_may2024_singleping.rds")

#saveRDS(hab_recs, "./Data/Telemetry/HH_daily_receiver_presence_2016-2022_habitat_static_latlon_may2024.rds")


######################## an addition
##want to add in some more habitat variables - 350 m range
#recs<-readRDS("./Data/Telemetry/HH_daily_receiver_presence_2016-2022_habitat_static_latlon_may2024.rds")
#daily<-readRDS("./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_may2024.rds")
#daily<-readRDS("./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_may2024_singleping.rds")


habitat_more<-read.csv("./01_data/03_large_files_LFS/02_processed_files/fromSL/hh_hard_substrate_rcvrbuff_aug2023.csv")
habitat2_more<-habitat_more %>% dplyr::select(station, mean_prop_hard)

hab_recs<-left_join(hab_recs,habitat2_more, by="station")
hab_daily<-left_join(hab_daily, habitat2_more, by="station")

habitat_more2<-read.csv("./01_data/03_large_files_LFS/02_processed_files/fromSL/HH_Receivers_DistWetland_RiverMouth_EVPresence_wRedhIllMarsh_Aug2023.csv")
habitat2_more2<-habitat_more2 %>% dplyr::select(station, RM_DistFix,Emerg_Pres,WL_DistFix,ClosestWL)

hab_recs<-left_join(hab_recs,habitat2_more2, by="station")
hab_daily<-left_join(hab_daily, habitat2_more2, by="station")

habitat_more3<-read.csv("./01_data/03_large_files_LFS/02_processed_files/fromSL/habitat_rcvr_LoS350buffer_secchi.csv")
habitat2_more3<-habitat_more3 %>% dplyr::select(station, secchi)

hab_recs<-left_join(hab_recs,habitat2_more3, by="station")
hab_daily<-left_join(hab_daily, habitat2_more3, by="station")

##updated SAV from water level 75m
habitat_more4<-read.csv("./01_data/03_large_files_LFS/02_processed_files/fromSL/hh_rcvr_350buff_75SAV_june2024.csv")

hab_recs<-left_join(hab_recs,habitat_more4, by="station")
hab_daily<-left_join(hab_daily, habitat_more4, by="station")




#have only save this file not the above one
saveRDS(hab_daily, "./01_data/03_large_files_LFS/02_processed_files/HH_daily_presence__habitat_singleping_Feb11.rds")

saveRDS(hab_recs, "./01_data/03_large_files_LFS/02_processed_files/HH_daily_receiver_presence__habitat_static_latlon.rds")



#################################################################

#did not do the below 300m one 

#################################################
##want to add in some more habitat variables - 300 m range
###remove columns with 350 m habitat values. 
#recs = subset(recs, select = -c(WL_AVG_mean, WL_AVG_SD,SAV_mean, SLOPE_mean,Weighted.Fetch.m.Mean, secchi, SAV, mean_prop_hard) )
#daily = subset(daily, select = -c(WL_AVG_mean, WL_AVG_SD,SAV_mean, SLOPE_mean,Weighted.Fetch.m.Mean, secchi, SAV, mean_prop_hard) )

#habitat_more<-read.csv("./Data/Habitat/dets_ruddensitivity/hh_rcvr_300los_habitat.csv")

#hab_recs<-left_join(recs,habitat_more, by="station")
#hab_daily<-left_join(daily, habitat_more, by="station")

#saveRDS(hab_daily, "./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_may2024_300m.rds")
#saveRDS(hab_daily, "./Data/Telemetry/HH_daily_presence_2016-2022_habitat_no30_may2024_singleping.rds")

#saveRDS(hab_recs, "./Data/Telemetry/HH_daily_receiver_presence_2016-2022_habitat_static_latlon_may2024_300m.rds")

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