
#########################

##QAQC Hamharb telemetry data for management report ##

##
#October 14th 2021 onwards for Rudd data 
unique(Ruddtelem_new2026$EST_release_date_time)

#only want to keep fish that were tagged in 2021 to current

Rudd<- Ruddtelem_new2026 %>% filter(detection_timestamp_EST >"2021-10-14 00:00:00")

unique(Rudd$transmitter_id)
unique(Rudd$tag_serial_number)
unique(Rudd$utc_release_date_time)
# 25 tagged fish here
#missing new batch of tagged fish from 2024
#transmitter IDs are 400-500
unique(Rudd$EST_release_date_time)
#fix station duplicates

detectionsGFGF$station_no[detectionsGFGF$station=="HAM-026"] <-"37"
detectionsGFGF$station_no[detectionsGFGF$station=="HAM-016"] <-"11"
detectionsGFGF$station[detectionsGFGF$station=="HAM-026"] <-"HAM-037"
detectionsGFGF$station[detectionsGFGF$station=="HAM-016"] <-"HAM-011"
detectionsGFGF$station_no[detectionsGFGF$station=="HAM-020"] <-"39"
detectionsGFGF$station[detectionsGFGF$station=="HAM-020"] <-"HAM-039"

##################

#### now to add some variables to the dataset

### Day of Year ##
detectionsGFGF$doy<- strftime(detectionsGFGF$detection_timestamp_EST, format = "%j")
detectionsGFGF$doy<-as.numeric(detectionsGFGF$doy)

detectionsGFGF$gen_season<-NA

detectionsGFGF$gen_season<-
  ifelse((detectionsGFGF$doy > 110) & (detectionsGFGF$doy<=161 ), "Spring",  #april 20th
         ifelse((detectionsGFGF$doy > 161) & (detectionsGFGF$doy<=274 ), "Summer",  #june 10th
                ifelse((detectionsGFGF$doy > 274) & (detectionsGFGF$doy<=324 ), "Fall", #oct 1st
                       ifelse((detectionsGFGF$doy > 324), "Winter",  #nov 20th
                              ifelse((detectionsGFGF$doy<=110 ), "Winter",NA)))))


####label whether in or out of harbour ####
detectionsGFGF$harbourpresence<-NA
detectionsGFGF$harbourpresence<-ifelse(detectionsGFGF$glatos_array=="HAM", "In","Out")



##saving this updated file now 
saveRDS(detectionsGFGF, "./goldfishdetections1_filtered_2015-2025.rds")
detectionsGF<-readRDS("./goldfishdetections1_filtered_2015-2025.rds")

###########

############## receiver groupings ########
detectionsGF$group<-ifelse(detectionsGF$station  %in% c( "HAM-052", "HAM-053", "HAM-054", "HAM-055", "HAM-056", "HAM-057","HAM-011",  "HAM-005","HAM-016"),"Piers 5-7", ##good.
                         ifelse(detectionsGF$station  %in% c( "HAM-030", "HAM-031", "HAM-032", "HAM-042", "HAM-043", "HAM-044","HAM-065"),"Cootes Paradise", #good.
                                ifelse(detectionsGF$station  %in% c( "HAM-029", "HAM-033", "HAM-059", "HAM-060", "HAM-061", "HAM-062","HAM-064","HAM-066","HAM-067", "HAM-068" ),"Grindstone", #good
                                       # ifelse(detectionsGF$station  %in% c( "HAM-051"),"Bayfront",
                                       #ifelse(detectionsGF$station  %in% c( "HAM-036"),"Macassa Bay",
                                       ifelse(detectionsGF$station  %in% c( "HAM-048", "HAM-018", "HAM-028", "HAM-037","HAM-063","HAM-024","HAM-027","HAM-026", "HAM-051","HAM-036"),"West End",
                                              ifelse(detectionsGF$station  %in% c( "HAM-034", "HAM-009"),"Slip",
                                                     ifelse(detectionsGF$station  %in% c( "HAM-008",  "HAM-010","HAM-035"),"Windermere",
                                                            ifelse(detectionsGF$station  %in% c( "HAM-013", "HAM-003", "HAM-002","HAM-012", "HAM-045", "HAM-006", "HAM-025","HAM-058","HAM-001"),"East End",
                                                                   ifelse(detectionsGF$station  %in% c( "HAM-022"),"Outside Harbour",
                                                                          ifelse(detectionsGF$station  %in% lake,"Outside Harbour",
                                                                                 # ifelse(detectionsGF$station  %in% c( "HAM-012", "HAM-045", "HAM-006", "HAM-025","HAM-058","HAM-001"),"North-east End",
                                                                                 ifelse(detectionsGF$station  %in% c( "HAM-046", "HAM-047", "HAM-007", "HAM-015", "HAM-014",  "HAM-023"),"North shore",
                                                                                        #    ifelse(detectionsGF$station  %in% c( "HAM-014",  "HAM-023"),"North-west End",
                                                                                        #       ifelse(detectionsGF$station  %in% c( "HAM-011",  "HAM-005","HAM-016"),"Outside Piers 5-7",
                                                                                        #  ifelse(detectionsGF$station  %in% c( "HAM-019", "HAM-039",  "HAM-041"),"Randle Reef area",
                                                                                        ifelse(detectionsGF$station  %in% c( "HAM-021", "HAM-004","HAM-017", "HAM-020","HAM-019", "HAM-039",  "HAM-041"),"Central",NA)))))))))))






##########

###QAQC the goldfish data for dead and alive fish, clip off any data were tags are sitting (i.e dead) etc.

#plot depth and abacus plots for remaining fish

##depth series plots - to QAQC mortalities/depth sensor errors, and visualize the data available.
###subset for fish that have detectionsGF since last QAQC

fish_plot<-detectionsGF %>% group_by(transmitter_id) %>% summarise(n=n_distinct(detection_timestamp_EST), maxtime=max(detection_timestamp_EST)) 
fish_plot <-fish_plot %>% filter(maxtime>as.POSIXct("2022-11-01 00:00:00"))


##create a subfolder for the depth series plots
dir.create(file.path("./Individual Plots goldfish/AbacusDepth"), recursive = TRUE)

###loop to make individual plots per unique fish ID

tags<-unique(fish_plot$transmitter_id)

for(i in 1:length(tags)){
  #i<-1
  db<- subset(detectionsGF, detectionsGF$transmitter_id == paste0(tags[i]))
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
    geom_point(col="chartreuse3",size=2)+
    # geom_line( aes(x= detection_timestamp_EST), col="firebrick1", size =1)+
    #geom_point(aes(x=end),shape= 4,col="black", size = 3)+
    # geom_segment(aes(x = min,yend=transmitter_id,xend = max),size=1)+
    labs(x="Date",y="Station")+
    xlim(first,last)+
        scale_x_datetime(labels = date_format("%m-%Y"))+
    theme_bw()+
    theme(text = element_text(size=14))#+facet_wrap(~common_name_e, scales="free_y")
  p1  
  
if( n_distinct(db$SensorType)<2){
  p2 <- ggplot(data=db, aes(x= detection_timestamp_EST,y=Sensor.Val)) + 
    #geom_line(aes(group=1))+#, alpha=0.8, col="black") + 
    geom_point(col="black")+
    
    labs(x="Date", y="Depth (m)")+#scale_y_reverse()+
    scale_x_datetime(labels = date_format("%m-%Y"))+
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
    scale_x_datetime(labels = date_format("%m-%Y"))+
    
    theme_bw()+theme(text=element_text(size=14))+
    scale_colour_manual(labels=c("Depth","Temperature"), values=c("black","tomato"))
  
  #p2
  }
  #margin = theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))
  hmmm<-ggarrange(p1,p2,nrow=1)
  hmmm<-annotate_figure(hmmm,
                        top = text_grob(paste('Tag ID',id1, '-', species, ''), color = "black", face = "bold", size = 14))
  
  ggsave(plot=hmmm, paste0("./Individual Plots goldfish/AbacusDepth",species,"_ID",id1,"_abacus_depth_plots.png"),  width = 35, height = 20,units = "cm", dpi = 400, bg="white")
}

###Going through each individual and clipping any data if needed

#ID 1376 needs to be clipped basically from mid May 2023 (doesnt actually provide much tagging info just a month
#I would drop from analysis)

#ID 1382 DEAD- TAGGING MORT drop from analysis 

#IF 

