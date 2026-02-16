############################
## Daily presence of detected fish, and receivers in system 
## Get random pseudo absence or conduct with imbalanced data
## Try both ways. Using imbalanced data for now as matches TH paper.
gc()

###########################
##load libraries
library(dplyr)
library(ggmap)
library(ggplot2)
library(tidyr)
library(patchwork)
library(plotly)
library(sjPlot)
#library(ggsn)
library(randomForest)
library(caret)
library(pdp)
library(iml)
library(ROCR)
library(pROC)
library(lme4)
library(data.table)
#library(rgdal) <- depreciated
library(sf)
library(viridis)
library(gridExtra)
library(raster)
#-------------------------------------------------------------#
### Function to simulate random forest model accuracy across a range of parameters
#-------------------------------------------------------------#

function_SimulateAccuracy <- function(predictor="SSp",
                                      data_Train,
                                      data_Test,
                                      loopweight = c(1,2,5,10,20,50,100),
                                      na.response = na.roughfix,
                                      model.formula= SSp ~.
){
  df_Loop_SimOutput <- data.frame()
  
  #Loop to build multiple Random Forest models
  #----------------------------#
  for(i in unique(loopweight)){
    
    print(paste("Simulating weight:", i))
    ###Simulate random forest perfomance for model tuning
    #----------------------------#
    model_RF_Loop <- randomForest(formula=model.formula, data = data_Train, replace=TRUE, 
                                  na.action=na.response, #Alternative to `na.action=na.omit,` Grow forest with NAs set as median value
                                  classwt=c(1, i), #Over-weight presences in zero-inflated data to balance class accuracy at cost of prediction accuracy
                                  importance=TRUE, do.trace=1000, ntree=1000)
    
    ###Investigate RandomForest model
    #----------------------------#
    #Make confusion Matrix on test dataset with 'caret'
    data_Test$prediction1 <- predict(model_RF_Loop, data_Test)
    df_loopA <- caret::confusionMatrix(data=data_Test$prediction1, reference=data_Test[[predictor]],  positive="1")
    df_loopA <- as.vector(df_loopA$table)
    df_loopB <- data.frame(SimulatedPresenceWeight = i,
                           AccurateAbsence = df_loopA[1],
                           FalsePredictedPresence = df_loopA[2],
                           FalsePredictedAbsence = df_loopA[3],
                           AccuratePresence = df_loopA[4])
    df_Loop_SimOutput <-rbind(df_Loop_SimOutput, df_loopB)
    
    rm(df_loopA); rm(df_loopB)
  }
  
  #Produce output
  #----------------------------#
  df_Loop_SimOutput <- df_Loop_SimOutput  %>% 
    #Describe Accuracy from confusion Matrices
    mutate(PredictionAccuracy = (AccurateAbsence+AccuratePresence)/
             (AccurateAbsence+AccuratePresence+FalsePredictedPresence+FalsePredictedAbsence),
           AbsenceAccuracy = AccurateAbsence/(AccurateAbsence+FalsePredictedPresence),
           PresenceAccuracy = AccuratePresence/(AccuratePresence+FalsePredictedAbsence)
    )
  
  #Plot output
  #----------------------------#
  a <- df_Loop_SimOutput  %>% 
    #Format data for plotting
    pivot_longer(cols = c(PredictionAccuracy, AbsenceAccuracy, PresenceAccuracy),
                 names_to = "AccuracyMetric", values_to = "Accuracy") %>% 
    #Plot the data
    ggplot(aes(x=as.factor(SimulatedPresenceWeight), y=Accuracy, colour=AccuracyMetric))+
    geom_line(aes(group=AccuracyMetric), size=1.5)+
    geom_point(size=3)+
    scale_colour_manual(values=c("#B21038", "black", "#0FA3B1"))+
    xlab("Simulated 'classwt' weight")
  suppressMessages(print(a))
  
  return(df_AccuracySimulationOutput=df_Loop_SimOutput)
}


##########################################################
##load up daily presence of detected fish

#update with QAQC and different detection ranges.

##file options: 
##HH_daily_presence_2016-2022_habitat_no30_may2024
##HH_daily_presence_2016-2022_habitat_no30_may2024_300m

##HH_daily_receiver_presence_2016-2022_habitat_static_latlon_may2024
##HH_daily_receiver_presence_2016-2022_habitat_static_latlon_may2024_300m

######right now for rudd these are teh data files 
#saveRDS(hab_daily, "./01_data/03_large_files_LFS/02_processed_files/HH_daily_presence__habitat_singleping.rds")
#saveRDS(hab_recs, "./01_data/03_large_files_LFS/02_processed_files/HH_daily_receiver_presence__habitat_static_latlon.rds")


#hab_recs file 

hab_recs <- readRDS("./01_data/03_large_files_LFS/02_processed_files/HH_daily_receiver_presence__habitat_static_latlon.rds")
#hab_daily<-readRDS("./01_data/03_large_files_LFS/02_processed_files/HH_daily_presence__habitat_singleping.rds")

#slightly updated
hab_daily<-readRDS("./01_data/03_large_files_LFS/02_processed_files/HH_daily_presence__habitat_singleping_Feb11.rds")

distance ="350"

## load up daily receiver presence over study duration
max(hab_recs$date)

colnames(hab_recs)

if(distance == '350'){
 hab_recs$perc_hard<-hab_recs$mean_prop_hard
hab_daily$perc_hard<-hab_daily$mean_prop_hard
}

unique(hab_recs$perc_hard)

unique(hab_daily$station)
### remove receiver 43 due to inaccurate substrate info and is a travel corridor, not habitat for fish.
hab_daily <- hab_daily %>% filter(!station=="HAM-043")

hab_recs <- hab_recs %>% filter(!station=="HAM-043")

### remove receivers 52, 54, 55, 56, 57 to remove overlap at piers and potential spatial autocorrelation issues.
hab_daily <- hab_daily %>% filter(!station %in% c("HAM-052","HAM-054","HAM-055","HAM-056","HAM-057"))
unique(hab_daily$station)

hab_recs <- hab_recs %>% filter(!station %in% c("HAM-052","HAM-054","HAM-055","HAM-056","HAM-057"))
unique(hab_recs$station)

#hmmm<-detections %>% filter(station == "HAM-058")
####rcvr 58 had absurd SAV values. 
## change SAV values to zero

############################################
rec_hab_summary<-hab_recs %>% group_by(station, WL_AVG_mean,Weighted.Fetch.m.Mean, perc_hard, secchi, SAV) %>% summarise (n=n_distinct(deploy_lat))

#write.csv(rec_hab_summary, "hh_rcvr_350los_habitat.csv")


### run without dist to wetlands
WL<-"NoWL"

#for(i in 2:length(spp)) {

#fish <- detections %>% filter(common_name_e == spp[i])
#fish is the detections file since we are just running for rudd 
#detections file called hab_daily


###min of 4 fish detected per day (changed from 5 in SL)
fish_date <-hab_daily %>% group_by(date) %>% summarise(n_fish =n_distinct(transmitter_id))
fish_date<-fish_date %>% filter(n_fish>3)
fourfishdates<-unique(fish_date$date)

colnames(hab_daily)

fish_all <- hab_daily %>% 
 group_by(transmitter_id, date, station, WL_AVG_mean, SAV, 
          secchi, Weighted.Fetch.m.Mean, Cluster_group2, Detailed_group, 
          RM_DistFix, Emerg_Pres, WL_DistFix, ClosestWL, perc_hard) %>% 
 summarise(n_fish = 1, .groups = "drop")  # Each row = 1 fish detection


fish_all$presence<-1
##filter to dates with 4 fish only
fish_all<-fish_all %>% filter(date %in% c(fourfishdates))

days <- as.Date(unique(fish_all$date))
rcvr <- hab_recs %>% 
 mutate(date = as.Date(date)) %>% 
 filter(date %in% days)

# Now filter
rcvr <- hab_recs %>% filter(date %in% days)
rcvr <- hab_recs %>% 
 mutate(date_only = as.Date(date)) %>% 
 filter(date_only %in% days)


## Get unique transmitter IDs
transmitter_ids <- unique(fish_all$transmitter_id)

## Create complete grid of all receiver-date-transmitter combinations
fish_all_rcvrs1 <- rcvr %>%
 crossing(transmitter_id = transmitter_ids) %>%
 left_join(
  fish_all %>% dplyr::select(transmitter_id, station, date, presence, n_fish), 
  by = c("transmitter_id", "station", "date")
 ) %>%
 mutate(
  n_fish = replace_na(n_fish, 0),
  presence = replace_na(presence, 0)
 )

###rename columns
fish_all_rcvrs1<-fish_all_rcvrs1 %>% 
  rename(
    WL = WL_AVG_mean,
   # WL_SD = WL_AVG_SD,
   # SAV = SAV_mean,
   # slope = SLOPE_mean,
    fetch = Weighted.Fetch.m.Mean,
    dist_rm = RM_DistFix,
    dist_WL = WL_DistFix,
    dist_WLRH = ClosestWL,
    hard2 = mean_prop_hard
  )


saveRDS(fish_all_rcvrs1, "./01_data/02_processed_files/all_rcvrs.rds")

### check that daily number of receivers is appropriate - yep
rcvr_daily_sum2<-fish_all_rcvrs1 %>% group_by(date) %>% summarise(n_stations=n_distinct(station))

###fill in absences
fish_all_rcvrs1$presence[is.na(fish_all_rcvrs1$presence)] <- 0
fish_all_rcvrs1$n_fish[is.na(fish_all_rcvrs1$n_fish)] <- 0

unique(fish_all_rcvrs1$presence)

#### should be good for brownscombe analyses



#the below is for the creation of the balanace dataframe which is not the final df used in the RF model 
### look into determining number of rcvrs detected on per day and randomly choosing same number of rcvrs that day that had absence to get balanced design
# fish_all_sum <- fish_all %>% group_by(date) %>% summarise(n_stations = n_distinct(station))
# 
# days <- unique(fish_all_sum$date)
# 
# #all_rcvrs<-fish_all_rcvrs %>% filter(presence==0)
# 
# 
# balanced_rcvrs <- data.frame()
# 
# ####loop to select date and # of receivers fish spp detected on and to pick that # of rcvrs with 0 presence.
# #loop to go through all individuals
# for (t in 1:length(days)) {
#  ##number of receivers with detections on specified day
#  n_station <- fish_all_sum[fish_all_sum$date == days[t], "n_stations"]
#  n_station <- unique(n_station$n_stations)
#  
#  ## get absence receivers for specified day
#  temp <- subset(all_rcvrs, all_rcvrs$date == paste0(days[t]))
#  
#  n_temp <- nrow(temp)
#  
#  set.seed(123)
#  ##in case number of absence receivers is less than presence receivers, select all absence receviers.
#  n_pick <- ifelse(n_temp > n_station, n_station, n_temp)
#  ## select at random the number of presence receivers from absence receivers
#  balanced <- temp[sample(nrow(temp), n_pick), ]
#  
#  
#  ###add to dataframe
#  balanced_rcvrs <- bind_rows(balanced_rcvrs, balanced)
# }
# 
# 
# #### combine absences with presences
# all_fish <- fish_all_rcvrs %>% filter(presence == 1)
# 
# balanced_fish_rcvrs <- bind_rows(all_fish, balanced_rcvrs)
# 
saveRDS(balanced_fish_rcvrs, "./01_data/02_processed_files/balanced_rcvrs.rds")

#####################################################################
#####################################################################

### random forest modelling - Brownscombe method vs balanced
#proportions of detections?
#the fish all dataframe is the modelSL ended up using for the analysis and paper 


length(which(fish_all_rcvrs1$presence==1))/length(which(fish_all_rcvrs1$presence==0))

### See the actual counts
table(fish_all_rcvrs1$presence)


#lets go with the balanced dataframe becaseu it retains absences of stations taht fish could be detected on 
#but not all possible combinations so all transmitter_ids plus all stations that could have absences
#just here is where fish were detected and here and the possible recevier locatiosn online that they were not detected on 
#ratio is 1:74 prsence/absence

length(which(balanced_fish_rcvrs$presence==1))/length(which(balanced_fish_rcvrs$presence==0))
#107% - much more balanced
### See the actual counts
table(balanced_fish_rcvrs$presence)



#### add season
#we are basing time periods of off general bins of life history strategy 
#overwintering habitat, pre/spawn/post, summer foraging, fall foraging
#spring: April 1 to June 15th 
#summer: june 16 to oct 3
#fall: october 4th to novemeber 15th 
#winter: November 16th to March 31st 


###get thermocline based season

library(lubridate)

# Define season date ranges (year-agnostic)
# Spring: April 1 to June 15
# Summer: June 16 to October 3
# Fall: October 4 to November 15
# Winter: November 16 to March 31

fish_all_rcvrs1 <- fish_all_rcvrs1 %>%
 mutate(
  month = month(date),
  day = day(date),
  # Create a sortable month-day value for comparison
  month_day = month * 100 + day,
  
  thermo_season = case_when(
   # Spring: April 1 (401) to June 15 (615)
   month_day >= 401 & month_day <= 615 ~ "Spring",
   
   # Summer: June 16 (616) to October 3 (1003)
   month_day >= 616 & month_day <= 1003 ~ "Summer",
   
   # Fall: October 4 (1004) to November 15 (1115)
   month_day >= 1004 & month_day <= 1115 ~ "Fall",
   
   # Winter: November 16 (1116) to March 31 (331)
   # Winter wraps around the year, so it's Nov 16 - Dec 31 OR Jan 1 - Mar 31
   month_day >= 1116 | month_day <= 331 ~ "Winter",
   
   TRUE ~ NA_character_
  )
 ) %>%
 dplyr::select(-month, -day, -month_day)  # Remove helper columns

# Verify the seasonality assignments
fish_all_rcvrs1 %>%
 group_by(thermo_season) %>%
 summarise(
  min_date = min(date),
  max_date = max(date),
  n_records = n()
 )

# Check a few specific dates to make sure it's working
fish_all_rcvrs1 %>%
 filter(date %in% as.Date(c("2022-04-01", "2022-06-15", "2022-06-16", 
                            "2022-10-03", "2022-10-04", "2022-11-15", 
                            "2022-11-16", "2023-03-31"))) %>%
 distinct(date, thermo_season) %>%
 arrange(date)

sum(is.na(fish_all_rcvrs1$thermo_season))

# 
# ###get thermocline based season

balanced_fish_rcvrs <- balanced_fish_rcvrs %>%
 mutate(
  month = month(date),
  day = day(date),
  # Create a sortable month-day value for comparison
  month_day = month * 100 + day,
  
  thermo_season = case_when(
   # Spring: April 1 (401) to June 15 (615)
   month_day >= 401 & month_day <= 615 ~ "Spring",
   
   # Summer: June 16 (616) to October 3 (1003)
   month_day >= 616 & month_day <= 1003 ~ "Summer",
   
   # Fall: October 4 (1004) to November 15 (1115)
   month_day >= 1004 & month_day <= 1115 ~ "Fall",
   
   # Winter: November 16 (1116) to March 31 (331)
   # Winter wraps around the year, so it's Nov 16 - Dec 31 OR Jan 1 - Mar 31
   month_day >= 1116 | month_day <= 331 ~ "Winter",
   
   TRUE ~ NA_character_
  )
 ) %>%
 dplyr::select(-month, -day, -month_day)  # Remove helper columns

# Verify the seasonality assignments
balanced_fish_rcvrs %>%
 group_by(thermo_season) %>%
 summarise(
  min_date = min(date),
  max_date = max(date),
  n_records = n()
 )

# Check a few specific dates to make sure it's working
balanced_fish_rcvrs %>%
 filter(date %in% as.Date(c("2022-04-01", "2022-06-15", "2022-06-16", 
                            "2022-10-03", "2022-10-04", "2022-11-15", 
                            "2022-11-16", "2023-03-31"))) %>%
 distinct(date, thermo_season) %>%
 arrange(date)

sum(is.na(balanced_fish_rcvrs$thermo_season))



fish_all_rcvrs1$thermo_season<-factor(fish_all_rcvrs1$thermo_season, levels = c("Spring", "Summer","Fall", "Winter"))
fish_all_rcvrs1$presence<-as.factor(fish_all_rcvrs1$presence)
fish_all_rcvrs1$Cluster_group2<-as.factor(fish_all_rcvrs1$Cluster_group2)
fish_all_rcvrs1$Emerg_Pres<-as.factor(fish_all_rcvrs1$Emerg_Pres)
fish_all_rcvrs1$year<-as.factor(format(fish_all_rcvrs1$date,"%Y"))

#season<-fish_all_rcvrs %>% group_by(date,thermo_season,year) %>% dplyr::summarise(n=n())
#fish2<-left_join(fish,season, by=c("date"))
#hmm<-fish2 %>% group_by(thermo_season, year) %>% dplyr::summarise(ind=n_distinct(transmitter_id),det=sum(n_detections),station=n_distinct(station))
#huh<- fish_all_rcvrs %>% group_by(thermo_season, year,date, presence) %>% dplyr::summarise(stations=n_distinct(station))
#herm<- fish_all_rcvrs %>%group_by(thermo_season, year,date) %>% dplyr::summarise(total=n_distinct(station))
#huh<-left_join(huh,herm, by=c("thermo_season", "year", "date"))
#huh$prop<-huh$stations/huh$total
#overall<- huh %>% group_by(thermo_season, year, presence)%>% dplyr::summarise(meanprop=mean(prop))

balanced_fish_rcvrs$thermo_season<-factor(balanced_fish_rcvrs$thermo_season, levels = c("Spring", "Summer","Fall", "Winter"))
balanced_fish_rcvrs$presence<-as.factor(balanced_fish_rcvrs$presence)
balanced_fish_rcvrs$Cluster_group2<-as.factor(balanced_fish_rcvrs$Cluster_group2)
balanced_fish_rcvrs$Emerg_Pres<-as.factor(balanced_fish_rcvrs$Emerg_Pres)
balanced_fish_rcvrs$year<-as.factor(format(balanced_fish_rcvrs$date,"%Y"))

##############################################################################

### add in diff hab variables for winter/summer. 


###### are any variables highly correlated?
##select numerical variables 
sum(is.na(fish_all_rcvrs1$WL))
hmm<-fish_all_rcvrs %>% filter(is.na(WL))

library(ggplot2)
library(reshape2)

# Calculate correlations
res <- cor(fish_all_rcvrs1[, c("WL", "SAV", "secchi", "fetch", 
                               "dist_rm", "dist_WL", "dist_WLRH", "hard2")],
           use = "complete.obs")

# Melt the correlation matrix for ggplot
melted_res <- reshape2::melt(res)

# Create heatmap
ggplot(melted_res, aes(Var1, Var2, fill = value)) +
 geom_tile(color = "white") +
 geom_text(aes(label = round(value, 2)), size = 3.5) +
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                      midpoint = 0, limit = c(-1, 1),
                      name = "Correlation") +
 theme_minimal() +
 theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
       axis.text.y = element_text(size = 10),
       axis.title = element_blank(),
       panel.grid = element_blank()) +
 coord_fixed() +
 labs(title = "Correlation Matrix of Environmental Variables")


###all less than 0.8
###fetch and WL correlated
### sand and SAv correlated
### slope and dist_WL correlated
### fetch and dist_WLRH correlated 
### dist_WL and dist_WLRH correlated but only choosing one of these anyways. 

saveRDS(fish_all_rcvrs1, "./01_data/02_processed_files/all_rcvrs.rds")
saveRDS(balanced_fish_rcvrs, "./01_data/02_processed_files/balanced_rcvrs.rds")
gc()

#split data into train and test 
fish_all_rcvrs1$nrow <- 1:length(fish_all_rcvrs1$date)
set.seed(123)
train <- fish_all_rcvrs1 %>% sample_frac(0.7) # 70/30 split.
max(train$date)
test <- fish_all_rcvrs1[!(fish_all_rcvrs1$nrow %in% train$nrow),]
max(test$date)

balanced_fish_rcvrs$nrow <- 1:length(balanced_fish_rcvrs$date)
set.seed(123)
train2 <- balanced_fish_rcvrs %>% sample_frac(0.7) # 70/30 split. 
test2 <- balanced_fish_rcvrs[!(balanced_fish_rcvrs$nrow %in% train2$nrow),]


#model formula
z <- formula(presence~thermo_season+year+SAV+WL+fetch+hard+dist_WLRH+Emerg_Pres)##Cluster_group2
z2<- formula(presence~thermo_season+SAV+WL+fetch+hard2+dist_WLRH+secchi)

if (WL == "NoWL") {
 # z2<- formula(presence~thermo_season+year+SAV+WL+fetch+hard2+secchi)
  z2<- formula(presence~thermo_season+SAV+WL+fetch+hard2+secchi)
  
}

### what about adding year. cuz variable receiver coverage?
### dist_WLRH seems to take over everything... is that accurate?

#Optimize classwt
#RF1_simulations <- function_SimulateAccuracy(predictor = "presence", model.formula=z,
#                                             data_Train = train, data_Test = test,
#                                             loopweight = c(100,10,2,1.5,1,0.95,0.8,0.1,0.01,0.001))

###LMB optimal level was 0.8.

###use 0.95 for everything for now.
#balanced

Forest <- randomForest(formula=z2, data = train2, replace=FALSE, na.action=na.omit,
                           importance=TRUE, classwt=c(1,0.95), do.trace=1000, ntree=1000)

#12.13% on out of bag error (OOB)
#class 1 error is 11.13% (error of absesnces)
#class 2 error is 12.72% (error of presneces)
#~88% accuracy good
print(Forest)
importance(Forest)
varImpPlot(Forest, main = "Variable Importance")


Forest_testpred <- predict(Forest, test2, type="response")
confusionMatrix(factor(Forest_testpred), 
                factor(test2$presence),
                positive = "1")

library(MLmetrics)

Accuracy(Forest_testpred, test2$presence)
#87%
Precision(Forest_testpred, test2$presence, positive = "1")
#87%
Recall(Forest_testpred, test2$presence, positive = "1")
#92%
F1_Score(Forest_testpred, test2$presence, positive = "1")
#89%

library(pROC)

# Get probability predictions
Forest_testprob <- predict(Forest, test2, type = "prob")[, 2]  # Prob of presence

# Create ROC curve
roc_obj <- roc(test2$presence, Forest_testprob)
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc(roc_obj), 3), ")"))

# AUC value
auc(roc_obj)
#AUC = 0.923 really good!




#unbalanced
#ForestUB <- randomForest(formula=z, data = train2, replace=FALSE, na.action=na.omit,
#                             importance=TRUE, do.trace=1000, ntree=1000)
#print(ForestUB)
#ForestUB_testpred <- predict(ForestUB, test2, type="response")
#confusionMatrix(ForestUB_testpred, test2$pres,  positive="1")


#this does the samething as above pick one for final code output and stick with it 
RFC_J_diagnostic <-data.frame(matrix(nrow = 6, ncol = 1))

#Build confusion matrix
test2$prediction1 <- predict(Forest, test2)

RFC_J_diagnostic$Summary2 <- 
  caret::confusionMatrix(
    data=test2$prediction1, 
    reference=test2$presence,  
    positive="1") 

#Display confusion matrix
print(RFC_J_diagnostic$Summary2)

#Print and copy values of interest
data.frame(Accuracy = paste0(round(RFC_J_diagnostic$Summary2[["overall"]][[1]], digits=2),
                             " (",
                             round(RFC_J_diagnostic$Summary2[["overall"]][[3]], digits=2),
                             ", ",
                             round(RFC_J_diagnostic$Summary2[["overall"]][[4]], digits=2),
                             ")"),
           `No Information Rate` = round(RFC_J_diagnostic$Summary2[["overall"]][[5]], digits=2),
           Sensitivity = round(RFC_J_diagnostic$Summary2[["byClass"]][[1]], digits=2),
           Specificity = round(RFC_J_diagnostic$Summary2[["byClass"]][[2]], digits=2),
           `Pos Pred Value` = round(RFC_J_diagnostic$Summary2[["byClass"]][[3]], digits=2),
           `Neg Pred Value` = round(RFC_J_diagnostic$Summary2[["byClass"]][[4]], digits=2),
           `Balanced Accuracy` = round(RFC_J_diagnostic$Summary2[["byClass"]][[11]], digits=2),
           `P value` = round(RFC_J_diagnostic$Summary2[["overall"]][[6]], digits=2)
) %>%
  print() %>% write.table(., file=paste0("./Results/New/Revise/",spp[i],"/",spp[i],"clipboard"), sep="\t", row.names=FALSE, col.names=T)




#####Variable importance##########################################----
#-------------------------------------------------------------#

#variable importance
varIMP <- data.frame(importance(Forest))
varIMP$predictor <- rownames(varIMP)
head(varIMP)

p<-ggplot(varIMP, aes(MeanDecreaseAccuracy, reorder(predictor,MeanDecreaseAccuracy)))+
  geom_histogram(stat="identity")+theme_bw()+ylab("Predictor")+ 
  theme(text = element_text(size=20))+
  scale_y_discrete(labels=c("thermo_season" = "Season", "slope" = "Slope","SAV"="% Submerged Veg", "fetch" = "Fetch", "secchi" = "Secchi Depth", "year" = "Year",
                            "WL"="Water Depth", "dist_WLRH"= "Distance to Wetland", "dist_rm"= "Distance to River Mouth",
                            "WL_SD"="Water Depth SD", "Emerg_Pres"="Emergent Veg Presence", "hard"="% Gravel/Cobble/Boulder1", "hard2"="% Gravel/Cobble/Boulder2"
                                                                                          ))
p
ggsave(plot=p, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_variableimportance_new2_", WL,".png"),  width = 15, height = 10,units = "cm", dpi = 400)



model_vars_balanced <- balanced_fish_rcvrs %>%
dplyr:: select(thermo_season, SAV, WL, fetch, hard2, dist_WLRH, secchi, presence)

# variable partial dependencies. Just SAV as eg. Everything past here will be computationally intensive ****
partialseason<-pdp::partial(Forest, pred.var = "thermo_season", prob = TRUE, which.class='1', train=model_vars_balanced)
partialSAV <- pdp::partial(Forest, pred.var = "SAV", prob = TRUE, which.class='1', train=balanced_fish_rcvrs)
partialWL <- pdp::partial(Forest, pred.var = "WL", prob = TRUE, which.class='1', train=balanced_fish_rcvrs)
partialWLRH <- pdp::partial(Forest,pred.var = "dist_WLRH", prob = TRUE, which.class='1', train=balanced_fish_rcvrs)
partialfetch<-pdp::partial(Forest, pred.var = "fetch", prob = TRUE, which.class='1', train=balanced_fish_rcvrs)
partialhard<-pdp::partial(Forest, pred.var = "hard2", prob = TRUE, which.class='1', train=balanced_fish_rcvrs)
partialsecchi<-pdp::partial(Forest, pred.var = "secchi", prob = TRUE, which.class='1', train=balanced_fish_rcvrs)
partialyear<- pdp::partial(Forest, pred.var = "year", prob = TRUE, which.class='1', train=balanced_fish_rcvrs)


#ggplot(partialslope, aes(slope, yhat))+
#  geom_point()+
#  geom_smooth()+
#  ylab(bquote("Marginal effect" ~(hat(y))))+
#  theme_bw()
p0<- ggplot(partialyear, aes(year, yhat))+
  geom_point()+
  geom_smooth()+
  theme(text = element_text(size=18))+
  xlab("Year")+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()
p0
ggsave(plot=p0, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_effect_year1_", WL,".png"),  width = 12, height = 10,units = "cm", dpi = 400)

p1<- ggplot(partialseason, aes(thermo_season, yhat))+
  geom_point()+
  geom_smooth()+
  theme(text = element_text(size=18))+
  xlab("Season")+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()
p1
ggsave(plot=p1, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_effect_season1_", WL,".png"),  width = 15, height = 10,units = "cm", dpi = 400)

#ggplot(partialsand, aes(sand, yhat))+
#  geom_point()+
#  geom_smooth()+
#  ylab(bquote("Marginal effect" ~(hat(y))))+
#  theme_bw()

p2<- ggplot(partialhard, aes(hard2, yhat))+
  geom_point()+
  geom_smooth()+
  theme(text = element_text(size=18))+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  xlab("% Gravel/Cobble/Boulder")+
  theme_bw()

p2
ggsave(plot=p2, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_effect_hard1_", WL,".png"),  width = 8, height = 6,units = "cm", dpi = 400)


p3<-ggplot(partialSAV, aes(SAV, yhat))+
  geom_point()+
  geom_smooth()+
  theme(text = element_text(size=18))+
  xlab("% Submerged Veg")+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()
p3
ggsave(plot=p3, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_effect_SAV1_", WL,".png"),  width = 8, height = 6,units = "cm", dpi = 400)


p4<-ggplot(partialWL, aes(WL, yhat))+
  geom_point()+
  geom_smooth()+
  theme(text = element_text(size=18))+
  xlab("Water Depth (m)")+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()
p4
ggsave(plot=p4, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_effect_depth1_", WL,".png"),  width = 8, height = 6,units = "cm", dpi = 400)


p5<-ggplot(partialfetch, aes(fetch, yhat))+
  geom_point()+
  geom_smooth()+
  theme(text = element_text(size=18))+
  xlab("Fetch (km)")+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()
p5
ggsave(plot=p5, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_effect_fetch1_", WL,".png"),  width = 8, height = 6,units = "cm", dpi = 400)



p6<-ggplot(partialdist_WLRH, aes(dist_WLRH, yhat))+
 geom_point()+
 geom_smooth()+
 theme(text = element_text(size=18))+
 xlab("Distance to Wetland (m)")+
 ylab(bquote("Marginal effect" ~(hat(y))))+
 theme_bw()
p6



#ggplot(partialdist_rm, aes(dist_rm, yhat))+
#  geom_point()+
#  geom_smooth()+
#  ylab(bquote("Marginal effect" ~(hat(y))))+
#  theme_bw()

p7<-ggplot(partialsecchi, aes(secchi, yhat))+
  geom_point()+
  geom_smooth()+
  theme(text = element_text(size=18))+
  xlab("Secchi (m)")+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()

p7
ggsave(plot=p7, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_effect_secchi_", WL,".png"),  width = 8, height = 6,units = "cm", dpi = 400)

#########################################################################
#library("iml")
#library("randomForest")
##data("Boston", package = "MASS")
#library(iml, include.only = c('Predictor', 'Interaction')) 
##plot_interactions <- list() #Make list to hold plots

#####Combined Life Stage##########################################----
#-------------------------------------------------------------#

#####Get rid of package:'iml' and its troublesum dependencies#####----
#-------------------------------------------------------------#


###need to fit dataset with only variables of interest
#data2 <-train %>% dplyr::select(thermo_season,year,SAV,WL,fetch,hard2,secchi)
#----------------------------#
###Interpret model variables with 'iml'
#----------------------------#
#invisible({
#  #Formally library all background loaded packages
#  suppressPackageStartupMessages(
#    lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE))
#  #Detach all non-base packages
#  lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE, force=TRUE)
#})

#Create a model object
#RF2_int_predictor <- iml::Predictor$new(Forest, data = data2, y=data2$presence, type='prob')

#Measure the interaction strength
#RF2_int_Interaction <- iml::Interaction$new(RF2_int_predictor, feature="thermo_season") #OPTIONAL: Specify interactions: feature="FishDiversity"

#Plot H value of variables or interaction terms 
#data3<- RF2_int_Interaction$results %>%  
#  dplyr::group_by(.feature) %>% 
#  dplyr::summarise(H=mean(.interaction)) %>% 
#  as.data.frame()  
  

#unique(data3$.feature)
#data3$.feature[data3$.feature=="fetch:thermo_season"] <- "fetch:season"
#data3$.feature[data3$.feature=="SAV:thermo_season"] <- "SAV:season"
#data3$.feature[data3$.feature=="hard2:thermo_season"] <- "hard substrate:season"
#data3$.feature[data3$.feature=="WL:thermo_season"] <- "depth:season"
#data3$.feature[data3$.feature=="secchi:thermo_season"] <- "Secchi:season"
##data3$.feature[data3$.feature=="year:thermo_season"] <- "year:season"

#data3<-data3 %>% filter(!.feature=="year:thermo_season")

  
#int<-  ggplot(data=data3, aes(x=H, y= reorder(.feature, H)))+
#  geom_histogram(stat="identity", col="black", fill="brown", alpha=0.4)+
#  ylab("Interaction")+xlab("H value")+
#   theme(text = element_text(size=18))
#int

#  ggsave(plot=int, paste0("./Results/New/",spp[i],"/",spp[i],"_season_interaction", WL,".png"),  width = 15, height = 10,units = "cm", dpi = 400)
  



#####Bivariate plots##############################################----
#-------------------------------------------------------------#
###ignore for now ####
#interaction dependencies. Season x dist wl

if (WL == "WL") {
partialseasonWLRH <- pdp::partial(Forest, pred.var = c("thermo_season","dist_WLRH"), prob = TRUE, which.class='1', train=balanced_fish_rcvrs)


pp1<- ggplot(partialseasonWLRH, aes(dist_WLRH, yhat, col=thermo_season))+geom_point()+geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+theme_bw()+
  ylim(0.1,0.8)+
  xlab("Distance to Wetland (m)")+ labs(col="Season")+ theme(text = element_text(size=18))

pp1

ggsave(plot=pp1, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_effect_season_distWL1.png"),  width = 15, height = 10,units = "cm", dpi = 400)

}

#interaction dependencies. Season x depth
partialseasonWL <- pdp::partial(Forest, pred.var = c("thermo_season","WL"), prob = TRUE, which.class='1', train=balanced_fish_rcvrs)


pp2<-ggplot(partialseasonWL, aes(WL, yhat, col=thermo_season))+geom_point()+geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+theme_bw()+
  ylim(0.1,0.8)+
  xlab("Water Depth (m)")+ labs(col="Season")+ theme(text = element_text(size=18))
pp2

ggsave(plot=pp2, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_effect_season_WL1_", WL,".png"),  width = 15, height = 10,units = "cm", dpi = 400)

##ggplot(partialseasonWL, aes(x=thermo_season, y=WL, fill=yhat))+
 # geom_tile()+
#  theme_bw()+
#  scale_fill_viridis_c()

#interaction dependencies. season X fetch
partialseasonfetch <- pdp::partial(Forest, pred.var = c("thermo_season","fetch"), prob = TRUE, which.class='1', train=balanced_fish_rcvrs)

pp3<- ggplot(partialseasonfetch, aes(fetch, yhat, col=thermo_season))+geom_point()+geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+theme_bw()+
  ylim(0.1,0.8)+
  xlab("Fetch (m)")+ labs(col="Season")+ theme(text = element_text(size=18))
pp3

ggsave(plot=pp3, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_effect_season_fetch1_", WL,".png"),  width = 15, height = 10,units = "cm", dpi = 400)


#ggplot(partialseasonfetch, aes(x=thermo_season, y=fetch, fill=yhat))+
#  geom_tile()+
#  theme_bw()+
#  scale_fill_viridis_c()

#interaction dependencies. season X SAV
partialseasonSAV <- pdp::partial(Forest, pred.var = c("thermo_season","SAV"), prob = TRUE, which.class='1', train=balanced_fish_rcvrs)

pp4<- ggplot(partialseasonSAV, aes(SAV, yhat, col=thermo_season))+geom_point()+geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+theme_bw()+
  ylim(0.1,0.8)+
  xlab("% Submerged Veg")+ labs(col="Season")+ theme(text = element_text(size=18))
pp4

ggsave(plot=pp4, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_effect_season_SAV1_", WL,".png"),  width = 15, height = 10,units = "cm", dpi = 400)


#ggplot(partialseasonfetch, aes(x=thermo_season, y=SAV, fill=yhat))+
#  geom_tile()+
#  theme_bw()+
#  scale_fill_viridis_c()

partialseasonhard <- pdp::partial(Forest, pred.var = c("thermo_season","hard2"), prob = TRUE, which.class='1', train=balanced_fish_rcvrs)

pp5<- ggplot(partialseasonhard, aes(hard2, yhat, col=thermo_season))+geom_point()+geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+theme_bw()+
  ylim(0.1,0.8)+
  xlab("% Gravel/Boulder/Cobble")+ labs(col="Season")+ theme(text = element_text(size=18))
pp5

ggsave(plot=pp5, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_effect_season_hard1_", WL,".png"),  width = 15, height = 10,units = "cm", dpi = 400)


partialseasonsecchi <- pdp::partial(Forest, pred.var = c("thermo_season","secchi"), prob = TRUE, which.class='1', train=balanced_fish_rcvrs)

pp6<- ggplot(partialseasonsecchi, aes(secchi, yhat, col=thermo_season))+geom_point()+geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+theme_bw()+
  ylim(0.1,0.8)+
  xlab("Secchi Depth (m)")+ labs(col="Season")+ theme(text = element_text(size=18))
pp6

ggsave(plot=pp6, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_effect_season_sechhi_", WL,".png"),  width = 15, height = 10,units = "cm", dpi = 400)


#the above do not seem to be correct will need to validate and revise - NT

############################################

############################################
### #interaction dependencies 3d. dist_WLRH x depth x SAV

### not working rn.
#partialWLRH_WL_SAV <- Forest %>% partial(pred.var = c("WL","dist_WLRH", "SAV"), prob = TRUE, which.class='1', train=fish_all_rcvrs)
##ggplot(partialseasonWLRH, aes(dist_WLRH, yhat, col=thermo_season))+geom_point()+geom_smooth()+
##  ylab(bquote("Marginal effect" ~(hat(y))))+theme_bw()

##ggplot(partialseasonWLRH, aes(x=thermo_season, y=dist_WLRH, fill=yhat))+
##  geom_tile()+
##  theme_bw()+
##  scale_fill_viridis_c()

#plot_ly(partialWLRH_WL_SAV, x=WL, y=dist_WLRH, z=SAV, type="scatter3d", mode="markers", color=yhat)
###########################################################

###########################################
#map of importance

####load hamilton harbour shapefile
shorelinemap<- st_read("./01_data/04_shapefiles/HH_Fineshoreline", "HH_WaterLines_02June2023")

###need to convert to lat long from UTMS or points
map_NAD83 <- st_transform(shorelinemap, CRS("+proj=longlat +datum=NAD83"))

plot(map_NAD83, col="grey")

#####################################################
###skipped
#spatial preds:
#seasons <- data.frame(thermo_season=unique(fish_all_rcvrs$thermo_season))
#receivers$year<-as.factor(format(receivers$date,"%Y"))


#rcvrinfo <- receivers %>% group_by(station,year) %>%  
#  summarise(hard2=mean(perc_hard), dist_WLRH=mean(ClosestWL), SAV=mean(SAV_mean), fetch=mean(Weighted.Fetch.m.Mean), WL=mean(WL_AVG_mean), secchi=mean(secchi),
#            lat=mean(deploy_lat),long=mean(deploy_long))  
##rcvrinfo$Emerg_Pres<-as.factor(rcvrinfo$Emerg_Pres)

#rcvrseason <- merge(seasons, rcvrinfo)
#rcvrseason <-rcvrseason %>% filter(as.numeric(year)<7)
#rcvrseason$year<-as.factor(as.character(rcvrseason$year))

#RFpred <- predict(Forest, rcvrseason, type="prob")

#rcvrseason$RFpred <- RFpred[,2]
#rcvrseason<-rcvrseason %>% group_by(station,thermo_season) %>% summarize(RFpred=mean(RFpred),long=mean(long), lat=mean(lat))

###without year
##rcvrinfo <- receivers %>% group_by(station,Emerg_Pres) %>%  
##  summarise(hard=mean(perc_hard), slope=mean(SLOPE_mean),sand=mean(per_sand),dist_WLRH=mean(ClosestWL),dist_rm=mean(RM_DistFix), SAV=mean(SAV_mean), fetch=mean(Weighted.Fetch.m.Mean), WL=mean(WL_AVG_mean), WL_SD=mean(WL_AVG_SD),
##            lat=mean(deploy_lat),long=mean(deploy_long))  
##rcvrinfo$Emerg_Pres<-as.factor(rcvrinfo$Emerg_Pres)

##rcvrseason <- merge(seasons, rcvrinfo)

##RFpred <- predict(Forest, rcvrseason, type="prob")

##rcvrseason$RFpred <- RFpred[,2]


#plot<-ggplot()+
#  geom_polygon(data=map_NAD83,aes(x=long, y = lat, group=group),col="darkgrey", fill="white")+ 
#  coord_fixed(1.3)+
#  geom_point(data=rcvrseason, aes(x=long,y=lat, fill=RFpred, size=RFpred), col="black", shape=21)+
#  #geom_text(data=group_summary, aes(x=long,y=lat-0.0025,label=location),size = 4)+
#  labs(x="Longitude", y="Latitude", size= "Presence Probability", fill= "Presence Probability")+facet_wrap(~thermo_season)+
#  theme(text = element_text(size = 18)) +
#  scale_fill_viridis()
##theme(panel.grid.major = element_blank()) 
##       panel.grid.minor = element_blank(),panel.border = element_blank())

#plot

#ggsave(plot=plot, paste0("./Results/",spp[i],"/",spp[i],"_Comboseason_model_prediction_station_map1.png"),  width = 30, height = 20,units = "cm", dpi = 400)


#########################################################################


###########################################################################
### grid habitat availability predictions - with no buffered habitat points. 

#model formula
#z <- formula(presence~year+thermo_season+SAV+WL+fetch+hard2+dist_WLRH+secchi)##Cluster_group2
z <- formula(presence~thermo_season+SAV+WL+fetch+hard2+dist_WLRH+secchi)##Cluster_group2

if (WL == "NoWL") {
 # z<- formula(presence~thermo_season+year+SAV+WL+fetch+hard2+secchi)
  z<- formula(presence~thermo_season+SAV+WL+fetch+hard2+secchi)
  
  
}

#Optimize classwt
#RF1_simulations <- function_SimulateAccuracy(predictor = "presence", model.formula=z,
#                                             data_Train = train, data_Test = test,
#                                             loopweight = c(100,10,2,1.5,1,0.95,0.8,0.1,0.01,0.001))





#balanced
Forest <- randomForest(formula=z, data = train, replace=FALSE, na.action=na.omit,
                       importance=TRUE, classwt=c(1,0.95), do.trace=1000, ntree=1000)

Forest_testpred <- predict(Forest, test, type="response")
confusionMatrix(Forest_testpred, test$pres, positive="1")


#spatial preds:
seasons <- data.frame(thermo_season=unique(balanced_fish_rcvrs$thermo_season))
years <- data.frame(year=unique(balanced_fish_rcvrs$year))

grid <-read.csv("./Data/HH_50mGrid_Depth_SAV_Fetch_WetlandDist_06June2023_final.csv")
hard <-read.csv("./Data/HH_Grid50_25mBuffer_PropHardSubstrate_07June2023.csv")

sav<-read.csv("./Data/Habitat/hh_50mgrid_75SAV_update.csv")
grid <-grid %>% dplyr::select(!(SAV))
grid<-left_join(grid,sav, by="ID")

grid<-left_join(grid,hard, by="ID")

#####################################




gridseason <- merge(seasons, grid)
gridseason <- merge(years, gridseason)
gridseason$hard2<-gridseason$hard
RFpred <- predict(Forest, gridseason, type="prob")

gridseason$RFpred <- RFpred[,2]
###remove NAs
gridseason<-gridseason %>% filter(!is.na(RFpred))
###order ascending
gridseason<-gridseason %>% arrange(RFpred)

###predict mean values across years for each season
gridseason2<-gridseason %>% group_by(thermo_season,ID,UTM.X,UTM.Y, Latitude, Longitude) %>% dplyr::summarize(RFpred=mean(RFpred))

plot<-ggplot()+
  geom_polygon(data=map_NAD83,aes(x=long, y = lat, group=group),col="darkgrey", fill="white")+ 
  coord_fixed(1.3)+
  geom_point(data=gridseason2, aes(x=Longitude,y=Latitude, col=RFpred),  shape=16, size=0.9)+
  #geom_text(data=group_summary, aes(x=long,y=lat-0.0025,label=location),size = 4)+
  labs(x="Longitude", y="Latitude", col= "Presence Probability")+facet_wrap(~thermo_season)+
  theme(text = element_text(size = 18)) +
  scale_colour_viridis()
#theme(panel.grid.major = element_blank()) 
#       panel.grid.minor = element_blank(),panel.border = element_blank())

plot

#ggsave(plot=plot, paste0("./Results/New/",spp[i],"/",spp[i],"_Comboseason_model_prediction_grid_map_new_", WL,".png"),  width = 30, height = 20,units = "cm", dpi = 400)

### plot raster with harbour outline
p<- ggplot()+
  geom_polygon(data=map_NAD83,aes(x=long, y = lat, group=group),col="black", fill=NA)+
  geom_tile(data=gridseason2, aes(x=Longitude, y=Latitude, fill=RFpred),width=0.00075,height=0.00075) +
  #  geom_polygon(data=shorelinemap,aes(x=long, y = lat, group=group),col="black", fill=NA)+
  #geom_tile(data=gridseason2, aes(x=UTM.X, y=UTM.Y, fill=RFpred))+
  #coord_equal()+
  coord_fixed(1.3)+
  labs(x="Longitude", y="Latitude", fill= "Presence Probability", title= spp[i])+
  
  scale_fill_viridis_c() + facet_wrap(~thermo_season)+
  theme(text = element_text(size = 18)) 

p

#ggsave(plot=p, paste0("./Results/New/",spp[i],"/",spp[i],"_Comboseason_model_prediction_raster_map_new_title_", WL,".png"),  width = 30, height = 20,units = "cm", dpi = 400)


p<- ggplot()+
  geom_polygon(data=map_NAD83,aes(x=long, y = lat, group=group),col="black", fill=NA)+
  geom_tile(data=gridseason2, aes(x=Longitude, y=Latitude, fill=RFpred),width=0.00075,height=0.00075) +
  #  geom_polygon(data=shorelinemap,aes(x=long, y = lat, group=group),col="black", fill=NA)+
  #geom_tile(data=gridseason2, aes(x=UTM.X, y=UTM.Y, fill=RFpred))+
  #coord_equal()+
  coord_fixed(1.3)+
  labs(x="Longitude", y="Latitude", fill= "Presence Probability")+
  
  scale_fill_viridis_c() + facet_wrap(~thermo_season)+
  theme(text = element_text(size = 18)) 

p

#ggsave(plot=p, paste0("./Results/New/",spp[i],"/",spp[i],"_Comboseason_model_prediction_raster_map_new_notitle_", WL,".png"),  width = 30, height = 20,units = "cm", dpi = 400)


area<-gridseason2 %>% group_by(thermo_season) %>% summarize(sum=sum(RFpred >=0.8)*2500/1000000) ###2500=area of cell in m and 1000000 is conversion from m2 to km2
#write.csv(area,paste0("./Results/New/",spp[i],"/",spp[i],"_area_80plus_", WL,".csv"))

###########################################################################
### grid habitat availability predictions
### habitat values are mean from 350 m buffer - similar to receiver values. 

#model formula
#z <- formula(presence~thermo_season+year+SAV+WL+fetch+hard2+dist_WLRH+secchi)##Cluster_group2
z <- formula(presence~thermo_season+SAV+WL+fetch+hard2+dist_WLRH+secchi)##Cluster_group2

if (WL == "NoWL") {
 # z<- formula(presence~thermo_season+year+SAV+WL+fetch+hard2+secchi)
  z<- formula(presence~thermo_season+SAV+WL+fetch+hard2+secchi)
  
  
}
#Optimize classwt
#RF1_simulations <- function_SimulateAccuracy(predictor = "presence", model.formula=z,
#                                             data_Train = train, data_Test = test,
#                                             loopweight = c(100,10,2,1.5,1,0.95,0.8,0.1,0.01,0.001))

###LMB optimal level was 0.8.

###use 0.95 for everything for now.
#balanced
Forest <- randomForest(formula=z, data = train, replace=FALSE, na.action=na.omit,
                       importance=TRUE, classwt=c(1,0.95), do.trace=1000, ntree=1000)

Forest_testpred <- predict(Forest, test, type="response")
confusionMatrix(Forest_testpred, test$pres, positive="1")


#spatial preds:
seasons <- data.frame(thermo_season=unique(fish_all_rcvrs$thermo_season))
years <-data.frame(year=unique(fish_all_rcvrs$year))

#### grid with mean buffered habitat values
grid <-read.csv("./Data/HH_50mGrid_Depth_SAV_Fetch_WetlandDist_06June2023_final_buff.csv")
hard <-read.csv("./Data/Habitat/hh_grid_hard_350buff.csv")
#sav<-read.csv("./Data/Habitat/hh_grid_SAV74_350buff.csv")
#sav<-read.csv("./Data/Habitat/hh_50mgrid_75SAVbuff_scrubbed.csv")
###updated in june 2024 to remove veg by indian creek.
sav<-read.csv("./Data/Habitat/hh_50mgrid_75SAVbuff_june2024.csv")
secchi<-read.csv("./Data/Habitat/hh_grid_secchi_350buff.csv")
Waterlevel<-read.csv("./Data/Habitat/hh_grid_WL_350buff.csv")


grid<-left_join(grid,hard, by="ID")
grid<-left_join(grid,sav, by="ID")
grid<-left_join(grid,secchi, by="ID")
grid<-left_join(grid,Waterlevel, by="ID")

gridseason <- merge(seasons, grid)
##include if year is in model
#gridseason<-merge(years,gridseason)
gridseason$hard2<-gridseason$hard

#gridseason$year<-2021
RFpred <- predict(Forest, gridseason, type="prob")

gridseason$RFpred <- RFpred[,2]
###remove NAs
gridseason<-gridseason %>% filter(!is.na(RFpred))
###order ascending
gridseason<-gridseason %>% arrange(RFpred)

#### take the mean value across years wtihin seasons
#gridseason2<-gridseason %>% dplyr::group_by(thermo_season,ID,UTM.X,UTM.Y, Latitude, Longitude) %>% dplyr::summarize(RFpred=mean(RFpred))

plot<-ggplot()+
  geom_polygon(data=map_NAD83,aes(x=long, y = lat, group=group),col="darkgrey", fill=NA)+ 
  coord_fixed(1.3)+
  geom_point(data=gridseason, aes(x=Longitude,y=Latitude, col=RFpred),  shape=16, size=0.9)+
  
    #geom_text(data=group_summary, aes(x=long,y=lat-0.0025,label=location),size = 4)+
  labs(x="Longitude", y="Latitude", col= "Presence Probability")+facet_wrap(~thermo_season)+
  theme(text = element_text(size = 18)) +
  scale_colour_viridis()
#theme(panel.grid.major = element_blank()) 
#       panel.grid.minor = element_blank(),panel.border = element_blank())

plot

#ggsave(plot=plot, paste0("./Results/New/NO43/",spp[i],"/",spp[i],"_Comboseason_model_prediction_gridbuff_map_new_",WL,".png"),  width = 30, height = 20,units = "cm", dpi = 400)


#################################
####################################################################################
### plot raster with harbour outline
p<- ggplot()+
  geom_polygon(data=map_NAD83,aes(x=long, y = lat, group=group),col="black", fill=NA)+
  geom_tile(data=gridseason, aes(x=Longitude, y=Latitude, fill=RFpred),width=0.00075,height=0.00075) +
  #  geom_polygon(data=shorelinemap,aes(x=long, y = lat, group=group),col="black", fill=NA)+
  #geom_tile(data=gridseason2, aes(x=UTM.X, y=UTM.Y, fill=RFpred))+
  #coord_equal()+
  coord_fixed(1.3)+
  labs(x="Longitude", y="Latitude", fill= "Presence Probability", title= spp[i])+
  scale_fill_viridis_c() +facet_wrap(~thermo_season)+
  theme(text = element_text(size = 18)) 

p

ggsave(plot=p, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_prediction_rasterbuff_map_new_title_",WL,".png"),  width = 30, height = 20,units = "cm", dpi = 400)

p<- ggplot()+
  geom_polygon(data=map_NAD83,aes(x=long, y = lat, group=group),col="black", fill=NA)+
  geom_tile(data=gridseason, aes(x=Longitude, y=Latitude, fill=RFpred),width=0.00075,height=0.00075) +
  #  geom_polygon(data=shorelinemap,aes(x=long, y = lat, group=group),col="black", fill=NA)+
  #geom_tile(data=gridseason2, aes(x=UTM.X, y=UTM.Y, fill=RFpred))+
  #coord_equal()+
  coord_fixed(1.3)+
  labs(x="Longitude", y="Latitude", fill= "Presence Probability")+
  scale_fill_viridis_c(limits=c(0,1),guide = guide_colourbar(direction = "horizontal")) +facet_wrap(~thermo_season)+
  guides(fill = guide_colourbar(barwidth = 10, barheight = 1, title.vjust=1, frame.colour="black"))+
  theme(text = element_text(size = 18),legend.position="bottom", legend.text=element_text(size=14)) 

p

ggsave(plot=p, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_prediction_rasterbuff_map_new_notitle2_",WL,".png"),  width = 30, height = 20,units = "cm", dpi = 400)



area<-gridseason %>% dplyr::group_by(thermo_season) %>% dplyr::summarize(sum=sum(RFpred >=0.80)*2500/1000000) ###2500=area of cell in m and 1000000 is conversion from m2 to km2
write.csv(area,paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_areabuff_80plus_",WL,".csv"))

###############################################################################################










#####################################################
#### efishing CPUE compared to prediction - LMB only as so few adults otherwise

if (spp[i]=="Largemouth Bass"){

efishmid<-read.csv("./Data/Telemetry/hh_efish_midpoints.csv")
efishmid<-efishmid %>% dplyr::select(Transect,mid_long,mid_lat)

efish<-read.csv("./Data/Telemetry/HH_TelemetryEfishing_2016-2021_SeasonalCPUE_LargemouthBass_7Sept2023.csv")
#efish<-efish %>% dplyr::rename("Common Carp" = "Carp.CPUE")
efish<-efish %>% dplyr::rename("Largemouth Bass" = "LMB.CPUE",
                               "Transect" = "UpdateTransect")

efish<-left_join(efish,efishmid, by="Transect")

#fish<-spp[i]

#efish <- efish %>% dplyr::select(UpdateTransect,thermo_season,Long,Lat, Zone, contains(fish))
sefishcoords<-efish %>% dplyr::select(mid_long,mid_lat)

gridseason<-as.data.frame(gridseason)
#summer<-gridseason %>% filter(thermo_season=="Summer")
coords <-gridseason %>% dplyr::select(Longitude,Latitude)
coords <- coords %>% rename(Long = Longitude, Lat = Latitude)
###not get nearest gridseason point to efishing with nearest neighbour
library(FNN)
c<-get.knnx(coords,sefishcoords,k=1)

sefishcoords$Longitude <- coords$Long[c$nn.index]
sefishcoords$Latitude <- coords$Lat[c$nn.index]

###bind back to dataframes
df_to_join <- unique(sefishcoords)
efish <- left_join(efish, df_to_join, by=c("mid_long","mid_lat"))

efish <- left_join(efish,gridseason, by=c("Longitude","Latitude", "thermo_season"))
efish <- efish %>% rename("CPUE"=spp[i])

efish$PA<-ifelse(efish$CPUE>0,1,0)
#efish$PA<-as.factor(efish$PA)
#write.csv(efish,"efish_predictions_matched_adultLMB_new.csv")
## all data
library(PresenceAbsence) ## install this library

efish<-efish %>% filter(thermo_season=="Summer")
#df$PA<-ifelse(df$CPUE>0,1,0) ## to make sure PA is not a factor
Keep.3<-c("Transect","PA","RFpred") ## it apparently wants a more limited dataset - so Column1=ID, Column2=PA, Column3=Props
df.sum.2<-efish[Keep.3]
optimal.thresholds(df.sum.2,threshold=101) ## gives a bunch of diff output, using MaxKappa

lm<-lm(data=efish, RFpred~CPUE)
summary(lm)
plot(lm)

library(betareg)

model.beta = betareg(RFpred ~ CPUE, data=efish)
library(lmtest)


lrtest(model.beta)

p<-ggplot(data=efish, aes(x=Long, y=Lat))+
  geom_polygon(data=map_NAD83,aes(x=long, y = lat, group=group),col="darkgrey", fill=NA)+ 
  coord_fixed(1.3)+#, xlim = c(-79.875, -79.865), ylim = c(43.268, 43.275))+
  geom_point(data=efish, aes(x=Longitude, y=Latitude), col="red")+
 # geom_point(data=efish, aes(x=Longitude, y=Latitude, size=RFpred))+
  geom_text(data=efish,aes(x=Longitude, y=Latitude, label=Transect))
   #geom_smooth(method = "lm", se = FALSE)+
 # facet_wrap(~thermo_season)
#  labs(y="Presence Probability")
p


p<-ggplot(data=efish , aes(x=CPUE, y=RFpred, col=thermo_season))+geom_point(size=2)+xlim(0,1)+ylim(0,1)+
 #geom_hline(yintercept = 0.51)+ ###overall optimal threshold for model having presence of fish
   #geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  #geom_smooth(method = "lm", se = FALSE)+
  #geom_text(data=efish,aes(x=CPUE, y=RFpred, label=UpdateTransect))+
    #facet_wrap(~thermo_season)+
  theme(text = element_text(size = 18)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(y="Presence Probability", col="Season")
p

ggsave(plot=p, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_prediction_vs_efishbuff_new_",WL,".png"),  width = 20, height = 10,units = "cm", dpi = 400)


plot<-ggplot()+
  geom_histogram(data=efish, aes(x=RFpred, fill=factor(PA)),alpha = 0.5, position = "dodge")
plot

plot<-ggplot(data=efish, aes(y=RFpred,x=factor(PA), fill=factor(PA)))+
  geom_boxplot(alpha = 0.5)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  labs(x="Presence/Absence", y = "Presenece Probability")+ theme(legend.position = "none")
plot
#ggsave(plot=plot, paste0("./Results/New/",spp[i],"/",spp[i],"_boxplot_prediction_vs_efishbuff_",WL,".png"),  width = 30, height = 20,units = "cm", dpi = 400)

efish$thermo_season<-factor(efish$thermo_season, levels=c("Spring","Summer","Fall","Winter"))

plot<-ggplot(data=efish, aes(y=RFpred,x=factor(PA), fill=factor(thermo_season)))+
  geom_boxplot(alpha = 0.5)+
  stat_summary(fun.y=mean, geom="point", size=3, aes(color=thermo_season), position = position_dodge(0.75)) +
  labs(x="Presence/Absence", fill="Season",color="Season", y="Presence Probability")
# theme(legend.position = "none")
plot
#ggsave(plot=plot, paste0("./Results/New/",spp[i],"/",spp[i],"_boxplot_season_prediction_vs_efishbuff_",WL,".png"),  width = 30, height = 20,units = "cm", dpi = 400)

stats<-efish %>% group_by( PA) %>% summarise(mean=mean(RFpred))



### plot raster with harbour outline
p<- ggplot()+
  geom_polygon(data=map_NAD83,aes(x=long, y = lat, group=group),col="black", fill=NA)+
  geom_tile(data=gridseason, aes(x=Longitude, y=Latitude, fill=RFpred),width=0.00075,height=0.00075) +
  geom_point(data=efish, aes(x=mid_long, y=mid_lat, size=CPUE), shape=21, alpha=0.5, fill="red")+
  coord_fixed(1.3)+
  labs(x="Longitude", y="Latitude", fill= "Presence Probability", title= spp[i])+
  scale_size(range = c(2, 12))+
  scale_fill_viridis_c() +facet_wrap(~thermo_season)+
  theme(text = element_text(size = 18)) 

p

ggsave(plot=p, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_CPUE_prediction_rasterbuff_map_",WL,".png"),  width = 30, height = 20,units = "cm", dpi = 400)

#####

### plot raster with harbour outline
p<- ggplot()+
  geom_polygon(data=map_NAD83,aes(x=long, y = lat, group=group),col="black", fill="grey")+
  geom_tile(data=gridseason, aes(x=Longitude, y=Latitude,fill=RFpred),width=0.00075,height=0.00075) +
  geom_point(data=efish, aes(x=mid_long, y=mid_lat, size=CPUE), shape=21, alpha=0.5, fill="red")+
  coord_fixed(1.3)+
  scale_fill_steps(n.breaks = 6, low="navy",high="yellow")+
  labs(x="Longitude", y="Latitude", fill= "Presence Probability", title= spp[i])+
  facet_wrap(~thermo_season)+
  theme(text = element_text(size = 18)) 

p

ggsave(plot=p, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_Comboseason_model_CPUE_92prediction_rasterbuff_map_",WL,".png"),  width = 30, height = 20,units = "cm", dpi = 400)


###summer only
p<- ggplot()+
  geom_polygon(data=map_NAD83,aes(x=long, y = lat, group=group),col="black", fill=NA)+
  geom_tile(data=gridseason %>% filter(thermo_season=="Summer"), aes(x=Longitude, y=Latitude, fill=RFpred),width=0.00075,height=0.00075) +
  geom_point(data=efish %>% filter(thermo_season=="Summer"), aes(x=mid_long, y=mid_lat, size=CPUE), shape=21, alpha=0.5, fill="red")+
  coord_fixed(1.3)+
  scale_size(range = c(2, 12))+
  labs(x="Longitude", y="Latitude", fill= "Presence Probability", title= paste0(spp[i], " - Summer"))+
  
  scale_fill_viridis_c() +
  theme(text = element_text(size = 18)) 

p

ggsave(plot=p, paste0("./Results/New/Revise/",spp[i],"/",spp[i],"_summer_model_CPUE_prediction_rasterbuff_map_",WL,".png"),  width = 30, height = 20,units = "cm", dpi = 400)

}


}

##### done analyses for this species. 

###########################################################################
### raster habitat availability predictions
####this did not work well and over projected where fish were and could not be done with year and season

########################
## load rasters
fetch <- raster::raster("./Data/GIS/Rasters/fetchweightedm.tif")
dist_WLRH <- raster::raster("./Data/GIS/Rasters/dist_WLRH.tif")
Emerg_Pres <- raster::raster("./Data/GIS/Rasters/Emerg_Pres.tif")
hard2 <- raster::raster("./Data/GIS/Rasters/hardbuff2.tif")
SAV <- raster::raster("./Data/GIS/Rasters/SAVbuffLW.tif")
WL <- raster::raster("./Data/GIS/Rasters/WLbuff.tif")

plot(fetch)
#### match extent of rasters to hard2 and SAV
fetch
dist_WLRH
Emerg_Pres
hard2
SAV
WL

### crop to same extent
dist_WLRH <- crop(dist_WLRH,extent(hard2))
Emerg_Pres <- crop(Emerg_Pres,extent(hard2))
SAV <- crop(SAV,extent(hard2))
fetch <- crop(fetch,extent(hard2))
WL <- crop(WL,extent(hard2))

#### resample so same resolution - use SAVa and hard2a as these are same
fetch <- resample(fetch,SAV,method='ngb')
dist_WLRH <- resample(dist_WLRH,SAV,method='ngb')
Emerg_Pres <- resample(Emerg_Pres,SAV,method='ngb')
WL <- resample(WL,SAV,method='ngb')
hard2<-resample(hard2,SAV,method='ngb')


#stack all rasters together
habitat_stack <- stack(SAV,WL,dist_WLRH,hard2,fetch, Emerg_Pres)

#plot(habitat_stack)
names(habitat_stack)
names(habitat_stack) <- c("SAV","WL","dist_WLRH","hard2","fetch", "Emerg_Pres")
###############################################
max(receivers$Weighted.Fetch.m.Mean)
### combine by season for moment. 
###might have to run individually by season!
trainspring<-train2 %>% filter(thermo_season=="Summer")

#model formula
z <- formula(presence~SAV+WL+dist_WLRH+hard2+fetch)##Cluster_group2

#Optimize classwt
#RF1_simulations <- function_SimulateAccuracy(predictor = "presence", model.formula=z,
#                                             data_Train = train, data_Test = test,
#                                             loopweight = c(100,10,2,1.5,1,0.95,0.8,0.1,0.01,0.001))

###LMB optimal level was 0.8.

###use 0.95 for everything for now.
#balanced
Forest <- randomForest(formula=z, data = trainspring, replace=FALSE, na.action=na.omit,
                       importance=TRUE, classwt=c(1,0.95), do.trace=1000, ntree=1000)

Forest_testpred <- predict(Forest, test, type="response")
confusionMatrix(Forest_testpred, test$pres, positive="1")

model_prob <- 1-raster::predict(model=Forest, object= habitat_stack, type='prob')

#plot(model_prob, col=viridis(10))#, col=palette)

model_prob_df <- as.data.frame(model_prob, xy = TRUE)
str(model_prob_df)

p<-ggplot()+
   geom_raster(data = model_prob_df , aes(x = x, y = y,
                                       fill = layer)) +
  coord_sf()+
 # geom_sf() +
#  geom_polygon(data = map_NAD83, aes(x = long, y = lat, group = group),fill=NA, colour = "grey35")+
    labs(x="Longitude", y="Latitude", fill="Probability")+
  scale_fill_viridis(na.value = "transparent", limits=c(0,1))

p 

#ggsave(plot=p, paste0("./Results/",spp[i],"/",spp[i],"_raster_model_prediction_allseasons.png"),  width = 30, height = 15,units = "cm", dpi = 400)



################################################################
#spatial preds:
seasons <- data.frame(thermo_season=unique(fish_all_rcvrs$thermo_season))
grid <-read.csv("./Data/HH_50mGrid_Depth_SAV_Fetch_WetlandDist_06June2023.csv")
hard <-read.csv("./Data/HH_Grid50_25mBuffer_PropHardSubstrate_07June2023.csv")
grid<-left_join(grid,hard, by="ID")


gridseason <- merge(seasons, grid)
gridseason$hard2<-gridseason$hard
RFpred <- predict(Forest, gridseason, type="prob")


#receivers$year<-as.factor(format(receivers$date,"%Y"))

#rcvrinfo <- receivers %>% group_by(station,Emerg_Pres,year) %>%  
#  summarise(hard=mean(perc_hard), slope=mean(SLOPE_mean),sand=mean(per_sand),dist_WLRH=mean(ClosestWL),dist_rm=mean(RM_DistFix), SAV=mean(SAV_mean), fetch=mean(Weighted.Fetch.m.Mean), WL=mean(WL_AVG_mean), WL_SD=mean(WL_AVG_SD),
#            lat=mean(deploy_lat),long=mean(deploy_long))  
#rcvrinfo$Emerg_Pres<-as.factor(rcvrinfo$Emerg_Pres)


gridseason$RFpred <- RFpred[,2]
###remove NAs
gridseason<-gridseason %>% filter(!is.na(RFpred))
###order ascending
gridseason<-gridseason %>% arrange(RFpred)

###without year
#rcvrinfo <- receivers %>% group_by(station,Emerg_Pres) %>%  
#  summarise(hard=mean(perc_hard), slope=mean(SLOPE_mean),sand=mean(per_sand),dist_WLRH=mean(ClosestWL),dist_rm=mean(RM_DistFix), SAV=mean(SAV_mean), fetch=mean(Weighted.Fetch.m.Mean), WL=mean(WL_AVG_mean), WL_SD=mean(WL_AVG_SD),
#            lat=mean(deploy_lat),long=mean(deploy_long))  
#rcvrinfo$Emerg_Pres<-as.factor(rcvrinfo$Emerg_Pres)

#rcvrseason <- merge(seasons, rcvrinfo)

#RFpred <- predict(Forest, rcvrseason, type="prob")

#rcvrseason$RFpred <- RFpred[,2]


plot<-ggplot()+
  geom_polygon(data=map_NAD83,aes(x=long, y = lat),col="darkgrey", fill="white")+ 
  coord_fixed(1.3)+
  geom_point(data=gridseason, aes(x=Longitude,y=Latitude, col=RFpred),  shape=16, size=0.9)+
  #geom_text(data=group_summary, aes(x=long,y=lat-0.0025,label=location),size = 4)+
  labs(x="Longitude", y="Latitude", col= "Presence Probability")+facet_wrap(~thermo_season)+
  theme(text = element_text(size = 18)) +
  scale_colour_viridis()
#theme(panel.grid.major = element_blank()) 
#       panel.grid.minor = element_blank(),panel.border = element_blank())

plot

ggsave(plot=plot, paste0("./Results/",spp[i],"/",spp[i],"_Comboseason_model_prediction_grid_map1.png"),  width = 30, height = 20,units = "cm", dpi = 400)



########################################################################



#########################################################################
###individual season analyses

season<- c("Spring", "Summer","Fall", "Winter")

myplots <- list() 
#t=1
#loop to go through all individuals
for (t in 1:length(season)){

  season_fish_all_rcvrs<- fish_all_rcvrs %>% filter(thermo_season==season[t])
  season_bal_fish_rcvrs<- balanced_fish_rcvrs %>% filter(thermo_season==season[t])
  
  
#split data into train and test 
season_fish_all_rcvrs$nrow <- 1:length(season_fish_all_rcvrs$date)
set.seed(123)
train <- season_fish_all_rcvrs %>% sample_frac(0.7) # 70/30 split. 
test <- season_fish_all_rcvrs[!(season_fish_all_rcvrs$nrow %in% train$nrow),]

#model formula
z <- formula(presence~SAV+WL+fetch+hard+dist_WLRH+Emerg_Pres)##slope, wd_sd, dist_rm

#Optimize classwt
#RF1_simulations <- function_SimulateAccuracy(predictor = "presence", model.formula=z,
#                                             data_Train = train, data_Test = test,
#                                             loopweight = c(100,10,2,1.5,1,0.95,0.8,0.1,0.01,0.001))

###LMB optimal level was 0.8.

###use 0.95 for everything for now.
#balanced
Forest <- randomForest(formula=z, data = train, replace=FALSE, na.action=na.omit,
                       importance=TRUE, classwt=c(1,0.95), do.trace=1000, ntree=1000)

Forest_testpred <- predict(Forest, test, type="response")
confusionMatrix(Forest_testpred, test$pres, positive="1")

#####Variable importance##########################################----
#-------------------------------------------------------------#

#variable importance
varIMP <- data.frame(importance(Forest))
varIMP$predictor <- rownames(varIMP)
head(varIMP)

p<-ggplot(varIMP, aes(MeanDecreaseAccuracy, reorder(predictor,MeanDecreaseAccuracy)))+
  geom_histogram(stat="identity")+theme_bw()+ylab("Predictor")+ 
  theme(text = element_text(size=16))+
  scale_y_discrete(labels=c("thermo_season" = "Season", "slope" = "Slope","SAV"="% Submerged Veg", "fetch" = "Fetch",
                            "WL"="Water Depth", "dist_WLRH"= "Distance to Wetland", "dist_rm"= "Distance to River Mouth",
                            "WL_SD"="Water Depth SD", "Emerg_Pres"="Emergent Veg Presence", "hard"="% Gravel/Cobble/Boulder"
  ))
p
ggsave(plot=p, paste0("./Results/",spp[i],"/",spp[i],"_singleseason_",season[t],"_model_variableimportance.png"),  width = 20, height = 15,units = "cm", dpi = 400)


# variable partial dependencies. Just SAV as eg. Everything past here will be computationally intensive ****
partialSAV <- Forest %>% partial(pred.var = "SAV", prob = TRUE, which.class='1', train=fish_all_rcvrs)
partialWL <- Forest %>% partial(pred.var = "WL", prob = TRUE, which.class='1', train=fish_all_rcvrs)
#partialWLSD <- Forest %>% partial(pred.var = "WL_SD", prob = TRUE, which.class='1', train=fish_all_rcvrs)
partialfetch<-Forest %>% partial(pred.var = "fetch", prob = TRUE, which.class='1', train=fish_all_rcvrs)
#partialClust<-Forest %>% partial(pred.var = "Cluster_group2", prob = TRUE, which.class='1', train=fish_all_rcvrs)
partialdist_WLRH <- Forest %>% partial(pred.var = "dist_WLRH", prob = TRUE, which.class='1', train=fish_all_rcvrs)
#partialdist_rm <-Forest %>% partial(pred.var = "dist_rm", prob = TRUE, which.class='1', train=fish_all_rcvrs)
#partialsand <- Forest %>% partial(pred.var = "sand", prob = TRUE, which.class='1', train=fish_all_rcvrs)
partialhard <- Forest %>% partial(pred.var = "hard", prob = TRUE, which.class='1', train=fish_all_rcvrs)
#partialslope <- Forest %>% partial(pred.var = "slope", prob = TRUE, which.class='1', train=fish_all_rcvrs)
partialEV <- Forest %>% partial(pred.var = "Emerg_Pres", prob = TRUE, which.class='1', train=fish_all_rcvrs)

#ggplot(partialslope, aes(slope, yhat))+
#  geom_point()+
#  geom_smooth()+
#  ylab(bquote("Marginal effect" ~(hat(y))))+
#  theme_bw()

#ggplot(partialsand, aes(sand, yhat))+
#  geom_point()+
#  geom_smooth()+
#  ylab(bquote("Marginal effect" ~(hat(y))))+
#  theme_bw()

p2<- ggplot(partialhard, aes(hard, yhat))+
  geom_point()+
  geom_smooth()+
  theme(text = element_text(size=18))+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  xlab("% Gravel/Cobble/Boulder")+
  theme_bw()
p2
ggsave(plot=p2, paste0("./Results/",spp[i],"/",spp[i],"_singleseason_",season[t],"_model_effect_hard.png"),  width = 15, height = 10,units = "cm", dpi = 400)


p3<-ggplot(partialSAV, aes(SAV, yhat))+
  geom_point()+
  geom_smooth()+
  theme(text = element_text(size=18))+
  xlab("% Submerged Veg")+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()
p3
ggsave(plot=p3, paste0("./Results/",spp[i],"/",spp[i],"_singleseason_",season[t],"_model_effect_SAV.png"),  width = 15, height = 10,units = "cm", dpi = 400)


p4<-ggplot(partialWL, aes(WL, yhat))+
  geom_point()+
  geom_smooth()+
  theme(text = element_text(size=18))+
  xlab("Water Depth (m)")+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()
p4
ggsave(plot=p4, paste0("./Results/",spp[i],"/",spp[i],"_singleseason_",season[t],"_model_effect_depth.png"),  width = 15, height = 10,units = "cm", dpi = 400)


#ggplot(partialWLSD, aes(WL_SD, yhat))+
#  geom_point()+
#  geom_smooth()+
#  ylab(bquote("Marginal effect" ~(hat(y))))+
#  theme_bw()

p5<-ggplot(partialfetch, aes(fetch, yhat))+
  geom_point()+
  geom_smooth()+
  theme(text = element_text(size=18))+
  xlab("Fetch (km)")+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()
p5
ggsave(plot=p5, paste0("./Results/",spp[i],"/",spp[i],"_singleseason_",season[t],"_model_effect_fetch.png"),  width = 15, height = 10,units = "cm", dpi = 400)


p6<-ggplot(partialdist_WLRH, aes(dist_WLRH, yhat))+
  geom_point()+
  geom_smooth()+
  theme(text = element_text(size=18))+
  xlab("Distance to Wetland (m)")+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()
p6
ggsave(plot=p6, paste0("./Results/",spp[i],"/",spp[i],"_singleseason_",season[t],"_model_effect_wetland.png"),  width = 15, height = 10,units = "cm", dpi = 400)


#ggplot(partialdist_rm, aes(dist_rm, yhat))+
#  geom_point()+
#  geom_smooth()+
#  ylab(bquote("Marginal effect" ~(hat(y))))+
#  theme_bw()

p7<-ggplot(partialEV, aes(Emerg_Pres, yhat))+
  geom_point()+
  geom_smooth()+
  theme(text = element_text(size=18))+
  xlab("Emergent Veg Presence")+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()
p7
ggsave(plot=p7, paste0("./Results/",spp[i],"/",spp[i],"_singleseason_",season[t],"_model_effect_emergveg.png"),  width = 15, height = 10,units = "cm", dpi = 400)

#########################################################################
########################################################

###########################################
#map of importance

####load hamilton harbour shapefile
shorelinemap<- readOGR("./Data/GIS", "HH_outline")

###need to convert to lat long from UTMS or points
map_NAD83 <- spTransform(shorelinemap, CRS("+proj=longlat +datum=NAD83"))

plot(map_NAD83, col="grey")


#spatial preds:
#seasons <- data.frame(thermo_season=unique(fish_all_rcvrs$thermo_season))

rcvrinfo <- receivers %>% group_by(station,Emerg_Pres) %>%  
  summarise(hard=mean(perc_hard), slope=mean(SLOPE_mean),sand=mean(per_sand),dist_WLRH=mean(ClosestWL),dist_rm=mean(RM_DistFix), SAV=mean(SAV_mean), fetch=mean(Weighted.Fetch.m.Mean), WL=mean(WL_AVG_mean), WL_SD=mean(WL_AVG_SD),
            lat=mean(deploy_lat),long=mean(deploy_long))  
rcvrinfo$Emerg_Pres<-as.factor(rcvrinfo$Emerg_Pres)

rcvrinfo$season<-season[t]
#rcvrseason <- merge(seasons, rcvrinfo)

RFpred <- predict(Forest, rcvrinfo, type="prob")
#RFpredub <- predict(ForestUB, rcvrseason, type="prob")
#RFpredob <- predict(RFfullob, nodeseason, type="prob")
#RFpred

rcvrinfo$RFpred <- RFpred[,2]
#nodeseason$RFubpred <- RFpredub[,2]
#nodeseason$RFobpred <- RFpredob[,2]

#nodeseasonmelt <- melt(nodeseason, measure.vars=c("RFpred","RFubpred","RFobpred"))
#head(nodeseasonmelt)
#nodeseasonmelt$model <- ifelse(nodeseasonmelt$variable=="RFpred", "RF weighted",
#                               ifelse(nodeseasonmelt$variable=="RFubpred", "RF", "RF overweighted"))
#nodeseasonmelt$model <- factor(nodeseasonmelt$model, levels=c("RF overweighted", "RF weighted", "RF"))

#Set map data
#register_google(key = "AIzaSyC7M4nwUb2wgOAPvn_g1hsWeN-3O0FfGz0")
#TOmap <- get_googlemap(center = c(lon=mean(TH_data_rec_env$lon), lat=mean(TH_data_rec_env$lat)),
#                       zoom = 12, size = c(640, 640), scale = 4,
#                       maptype = c("satellite"))

plot<-ggplot()+
  geom_polygon(data=map_NAD83,aes(x=long, y = lat),col="darkgrey", fill="white")+ 
  coord_fixed(1.3)+
  geom_point(data=rcvrinfo, aes(x=long,y=lat, fill=RFpred, size=RFpred), col="black", shape=21)+
  #geom_text(data=group_summary, aes(x=long,y=lat-0.0025,label=location),size = 4)+
  labs(x="Longitude", y="Latitude", size= "Presence Probability", fill= "Presence Probability")+
  theme(text = element_text(size = 18)) +facet_wrap(~season)+
  scale_fill_viridis()

if(season[t]=="Spring"){
  plot<-plot+theme(legend.position="none")
}
if(season[t]=="Fall"){
  plot<-plot+theme(legend.position="none")
}
if(season[t]=="Winter"){
  plot<-plot+theme(legend.position="none")
}
 
plot

myplots[[t]] <- plot

}

margin = theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))
all_plot<-grid.arrange(grobs = lapply(myplots, "+", margin),nrow=2)
all_plot

ggsave(plot=all_plot, paste0("./Results/",spp[i],"/",spp[i],"_singleseason_model_prediction_station_map.png"),  width = 30, height = 20,units = "cm", dpi = 400)
###looks ugly but not sure how to fix rn


#########################################################################





















#########################################################

### not doing for the moment.
#variable importance
varIMP <- data.frame(importance(ForestUB))
varIMP$predictor <- rownames(varIMP)
head(varIMP)

ggplot(varIMP, aes(MeanDecreaseAccuracy, reorder(predictor,MeanDecreaseAccuracy)))+
  geom_histogram(stat="identity")+theme_bw()+ylab("Predictor")


# variable partial dependencies. Just SAV as eg. Everything past here will be computationally intensive ****
partialSAV <- ForestUB %>% partial(pred.var = "SAV", prob = TRUE, which.class='1', train=balanced_fish_rcvrs)
partialWL <- ForestUB %>% partial(pred.var = "WL", prob = TRUE, which.class='1', train=balanced_fish_rcvrs)
partialfetch <- ForestUB %>% partial(pred.var = "fetch", prob = TRUE, which.class='1', train=balanced_fish_rcvrs)

ggplot(partialSAV, aes(SAV, yhat))+
  geom_point()+
  geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()

ggplot(partialWL, aes(WL, yhat))+
  geom_point()+
  geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()

ggplot(partialfetch, aes(fetch, yhat))+
  geom_point()+
  geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()

#####Bivariate plots##############################################----
#-------------------------------------------------------------#
#interaction dependencies. Season x depth
partialseasonWL <- ForestUB %>% partial(pred.var = c("thermo_season","WL"), prob = TRUE, which.class='1', train=balanced_fish_rcvrs)
ggplot(partialseasonWL, aes(WL, yhat, col=thermo_season))+geom_point()+geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+theme_bw()
ggplot(partialseasonWL, aes(x=thermo_season, y=WL, fill=yhat))+
  geom_tile()+
  theme_bw()+
  scale_fill_viridis_c()

################################################################

#########################################################
############ seasons as separate ########
### curious if similar results
#####split by season - loop eventually



spr_fish_all_rcvrs<- fish_all_rcvrs %>% filter(thermo_season=="Spring")
spr_bal_fish_rcvrs<- balanced_fish_rcvrs %>% filter(thermo_season=="Spring")

#split data into train and test 
spr_fish_all_rcvrs$nrow <- 1:length(spr_fish_all_rcvrs$date)
set.seed(123)
train <- spr_fish_all_rcvrs %>% sample_frac(0.7) # 70/30 split. 
test <- spr_fish_all_rcvrs[!(spr_fish_all_rcvrs$nrow %in% train$nrow),]

#spr_bal_fish_rcvrs$nrow <- 1:length(spr_bal_fish_rcvrs$date)
#set.seed(123)
#train2 <- spr_bal_fish_rcvrs %>% sample_frac(0.7) # 70/30 split. 
#test2 <- spr_bal_fish_rcvrs[!(spr_bal_fish_rcvrs$nrow %in% train2$nrow),]


#model formula
z <- formula(presence~slope+SAV+WL+WL_SD+fetch+hard+sand+dist_rm+dist_WLRH+Emerg_Pres)##Cluster_group2
#z <- formula(presence~SAV+WL+fetch+hard+sand+dist_WLRH+Emerg_Pres)##Cluster_group2


#balanced
Forest <- randomForest(formula=z, data = train, replace=FALSE, na.action=na.omit,
                       importance=TRUE, classwt=c(1,0.95), do.trace=1000, ntree=1000)

Forest_testpred <- predict(Forest, test, type="response")
confusionMatrix(Forest_testpred, test$pres, positive="1")


#unbalanced
#ForestUB <- randomForest(formula=z, data = train2, replace=FALSE, na.action=na.omit,
#                         importance=TRUE, do.trace=1000, ntree=1000)
#print(ForestUB)
#ForestUB_testpred <- predict(ForestUB, test2, type="response")
#confusionMatrix(ForestUB_testpred, test2$pres,  positive="1")


#####Variable importance##########################################----
#-------------------------------------------------------------#

#variable importance
varIMP <- data.frame(importance(Forest))
varIMP$predictor <- rownames(varIMP)
head(varIMP)

ggplot(varIMP, aes(MeanDecreaseAccuracy, reorder(predictor,MeanDecreaseAccuracy)))+
  geom_histogram(stat="identity")+theme_bw()+ylab("Predictor")

# variable partial dependencies. Just SAV as eg. Everything past here will be computationally intensive ****
partialSAV <- Forest %>% partial(pred.var = "SAV", prob = TRUE, which.class='1', train=fish_all_rcvrs)
partialWL <- Forest %>% partial(pred.var = "WL", prob = TRUE, which.class='1', train=fish_all_rcvrs)
partialWLSD <- Forest %>% partial(pred.var = "WL_SD", prob = TRUE, which.class='1', train=fish_all_rcvrs)
partialfetch<-Forest %>% partial(pred.var = "fetch", prob = TRUE, which.class='1', train=fish_all_rcvrs)
partialdist_WLRH <- Forest %>% partial(pred.var = "dist_WLRH", prob = TRUE, which.class='1', train=fish_all_rcvrs)
partialdist_rm <-Forest %>% partial(pred.var = "dist_rm", prob = TRUE, which.class='1', train=fish_all_rcvrs)
partialsand <- Forest %>% partial(pred.var = "sand", prob = TRUE, which.class='1', train=fish_all_rcvrs)
partialhard <- Forest %>% partial(pred.var = "hard", prob = TRUE, which.class='1', train=fish_all_rcvrs)
partialEV <- Forest %>% partial(pred.var = "Emerg_Pres", prob = TRUE, which.class='1', train=fish_all_rcvrs)


ggplot(partialsand, aes(sand, yhat))+
  geom_point()+
  geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()

ggplot(partialhard, aes(hard, yhat))+
  geom_point()+
  geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()

ggplot(partialSAV, aes(SAV, yhat))+
  geom_point()+
  geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()

ggplot(partialWL, aes(WL, yhat))+
  geom_point()+
  geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()

ggplot(partialWLSD, aes(WL_SD, yhat))+
  geom_point()+
  geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()

ggplot(partialfetch, aes(fetch, yhat))+
  geom_point()+
  geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()

#ggplot(partialClust, aes(Cluster_group2, yhat))+
#  geom_point()+
#  geom_smooth()+
#  ylab(bquote("Marginal effect" ~(hat(y))))+
#  theme_bw()

ggplot(partialdist_WLRH, aes(dist_WLRH, yhat))+
  geom_point()+
  geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()

ggplot(partialdist_rm, aes(dist_rm, yhat))+
  geom_point()+
  geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()

ggplot(partialEV, aes(Emerg_Pres, yhat))+
  geom_point()+
  geom_smooth()+
  ylab(bquote("Marginal effect" ~(hat(y))))+
  theme_bw()

##########################
#interaction dependencies. Season x depth
partialSAVWL <- Forest %>% partial(pred.var = c("SAV","WL"), prob = TRUE, which.class='1', train=spr_fish_all_rcvrs)
#ggplot(partialSAVWL, aes(WL, yhat, col=SAV))+geom_point()+geom_smooth()+
#  ylab(bquote("Marginal effect" ~(hat(y))))+theme_bw()
ggplot(partialSAVWL, aes(x=SAV, y=WL, fill=yhat))+
  geom_tile()+
  theme_bw()+
  scale_fill_viridis_c()

partialWLRH_WL <- Forest %>% partial(pred.var = c("dist_WLRH","WL"), prob = TRUE, which.class='1', train=spr_fish_all_rcvrs)
#ggplot(partialSAVWL, aes(WL, yhat, col=SAV))+geom_point()+geom_smooth()+
#  ylab(bquote("Marginal effect" ~(hat(y))))+theme_bw()
ggplot(partialWLRH_WL, aes(x=dist_WLRH, y=WL, fill=yhat))+
  geom_tile()+
  theme_bw()+
  scale_fill_viridis_c()

partialWLRH_fetch <- Forest %>% partial(pred.var = c("dist_WLRH","fetch"), prob = TRUE, which.class='1', train=spr_fish_all_rcvrs)
#ggplot(partialSAVWL, aes(WL, yhat, col=SAV))+geom_point()+geom_smooth()+
#  ylab(bquote("Marginal effect" ~(hat(y))))+theme_bw()
ggplot(partialWLRH_fetch, aes(x=dist_WLRH, y=fetch, fill=yhat))+
  geom_tile()+
  theme_bw()+
  scale_fill_viridis_c()
###############################################################
