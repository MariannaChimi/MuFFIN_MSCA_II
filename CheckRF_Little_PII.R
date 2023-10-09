
rm(list = ls())

library(data.table)
library(zoo)
library(caTools)
library(ggplot2)
library(ggpubr)
library(Rmixmod)
library(tidyverse)
library(lubridate)
library(viridis)
library(plyr)
library(smoother)
library("ggsci")
library("momentuHMM")
options(digits.secs=9)

################### ################### ###################
################### ################### ###################
##################### read files
################### ################### ###################
################### ################### ###################


filenamesTrips <-
  list.files(
    "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/2018Acc_RF",
    pattern = "*.csv",
    full.names = TRUE
  )
filenamesTrips


#### read file
#accData<-fread("/Users/mariannachimienti/Dropbox/MarieCurie/DataLittle/G3002MP19_S1Cut.csv",header=TRUE)
#  TripID<-19
 # filenamesTrips[TripID]
# 
#   for (TripID in 1:length(filenamesTrips)) {
#   accData <- fread(filenamesTrips[TripID], header = TRUE)
#   print(filenamesTrips[TripID])
#   print(accData[1,1:5])
#  }
 # 
 # accData$TagID<-"G3007FP3"
 # head(accData)
 # write.csv(accData,"/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/2018Acc_RF/G3007FP3_S125Hz_Trip1_RF.csv", row.names = FALSE)
 # 
 
 for (TripID in 1:length(filenamesTrips)) { #change this line if you want more or less files running
  accData <- fread(filenamesTrips[TripID], header = TRUE)
# accData<-accData[,-c("V1")]
# names(accData) <- gsub(x = names(accData), pattern = "\\.", replacement = "_")
# names(accData) <- gsub(x = names(accData), pattern = "\\-", replacement = "_")
# names(accData) <- gsub(x = names(accData), pattern = "\\ ", replacement = "_")
# names(accData) <- gsub(x = names(accData), pattern = "\\(", replacement = "_")
# names(accData) <- gsub(x = names(accData), pattern = "\\)", replacement = "_")
# names(accData) <- gsub(x = names(accData), pattern = "\\?", replacement = "_")
# 
# options(digits.secs=9)
# accData$DatesPos<-as.POSIXct(accData$Timestamp, format='%d/%m/%Y %H:%M:%OS',tz="UTC")
# head(accData)
#
# unique(accData$Pred_RF)
# table(accData$Pred_RF)/nrow(accData)

# print(summary(accData$depth25Hz[which(accData$Pred_RF=="Preen/highFlap_W")]))
# }
  print(summary(accData$depth25Hz[which(accData$Pred_RF=="Hunting")]))
}

accData$Pred_RF<-as.factor(accData$Pred_RF)
dim(accData)
# #
start<-300000
end<-400000

plot1<-ggplot(accData[start:end,]) +
  geom_line(aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=(depth25Hz*-1),colour=Pred_RF), size=2)+
  scale_colour_jco()+xlab("")+ylab("Depth (m)")
plot1

plot3<-ggplot(accData[start:end,]) +
  geom_line(aes( x=DatesPos, y=VeDBA),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=VeDBA,colour=Pred_RF), size=2)+
  scale_colour_jco()+xlab("")+ylab("VeDBA")

plot4<-ggplot(accData[start:end,]) +
  geom_line(aes( x=DatesPos, y=SD_DV2),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=SD_DV2,colour=Pred_RF), size=2)+
  scale_colour_jco()+xlab("")+ylab("SD_DV2")

plot5<-ggplot(accData[start:end,]) +
  geom_line(aes( x=DatesPos, y=PitchDiff),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=PitchDiff,colour=Pred_RF), size=2)+
  scale_colour_jco()+xlab("")+ylab("Pitch")
#dev.new()
#ggarrange(plot1,plot3,plot5, ncol=1,common.legend = TRUE)



################### ################### ###################
################### create catching DF
################### ################### ###################
# subDataDive<-accData[which(accData$Sur_NotSur==1),]
# summary(subDataDive$depth25Hz)
# 
# ####### divide Dives in events and extrat time spent in different states.
# subDataDive$gap <- c(0, diff(subDataDive$DatesPos,units = "secs") > 0.0500)
# subDataDive$group <- cumsum(subDataDive$gap) + 1
# #plot(subDataDive$depth25Hz[which(subDataDive$group==200)], type="b")
# 
# CatchingDF<-data.frame(NA,NA,NA,NA,NA,NA)# empty dataframe for storing results
# rowCount<-1
# 
# for (DiveEv in 1:length(unique(subDataDive$group))) {
#   ###### divide each Dives in catching events and extrat time spent in different catching events
#   dive_ev <- subDataDive[which(subDataDive$group == DiveEv &
#                                  subDataDive$StatesNames == "Hunting"), ]
# 
#   dive_ev$gapCatch <- c(0, diff(dive_ev$DatesPos, units = "secs") > 0.0500)
#   dive_ev$groupCatch <- cumsum(dive_ev$gapCatch) + 1
#   #unique(dive_ev$groupCatch)
# 
#   if (nrow(dive_ev) > 2) {
#     for (catchEv in 1:length(unique(dive_ev$groupCatch))) {
#       dive_evSub <- dive_ev[which(dive_ev$groupCatch == catchEv), ]
# 
#       if(nrow(dive_evSub)>1){ #### see if you want to discard or not just 1 data point
#         CatchingDF[rowCount, 1] <- dive_evSub$Timestamp[1] #start
#         CatchingDF[rowCount, 2] <-
#           dive_evSub$Timestamp[nrow(dive_evSub)] #end
# 
#         duration <-
#           difftime(dive_evSub$DatesPos[nrow(dive_evSub)], dive_evSub$DatesPos[1], units = "secs")
#         CatchingDF[rowCount, 3] <- as.numeric(duration)#duration
# 
#         CatchingDF[rowCount, 4] <- mean(dive_evSub$depth25Hz)#depth
#         CatchingDF[rowCount, 5] <- mean(dive_evSub$VeDBA)#VeDBA
#         CatchingDF[rowCount, 6] <- unique(dive_ev$group)
# 
#         rowCount <- rowCount + 1
#       }
#     }
#   }
# }
# 
# 
# colnames(CatchingDF)<- c("DateTime_start","DateTime_end","duration_sec","depth_m","VeDBA","diveID")
# head(CatchingDF)
# options(digits.secs=9)
# CatchingDF$Animal_ID<-as.factor(unique(accData$TagID))
# CatchingDF$DatesPos<-as.POSIXct(CatchingDF$DateTime_start, format='%d/%m/%Y %H:%M:%OS',tz="UTC")
# 
# # CatchingDF$diveID<-as.factor(CatchingDF$diveID)
# #
# #CatchingDF<-CatchingDF[which(CatchingDF$depth_m>=2),]
# #
# # hist(CatchingDFLong$VeDBA)
# #
# # ggplot(CatchingDFLong, aes(x=diveID, y=VeDBA)) +
# #   geom_boxplot(fill="slateblue", alpha=0.2) +
# #   xlab("")
# #
# # plot(CatchingDFLong$duration_sec)
# 
# 
# library(hrbrthemes)
# library(viridis)

# # ViolingP <- ggplot(CatchingDF, aes(x=Animal_ID, y=depth_m)) +
# #   geom_violin(fill="dodgerblue2")+
# #   geom_boxplot(width=0.1, color="grey", alpha=0.2) +
# # #  scale_fill_viridis(discrete = TRUE) +
# #   theme_bw() +
# #   theme(
# #     legend.position="none",
# #     plot.title = element_text(size=20)
# #   ) +ylab("depth (m)")+xlab("")
# #
# # ViolingP
# #
#
#
#
# summary(CatchingDF$depth_m)
# plotCatching<-ggplot() +
#   geom_line(data=accData,aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=0.5)+
#   geom_point(data=CatchingDF,aes( x=DatesPos, y=(depth_m*-1),colour=depth_m), size=2)+ylab("depth (m)")+xlab("")+
#   scale_colour_viridis_c(name="depth (m)")
#
# #plotCatching
#
# summary(CatchingDF$duration_sec)
# MinVal<-round(min(CatchingDF$duration_sec))
# MaxVal<-round(max(CatchingDF$duration_sec))
#
# library("scales")
# plotCatchingDur<-ggplot() +
#   geom_line(data=accData,aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=0.5)+
#   geom_point(data=CatchingDF,aes( x=DatesPos, y=(depth_m*-1),colour=duration_sec), size=2)+ylab("depth (m)")+xlab("")+
#   scale_colour_gradientn(colours = c("papayawhip","peachpuff3","peachpuff4","salmon3","salmon4"),
#                          values = rescale(c(MinVal,2.5,5,MaxVal)),
#                          guide = "colorbar", limits=c(0,MaxVal), name= "duration (sec)")
#
#
# #plotCatchingDur
#
#
# plotCatchingVedBA<-ggplot() +
#   geom_line(data=accData,aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=0.5)+
#   geom_point(data=CatchingDF,aes( x=DatesPos, y=(depth_m*-1),colour=VeDBA), size=2)+ylab("depth (m)")+xlab("")+
#   scale_colour_viridis_c(option = "plasma", name= "VeDBA (g)")
#
# #plotCatchingVedBA
#
# dev.new()
# ggarrange(plotCatching,plotCatchingDur,plotCatchingVedBA,ncol=1,nrow=3,common.legend = FALSE)
#
#

# plotCatchDur<-ggplot() +
#   geom_point(data=CatchingDF,aes( x=duration_sec, y=VeDBA),colour="black", size=2)+ylab("VeDBA (g)")+xlab("depth (m)")
# dev.new()
# plotCatchDur


################### ################### ###################
################### create catching DF and match with GPS
################### ################### ###################

LittleGPSdata<-readRDS("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/lp_interpolated_gps_2010_2020.rds", refhook = NULL)
LittleGPSdata<-as.data.frame(LittleGPSdata)
LittleGPSdata_prep<-prepData(LittleGPSdata[,c("ID","mu.x","mu.y")],coordNames =c("mu.x","mu.y") )
hist(LittleGPSdata_prep$step/1000)



LittleGPSdata$Year<-substr(LittleGPSdata$ID,(nchar(LittleGPSdata$ID)-3),nchar(LittleGPSdata$ID))
head(LittleGPSdata)


yearSel<-LittleGPSdata[which(LittleGPSdata$Year=="2020"),]

# ####### 2016 fix PGG in PG
# unique(yearSel$bird_ID)
# yearSel$bird_ID<-str_replace(yearSel$bird_ID, "PGG", "PG")
# unique(yearSel$bird_ID)

### variables to add from Accelerometer
yearSel$HuntingTime_sec<-NA
yearSel$LengthInt_sec<-NA
yearSel$meanDivingDepth_m<-NA
yearSel$maxDivingDepth_m<-NA
yearSel$meanVeDBASubSwimming<-NA
yearSel$sdVeDBASubSwimming<-NA

head(yearSel)
IDGPS_List<-unique(yearSel$bird_ID)
listAccLoad<-list()

###### select acc data within the GPS list
for (i in 1:length(IDGPS_List)){
rowID<-grep(IDGPS_List[i], filenamesTrips)

listAccLoad[[i]]<-rowID

}
  

DFAccLoad <- ldply (listAccLoad, data.frame)
colnames(DFAccLoad)<-"rowID"

filenamesTripsFinal<-filenamesTrips[DFAccLoad$rowID]
IDGPS_List
filenamesTripsFinal

ListHuntingDF<-list()

#TripID<-19 #length(filenamesTripsFinal)
for (TripID in 1:length(filenamesTripsFinal)) { #change this line if you want more or less files running
  accData <- fread(filenamesTripsFinal[TripID], header = TRUE,stringsAsFactors=FALSE)

  accData<-accData[,-c("V1")]
  names(accData) <- gsub(x = names(accData), pattern = "\\.", replacement = "_")  
  names(accData) <- gsub(x = names(accData), pattern = "\\-", replacement = "_") 
  names(accData) <- gsub(x = names(accData), pattern = "\\ ", replacement = "_") 
  names(accData) <- gsub(x = names(accData), pattern = "\\(", replacement = "_") 
  names(accData) <- gsub(x = names(accData), pattern = "\\)", replacement = "_") 
  names(accData) <- gsub(x = names(accData), pattern = "\\?", replacement = "_") 
  #print(head(accData))
  
  # ##############################################################
  # #### timestamp in AU time zone, force tz if file is saved as Posix
  # ##############################################################
  options(digits.secs=9)
  
  if(class(accData$Timestamp[1])=="POSIXct" | class(accData$Timestamp[1])=="POSIXt"){
  
  print("force tz")  
  accData$Timestamp <- force_tz(accData$Timestamp, tzone = "Australia/Melbourne")
  accData$DatesPos<-as.POSIXct(accData$Timestamp, format='%Y-%m-%d %H:%M:%OS',tz="Australia/Melbourne")
  print(head(accData))
  print(accData$DatesPos[1])
  }
  
  if(class(accData$Timestamp)=="character"){
  accData$DatesPos<-as.POSIXct(accData$Timestamp, format='%d/%m/%Y %H:%M:%OS',tz="Australia/Melbourne")
  print(head(accData))
  print(accData$DatesPos[1])
  }
  
  ##############################################################
  #### change timestamp to AU time zone
  ##############################################################
  # options(digits.secs=9)
  # accData$DatesPos<-as.POSIXct(accData$Timestamp, format='%d/%m/%Y %H:%M:%OS',tz="UTC")
  # head(accData)
  # 
  # 
  # accData$Timestamp_UTC<-accData$Timestamp
  # accData$Timestamp<-with_tz(accData$DatesPos, tz = "Australia/Melbourne")
  # head(accData)
  # 
  # 
  # options(digits.secs=9)
  # accData$DatesPos<-as.POSIXct(accData$Timestamp, format='%d/%m/%Y %H:%M:%OS',tz="Australia/Melbourne")
  # head(accData)
  # tail(accData)
  ##############################################################
  #### select GPS ID and calculate hunting
  ##############################################################
  unique(accData$TagID)
 
  ##############################################################
  #### select Year, adjust for animal IDs so they match
  
  #----- 2020
  #IDSel<-unique(substr(accData$TagID,1,(nchar(accData$TagID)-3)))

  # ----- 2019,2018
  # accData$TagID<-str_replace(accData$TagID, "_S1", "")
  # IDSel<-unique(accData$TagID)
  # 
  # accData$TagID<-str_replace(accData$TagID, "_S2", "")
  # IDSel<-unique(accData$TagID)
  
  # #----- 2017
  # IDSel<-unique(accData$TagID)
  # if(IDSel=="G_3061_F_628621_20171123(2)") {IDSel<-"G_3061_F_628621_20171123"}
  # 
  # if(IDSel=="PG_3060_F_984776_171215") {
  #   accData$TagID<-"PG_3060_F_984776_20171215"
  #           IDSel<-"PG_3060_F_984776_20171215"}
  # 
  # if(IDSel=="PG_3061_M_629377_171217") {
  #   accData$TagID<-"PG_3061_M_629377_20171217"
  #           IDSel<-"PG_3061_M_629377_20171217"}
  # 
  # if(IDSel=="PG_3082_F_978702_171220") {
  #   accData$TagID<-"PG_3082_F_978702_20171220"
  #   IDSel<-"PG_3082_F_978702_20171220"}
  # 
  # if(IDSel=="PG_3090_M_644978_171218") {
  #   accData$TagID<-"PG_3090_M_644978_20171218"
  #   IDSel<-"PG_3090_M_644978_20171218"}
  # 
  # if(IDSel=="PG_3098_M_453900_171214") {
  #   accData$TagID<-"PG_3098_M_453900_20171214"
  #   IDSel<-"PG_3098_M_453900_20171214"}
  # 
  # if(IDSel=="G_4017_M_604841_20171123(2)") {IDSel<-"G_4017_M_604841_20171123"}
  # 
  # if(IDSel=="PG_PG_4026_F_985052_171220") {
  #   accData$TagID<-"PG_4026_F_985052_20171220"
  #   IDSel<-"PG_4026_F_985052_20171220"}
  # 
  # #----- 2016
  # IDSel<-unique(accData$TagID)
  # if(IDSel=="G3013F_9848131_20161005") {
  #   accData$TagID<-"G3013F_984813_20161005"
  #   IDSel<-"G3013F_984813_20161005"}
  # 
  # if(IDSel=="G3102F_118047_20161007") {
  #   accData$TagID<-"G3102F_118047_20161005"
  #   IDSel<-"G3102F_118047_20161005"}
  # 
  # if(IDSel=="G3080F_453724_20161019_S1") {
  #   accData$TagID<-"G3080F_453724_20161019"
  #   IDSel<-"G3080F_453724_20161019"}
  # 
  # if(IDSel=="I3020F_20160927") {
  #   accData$TagID<-"I3020F_978872_20160927"
  #   IDSel<-"I3020F_978872_20160927"}
  # 
  # if(IDSel=="I3122F_628167_20161008") {
  #   accData$TagID<-"I3122F_628167_20161007"
  #   IDSel<-"I3122F_628167_20161007"}
  # 
  # if(IDSel=="I3053M_643814_20161010_2") {
  #   accData$TagID<-"I3053M_643814_20161010"
  #   IDSel<-"I3053M_643814_20161010"}
  # 
  # if(IDSel=="I3114M_20161003") {
  #   accData$TagID<-"I3114M_650790_20161003"
  #   IDSel<-"I3114M_650790_20161003"}
  # 
  # if(IDSel=="PG3023F_978539_20161107_S1") {
  #   accData$TagID<-"PG3023F_978539_2016110"
  #   IDSel<-"PG3023F_978539_2016110"}
  # 
  # if(IDSel=="PG3062F_605430_20161107") {
  #   accData$TagID<-"PG3062F_605430_2016110"
  #   IDSel<-"PG3062F_605430_2016110"}
  # 
  # if(IDSel=="PG3097F_978702_20161110") {
  #   accData$TagID<-"PG3097F_978702_2016111"
  #   IDSel<-"PG3097F_978702_2016111"}
  # 
  # if(IDSel=="PG3103F_628965_20161107") {
  #   accData$TagID<-"PG3103F_628965_2016110"
  #   IDSel<-"PG3103F_628965_2016110"}
  # 
  # if(IDSel=="PG3116M_629321_20161107") {
  #   accData$TagID<-"PG3116M_629321_2016110"
  #   IDSel<-"PG3116M_629321_2016110"}
  # 
  # if(IDSel=="PG8061M_644936_20161108") {
  #   accData$TagID<-"PG8061M_644936_2016110"
  #   IDSel<-"PG8061M_644936_2016110"}

  # #----- 2015
   # accData$TagID<-str_replace(accData$TagID, "_2015_25Hz", "")
   # accData$TagID<-str_replace(accData$TagID, "_1", "")
   # IDSel<-unique(accData$TagID)
   # 
   # if(IDSel=="3065F628212-20151013_G") {
   #   accData$TagID<-"3065F628212-20151011_G"
   #   IDSel<-"3065F628212-20151011_G"}
   # 
   # if(IDSel=="3091F9848042-20151011_G") {
   #   accData$TagID<-"3091F9848042-20151011_"
   #   IDSel<-"3091F9848042-20151011_"}
   # 
   # if(IDSel=="3104M604759-20151005_G") {
   #   accData$TagID<-"3104M604759-20150510_G"
   #   IDSel<-"3104M604759-20150510_G"}
   # 
   # if(IDSel=="3113M603689-2015919_I") {
   #   accData$TagID<-"3113M603689-20150919_I"
   #   IDSel<-"3113M603689-20150919_I"}
   # 
  
   # # #----- 2014
   # IDSel<-unique(accData$TagID)
   # IDSel<-str_replace(IDSel, "PG", "P")
   
   # #----- 2013
  #  IDSel<-unique(accData$TagID)
  # 
  # if(IDSel=="G-1018F") {
  #   accData$TagID<-"G-1018-F"
  #   IDSel<-"G-1018-F"}
  # 
  # if(IDSel=="G-3114-F") {
  #   accData$TagID<-"G-3114-M"
  #   IDSel<-"G-3114-M"}
  # 
  # #----- 2012
  #IDSel<-unique(accData$TagID)
  
  # #----- 2011
  #  IDSel<-unique(accData$TagID)
  # 
  # if(IDSel=="G34023M") {
  #   accData$TagID<-"G4023M"
  #   IDSel<-"G4023M"}
  # #----- 2010
  IDSel<-unique(accData$TagID)
  # #############################################################

  
  yearSel_ID<-yearSel[which(yearSel$bird_ID==IDSel),] ### select matching GPS file
  dim(yearSel_ID)
  print(unique(accData$TagID))
  print(unique(yearSel_ID$bird_ID))
  
  
  yearSel_ID$dt_1<-c(yearSel_ID$dt[2:nrow(yearSel_ID)],(yearSel_ID$dt[nrow(yearSel_ID)]+(60*15))) #add 15 min to the last datapoint
  yearSel_ID$dt_1<-as.POSIXct(yearSel_ID$dt_1, format='%Y-%m-%d %H:%M:%S',tz="Australia/Melbourne")
  
  yearSel_ID$timeDiff<-difftime(yearSel_ID$dt_1,yearSel_ID$dt,units="sec")
  yearSel_ID[,c("dt","dt_1","ID","timeDiff")]
  #plot(yearSel_ID$mu.x, yearSel_ID$mu.y, type="b")
  
  yearSel_ID$dt_11<-ifelse(yearSel_ID$timeDiff>900, #if there are multiple chunks
                          yearSel_ID$dt_11<-as.character(yearSel_ID$dt+(60*15)),
                          yearSel_ID$dt_11<-as.character(yearSel_ID$dt_1))
 
  yearSel_ID$dt_11<-as.POSIXct(yearSel_ID$dt_11, format='%Y-%m-%d %H:%M:%S',tz="Australia/Melbourne")
  yearSel_ID<-within(yearSel_ID,rm("dt_1"))
  yearSel_ID[,c("dt","dt_11","ID","timeDiff")]
  
  ###maybe select what is within the acc to don't have duplicates
  
  for(ii in 1:nrow(yearSel_ID)){
    
    timeWin<-yearSel_ID[ii,]
    timeWin$dt_1<-as.POSIXct(timeWin$dt_1, format='%Y-%m-%d %H:%M:%S',tz="Australia/Melbourne")
    timeWin$dt_11<-as.POSIXct(timeWin$dt_11, format='%Y-%m-%d %H:%M:%S',tz="Australia/Melbourne")
    
    subAcc<-accData[which(accData$DatesPos>=timeWin$dt & accData$DatesPos<timeWin$dt_11),]
    #print(dim(subAcc))
    #huntTime<-nrow(subAcc[which(subAcc$StatesNames=="Hunting"),]) #forEM
    huntTime<-nrow(subAcc[which(subAcc$Pred_RF=="Hunting"),]) #for RF
    #print(huntTime)
    huntTime<-ifelse(is.null(huntTime),huntTime<-0,huntTime<-huntTime)
    yearSel_ID$HuntingTime_sec[ii]<-huntTime/25 #time hunting in sec
    yearSel_ID$LengthInt_sec[ii]<-nrow(subAcc)/25

    meanDepth<-mean(subAcc$depth25Hz[which(subAcc$Sur_NotSur==1)]) #select diving part only
    meanDepth<-ifelse(is.na(meanDepth),meanDepth<-0,meanDepth<-meanDepth)
    yearSel_ID$meanDivingDepth_m[ii]<-meanDepth

    maxDepth<-max(subAcc$depth25Hz[which(subAcc$Sur_NotSur==1)]) #select diving part only
    maxDepth<-ifelse(is.infinite(maxDepth),maxDepth<-0,maxDepth<-maxDepth)
    yearSel_ID$maxDivingDepth_m[ii]<-maxDepth
    
    
    SubSurface<-subAcc[which(subAcc$Sur_NotSur==0),]#select subsurface part only
    #SubSurface<-SubSurface[which(SubSurface$StatesNames=="Swim/Porpoise"  |  SubSurface$StatesNames=="Rest"),]
    SubSurface<-SubSurface[which(SubSurface$Pred_RF=="Swim/Porpoise"  |  SubSurface$Pred_RF=="Rest"),]
    meanVeDBASwim<-mean(SubSurface$VeDBA) 
    yearSel_ID$meanVeDBASubSwimming[ii]<-meanVeDBASwim
    
    sdVeDBASwim<-sd(SubSurface$VeDBA) 
    yearSel_ID$sdVeDBASubSwimming[ii]<-sdVeDBASwim
    
  }
  # as there might be several chunks, hunting would be 0 and length of interval also 0 as there would be no match
  #between acc and GPS. GPS has trips/chunks within 1 file. acc has different files for each trip.
 # print(TripID)
  yearSel_ID<-subset(yearSel_ID, yearSel_ID$LengthInt_sec>0)
  print(yearSel_ID$HuntingTime_sec)
  print(yearSel_ID$LengthInt_sec)
  ListHuntingDF[[TripID]]<-yearSel_ID

}

HuntingDF <- ldply (ListHuntingDF, data.frame)
head(HuntingDF)

plot(HuntingDF$HuntingTime_sec~HuntingDF$maxDivingDepth_m)
boxplot(HuntingDF$maxDivingDepth_m)

write.csv(HuntingDF,"/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/AccGPS_matchDF_2010.csv", row.names = FALSE)

############### ############### ############### ############### ###############
############### read data and map
############### ############### ############### ############### ############### 
rm(list = ls())

library(sp)
library(raster)
library(data.table)
library(rasterVis)
library(stars)
library(terra)
library(tidyr)
library(dplyr)
library(viridis)
library(ggpubr)
library("scales")
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(zoo)
library(caTools)
library(tidyverse)
library(lubridate)
library(plyr)
library(smoother)
library("ggsci")
library("momentuHMM")
library(geosphere)
library("ggspatial")
options(digits.secs=9)




LP_2020<-fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/AccGPS_matchDF_2020.csv", header = TRUE)
LP_2019<-fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/AccGPS_matchDF_2019.csv", header = TRUE)
LP_2018<-fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/AccGPS_matchDF_2018.csv", header = TRUE)
LP_2017<-fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/AccGPS_matchDF_2017.csv", header = TRUE)
LP_2016<-fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/AccGPS_matchDF_2016.csv", header = TRUE)
LP_2015<-fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/AccGPS_matchDF_2015.csv", header = TRUE)
LP_2014<-fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/AccGPS_matchDF_2014.csv", header = TRUE)
LP_2013<-fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/AccGPS_matchDF_2013.csv", header = TRUE)
LP_2012<-fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/AccGPS_matchDF_2012.csv", header = TRUE)
LP_2011<-fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/AccGPS_matchDF_2011.csv", header = TRUE)
LP_2010<-fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/AccGPS_matchDF_2010.csv", header = TRUE)

LPAcc_all<-rbind(LP_2020,LP_2019,LP_2018,LP_2017,LP_2016,LP_2015,LP_2014,LP_2013,LP_2012,LP_2011,LP_2010)
head(LPAcc_all)
dim(LPAcc_all)


LPAcc_all$bird_IDYear<-paste(LPAcc_all$bird_ID,LPAcc_all$season,sep="_")
LPAcc_all$bird_IDYear<-as.factor(LPAcc_all$bird_IDYear)
length(unique(LPAcc_all$bird_IDYear))


LPAcc_all$dt <- force_tz(LPAcc_all$dt , tzone = "Australia/Melbourne")
LPAcc_all$dtGMT<-with_tz(LPAcc_all$dt, tz = "GMT")
LPAcc_all$GMT_Day<-substr(LPAcc_all$dtGMT,1,10)

#### remove not ok trips
#bla<-LPAcc_all[which(LPAcc_all$ID=="I-3002-M_8_2014"),]
#bla1<-LPAcc_all[which(LPAcc_all$ID=="I-3113-F_1_2014"),]
#bla2<-LPAcc_all[which(LPAcc_all$ID=="PG_4026_F_985052_20171220_3_2017"),]
LPAcc_all<-LPAcc_all[-which(LPAcc_all$ID=="G3051F_2011"),]

#calculate step length
LittleGPSdata_prep<-prepData(LPAcc_all[,c("ID","mu.x","mu.y")],coordNames =c("mu.x","mu.y") )
hist(LittleGPSdata_prep$step/1000, breaks=50, xlab=("step length (Km)"),main="")

LPAcc_all$step<-LittleGPSdata_prep$step
#check 
LPAcc_allID<-LPAcc_all[which(LPAcc_all$ID=="G3002MP19_2020"),]

LPAcc_all$angle<-LittleGPSdata_prep$angle
LPAcc_all$angleDeg<-LittleGPSdata_prep$angle*(180/pi)

#### get world and set projection
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ESPG <- x<-rgdal::make_EPSG()
myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
proj.aeqd <- myESPG$prj4

#Conversion of data frame to sf object
df_sf <- st_as_sf(x = LPAcc_all,                         
                  coords = c("mu.x", "mu.y"),
                  crs = proj.aeqd) #"+proj=utm +zone=55 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "

#Projection transformation
sfc = st_transform(df_sf, crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

CorrdLatLon<-st_coordinates(sfc) 
head(CorrdLatLon)
colnames(CorrdLatLon)<-c("Lon","Lat")

#calculate bearing
LPAcc_all$Lon<-CorrdLatLon[,1]
LPAcc_all$Lat<-CorrdLatLon[,2]


listLP_all<-list()
uniqueChunks<-unique(LPAcc_all$ID)
for(i in 1:length(uniqueChunks)){
  LPAcc_all_sub<-LPAcc_all[which(LPAcc_all$ID==uniqueChunks[i]),]
  
  points1Little<-matrix(nrow = nrow(LPAcc_all_sub), ncol = 2)
  points1Little[,1]<- c(NA,LPAcc_all_sub$Lon[1:(nrow(LPAcc_all_sub)-1)])
  points1Little[,2]<- c(NA,LPAcc_all_sub$Lat[1:(nrow(LPAcc_all_sub)-1)])
  
  points2Little<-matrix(nrow = nrow(LPAcc_all_sub), ncol = 2)
  points2Little[,1]<- LPAcc_all_sub$Lon
  points2Little[,2]<- LPAcc_all_sub$Lat
  
  BearingAngle<-geosphere::bearing(points1Little, points2Little)
  LPAcc_all_sub$BearingAngle<-(BearingAngle + 360) %% 360
#  LPAcc_all_sub$BearingAngle<-BearingAngle 
  listLP_all[[i]]<-LPAcc_all_sub
  
  
}

LPAcc_all <- ldply(listLP_all, data.frame)

summary(LPAcc_all$BearingAngle)
hist(LPAcc_all$BearingAngle,breaks=50)
LPAcc_all$hourAU<-as.factor(substr(LPAcc_all$dt,12,13))
unique(LPAcc_all$hourAU)

LPOrientation<-LPAcc_all %>%
               group_by(hourAU) %>%
               dplyr::summarize(mean = mean(BearingAngle,na.rm=T),sd = sd(BearingAngle,na.rm=T))



d2c.2 <- function(x) {
  upper <- seq(from = 11.25, by = 22.5, length.out = 17)
  card1 <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')
  ifelse(x>360 | x<0,NA,card1[findInterval(x,upper,rightmost.closed = T)+1])
}

LPAcc_all$BearingAngleCard<-d2c.2(LPAcc_all$BearingAngle)

UniqueForID<-tapply(LPAcc_all$ID,LPAcc_all$stage_season, unique)

UniqueNestID<-tapply(LPAcc_all$bird_ID,LPAcc_all$stage_season, unique)

length(unique(LPAcc_all$bird_ID))
length(unique(LPAcc_all$bird_IDYear))

for(i in 1:length(UniqueNestID)){
print(length(UniqueForID[[i]]))
print(length(UniqueNestID[[i]]))
}

table(LPAcc_all$stage_season)


########################################################
################ maps
########################################################

sfc$stage1<-factor(sfc$stage, levels = c("I", "G", "P"))

maxValue<-max(sfc$HuntingTime_sec)

mapHunting<-ggplot(data = world) +
  geom_sf() +
  geom_sf(data = sfc, aes(size=HuntingTime_sec,fill=as.factor(stage1)),alpha=0.6,shape=21) +
  coord_sf(xlim = c(144.9, 146), ylim = c(-40, -38.4), expand = TRUE)+
  scale_x_continuous(breaks = seq(145.0, 146, by = 0.3)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), name="Stage", labels=c("Incubation","Guard","Post Guard"))+
  guides(fill = guide_legend(override.aes = list(size = 5)))+
  scale_size_continuous(limits=c(0,maxValue),breaks=seq(0,maxValue,100),range=c(1,10), name= "Hunting duration (sec)")+
  facet_wrap(vars(season),ncol=6)+
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
        #legend.key.height = unit(1, 'cm'), #change legend key height
        #legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=15),
        axis.text = element_text(size=15)) + #change legend text font size
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) 

mapHunting

HuntingOnly<-LPAcc_all[which(LPAcc_all$maxDivingDepth_m>0 & LPAcc_all$LengthInt_sec>=900),]
HuntingOnly$stage1<-factor(HuntingOnly$stage, levels = c("I", "G", "P"))

# 
# plotmaxDepth<-ggplot()+
#   geom_violin(data=HuntingOnly,aes(y=maxDivingDepth_m,x=as.factor(season)))+
#   geom_boxplot(data=HuntingOnly,aes(y=maxDivingDepth_m,x=as.factor(season)),width=0.1,fill="gray50")+
#   ylab("max diving depth (m)")+xlab("")+
#   theme(#legend.key.size = unit(8, 'cm'), #change legend key size
#   #legend.key.height = unit(1, 'cm'), #change legend key height
#   #legend.key.width = unit(1, 'cm'), #change legend key width
#   legend.title = element_text(size=15), #change legend title font size
#   legend.text = element_text(size=15),
#   axis.text = element_text(size=15),
#   text= element_text(size=15))
# 
# #plotmaxDepth
# 
# 
# #create overlaying density plots
# plotmaxDepthDens<-ggplot()+
#   geom_density(data=HuntingOnly, aes(x=maxDivingDepth_m, fill=as.factor(ID)),alpha=.25)+
#   xlab("max diving depth (m)")+ylab("density")+
#   facet_wrap(vars(season),ncol=4)+
#   theme(legend.position = "none") +      
#   theme(#legend.key.size = unit(8, 'cm'), #change legend key size
#     #legend.key.height = unit(1, 'cm'), #change legend key height
#     #legend.key.width = unit(1, 'cm'), #change legend key width
#     legend.title = element_text(size=15), #change legend title font size
#     legend.text = element_text(size=15),
#     axis.text = element_text(size=15),
#     text= element_text(size=15))
# 
# plotmaxDepthDens
# 
# 
# #create overlaying density plots
# plotmaxHuntDens<-ggplot()+
#   geom_density(data=HuntingOnly, aes(x=HuntingTime_sec, fill=as.factor(ID)),alpha=.25)+
#   xlab("time spent hunting (sec)")+ylab("density")+
#   facet_wrap(vars(season),ncol=4)+
#   theme(legend.position = "none") +      
#   theme(#legend.key.size = unit(8, 'cm'), #change legend key size
#     #legend.key.height = unit(1, 'cm'), #change legend key height
#     #legend.key.width = unit(1, 'cm'), #change legend key width
#     legend.title = element_text(size=15), #change legend title font size
#     legend.text = element_text(size=15),
#     axis.text = element_text(size=15),
#     text= element_text(size=15))
# 
# plotmaxHuntDens
# 

#create overlaying density plots

tapply(HuntingOnly$HuntingTime_sec, HuntingOnly$stage1, mean)
tapply(HuntingOnly$HuntingTime_sec, HuntingOnly$stage1, sd)

tapply(HuntingOnly$maxDivingDepth_m, HuntingOnly$stage1, mean)
tapply(HuntingOnly$maxDivingDepth_m, HuntingOnly$stage1, sd)

tapply(HuntingOnly$HuntingTime_sec, HuntingOnly$season, mean)
tapply(HuntingOnly$HuntingTime_sec, HuntingOnly$season, sd)

tapply(HuntingOnly$maxDivingDepth_m, HuntingOnly$season, mean)
tapply(HuntingOnly$maxDivingDepth_m, HuntingOnly$season, sd)

tapply(HuntingOnly$maxDivingDepth_m, HuntingOnly$season, max)


HuntingOnly$season<- as.factor(HuntingOnly$season)
# Compute the analysis of variance
res.aov <- aov(HuntingTime_sec ~ season, data = HuntingOnly)
# Summary of the analysis
summary(res.aov)
TukeyHSD(res.aov)
plot(res.aov, 1)
plot(res.aov, 2)
# Extract the residuals



plotmaxHuntDepth<-ggplot()+
  geom_point(data=HuntingOnly, aes(x=maxDivingDepth_m,y=HuntingTime_sec, fill=as.factor(stage1)),colour="gray55",alpha=.7,pch=21)+
  ylab("time spent hunting (sec)")+xlab("max diving depth (m)")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), name="Stage: ", labels=c("Incubation","Guard","Post Guard"))+
  facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +  
  guides(fill = guide_legend(override.aes = list(size=5)))+
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=15),
    text= element_text(size=15))

plotmaxHuntDepth

# 
# 
# ploHuntTime<-ggplot()+
#   geom_violin(data=HuntingOnly,aes(y=HuntingTime_sec,x=as.factor(season)))+ 
#   geom_boxplot(data=HuntingOnly,aes(y=HuntingTime_sec,x=as.factor(season)),width=0.1,fill="gray50")+
#   ylab("time spent hunting (sec)")+xlab("")+
#   theme(#legend.key.size = unit(8, 'cm'), #change legend key size
#     #legend.key.height = unit(1, 'cm'), #change legend key height
#     #legend.key.width = unit(1, 'cm'), #change legend key width
#     legend.title = element_text(size=15), #change legend title font size
#     legend.text = element_text(size=15),
#     axis.text = element_text(size=15),
#     text= element_text(size=15))
# 
# #ploHuntTime
# 
# 
# ggarrange(plotmaxDepth,ploHuntTime,ncol=1)
# 
# 
# G_Foraging<-LPAcc_all[which(LPAcc_all$stage=="I"),]
# summary(G_Foraging)
# 
# plotmaxDepth<-ggplot()+
#   geom_violin(data=LPAcc_all,aes(y=maxDivingDepth_m,x=as.factor(season)))+ 
#   geom_boxplot(data=LPAcc_all,aes(y=maxDivingDepth_m,x=as.factor(season)),width=0.1,fill="gray50")+
#   ylab("max diving depth (m)")+xlab("")+
#   theme(#legend.key.size = unit(8, 'cm'), #change legend key size
#     #legend.key.height = unit(1, 'cm'), #change legend key height
#     #legend.key.width = unit(1, 'cm'), #change legend key width
#     legend.title = element_text(size=15), #change legend title font size
#     legend.text = element_text(size=15),
#     axis.text = element_text(size=15),
#     text= element_text(size=15))
# 
# #plotmaxDepth
# 
# 
# ploHuntTime<-ggplot()+
#   geom_violin(data=LPAcc_all,aes(y=HuntingTime_sec,x=as.factor(season)))+ 
#   geom_boxplot(data=LPAcc_all,aes(y=HuntingTime_sec,x=as.factor(season)),width=0.1,fill="gray50")+
#   ylab("time spent hunting (sec)")+xlab("")+
#   theme(#legend.key.size = unit(8, 'cm'), #change legend key size
#     #legend.key.height = unit(1, 'cm'), #change legend key height
#     #legend.key.width = unit(1, 'cm'), #change legend key width
#     legend.title = element_text(size=15), #change legend title font size
#     legend.text = element_text(size=15),
#     axis.text = element_text(size=15),
#     text= element_text(size=15))
# 
# #ploHuntTime
# 
# 
# ggarrange(plotmaxDepth,ploHuntTime,ncol=2)
# 

###############################
#create rasters

head(LPAcc_all)
summary(LPAcc_all$mu.x)
summary(LPAcc_all$mu.y)


#boxRaster
xMin<-1100000 #min(LPAcc_all_sub$mu.x)-200000 #add 10 Km on each side
xMax<-1250000 #max(LPAcc_all_sub$mu.x)+200000
  
yMin<- -4450000   #min(LPAcc_all_sub$mu.y)-200000 #add 5 Km on each side
yMax<- -4250000
# create a raster object
r_obj <- raster(vals=NA,xmn=xMin, xmx=xMax, ymn= yMin, ymx= yMax, resolution=c(10000,10000))


HuntingOnly<-LPAcc_all[which(LPAcc_all$maxDivingDepth_m>0 & LPAcc_all$LengthInt_sec>=900),]
#HuntingOnly<-LPAcc_all[which(LPAcc_all$LengthInt_sec>=900),]
#dim(LPAcc_allV2)
##############################################################
############################### Yearly rasters
##############################################################

seasonList<-unique(HuntingOnly$Year)

rasterListHunt<-stack()
rasterListHuntSD<-stack()
rasterListHuntSUM<-stack()
rasterListDepth<-stack()
rasterListDepthSD<-stack()
rasterListCount<-stack()
rasterListCountPoint<-stack()
rasterListStep<-stack()

for (i in 1 :length(seasonList)){

LPAcc_all_sub<-HuntingOnly[which(HuntingOnly$Year==seasonList[i]),]
xy<-as.data.frame(LPAcc_all_sub[,c("mu.x","mu.y")])
vals<-LPAcc_all_sub$HuntingTime_sec
print(summary(vals))
# use rasterize to create desired raster
LP_raster <- rasterize(x=xy, # lon-lat data
                    y=r_obj, # raster object
                    field=vals, # vals to fill raster with
                    fun=median) # aggregate function
LP_raster@data@names<-paste("Season_",as.character(seasonList[i]),sep="")
rasterListHunt<-stack(LP_raster,rasterListHunt)

# use rasterize to create desired raster
LP_rasterSD <- rasterize(x=xy, # lon-lat data
                       y=r_obj, # raster object
                       field=vals, # vals to fill raster with
                       fun=sd) # aggregate function
LP_rasterSD@data@names<-paste("Season_",as.character(seasonList[i]),sep="")
rasterListHuntSD<-stack(LP_rasterSD,rasterListHuntSD)

# use rasterize to create desired raster
LP_rasterSUM <- rasterize(x=xy, # lon-lat data
                         y=r_obj, # raster object
                         field=vals, # vals to fill raster with
                         fun=sum) # aggregate function
LP_rasterSUM@data@names<-paste("Season_",as.character(seasonList[i]),sep="")
rasterListHuntSUM<-stack(LP_rasterSUM,rasterListHuntSUM)

# use rasterize to create desired raster
LP_CountPoints <- rasterize(x=xy, # lon-lat data
                       y=r_obj, # raster object
                       field=vals, # vals to fill raster with
                       fun='count') # aggregate function
LP_CountPoints@data@names<-paste("Season_",as.character(seasonList[i]),sep="")
rasterListCountPoint<-stack(LP_CountPoints,rasterListCountPoint)

vals<-LPAcc_all_sub$maxDivingDepth_m
summary(vals)
# use rasterize to create desired raster
LP_Depthraster <- rasterize(x=xy, # lon-lat data
                       y=r_obj, # raster object
                       field=vals, # vals to fill raster with
                       fun=median) # aggregate function
LP_Depthraster@data@names<-paste("Season_",as.character(seasonList[i]),sep="")
rasterListDepth<-stack(LP_Depthraster,rasterListDepth)

# use rasterize to create desired raster
LP_DepthrasterSD <- rasterize(x=xy, # lon-lat data
                            y=r_obj, # raster object
                            field=vals, # vals to fill raster with
                            fun=sd) # aggregate function
LP_DepthrasterSD@data@names<-paste("Season_",as.character(seasonList[i]),sep="")
rasterListDepthSD<-stack(LP_DepthrasterSD,rasterListDepthSD)

rCount <- raster(vals=NA,xmn=xMin, xmx=xMax, ymn= yMin, ymx= yMax, resolution=c(10000,10000))
#LPAcc_all_sub[, rastercell := cellFromXY(r_obj, .SD[,.(LPAcc_all_sub$mu.x, LPAcc_all_sub$mu.y)])]
LPAcc_all_sub$rastercell <- cellFromXY(rCount, LPAcc_all_sub[,c("mu.x","mu.y")])
# aggregate how many unique categories there are in each raster cell
data_count_1 <- aggregate(data = LPAcc_all_sub,                # Applying aggregate
                          bird_IDYear ~ rastercell,
                          function(x) length(unique(x)))
# update the values of the raster with these values
rCount[data_count_1[, "rastercell"]] <- data_count_1[, "bird_IDYear"] 

rCount@data@names<-paste("Season_",as.character(seasonList[i]),sep="")
rasterListCount<-stack(rCount,rasterListCount)


vals<-LPAcc_all_sub$step
summary(vals)
# use rasterize to create desired raster
LP_Step_raster <- rasterize(x=xy, # lon-lat data
                       y=r_obj, # raster object
                       field=vals, # vals to fill raster with
                       fun=median,
                       na.rm=TRUE) # aggregate function
LP_Step_raster@data@names<-paste("Season_",as.character(seasonList[i]),sep="")
rasterListStep<-stack(LP_Step_raster,rasterListStep)

}

################ adjust projections

ESPG <- x<-rgdal::make_EPSG()
myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
proj.aeqd <- myESPG$prj4

crs(rasterListHunt) <- proj.aeqd
rasterListHunt_LatLon <- projectRaster(rasterListHunt,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListHunt)
 plot(rasterListHunt_LatLon)

crs(rasterListHuntSD) <- proj.aeqd
rasterListHuntSD_LatLon <- projectRaster(rasterListHuntSD,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(rasterListHuntSD)
 plot(rasterListHuntSD_LatLon)

 
 crs(rasterListHuntSUM) <- proj.aeqd
 rasterListHuntSUM_LatLon <- projectRaster(rasterListHuntSUM,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
 plot(rasterListHuntSUM)
 plot(rasterListHuntSUM_LatLon)
 
 
crs(rasterListDepth) <- proj.aeqd
rasterListDepth_LatLon <- projectRaster(rasterListDepth,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListDepth)
# plot(rasterListDepth_LatLon)

crs(rasterListDepthSD) <- proj.aeqd
rasterListDepthSD_LatLon <- projectRaster(rasterListDepthSD,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListDepthSD)
# plot(rasterListDepthSD_LatLon)


crs(rasterListCount) <- proj.aeqd
rasterListCount_LatLon <- projectRaster(rasterListCount,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListCount)
# plot(rasterListCount_LatLon)

crs(rasterListCountPoint) <- proj.aeqd
rasterListCountPoint_LatLon <- projectRaster(rasterListCountPoint,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListCountPoint)
# plot(rasterListCountPoint_LatLon)

crs(rasterListStep) <- proj.aeqd
rasterListStep_LatLon <- projectRaster(rasterListStep,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(rasterListStep)
# plot(rasterListCountPoint_LatLon)


################ write rasters
library(terra)

rasterListHunt<-rast(rasterListHunt)
names(rasterListHunt) 
terra::writeRaster(rasterListHunt, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Year/Hunting_Year.tif",overwrite=TRUE)

rasterListHuntSD<-rast(rasterListHuntSD)
names(rasterListHuntSD) 
terra::writeRaster(rasterListHuntSD, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Year/HuntingSD_Year.tif",overwrite=TRUE)

rasterListHuntSUM<-rast(rasterListHuntSUM)
names(rasterListHuntSUM) 
terra::writeRaster(rasterListHuntSUM, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Year/HuntingSUM_Year.tif",overwrite=TRUE)

rasterListDepth<-rast(rasterListDepth)
names(rasterListDepth) 
terra::writeRaster(rasterListDepth, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Year/Depth_Year.tif",overwrite=TRUE)

rasterListDepthSD<-rast(rasterListDepthSD)
names(rasterListDepthSD) 
terra::writeRaster(rasterListDepthSD, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Year/DepthSD_Year.tif",overwrite=TRUE)

rasterListCount<-rast(rasterListCount)
names(rasterListCount) 
terra::writeRaster(rasterListCount, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Year/IDCount_Year.tif",overwrite=TRUE)

rasterListCountPoint<-rast(rasterListCountPoint)
names(rasterListCountPoint) 
terra::writeRaster(rasterListCountPoint, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Year/PointCount_Year.tif",overwrite=TRUE)

rasterListStep<-rast(rasterListStep)
names(rasterListStep) 
terra::writeRaster(rasterListStep, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Year/StepLength_Year.tif",overwrite=TRUE)

################ create df and plot

r_stack_df <- as.data.frame(rasterListHuntSUM_LatLon, xy = TRUE) %>% 
  tidyr::pivot_longer(cols = !c(x, y), 
                      names_to = 'variable', 
                      values_to = 'value')


season.labs <- c("2010", "2011", "2012","2013","2014","2015","2016","2017","2018","2019","2020") #what i want to appear
names(season.labs) <- unique(r_stack_df$variable)

summary(r_stack_df$value)
hist(r_stack_df$value,breaks=50)
MinVal<-round(min(r_stack_df$value,na.rm=T))
MaxVal<-round(max(r_stack_df$value,na.rm=T))

mapHuntingRaster<-ggplot(data = world) +
  geom_sf() +
  geom_raster(data = r_stack_df, 
              aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(breaks=seq(0,MaxVal,5000),na.value=NA,name= "sum hunting (sec)",option="A",direction=-1) +
  coord_sf(xlim = c(144.9, 146), ylim = c(-40, -38.4), expand = TRUE)+
  scale_x_continuous(breaks = seq(145.0, 146, by = 0.3)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #guides(fill = guide_legend(override.aes = list(size = 5)))+
  xlab("")+ylab("")+ theme(legend.position="right")+
#  scale_size_continuous(limits=c(0,maxValue),breaks=seq(0,maxValue,100),range=c(1,10), name= "Hunting duration (sec)")+
  facet_wrap(~ variable,labeller = labeller(variable=season.labs))+ 
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=15)) + #change legend text font size
  annotation_scale(location = "bl", width_hint = 0.5) #+
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) 

mapHuntingRaster


rDepth_stack_df <- as.data.frame(rasterListDepth_LatLon, xy = TRUE) %>% 
  tidyr::pivot_longer(cols = !c(x, y), 
                      names_to = 'variable', 
                      values_to = 'value')


season.labs <- c("2010", "2011", "2012","2013","2014","2015","2016","2017","2018","2019","2020") #what i want to appear
names(season.labs) <- unique(rDepth_stack_df$variable)

summary(rDepth_stack_df$value)
hist(rDepth_stack_df$value,breaks=50)
MinVal<-round(min(rDepth_stack_df$value,na.rm=T))
MaxVal<-round(max(rDepth_stack_df$value,na.rm=T))

mapDepthRaster<-ggplot(data = world) +
  geom_sf() +
  geom_raster(data = rDepth_stack_df, 
              aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(breaks=seq(MinVal,MaxVal,10),na.value=NA,name= "av. max depth (m)",option="A",direction=-1) +
  coord_sf(xlim = c(144.9, 146), ylim = c(-40, -38.4), expand = TRUE)+
  scale_x_continuous(breaks = seq(145.0, 146, by = 0.3)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #guides(fill = guide_legend(override.aes = list(size = 5)))+
  xlab("")+ylab("")+ theme(legend.position="right")+
  #  scale_size_continuous(limits=c(0,maxValue),breaks=seq(0,maxValue,100),range=c(1,10), name= "Hunting duration (sec)")+
  facet_wrap(~ variable,labeller = labeller(variable=season.labs))+ 
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=15)) + #change legend text font size
  annotation_scale(location = "bl", width_hint = 0.5) #+
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) 

mapDepthRaster


rCount_stack_df <- as.data.frame(rasterListCount_LatLon, xy = TRUE) %>% 
  tidyr::pivot_longer(cols = !c(x, y), 
                      names_to = 'variable', 
                      values_to = 'value')

season.labs <- c("2010", "2011", "2012","2013","2014","2015","2016","2017","2018","2019","2020") #what i want to appear
names(season.labs) <- unique(rCount_stack_df$variable)

summary(rCount_stack_df$value)
hist(rCount_stack_df$value,breaks=50)
MinVal<-round(min(rCount_stack_df$value,na.rm=T))
MaxVal<-round(max(rCount_stack_df$value,na.rm=T))

mapIDCountRaster<-ggplot(data = world) +
  geom_sf() +
  geom_raster(data = rCount_stack_df, 
              aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(breaks=seq(MinVal,MaxVal,7),na.value=NA,name= "n unique ID",option="A",direction=-1) +
  coord_sf(xlim = c(144.9, 146), ylim = c(-40, -38.4), expand = TRUE)+
  scale_x_continuous(breaks = seq(145.0, 146, by = 0.3)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #guides(fill = guide_legend(override.aes = list(size = 5)))+
  xlab("")+ylab("")+ theme(legend.position="right")+
  #  scale_size_continuous(limits=c(0,maxValue),breaks=seq(0,maxValue,100),range=c(1,10), name= "Hunting duration (sec)")+
  facet_wrap(~ variable,labeller = labeller(variable=season.labs))+ 
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=15)) + #change legend text font size
  annotation_scale(location = "bl", width_hint = 0.5) #+
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) 

mapIDCountRaster


rCountPoint_stack_df <- as.data.frame(rasterListCountPoint_LatLon, xy = TRUE) %>% 
  tidyr::pivot_longer(cols = !c(x, y), 
                      names_to = 'variable', 
                      values_to = 'value')

season.labs <- c("2010", "2011", "2012","2013","2014","2015","2016","2017","2018","2019","2020") #what i want to appear
names(season.labs) <- unique(rCountPoint_stack_df$variable)

summary(rCountPoint_stack_df$value)
hist(rCountPoint_stack_df$value,breaks=50)
MinVal<-round(min(rCountPoint_stack_df$value,na.rm=T))
MaxVal<-round(max(rCountPoint_stack_df$value,na.rm=T))

mapIDCountPointRaster<-ggplot(data = world) +
  geom_sf() +
  geom_raster(data = rCountPoint_stack_df, 
              aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(breaks=seq(0,MaxVal,200),na.value=NA,name= "n points",option="A",direction=-1) +
  coord_sf(xlim = c(144.9, 146), ylim = c(-40, -38.4), expand = TRUE)+
  scale_x_continuous(breaks = seq(145.0, 146, by = 0.3)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #guides(fill = guide_legend(override.aes = list(size = 5)))+
  xlab("")+ylab("")+ theme(legend.position="right")+
  #  scale_size_continuous(limits=c(0,maxValue),breaks=seq(0,maxValue,100),range=c(1,10), name= "Hunting duration (sec)")+
  facet_wrap(~ variable,labeller = labeller(variable=season.labs))+ 
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=10),
    axis.text = element_text(size=15)) + #change legend text font size
  annotation_scale(location = "bl", width_hint = 0.5) #+
  # annotation_north_arrow(location = "bl", which_north = "true", 
  #                        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                        style = north_arrow_fancy_orienteering) 

mapIDCountPointRaster



##############################################################
############################### Daily-Stage rasters
##############################################################

#create rasters

head(LPAcc_all)
summary(LPAcc_all$mu.x)
summary(LPAcc_all$mu.y)


#boxRaster
xMin<-1100000 #min(LPAcc_all_sub$mu.x)-200000 #add 10 Km on each side
xMax<-1250000 #max(LPAcc_all_sub$mu.x)+200000

yMin<- -4450000   #min(LPAcc_all_sub$mu.y)-200000 #add 5 Km on each side
yMax<- -4250000
# create a raster object
r_obj <- raster(vals=NA,xmn=xMin, xmx=xMax, ymn= yMin, ymx= yMax, resolution=c(10000,10000))


HuntingOnly<-LPAcc_all[which(LPAcc_all$maxDivingDepth_m>0 & LPAcc_all$LengthInt_sec>=900),]
#HuntingOnly<-LPAcc_all


seasonList<-unique(HuntingOnly$stage)

rasterListHunt<-stack()
rasterListHuntSUM<-stack()
rasterListDepth<-stack()
rasterListCount<-stack()
rasterListCountPoint<-stack()
rasterListStep<-stack()

for (i in 1 :length(seasonList)){

  LPAcc_all_sub<-HuntingOnly[which(HuntingOnly$stage==seasonList[i]),]
  DayList<-unique(LPAcc_all_sub$GMT_Day)
  
  for(qq in 1:length(DayList)){
    
  LPAcc_all_subDay<-LPAcc_all_sub[which(LPAcc_all_sub$GMT_Day==DayList[qq]),]
  xy<-as.data.frame(LPAcc_all_subDay[,c("mu.x","mu.y")])
  vals<-LPAcc_all_subDay$HuntingTime_sec
  #print(summary(vals))
  # use rasterize to create desired raster
  LP_raster <- rasterize(x=xy, # lon-lat data
                         y=r_obj, # raster object
                         field=vals, # vals to fill raster with
                         fun=median) # aggregate function
  LP_raster@data@names<-paste("Stage_",as.character(seasonList[i]),"_Day_",as.character(DayList[qq]),sep="")
  rasterListHunt<-stack(LP_raster,rasterListHunt)


  # use rasterize to create desired raster
  LP_rasterSUM <- rasterize(x=xy, # lon-lat data
                            y=r_obj, # raster object
                            field=vals, # vals to fill raster with
                            fun=sum) # aggregate function
  LP_rasterSUM@data@names<-paste("Stage_",as.character(seasonList[i]),"_Day_",as.character(DayList[qq]),sep="")
  rasterListHuntSUM<-stack(LP_rasterSUM,rasterListHuntSUM)

  # use rasterize to create desired raster
  LP_CountPoints <- rasterize(x=xy, # lon-lat data
                              y=r_obj, # raster object
                              field=vals, # vals to fill raster with
                              fun='count') # aggregate function
  LP_CountPoints@data@names<-paste("Stage_",as.character(seasonList[i]),"_Day_",as.character(DayList[qq]),sep="")
  rasterListCountPoint<-stack(LP_CountPoints,rasterListCountPoint)

  vals<-LPAcc_all_subDay$maxDivingDepth_m
  summary(vals)
  # use rasterize to create desired raster
  LP_Depthraster <- rasterize(x=xy, # lon-lat data
                              y=r_obj, # raster object
                              field=vals, # vals to fill raster with
                              fun=median) # aggregate function
  LP_Depthraster@data@names<-paste("Stage_",as.character(seasonList[i]),"_Day_",as.character(DayList[qq]),sep="")
  rasterListDepth<-stack(LP_Depthraster,rasterListDepth)


  rCount <- raster(vals=NA,xmn=xMin, xmx=xMax, ymn= yMin, ymx= yMax, resolution=c(10000,10000))
  #LPAcc_all_sub[, rastercell := cellFromXY(r_obj, .SD[,.(LPAcc_all_sub$mu.x, LPAcc_all_sub$mu.y)])]
  LPAcc_all_sub$rastercell <- cellFromXY(rCount, LPAcc_all_sub[,c("mu.x","mu.y")])
  # aggregate how many unique categories there are in each raster cell
  data_count_1 <- aggregate(data = LPAcc_all_sub,                # Applying aggregate
                            bird_IDYear ~ rastercell,
                            function(x) length(unique(x)))
  # update the values of the raster with these values
  rCount[data_count_1[, "rastercell"]] <- data_count_1[, "bird_IDYear"]

  rCount@data@names<-paste("Stage_",as.character(seasonList[i]),"_Day_",as.character(DayList[qq]),sep="")
  rasterListCount<-stack(rCount,rasterListCount)


  vals<-LPAcc_all_subDay$step
  #summary(vals)
  # use rasterize to create desired raster
  LP_Step_raster <- rasterize(x=xy, # lon-lat data
                              y=r_obj, # raster object
                              field=vals, # vals to fill raster with
                              fun=median,
                              na.rm=TRUE) # aggregate function
  LP_Step_raster@data@names<-paste("Stage_",as.character(seasonList[i]),"_Day_",as.character(DayList[qq]),sep="")
  rasterListStep<-stack(LP_Step_raster,rasterListStep)
  }
}
################ adjust projections

ESPG <- x<-rgdal::make_EPSG()
myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
proj.aeqd <- myESPG$prj4

crs(rasterListHunt) <- proj.aeqd
rasterListHunt_LatLon <- projectRaster(rasterListHunt,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListHunt)
plot(rasterListHunt_LatLon)

crs(rasterListDepth) <- proj.aeqd
rasterListDepth_LatLon <- projectRaster(rasterListDepth,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListDepth)
# plot(rasterListDepth_LatLon)


crs(rasterListCount) <- proj.aeqd
rasterListCount_LatLon <- projectRaster(rasterListCount,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListCount)
# plot(rasterListCount_LatLon)

crs(rasterListCountPoint) <- proj.aeqd
rasterListCountPoint_LatLon <- projectRaster(rasterListCountPoint,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListCountPoint)
# plot(rasterListCountPoint_LatLon)

crs(rasterListStep) <- proj.aeqd
rasterListStep_LatLon <- projectRaster(rasterListStep,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(rasterListStep)
# plot(rasterListCountPoint_LatLon)


################ write rasters
library(terra)

rasterListHunt<-rast(rasterListHunt)
names(rasterListHunt)
terra::writeRaster(rasterListHunt, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Stage/Hunting_Stage.tif",overwrite=TRUE)

rasterListDepth<-rast(rasterListDepth)
names(rasterListDepth)
terra::writeRaster(rasterListDepth, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Stage/Depth_Stage.tif",overwrite=TRUE)

rasterListCount<-rast(rasterListCount)
names(rasterListCount)
terra::writeRaster(rasterListCount, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Stage/IDCount_Stage.tif",overwrite=TRUE)

rasterListCountPoint<-rast(rasterListCountPoint)
names(rasterListCountPoint)
terra::writeRaster(rasterListCountPoint, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Stage/PointCount_Stage.tif",overwrite=TRUE)

rasterListStep<-rast(rasterListStep)
names(rasterListStep)
terra::writeRaster(rasterListStep, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Stage/StepLength_Stage.tif",overwrite=TRUE)


##############################################################
############################### stage_season rasters
##############################################################

seasonList<-unique(HuntingOnly$stage_season)

rasterListHunt<-stack()
rasterListHuntSD<-stack()
rasterListHuntSUM<-stack()
rasterListDepth<-stack()
rasterListDepthSD<-stack()
rasterListCount<-stack()
rasterListCountPoint<-stack()
rasterListStep<-stack()


for (i in 1 :length(seasonList)){
  
  LPAcc_all_sub<-HuntingOnly[which(HuntingOnly$stage_season==seasonList[i]),]
  xy<-as.data.frame(LPAcc_all_sub[,c("mu.x","mu.y")])
  vals<-LPAcc_all_sub$HuntingTime_sec
  summary(vals)
  # use rasterize to create desired raster
  LP_raster <- rasterize(x=xy, # lon-lat data
                         y=r_obj, # raster object
                         field=vals, # vals to fill raster with
                         fun=median) # aggregate function
  LP_raster@data@names<-as.character(seasonList[i])
  rasterListHunt<-stack(LP_raster,rasterListHunt)
  
  # use rasterize to create desired raster
  LP_rasterSD <- rasterize(x=xy, # lon-lat data
                         y=r_obj, # raster object
                         field=vals, # vals to fill raster with
                         fun=sd) # aggregate function
  LP_rasterSD@data@names<-as.character(seasonList[i])
  rasterListHuntSD<-stack(LP_rasterSD,rasterListHuntSD)
  
    # use rasterize to create desired raster
    LP_rasterSUM <- rasterize(x=xy, # lon-lat data
                              y=r_obj, # raster object
                              field=vals, # vals to fill raster with
                              fun=sum) # aggregate function
    LP_rasterSUM@data@names<-as.character(seasonList[i])
    rasterListHuntSUM<-stack(LP_rasterSUM,rasterListHuntSUM)
  
  # use rasterize to create desired raster
  LP_CountPoints <- rasterize(x=xy, # lon-lat data
                              y=r_obj, # raster object
                              field=vals, # vals to fill raster with
                              fun='count') # aggregate function
  LP_CountPoints@data@names<-as.character(seasonList[i])
  rasterListCountPoint<-stack(LP_CountPoints,rasterListCountPoint)
  
  vals<-LPAcc_all_sub$maxDivingDepth_m
  summary(vals)
  
  # use rasterize to create desired raster
  LP_Depthraster <- rasterize(x=xy, # lon-lat data
                              y=r_obj, # raster object
                              field=vals, # vals to fill raster with
                              fun=median) # aggregate function
  LP_Depthraster@data@names<-as.character(seasonList[i])
  rasterListDepth<-stack(LP_Depthraster,rasterListDepth)
  
  # use rasterize to create desired raster
  LP_DepthrasterSD <- rasterize(x=xy, # lon-lat data
                              y=r_obj, # raster object
                              field=vals, # vals to fill raster with
                              fun=sd) # aggregate function
  LP_DepthrasterSD@data@names<-as.character(seasonList[i])
  rasterListDepthSD<-stack(LP_DepthrasterSD,rasterListDepthSD)
  
  
  rCount <- raster(vals=NA,xmn=xMin, xmx=xMax, ymn= yMin, ymx= yMax, resolution=c(10000,10000))
  #LPAcc_all_sub[, rastercell := cellFromXY(r_obj, .SD[,.(LPAcc_all_sub$mu.x, LPAcc_all_sub$mu.y)])]
  LPAcc_all_sub$rastercell <- cellFromXY(rCount, LPAcc_all_sub[,c("mu.x","mu.y")])
  # aggregate how many unique categories there are in each raster cell
  data_count_1 <- aggregate(data = LPAcc_all_sub,                # Applying aggregate
                            bird_IDYear ~ rastercell,
                            function(x) length(unique(x)))
  # update the values of the raster with these values
  rCount[data_count_1[, "rastercell"]] <- data_count_1[, "bird_IDYear"] 
  
  rCount@data@names<-as.character(seasonList[i])
  rasterListCount<-stack(rCount,rasterListCount)
  
  
  vals<-LPAcc_all_sub$step
  summary(vals)
  # use rasterize to create desired raster
  LP_Step_raster <- rasterize(x=xy, # lon-lat data
                              y=r_obj, # raster object
                              field=vals, # vals to fill raster with
                              fun=median,
                              na.rm=TRUE) # aggregate function
  LP_Step_raster@data@names<-as.character(seasonList[i])
  rasterListStep<-stack(LP_Step_raster,rasterListStep)
  
}
################ adjust projections

ESPG <- x<-rgdal::make_EPSG()
myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
proj.aeqd <- myESPG$prj4

crs(rasterListHunt) <- proj.aeqd
rasterList_LatLon <- projectRaster(rasterListHunt,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListHunt)
# plot(rasterListHunt_LatLon)

crs(rasterListHuntSD) <- proj.aeqd
rasterListHuntSD_LatLon <- projectRaster(rasterListHuntSD,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListHunt)
# plot(rasterListHuntSD_LatLon)

crs(rasterListHuntSUM) <- proj.aeqd
rasterListHuntSUM_LatLon <- projectRaster(rasterListHuntSUM,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListHuntSUM)
 plot(rasterListHuntSUM_LatLon)

crs(rasterListDepth) <- proj.aeqd
rasterListDepth_LatLon <- projectRaster(rasterListDepth,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListDepth)
# plot(rasterListDepth_LatLon)

crs(rasterListDepthSD) <- proj.aeqd
rasterListDepthSD_LatLon <- projectRaster(rasterListDepthSD,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListDepth)
# plot(rasterListDepthSD_LatLon)

crs(rasterListCount) <- proj.aeqd
rasterListCount_LatLon <- projectRaster(rasterListCount,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListCount)
# plot(rasterListCount_LatLon)

crs(rasterListCountPoint) <- proj.aeqd
rasterListCountPoint_LatLon <- projectRaster(rasterListCountPoint,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListCountPoint)
# plot(rasterListCountPoint_LatLon)

crs(rasterListStep) <- proj.aeqd
rasterListStep_LatLon <- projectRaster(rasterListStep,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(rasterListStep)
# plot(rasterListCountPoint_LatLon)

################ write rasters
library(terra)

rasterListHunt<-rast(rasterListHunt)
names(rasterListHunt) 
terra::writeRaster(rasterListHunt, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Season_Stage/Hunting_Season_Stage.tif",overwrite=TRUE)

rasterListHuntSD<-rast(rasterListHuntSD)
names(rasterListHuntSD) 
terra::writeRaster(rasterListHuntSD, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Season_Stage/HuntingSD_Season_Stage.tif",overwrite=TRUE)

rasterListHuntSUM<-rast(rasterListHuntSUM)
names(rasterListHuntSUM) 
terra::writeRaster(rasterListHuntSUM, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Season_Stage/HuntingSUM_Season_Stage.tif",overwrite=TRUE)

rasterListDepth<-rast(rasterListDepth)
names(rasterListDepth) 
terra::writeRaster(rasterListDepth, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Season_Stage/Depth_Season_Stage.tif",overwrite=TRUE)

rasterListDepthSD<-rast(rasterListDepthSD)
names(rasterListDepthSD) 
terra::writeRaster(rasterListDepthSD, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Season_Stage/DepthSD_Season_Stage.tif",overwrite=TRUE)

rasterListCount<-rast(rasterListCount)
names(rasterListCount) 
terra::writeRaster(rasterListCount, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Season_Stage/IDCount_Season_Stage.tif",overwrite=TRUE)

rasterListCountPoint<-rast(rasterListCountPoint)
names(rasterListCountPoint) 
terra::writeRaster(rasterListCountPoint, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Season_Stage/PointCount_Season_Stage.tif",overwrite=TRUE)

rasterListStep<-rast(rasterListStep)
names(rasterListStep) 
terra::writeRaster(rasterListStep, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Season_Stage/StepLength_Season_Stage.tif",overwrite=TRUE)

##############################################################
############################### daily rasters
##############################################################

seasonList<-unique(HuntingOnly$GMT_Day)

rasterListHunt<-stack()
rasterListHuntSUM<-stack()
rasterListDepth<-stack()
rasterListCount<-stack()
rasterListCountPoint<-stack()
rasterListAngle<-stack()
rasterListStep<-stack()
rasterListBearing<-stack()


for (i in 1 :length(seasonList)){
  
  LPAcc_all_sub<-HuntingOnly[which(HuntingOnly$GMT_Day==seasonList[i]),]
  xy<-as.data.frame(LPAcc_all_sub[,c("mu.x","mu.y")])
  vals<-LPAcc_all_sub$HuntingTime_sec
  summary(vals)
  # use rasterize to create desired raster
  LP_raster <- rasterize(x=xy, # lon-lat data
                         y=r_obj, # raster object
                         field=vals, # vals to fill raster with
                         fun=median) # aggregate function
  LP_raster@data@names<-paste("Day_",as.character(seasonList[i]),sep="")
  rasterListHunt<-stack(LP_raster,rasterListHunt)

  LPAcc_all_sub<-HuntingOnly[which(HuntingOnly$GMT_Day==seasonList[i]),]
  xy<-as.data.frame(LPAcc_all_sub[,c("mu.x","mu.y")])
  vals<-LPAcc_all_sub$HuntingTime_sec
  summary(vals)
  
  # use rasterize to create desired raster
  LP_rasterSUM <- rasterize(x=xy, # lon-lat data
                            y=r_obj, # raster object
                            field=vals, # vals to fill raster with
                            fun=sum) # aggregate function
  LP_rasterSUM@data@names<-paste("Day_",as.character(seasonList[i]),sep="")
  rasterListHuntSUM<-stack(LP_rasterSUM,rasterListHuntSUM)
  
    
  # use rasterize to create desired raster
  LP_CountPoints <- rasterize(x=xy, # lon-lat data
                              y=r_obj, # raster object
                              field=vals, # vals to fill raster with
                              fun='count') # aggregate function
  LP_CountPoints@data@names<-paste("Day_",as.character(seasonList[i]),sep="")
  rasterListCountPoint<-stack(LP_CountPoints,rasterListCountPoint)
  
  vals<-LPAcc_all_sub$maxDivingDepth_m
  summary(vals)
  # use rasterize to create desired raster
  LP_Depthraster <- rasterize(x=xy, # lon-lat data
                              y=r_obj, # raster object
                              field=vals, # vals to fill raster with
                              fun=median) # aggregate function
  LP_Depthraster@data@names<-paste("Day_",as.character(seasonList[i]),sep="")
  rasterListDepth<-stack(LP_Depthraster,rasterListDepth)
  
  rCount <- raster(vals=NA,xmn=xMin, xmx=xMax, ymn= yMin, ymx= yMax, resolution=c(10000,10000))
  #LPAcc_all_sub[, rastercell := cellFromXY(r_obj, .SD[,.(LPAcc_all_sub$mu.x, LPAcc_all_sub$mu.y)])]
  LPAcc_all_sub$rastercell <- cellFromXY(rCount, LPAcc_all_sub[,c("mu.x","mu.y")])
  # aggregate how many unique categories there are in each raster cell
  data_count_1 <- aggregate(data = LPAcc_all_sub,                # Applying aggregate
                            bird_IDYear ~ rastercell,
                            function(x) length(unique(x)))
  # update the values of the raster with these values
  rCount[data_count_1[, "rastercell"]] <- data_count_1[, "bird_IDYear"] 
  
  rCount@data@names<-paste("Day_",as.character(seasonList[i]),sep="")
  rasterListCount<-stack(rCount,rasterListCount)
  
  
  vals<-LPAcc_all_sub$step
  summary(vals)
  # use rasterize to create desired raster
  LP_Step_raster <- rasterize(x=xy, # lon-lat data
                              y=r_obj, # raster object
                              field=vals, # vals to fill raster with
                              fun=median,
                              na.rm=TRUE) # aggregate function
  LP_Step_raster@data@names<-paste("Day_",as.character(seasonList[i]),sep="")
  rasterListStep<-stack(LP_Step_raster,rasterListStep)
  
  
 }

################ adjust projections

ESPG <- x<-rgdal::make_EPSG()
myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
proj.aeqd <- myESPG$prj4

crs(rasterListHunt) <- proj.aeqd
rasterListHunt_LatLon <- projectRaster(rasterListHunt,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListHunt)
# plot(rasterListHunt_LatLon)

crs(rasterListHuntSUM) <- proj.aeqd
rasterListHuntSUM_LatLon <- projectRaster(rasterListHuntSUM,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListHunt)
 plot(rasterListHuntSUM_LatLon)


crs(rasterListDepth) <- proj.aeqd
rasterListDepth_LatLon <- projectRaster(rasterListDepth,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListDepth)
# plot(rasterListDepth_LatLon)

crs(rasterListCount) <- proj.aeqd
rasterListCount_LatLon <- projectRaster(rasterListCount,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListCount)
# plot(rasterListCount_LatLon)

crs(rasterListCountPoint) <- proj.aeqd
rasterListCountPoint_LatLon <- projectRaster(rasterListCountPoint,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(rasterListCountPoint)
# plot(rasterListCountPoint_LatLon)


crs(rasterListStep) <- proj.aeqd
rasterListStep_LatLon <- projectRaster(rasterListStep,crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(rasterListStep)
# plot(rasterListCountPoint_LatLon)



################ write rasters
library(terra)

rasterListHunt<-rast(rasterListHunt)
names(rasterListHunt) 
terra::writeRaster(rasterListHunt, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Daily/Hunting_Daily.tif",overwrite=TRUE)

rasterListHuntSUM<-rast(rasterListHuntSUM)
names(rasterListHuntSUM) 
terra::writeRaster(rasterListHuntSUM, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Daily/HuntingSUM_Daily.tif",overwrite=TRUE)

rasterListDepth<-rast(rasterListDepth)
names(rasterListDepth) 
terra::writeRaster(rasterListDepth, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Daily/Depth_Daily.tif",overwrite=TRUE)

rasterListCount<-rast(rasterListCount)
names(rasterListCount) 
terra::writeRaster(rasterListCount, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Daily/IDCount_Daily.tif",overwrite=TRUE)

rasterListCountPoint<-rast(rasterListCountPoint)
names(rasterListCountPoint) 
terra::writeRaster(rasterListCountPoint, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Daily/PointCount_Daily.tif",overwrite=TRUE)

rasterListStep<-rast(rasterListStep)
names(rasterListStep) 
terra::writeRaster(rasterListStep, "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Daily/StepLength_Daily.tif",overwrite=TRUE)


