#rm(list = ls())
rm(list=(ls()[ls()!="final_model"]))
gc() 
#### load workspace with RF model
################### ################### ###################
################### ################### ###################
##################### load packages
################## ################### ###################
################### ################### ###################

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
library(dplyr)

library(tidymodels)
library(workflows)
library(tune)
library(rsample)
library(recipes)
library(yardstick)
library(usemodels)
library(randomForest)
library(caret)
library(ranger)
library(parsnip)
library(doParallel)
options(digits.secs=20)   

#### on mac to open several R windows, open terminal and paste:open -n /Applications/R.app
################### ################### ###################
################### ################### ###################
##################### read file
################### ################### ###################
################### ################### ###################
thr_depth<-1 #consider diving at 1 meters

filenames <-
  list.files(
    "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/2010AccData_trips",
    pattern = "*.csv",
    full.names = TRUE
  )

#10:length(filenames)


for (TripID in 1:2) { #change this line if you want more or less files running
  accData <- fread(filenames[TripID], header = TRUE)
  accData$DatesPos<-as.POSIXct(accData$Timestamp, format='%d/%m/%Y %H:%M:%OS',tz="UTC")
  
  PitchAdj<-median(accData$Pitch[which(accData$depth25Hz<=thr_depth)])
  accData$PitchDiff<-accData$Pitch-PitchAdj ###diff from animal mean
  
  #head(accData)
  
  predAcc<-predict(final_model, new_data = accData)
  accData$Pred_RF<-predAcc$.pred_class
  
  #head(accData)
  ######################################################################
  ####### plot results
  ######################################################################
  # subDataDive <- accData[which(accData$Sur_NotSur == 1), ]
  # dim(subDataDive)
  # subDataDive$States_lev<-factor(subDataDive$Pred_RF, levels = c("Descending","Swimming","Hunting","Ascending"))
  # 
  # # #
  # start<-100000
  # end<-120000
  # 
  # plot1<-ggplot(subDataDive[start:end,]) +
  #   geom_line(aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=0.5)+
  #   geom_point(aes( x=DatesPos, y=(depth25Hz*-1),colour=States_lev), size=2)+
  #   scale_colour_jco()+xlab("")+ylab("Depth (m)")
  # #plot1
  # 
  # plot3<-ggplot(subDataDive[start:end,]) +
  #   geom_line(aes( x=DatesPos, y=VeDBA),colour="gray50", size=0.5)+
  #   geom_point(aes( x=DatesPos, y=VeDBA,colour=States_lev), size=2)+
  #   scale_colour_jco()+xlab("")+ylab("VeDBA")
  # 
  # plot4<-ggplot(subDataDive[start:end,]) +
  #   geom_line(aes( x=DatesPos, y=SD_DV2),colour="gray50", size=0.5)+
  #   geom_point(aes( x=DatesPos, y=SD_DV2,colour=States_lev), size=2)+
  #   scale_colour_jco()+xlab("")+ylab("SD_DV2")
  # 
  # plot5<-ggplot(subDataDive[start:end,]) +
  #   geom_line(aes( x=DatesPos, y=Pitch),colour="gray50", size=0.5)+
  #   geom_point(aes( x=DatesPos, y=Pitch,colour=States_lev), size=2)+
  #   scale_colour_jco()+xlab("")+ylab("Pitch")
  # dev.new()
  # ggarrange(plot1,plot3,plot5, ncol=1,common.legend = TRUE)
  # # 
  # ################### ################### ###################
  # 
  # box1<-ggplot(subDataDive) +
  #   geom_boxplot(aes( x=States_lev, y=(depth25Hz*-1),fill=States_lev),colour="gray25")+
  #   scale_fill_jco()+xlab("")+ylab("Depth (m)")
  # 
  # box2<-ggplot(subDataDive) +
  #   geom_boxplot(aes( x=States_lev, y=changeDepth25Hz,fill=States_lev),colour="gray25")+
  #   scale_fill_jco()+xlab("")
  # 
  # box3<-ggplot(subDataDive) +
  #   geom_boxplot(aes( x=States_lev, y=SD_DV2,fill=States_lev),colour="gray25")+
  #   scale_fill_jco()+xlab("")
  # 
  # box4<-ggplot(subDataDive) +
  #   geom_boxplot(aes( x=States_lev, y=Pitch,fill=States_lev),colour="gray25")+
  #   scale_fill_jco() +xlab("")
  # 
  # box5<-ggplot(subDataDive) +
  #   geom_boxplot(aes( x=States_lev, y=VeDBA,fill=States_lev),colour="gray25")+
  #   scale_fill_jco() +xlab("")
  # 
  # # dev.new()
  # # ggarrange(box1,box2,box3,box4,box5,box6,ncol=2,nrow=3,common.legend = TRUE)
  # 
  # dev.new()
  # ggarrange(box1,box2,box3,box4,box5,ncol=3,nrow=2,common.legend = TRUE)
  
  
  ###############################################################
  ##### write new file with RF results
  ###############################################################
  ID_names <-
    list.files(
      "/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/2010AccData_trips",
      pattern = "*.csv",
      full.names = FALSE
    )
 
  ID_names<-tools::file_path_sans_ext(ID_names)
  accData<-within(accData,rm("DatesPos")) # remove datesPos might create problems when loading
  write.csv(accData, paste("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/2010Acc_RF/",ID_names[TripID],"_RF1.csv",sep=""), row.names = TRUE)
  
  rm(accData,predAcc)
  gc() 
  
  } 

#####################################################
quit(save="no")
  
  
  
  
  
  
  
  
  
  
  