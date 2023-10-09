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
options(digits.secs=9)



########################################################################
######### read hunting rasters
########################################################################
Hunting_S_S<-stack("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Stage/Hunting_Stage.tif")
Depth_S_S<-stack("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Stage/Depth_Stage.tif")
IDCount_S_S<-stack("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Stage/IDCount_Stage.tif")
PointCount_S_S<-stack("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Stage/PointCount_Stage.tif")
Step_S_S<-stack("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Results/Rasters_LP/Stage/StepLength_Stage.tif")


Hunting_S_S_df <- as.data.frame(Hunting_S_S, xy = TRUE) %>% 
  tidyr::pivot_longer(cols = !c(x, y), 
                      names_to = 'variable', 
                      values_to = 'value')

Hunting_S_S_df$Stage<-substr(Hunting_S_S_df$variable,7,7)
Hunting_S_S_df$Year<- substr(Hunting_S_S_df$variable,13,16)
head(Hunting_S_S_df)

Depth_S_S_df <- as.data.frame(Depth_S_S, xy = TRUE) %>% 
  tidyr::pivot_longer(cols = !c(x, y), 
                      names_to = 'variable', 
                      values_to = 'value')

Depth_S_S_df$Stage<-substr(Depth_S_S_df$variable,7,7)
Depth_S_S_df$Year<- substr(Depth_S_S_df$variable,13,16)
head(Depth_S_S_df)

IDCount_S_S_df <- as.data.frame(IDCount_S_S, xy = TRUE) %>% 
  tidyr::pivot_longer(cols = !c(x, y), 
                      names_to = 'variable', 
                      values_to = 'value')

IDCount_S_S_df$Stage<-substr(IDCount_S_S_df$variable,7,7)
IDCount_S_S_df$Year<- substr(IDCount_S_S_df$variable,13,16)
head(IDCount_S_S_df)

PointCount_S_S_df <- as.data.frame(PointCount_S_S, xy = TRUE) %>% 
  tidyr::pivot_longer(cols = !c(x, y), 
                      names_to = 'variable', 
                      values_to = 'value')

PointCount_S_S_df$Stage<-substr(PointCount_S_S_df$variable,7,7)
PointCount_S_S_df$Year<- substr(PointCount_S_S_df$variable,13,16)
head(PointCount_S_S_df)

Step_S_S_df <- as.data.frame(Step_S_S, xy = TRUE) %>% 
  tidyr::pivot_longer(cols = !c(x, y), 
                      names_to = 'variable', 
                      values_to = 'value')

Step_S_S_df$Stage<-substr(Step_S_S_df$variable,7,7)
Step_S_S_df$Year<- substr(Step_S_S_df$variable,13,16)
head(Step_S_S_df)

hist(Hunting_S_S_df$value,breaks=100)

#attach variables to one global dataset
Hunting_S_S_df$MaxDepth<-Depth_S_S_df$value
Hunting_S_S_df$stepL<-Step_S_S_df$value

GMT_Day<-substr(Hunting_S_S_df$variable,13,22)
GMT_Day<-gsub("\\.", "-",GMT_Day)
Hunting_S_S_df$GMT_Day<-GMT_Day

head(Hunting_S_S_df)
colnames(Hunting_S_S_df)[4]<-"HuntMEAN"

plot(Hunting_S_S_df$stepL,Hunting_S_S_df$HuntMEAN)
Hunting_S_S_df<-Hunting_S_S_df[which(!is.na(Hunting_S_S_df$HuntMEAN)),]
summary(Hunting_S_S_df)

Hunting_envVar_merged<-list() #create empty list to store final results

########################################################################
#boxRaster
########################################################################
xMin<-1100000 #min(LPAcc_all_sub$mu.x)-200000 #add 10 Km on each side
xMax<-1250000 #max(LPAcc_all_sub$mu.x)+200000

yMin<- -4450000   #min(LPAcc_all_sub$mu.y)-200000 #add 5 Km on each side
yMax<- -4250000

resR<-10000
# create a raster object
r_obj <- raster(vals=NA,xmn=xMin, xmx=xMax, ymn= yMin, ymx= yMax, resolution=c(resR,resR))


##### get data frame of interest from RCode_Analysis_Little_PII
HuntingOnly<-LPAcc_all[which(LPAcc_all$maxDivingDepth_m>0 & LPAcc_all$LengthInt_sec>=900),]
#HuntingOnly<-LPAcc_all[which(LPAcc_all$LengthInt_sec>=900),]

head(HuntingOnly)
#seasonList<-unique(HuntingOnly$Year)
# hist(HuntingOnly$BearingAngle,breaks=50)
# table(HuntingOnly$hourAU)

########################################################################
######### read environmental variables
########################################################################

Bathymetry <- readRDS("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Environment data/Bathymetry.RData")
head(Bathymetry)


#/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Environment data/Thermocline
ThermoclineData <- fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Environment data/Tableau thermocline.csv",header = TRUE)
str(ThermoclineData)
ThermoclineData$Year<-substr(ThermoclineData$time,1,4)
colnames(ThermoclineData)[4]<-"GMT_Day"


ChloroData <- fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Environment data/Chloro updated 2020.csv",header = TRUE)
str(ChloroData)
ChloroData$Year<-substr(ChloroData$time,1,4)
colnames(ChloroData)[1]<-"N"
colnames(ChloroData)[5]<-"GMT_Day"


SSTData <- fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Environment data/SST_2010_2020.csv",header = TRUE)
str(SSTData)
SSTData$Year<-substr(SSTData$GMT_Day,1,4)


CurrentData <- fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Environment data/Current.csv",header = TRUE)
str(CurrentData)
CurrentData$Year<-substr(CurrentData$time,1,4)
colnames(CurrentData)[4]<-"GMT_Day"


# WavesData <- fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Environment data/Waves_2010_2020.csv",header = TRUE)
# str(WavesData)

ZSD_Data <- fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/Environment data/ZSD_2010_2020.csv",header = TRUE)
str(ZSD_Data)
ZSD_Data$Year<-substr(ZSD_Data$GMT_Day,1,4)

########################################################################
######### process environmental variables
########################################################################
##### select year
S_S_List<-unique(Hunting_S_S_df$GMT_Day)
LevCount<-1

for(qq in 1:length(S_S_List)){
  GMT_DaySel<-S_S_List[qq]
  yearAn<-substr(GMT_DaySel,1,4)
  HuntingOnlyYearSel<-HuntingOnly[which(HuntingOnly$GMT_Day==GMT_DaySel),]
  
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
  ##### ##### ##### ##### ##### ##### Bathymetry
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
  
  #Conversion of data frame to sf object
  Bathymetry_sf <- st_as_sf(x = Bathymetry,                         
                            coords = c("lon", "lat"),
                            crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") #"+proj=utm +zone=55 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
  
  #Projection transformation
  ESPG <- x<-rgdal::make_EPSG()
  myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
  proj.aeqd <- myESPG$prj4
  
  Bathymetry_sf_Alb = st_transform(Bathymetry_sf, crs =myESPG$prj4 )
  head(Bathymetry_sf_Alb)
  
  Bathymetry_sf_Albdf <- Bathymetry_sf_Alb %>% st_drop_geometry()
  Bathymetry_sf_Albdf<-cbind(Bathymetry_sf_Albdf,data.frame(st_coordinates(Bathymetry_sf_Alb[,1])))
  head(Bathymetry_sf_Albdf)
  Bathymetry_sf_Albdf$depth[which(Bathymetry_sf_Albdf$depth>0)]<-NA
  
  xy<-as.data.frame(Bathymetry_sf_Albdf[,c("X","Y")])
  valsBath<-Bathymetry_sf_Albdf$depth
  summary(valsBath)
  # use rasterize to create desired raster
  Bath_raster <- rasterize(x=xy, # lon-lat data
                           y=r_obj, # raster object
                           field=valsBath, # vals to fill raster with
                           fun=median,
                           na.rm=TRUE) # aggregate function
  
  Bath_raster@data@names<-"Bathymetry"
  
 
  plot(Bath_raster)
  
  
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
  ##### ##### ##### ##### ##### ##### Thermocline
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
  # The Thermocline depth is the depth at which the temperature is 2°C below the surface temperature. 
  # The Thermocline gradient is the intensity of temperature variation between the 2 levels surrounding the thermocline depth. 
  
  
  ##### ##### Select year and match data
  #Conversion of data frame to sf object
  thermo_estYearSel<-ThermoclineData[which(ThermoclineData$Year==yearAn),]
  thermo_estYearSel_sf <- st_as_sf(x = thermo_estYearSel,                         
                                   coords = c("lon", "lat"),
                                   crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") #"+proj=utm +zone=55 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
  
  #Projection transformation
  ESPG <- x<-rgdal::make_EPSG()
  myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
  proj.aeqd <- myESPG$prj4
  
  thermo_estYearSel_sf_Alb = st_transform(thermo_estYearSel_sf, crs =myESPG$prj4 )
  head(thermo_estYearSel_sf_Alb)
  
  thermo_est_sf_Albdf <- thermo_estYearSel_sf_Alb %>% st_drop_geometry()
  thermo_est_sf_Albdf<-cbind(thermo_est_sf_Albdf,data.frame(st_coordinates(thermo_estYearSel_sf_Alb[,1])))
  head(thermo_est_sf_Albdf)
  
  # create a raster object
  r_obj <- raster(vals=NA,xmn=xMin, xmx=xMax, ymn= yMin, ymx= yMax, resolution=c(resR,resR))
  
  #take matching days from GPS dataset for each season
  thermo_BreedYear_sub<-thermo_est_sf_Albdf[which(thermo_est_sf_Albdf$GMT_Day>=min(HuntingOnlyYearSel$GMT_Day) & thermo_est_sf_Albdf$GMT_Day<=max(HuntingOnlyYearSel$GMT_Day)),]
  head(thermo_BreedYear_sub)
  
  ##### ##### Select year and match data
  thermo_BreedYear_subGrad<-thermo_BreedYear_sub[which(!is.na(thermo_BreedYear_sub$thermo_grad)),]
  xy<-as.data.frame(thermo_BreedYear_subGrad[,c("X","Y")])
  valsThermoG<-thermo_BreedYear_subGrad$thermo_grad
  summary(valsThermoG)
  # use rasterize to create desired raster
  thermo_grad_rasterSeason <- rasterize(x=xy, # lon-lat data
                                        y=r_obj, # raster object
                                        field=valsThermoG, # vals to fill raster with
                                        fun=mean,
                                        na.rm=TRUE) # aggregate function
  
  thermo_grad_rasterSeason@data@names<-"Mean_thermoGrad"
  
  plot(thermo_grad_rasterSeason)
  
  # # use rasterize to create desired raster
  # thermo_gradSD_rasterSeason <- rasterize(x=xy, # lon-lat data
  #                                         y=r_obj, # raster object
  #                                         field=vals, # vals to fill raster with
  #                                         fun=sd) # aggregate function
  # 
  # thermo_gradSD_rasterSeason@data@names<-"SD_thermoGrad"
  # 
  # plot(thermo_gradSD_rasterSeason)
  # 
  thermo_BreedYear_subDepth<-thermo_BreedYear_sub[which(!is.na(thermo_BreedYear_sub$thermo_depthmin)),]
  xy<-as.data.frame(thermo_BreedYear_subDepth[,c("X","Y")])
  valsThermoD<-thermo_BreedYear_subDepth$thermo_depthmin
  summary(valsThermoD)
  # use rasterize to create desired raster
  thermo_depthmin_rasterSeason <- rasterize(x=xy, # lon-lat data
                                            y=r_obj, # raster object
                                            field=valsThermoD, # vals to fill raster with
                                            fun=mean,
                                            na.rm=TRUE) # aggregate function
  
  thermo_depthmin_rasterSeason@data@names<-"Mean_thermoDepth"
  
  plot(thermo_depthmin_rasterSeason)
  
  # # use rasterize to create desired raster
  # thermo_depthminSD_rasterSeason <- rasterize(x=xy, # lon-lat data
  #                                             y=r_obj, # raster object
  #                                             field=vals, # vals to fill raster with
  #                                             fun=sd) # aggregate function
  # 
  # thermo_depthminSD_rasterSeason@data@names<-"SD_thermoDepth"
  # 
  # plot(thermo_depthminSD_rasterSeason)
  # 
  # 
  
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
  ##### ##### ##### ##### ##### ##### SST
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
  ##### ##### Select year and match data
  
  
  #Conversion of data frame to sf object
  SST_estYearSel<-SSTData[which(SSTData$Year==yearAn),]
  SST_estYearSel_sf <- st_as_sf(x = SST_estYearSel,                         
                                coords = c("lon", "lat"),
                                crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") #"+proj=utm +zone=55 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
  
  #Projection transformation
  ESPG <- x<-rgdal::make_EPSG()
  myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
  proj.aeqd <- myESPG$prj4
  
  SST_estYearSel_sf_Alb = st_transform(SST_estYearSel_sf, crs =myESPG$prj4 )
  head(SST_estYearSel_sf_Alb)
  
  SST_estYearSel_sf_Albdf <- SST_estYearSel_sf_Alb %>% st_drop_geometry()
  SST_estYearSel_sf_Albdf<-cbind(SST_estYearSel_sf_Albdf,data.frame(st_coordinates(SST_estYearSel_sf_Alb[,1])))
  head(SST_estYearSel_sf_Albdf)
  
  
  
  # create a raster object
  r_obj <- raster(vals=NA,xmn=xMin, xmx=xMax, ymn= yMin, ymx= yMax, resolution=c(resR,resR))
  
  #take matching days from GPS dataset for each season
  weekGMT_DaySel<-as.character(as.Date(GMT_DaySel)-7) ## weekly mean
  SST_BreedYear_sub<-SST_estYearSel_sf_Albdf[which(SST_estYearSel_sf_Albdf$GMT_Day>=weekGMT_DaySel & SST_estYearSel_sf_Albdf$GMT_Day<=max(HuntingOnlyYearSel$GMT_Day)),]
 # SST_BreedYear_sub<-SST_estYearSel_sf_Albdf[which(SST_estYearSel_sf_Albdf$GMT_Day>=min(HuntingOnlyYearSel$GMT_Day) & SST_estYearSel_sf_Albdf$GMT_Day<=max(HuntingOnlyYearSel$GMT_Day)),]
  head(SST_BreedYear_sub)

  SST_BreedYear_sub_SST<-SST_BreedYear_sub[which(!is.na(SST_BreedYear_sub$sst)),]
  xy<-as.data.frame(SST_BreedYear_sub_SST[,c("X","Y")])
  valsSST<-SST_BreedYear_sub_SST$sst
  summary(valsSST)
  # use rasterize to create desired raster
  SST_rasterSeason <- rasterize(x=xy, # lon-lat data
                                y=r_obj, # raster object
                                field=valsSST, # vals to fill raster with
                                fun=mean,
                                na.rm=TRUE) # aggregate function
  
  SST_rasterSeason@data@names<-"Mean_SST"
  
  plot(SST_rasterSeason)
  
  # use rasterize to create desired raster
  SST_SDrasterSeason <- rasterize(x=xy, # lon-lat data
                                  y=r_obj, # raster object
                                  field=valsSST, # vals to fill raster with
                                  fun=sd,
                                  na.rm=TRUE) # aggregate function

  SST_SDrasterSeason@data@names<-"SD_SST"

  plot(SST_SDrasterSeason)


  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
  ##### ##### ##### ##### ##### ##### ZSD
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
  ##### ##### Select year and match data
  #lower the Secchi depth, the higher the algal concentration and productivity
  #Turbidity was represented as Secchi Disc Depth (SDD) in metres (the depth at which a calibrated black and white disc is still visible from the surface), 
  #calculated using the Morel algorithm (Morel et al., 2007). Low values of SDD indicate high turbidity and high SDD values indicate low turbidity.
  
  
  #Conversion of data frame to sf object
  ZSD_estYearSel<-ZSD_Data[which(ZSD_Data$Year==yearAn),]
  ZSD_estYearSel_sf <- st_as_sf(x = ZSD_estYearSel,                         
                                coords = c("lon", "lat"),
                                crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") #"+proj=utm +zone=55 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
  
  #Projection transformation
  ESPG <- x<-rgdal::make_EPSG()
  myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
  proj.aeqd <- myESPG$prj4
  
  ZSD_estYearSel_sf_Alb = st_transform(ZSD_estYearSel_sf, crs =myESPG$prj4 )
  head(ZSD_estYearSel_sf_Alb)
  
  ZSD_estYearSel_sf_Albdf <- ZSD_estYearSel_sf_Alb %>% st_drop_geometry()
  ZSD_estYearSel_sf_Albdf<-cbind(ZSD_estYearSel_sf_Albdf,data.frame(st_coordinates(ZSD_estYearSel_sf_Alb[,1])))
  head(ZSD_estYearSel_sf_Albdf)
  
  
  
  # create a raster object
  r_obj <- raster(vals=NA,xmn=xMin, xmx=xMax, ymn= yMin, ymx= yMax, resolution=c(resR,resR))
  
  #take matching days from GPS dataset for each season
  ZSD_BreedYear_sub<-ZSD_estYearSel_sf_Albdf[which(ZSD_estYearSel_sf_Albdf$GMT_Day>=min(HuntingOnlyYearSel$GMT_Day) & ZSD_estYearSel_sf_Albdf$GMT_Day<=max(HuntingOnlyYearSel$GMT_Day)),]
  head(ZSD_BreedYear_sub)
  
  ZSD_BreedYear_sub_SZD<-ZSD_BreedYear_sub[which(!is.na(ZSD_BreedYear_sub$ZSD_value)),]
  xy<-as.data.frame(ZSD_BreedYear_sub_SZD[,c("X","Y")])
  valsZSD<-ZSD_BreedYear_sub_SZD$ZSD_value
  summary(valsZSD)
  # use rasterize to create desired raster
  ZSD_rasterSeason <- rasterize(x=xy, # lon-lat data
                                y=r_obj, # raster object
                                field=valsZSD, # vals to fill raster with
                                fun=mean,
                                na.rm=TRUE) # aggregate function
  
  ZSD_rasterSeason@data@names<-"Mean_ZSD"
  
  plot(ZSD_rasterSeason)
  
  # # use rasterize to create desired raster
  # ZSD_SDrasterSeason <- rasterize(x=xy, # lon-lat data
  #                                 y=r_obj, # raster object
  #                                 field=vals, # vals to fill raster with
  #                                 fun=sd) # aggregate function
  # 
  # ZSD_SDrasterSeason@data@names<-"SD_ZSD"
  # 
  # plot(ZSD_SDrasterSeason)
  # 
  
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
  ##### ##### ##### ##### ##### ##### Currents
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
  ##### ##### Select year and match data
  
  
  #Conversion of data frame to sf object
  Current_estYearSel<-CurrentData[which(CurrentData$Year==yearAn),]
  Current_estYearSel_sf <- st_as_sf(x = Current_estYearSel,                         
                                    coords = c("lon", "lat"),
                                    crs = " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") #"+proj=utm +zone=55 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
  
  #Projection transformation
  ESPG <- x<-rgdal::make_EPSG()
  myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
  proj.aeqd <- myESPG$prj4
  
  Current_estYearSel_sf_Alb = st_transform(Current_estYearSel_sf, crs =myESPG$prj4 )
  head(Current_estYearSel_sf_Alb)
  
  Current_estYearSel_sf_Albdf <- Current_estYearSel_sf_Alb %>% st_drop_geometry()
  Current_estYearSel_sf_Albdf<-cbind(Current_estYearSel_sf_Albdf,data.frame(st_coordinates(Current_estYearSel_sf_Alb[,1])))
  head(Current_estYearSel_sf_Albdf)
  
  
  
  # create a raster object
  r_obj <- raster(vals=NA,xmn=xMin, xmx=xMax, ymn= yMin, ymx= yMax, resolution=c(resR,resR))
  
  #take matching days from GPS dataset for each season
  Current_BreedYear_sub<-Current_estYearSel_sf_Albdf[which(Current_estYearSel_sf_Albdf$GMT_Day>=min(HuntingOnlyYearSel$GMT_Day) & Current_estYearSel_sf_Albdf$GMT_Day<=max(HuntingOnlyYearSel$GMT_Day)),]
  head(Current_BreedYear_sub)
  
  Current_BreedYear_sub_Speed<-Current_BreedYear_sub[which(!is.na(Current_BreedYear_sub$speed)),]
  xy<-as.data.frame(Current_BreedYear_sub_Speed[,c("X","Y")])
  valsCur<-Current_BreedYear_sub_Speed$speed
  summary(valsCur)
  # use rasterize to create desired raster
  CurrentSpeed_rasterSeason <- rasterize(x=xy, # lon-lat data
                                         y=r_obj, # raster object
                                         field=valsCur, # vals to fill raster with
                                         fun=mean,
                                         na.rm=TRUE) # aggregate function
  
  CurrentSpeed_rasterSeason@data@names<-"Mean_Speed"
  
  plot(CurrentSpeed_rasterSeason)
  
  # # use rasterize to create desired raster
  # CurrentSpeedSD_rasterSeason <- rasterize(x=xy, # lon-lat data
  #                                          y=r_obj, # raster object
  #                                          field=vals, # vals to fill raster with
  #                                          fun=sd) # aggregate function
  # 
  # CurrentSpeedSD_rasterSeason@data@names<-"SD_Speed"
  # 
  # plot(CurrentSpeedSD_rasterSeason)
  # 
  
  
  
  ################################################
  ###select year and match env variable with hunting
  ################################################
  Hunting_S_S_df_dfYearSel<-Hunting_S_S_df[which(Hunting_S_S_df$GMT_Day==GMT_DaySel),]
  dim(Hunting_S_S_df_dfYearSel)
  
  StageUni<-unique(Hunting_S_S_df_dfYearSel$Stage)
  
  for (mm in 1:length(StageUni)){
    
    Hunting_S_S_df_dfYearSel_Stage<-Hunting_S_S_df_dfYearSel[which(Hunting_S_S_df_dfYearSel$Stage==StageUni[mm]),]
    
    Hunting_S_S_df_dfYearSel_Stage$thermo_grad<-NA  #daily
    Hunting_S_S_df_dfYearSel_Stage$thermo_depthmin<-NA  #daily
    Hunting_S_S_df_dfYearSel_Stage$Bath<-NA #static
    Hunting_S_S_df_dfYearSel_Stage$SST<-NA #daily
    Hunting_S_S_df_dfYearSel_Stage$SST_SD<-NA #daily
    Hunting_S_S_df_dfYearSel_Stage$ZSD<-NA #daily
    Hunting_S_S_df_dfYearSel_Stage$Current_speed<-NA
  
  
  #Conversion of data frame to sf object
  Hunting_S_S_df_dfYearSel_sf <- st_as_sf(x = Hunting_S_S_df_dfYearSel_Stage,                         
                                         coords = c("x", "y"),
                                         crs = proj.aeqd) #"+proj=utm +zone=55 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
  
  
    ##Bathymetry
    Bath_rasterVal<-raster::extract(Bath_raster, Hunting_S_S_df_dfYearSel_sf, method='simple', buffer=NULL)
    Hunting_S_S_df_dfYearSel_Stage$Bath<-Bath_rasterVal
    
    ##thermocline
    thermogradVal<-raster::extract(thermo_grad_rasterSeason, Hunting_S_S_df_dfYearSel_sf, method='simple', buffer=NULL)
    Hunting_S_S_df_dfYearSel_Stage$thermo_grad<-thermogradVal
    
    thermo_depthminVal<-raster::extract(thermo_depthmin_rasterSeason, Hunting_S_S_df_dfYearSel_sf, method='simple', buffer=NULL)
    Hunting_S_S_df_dfYearSel_Stage$thermo_depthmin<-thermo_depthminVal
    
    ##SST 
    subSSTVal<-raster::extract(SST_rasterSeason, Hunting_S_S_df_dfYearSel_sf, method='simple', buffer=NULL)
    Hunting_S_S_df_dfYearSel_Stage$SST<-subSSTVal
    
    subSSTValSD<-raster::extract(SST_SDrasterSeason, Hunting_S_S_df_dfYearSel_sf, method='simple', buffer=NULL)
    Hunting_S_S_df_dfYearSel_Stage$SST_SD<-subSSTValSD
    
    ##current 
    subSpeedVal<-raster::extract(CurrentSpeed_rasterSeason, Hunting_S_S_df_dfYearSel_sf, method='simple', buffer=NULL)
    Hunting_S_S_df_dfYearSel_Stage$Current_speed<-subSpeedVal
    
    ##ZSD 
    subZSDVal<-raster::extract(ZSD_rasterSeason, Hunting_S_S_df_dfYearSel_sf, method='simple', buffer=NULL)
    Hunting_S_S_df_dfYearSel_Stage$ZSD<-subZSDVal
  
  
  head(Hunting_S_S_df_dfYearSel_Stage)
  
  Hunting_envVar_merged[[LevCount]]<-Hunting_S_S_df_dfYearSel_Stage[which(!is.na(Hunting_S_S_df_dfYearSel_Stage$HuntMEAN)),]
  LevCount<-LevCount+1
  
  }
}


Hunting_SeasonStage_dfYearSelClean <- ldply (Hunting_envVar_merged, data.frame)
head(Hunting_SeasonStage_dfYearSelClean)
dim(Hunting_SeasonStage_dfYearSelClean)
table(Hunting_SeasonStage_dfYearSelClean$variable)
write.csv(Hunting_SeasonStage_dfYearSelClean,"/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/HuntingEnv_matchSeasonStage_2010_2020.csv", row.names = FALSE)

summary(Hunting_SeasonStage_dfYearSelClean)


##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### Data analysis
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

library(regclass)
library(corrplot)
library(mgcv)
library(lme4)
library(gamm4)
library(olsrr)
library(MuMIn)
library(mgcViz)


Hunting_SeasonStage<-fread("/Users/mariannachimienti/MarieCurie/Paper_II_data&analysis/HuntingEnv_matchSeasonStage_2010_2020.csv",header = TRUE)
Hunting_SeasonStage$Year<-as.factor(Hunting_SeasonStage$Year)
Hunting_SeasonStage$Stage<-as.factor(Hunting_SeasonStage$Stage)
Hunting_SeasonStage$StageReL<-factor(Hunting_SeasonStage$Stage, levels = c("I", "G", "P"))

Hunting_SeasonStage$Month<- as.factor(substr(Hunting_SeasonStage$GMT_Day,6,7))
Hunting_SeasonStage$StageYear<- as.factor(paste(Hunting_SeasonStage$Stage,Hunting_SeasonStage$Year,sep="_"))

my_date <- as.Date(Hunting_SeasonStage$GMT_Day)          # Create example date
head(my_date) 
Hunting_SeasonStage$my_julian <- as.factor(as.POSIXlt(my_date)$yday)    # Convert date to Julian day
Hunting_SeasonStage$my_week <- as.numeric(strftime(Hunting_SeasonStage$GMT_Day, format = "%V"))
Hunting_SeasonStage$my_weekYear<-as.factor(paste(Hunting_SeasonStage$my_week,Hunting_SeasonStage$Year,sep="_"))



Hunting_SeasonStage<-Hunting_SeasonStage[-which(is.na(Hunting_SeasonStage$ZSD)),]
Hunting_SeasonStage<-Hunting_SeasonStage[-which(is.na(Hunting_SeasonStage$stepL)),]
Hunting_SeasonStage$Bath<-Hunting_SeasonStage$Bath*-1
summary(Hunting_SeasonStage)
table(Hunting_SeasonStage$Month)
Hunting_SeasonStage <- Hunting_SeasonStage[order(Hunting_SeasonStage$GMT_Day),]
Hunting_SeasonStage$GMT_Day_F<-as.factor(Hunting_SeasonStage$GMT_Day)
Hunting_SeasonStage$variable<-as.factor(Hunting_SeasonStage$variable)
str(Hunting_SeasonStage)


#Calculate distance to the colony

ESPG <- x<-rgdal::make_EPSG()
myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
proj.aeqd <- myESPG$prj4

Hunting_sf <- st_as_sf(x = Hunting_SeasonStage,                         
                       coords = c("x", "y"),
                       crs = proj.aeqd) #"+proj=utm +zone=55 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "

df <- data.frame(latitude=c(-38.21),
                 longitude=c(145.15))

#Conversion of data frame to sf object
df_sf <- st_as_sf(x = df,                         
                  coords = c("longitude", "latitude"),
                  crs =  "+proj=longlat +datum=WGS84")

#Projection transformation
sfc = st_transform(df_sf, crs = proj.aeqd)


bla<-st_distance(Hunting_sf, sfc)

Hunting_SeasonStage$distToColony<-as.numeric(bla)/1000
summary(Hunting_SeasonStage$distToColony)

head(Hunting_SeasonStage)

hist(Hunting_SeasonStage$HuntMEAN,breaks=50)
hist(Hunting_SeasonStage$Current_speed,breaks=50)

mean(Hunting_SeasonStage$stepL)
sd(Hunting_SeasonStage$stepL)

mean(Hunting_SeasonStage$Bath)
sd(Hunting_SeasonStage$Bath)

mean(Hunting_SeasonStage$ZSD)
sd(Hunting_SeasonStage$ZSD)


tapply(Hunting_SeasonStage$stepL, Hunting_SeasonStage$Year, mean)
tapply(Hunting_SeasonStage$stepL, Hunting_SeasonStage$Year, sd)

tapply(Hunting_SeasonStage$stepL, Hunting_SeasonStage$Stage, mean)
tapply(Hunting_SeasonStage$stepL, Hunting_SeasonStage$Stage, sd)

tapply(Hunting_SeasonStage$Bath, Hunting_SeasonStage$Year, mean)
tapply(Hunting_SeasonStage$Bath, Hunting_SeasonStage$Year, sd)

tapply(Hunting_SeasonStage$Bath, Hunting_SeasonStage$Stage, mean)
tapply(Hunting_SeasonStage$Bath, Hunting_SeasonStage$Stage, sd)

tapply(Hunting_SeasonStage$ZSD, Hunting_SeasonStage$Year, mean)
tapply(Hunting_SeasonStage$ZSD, Hunting_SeasonStage$Year, sd)

tapply(Hunting_SeasonStage$ZSD, Hunting_SeasonStage$Stage, mean)
tapply(Hunting_SeasonStage$ZSD, Hunting_SeasonStage$Stage, sd)

tapply(Hunting_SeasonStage$SST, Hunting_SeasonStage$Year, mean)
tapply(Hunting_SeasonStage$SST, Hunting_SeasonStage$Year, sd)

tapply(Hunting_SeasonStage$SST, Hunting_SeasonStage$Stage, mean)
tapply(Hunting_SeasonStage$SST, Hunting_SeasonStage$Stage, sd)


#create overlaying density plots
plot1<-ggplot()+
  geom_density(data=Hunting_SeasonStage, aes(x=HuntMEAN, fill=as.factor(StageReL)),color="gray50",alpha=.5)+
 # scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), name="Stage:", labels=c("Incubation","Guard","Post Guard"))+
  xlab("av. Hunting time (sec)")+ylab("density")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

 #plot1

plot2<-ggplot()+
  geom_density(data=Hunting_SeasonStage, aes(x=MaxDepth, fill=as.factor(StageReL)),color="gray50",alpha=.5)+
  # scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), name="Stage:", labels=c("Incubation","Guard","Post Guard"))+
  xlab("Max diving depth (m)")+ylab("density")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

# plot2

plot3<-ggplot()+
  geom_density(data=Hunting_SeasonStage, aes(x=stepL, fill=as.factor(StageReL)),color="gray50",alpha=.5)+
  # scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), name="Stage:", labels=c("Incubation","Guard","Post Guard"))+
  xlab("step length (m)")+ylab("density")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

# plot3

plot4<-ggplot()+
  geom_density(data=Hunting_SeasonStage, aes(x=thermo_grad, fill=as.factor(StageReL)),color="gray50",alpha=.5)+
  # scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), name="Stage:", labels=c("Incubation","Guard","Post Guard"))+
  xlab("thermocline gradient (°C)")+ylab("density")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

# plot4

plot5<-ggplot()+
  geom_density(data=Hunting_SeasonStage, aes(x=thermo_depthmin, fill=as.factor(StageReL)),color="gray50",alpha=.5)+
  # scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), name="Stage:", labels=c("Incubation","Guard","Post Guard"))+
  xlab("min thermocline depth (m)")+ylab("density")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

# plot5

plot6<-ggplot()+
  geom_density(data=Hunting_SeasonStage, aes(x=SST, fill=as.factor(StageReL)),color="gray50",alpha=.5)+
  # scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), name="Stage:", labels=c("Incubation","Guard","Post Guard"))+
  xlab("SST (°C)")+ylab("density")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

# plot6

plot7<-ggplot()+
  geom_density(data=Hunting_SeasonStage, aes(x=ZSD, fill=as.factor(StageReL)),color="gray50",alpha=.5)+
  # scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), name="Stage:", labels=c("Incubation","Guard","Post Guard"))+
  xlab("ZSD (m)")+ylab("density")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

# plot7

plot8<-ggplot()+
  geom_density(data=Hunting_SeasonStage, aes(x=Current_speed, fill=as.factor(StageReL)),color="gray50",alpha=.5)+
  # scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), name="Stage:", labels=c("Incubation","Guard","Post Guard"))+
  xlab("Current speed (m/s)")+ylab("density")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

# plot8
plot9<-ggplot()+
  geom_density(data=Hunting_SeasonStage, aes(x=Bath, fill=as.factor(StageReL)),color="gray50",alpha=.5)+
  # scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), name="Stage:", labels=c("Incubation","Guard","Post Guard"))+
  xlab("Bathymetry (m)")+ylab("density")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))


ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,ncol=3,nrow=3, common.legend=TRUE) 

###### plot data points
#create overlaying density plots
plot1<-ggplot()+
  geom_point(data=Hunting_SeasonStage, aes(x=MaxDepth,y=HuntMEAN, fill=as.factor(StageReL)),colour="gray55",alpha=.7,pch=21,size=3)+
  scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  xlab("Max diving depth (m)")+ylab("av. Hunting time (sec)")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

plot1


plot3<-ggplot()+
  geom_point(data=Hunting_SeasonStage, aes(x=stepL,y=HuntMEAN, fill=as.factor(StageReL)),colour="gray55",alpha=.7,pch=21,size=3)+
  scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  xlab("step length (m)")+ylab("av. Hunting time (sec)")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

# plot3

plot4<-ggplot()+
  geom_point(data=Hunting_SeasonStage, aes(x=thermo_grad,y=HuntMEAN, fill=as.factor(StageReL)),colour="gray55",alpha=.7,pch=21,size=3)+
  scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  xlab("thermocline gradient (°C)")+ylab("av. Hunting time (sec)")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

# plot4

plot5<-ggplot()+
  geom_point(data=Hunting_SeasonStage, aes(x=thermo_depthmin,y=HuntMEAN, fill=as.factor(StageReL)),colour="gray55",alpha=.7,pch=21,size=3)+
  scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  xlab("min thermocline depth (m)")+ylab("av. Hunting time (sec)")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

# plot5

plot6<-ggplot()+
  geom_point(data=Hunting_SeasonStage, aes(x=SST,y=HuntMEAN, fill=as.factor(StageReL)),colour="gray55",alpha=.7,pch=21,size=3)+
  scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  xlab("SST (°C)")+ylab("av. Hunting time (sec)")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

# plot6

plot7<-ggplot()+
  geom_point(data=Hunting_SeasonStage, aes(x=ZSD,y=HuntMEAN, fill=as.factor(StageReL)),colour="gray55",alpha=.7,pch=21,size=3)+
  scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  xlab("ZSD (m)")+ylab("av. Hunting time (sec)")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

# plot7

plot8<-ggplot()+
  geom_point(data=Hunting_SeasonStage, aes(x=Current_speed,y=HuntMEAN, fill=as.factor(StageReL)),colour="gray55",alpha=.7,pch=21,size=3)+
  scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  xlab("Current speed (m/s)")+ylab("av. Hunting time (sec)")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

# plot8
plot9<-ggplot()+
  geom_point(data=Hunting_SeasonStage, aes(x=Bath,y=HuntMEAN, fill=as.factor(StageReL)),colour="gray55",alpha=.7,pch=21,size=3)+
  scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  xlab("Bathymetry (m)")+ylab("av. Hunting time (sec)")+
  #    facet_wrap(vars(season),ncol=4)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))


ggarrange(plot1,plot3,plot4,plot5,plot6,plot7,plot8,plot9,ncol=3,nrow=3, common.legend=TRUE) 



plotStages<-ggplot()+
  geom_point(data=Hunting_SeasonStage, aes(x=SST,y=HuntMEAN, fill=as.factor(StageReL)),colour="gray55",alpha=.7,pch=21,size=3)+
  scale_fill_viridis(option = "G",discrete = TRUE,name="") +
  xlab("SST (°C)")+ylab("av. Hunting time (sec)")+
  facet_wrap(vars(StageReL),ncol=1)+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

plotStages

str(Hunting_SeasonStage)
Hunting_SeasonStage$variable<-as.factor(Hunting_SeasonStage$variable)

########## ########## ########## ########## 
########## check data correlation and VIF
########## ########## ########## ########## 
dataCorr<-Hunting_SeasonStage[,c(4,7,8,10:16)]
M = cor(dataCorr)
corrplot(M)

table(Hunting_SeasonStage$Year)

m_lm2 <- glm(HuntMEAN ~MaxDepth+thermo_depthmin+thermo_grad+Bath+Current_speed+ZSD+SST_SD+SST, data=Hunting_SeasonStage)
VIF(m_lm2)

########## ########## ########## ########## 
########## test spline on thermocline
########## ########## ########## ########## 

m_ngamSep <- gam(HuntMEAN ~s(MaxDepth)+s(thermo_depthmin)+s(thermo_grad)+s(Bath)+s(SST,by=StageReL)+ 
                 s(Current_speed)+s(ZSD)+s(stepL)+s(x,y)+s(GMT_Day_F,bs="re")+StageReL+Year, 
               family=Tweedie(p=1.9,link=power(0)), method="REML",
               data=Hunting_SeasonStage)


m_ngamS <- gam(HuntMEAN ~s(MaxDepth)+s(thermo_depthmin,thermo_grad)+s(Bath)+s(SST,by=StageReL)+ 
                 s(Current_speed)+s(ZSD)+s(stepL)+s(x,y)+s(GMT_Day_F,bs="re")+StageReL+Year, 
               family=Tweedie(p=1.9,link=power(0)), method="REML",
               data=Hunting_SeasonStage)

m_ngamTe <- gam(HuntMEAN ~s(MaxDepth)+te(thermo_depthmin,thermo_grad)+s(Bath)+s(SST,by=StageReL)+ 
                 s(Current_speed)+s(ZSD)+s(stepL)+s(x,y)+s(GMT_Day_F,bs="re")+StageReL+Year, 
               family=Tweedie(p=1.9,link=power(0)), method="REML",
               data=Hunting_SeasonStage)


m_ngamTi <- gam(HuntMEAN ~s(MaxDepth)+ti(thermo_depthmin,thermo_grad)+s(Bath)+s(SST,by=StageReL)+ 
              s(Current_speed)+s(ZSD)+s(stepL)+s(x,y)+s(GMT_Day_F,bs="re")+StageReL+Year, 
              family=Tweedie(p=1.9,link=power(0)), method="REML",
               data=Hunting_SeasonStage)

AIC(m_ngamSep,m_ngamS,m_ngamTe,m_ngamTi)


summary(m_ngamTe)
par(mfrow=c(2,2))
gam.check(m_ngamTe)

par(mfrow=c(1,2))
acf(resid(m_ngamTe))
pacf(resid(m_ngamTe))

#### gamm version
##### it has problems in convergence, remove SST and Bath

m_ngamMixed <- gamm(HuntMEAN ~s(MaxDepth)+s(stepL)+s(x,y)+s(ZSD)+te(thermo_depthmin,thermo_grad)+
                      s(Current_speed)+Year,
                    family=Tweedie(p=1.4),
                    correlation = corAR1(value=0.18),
                    data=Hunting_SeasonStage)

par(mfrow=c(1,2))
acf(resid(m_ngamMixed$gam))
pacf(resid(m_ngamMixed$gam))

summary(m_ngamMixed$gam)
par(mfrow=c(2,2))
gam.check(m_ngamMixed$gam)

# pd <- data.frame(stepL=seq(min(Hunting_SeasonStage$stepL),max(Hunting_SeasonStage$stepL),length=1000),
#                  thermo_depthmin=mean(Hunting_SeasonStage$thermo_depthmin),
#                  thermo_grad=mean(Hunting_SeasonStage$thermo_grad),
#                  Bath=mean(Hunting_SeasonStage$Bath),
#                  SST=mean(Hunting_SeasonStage$SST),
#                  ZSD=mean(Hunting_SeasonStage$ZSD),
#                  MaxDepth=mean(Hunting_SeasonStage$MaxDepth),
#                  x=mean(Hunting_SeasonStage$x),
#                  y=mean(Hunting_SeasonStage$y),
#                  Current_speed=mean(Hunting_SeasonStage$Current_speed),
#                  StageReL="P",
#                  Year="2011")
# pv <- predict(m_ngamSep,newdata=pd,type="link",se=TRUE)
# with(Hunting_SeasonStage,plot(stepL,HuntMEAN,ylim=c(0,400),col=3))
# lines(pd$stepL,exp(pv$fit),col=2)
# lines(pd$stepL,exp(pv$fit+2*pv$se.fit),col=2)
# lines(pd$stepL,exp(pv$fit-2*pv$se.fit),col=2)
# 

m_ngamSepViz <- getViz(m_ngamTe)
print(plot(m_ngamSepViz, allTerms = T), pages = 1)

########## ########## ########## ########## 
########## look at model and results
########## ########## ########## ########## 

m_ngamTe <- gam(HuntMEAN ~s(MaxDepth)+te(thermo_depthmin,thermo_grad)+s(Bath)+s(SST,by=StageReL)+ 
                 s(Current_speed)+s(ZSD)+s(stepL)+s(x,y)+s(GMT_Day_F,bs="re")+StageReL+Year, 
                 family=Tweedie(p=1.9,link=power(0)), method="REML",
                 data=Hunting_SeasonStage)

summary(m_ngamTe)
par(mfrow=c(2,2))
gam.check(m_ngamTe)

m_ngamSepViz <- getViz(m_ngamTe)
print(plot(m_ngamSepViz, allTerms = T), pages = 1)

par(mfrow=c(1,2))
acf(resid(m_ngamTe))
pacf(resid(m_ngamTe))


pd <- data.frame(SST=seq(min(Hunting_SeasonStage$SST[which(Hunting_SeasonStage$StageReL=="G")]),max(Hunting_SeasonStage$SST[which(Hunting_SeasonStage$StageReL=="G")]),length=1000),
                 thermo_depthmin=mean(Hunting_SeasonStage$thermo_depthmin),
                 thermo_grad=mean(Hunting_SeasonStage$thermo_grad),
                 Bath=mean(Hunting_SeasonStage$Bath),
                 stepL=mean(Hunting_SeasonStage$stepL),
                 ZSD=mean(Hunting_SeasonStage$ZSD),
                 MaxDepth=mean(Hunting_SeasonStage$MaxDepth),
                 x=mean(Hunting_SeasonStage$x),
                 y=mean(Hunting_SeasonStage$y),
                 Current_speed=mean(Hunting_SeasonStage$Current_speed),
                 StageReL="G",
                 Year="2010")
pv <- predict(m_ngamTi,newdata=pd,type="link",se=TRUE)
pd$fit<-pv$fit
pd$se.fit<-pv$se.fit
# with(Hunting_SeasonStage,plot(stepL,HuntMEAN,ylim=c(0,400),col=3))
# lines(pd$stepL,exp(pv$fit),col=2)
# lines(pd$stepL,exp(pv$fit+2*pv$se.fit),col=2)
# lines(pd$stepL,exp(pv$fit-2*pv$se.fit),col=2)


plotStageG<-ggplot()+
  geom_point(data=Hunting_SeasonStage[which(Hunting_SeasonStage$StageReL=="G")], aes(x=SST,y=HuntMEAN),fill="gray55",colour="gray55",alpha=.7,pch=21,size=3)+
  geom_line(data=pd, aes(x=SST,y=exp(fit)),col="black",linetype=1)+
  geom_line(data=pd, aes(x=SST,y=exp(fit+2*se.fit)),col="black",linetype=2)+
  geom_line(data=pd, aes(x=SST,y=exp(fit-2*se.fit)),col="black",linetype=2)+
  xlab("SST (°C)")+ylab("av. Hunting time (sec)")+
  xlim(min(Hunting_SeasonStage$SST),max(Hunting_SeasonStage$SST))+
  ylim(min(Hunting_SeasonStage$HuntMEAN),max(Hunting_SeasonStage$HuntMEAN))+
  labs(title = "Guard")+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

pd <- data.frame(SST=seq(min(Hunting_SeasonStage$SST[which(Hunting_SeasonStage$StageReL=="I")]),max(Hunting_SeasonStage$SST[which(Hunting_SeasonStage$StageReL=="I")]),length=1000),
                 thermo_depthmin=mean(Hunting_SeasonStage$thermo_depthmin),
                 thermo_grad=mean(Hunting_SeasonStage$thermo_grad),
                 Bath=mean(Hunting_SeasonStage$Bath),
                 stepL=mean(Hunting_SeasonStage$stepL),
                 ZSD=mean(Hunting_SeasonStage$ZSD),
                 MaxDepth=mean(Hunting_SeasonStage$MaxDepth),
                 x=mean(Hunting_SeasonStage$x),
                 y=mean(Hunting_SeasonStage$y),
                 Current_speed=mean(Hunting_SeasonStage$Current_speed),
                 StageReL="I",
                 Year="2010")
pv <- predict(m_ngamTe,newdata=pd,type="link",se=TRUE)
pd$fit<-pv$fit
pd$se.fit<-pv$se.fit
# with(Hunting_SeasonStage,plot(stepL,HuntMEAN,ylim=c(0,400),col=3))
# lines(pd$stepL,exp(pv$fit),col=2)
# lines(pd$stepL,exp(pv$fit+2*pv$se.fit),col=2)
# lines(pd$stepL,exp(pv$fit-2*pv$se.fit),col=2)


plotStageI<-ggplot()+
  geom_point(data=Hunting_SeasonStage[which(Hunting_SeasonStage$StageReL=="I")], aes(x=SST,y=HuntMEAN),fill="gray55",colour="gray55",alpha=.7,pch=21,size=3)+
  geom_line(data=pd, aes(x=SST,y=exp(fit)),col="black",linetype=1)+
  geom_line(data=pd, aes(x=SST,y=exp(fit+2*se.fit)),col="black",linetype=2)+
  geom_line(data=pd, aes(x=SST,y=exp(fit-2*se.fit)),col="black",linetype=2)+
  xlab("SST (°C)")+ylab("av. Hunting time (sec)")+
  xlim(min(Hunting_SeasonStage$SST),max(Hunting_SeasonStage$SST))+
  ylim(min(Hunting_SeasonStage$HuntMEAN),max(Hunting_SeasonStage$HuntMEAN))+
  labs(title = "Incubation")+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))



pd <- data.frame(SST=seq(min(Hunting_SeasonStage$SST[which(Hunting_SeasonStage$StageReL=="P")]),max(Hunting_SeasonStage$SST[which(Hunting_SeasonStage$StageReL=="P")]),length=1000),
                 thermo_depthmin=mean(Hunting_SeasonStage$thermo_depthmin),
                 thermo_grad=mean(Hunting_SeasonStage$thermo_grad),
                 Bath=mean(Hunting_SeasonStage$Bath),
                 stepL=mean(Hunting_SeasonStage$stepL),
                 ZSD=mean(Hunting_SeasonStage$ZSD),
                 MaxDepth=mean(Hunting_SeasonStage$MaxDepth),
                 x=mean(Hunting_SeasonStage$x),
                 y=mean(Hunting_SeasonStage$y),
                 Current_speed=mean(Hunting_SeasonStage$Current_speed),
                 StageReL="P",
                 Year="2010")
pv <- predict(m_ngamTe,newdata=pd,type="link",se=TRUE)
pd$fit<-pv$fit
pd$se.fit<-pv$se.fit
# with(Hunting_SeasonStage,plot(stepL,HuntMEAN,ylim=c(0,400),col=3))
# lines(pd$stepL,exp(pv$fit),col=2)
# lines(pd$stepL,exp(pv$fit+2*pv$se.fit),col=2)
# lines(pd$stepL,exp(pv$fit-2*pv$se.fit),col=2)


plotStageP<-ggplot()+
  geom_point(data=Hunting_SeasonStage[which(Hunting_SeasonStage$StageReL=="P")], aes(x=SST,y=HuntMEAN),fill="gray55",colour="gray55",alpha=.7,pch=21,size=3)+
  geom_line(data=pd, aes(x=SST,y=exp(fit)),col="black",linetype=1)+
  geom_line(data=pd, aes(x=SST,y=exp(fit+2*se.fit)),col="black",linetype=2)+
  geom_line(data=pd, aes(x=SST,y=exp(fit-2*se.fit)),col="black",linetype=2)+
  xlab("SST (°C)")+ylab("av. Hunting time (sec)")+
  xlim(min(Hunting_SeasonStage$SST),max(Hunting_SeasonStage$SST))+
  ylim(min(Hunting_SeasonStage$HuntMEAN),max(Hunting_SeasonStage$HuntMEAN))+
  labs(title = "Post Guard")+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

ggarrange(plotStageI,plotStageG,plotStageP,ncol=1,nrow=3, common.legend=TRUE) 


########## ########## ########## ########## 
### need to do model selection and run all combinations
########## ########## ########## ########## 

options(na.action="na.fail")
combinations <- dredge(m_ngamTe)
print(combinations)


########## ########## ########## ########## 
### Final model
########## ########## ########## ########## 

# ~s(MaxDepth)+te(thermo_depthmin,thermo_grad)+s(Bath)+s(SST,by=StageReL)+ 
#   s(Current_speed)+s(ZSD)+s(stepL)+s(x,y)+

gamFinalN <-  gam(HuntMEAN ~s(MaxDepth)+s(SST,by=StageReL)+s(ZSD)+s(stepL)+s(x,y)+s(GMT_Day_F,bs="re")+StageReL+Year, 
                  family=Tweedie(p=1.9,link=power(0)), method="REML",
                  data=Hunting_SeasonStage)

AIC(gamFinalN)
summary(gamFinalN)
par(mfrow=c(2,2))
gam.check(gamFinalN)

par(mfrow=c(1,2))
acf(resid(gamFinalN))
pacf(resid(gamFinalN))


m_ngamFinal <- getViz(gamFinalN)
print(plot(m_ngamFinal, allTerms = T), pages = 1)


########## ########## ########## ########## 
### Final model gamm option
########## ########## ########## ########## 

# Hunting_SeasonStage$GMT_Day<-as.factor(Hunting_SeasonStage$GMT_Day)
# 
# m_ngamFinal <- gamm(HuntMEAN ~s(MaxDepth)+s(thermo_depthmin,thermo_grad)+s(Current_speed)+s(ZSD)+s(stepL)+s(x,y)+s(SST)+Stage+Year, 
#                     family=Tweedie(p=1.9,link=power(0)), correlation = corAR1(),
#                     data=Hunting_SeasonStage)
# 
# m_ngamFinal <- gamm(HuntMEAN ~s(MaxDepth)+s(thermo_depthmin,thermo_grad)+s(Current_speed)+s(ZSD)+s(stepL)+s(x,y)+Stage+Year, 
#                     family=Tweedie(p=1.9,link=power(0)), correlation = corARMA(p=1,q=1),
#                     data=Hunting_SeasonStage)
# 
# m_ngamFinal <- gamm(HuntMEAN ~s(MaxDepth)+s(thermo_depthmin,thermo_grad)+s(Current_speed)+s(ZSD)+s(stepL)+s(x,y)+Stage+Year, 
#                     family=Tweedie(p=1.1), correlation = corExp(form = ~ 1 | Month),
#                     data=Hunting_SeasonStage)
# 
# 
# summary(m_ngamFinal$gam)
# par(mfrow=c(2,2))
# gam.check(m_ngamFinal$gam)
# 
# par(mfrow=c(1,2))
# acf(resid(m_ngamFinal$lme))
# pacf(resid(m_ngamFinal$gam))
# 
# m_ngamFinal <- getViz(m_ngamFinal$gam)
# print(plot(m_ngamFinal, allTerms = T), pages = 1)
# 
# 

########## ########## ########## ########## 
### Final model plotting
########## ########## ########## ########## 

plot1<-plot(m_ngamFinal, allTerms = T,select=1) +xlab("Maximum diving depth (m)") + ylab("s(Maximum diving depth (m), 8.2)") +
     theme(#legend.key.size = unit(8, 'cm'), #change legend key size
         #legend.key.height = unit(1, 'cm'), #change legend key height
         #legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_blank(), #change legend title font size
          legend.text = element_text(size=20),
          axis.text = element_text(size=20),
          text= element_text(size=20))
plot1


plot2<-plot(m_ngamFinal, allTerms = T,select=5) +xlab("ZSD (m)") + ylab("s(ZSD (m), 1.9)") + 
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_blank(), #change legend title font size
    legend.text = element_text(size=20),
    axis.text = element_text(size=20),
    text= element_text(size=20))

plot2


plot3<-plot(m_ngamFinal, allTerms = T,select=7) + 
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_blank(), #change legend title font size
    legend.text = element_text(size=20),
    axis.text = element_text(size=20),
    text= element_text(size=20))

plot3

plot4<-plot(m_ngamFinal, allTerms = T,select=6) +xlab("Step length (m)")  + ylab("Step length (m), 1)") + 
  scale_x_continuous(breaks = c(0, 500, 1000,1500,2000))+
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_blank(), #change legend title font size
    legend.text = element_text(size=20),
    axis.text = element_text(size=20),
    text= element_text(size=20))

plot4

plot5<-plot(m_ngamFinal, allTerms = T,select=10)+xlab("")+ ylab("f(Season)") + 
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_blank(), #change legend title font size
    legend.text = element_text(size=20),
    axis.text = element_text(size=20),
    text= element_text(size=20))

plot5



### effect of SST

pd <- data.frame(SST=seq(min(Hunting_SeasonStage$SST[which(Hunting_SeasonStage$StageReL=="G")]),max(Hunting_SeasonStage$SST[which(Hunting_SeasonStage$StageReL=="G")]),length=1000),
                 stepL=mean(Hunting_SeasonStage$stepL),
                 ZSD=mean(Hunting_SeasonStage$ZSD),
                 MaxDepth=mean(Hunting_SeasonStage$MaxDepth),
                 x=mean(Hunting_SeasonStage$x),
                 y=mean(Hunting_SeasonStage$y),
                 GMT_Day_F="2010-10-25",
                 StageReL="G",
                 Year="2010")
pv <- predict(gamFinalN,newdata=pd,type="link",se=TRUE)
pd$fit<-pv$fit
pd$se.fit<-pv$se.fit
# with(Hunting_SeasonStage,plot(stepL,HuntMEAN,ylim=c(0,400),col=3))
# lines(pd$stepL,exp(pv$fit),col=2)
# lines(pd$stepL,exp(pv$fit+2*pv$se.fit),col=2)
# lines(pd$stepL,exp(pv$fit-2*pv$se.fit),col=2)


plotStageG<-ggplot()+
  geom_point(data=Hunting_SeasonStage[which(Hunting_SeasonStage$StageReL=="G")], aes(x=SST,y=HuntMEAN),fill="gray55",colour="gray55",alpha=.7,pch=21,size=3)+
  geom_line(data=pd, aes(x=SST,y=exp(fit)),col="black",linetype=1)+
  geom_line(data=pd, aes(x=SST,y=exp(fit+2*se.fit)),col="black",linetype=2)+
  geom_line(data=pd, aes(x=SST,y=exp(fit-2*se.fit)),col="black",linetype=2)+
  xlab("")+ylab("Hunting time (sec)")+
  xlim(min(Hunting_SeasonStage$SST),max(Hunting_SeasonStage$SST))+
  ylim(min(Hunting_SeasonStage$HuntMEAN),max(Hunting_SeasonStage$HuntMEAN))+
  labs(title = "Guard")+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

pd <- data.frame(SST=seq(min(Hunting_SeasonStage$SST[which(Hunting_SeasonStage$StageReL=="I")]),max(Hunting_SeasonStage$SST[which(Hunting_SeasonStage$StageReL=="I")]),length=1000),
                 stepL=mean(Hunting_SeasonStage$stepL),
                 ZSD=mean(Hunting_SeasonStage$ZSD),
                 MaxDepth=mean(Hunting_SeasonStage$MaxDepth),
                 x=mean(Hunting_SeasonStage$x),
                 y=mean(Hunting_SeasonStage$y),
                 GMT_Day_F="2010-10-25",
                 StageReL="G",
                 Year="2010")
pv <- predict(gamFinalN,newdata=pd,type="link",se=TRUE)
pd$fit<-pv$fit
pd$se.fit<-pv$se.fit
# with(Hunting_SeasonStage,plot(stepL,HuntMEAN,ylim=c(0,400),col=3))
# lines(pd$stepL,exp(pv$fit),col=2)
# lines(pd$stepL,exp(pv$fit+2*pv$se.fit),col=2)
# lines(pd$stepL,exp(pv$fit-2*pv$se.fit),col=2)


plotStageI<-ggplot()+
  geom_point(data=Hunting_SeasonStage[which(Hunting_SeasonStage$StageReL=="I")], aes(x=SST,y=HuntMEAN),fill="gray55",colour="gray55",alpha=.7,pch=21,size=3)+
  geom_line(data=pd, aes(x=SST,y=exp(fit)),col="black",linetype=1)+
  geom_line(data=pd, aes(x=SST,y=exp(fit+2*se.fit)),col="black",linetype=2)+
  geom_line(data=pd, aes(x=SST,y=exp(fit-2*se.fit)),col="black",linetype=2)+
  xlab("")+ylab("Hunting time (sec)")+
  xlim(min(Hunting_SeasonStage$SST),max(Hunting_SeasonStage$SST))+
  ylim(min(Hunting_SeasonStage$HuntMEAN),max(Hunting_SeasonStage$HuntMEAN))+
  labs(title = "Incubation")+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))



pd <- data.frame(SST=seq(min(Hunting_SeasonStage$SST[which(Hunting_SeasonStage$StageReL=="P")]),max(Hunting_SeasonStage$SST[which(Hunting_SeasonStage$StageReL=="P")]),length=1000),
                 stepL=mean(Hunting_SeasonStage$stepL),
                 ZSD=mean(Hunting_SeasonStage$ZSD),
                 MaxDepth=mean(Hunting_SeasonStage$MaxDepth),
                 x=mean(Hunting_SeasonStage$x),
                 y=mean(Hunting_SeasonStage$y),
                 GMT_Day_F="2010-10-25",
                 StageReL="G",
                 Year="2010")
pv <- predict(gamFinalN,newdata=pd,type="link",se=TRUE)
pd$fit<-pv$fit
pd$se.fit<-pv$se.fit
# with(Hunting_SeasonStage,plot(stepL,HuntMEAN,ylim=c(0,400),col=3))
# lines(pd$stepL,exp(pv$fit),col=2)
# lines(pd$stepL,exp(pv$fit+2*pv$se.fit),col=2)
# lines(pd$stepL,exp(pv$fit-2*pv$se.fit),col=2)


plotStageP<-ggplot()+
  geom_point(data=Hunting_SeasonStage[which(Hunting_SeasonStage$StageReL=="P")], aes(x=SST,y=HuntMEAN),fill="gray55",colour="gray55",alpha=.7,pch=21,size=3)+
  geom_line(data=pd, aes(x=SST,y=exp(fit)),col="black",linetype=1)+
  geom_line(data=pd, aes(x=SST,y=exp(fit+2*se.fit)),col="black",linetype=2)+
  geom_line(data=pd, aes(x=SST,y=exp(fit-2*se.fit)),col="black",linetype=2)+
  xlab("SST (°C)")+ylab("Hunting time (sec)")+
  xlim(min(Hunting_SeasonStage$SST),max(Hunting_SeasonStage$SST))+
  ylim(min(Hunting_SeasonStage$HuntMEAN),max(Hunting_SeasonStage$HuntMEAN))+
  labs(title = "Post Guard")+
  theme(legend.position = "top") +      
  theme(#legend.key.size = unit(8, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15),
    axis.text = element_text(size=10),
    text= element_text(size=15))

ggarrange(plotStageI,plotStageG,plotStageP,ncol=1,nrow=3, common.legend=TRUE) 












