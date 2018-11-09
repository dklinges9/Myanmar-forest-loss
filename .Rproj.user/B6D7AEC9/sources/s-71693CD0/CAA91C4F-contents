#This is a script that calculates forest loss statistics for all 15 states in Myanmar. It uses SDMTools::PatchStat to output the average patch
#size (in square meters) and the total number of patches for forest loss in Hansen v1.5. Patch statistics are further broken out by year of 
#loss in Hansen (2000-2017) by using the "lossyear" band instead of "loss." See Sumalika Biswas for details.
#
#Audrey Lothspeich
#2018

##### Load Data #####

setwd("W:/Personal/Interns/2018/AudreyLothspeich/Myanmar Forest Loss")

library(dplyr)
library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(SDMTools)


options(stringsAsFactors = F)

projected<-'+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'
states<-readOGR(dsn = "./Myanmar Borders", layer = "MMR_adm1") #Myanmar administrative areas taken from http://www.diva-gis.org/gdata

#I downloaded subsets of Hansen that cover Myanmar directly from 
#https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.5.html
#and initially had to merge them into a single forest loss layer for Myanmar

#Hansen1<-raster("./Myanmar_Hansen/Hansen_GFC-2017-v1.5_lossyear_30N_090E.tif")
#Hansen2<-raster("./Myanmar_Hansen/Hansen_GFC-2017-v1.5_lossyear_20N_090E.tif")
#Hansen3<-raster("./Myanmar_Hansen/Hansen_GFC-2017-v1.5_lossyear_10N_090E.tif")
#Hansen4<-raster("./Myanmar_Hansen/Hansen_GFC-2017-v1.5_lossyear_30N_100E.tif")

#Hansen<-raster::merge(Hansen1,Hansen2,Hansen3,Hansen4)

#writeRaster(Hansen, "./Myanmar_Hansen/All_Hansen.tif", format = "GTiff")

Hansen<-raster("./Myanmar_Hansen/All_Hansen.tif") #%>% projectRaster(crs = crs(projected))

##### Calculating Fragmentation #####

for (i in 1:nrow(states)){
  output<-data.frame() #have it output for each state so that I don't lose progress if something breaks.
  state<-states[i,]
  targetHansen<- crop(Hansen, state)
  #targetHansen<-projectRaster(from = targetHansen, res = 30, crs = crs(projected), method = 'ngb') #projection made the process very slow
  for (j in 1:17){ #years in the dataset are flagged as 1-17
    print(j)
    to<-rep(NA, 18)
    to[j+1]<-1
    reclassify<-cbind(c(0:17), to)
    colnames(reclassify)<-c('is', 'becomes')
    stateLoss<-reclassify(targetHansen, rcl = reclassify)
    
    #can't use dplyr pipelines here because SDMTools::ConnCompLabel and SDMTools::PatchStat don't accept them
    patches<- ConnCompLabel(stateLoss)
    patches<-PatchStat(patches, cellsize = 30, latlon = T)[,6]%>%as.data.frame() #selecting just area from patchstat 
    rowOutput<-c(state@data$NAME_1, j, nrow(patches), mean(patches[,1])) #nrow gives the number of loss patches, mean gives the patch size
    output<-rbind(output, rowOutput)
    colnames(output)<-c("State", "Year", "Number of Loss Patches", "Avg Patch Size (m^2)")
  }
  write.csv(output, paste0("./Myanmar_Hansen/", state@data$NAME_1, ".csv"))
}

##### Data Clean-up #####

states<-list.files("./Myanmar_Hansen", pattern = ".csv", full.names = T)
fragstats<-data.frame()
for (state in states){
  current<-read.csv(state)
  current<-current[,-c(1)] #removes an unnecessary objectid column
  for (i in 1:nrow(current)){ #get the actual year
    if(nchar(current$Year[i]) == 1){
      current$Year[i]<-paste0('200', i)
    }else{
      current$Year[i]<-paste0('20', i)
    }
  }
  fragstats<-rbind(fragstats,current)
}

write.csv(fragstats, "./Myanmar_Hansen/Myanmar_ForestLoss_FragStats.csv")
