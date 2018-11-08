#Conservation GIS Lab
#NASA Myanmar Project
#David Klinges, klingesd@si.edu

#Code excerpts from:
#Audrey Lothspeich, lothspeicha@si.edu
#Get forest cover patch statistics per cell #####

#Processing conducted on Hansen 2016 imagery.

################# PREP SPACE ##################

install.packages('raster')
install.packages('sp')
install.packages('rgdal')
install.packages('SDMTools')
install.packages('tiff')
install.packages('doParallel')
install.packages('foreach')
install.packages('parallel')
install.packages('dplyr')
install.packages('doSNOW')
install.packages('foreign')

library(tidyverse)
library(raster)
library(sp)
library(rgdal)
library(SDMTools)
library(tiff)
library(doParallel)
library(foreach)
library(parallel)
library(dplyr)
library(doSNOW)
library(foreign)


############# SET COORDINATE SYSTEM AND PROJECTION, LOAD DATA ###############

## Variable coding
# Limt for forest cover
cover.percent <- 25
# Number of townships in current image
n_townships <- 4

# Increase the storage capacity, if needed
#memory.limit(size=4000)

# Using CRS notation with rgdal and sp packages
EPSG <- make_EPSG() #If reference data frame of CRS codes is needed

# Coordinate system is WGS84, projection is UTM zone 47
#   note: the country of Myanmar spans across both zone 46 and 47; however, more of the country's
#   area is in zone 47.
CRS("+proj=utm +zone=10 +datum=WGS84")
EPSG[grep("32610", EPSG$code),]
projected <- '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs'

# Load Hansen 2016 images
# Repeat for each Hansen image needed
treecover2000 <- raster('./data/inputs/Hansen_2016/Hansen_GFC-2016-v1.4_treecover2000_30N_100E.tif')
lossyear <- raster('./data/inputs/Hansen_2016/Hansen_GFC-2016-v1.4_lossyear_30N_100E.tif')
gain <- raster("./data/inputs/Hansen_2016/Hansen_GFC-2016-v1.4_gain_30N_100E.tif")

# Load shapefile of polygons of Myanmar townships
# Township polygon was clipped to current Hansen image in ArcMap
township <- readOGR(dsn = "./data/inputs/shapefiles/active_township_poly",layer = "30N_100E_townships")

##################### CALC FOREST COVER #############################

#Clip Hansen to township shapefiles
treecover2000.crop <- crop(treecover2000, township)
lossyear.crop <- crop(lossyear, township)
gain.crop <- crop(gain, township)

#get forest cover to the decided threshold (25%)
get.threshold <- function(vector){
  for(i in 1:(length(vector))) {
    if (vector[i] < cover.percent){
      vector[i]<-NA
    }
  }
  return(vector)
}
thresh.treecover2000<- calc(treecover2000.crop, fun = get.threshold)

#test
thresh.treecover2000
plot(thresh.treecover2000)
 
#clip the "forest" to only places where no loss was recorded during the survey years
#Filter images to just the years needed
remain.forest1 <- mask(thresh.treecover2000, lossyear.crop, updatevalue = 0)
remain.forest2 <- mask(remain.forest1, lossyear.crop, inverse = TRUE, updatevalue = 1)

#add gained forest to non-lost "forest"
gain.check <- function(vector1, vector2){
  for (i in 1:length(vector1)){
    val1 <- vector1[i]
    if (is.na(val1)){
      val1 <- 0
    }
    val2 <- vector2[i]
    if (is.na(val2)){
      val2 <- 0
    }
    if ((val1+val2) < 1){
      vector2[i] <- 0
    } else{
      vector2[i] <- 1
    }
  }
  return(vector2)
}

remain.forest3 <-overlay(gain.crop, remain.forest2, fun = gain.check)

#Save the raster so you don't have to go through this whole process every time
writeRaster(remain.forest3, "./data/fragstats/intermediate_products/30N_100E/remain.forest3", format = "GTiff")

remain.forest4 <-projectRaster(remain.forest3, res = 30, crs = projected, method = 'ngb')

#Save the raster so you don't have to go through this whole process every time
writeRaster(remain.forest4, "./data/fragstats/intermediate_products/30N_100E/remainforest4", format = "GTiff")

remain.forest5<-trim(remain.forest4)
writeRaster(remain.forest5, "./data/fragstats/intermediate_products/30N_100E/remainforest5", format = "GTiff")


################### IMPORT AND PREP TOWNSHIP POLYGONS #######################

#In case you need to load exported tif
remain.forest5 <- raster('./data/fragstats/intermediate_products/30N_100E/remainforest5.tif')

#Project township shapefile
township.projected <- spTransform(township, projected)

#Add an identifying column
township.projected$ID <- c(1:n_townships)

#Coerce to points, then to data frame
lin <- as(township.projected, "SpatialLinesDataFrame")
pts <- as.data.frame(as(lin, "SpatialPointsDataFrame"))

#create a list of townships to loop through as a sample
#the list will only contain townships within the given Hansen image
town_list <- vector("list", n_townships)

for (i in 1:n_townships) {
  poly <- as.data.frame(pts %>%
                          filter(ID == i))
  poly$coords.x1 <- as.numeric(poly$coords.x1)
  poly$coords.x2 <- as.numeric(poly$coords.x2)
  coor <- c(poly$coords.x1, poly$coords.x2)
  town_list[[i]] <- matrix(coor, nrow = length(poly$coords.x1), ncol = 2)
}


#Begin parallel processing session
#c1 <- makeCluster(2)
#registerDoParallel(c1)
registerDoParallel(cores = 4)

# Example of using dopar. Use the elapse time to elavaluate your machine's processing time
# This operation should take about 15-30 seconds
x <- iris[which(iris[,5] != "setosa"), c(1,5)]
trials <- 10000
ptime <- system.time({
  r <- foreach(icount(trials), .combine=cbind) %dopar% {
    ind <- sample(100, 100, replace=TRUE)
    result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
    coefficients(result1)
    }
  })[3]
ptime

########################## CLIP HANSEN TO TOWNS #########################

#Looping through each township, crop Hansen forest raster to township polygon and get forest area
clipHansen <- function(raster, towns, projection){
    foreach(i = 1:length(towns), .packages = c("sp", "raster"),
                        .verbose = TRUE) %dopar% { #clip Hansen forest to each township shapefile
                  
                  #prep township poly
                  print("create poly")
                  poly<- Polygons(c(Polygon(towns[[i]])), 1)
                  poly.df<- SpatialPolygons(c(poly), proj4string = CRS(projection))
                
                  #crop raster to poly
                  print("begin crop")
                  current.raster <-crop(raster, poly.df) #like arcmap crop
                  print("begin mask")
                  current.raster<-mask(current.raster,poly.df)
                  print("begin trim")
                  current.raster <- trim(current.raster)
                  writeRaster(current.raster, paste0("./data/fragstats/intermediate_products/30N_100E/raster_", i), format = "GTiff")
                  
  }
}

time <- system.time({
        clipHansen(remain.forest5, town_list, projected)
})
time

#################################### COUNT FOREST CELLS ###############################

#get the simple area of forest per raster
count.cells<- function(vector){ #function to count the number of light-cells per individual raster
  count<-0
  vector[is.na(vector)] <- 0
  for (i in 1:length(vector)){
    if (vector[i]==1)
      count<-count+1
  }
  return(c(count))
}

countInTowns <- function(towns, raster){
  final.poly <- foreach(i = 1:length(raster), .combine = rbind, .packages = c("sp", "raster"), .export = "count.cells", 
                        .verbose = TRUE) %dopar% { #get count of forest raster cells for each township raster.

                          #prep township poly
                          print("create poly")
                          poly<- Polygons(c(Polygon(i)), 1)
                          #poly<- Polygons(c(Polygon(pts)), i)
                          poly.df<- SpatialPolygons(c(poly), proj4string = CRS(projection))
                          
                          current.raster <- raster[[i]]
                          
                          #print(paste(i, "pre-count.cells"))
                          count<- as.data.frame(count.cells(current.raster))
                          #print(count)
                          write.csv(count, paste0("count_", i, ".csv"))
                          joined.data<-SpatialPolygonsDataFrame(poly.df,count, match.ID = FALSE)
                          joined.data
                          #if (i==1){
                          #  final.poly<-joined.data
                          #}else{
                          #  final.poly<-rbind(final.poly, joined.data) #add township poly to a final layer with all polys
                          #}
                          
                          #print(paste(i, " complete"))
                        }
  return(final.poly)
}

#plot(joined.data)

# Import rasters back in to R

raster_1 <- raster("./data/fragstats/intermediate_products/30N_100E/raster_1.tif")
raster_2 <- raster("./data/fragstats/intermediate_products/30N_100E/raster_2.tif")
raster_3 <- raster("./data/fragstats/intermediate_products/30N_100E/raster_3.tif")
raster_4 <- raster("./data/fragstats/intermediate_products/30N_100E/raster_4.tif")

raster_list <- as.list(raster_1, raster_2, raster_3, raster_4)

# Run function
time <- system.time({
  final.poly <- countInTowns(raster_list)
})
time
                          

#I would be impressed if you get to this point by the end of the month
plot(current.raster)
length(remain.forest5)
area(remain.forest5)


############################### GET PATCH STATS ###########################################

#Import town rasters
#raster_list_1 was in short, _2 was in extra, _3 was all the other ones
setwd('F:/David/Myanmar/ArcMap_working_folder/Hansen_2016/rasters/town_20N_90E')
raster_list <- lapply(Sys.glob("raster_*.tif"), raster)

folder = paste0(getwd(), "/data/fragstats/intermediate_products/30N_100E/")

#rename files so that they are processed in numeric order, not string order
files <- vector("list", n_townships)
for(i in 1:n_townships) {
  raster <- raster(paste0(folder, "raster_", i, ".tif"))
  files[i] <- raster
}

plot(raster_list[[93]])

#raster_list <- lapply('C:/Users/KlingesD/short', read.dta)
raster_list <- c(files)



#readAll(getValues(remain.forest5))

#attribute <- names(final.poly@data)[1]
#template <- raster(nrows=nrow(grid_raster), ncols=ncol(grid_raster), ext = extent(projectExtent(grid_raster,projected)), crs = projected)
#forestCover.grid <- rasterize(final.poly, template, attribute)
#forestCover.grid<-setNames(forestCover.grid,"forestArea") # area of forest in square meters
#forestCover.grid@data@values <- forestCover.grid@data@values*900
#plot(forestCover.grid)

#writeRaster(forestCover.grid, "./Forest Cover/ForestArea", format = "GTiff")


#get patch statstics for individual 12kmx12km polygons, then join back into a spatialpolygonsdataframe
patchstat <- function(rasterlist) {
for (i in 1:length(rasterlist)){
  #poly<- Polygons(c(Polygon(towns[[i]])), 1)
  #poly.df<- SpatialPolygons(c(poly), proj4string = CRS(projection))
  #poly.df<- SpatialPolygons(c(poly), proj4string = projected)
  #print("begin crop")
  #current.raster<-crop(remain.forest5,poly.df, add = TRUE)
  print(paste("asc from raster", i))
  patches <- asc.from.raster(rasterlist[[i]])
  print("conncomplabel")
  patches <- ConnCompLabel(patches)
  print("patchstat")
  patchstats <- PatchStat(patches)
  print(paste("end patchstat", i))
  patchstats<- as.data.frame(lapply(patchstats[,2:ncol(patchstats)], FUN = mean))
  patchstats$ID <- c(i)
  #write.csv(patchstats, paste0("patchstats_", i, ".csv"))
  
  if (i==1){
    final.stats<-patchstats
  }else{
    final.stats<-rbind(final.stats, patchstats)
  }
  #joined.data2<-SpatialPolygonsDataFrame(poly.df,patchstats, match.ID = FALSE)
  #if (i==1){
  #  final.poly2<-joined.data2
  #}else{
  #  final.poly2<-rbind(final.poly2, joined.data2)
  #}
  }
  return(final.stats)
}

time <- system.time({
  statout <- patchstat(raster_list)
})
time

statout

write.csv(statout, "./data/fragstats/finished_products/30N_100E/Myanmar_30N_100E_fragstats.csv")





#Combine patchstat data
setwd('F:/David/Myanmar/ArcMap_working_folder/Hansen_2016/fragdata')
patchstat_total <- lapply(Sys.glob("patchstats_*.csv"), read.csv)


#now get the patch stats calculated into a stack of rasters with each attribute corresponding to a band in the raster
for (i in 1:length(names(final.poly@data))){
  attribute <- names(final.poly@data)[i]
  template <- raster(nrows=nrow(grid_raster), ncols=ncol(grid_raster), ext = extent(grid_raster), crs = projected)
  band <- rasterize(final.poly, template, attribute)
  band<-setNames(band,attribute)
  if (i == 1){
    fragment.grid <- band
  }else{
    fragment.grid<- stack(c(fragment.grid, band))
  }
}
fragment.grid <- subset(fragment.grid, subset = c("n.cell", "n.edges.perimeter"))
writeRaster(fragment.grid, "./Forest Cover/fragstats", format = "GTiff")