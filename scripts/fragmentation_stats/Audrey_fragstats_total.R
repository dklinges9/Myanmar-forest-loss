#This is a script to create a set of environmental variables related to panda distribution during the fourth survey. #
#The output is a grid of points over the survey area (for extraction of variables), spaced by the gridSize variable  #
#Variables extracted for this grid include distance to nearest third survey point and distance to core area.         #
#Other variables output are area of old reserves within the grid (m^2), area of new reserves within the grid (m^2),  #
#forest cover (from Hansen), forest fragmentation (from recalculated Hansen), and light pollution                    #
#                                                                                                                    #
#Audrey Lothspeich                                                                                                   #
#2018                                                                                                                #

library(rgdal)
library(sp)
library(FNN)
library(rgeos)
library(raster)
library(SDMTools)

setwd("F:/HaydeeHY/Pandastats")

gridSize = 12000 #in meters
cover.percent = 25

#Load data #####

panda01 <- as.data.frame(read.csv("3rd_survey_points.csv", header = TRUE, sep = ','))
panda01 <- panda01[,4:5]
panda01 <- SpatialPointsDataFrame(coords = cbind(panda01$longitudex, panda01$latitudey),panda01,
                                  proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

mount <- readOGR('./Mt_Qinglin/mt_Qinling_PR.shp', layer = "mt_Qinling_PR")
#project it to a projection that maintains distance, so that 12km grid size can be specified. WGS 84 / World Equidistant Cylindrical
WGS<-proj4string(mount) #unprojected lat/long WGS1984
projected = '+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'
bioclim<-raster("./CHELSA_Bioclim/CHELSA_Bio_Average2004_2013.tif")

bioclim <- projectRaster(bioclim, crs = CRS(projected))
bioclim.poly <- rasterToPolygons(bioclim)

grid <- makegrid(bioclim.poly, cellsize = gridSize)
grid <- SpatialPoints(grid, proj4string = CRS(projected))

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = TRUE)
}

grid<- gClip(grid, bioclim.poly)

plot(grid, pch = '.')
grid_unprojected<-spTransform(grid, WGS)
projectExtent(grid_unprojected, WGS)

grid_df<- SpatialPointsDataFrame(grid, as.data.frame(cbind(c(rep(NaN, length(grid))))), proj4string = CRS(projected))
grid_raster <- rasterFromXYZ(grid_unprojected, crs = WGS)
grid.poly <- rasterToPolygons(grid_raster)

density <- raster('./Core Areas/KernelDensity_PluginBW_Resample_Clip.tif')

reserves = readOGR("F:/HaydeeHY/Pandastats/Protected Areas", "reserves_wgs84")
#I dissolved the reserve area in arcmap to get a more accurate "reserve area" later in the script.
#Otherwise it gets the area of only one of the reserves in the cell, regardless of how many areas are within the cell
dissolved.reserves = readOGR("F:/HaydeeHY/Pandastats/Protected Areas", "dissolved_reserves_wgs84")
dissolved.new.reserves = readOGR("F:/HaydeeHY/Pandastats/Protected Areas", "dissolved_new_reserves_wgs84")


tree.cover <- raster("./Forest Cover/Hansen_GFC-2016-v1.4_treecover2000_40N_100E.tif")
tree.cover <- crop(tree.cover, grid.poly)

gain <- raster("./Forest Cover/Hansen_GFC-2016-v1.4_gain_40N_100E.tif")
gain <- crop(gain, grid.poly)

loss.year<-raster("./Forest Cover/Hansen_GFC-2016-v1.4_lossyear_40N_100E.tif")
loss.year <- crop(loss.year, grid.poly)

light <- raster("./Light Pollution/LightPollution.tif", band = 1)

bamboo <- raster("./Bamboo model outcome/Predicted_Bamboo_10yrclimate_4thSurvey.tif")

#Calculate distance to nearest 3rd survey point using nearest neighbor#####

panda01 <- spTransform(panda01, '+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ')
survey_dist<-get.knnx(panda01@coords, grid@coords, k=1, algorithm = c("kd_tree"))

#write values to randompoints
names(grid_df@data)<-"survey_dist" #create the column
for (i in 1:nrow(grid_df)){
  grid_df@data[i,1] <- survey_dist$nn.dist[i,]
}


#Calculate distance to the "core area" as defined by kernel density (see script 13) #####

#convert core raster to a vector from which to calculate euclidean distance
core.dist <- function(vector){
  for (i in 1:length(vector)){
    if (!is.na(vector[i])){ 
      vector[i] <- 1 
    }
  }
  return(vector)
}
core <- calc(density, core.dist)
core <- rasterToPolygons(core)
core <- spTransform(core,'+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

plot(core)
plot(grid, add = TRUE)

#Write the values for core distance
grid_df@data$core_dist <- NaN #create the column
for (i in 1:nrow(grid_df)){
  grid_df@data[i,]$core_dist <- gDistance(core, grid_df[i,])
}

#Save random points in the same projection as the original data #####
grid_df <- spTransform(grid_df, WGS)
writeOGR(grid_df, "./Dispersal/RandomPointsGrid_clipped", "RandomPoints", "ESRI Shapefile", overwrite_layer = TRUE)

#Create survey and core rasters from the random points grid #####
surv.dist.grid <- rasterFromXYZ(cbind(grid_df@coords[,1], grid_df@coords[,2], grid_df@data[,1]),
                                crs = WGS)
surv.dist.grid@data@names <- "surv.dist"
plot(surv.dist.grid)
writeRaster(surv.dist.grid, "./Dispersal/Survey3DistanceGrid_clipped", format = "GTiff")

core.dist.grid <- rasterFromXYZ(cbind(grid_df@coords[,1], grid_df@coords[,2], grid_df@data[,2]),
                                crs = WGS)
core.dist.grid@data@names <- "core.dist"
plot(core.dist.grid)
writeRaster(core.dist.grid, "./Dispersal/CoreDistanceGrid_clipped", format = "GTiff")

#Get the area of reserves area per cell #####
reserve.grid <- raster(extent(grid_raster), nrows=nrow(grid_raster), ncols=ncol(grid_raster), ext=extent(grid_raster), crs=WGS)

reserve.poly <- rasterToPolygons(reserve.grid)
reserve.poly <- spTransform(reserve.poly,'+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
dissolved.reserves <- spTransform(dissolved.reserves, '+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

getArea <- function(vector, reservefile){
  for (i in 1:nrow(vector)){
    shape <- vector[i,]
    if(!is.null(intersect(shape, reservefile))){
      intersection <- intersect(shape, reservefile)
      vector@data$layer[i] <- area(intersection)
    }
  }
  return(vector)
}
reserve.poly<-getArea(reserve.poly, dissolved.reserves)
reserve.poly<-spTransform(reserve.poly, WGS)
reserve.grid<-rasterize(reserve.poly, reserve.grid, reserve.poly$layer)

for (i in 1:length(reserve.grid)){
  if (is.na(reserve.grid[i])){
    reserve.grid[i]<-0
  }
}

#Save raster
writeRaster(reserve.grid, "./Protected Areas/ReserveGrid_clipped", format = "GTiff")

#Get area of new reserves per cell#####
new.reserve.grid <- raster(extent(grid_raster), nrows=nrow(grid_raster), ncols=ncol(grid_raster), ext=extent(grid_raster), crs=WGS)

new.reserve.poly <- rasterToPolygons(new.reserve.grid)
new.reserve.poly <- spTransform(new.reserve.poly,'+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
dissolved.new.reserves <- spTransform(dissolved.new.reserves, '+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

new.reserve.poly<-getArea(new.reserve.poly, dissolved.new.reserves)
new.reserve.poly<-spTransform(new.reserve.poly, WGS)
new.reserve.grid<-rasterize(new.reserve.poly, new.reserve.grid, new.reserve.poly$layer)

for (i in 1:length(new.reserve.grid)){
  if (is.na(new.reserve.grid[i])){
    new.reserve.grid[i]<-0
  }
}

#save raster
writeRaster(new.reserve.grid, "./Protected Areas/NewReserveGrid_clipped", format = "GTiff")

#Get forest cover patch statistics per cell #####

#get forest cover to the decided threshold (25%)
get.threshold <- function(vector){
  for (i in 1:length(vector)){
    if (vector[i] < cover.percent){
      vector[i]<-NA
    }
  }
  return(vector)
}
thresh.tree.cover<- calc(tree.cover, get.threshold)

#clip the "forest" to only places where no loss was recorded during the survey years
remain.forest <- mask(thresh.tree.cover, loss.year, maskvalue = c(1:12), updatevalue = 0)
remain.forest <- mask(remain.forest, loss.year, maskvalue = c(1:12), inverse = TRUE, updatevalue = 1)

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

remain.forest<-overlay(gain, remain.forest, fun = gain.check)

remain.forest<-projectRaster(remain.forest, res = 30, crs = projected, method = 'ngb')

remain.forest<-trim(remain.forest)

#get the simple area of forest per raster
count.cells<- function(vector){ #function to count the number of light-cells per individual raster
  count<-0
  for (i in 1:length(vector)){
    if (vector[i]==1){
      count<-count+1
    }
  }
  return(c(count))
}

countInGrid <- function(raster){
  for (i in 1:length(grid)){ #get count of forest raster cells for each grid cell.
    point <- as.numeric(grid@coords[i,])
    bbox <- rbind(cbind(point[1]+(gridSize/2), point[2]+(gridSize/2)),cbind(point[1]+(gridSize/2), point[2]-(gridSize/2)), 
                  cbind(point[1]-(gridSize/2),point[2]-(gridSize/2)), cbind(point[1]-(gridSize/2), point[2]+(gridSize/2)), 
                  cbind(point[1]+(gridSize/2), point[2]+(gridSize/2)))
    poly<- Polygons(c(Polygon(bbox)), i)
    poly.df<- SpatialPolygons(c(poly), proj4string = CRS(projected))
    plot(raster)
    plot(poly.df, add = TRUE)
    current.raster<-crop(raster,poly.df)
    count<- as.data.frame(count.cells(current.raster@data@values))
    joined.data<-SpatialPolygonsDataFrame(poly.df,count, match.ID = FALSE)
    if (i==1){
      final.poly<-joined.data
    }else{
      final.poly<-rbind(final.poly, joined.data)
    }
  }
  return(final.poly)
}

final.poly <- countInGrid(remain.forest)

attribute <- names(final.poly@data)[1]
template <- raster(nrows=nrow(grid_raster), ncols=ncol(grid_raster), ext = extent(projectExtent(grid_raster,projected)), crs = projected)
forestCover.grid <- rasterize(final.poly, template, attribute)
forestCover.grid<-setNames(forestCover.grid,"forestArea") # area of forest in square meters
forestCover.grid@data@values <- forestCover.grid@data@values*900
plot(forestCover.grid)

writeRaster(forestCover.grid, "./Forest Cover/ForestArea_clipped", format = "GTiff")

#get patch statstics for individual 12kmx12km polygons, then join back into a spatialpolygonsdataframe

for (i in 1:nrow(grid_df)){
  point <- as.numeric(grid@coords[i,])
  bbox <- rbind(cbind(point[1]+(gridSize/2), point[2]+(gridSize/2)),cbind(point[1]+(gridSize/2), point[2]-(gridSize/2)), 
                cbind(point[1]-(gridSize/2),point[2]-(gridSize/2)), cbind(point[1]-(gridSize/2), point[2]+(gridSize/2)), 
                cbind(point[1]+(gridSize/2), point[2]+(gridSize/2)))
  poly<- Polygons(c(Polygon(bbox)), i)
  poly.df<- SpatialPolygons(c(poly), proj4string = CRS(projected))
  plot(remain.forest)
  plot(poly.df, add = TRUE)
  current.raster<-crop(remain.forest,poly.df, add = TRUE)
  patches <- asc.from.raster(current.raster)
  patches <- ConnCompLabel(patches)
  patchstats <- PatchStat(patches)
  patchstats<- as.data.frame(lapply(patchstats[,2:ncol(patchstats)], FUN = mean))
  joined.data<-SpatialPolygonsDataFrame(poly.df,patchstats, match.ID = FALSE)
  if (i==1){
    final.poly<-joined.data
  }else{
    final.poly<-rbind(final.poly, joined.data)
  }
}

#now get the patch stats calculated into a stack of rasters with each attribute corresponding to a band in the raster

for (i in 1:length(names(final.poly@data))){
  attribute <- names(final.poly@data)[i]
  template <- raster(nrows=nrow(grid_raster), ncols=ncol(grid_raster), ext = extent(grid_raster))
  template <- projectRaster(template, crs = projected)
  band <- rasterize(final.poly, template, attribute, update = T)
  band<-setNames(band,attribute)
  if (i == 1){
    fragment.grid <- band
  }else{
    fragment.grid<- stack(c(fragment.grid, band))
  }
}
fragment.grid <- subset(fragment.grid, subset = c("n.cell", "n.edges.perimeter"))
writeRaster(fragment.grid, "./Forest Cover/fragstats_clipped", format = "GTiff")