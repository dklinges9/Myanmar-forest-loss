
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
grid.projected <- spTransform(grid, projected)

#get the simple area of forest per raster
count.cells<- function(vector){ #function to count the number of light-cells per individual raster
  count<-0
  for (i in 1:length(vector)){
    if (vector[i]==1)
      count<-count+1
  }
  return(c(count))
}

countInGrid <- function(raster){
  for (i in 1:480){ #get count of light raster cells for each grid cell.
    point <- as.numeric(grid.projected@coords[i,])
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

writeRaster(forestCover.grid, "./Forest Cover/ForestArea", format = "GTiff")

#get patch statstics for individual 12kmx12km polygons, then join back into a spatialpolygonsdataframe

for (i in 1:480){
  point <- as.numeric(grid.projected@coords[i,])
  bbox <- rbind(cbind(point[1]+(gridSize/2), point[2]+(gridSize/2)),cbind(point[1]+(gridSize/2), point[2]-(gridSize/2)), 
                cbind(point[1]-(gridSize/2),point[2]-(gridSize/2)), cbind(point[1]-(gridSize/2), point[2]+(gridSize/2)), 
                cbind(point[1]+(gridSize/2), point[2]+(gridSize/2)))
  poly<- Polygons(c(Polygon(bbox)), i)
  poly.df<- SpatialPolygons(c(poly), proj4string = projected)
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