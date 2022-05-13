# uses R 4.0.3
# use task array to rasterise shapefiles (first projecting to longlat)
# then crop to AOH within the same function
# allows use of terra package. requires at least terra 1.3.4 (which has src function)
# v2 (19 Sep 2021). to do in bulk rather than indv
# 4 May 2022. do indv cos else task array perpetually stuck in queue.
library(terra)

setwd('/shared/edwards_lab1/User/bop19jss/Ch3-biodiversity')

# get the index of species to do
args <- commandArgs(trailingOnly = TRUE)
# rasterList <- readLines("./RasterList_2022-05-04.txt")

# getting the one species from the index file
species <- args[1]

# elevation raster where cells not covered by forest in 2010 and 2018 have been masked out
tmp10 <- rast('./elevation_TropForResampled_maskedForest2010.tif')
tmp18 <- rast('./elevation_TropForResampled_maskedForest2018.tif')

# species data to get elevation range
speciesData <- read.csv('./FinalForestSpeciesList_BufferedElevRange.csv')

## function to rasterise shapefiles
rasterise_AOH <- function(species){
  # read in shapefile
  speciesFile <- vect(paste0('./ForestVertebrateShapefiles/', species, '.gpkg'))
  
  # re-project shapefile from ESRI 54009 to EPSG 4326
  shp4326 <- project(speciesFile, "epsg:4326")
  
  # rasterise shapefile
  if(nrow(shp4326) > 1){
    # if there are more than one polygon for the species
    r_poly <- lapply(1:nrow(shp4326), function(x){terra::rasterize(shp4326[x,], tmp10, touches=TRUE, background=0)})
    # using touches=TRUE so even if polygon is small such that it doesn't cross centre of cell
    # the cell will still be considered as present   
    r_poly <- terra::mosaic(src(r_poly), fun="max")
  } else if(nrow(shp4326) == 1){ 
    # if there is only one polygon
    r_poly<- terra::rasterize(shp4326, tmp10, touches=TRUE, background=0)
  }
  
  # get the species name to search in database
  spName <- gsub("_", " ", gsub("^[^-]*-([^.]+).*", "\\1", species))
  
  # species elevational limits
  elevLower <- speciesData[which(speciesData$Species==spName),'Elev_lower']
  elevUpper <- speciesData[which(speciesData$Species==spName),'Elev_upper']
  
  # clip to altitude limits for 2010
  if(!file.exists(paste0("./Forest2010-AOH/", species, ".tif"))){
    cat("clipping to 2010 AOH", "\n")
    elevMask <- tmp10
    aoh_2010 <- r_poly
    if(!is.na(elevLower) & !is.na(elevUpper)){
      # if there is lower and upper bounds
      elevMask[elevMask<elevLower] <- NA
      elevMask[elevMask>elevUpper] <- NA
      aoh_2010 <- mask(aoh_2010, mask=elevMask, maskvalues=NA, updatevalue=0)
    } else if(is.na(elevLower) & !is.na(elevUpper)){
      # if there is upper bound
      elevMask[elevMask>elevUpper] <- NA
      aoh_2010 <- mask(aoh_2010, mask=elevMask, maskvalues=NA, updatevalue=0)
    } else if(!is.na(elevLower) & is.na(elevUpper)){
      # if there is lower bound
      elevMask[elevMask<elevLower] <- NA
      aoh_2010 <- mask(aoh_2010, mask=elevMask, maskvalues=NA, updatevalue=0)
    } # if lower and upper bound are NA, then leave it
    # change NAs to 0s so I can sum up for species richness
    aoh_2010[is.na(aoh_2010)] <- 0
    # write out the raster for the species
    writeRaster(aoh_2010, filename=paste0('./Forest2010-AOH/', species, '.tif'))
  }
  
  # clip to altitude limits for 2018
  cat("clipping to 2018 AOH", "\n")
  elevMask <- tmp18
  aoh_2018 <- r_poly
  # if there is lower and upper bounds
  if(!is.na(elevLower) & !is.na(elevUpper)){
      elevMask[elevMask<elevLower] <- NA
      elevMask[elevMask>elevUpper] <- NA
      aoh_2018 <- mask(aoh_2018, mask=elevMask, maskvalues=NA, updatevalue=0)
    } else if(is.na(elevLower) & !is.na(elevUpper)){
      # if there is upper bound
      elevMask[elevMask>elevUpper] <- NA
      aoh_2018 <- mask(aoh_2018, mask=elevMask, maskvalues=NA, updatevalue=0)
    } else if(!is.na(elevLower) & is.na(elevUpper)){
      # if there is lower bound
      elevMask[elevMask<elevLower] <- NA
      aoh_2018 <- mask(aoh_2018, mask=elevMask, maskvalues=NA, updatevalue=0)
    } # if lower and upper bound are NA, then leave it
    # change NAs to 0s so I can sum up for species richness
    aoh_2018[is.na(aoh_2018)] <- 0
    # write out the raster for the species
    writeRaster(aoh_2018, filename=paste0('./Forest2018-AOH/', species, '.tif'))
    } 

# cat('processing', length(speciesList), 'files in this task', sep=" ", '\n')
cat(species, "\n")

tryCatch({
  rasterise_AOH(species)}, 
  error=function(e){cat("ERROR: ", conditionMessage(e), "\n")})
terra::tmpFiles(current=TRUE, orphan=TRUE, old=TRUE, remove=TRUE)

cat("finished", "\n")