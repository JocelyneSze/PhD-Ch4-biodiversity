#### ===== Rasterise shapefiles and obtain AOH for 2020 ===== ####
# Note: done in HPC ShARC as task array for each species using R v4.0.3
# For each species distribution shapefile,
# rasterise and crop it to forest extents in 2020
# and the species' altitudinal limits

library(terra)
# terra v1.3.22, GDAL v3.3.1, proj v7.1.0, geos v3.6.1, sqlite v3.32.3

#### FUNCTION to rasterise and crop to AOH ######
rasterise_AOH <- function(species){
  # read in shapefile
  speciesFile <- vect(paste0('./ForestVertebrateShapefiles/', species, '.gpkg'))
  
  # re-project shapefile from ESRI 54009 to EPSG 4326
  shp4326 <- project(speciesFile, "epsg:4326")
  
  # rasterise shapefile
  if(nrow(shp4326) > 1){
    # if there are more than one polygon for the species
    r_poly <- lapply(1:nrow(shp4326), function(x){terra::rasterize(shp4326[x,], tmp20, touches=TRUE, background=0)})
    # using touches=TRUE so even if polygon is small such that it doesn't cross centre of cell
    # the cell will still be considered as present   
    r_poly <- terra::mosaic(src(r_poly), fun="max")
  } else if(nrow(shp4326) == 1){ 
    # if there is only one polygon
    r_poly<- terra::rasterize(shp4326, tmp20, touches=TRUE, background=0)
  }
  
  # get the species name to search in database
  spName <- gsub("_", " ", gsub("^[^-]*-([^.]+).*", "\\1", species))
  
  # species elevational limits
  elevLower <- speciesData[which(speciesData$Species==spName),'Elev_lower']
  elevUpper <- speciesData[which(speciesData$Species==spName),'Elev_upper']
  
  # clip to altitude limits for 2020
  cat("clipping to 2020 AOH", "\n")
  elevMask <- tmp20
  aoh_2020 <- r_poly
  # if there is lower and upper bounds
  if(!is.na(elevLower) & !is.na(elevUpper)){
    elevMask[elevMask<elevLower] <- NA
    elevMask[elevMask>elevUpper] <- NA
    aoh_2020 <- mask(aoh_2020, mask=elevMask, maskvalues=NA, updatevalue=0)
  } else if(is.na(elevLower) & !is.na(elevUpper)){
    # if there is upper bound
    elevMask[elevMask>elevUpper] <- NA
    aoh_2020 <- mask(aoh_2020, mask=elevMask, maskvalues=NA, updatevalue=0)
  } else if(!is.na(elevLower) & is.na(elevUpper)){
    # if there is lower bound
    elevMask[elevMask<elevLower] <- NA
    aoh_2020 <- mask(aoh_2020, mask=elevMask, maskvalues=NA, updatevalue=0)
  } # if lower and upper bound are NA, then leave it
  # mask unnecessary areas to reduce file size
  aoh_2020 <- mask(aoh_2020, tmp20)
  # write out the raster for the species
  writeRaster(aoh_2020, filename=paste0('./Forest2020-AOH/', species, '.tif'))
} 

# ---- extract AOH -----
setwd('/file/path')

# get the index of species to do
args <- commandArgs(trailingOnly = TRUE)

# getting the one species from the index file
species <- args[1]

# elevation raster where cells not covered by forest in 2010 and 2018 have been masked out
tmp20 <- rast('./elevation_TropForResampled_maskedForest2020.tif')

# species data to get elevation range
speciesData <- read.csv('./FinalForestSpeciesList_BufferedElevRange.csv')

# template data for NAs in oceans
tmp <- rast("./gadm36_Countries_complete_resampled.tif")

cat(species, "\n")

tryCatch({
  rasterise_AOH(species)}, 
  error=function(e){cat("ERROR: ", conditionMessage(e), "\n")})
terra::tmpFiles(current=TRUE, orphan=TRUE, old=TRUE, remove=TRUE)

cat("finished", "\n")