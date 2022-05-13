# using R 4.0.3
# using raster instead of terra cos terra seems to create lots of 
# tmp files that cannot be removed and clog up memory...
# but raster takes FOREVER so moving back to terra but doing as task array
library(terra)
setwd('/shared/edwards_lab1/User/bop19jss/Ch3-biodiversity')

# get the raster list 
args <- commandArgs(trailingOnly = TRUE)

# function for the actual processing
crop_aoh <- function(rasterFile){
  # read in raster
  r <- rast(rasterFile)
  # cut out padding to speed up processing.
  r <- trim(r, value=0) 
  # get the species name 
  spName <- gsub("_", " ", gsub("^[^-]*-([^.]+).*", "\\1", basename(rasterFile)))
  # mask species distribution to forest 2018 extent (which was done for elevation)
  elevMask <- elevationRaster
  elevMask <- crop(elevMask, r)
  r <- mask(r, mask=elevMask, maskvalues=NA, updatevalue=0)
  # clip to altitude limits
  elevLower <- speciesData[which(speciesData$Species==spName),'Elev_lower']
  elevUpper <- speciesData[which(speciesData$Species==spName),'Elev_upper']
  # if there is lower and upper bounds
  if(!is.na(elevLower) & !is.na(elevUpper)){
    elevMask[elevMask<elevLower] <- NA
    elevMask[elevMask>elevUpper] <- NA
    r <- mask(r, mask=elevMask, maskvalues=NA, updatevalue=0)
  } else if(is.na(elevLower) & !is.na(elevUpper)){
    # if there is upper bound
    elevMask[elevMask>elevUpper] <- NA
    r <- mask(r, mask=elevMask, maskvalues=NA, updatevalue=0)
  } else if(!is.na(elevLower) & is.na(elevUpper)){
    # if there is lower bound
    elevMask[elevMask<elevLower] <- NA
    r <- mask(r, mask=elevMask, maskvalues=NA, updatevalue=0)
  } # if lower and upper bound are NA, then leave it
  # change NAs to 0s
  r[is.na(r)] <- 0
  # write out the raster for the species
  writeRaster(r, filename=paste0(pathOut, '/', basename(rasterFile)))
  # clear temporary files 
  terra::tmpFiles(current=TRUE, old=TRUE, orphan=TRUE, remove=TRUE)
}

# required files
elevationRaster <- rast('./elevation_TropForResampled_54009_maskedForest2018.tif')
speciesData <- read.csv('./FinalForestSpeciesList_BufferedElevRange.csv')
rasterList <- readLines(args[1])
pathOut = paste0(dirname(rasterList[1]), '-AOH') 

# filter out species files that have already been rasterised. 
alrPresent <- unname(sapply(list.files(pathOut, pattern='\\.tif'), function(x) strsplit(basename(x), '\\.')[[1]][1]))
allFiles <- unname(sapply(rasterList, function(x) strsplit(basename(x), '\\.')[[1]][1]))
toDo <- unname(sapply(setdiff(allFiles, alrPresent), function(x) paste0(dirname(rasterList[1]), '/', x, '.tif')))

cat('processing', length(toDo), 'files in this task', sep=" ", '\n')

for (i in 1:length(toDo)){
  cat(i, toDo[i], sep=" ", '\n')
  # tryCatch
  tryCatch({
    crop_aoh(toDo[i])
  }, error=function(e){cat("ERROR: ", conditionMessage(e), "\n")})
  terra::tmpFiles(current=TRUE, orphan=TRUE, old=TRUE, remove=TRUE)
}