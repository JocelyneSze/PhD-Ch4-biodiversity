#### ========= Create threat score/range-size rarity rasters ========= ####
# Note: done in HPC ShARC as task array, with 25 files/task using R v4.0.3
# requested mem 160GB
# For each species, take its AOH raster (binary 1/0 for presence/absence) 
# and replace presence with its threat score value or range-size rarity 
# (inverse of AOH range)

library(terra)
# terra v1.3.22, GDAL v3.3.1, proj v7.1.0, geos v3.6.1, sqlite v3.32.3

setwd('/file/path')

## I would need this to loop through for each species 
args <- commandArgs(trailingOnly = TRUE) 
# args[1] would be SGE_TASK_ID 1 - 475 for the 475 sets of 25 species

# get species files
aohFiles <- list.files('./Forest2020-AOH', pattern='\\.tif$', full.names=TRUE)
# split files into 475 sets of 25 
splitFiles <- split(aohFiles, ceiling(seq_along(aohFiles)/25))
# obtain just the 25 files for this job
aohFiles <- splitFiles[[args[1]]]

# reference file with species name and threat score
refFile <- read.csv("ForestSpeciesList_AOH2020_ThreatScore.csv")

# print some info
cat('this is job no.', args[[1]], "\n")

for(i in 1:length(aohFiles)){
  cat('species no.', i, "\n")
  
  # get species name from the filename
  species <- gsub("\\.tif", "", basename(aohFiles[i])) 
  cat(species, "\n")
  # get the species name to search in refFile
  spName <- gsub("_", " ", gsub("^[^-]*-([^.]+).*", "\\1", species))
  
  # get class from refFile
  vertClass <- refFile[which(refFile$Species==spName), 'Class']
  
  # grab the inverse range from the file
  invRange <- refFile[which(refFile$Species==spName),'inverseRange']
  # if this species doesn't have remaining habitat left, skip it.
  if(length(invRange)==0) next
  
  # also grab the threat score to know whether this sp is threatened or not
  threat <- refFile[which(refFile$Species==spName),'ThreatScore']
  
  # read in the raster file
  r <- rast(aohFiles[i])
  # mask this file
  tmp20 <- rast('./elevation_TropForResampled_maskedForest2020.tif')
  r <- mask(r, tmp20)
  
  # convert the range presence value (1) to invRange value
  r2 <- r
  r2[r2==1 ] <- invRange
  # convert the range presence value (1) to threat score value
  r[r==1] <- threat
  
  # for threat score and inverse range, save each class and IUCN cat separately
  writeRaster(r, paste0("./Forest2020/ThreatScore/", vertClass, "/ThreatScore", threat, "_", species, ".tif"))
  writeRaster(r2, paste0("./Forest2020/InverseRange/", vertClass, "/ThreatScore", threat, "_", species, ".tif"))
  
}

cat('finished for loop', '\n')
