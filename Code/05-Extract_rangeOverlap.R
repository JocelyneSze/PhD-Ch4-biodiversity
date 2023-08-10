#### ================ Calculate AOH range size ================ ####
# Note: done in HPC ShARC as task array for each species using R v4.0.3
# For each species' AOH raster, count the number of AOH pixels 
# for each protection type, where 0=unprotected, 1=PA, 2=IL, 3=PIA

library(terra)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
# terra v1.3.22, GDAL v3.3.1, proj v7.1.0, geos v3.6.1, sqlite v3.32.3
# dplyr v1.0.7, readr v1.4.0, tidyr v1.1.4, stringr v1.4.0

#### FUNCTION to calculate how much of range falls within each protection type ######
calculate_range <- function(species){
  # read in raster
  speciesFile <- rast(paste0('./Forest2020-AOH/', species, '.tif'))
  # extract sum of range presence for each protection type by
  # masking the types with species file
  rangePresence <- mask(types, speciesFile, inverse=TRUE, maskvalue=1)
  DF <- freq(rangePresence)
  DF <- as.data.frame(DF) %>% 
    select(-layer) 
  return(DF)
}

# ---- extract range values ----
setwd('/file/path')

# args[1] is the set of files to do in this task
args <- commandArgs(trailingOnly = TRUE)
setNo <- args[1]

# list of files to do
aohFiles <- readLines("./calcRange20List.txt")
# split the files to do into groups of 15. 
splitFiles <- split(aohFiles, ceiling(seq_along(aohFiles)/15))
aohFiles <- splitFiles[[setNo]]
# get the names in the same format as in the FinalForestSpeciesList file
speciesList <- unlist(lapply(aohFiles, function(x) gsub("_", " ", gsub("^[^-]*-([^.]+).*", "\\1", strsplit(x, "\\.")[[1]][1]))))
# check if species has already been done
finishedFile <- read_csv('./ForestSpeciesList_AOH2020.csv')
speciesList <- speciesList[!(speciesList %in% finishedFile$Species)]
aohFiles <- str_subset(aohFiles, gsub(" ", "_", speciesList))

# list of species file
speciesData <- read_csv('./FinalForestSpeciesList_BufferedElevRange.csv') 

# protection type raster
types <- rast("./WDPA_IL_PIA_buffered_EPSG4326.tif")

for(i in 1:length(aohFiles)){
  tryCatch({
    species <- aohFiles[i]
    cat(i, species, "\n")
    DF <- calculate_range(species)
    finalDF <- DF %>% 
      expand(value=0:3) %>% 
      left_join(DF) %>% 
      replace_na(list(count=0))
    # get the species name to search in database
    spName <- gsub("_", " ", gsub("^[^-]*-([^.]+).*", "\\1", species))
    # get the species info and key in range values
    spDF <- speciesData %>%
      filter(Species %in% spName) %>%
      mutate(Unprotected_nPixels = filter(finalDF, value==0)$count,
             PA_nPixels = filter(finalDF, value==1)$count,
             IL_nPixels = filter(finalDF, value==2)$count,
             PIA_nPixels = filter(finalDF, value==3)$count)
    write_csv(spDF, './ForestSpeciesList_AOH2020.csv', append=TRUE) 
  }, 
  error=function(e){cat("ERROR: ", conditionMessage(e), "\n")})
  terra::tmpFiles(current=TRUE, orphan=TRUE, old=TRUE, remove=TRUE)
}

cat('finished writing this set of species info', '\n')

