#### ============ Get country data frame ============== ####
# Note: done in HPC ShARC as task array for each country 
# requested mem = 50GB
# Clip and mask raster to country boundary and convert to data frames
# for species richness/threat score/range-size rarity
# for within IPL, 0-10km buffer area outside IPL, 
# 10-50km buffer area outside IPL, and 
# outside 50km of IPL for all taxa combined
# and only within IPL and 0-10km buffer area for separate taxa

library(terra)
library(data.table)
# terra v1.3.22, GDAL v3.3.1, proj v7.1.0, geos v3.6.1, sqlite v3.32.3
# data.table v1.14.2

setwd('/file/path')

# list of rasters
rasterList <- c("./Permutation/All_taxa.tif",
                "./Permutation/Sep_taxa_SR.tif",
                "./Permutation/Sep_taxa_TS.tif",
                "./Permutation/Sep_taxa_IR.tif")

## COUNTRY DATA
## one country for each task array job, so get the country index
args <- commandArgs(trailingOnly = TRUE) 
# args[1] is a number from 1-55 for the countries with IPL in tropics
no <- as.numeric(args[1])

countries <- c("ARG","AUS","BDI","BEN","BGD","BLZ","BOL","BRA","CAF","CHN","CIV",
          "CMR","COD","COG","COL","CRI","DMA","ECU","ETH","GAB","GTM",
          "GUF","GUY","HND","IDN","IND","KEN","KHM","LAO","LKA","MEX","MMR","MYS","NCL",
          "NGA","NIC","NPL","PAK","PAN","PER","PHL","PRY","RWA","SLV",
          "SUR","TGO","THA","TWN","TZA","UGA","USA","VEN","VNM")

## country polygon
gadm <- vect("./gadm36_Countries_epsg4326_tropical.gpkg")
gadm <- gadm[which(gadm$GID_0 == countries[no]),]
cat(args[1], gadm$NAME_0, "\n")

## for each raster file
for(i in 1:length(rasterList)){
  # read in target raster
  id <- strsplit(basename(rasterList[i]), "\\.")[[1]][1]
  cat(id, '\n')
  tStack <- rast(rasterList[i])
  
  # clip target rasters to country size
  gadmStack <- crop(tStack, gadm)
  gadmStack <- mask(gadmStack, gadm)
  
  # if all the values in this country are NA, end task
  if(all(is.na(values(gadmStack)))){
    cat("meh, all NA values", "\n")
  } else {
    # initiate a list to catch the output
    df <- list()
    # for each of the target rasters, get the data frame
    for(j in 1:nlyr(gadmStack)){
      if(all(is.na(values(gadmStack[[j]])))){
        df[[j]] <- data.frame(value = NA, scenario = names(gadmStack)[[j]])
      } else{
        df[[j]] <- as.data.frame(gadmStack[[j]])
        df[[j]]$scenario = names(gadmStack)[[j]]
        names(df[[j]]) <- c("value", "scenario")
      }
    }
    # combine into one df.
    finalDF <- rbindlist(df)
    fwrite(finalDF, paste0("./CountryData/",id,"_",gadm$GID_0,".csv"))
  }
}

cat('finished extracting for this country', '\n')

