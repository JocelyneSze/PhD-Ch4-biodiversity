library(terra)

setwd('/shared/edwards_lab1/User/bop19jss/Ch3-biodiversity')

aohFiles <- list.files('./ForestVertebrateRasters-AOH', pattern='\\.tif$', full.names=TRUE) # should be 11,870 species

elevationRaster <- rast('./elevation_TropForResampled_54009_maskedForest2018.tif')
e <- ext(elevationRaster)

rr <- lapply(aohFiles, rast)

for(i in 1:length(aohFiles)){
  cat(i, '\n')

  rr[i] <- extend(rr[i], e)
}

speciesStack <- rast(rr)
finalRaster <- calc(speciesStack, sum)
writeRaster(finalRaster, './ForestVertebrates_SpRichness_54009.tif')
