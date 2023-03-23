#### ============= Create intermediate raster stacks ================= ####
# Note: done in HPC ShARC as task array, with 25 files/task using R v4.0.3
# requested mem 80GB
# Stack the individual species rasters in groups of 25 as an 
# intermediate stage, grouped by:
# species richness, threat score, range-size rarity for
# each vertebrate taxa (amphibians, birds, mammals, reptiles)
# as threatened or not threatened species,
# where if threat score > 4 = threatened, else not threatened

library(terra)
library(stringr)
# terra v1.3.22, GDAL v3.3.1, proj v7.1.0, geos v3.6.1, sqlite v3.32.3
# stringr v1.4.0

setwd('/file/path')

args <- commandArgs(trailingOnly = TRUE) 
# args[[1]] is either SpeciesRichness or ThreatScore or InverseRange
# args[[2]] is either AMPHIBIA, AVES, MAMMALIA, or REPTILIA
# args[[3]] is task id. for threatened: amph=32; birds=35; mam=33; rep=35
# for not threatened: amph=27; birds=222; mam=37; rep=47
# args[[4]] is either ThreatenedSpecies or NotThreatenedSpecies

if (args[[4]] == 'ThreatenedSpecies'){
  # ThreatScore>4 = threatened species
  listFiles <- list.files(paste0("./Forest2020/", args[[1]], "/", args[[2]]),
                        full.names=TRUE, recursive=FALSE)
  listFiles <- str_subset(listFiles, "ThreatScore([6-8]|[12][0-9]|3[02])")
} else if(args[[4]] == "NotThreatenedSpecies"){
  listFiles <- list.files(paste0("./Forest2020/", args[[1]], "/", args[[2]]),
                        full.names=TRUE, recursive=FALSE)
  listFiles <- str_subset(listFiles, "ThreatScore[0-4]_")
}

# split files into sets of 25
splitFiles <- split(listFiles, ceiling(seq_along(listFiles)/25))
listFiles <- splitFiles[[args[3]]]

cat('this is for',args[[1]], args[[2]], args[[4]], 'job no.', args[[3]], "\n")
cat('number of files', length(listFiles), "\n")
cat(head(listFiles), '\n')
cat('reading in files', '\n')
rr <- lapply(listFiles, rast)

cat('sum the list', '\n')
finalRaster <- Reduce("+", rr) 
finalRaster
cat('write raster for this group', '\n')
writeRaster(finalRaster, paste0('./Forest2020/', args[[1]], '_', args[[2]], '_', args[[4]], '_', args[[3]], '.tif'))

cat('finished', '\n')