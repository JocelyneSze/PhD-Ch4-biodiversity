#### ================= Process final rasters ================== ####
# Note: done in HPC ShARC in interactive mode (vmem=60G)
# From intermediate stacked rasters, create final rasters for 
# species richness/threat score/range-size rarity for 
# each vertebrate taxa (amphibia/aves/mammalia/reptilia) 
# and status (threatened/not threatened), 
# weighting by species richness for threat score and range-size rarity,
# then mask with IPL/10 km buffer/50 km buffer and all outside

library(terra)
# terra v1.3.22, GDAL v3.3.1, proj v7.1.0, geos v3.6.1, sqlite v3.32.3

setwd('/file/path')

# ---- 1. finish stacking rasters into groups ----
# 08-Stack_rasters.R grouped individual species rasters into stacks of 25 using task array
# this section combined these stacks of 25 species into groups defined by variable, taxa and status

# dataframe for no. of files (of 25 species) there should be for each group
refDF <- data.frame(variable = c(rep('SpeciesRichness',8),rep('ThreatScore',8),rep('InverseRange',8)),
                    taxa = rep(c('AMPHIBIA','AVES','MAMMALIA','REPTILIA'),6),
                    status = rep(c(rep('Threatened',4),rep('NotThreatened',4)),3),
                    noFiles = rep(c(32,35,33,35,27,221,37,47),3))

for(i in 1:nrow(refDF)){
  noFiles = refDF$noFiles[i]
  pattern <- paste0('^',refDF$variable[i],'_',refDF$taxa[i],'_',refDF$status[i],'Species')
  cat(pattern, '\n')
  # get all the files relevant for the group
  allFiles <- list.file('./Forest2020', pattern=pattern, full.names=TRUE)
  # check that the no. of files is correct
  try(if(length(allFiles)!=refDF$noFiles[i]) stop('wrong no. of files!'))
  # stack these files
  rr <- lapply(allFiles, rast)
  finalRaster <- Reduce("+", rr)
  names(finalRaster) <- paste0(refDF$variable[i],'_',refDF$taxa[i],'_',refDF$status[i])
  writeRaster(finalRaster, paste0("./Forest2020/", refDF$variable[i],'_only',refDF$taxa[i],'_',refDF$status[i], ".tif"))
  # remove the intermediate files
  file.remove(allFiles)
}

# ---- 2. combine rasters ----
library(stringr)

#### combine threatened and not threatened for each variable and taxa
get_taxaTotal <- function(variable, taxa){
  # get the file list based on the variable
  fileList <- list.files("./Forest2020", pattern=paste0(variable, "_only"),
                         full.names=TRUE)
  # grab the two files (threatened & non-threatened) 
  vert <- str_subset(fileList, taxa)
  print(vert)
  # read in rasters
  r <- lapply(vert, rast)
  # add up the two files
  finalR <- Reduce("+", r)
  names(finalR) <- paste0(variable, "_", taxa)
  # save raster
    writeRaster(finalR, paste0("./Forest2020/", variable, "_only", taxa, ".tif"))
}

allVariable = c("SpeciesRichness", "ThreatScore", "InverseRange")
allTaxa = c("AMPHIBIA", "AVES", "MAMMALIA", "REPTILIA")
for(i in 1:length(allVariable)){
  for(j in 1:length(allTaxa)){
    get_taxaTotal(allVariable[i],allTaxa[j])
  }
}

#### combine the 4 taxa for each variable
get_varTotal <- function(variable){
  # get the file list for the variable
  fileList <- list.files("./Forest2020", pattern=paste0(variable, "_only"),
                         full.names=TRUE)
  # remove files for threatened/not threatened
  vert <- str_subset(fileList, 'Threatened', negate=TRUE)
  print(vert)
  # read in rasters
  r <- lapply(vert, rast)
  # add up the two files
  finalR <- Reduce("+", r)
  names(finalR) <- paste0(variable)
  # save raster
  writeRaster(finalR, paste0("./Forest2020/", variable, ".tif"))
}

allVariable = c("SpeciesRichness", "ThreatScore", "InverseRange")
for(i in 1:length(allVariable)){
  get_varTotal(allVariable[i])
}

# ---- 3. weight threat score / inverse range by species richness ----
# do this for each taxa and for combined taxa respectively

allVariable = c("ThreatScore", "InverseRange")
files = c(".tif","_onlyAMPHIBIA.tif","_onlyAVES.tif","_onlyMAMMALIA.tif","_onlyREPTILIA.tif")

for(i in 1:length(allVariable)){
  for(j in 1:length(files)){
    cat(paste0('./Forest2020/',allVariable[i],files[j]), "\n")
    r <- rast(paste0('./Forest2020/',allVariable[i],files[j]))
    sr <- rast(paste0('./Forest2020/SpeciesRichness',files[j]))
    newR <- r/sr 
    # because SR could be 0 in some cells, there'll be additional NAs
    # but these cells need to have value 0 not NA. replace those
    # additional NAs with the 0s in sr 
    newR2 <- cover(newR, sr, values=NA, 
                  filename=gsub(".tif", "_SRweighted.tif", paste0('./Forest2020/',allVariable[i],files[j])))
  }
}
# ---- 4. mask rasters with IPL/buffer polygons ----
## to get IPL only, 10km buffer only for all taxa combined and separate 
## and 50km buffer only, and all outside IPL areas for all taxa combined
## also mask out PAs from the rasters

# PA values
pas <- rast("./WDPA_IL_PIA_buffered_EPSG4326.tif")

#### for all taxa combined 
allTaxa <- c("./Forest2020/SpeciesRichness.tif",
             "./Forest2020/ThreatScore_SRweighted.tif",
             "./Forest2020/InverseRange_SRweighted.tif")
# read in rasters
r <- rast(allTaxa)
names(r) <- c("SR_All", "TS_All", "IR_All")
# mask out PAs
target <- mask(r, pas, maskvalues=1)
# get layer of just IPLs
ipl <- vect("./IPL.gpkg")
ipl <- project(ipl, "epsg:4326")
target_ipl <- mask(target, ipl) 
names(target_ipl) <- paste0("IPL_", names(target_ipl))
# get layer of just 10km buffers
buff <- vect("./IPL_10kmBufferOnly_union_sf.gpkg")
buff <- project(buff, "epsg:4326")
target_buff <- mask(target, buff)
names(target_buff) <- paste0("buff10_", names(target_buff))
# get layer of just 50km buffer (- the 10km buffer)
buff50 <- vect("./IPL_10-50kmBufferOnly_union_sf.gpkg")
buff50 <- project(buff50, "epsg:4326")
target_buff50 <- mask(target, buff50)
names(target_buff50) <- paste0("buff50_", names(target_buff50))
# get layer of all outside IPL (- the 10km and 50km buffer)
ipl50 <- vect("./IPL_inc50kmBuffer_union_sf.gpkg")
ipl50 <- project(ipl50, "epsg:4326")
target_out <- mask(target, ipl50, inverse=TRUE)
names(target_out) <- paste0("out_", names(target_out))
# stack them together keeping species richness, threat score and 
# inverse range together in the same raster. total 12 layers
finalR <- c(target_ipl, target_buff, target_buff50, target_out)
writeRaster(finalR, "./Permutation/All_taxa.tif")

#### separate taxa for IPL and 10 km buffer only
sepTaxa <- c("./Forest2020/SpeciesRichness_onlyAMPHIBIA.tif",
             "./Forest2020/SpeciesRichness_onlyAVES.tif",
             "./Forest2020/SpeciesRichness_onlyMAMMALIA.tif",
             "./Forest2020/SpeciesRichness_onlyREPTILIA.tif",
             "./Forest2020/ThreatScore_onlyAMPHIBIA_SRweighted.tif",
             "./Forest2020/ThreatScore_onlyAVES_SRweighted.tif",
             "./Forest2020/ThreatScore_onlyMAMMALIA_SRweighted.tif",
             "./Forest2020/ThreatScore_onlyREPTILIA_SRweighted.tif",
             "./Forest2020/InverseRange_onlyAMPHIBIA_SRweighted.tif",
             "./Forest2020/InverseRange_onlyAVES_SRweighted.tif",
             "./Forest2020/InverseRange_onlyMAMMALIA_SRweighted.tif",
             "./Forest2020/InverseRange_onlyREPTILIA_SRweighted.tif")
# read in rasters
r <- rast(sepTaxa)
names(r) <- c("SR_Amphibia","SR_Aves","SR_Mammalia","SR_Reptilia",
              "TS_Amphibia","TS_Aves","TS_Mammalia","TS_Reptilia",
              "IR_Amphibia","IR_Aves","IR_Mammalia","IR_Reptilia")
# mask out PAs
target <- mask(r, pas, maskvalues=1)
# get layer of just IPLs
ipl <- vect("./IPL.gpkg")
ipl <- project(ipl, "epsg:4326")
target_ipl <- mask(target, ipl) 
names(target_ipl) <- paste0("IPL_", names(target_ipl))
# get layer of just 10km buffers
buff <- vect("./IPL_10kmBufferOnly_union_sf.gpkg")
buff <- project(buff, "epsg:4326")
target_buff <- mask(target, buff)
names(target_buff) <- paste0("buff10_", names(target_buff))
# save species richness, threat score and inverse range stacks separately
# 8 layers in each file
spR <- c(subset(target_ipl, 1:4), subset(target_buff, 1:4))
writeRaster(spR, "./Permutation/Sep_taxa_SR.tif")
tsR <- c(subset(target_ipl, 5:8), subset(target_buff, 5:8))
writeRaster(spR, "./Permutation/Sep_taxa_TS.tif")
irR <- c(subset(target_ipl, 9:12), subset(target_buff, 9:12))
writeRaster(spR, "./Permutation/Sep_taxa_IR.tif")

