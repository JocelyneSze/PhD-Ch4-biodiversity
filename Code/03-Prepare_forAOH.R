#### ========= Prepare for extracting AOH ============= ####
# Note: done on local computer using R version 4.1.1
# Complete reference file for tropical forest-dependent species and
# associated elevational range and split species distribution file
# for each species
pacman::p_load(tidyverse, sf, terra)
# switched to using terra package 1.4.22 (instead of raster)
## tidyverse v1.3, sf v0.9-8, terra v1.4.22

# ---- compile list of forest vertebrates ----
others <- read_csv('../Data/Raw/IUCNdata_IntersectsTropical_others.csv')
birds <- read_csv('../Data/Raw/IUCNdata_IntersectsTropical_Birds.csv')

## join the two datasets
others <- others %>% 
  filter(Habitat=='Forest important'|Habitat=='Forest unimportant') %>% 
  mutate(ForestDependency = NA) # 5414 obs 

finalList <- rbind(birds, others) %>% 
  mutate(Class = as.factor(Class),
         Category = as.factor(Category),
         Habitat = as.factor(Habitat),
         PopTrend = as.factor(PopTrend),
         Rep_AOO_km2 = as.numeric(Rep_AOO_km2),
         Rep_EOO_km2 = as.numeric(Rep_EOO_km2))
write_csv(finalList, '../Data/Raw/FinalForestSpeciesList.csv')

# ---- check altitudinal ranges ----
# Ficetola added 300m buffer to elevational range for amphibians
# https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-several-columns-on-a-subset-of-rows
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

speciesList <- read_csv('../Data/Raw/FinalForestSpeciesList.csv') %>%  # 11872
  mutate_cond(Class=='AMPHIBIA', Elev_lower=Elev_lower-300) %>% 
  mutate_cond(Class=='AMPHIBIA', Elev_upper=Elev_upper+300) %>% 
  write_csv('../Data/Raw/FinalForestSpeciesList_BufferedElevRange.csv')

###### FUNCTION to split shapefile by species #####
# function to split each taxa shapefile by species 
split_shapes <- function(shapefile, taxa, path_out){
  speciesList <- unique(shapefile$binomial)
  for (i in 1:length(speciesList)){
    speciesFile <- shapefile[which(shapefile$binomial == speciesList[i]),]
    species <- paste0(strsplit(speciesList[i], " ")[[1]][1:2], collapse="_")
    fname <- paste0(path_out, '/', taxa, '-', species, ".gpkg")
    write_sf(speciesFile, fname)
  }}
# ---- split the taxa shapefiles to indv species------
# because we're going to rasterise and crop to AOH for 
# each species as a task array in HPC 

shpFiles <- c('../Data/Raw/IUCN-SpeciesDistributions/MAMMALS_TERRESTRIAL_ONLY/IntersectsTropical_filtered_EPSG54009_MAMMALS_TERRESTRIAL_ONLY.shp',
              '../Data/Raw/IUCN-SpeciesDistributions/AMPHIBIANS/IntersectsTropical_filtered_EPSG54009_AMPHIBIANS.shp',
              '../Data/Raw/IUCN-SpeciesDistributions/REPTILES/IntersectsTropical_filtered_EPSG54009_REPTILES.shp',
              '../Data/Raw/BirdLifeInternational-BOTW2020/IntersectsTropical_filtered_EPSG54009_BIRDS.gpkg')

speciesList <- read.csv('../Data/Raw/FinalForestSpeciesList.csv')

# run for all the 4 taxa
for (shapefile in shpFiles){
  taxaFile <- read_sf(shapefile)
  taxa <- strsplit(strsplit(basename(shapefile), '\\_')[[1]][4], '\\.')[[1]][1]
  cat(taxa, '\n')
  path_out <- '../Data/Raw/ForestVertebrateShapefiles'
  # filter to only forest vertebrate species
  taxaFile <- taxaFile %>% 
    filter(binomial %in% speciesList$Species)
  # split the polygons by species
  speciesFile <- split_shapes(taxaFile, taxa, path_out)
}
