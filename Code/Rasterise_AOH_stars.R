# uses R 4.0.3, using script adapted from Simon's example_script_for_Jocelyne.R which is based on stars package
# workflow starts from the all species for each of the 4 classes, consolidating forest specialists into one gpkg
# then rasterise and crop to AOH
# 6 Oct 2021
# need to download the developer branch of stars
# devtools::install_github("r-spatial/stars")

# 2 May 2022. figured might as well stick with terra instead of using stars
# cos the main reason for using stars (use with tidyverse mutate and select)
# doesnt actually work
# trial code
# m = matrix(1:20, nrow = 5, ncol = 4)
# dim(m) = c(x = 5, y = 4)
# s = st_as_stars(m)
# s %>% mutate(A2 = A1>5) -> s2
# end up with attribute A2 Mode: logical FALSE:5 TRUE:15 rather than what I wanted
# which requires 
# s2 <- s
# s2[s2<5] <- NA
# s[is.na(s2)] <- NA
# which is the same as terra so going back to terra
library(sf)
library(data.table)
library(stars)
library(dplyr)
library(magrittr)

# turn off s2
sf_use_s2(FALSE)

# ---- obtain one shapefile for forests specialists from all 4 classes ----
# filter species distribution polgyons to only forest specialists & merge the classes to one dataset
# ultimately should have one sf file with one row per species

#### OPTION ONE - if I hadn't already split the files to indv species ####
# forest specialists species data 
speciesData <- read.csv('./FinalForestSpeciesList_BufferedElevRange.csv')

# function to filter species file
filter_species <- function(shapefile){
  sf <- read_sf(shapefile) %T>% {
    cat('there are originally', nrow(.), 'of rows and ', length(unique(.$binomial)), 'of species', '\n')} %>% 
    filter(binomial %in% speciesData$Species) %T>% {
      cat('after filtering, there are', nrow(.), 'of rows and ', length(unique(.$binomial)), 'of species', '\n')} %>% 
    group_by(binomial) %>%
    st_make_valid() %>% 
    summarise(geometry = st_union(st_geometry(.))) %>% # for gpkg, sf_column is called geom. maybe can use st_geometry(.)
    st_transform(4326) %>% 
    st_make_valid()
  return(sf)
}

cat("working on mammals now...", "\n")
mammals <- filter_species('./SpeciesDistributions/IntersectsTropical_filtered_EPSG54009_MAMMALS_TERRESTRIAL_ONLY.shp')
  # 8775 rows (4284 species) -> 2478 rows (1759 species)
cat("working on amphibians now...", "\n")
amphibians <- filter_species('./SpeciesDistributions/IntersectsTropical_filtered_EPSG54009_AMPHIBIANS.shp')
  # 6803 rows (5939 species) -> 1609 rows (1472 species)
cat("working on reptiles now...", "\n")
reptiles <- filter_species('./SpeciesDistributions/IntersectsTropical_filtered_EPSG54009_REPTILES.shp')
   # 6599 rows (5282 species) -> 2567 rows (2182 species)
cat("working on birds now...", "\n")
birds <- filter_species('./SpeciesDistributions/IntersectsTropical_filtered_EPSG54009_BIRDS.gpkg')
  # 12823 rows (9713 species) -> 8119 rows (6459 species)

finalShapefile <- st_union(mammals,amphibians) %>% 
  st_union(reptiles) %>% 
  st_union(birds) %>% 
  write_sf("./SpeciesDistributions/TropicalForestVertebrates_EPSG4326.gpkg")

#### OPTION TWO - since I alr have the 11872 target species but in indv files ####
# done on 28 April 2022! on HPC
setwd('/shared/edwards_lab1/User/bop19jss/Ch3-biodiversity')

fileList <- list.files("./ForestVertebrateShapefiles", pattern = "*.gpkg", full.names = TRUE)
# length(fileList) = 11872
shapefileList <- lapply(fileList, read_sf)
allSpecies <- sf::st_as_sf(data.table::rbindlist(shapefileList, fill=TRUE))
allSpecies <- st_make_valid(allSpecies)  
allSpecies <- allSpecies %>% 
  group_by(binomial) %>%
  summarise(geom = st_union(geom))
write_sf(allSpecies, "./SpeciesDistributions/TropicalForestVertebrates.gpkg")
allSpecies <- st_transform(allSpecies, 4326) %>% 
  write_sf("./SpeciesDistributions/TropicalForestVertebrates_EPSG4326.gpkg")

# --- rasterise species polygons ----
# setwd('/shared/edwards_lab1/User/bop19jss/Ch3-biodiversity')
# for HPC, "../Data/Raw/" not necessary
# running it on my laptop to troubleshoot stars

# mask for 2010 forest area with elevation
tmp10 <- read_stars('../Data/Raw/elevation_TropForResampled_maskedForest2010.tif')

# test
new10 <- tmp10 %>% 
  mutate(newRange = tmp10<1000)
st_as_stars(new10)
# Error in `mutate_cols()`:
#   ! Problem with `mutate()` column `newRange`.
# ℹ `newRange = tmp10 < 1000`.
# x `newRange` must be a vector, not a `stars_proxy/stars` object.
# Run `rlang::last_error()` to see where the error occurred.
# mask for 2018 forest area with eleavation
tmp18 <- read_stars('../Data/Raw/elevation_TropForResampled_maskedForest2018.tif')
# shapefile with all forest vertebrates (one species per row)
finalShapefile <- read_sf('../Data/Raw/TropicalForestVertebrates_EPSG4326.gpkg')
# finalShapefile <- read_sf('./SpeciesDistributions/TropicalForestVertebrates_EPSG4326.gpkg') # for HPC
# forest specialists species data 
speciesData <- read.csv('../Data/Raw/FinalForestSpeciesList_BufferedElevRange.csv')

# create final rasters
rasts10 <- rep(NA, nrow(finalShapefile))
names(rasts10) <- finalShapefile$binomial
rasts18 <- rep(NA, nrow(finalShapefile))
names(rasts18) <- finalShapefile$binomial

# create template for species file rasterisation (NB: critical to add nx and ny! else wrong resolution)
template = st_as_stars(st_bbox(tmp10), nx=43200, ny=8293)

# function to crop
for(i in 1:length(rasts10)) {
  print(i)
  # get the species name to search in database
  spName <- gsub("_", " ", gsub("^[^-]*-([^.]+).*", "\\1", names(rasts10)[i]))
  elevLower <- speciesData[which(speciesData$Species==spName),'Elev_lower']
  elevUpper <- speciesData[which(speciesData$Species==spName),'Elev_upper']
  
  # rasterise the species distribution
  rast_temp <- st_rasterize(finalShapefile[i,], template, align=TRUE, options=c("ALL_TOUCHED=TRUE")) 
  # this is now a stars object with 2 dimensions (x & y) and 1 attribute (presence/absence)
  
  # clip to AOH
  rast_temp2 <- c(tmp10, tmp18, rast_temp) %>% 
    # crop to range distribution to speed up processing
    st_crop(st_bbox(finalShapefile[i,])) %>%
    # create AOH based on 2010 and 2018 forest area & elevation
    # !!! NB THIS DOES NOT WORK. MUTATE DOES NOT WORK LIKE THIS 
    {if(!is.na(elevLower) & !is.na(elevUpper)) # if there are lower and upper limits
      mutate(., newRange10 = tmp10 >= elevLower & tmp10 <= elevUpper,
             newRange18 = tmp18 >= elevLower & tmp18 <= elevUpper
             ) else if(is.na(elevLower) & !is.na(elevUpper)) # if only upper limit
               mutate(., newRange10 = tmp10 <= elevUpper,
                      newRange18 = tmp18 <= elevUpper
                      ) else if(!is.na(elevLower) & is.na(elevUpper)) # if only lower limits
                        mutate(., newRange10 = tmp10 >= elevLower,
                               newRange18 = tmp18 >= elevLower
                        ) else (mutate(., newRange10 = !is.na(tmp10), # if no limits specified
                                              newRange18 = !is.na(tmp18)))} %>% 
    dplyr::select(newRange10, newRange18) 
  
  rasts10[[i]] <- rast_temp2[`newRange10`]
  rasts18[[i]] <- rast_temp2[`newRange18`]
}

# see e.g. 
plot(rasts$`Amazona amazonica`)

# you can stack these into a single stars object with:
spRichness10 <- do.call(c, rasts10)
spRichness18 <- do.call(c, rasts18)