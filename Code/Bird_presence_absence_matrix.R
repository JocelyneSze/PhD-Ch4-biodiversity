##################################################################################
#### Get species richness maps from IUCN, BOTW and GARD occurrence shapefiles ####
#### Mike Massam, 2020                                                        ####
#### mikemassam@gmail.com; mikemassam@sheffield.ac.uk                         ####
#### This scripts borrows from code by Dr. Brunno Freire Oliveira             ####
##################################################################################

### Preamble ###
library(sp)
library(rgdal)
library(raster)
library(letsR)
library(maptools)
library(sf)
library(tidyverse)
rm(list = ls())

# Define colour ramp for plotting
colfunc <- colorRampPalette(c("#4575b5ff", "#f5fad2ff", "#d73027ff"))
# Function to help define legend ticks
func_splint <- function(x,interval=4) {
  require(ggplot2)
  is.odd <- function(x) x %% 2 != 0
  
  a <- levels(cut_interval(x,interval-1))
  b <- unlist(strsplit(a,','))
  c <- gsub('[','',b,fixed="TRUE")
  d <- gsub(']','',c,fixed="TRUE")
  e <- gsub('(','',d,fixed="TRUE")
  f <- gsub(')','',e,fixed="TRUE")
  return(as.numeric(c(unique(f))))
}

#### Read trade data ####
trade <- read.csv("Data/trade.csv", stringsAsFactors = F)

#### Create rasterised world map #### 
# Create equal area (111x111km) global raster
ras <- raster(xmn = -20592508, xmx = 20588492, ymn = -5743602, ymx = 6573398,
              crs = CRS("+proj=cea +datum=WGS84"))
res(ras) <- 111000
values(ras) <- 1

# Load global land shape file
world <- readOGR("Data/Spatial/ne_50m_land_no_artic.shp", layer = "ne_50m_land_no_artic") %>% 
  spTransform(CRS("+proj=cea +datum=WGS84")) # Project to equal area

# Polygon of world extent
world_ext <- extent(ras) %>% 
  as("SpatialPolygons") %>% 
  as("SpatialPolygonsDataFrame")
world_ext$binomial <- "all"
proj4string(world_ext) <- "+proj=cea +datum=WGS84"

# Rasterise land polygon
# Values are proportion of cell that is land
world_ras <- rasterize(world, ras, getCover = T)


#### Read spatial occurrence polygons ####
#### BIRDS ####
# Large RAM requirements so gpkg split into individual species shapefiles

# All birds occurrence
spp <- trade %>% 
  filter(taxa == "Bird") %>%
  pull(species)

# Get template community matrix to fill when looping by making a dataframe where all coordinates have species presence
occr_template <- lets.presab(shapes = world_ext,
                             resol = 111000, # Grid of 111kmx111km
                             crs = CRS("+proj=cea +datum=WGS84"), # CRS of shapefile
                             crs.grid = CRS("+proj=cea +datum=WGS84"), # CRS of grid
                             count = T, 
                             xmn = -20592508, xmx = 20588492, ymn = -5743602, ymx = 6573398) 

# Remove "all" column to leave coordinates for merging
occr_matrix <- occr_template$Presence_and_Absence_Matrix %>% 
  as.data.frame() %>% 
  select(-all)

for(i in 1:length(spp)) {
  i.spp <- spp[i] # Get ith species
  i.file <-paste0("Data/Spatial/Bird_shapefiles/binomial_", i.spp, ".shp") # Get ith species shapefile
  if(file.exists(i.file)) { # Check that the file exists (i.e. species range is available)
    # Read species range using sf package
    spp_range <- read_sf(dsn = i.file, layer = paste0("binomial_", i.spp)) %>% 
      as("Spatial") # Convert to SpatialPolygonsDataFrame
    
    # Get occurrence 
    occr.tmp <- lets.presab(shapes = spp_range, resol = 111000, # Grid of 111kmx111km
                            crs = CRS("+proj=longlat +datum=WGS84 +no_defs"), # CRS of shapefile
                            crs.grid = CRS("+proj=cea +datum=WGS84"), # CRS of grid
                            count = T, 
                            cover = 0,
                            presence = 1:6, # Presence statuses to consider - see IUCN metadata (doesn't apply for reptiles)
                            origin = 1:6, # Origin statuses to consider - see IUCN metadata (doesn't apply for reptiles)
                            seasonal = 1:5, # Seasonal statuses to consider - see IUCN metadata (doesn't apply for reptiles)
                            xmn = -20592508, xmx = 20588492, ymn = -5743602, ymx = 6573398)
    
    # Get presence-absence matrix
    occr_matrix.tmp <- occr.tmp$Presence_and_Absence_Matrix %>% 
      as.data.frame()
    
    # Merge to community matrix
    occr_matrix <- occr_matrix %>% 
      left_join(occr_matrix.tmp, by = c("Longitude(x)", "Latitude(y)")) %>% # Merge by long and lat columns
      mutate_at(vars(matches(i.spp)), ~ replace_na(., 0)) # Fill species column NAs with 0s
    print(paste0(i.spp, "...done"))
  } else { # If file does not exist, skip the species
    print(paste0(i.spp, "...NO SPATIAL DATA"))
    next
  }
}

write.csv(occr_matrix, "Data/Spatial/Bird_occurrence_matrix.csv", row.names = F)