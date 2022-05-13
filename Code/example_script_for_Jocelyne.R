# Example script for clipping ranges to forest and elevation. 
#
# I've only run for 200 species here, because memory requirements get a bit 
# much for my desktop, but it'd just be a question of sticking it on the HPC with
# a boatload of RAM. For 200 species it takes a couple of minutes, so whole job
# should be no more than a couple of hours. You can split it and run it up 
# and run it in parallel to avoid overly heavy memory usage. 
#
# On line 72 you need to add some species-specific elevation data to clip by
#
# All the earth engine stuff was just so I had some layers to work with (tc and 
# ele at 10km resolution), you can drop all of that and lose those dependencies.

## Packages ----
library(rgee); library(sf); library(stars); library(dplyr); 
library(rnaturalearth)

# polygons for neotropical species
# this is an sf file with one row per species
range_maps <- readRDS("birdlife_maps/range_maps_neotropical_endemics.rds") %>%
    select(-is_valid)

## Set up rgee session ----
ee_Initialize(drive=TRUE, gcs=TRUE)

## EE datasets ----
ALOS <- ee$Image("JAXA/ALOS/AW3D30/V2_2")$select("AVE_DSM")
tc_modis <- ee$ImageCollection("MODIS/006/MOD44B")
worldclim <- ee$Image("WORLDCLIM/V1/BIO")

# rectangle designating neotropics ----
geometry <- ee$Geometry$Rectangle(
    coords = c(-179,-23.5,179,23.5),
    proj = "EPSG:4326",
    geodesic = FALSE
)

# (1) Get elevation and treecover layers ----       
# You presumably already have these stored somewhere..
# get MODIS treecover in 2010
pct_tc <- tc_modis$select('Percent_Tree_Cover')
listOfImages <- pct_tc$toList(pct_tc$size())
tc_2010 <- ee$Image(listOfImages$get(10))

# export for tropics 10k resolution, WGS84 projection
tc_stars <- ee_as_stars(tc_2010, region = geometry, scale = 1e4, crs="epsg:4326")
write_stars(tc_stars, "data/covariate_layers/treecover_10km_tropics_modis_2010.tif")
tc_stars_local <- read_stars("data/covariate_layers/treecover_10km_tropics_modis_2010.tif")

# elevation
ALOS_stars <- ee_as_stars(ALOS, region = geometry, scale = 1e4, crs="epsg:4326")
write_stars(ALOS_stars, "data/covariate_layers/elevation_10km_tropics_ALOS.tif")
ALOS_stars_local <- read_stars("data/covariate_layers/elevation_10km_tropics_ALOS.tif")

# ..& join to single stars object
ele_tc <- c(tc_stars_local, ALOS_stars_local) %>%
    select(treecover = treecover_10km_tropics_modis_2010.tif, 
           elevation = elevation_10km_tropics_ALOS.tif) %>%
    mutate(is_forest = treecover > 50) #..or whatever threshold you are using

# Rasterise species polygons ----
# This is memory heavy, so I've only run for 200 locally, but only takes a minute 
# or so (extrapolating wildly I'd expect it to take <1hr to do 10,000 or so)
grd <- ele_tc %>% select(treecover)
grd[[1]] <- NA

rasts <- rep(NA, nrow(range_maps))
names(rasts) <- range_maps$SCINAME

# only do 200 here
rasts <- rasts[1:200]

for(i in 1:length(rasts)) {
    print(i)
    rast_temp <- st_rasterize(range_maps[i,], grd, options=c("ALL_TOUCHED=T"))
    rast_temp2 <- c(ele_tc, rast_temp) %>%
        mutate(new_range = !is.na(ID) & is_forest == 1) %>% # & elevation < 400 etc..
        select(new_range) 
        
    rasts[[i]] <- rast_temp2
}

# see e.g. 
plot(rasts$`Amazona amazonica`)

# you can stack these into a single stars object with:
test2 <- do.call(c, rasts)
