#### ====== Create layers needed for analysis ====== ####
# Note: done on local computer, R version 4.1.1
pacman::p_load(tidyverse, sf, wdpar, data.table, terra)
## tidyverse v1.3, sf v1.0.4, wdpar v1.3.2, data.table v1.14.0, terra v1.5.21
# used a mix of R and ArcGIS (Erase tool) and QGIS (difference tool),
# when R could not handle the geoprocessing and crashed

#### protection type raster layer ####
# ---- clean PA layer ----
# all PAs in Jan 2020 (n=10551)
pa <- read_sf('../Data/Raw/WDPA/WDPA_TropicalForests_notMarine.gpkg') 

# clean layer (i.e. remove overlaps) (Steps 7 to 15 in manual)
pa$IUCN_CAT <- factor(as.character(pa$IUCN_CAT), 
                           levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Not Reported", 
                                      "Not Applicable", "Not Assigned"))
geometry_precision = 1000
pa <- sf::st_set_precision(pa, geometry_precision)
pa <- st_erase_overlaps(pa[order(pa$IUCN_CAT,pa$STATUS_YR), ], verbose=TRUE)

pa$IUCN_CAT <- as.character(pa$IUCN_CAT)
pa <- pa[!sf::st_is_empty(pa), ] 
pa <- suppressWarnings(sf::st_collection_extract(pa, "POLYGON")) 
pa <- sf::st_set_precision(pa, geometry_precision)  

# repair geometry
pa <- sf::st_set_precision(pa, geometry_precision)
pa <- st_make_valid(pa)
pa <- pa[!sf::st_is_empty(pa), ]
pa <- suppressWarnings(sf::st_collection_extract(pa, "POLYGON"))
pa <- sf::st_set_precision(pa, geometry_precision)

# format attribute layer for readability
pa$MARINE <- as.character(pa$MARINE)
pa$MARINE[pa$MARINE == "0"] <- "Terrestrial"
pa$MARINE[pa$MARINE == "1"] <- "Partial"
pa$STATUS_YR[pa$STATUS_YR == 0] <- NA_real_
pa$NO_TK_AREA[pa$NO_TAKE %in% c("Not Reported", "Not Applicable")] <- NA_real_

# save final cleaned wdpa output, overwriting old one
st_write(pa, dsn='../Data/Raw/ProtectionTypes/WDPA_TropicalForests.gpkg',
         append=FALSE)

# ---- obtain spatial intersect of PA and IL (PIA) ----
# 1st, split PAs that are governed by Indigenous peoples or not

#  NOT governed by IP
pa_noIP <- pa[which(pa$GOV_TYPE != "Indigenous peoples"),] # 10080 features
pa_noIP <- st_make_valid(pa_noIP)
st_write(pa_noIP, '../Data/Raw/ProtectionTypes/WDPA_TropicalForests_noIP.shp')      

# governed by IP
pa_IP <- pa[which(pa$GOV_TYPE == "Indigenous peoples"),] # 471
st_write(pa_IP, '../Data/Raw/ProtectionTypes/WDPA_TropicalForests_withIP.shp') 

# 2nd, to get spatial intersect of PA and IL.
ipl <- read_sf('../Data/Raw/Garnett2018_IPL_2017/Dinerstein-TEOW/IPL2017_TropicalForests17.shp')
ipl <- st_make_valid(ipl)
st_crs(pa_noIP) <- st_crs(ipl)
pa_IPL_intersect <- st_intersection(pa_noIP, ipl) # 4182 obs
pa_IPL_intersect2 <- aggregate(pa_IPL_intersect, list(pa_IPL_intersect$WDPAID), function(x) x[1]) # 2564 obs
pa_IPL_intersect2 <- select(pa_IPL_intersect2, -Group.1) # 2564 obs
write_sf(pa_IPL_intersect2, '../Data/Raw/ProtectionTypes/WDPA_noIP_intersectIPL2017_dissolvebyWDPAID.gpkg')

# 3rd, join the layer where PAs are governed by IP with the layer where PA intersect IL
# pa_IP doesn't have Name_ and ORIG_FID columns
pa_IP[,c('Name_', 'ORIG_FID')] <- NA
# join the intersecting layer and PAs governed by IP layer
st_crs(pa_IP) <- st_crs(ipl)
pa_ipl_IP <- rbind(pa_IP,pa_IPL_intersect2) # 3035 features
pa_ipl_IP <- st_buffer(pa_ipl_IP, 0)
pa_ipl_IP <- st_make_valid(pa_ipl_IP)
write_sf(pa_ipl_IP, '../Data/Raw/ProtectionTypes/PIA.shp') 

# ---- get PAs without PIAs ----
#  used ArcMap Analysis>Overlay>Erase tool (Erase analysis) to clip out 
#  PIA.shp from WDPA_TropicalForests_noIP.shp, 
#  to get WDPA_noIP_erasedPIA.shp

# ---- get ILs without PIAs ----
# used ArcMap Analysis>Overlay>Erase tool to clip out 
# PIA.shp from IPL2017_TropicalForests17.shp, 
# to get IPL_erasedPIA.shp
# 
# ---- combine PAs, IAs and PIAs ----
# combine the different ones into one. 

# read in shapefiles and add column for protection type
pa <- st_read("./WDPA_noIP_erasedPIA.shp") %>% 
  mutate(ProType = "PA")
il <- st_read("./IPL_erasedPIA.shp") %>% 
  # has z dimension which is throwing error. drop z dimension
  st_zm() %>% 
  mutate(ProType = "IL")
pia <- st_read("./PIA.shp") %>% 
  mutate(ProType = "PIA")

# bind together 
allTypes <- st_as_sf(data.table::rbindlist(list(pa, il, pia), fill = TRUE))
allTypes <- st_buffer(allTypes, 0)
# add additional column for numeric protection type
allTypes <- allTypes %>% 
  mutate(Types = case_when(ProType=="PA" ~ 1,
                           ProType=="IL" ~ 2,
                           ProType=="PIA" ~ 3))
st_write(allTypes4326, './WDPA_IL_PIA_buffered.gpkg')

# ----- rasterise -----
# using terra 
gadm <- rast("../Data/Raw/Country Boundaries/gadm36_Countries.tif")
allTypes <- vect("../Data/Raw/ProtectionTypes/WDPA_IL_PIA_buffered.gpkg") %>% 
  project("EPSG:4326")
allTypesR <- terra::rasterize(allTypes, gadm, field='Types', background=0)
allTypesR <- mask(allTypesR, gadm)
writeRaster(allTypesR, "../Data/Raw/ProtectionTypes/WDPA_IL_PIA_buffered_EPSG4326.tif")


#### IPL vs buffer area layers ####
# ---- IPL polygons ----
ipl <- read_sf("./WDPA_IL_PIA_buffered.gpkg")
ipl <- ipl %>% 
  filter(Types != 1) # pick out IL and PIA 
ipl <- st_make_valid(ipl)
write_sf(ipl, "../Data/Raw/ProtectionTypes/IPL.gpkg")

# ---- 10 km buffer polygons ----
buff10k <- st_buffer(ipl, dist=10000) 
buff10k <- st_make_valid(buff10k)
write_sf(buff10k, "../Data/Raw/ProtectionTypes/IPL_inc10kmBuffer_sf.gpkg")
# used the Difference tool in QGIS with IPL.gpkg to get difference of 10 km buffer only
# unionise the resultant buffer only output to remove overlaps
buffOnly10k <- read_sf("../Data/Raw/ProtectionTypes/IPL_10kmBufferOnly_sf.gpkg")
buffOnly10k <- st_make_valid(buffOnly10k)
buffOnly10k <- st_union(buffOnly10k)
write_sf(buffOnly10k, "../Data/Raw/ProtectionTypes/IPL_10kmBufferOnly_union_sf.gpkg")

# ---- 50km buffer polygons ----
buff50k <- st_buffer(ipl, dist=50000)
buff50k <- st_make_valid(buff50k)
write_sf(buff50k, "../Data/Raw/ProtectionTypes/IPL_inc50kmBuffer_sf.gpkg")
# used the Difference tool in QGIS with IPL.gpkg to get difference of the 50km buffer only
# unionise the resultant buffer only output to remove overlaps
buffOnly50k <- read_sf("../Data/Raw/ProtectionTypes/IPL_50kmBufferOnly_sf.gpkg")
buffOnly50k <- st_make_valid(buffOnly50k)
buffOnly50k <- st_union(buffOnly50k)
write_sf(buffOnly50k, "../Data/Raw/ProtectionTypes/IPL_50kmBufferOnly_union_sf.gpkg")

# used the Difference tool in QGIS with IPL_inc10kmBuffer_sf.gpkg to get the difference of 10-50km buffer
buffOnly50k <- read_sf("../Data/Raw/ProtectionTypes/IPL_10-50kmBufferOnly_sf.gpkg")
buffOnly50k <- st_make_valid(buffOnly50k)
buffOnly50k <- st_union(buffOnly50k)
buffOnly50k <- st_make_valid(buffOnly50k)
write_sf(buffOnly50k, "../Data/Raw/ProtectionTypes/IPL_10-50kmBufferOnly_union_sf.gpkg")

