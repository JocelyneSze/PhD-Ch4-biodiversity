## explore datasets
# 7 Apr 2021 (updated to R version 4.0.5 on 6 Apr)
# test code

#### PREDICTS ####
## convert to sf object
predicts <- readRDS('../Data/Raw/Hudson2016_PREDICTS/database.rds')
head(predicts) #3250404

siteloc <- predicts %>% 
  dplyr::select(Reference, Diversity_metric, Sample_end_latest, Predominant_land_use, Source_for_predominant_land_use, Years_since_fragmentation_or_conversion, Coordinates_method, Longitude, Latitude, Country, Taxon, Effort_corrected_measurement) %>% 
  drop_na(Longitude) %>% #7852
  st_as_sf(coords=c('Longitude','Latitude'), crs=4326) #3242552

## tropical forest area (not used)
tropfor <- read_sf('~/Google Drive/Ch2-deforestation/Data/Raw/DinersteinEcoregions2017/Ecoregions2017_TropicalForests.shp')

# filter sites that are within tropical forests
tropsites <- siteloc[tropfor,] #1241702
write_sf(tropsites, '../Data/Raw/Hudson2016_PREDICTS/database_clippedTropFor_epsg4326.shp')
tropsites <- read_sf('../Data/Raw/Hudson2016_PREDICTS/database_clippedTropFor_epsg4326.shp')
head(tropsites)

## see how many within each type
types <- read_sf('~/Google Drive/Ch2-deforestation/Data/Raw/Type/Type_12Aug_countryIntersect_epsg4326.shp') %>% 
  dplyr::select(ISO3, are_km2, type, IUCN_CA, NAME_0) 

sitesintypes <- st_intersects(siteloc, types) # a lot quicker than st_join 
# which points fall inside the first polygon
sitesintypes[[1]]
# which points fall inside a polygon
sites_logical <- lengths(sitesintypes) > 0
target_points <- siteloc[sites_logical,] #1209498 obs
write_sf(target_points, '../Data/Raw/Hudson2016_PREDICTS/database_withinTypes_epsg4326.shp')

# want to get data for which points are in what kind of PA.
target_points2 <- st_intersection(target_points, types) # still takes > 24 hours

# see https://github.com/claudialouisegray/PREDICTS_WDPA/blob/master/Protected_Area_analyses_CLG/Analysis_simple_sp_rich.R
# 
# 
#### Benitez-Lopez ####
defaunation <- read.csv('../Data/Raw/Benitez-Lopez_DefaunationIndex/huntmamdata.csv')
summary(defaunation)
unique(defaunation$Reference) # 114

siteloc <- defaunation %>% 
  st_as_sf(coords=c('X','Y'), crs=4326) #3242552
write_sf(siteloc, '../Data/Raw/Benitez-Lopez_DefaunationIndex/huntmamdata_epsg4326.shp')


#### BII ####
library(exactextractr)
bii <- raster('../Data/Raw/NHM_BII/lbii.asc')
  # dimensions : 18000, 43200, 777600000  (nrow, ncol, ncell)
  # resolution : 0.008333333, 0.008333333  (x, y)
  # extent     : -180, 180, -60, 90  (xmin, xmax, ymin, ymax)
  # crs        : NA 
  # values     : 0.3611694, 1.387378  (min, max)
types <- read_sf('~/Google Drive/Ch2-deforestation/Data/Raw/Type/Type_12Aug_countryIntersect_epsg4326.shp') %>% 
  dplyr::select(ISO3, type, IUCN_CA, STATUS_, NAME_0) %>% 
  mutate(type = as.factor(type))
crs(bii) <- crs(types)

# force NAs in bii to be 0
bii[is.na(bii)] <- 0
writeRaster(bii, '../Data/Raw/NHM_BII/lbii.tif')

# get some preliminary data on bii for each type
types$bii_sum <- exact_extract(bii, types, fun='sum')
types$bii_mean <- exact_extract(bii, types, fun='mean')
types$bii_stdev <- exact_extract(bii, types, fun='stdev')

# check area (raster::area works pretty well for non-projected crs but lets stick with 54009)
# types2$area_sqkm <- raster::area(as(types2, 'Spatial'))/1000000
# types3 <- st_transform(types2, 54009)
# types3$area_sqkm_54009 <- st_area(types3)/1000000

types_54009 <- types %>% 
  st_transform(54009) %>% 
  mutate(area_sqkm = st_area(types)/1000000)

df <- as_tibble(types_54009) %>% 
  mutate(ISO3 = as.factor(ISO3)) %>% #ISO3 seems to be the wrong thing I want. It's from PA data.
  group_by(type) %>% 
  summarise(total_bii_sum = sum(bii_sum),
            mean_bii_sum = mean(bii_sum),
            mean_bii_mean = mean(bii_mean),
            mean_bii_stdev = mean(bii_stdev),
            total_area_sqkm = sum(area_sqkm))

typesv2 <- st_transform(types_54009, 4326)
write_sf(typesv2, '../Data/Raw/ProtectionTypes_BII_epsg4326.shp')
# re-do this bit with accurate ISO3 data.
## let's look at the data
summary(df)

#### IUCN sp range ####
#install.packages('letsR')
#install.packages('rredlist')

# tropical extent
types <- read_sf('~/Google Drive/Ch2-deforestation/Data/Raw/Type/Type_12Aug_countryIntersect_epsg4326.shp') %>% 
  dplyr::select(GID_0, NAME_0, type, IUCN_CA) 

   # ---- mammals ----
    mammals <- read_sf('../Data/Raw/IUCN-SpeciesDistributions/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp')
    glimpse(mammals) # 12483 obs 
    # count number of sp
    length(unique(mammals$binomial)) # 5593 sp
      
    # following conditions used to create these rasters: https://www.iucnredlist.org/resources/other-spatial-downloads
    mam2 <- mammals %>% 
      select(binomial, presence, origin, seasonal, legend, class, category, terrestial) %>% 
      filter(terrestial=='true',
             presence %in% c(1,4),
             origin %in% c(1,2)) # 11552 obs
    length(unique(mam2$binomial)) # 5568 sp
    
    # keep only species which fall within tropical extents
    mamtrop <- st_intersects(mam2, types) # a lot quicker than st_join 
    # which points fall inside a polygon
    mam_logical <- lengths(mamtrop) > 0
    mam3 <- mam2[mam_logical,] # 8580 obs
    length(unique(mam3$binomial)) # 4280 sp
    
    write_sf(mam3, '../Data/Raw/IUCN-SpeciesDistributions/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_filtered.shp')
    
    
   # ---- amphibians ----
    amph <- read_sf('../Data/Raw/IUCN-SpeciesDistributions/AMPHIBIANS/AMPHIBIANS.shp')
    glimpse(amph) # 8598 obs
    length(unique(amph$binomial)) # 7118 sp
    
    # following conditions used to create these rasters: https://www.iucnredlist.org/resources/other-spatial-downloads
    amph2 <- amph %>% 
      select(binomial, presence, origin, seasonal, legend, class, category, terrestial) %>% 
      filter(terrestial=='true',
             presence %in% c(1,4),
             origin %in% c(1,2)) # 7954 obs
    length(unique(amph2$binomial)) # 6999 sp

    # keep only species which fall within tropical extents
    amphtrop <- st_intersects(amph2, types) # a lot quicker than st_join 
    # which points fall inside a polygon
    amph_logical <- lengths(amphtrop) > 0
    amph3 <- amph2[amph_logical,] # 6777 obs
    length(unique(amph3$binomial)) # 5990
    
    write_sf(amph3, '../Data/Raw/IUCN-SpeciesDistributions/AMPHIBIANS/AMPHIBIANS_filtered.shp')
    
   # ---- reptiles ----
    rept <- read_sf('../Data/Raw/IUCN-SpeciesDistributions/REPTILES/REPTILES.shp')
    glimpse(rept) # 10890 obs 
    # count number of sp
    length(unique(rept$binomial)) # 7860 sp
    
    # following conditions used to create these rasters: https://www.iucnredlist.org/resources/other-spatial-downloads
    rept2 <- rept %>% 
      select(binomial, presence, origin, seasonal, legend, class, category, terrestial) %>% 
      filter(terrestial=='true',
             presence %in% c(1,4),# stopped cos realised I should exclude 'considered extinct' following Tracewski
             origin %in% c(1,2)) 
    length(unique(rept2$binomial))
    
    # keep only species which fall within tropical extents
    repttrop <- st_intersects(rept2, types) # a lot quicker than st_join 
    # which points fall inside a polygon
    rept_logical <- lengths(repttrop) > 0
    rept3 <- rept2[rept_logical,] 
    length(unique(rept3$binomial))
    
    write_sf(mam3, '../Data/Raw/IUCN-SpeciesDistributions/REPTILES/IntersectsTropical_filtered_EPSG54009_REPTILES.shp')
    
   # ---- birds ----
    library(rgdal)
    pacman::p_load(sf, tidyverse)
    tropFor <- read_sf('../Data/Raw/TropicalForests_GADM_EPSG54009.shp')
    
   # use sf
    sf::sf_extSoftVersion()
    # GEOS           GDAL         proj.4 GDAL_with_GEOS     USE_PROJ_H
    # "3.8.1"        "3.1.4"        "6.3.1"         "true"         "true"
    
    birds <- st_read(dsn='../Data/Raw/BirdLifeInternational-BOTW2020/BOTW.gdb',
                     layer="All_Species")
      # In CPL_read_ogr(dsn, layer, query, as.character(options), quiet,  :
      #                   GDAL Message 1: organizePolygons() received a polygon with more than 100 parts.  The processing may be really slow.  You can skip the processing by setting METHOD=SKIP.
    #17566 obs; 11154 spp.
    
    # filter to extant (presence=1,2,3) and native (origin=1,2)
    birds <- birds %>% 
      dplyr::select(binomial, presence, origin, seasonal) %>% 
      filter(presence %in% c(1,2,3), 
             origin %in% c(1,2)) 
    # 16431 obs; #10967 spp.
    
    # reproject to 54009
    birds <- st_transform(birds, crs="ESRI:54009")
    birds <- st_buffer(birds, 0)
      # Error in CPL_geos_binop(st_geometry(x), st_geometry(y), op, par, pattern,  : 
      #                           Evaluation error: ParseException: Unknown WKB type 12.
      # from some kind of complicated geometry: https://github.com/r-spatial/sf/issues/427
      unique(st_geometry_type(st_geometry(birds)))
      # MULTIPOLYGON MULTISURFACE
    birds <- st_cast(birds, 'MULTIPOLYGON')
    birds <- st_buffer(birds, 0)
    # filter those that intersect tropical extents 
    trop <- st_intersects(birds, tropFor) 
    # which distributions fall inside tropical polygons
    trop_logical <- lengths(trop) > 0
    birds <- birds[trop_logical,] 
    # 13225 obs; 9984 sp.
    
  # filter to terrestrial using the biodiversitymapping.com list
    birdList <- read_csv('../Data/Raw/BirdLifeInternational-BOTW2020/Birds_species_lists_2019_03d14.csv')
    # 10732 sp
    
    birds_terr <- birds %>% 
      filter(binomial %in% birdList$ScientificName)
    # 12823 obs; 9713 sp
    
    birds_terr2 <- st_collection_extract(birds_terr, "POLYGON")
    write_sf(birds_terr2, '../Data/Raw/BirdLifeInternational-BOTW2020/IntersectsTropical_filtered_EPSG54009_BIRDS.gpkg')
  # ok gpkg works. I should stick with gpkg.
    
    spList <- as_tibble(birds) %>% 
      select(binomial) %>% 
      rename(Species = binomial) %>% 
      distinct()
    write_csv(spList, '../Data/Raw/BirdLifeInternational-BOTW2020/SpeciesList_IntersectsTropical.csv')
    
  # filter to forest habitat birds
    forestBirdBLI <- read_csv('../Data/Raw/BirdLifeInternational-BOTW2020/ForestSpecies_20210421_23675.csv')
    
    spListOverlapBLI <- spListOverlapBLI %>% 
      filter(Species %in% forestBirdBLI$`Scientific name`)
    write_csv(spList, '../Data/Raw/BirdLifeInternational-BOTW2020/ForestSpeciesList_IntersectsTropical.csv')
    
#### functions to extract habitat and category data ####
  # get API for the IUCN red list (http://apiv3.iucnredlist.org/about), then use rredlist package to get (https://docs.ropensci.org/rredlist/articles/rredlist.html_)
  # get habitat preference & altitudinal limit data for all species. 
  # for forest amphibians, habitat = forest only
  # for forest mammals, habitat = forest only. 
  # for forest birds, habitat = med/high forest dependency
  
  ICUN_REDLIST_KEY = 'b53de41d17cbbfbd1b2e739dd8033bafb84976ce9347b5ea59d3143e3a60003a'
    
  ## the usual progress bar option from here: https://cnuge.github.io/post/progress_bar/
  # Initiate the progress bar, progress is defined as a range from 0 to the length of x
  progress_bar = txtProgressBar(min=0, max=n, style = 1, char="=")
  #Increase the amount the progress bar is filled by setting the value to i.
  setTxtProgressBar(progress_bar, value = i)
  close(progress_bar)
      
  library(rredlist)
  library(tidyverse)
  
  # from here: https://github.com/bienflorencia/rBiodiversidata/blob/master/Data%20Cleaning%20and%20Standardisation%20Scripts/retrieve_IUCN_data.R
  # FUNCTION
    retrieve_IUCN_data <- function(speciesList){
      IUCN_status <- data.frame(Species = character(), Status = character(), 
                                Trend = character(), stringsAsFactors=FALSE)
      for(sp in speciesList){
        UICN_search <- rl_search(name = sp)
        if (length(UICN_search$result) == 0){
          IUCN_status_sp <- data.frame(Species = sp, 
                                       Status = 'NA', 
                                       Trend = 'NA', stringsAsFactors=FALSE)
          IUCN_status <- rbind(IUCN_status, IUCN_status_sp)
          cat(sp,'----- CHECK\n')
        }
        else {
          IUCN_status_sp <- data.frame(Species = UICN_search$result$scientific_name, 
                                       Status = UICN_search$result$category, 
                                       Trend = UICN_search$result$population_trend, stringsAsFactors=FALSE)
          IUCN_status <- rbind(IUCN_status, IUCN_status_sp)
        }
      }
      return(IUCN_status)
    }
  
  # what I wrote:
    retrieve_info <- function(speciesList){
      df <- data.frame(Species = character(), 
                       Class = character(),
                       Category = character(), 
                       Habitat = character(),
                       PopTrend = character(), 
                       Rep_AOO_km2 = numeric(),
                       Rep_EOO_km2 = numeric(),
                       Elev_lower = integer(),
                       Elev_upper = integer(),
                       stringsAsFactors=FALSE)
      print('Querying IUCN Red List API:')
      n = dim(speciesList)[1]
      ## get data for each sp in the list
      for(i in 1:n){ # would have used for(sp in speciesList) but need i for progress bar?
        # print a progress bar: https://stackoverflow.com/questions/26919787/r-text-progress-bar-in-for-loop
        extra <- nchar('||100%')
        width <- getOption('width')
        step <- round(i / n * (width - extra))
        text <- sprintf('|%s%s|% 3s%%', strrep('=', step),
                        strrep(' ', width - step - extra), round(i / n * 100))
        cat(text)
        cat(if (i == n) '\n' else '\r') # '\014' clears the console
        ## incorporate 1s delay between each query
        Sys.sleep(1)
        ## get habitat data from website
        sp <- speciesList$Species[i]
        iucnSearch <- rl_search(name=sp, key=apikey)
        # IF species cannot be found
        if (length(iucnSearch$result) == 0){ 
          spDf <- data.frame(Species = sp, 
                             Class = 'Not found', # speciesList$Class[i]
                             Category = 'Not found', # speciesList$Category[i]
                             Habitat = 'Not found',
                             PopTrend = 'Not found', 
                             Rep_AOO_km2 = NA,
                             Rep_EOO_km2 = NA,
                             Elev_lower = NA,
                             Elev_upper = NA,
                             stringsAsFactors=FALSE)
          df <- rbind(df, spDf)
        } else { # IF species is found
          # run search for habitat
          iucnHabitat <- rl_habitats(sp, key=apikey)
          # IF habitat cannot be found
          if (length(iucnHabitat$result)==0){
            spDf <- data.frame(Species = sp, 
                               Class = iucnSearch$result$class,
                               Category = iucnSearch$result$category, 
                               Habitat = 'Not found',
                               PopTrend = iucnSearch$result$population_trend, 
                               Rep_AOO_km2 = iucnSearch$result$aoo_km2,
                               Rep_EOO_km2 = iucnSearch$result$eoo_km2,
                               Elev_lower = iucnSearch$result$elevation_lower,
                               Elev_upper = iucnSearch$result$elevation_upper,
                               stringsAsFactors=FALSE)
            df <- rbind(df, spDf)
          } else { # IF there is habitat data
            ## extract habitat info to a single value
            spHabitat <- what_habitat(iucnHabitat$result)
            # IF habitat is only forest, or forest+others
            if(spHabitat != "Other habitats"){
              ## obtain elevation info
              spDf <- data.frame(Species = sp, 
                                 Class = iucnSearch$result$class,
                                 Category = iucnSearch$result$category, 
                                 Habitat = spHabitat,
                                 PopTrend = iucnSearch$result$population_trend, 
                                 Rep_AOO_km2 = iucnSearch$result$aoo_km2,
                                 Rep_EOO_km2 = iucnSearch$result$eoo_km2,
                                 Elev_lower = iucnSearch$result$elevation_lower,
                                 Elev_upper = iucnSearch$result$elevation_upper,
                                 stringsAsFactors=FALSE)
              df <- rbind(df, spDf)
            } else { # IF habitat is not forest
              spDf <- data.frame(Species = sp, 
                                 Class = iucnSearch$result$class,
                                 Category = iucnSearch$result$category, 
                                 Habitat = spHabitat,
                                 PopTrend = NA, 
                                 Rep_AOO_km2 = NA,
                                 Rep_EOO_km2 = NA,
                                 Elev_lower = NA,
                                 Elev_upper = NA,
                                 stringsAsFactors=FALSE)
              df <- rbind(df, spDf)
            }
          }
        }
      }
      return(df)
    }
     
  # adapted from here: https://gist.github.com/Martin-Jung/ad250b96f07944cf5f0b
  # this FUNCTION takes the habitat info of a species and returns a value of 
  # 'Other habitats' (non-forest sp), 
  # 'Forest important' (only forest listed and of major importance), 
  # 'Forest unimportant' (only forest listed but of unknown or not major importance), or 
  # 'Forest generalist' (forest and other habitats)
  what_habitat <- function(results){
    # collapse dataframe to vector 
    hab <- apply(results, 1, paste, collapse=" ")
    # create a vector to collect 'answers' for each row
    resp <- vector()
    # check the habitat info 
    for(i in 1:length(hab)){   
      # catch the condition that forest is not listed
      if(length(grep("Forest", hab[i]))==0) resp <- c(resp, 'others') else {
        # does habitat have 'forest' and major importance 'yes'?
        ix <- c( Reduce('&', lapply(c("Forest","Yes"), grepl, hab[i])),
                 Reduce('&', lapply(c("forest","yes"), grepl, hab[i])),
                 Reduce('&', lapply(c("Forest","yes"), grepl, hab[i])),
                 Reduce('&', lapply(c("forest","Yes"), grepl, hab[i])))
        if(any(ix)) resp <- c(resp,"forest-important")
        # does habitat have 'forest' and major importance 'no'?
        iy <- c( Reduce('&', lapply(c("Forest","No"), grepl, hab[i])),
                 Reduce('&', lapply(c("forest","no"), grepl, hab[i])),
                 Reduce('&', lapply(c("Forest","no"), grepl, hab[i])),
                 Reduce('&', lapply(c("forest","No"), grepl, hab[i])))
        if(any(iy)) resp <- c(resp,"forest-not-important")
        # does habitat have 'forest' and major importance 'NA'?
        iz <- c(Reduce('&', lapply(c("Forest","NA"), grepl, hab[i])),
                Reduce('&', lapply(c("forest","NA"), grepl, hab[i])))
        if(any(iz)) resp <- c(resp, "forest-NAimportance")
      }
    } # reduce the possible combinations to a single output
    # if forest is not listed as one of the habitats
    if(length(grep("others", resp))==length(resp)) return("Other habitats") else
      # if only forest is listed as habitats
      if(length(grep("forest", resp))==length(resp)) {
        # is forest habitat listed as important
        fi <- str_detect(resp,"forest-important")
        if(any(fi)) return("Forest important") else return("Forest unimportant")
      } else # if forest listed as one amongst others
        return("Forest generalist")
  }
  
#### checking retrieved IUCN data ####
  # ---- check duplicates ----
      # had saved the IUCN data for mammals amphibians and reptiles, but not for birds.
      IUCN_data <- rbind(others, birds)  
      # find out why there are two more observations than the original input (does not work for tibbles)
        IUCN_data[!which(IUCN_data$Species %in% spList$Species),]
        # none are outside the list given
        IUCN_data[which(duplicated(IUCN_data$Species)),]
        # two duplicates
        # Species    Class Category            Habitat   PopTrend
        # 4433 Sphaerodactylus dunni REPTILIA       LC Forest unimportant     Stable
        # 5243        Ninia espinali REPTILIA       NT Forest unimportant Decreasing
        # Rep_AOO_km2 Rep_EOO_km2 Elev_lower Elev_upper
        # 4433        <NA>      18,598         60        700
        # 5243        <NA>        3600       1040       2270
        IUCN_data[IUCN_data$Species=='Sphaerodactylus dunni',]
        IUCN_data[IUCN_data$Species=='Ninia espinali',]
        # the second entry of both have reported EOO data. keep second entries, remove first
        IUCN_data <- IUCN_data[-c(4432,5242),]
        # forgot to save this output so have to filter out again
      
      # using tibble
      others <- filter(others, !(Species=='Sphaerodactylus dunni' & Rep_EOO_km2 == 0))
      others <- filter(others, !(Species=='Ninia espinali' & Rep_EOO_km2 == 0))
      write_csv(others, '../Data/Raw/IUCN-SpeciesDistributions/IUCNdata_IntersectsTropical.csv')
        # crap overwrote the species list file. recreate the species list file (28 Apr)
        group_data <- read_sf('../Data/Raw/IUCN-SpeciesDistributions/MAMMALS_TERRESTRIAL_ONLY/IntersectsTropical_filtered_EPSG54009_MAMMALS_TERRESTRIAL_ONLY.shp')
        spList_group <- data.frame(Species=group_data$binomial, 
                                   Class=group_data$class,
                                   Category=group_data$category, 
                                   stringsAsFactors=FALSE)
        spList <- rbind(spList, spList_group)
        group_data <- read_sf('../Data/Raw/IUCN-SpeciesDistributions/AMPHIBIANS/IntersectsTropical_filtered_EPSG54009_AMPHIBIANS.shp')
        spList_group <- data.frame(Species=group_data$binomial, 
                                   Class=group_data$class,
                                   Category=group_data$category, 
                                   stringsAsFactors=FALSE)
        spList <- rbind(spList, spList_group)
        group_data <- read_sf('../Data/Raw/IUCN-SpeciesDistributions/REPTILES/IntersectsTropical_filtered_EPSG54009_REPTILES.shp')
        spList_group <- data.frame(Species=group_data$binomial, 
                                   Class=group_data$class,
                                   Category=group_data$category, 
                                   stringsAsFactors=FALSE)
        spList <- rbind(spList, spList_group)
        write_csv(spList, '../Data/Raw/IUCN-SpeciesDistributions/SpeciesList_IntersectsTropical.csv')
      
      # same with complete iucn data (mammals amphibians reptiles birds)  
      IUCN_data <- filter(IUCN_data, !(Species=='Sphaerodactylus dunni' & Rep_EOO_km2 == 0))
      IUCN_data <- filter(IUCN_data, !(Species=='Ninia espinali' & Rep_EOO_km2 == 0))
      write_csv(IUCN_data, '../Data/Raw/IUCNdata_IntersectsTropical.csv')
      
  # ---- check missing info ----
      ## check species which cannot be found
        toCheck <-IUCN_data %>% 
          filter(Class == 'Not found')
        print(toCheck$Species)
          # [1] "Calamaria linnaei"             "Melanosuchus niger"           
          # [3] "Mesaspis juarezi"              "Amerotyphlops tycherus"       
          # [5] "Pseudorabdion albonuchalis"    "Argyrophis roxaneae"          
          # [7] "Mesaspis viridiflava"          "Madatyphlops madagascariensis"
          # [9] "Crocodylus porosus"            "Xenotyphlops mocquardi"       
          # [11] "Indotyphlops ozakiae"          "Osteolaemus tetraspis"        
          # [13] "Dendrelaphis formosus"         "Calamaria margaritophora"     
          # [15] "Dipsas gaigeae"                "Oligodon hamptoni"            
          # [17] "Lycodon synaptor"              "Scinax camposseabrai"         
          # [19] "Murina leucogaster"            "Antigone rubicunda"           
          # [21] "Antigone antigone"             "Antigone canadensis"          
          # [23] "Antigone vipio"               
        
        # looks like it's because rl_habitats can't find these species
        # but they do exist in rl_search
        # re-writing function to search in rl_search first
        retrieve_info <- function(speciesList){
          df <- data.frame(Species = character(), 
                           Class = character(),
                           Category = character(), 
                           Habitat = character(),
                           PopTrend = character(), 
                           Rep_AOO_km2 = numeric(),
                           Rep_EOO_km2 = numeric(),
                           Elev_lower = integer(),
                           Elev_upper = integer(),
                           stringsAsFactors=FALSE)
          print('Querying IUCN Red List API:')
          n = dim(speciesList)[1]
          ## get data for each sp in the list
          for(i in 1:n){ # would have used for(sp in speciesList) but need i for progress bar?
            # print a progress bar: https://stackoverflow.com/questions/26919787/r-text-progress-bar-in-for-loop
            extra <- nchar('||100%')
            width <- getOption('width')
            step <- round(i / n * (width - extra))
            text <- sprintf('|%s%s|% 3s%%', strrep('=', step),
                            strrep(' ', width - step - extra), round(i / n * 100))
            cat(text)
            cat(if (i == n) '\n' else '\r') # '\014' clears the console
            ## incorporate 1s delay between each query
            Sys.sleep(1)
            ## get habitat data from website
            sp <- speciesList$Species[i]
            iucnSearch <- rl_search(name=sp, key=apikey)
            # IF species cannot be found
            if (length(iucnSearch$result) == 0){ 
              spDf <- data.frame(Species = sp, 
                                 Class = 'Not found', # speciesList$Class[i]
                                 Category = 'Not found', # speciesList$Category[i]
                                 Habitat = 'Not found',
                                 PopTrend = 'Not found', 
                                 Rep_AOO_km2 = NA,
                                 Rep_EOO_km2 = NA,
                                 Elev_lower = NA,
                                 Elev_upper = NA,
                                 stringsAsFactors=FALSE)
              df <- rbind(df, spDf)
            } else { # IF species is found
              # run search for habitat
              iucnHabitat <- rl_habitats(sp, key=apikey)
              # IF habitat cannot be found
              if (length(iucnHabitat$result)==0){
                spDf <- data.frame(Species = sp, 
                                   Class = iucnSearch$result$class,
                                   Category = iucnSearch$result$category, 
                                   Habitat = 'Not found',
                                   PopTrend = iucnSearch$result$population_trend, 
                                   Rep_AOO_km2 = iucnSearch$result$aoo_km2,
                                   Rep_EOO_km2 = iucnSearch$result$eoo_km2,
                                   Elev_lower = iucnSearch$result$elevation_lower,
                                   Elev_upper = iucnSearch$result$elevation_upper,
                                   stringsAsFactors=FALSE)
                df <- rbind(df, spDf)
              } else { # IF there is habitat data
                ## extract habitat info to a single value
                spHabitat <- what_habitat(iucnHabitat$result)
                # IF habitat is only forest, or forest+others
                if(spHabitat != "Other habitats"){
                  ## obtain elevation info
                  spDf <- data.frame(Species = sp, 
                                     Class = iucnSearch$result$class,
                                     Category = iucnSearch$result$category, 
                                     Habitat = spHabitat,
                                     PopTrend = iucnSearch$result$population_trend, 
                                     Rep_AOO_km2 = iucnSearch$result$aoo_km2,
                                     Rep_EOO_km2 = iucnSearch$result$eoo_km2,
                                     Elev_lower = iucnSearch$result$elevation_lower,
                                     Elev_upper = iucnSearch$result$elevation_upper,
                                     stringsAsFactors=FALSE)
                  df <- rbind(df, spDf)
                } else { # IF habitat is not forest
                  spDf <- data.frame(Species = sp, 
                                     Class = NA,
                                     Category = NA, 
                                     Habitat = spHabitat,
                                     PopTrend = NA, 
                                     Rep_AOO_km2 = NA,
                                     Rep_EOO_km2 = NA,
                                     Elev_lower = NA,
                                     Elev_upper = NA,
                                     stringsAsFactors=FALSE)
                  df <- rbind(df, spDf)
                }
              }
            }
          }
          return(df)
        }
        missingData <- retrieve_info2(toCheck)
          # except for the 4 Antigone birds, rest are found in IUCN red list
        
        IUCNdata <- IUCN_data %>% 
          filter(!(Species %in% toCheck$Species)) %>% 
          rbind(missingData)
        
        # turns out Antigone is old genus name, should be Grus now
        test <- toCheck %>% 
          mutate(Species = str_replace(Species, 'Antigone', 'Grus')) %>% 
          filter(str_detect(Species, 'Grus'))
        
        missingBirds <- retrieve_info2(test)
        
        IUCNdata <- IUCNdata %>% 
          filter(!(str_detect(Species, 'Antigone'))) %>% 
          rbind(missingBirds)
         
      # okay all good now
        write_csv(IUCNdata, '../Data/Raw/IUCNdata_IntersectsTropical_allClasses.csv')
  # ---- cross check with tracewski? ----
        ## compare with Tracewski et al 2016's list?
        tracewski <- read.csv('~/Google Drive/Background readings/Biodiversity/Tracewski et al 2016 - SM sp list.csv')
        
        forestOnly <- IUCNdata %>% 
          filter(Habitat=='Forest important'|Habitat=='Forest unimportant') %>% 
          mutate(Class = as.factor(Class),
                 Category = as.factor(Category),
                 Habitat = as.factor(Habitat),
                 PopTrend = as.factor(PopTrend),
                 Rep_AOO_km2 = as.numeric(Rep_AOO_km2),
                 Rep_EOO_km2 = as.numeric(Rep_EOO_km2))
        
        inboth <- forestOnly[forestOnly$Species %in% tracewski$name,]
        # Species               Class      Category                Habitat    
        # Length:3933        AMPHIBIA: 753   CR: 202   Forest important  :3027  
        # Class :character   AVES    :2289   DD: 256   Forest unimportant: 906  
        # Mode  :character   MAMMALIA: 891   EN: 427                            
        #                    REPTILIA:   0   EX:   1                            
        #                    LC:2176                            
        #                    NT: 410                            
        #                    VU: 461                                                 
        
        # maybe compare with the first species list
        spList_tracewski <- IUCNdata[IUCNdata$Species %in% tracewski$name,]
        # Species               Class         Category                  Habitat    
        # Length:9103        AMPHIBIA:2702   LC     :5650   Forest generalist :5126  
        # Class :character   AVES    :5204   VU     : 901   Forest important  :3027  
        # Mode  :character   MAMMALIA:1154   EN     : 864   Forest unimportant: 906  
        #                    REPTILIA:   0   NT     : 802   Not found         :   1  
        #                    NA's    :  43   DD     : 498   Other habitats    :  43  
        #                                    (Other): 345                            
        #                                    NA's   :  43         
        
        # not exactly the same. after discussing with supervisors, decide to just stick
        # with criteria listed and not use Tracewski's list
#### function to scrape BirdLife website for forest dependency ####
  # from Simon's script: https://github.com/jsocolar/colombiaBeta/blob/master/bird_lists_maps_traits/birdlife_scraper.R
  # code from traits package on extracting birdlife data: https://rdrr.io/cran/traits/src/R/birdlife.R   
  
  scrape_birdlife <- function(speciesList){
    n = nrow(speciesList)
    ## add a new column to the current list for forest dependency
    df <- speciesList %>% 
      add_column(ForestDependency = NA)
    print('Checking BirdLife Datazone:')
    ## run through the list
    for (i in 1:n){
      ## print a progress bar: https://stackoverflow.com/questions/26919787/r-text-progress-bar-in-for-loop
      extra <- nchar('||100%')
      width <- getOption('width')
      step <- round(i / n * (width - extra))
      text <- sprintf('|%s%s|% 3s%%', strrep('=', step),
                      strrep(' ', width - step - extra), round(i / n * 100))
      cat(text)
      cat(if (i == n) '\n' else '\r') # '\014' clears the console
      
      ## incorporate 1s delay between each query
      Sys.sleep(1)
      
      ## apply trycatch to skip if run into error
      tryCatch({ # try to get data
        ## obtain the url
        sn <- speciesList$Species[i]
        cn <- speciesList$CommonName[i]
        cn <- gsub("'", "", cn)
        cn <- gsub('á', 'a', cn)
        cn <- gsub('ç', 'c', cn)
        cn <- gsub('ä', 'a', cn)
        cn <- gsub('ü', 'u', cn)
        cn <- gsub('ö','o', cn)
        urlname <- paste(c(strsplit(cn, ' ')[[1]], strsplit(sn, ' ')[[1]]), sep = '-', collapse = '-')
        theurl <- paste0('http://datazone.birdlife.org/species/factsheet/', urlname, '/details')
        # alternative way of getting url
        # bl_base <- "http://datazone.birdlife.org/species/factsheet"
        # bl_url <- function(x) sprintf("%s/%s/details", bl_base, x)
        # theurl <- bl_url(urlname)
        
        ## read html from website
        web <- read_html(theurl)
        
        ## extract table data from html
        tables <- html_elements(web, '.table') %>% 
          html_table()
        
        ## find the table which lists forest dependency
        fdTable <- tables[grep('Forest dependency', tables)]
        
        ## input value into my table
        df$ForestDependency[i] <- fdTable[[1]]$X4[1]
      }, error=function(cond){ # optional error catching
        cat('Error at ', speciesList$Species[i], conditionMessage(cond), '\n')
      }, warning=function(cond){ # optional warning catching
        cat('Warning issued at ', speciesList$Species[i], conditionMessage(cond), '\n')
      })
    }
    return(df)
  }
  ## other options for extracting forest dependency data
  ## option ONE: use readLines
  test <- readLines(theurl)
  fdLine <- grep('Forest dependency', test)
  test[fdLine+1] # not very reliable
  
  ## option TWO: get line from html
  sth <- html_elements(web, '.table') %>% 
    html_children() %>% 
    html_text()
  fdLine <- grep('Forest dependency', sth)
  sth[fdLine]
  
#### get bird forest dependency data ----
  # check with Simon's data
  birdTraits <- readRDS('../Data/Raw/BirdLifeInternational-BOTW2020/birdlife_traits_df.rds')
  
  birdForest <- forestOnly %>% 
    filter(Class == 'AVES')
  
  birdForestList <- birdForest[birdForest$Species %in% birdTraits$species,]
    # only 1357 of my list overlaps with Simon's. I wonder why so few.
  notOverlap <- birdForest[!(birdForest$Species %in% birdTraits$species),]
    # so looks like Simon's data is not global?
    # or it is somewhat but not completely...
  
 # get the forest bird list to check
  IUCNdata <- read_csv('../Data/Raw/IUCNdata_IntersectsTropical_allClasses.csv')
  forestBirdBLI <- read_csv(birdList <- read_csv('../Data/Raw/BirdLifeInternational-BOTW2020/ForestSpecies_20210421_23675.csv'))
  
  # join the list we have for IUCN data with birdlife data because need common name
  IUCNbirds <- IUCNdata %>% 
    filter(Class == 'AVES') %>% 
    left_join(forestBirdBLI, by=c('Species'='Scientific name')) %>% 
    rename(CommonName = `English name`) %>% 
    select(-`Global IUCN Red List Category`)
  
  write_csv(IUCNbirds, '../Data/Raw/BirdLifeInternational-BOTW2020/IUCNdata_IntersectsTropical_ForestBirds.csv')
  
#### checking birdlife forest dependence data ####
  # ---- merge with v9 data ----
  # v9 data sent by Emma Hughes
  BLv9 <- read.csv('../Data/Raw/BirdLifeInternational-BOTW2020/BirdLife_Checklist_Version_9_Data.csv')
  
  BirdLifeData2 <- BirdLifeData %>% 
    left_join(BLv9[,c(5,18)], by=c('Species'='Scientific.name'))
  
  # for some reason or other I end up with 8927 obs instead of just 7931?
  BL3 <- BirdLifeData2 %>% 
    distinct() # less 72 rows
  # find out which in bl2 not in bl1
  extra <- BL3 %>% 
    filter(duplicated(BL3$Species)|duplicated(BL3$Species, fromLast = TRUE))
  # looks like for 924 species, entries are duplicated and one version has NA for forest.dependency
  nas <- BL3[is.na(BL3$forest.dependency),]
  # but there are 932 NAs in that column so can't just remove all entries with NA there.
  extra2 <- BL3 %>% 
    filter(duplicated(BL3$Species)) 
  all(is.na(extra2$forest.dependency)) # TRUE

  # looks like these 924 obs are all NAs for forest.dependency.
  BL4 <- BL3 %>% 
    anti_join(extra2)
  # yay it's 7931 now! check it's the same 7931 as before
  all(BL4$Species %in% IUCNbirds$Species) # TRUE
  all(IUCNbirds$Species %in% BL4$Species) # TRUE
  # ok I think this is good
  
  # ---- check for consistency across v9 and v10 ----
  # check that the data I extracted ForestDependency is the same
  # as Emma's dataset from forest.dependency
  all(BL4$ForestDependency == BL4$forest.dependency) # FALSE
  
  # create a col where if v10 is same as v9, take the v10 value, else enter 'check'
  BL4 <- BL4 %>% 
    mutate(ForestDependence = ifelse(ForestDependency==forest.dependency, ForestDependency, 'check'))
  
  print(filter(BL4, ForestDependence=='check'), width=Inf)
    # 1 Cercococcyx mechowi   AVES  LC       Forest important  Decreasing
    # 2 Aramides cajaneus     AVES  LC       Forest generalist Decreasing
    # 3 Eclectus roratus      AVES  LC       Forest generalist Decreasing
    # 4 Ortalis guttata       AVES  LC       Forest generalist Unknown   
    # 5 Pyrrhura melanura     AVES  LC       Forest important  Decreasing
    # 6 Zosterops everetti    AVES  LC       Forest generalist Decreasing
    # 7 Zosterops japonicus   AVES  LC       Forest generalist Unknown   
    # 8 Zosterops palpebrosus AVES  LC       Forest generalist Decreasing
    # 9 Lamprolia klinesmithi AVES  VU       Forest generalist Decreasing
    # 10 Atlapetes blancae
  print(filter(BL4, is.na(ForestDependency)), width=Inf)
    # 1 Cossypha semirufa      AVES  LC       Forest generalist  Decreasing
    # 2 Cyornis ruckii         AVES  CR       Forest unimportant Unknown   
    # 3 Geokichla oberlaenderi AVES  NT       Forest important   Decreasing
    # 4 Merops boehmi          AVES  LC       Forest generalist  Stable    
    # 5 Neafrapus boehmi       AVES  LC       Forest generalist  Decreasing
    # 6 Phylloscopus soror     AVES  LC       Forest important   Stable    
    # 7 Laniarius luehderi     AVES  LC       Forest generalist  Stable    
    # 8 Laniarius fuelleborni 
        
  # ok so problem species are
    toCheck <- rbind(filter(BL4, is.na(ForestDependency)), filter(BL4, ForestDependence=='check'))
    
    # code to extract from web
    sn <- toCheck$Species[i]
    cn <- toCheck$CommonName[i]
    urlname <- paste(c(strsplit(cn, ' ')[[1]], strsplit(sn, ' ')[[1]]), sep = '-', collapse = '-')
    theurl <- paste0('http://datazone.birdlife.org/species/factsheet/', urlname, '/details')
    web <- read_html(theurl)
    tables <- html_elements(web, '.table') %>% 
      html_table()
    fdTable <- tables[grep('Forest dependency', tables)]
    
    # for 1-8, it's cos of not gsubing characters enough
      cn <- toCheck$CommonName[i]
    # for 1/2/7/8 is cos ü not replaced by u, 3 is ä, 4/5/6 is ö
    
    # for 9-18, it's cos the website shows sth different to v9
      toCheck[9:18,13:15]
    # 9. website says not normally occur, v9 says high forest dep. will take v9
    # for 10/12/14:16, website lists unset lol. will take v9  
    # for 11/13/17, website says High, v9 says Med, that's ok
    # for 17, website says Low, v9 is unknown, will take website
      
    # check against tracewski as well
    tracewski <- read.csv('~/Google Drive/Background readings/Biodiversity/Tracewski et al 2016 - SM sp list.csv')
    filter(tracewski, name==sn)
    toCheck$Species %in% tracewski$name
    # [1]  TRUE FALSE FALSE FALSE  TRUE 
    # [6] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    # [12] FALSE  TRUE  TRUE FALSE  TRUE FALSE FALSE
    
  # sort out these toCheck ones (prob more efficient ways but I cant be bothered)
    toCheck2 <- toCheck %>% 
      mutate(ForestDependency = replace(ForestDependency, Species=="Cossypha semirufa", 'Medium'),
             ForestDependency = replace(ForestDependency, Species=="Cyornis ruckii", 'Unknown'),
             ForestDependency = replace(ForestDependency, Species=="Geokichla oberlaenderi", 'High'),
             ForestDependency = replace(ForestDependency, Species=="Merops boehmi", 'Low'),
             ForestDependency = replace(ForestDependency, Species=="Neafrapus boehmi", 'Medium'),
             ForestDependency = replace(ForestDependency, Species=="Phylloscopus soror", 'High'),
             ForestDependency = replace(ForestDependency, Species=="Laniarius luehderi", 'Medium'),
             ForestDependency = replace(ForestDependency, Species=="Laniarius fuelleborni", 'Medium')) %>% 
      mutate(ForestDependence = replace(ForestDependence, Species=="Cossypha semirufa", 'Medium'),
             ForestDependence = replace(ForestDependence, Species=="Cyornis ruckii", 'Unknown'),
             ForestDependence = replace(ForestDependence, Species=="Geokichla oberlaenderi", 'High'),
             ForestDependence = replace(ForestDependence, Species=="Merops boehmi", 'Low'),
             ForestDependence = replace(ForestDependence, Species=="Neafrapus boehmi", 'Medium'),
             ForestDependence = replace(ForestDependence, Species=="Phylloscopus soror", 'High'),
             ForestDependence = replace(ForestDependence, Species=="Laniarius luehderi", 'Medium'),
             ForestDependence = replace(ForestDependence, Species=="Laniarius fuelleborni", 'Medium'),
             ForestDependence = replace(ForestDependence, Species=="Cercococcyx mechowi", 'High'), # cos website seems wrong
             ForestDependence = replace(ForestDependence, Species=="Aramides cajaneus", 'Medium'), # if unset, take v9 value
             ForestDependence = replace(ForestDependence, Species=="Eclectus roratus", 'High'), # if v10 is H and v9 M, keep High
             ForestDependence = replace(ForestDependence, Species=="Ortalis guttata", 'Low'),
             ForestDependence = replace(ForestDependence, Species=="Pyrrhura melanura", 'High'),
             ForestDependence = replace(ForestDependence, Species=="Zosterops everetti", 'Medium'),
             ForestDependence = replace(ForestDependence, Species=="Zosterops japonicus", 'Low'),
             ForestDependence = replace(ForestDependence, Species=="Zosterops palpebrosus", 'Medium'),
             ForestDependence = replace(ForestDependence, Species=="Lamprolia klinesmithi", 'High'),
             ForestDependence = replace(ForestDependence, Species=="Atlapetes blancae", 'Low'))
      
    # replace the missing/erroneous data with the 'fixed' 18 rows 
    BL5 <- BL4 %>% 
      anti_join(toCheck) %>% 
      rbind(toCheck2) %>% 
      rename(ForestDep_v10 = ForestDependency,
             ForestDep_v9 = forest.dependency,
             ForestDep_final = ForestDependence)
    
    # oops I missed out looking at NAs in BL4$ForestDependence, that I created with an ifelse statement of v9 == v10
    # which came about because v9 value was NA
    BL5[is.na(BL5$ForestDep_final),c(1,4,13:15)]
    # ah ok, cos v9 is NA. 
    BL5[is.na(BL5$ForestDep_v9),c(1,4,13:15)] # same
    # for values that are NA in v9, give the value from v10
    BL5[is.na(BL5$ForestDep_v9),15] <- BL5[is.na(BL5$ForestDep_v9),13]
    
    # great, let's save this version
    write_csv(BL5, '../Data/Raw/BirdLifeInternational-BOTW2020/BirdLifeData_IUCNdata_IntersectsTropical_ForestBirds.csv')
    
    # and get a 'clean' version to use moving forward.
    BL6 <- BL5 %>% 
      dplyr::select(-c(ForestDep_v10, ForestDep_v9)) %>% 
      rename(ForestDependency = ForestDep_final) 
    write_csv(BL6, '../Data/Raw/BirdLifeInternational-BOTW2020/BirdLifeData_IUCNdata_IntersectsTropical_ForestBirds_clean.csv')
    
    # now get just the med and high forest dependency birds
    BL7 <- BL6 %>% 
      filter(ForestDependency=='Medium'|ForestDependency=='High') %>% # 6459 obs
      dplyr::select(-c(SpcRecID, CommonName, Family))
    
    write_csv(BL7, '../Data/Raw/IUCNdata_IntersectsTropical_Birds.csv')
    
##### rasterise shapefiles ####
  # ---- get final list of forest vertebrates ----
    others <- read_csv('../Data/Raw/IUCNdata_IntersectsTropical_others.csv') # 15505
    birds <- read_csv('../Data/Raw/IUCNdata_IntersectsTropical_Birds.csv') # 6459
    
    ## join the two datasets
    birds <- birds %>% 
      dplyr::select(-c(SpcRecID, CommonName, Family))
    
    finalList <- others %>% 
      filter(Habitat=='Forest important'|Habitat=='Forest unimportant') %>% #5414 obs
      mutate(ForestDependency = NA) %>% 
      rbind(birds) %>% 
      mutate(Class = as.factor(Class),
             Category = as.factor(Category),
             Habitat = as.factor(Habitat),
             PopTrend = as.factor(PopTrend),
             Rep_AOO_km2 = as.numeric(Rep_AOO_km2),
             Rep_EOO_km2 = as.numeric(Rep_EOO_km2),
             ForestDependency = as.factor(ForestDependency))

    summary(finalList) # hm strange, there's one in EX category
    print(finalList[which(finalList$Category=='EX'),], width=Inf)
    #     Species         Class    Category Habitat            
    #   1 Sus bucculentus MAMMALIA EX       Forest unimportant
    # lol in the IUCN shapefile presence=2 (probably extant) when it's in extinct category
    # remove it
    finalList <- finalList %>% 
      filter(!Species=='Sus bucculentus') # 11872 obs
    write_csv(finalList, '../Data/Raw/FinalForestSpeciesList.csv')
    #     Class      Category                Habitat    
    # AMPHIBIA:1472   CR: 533   Forest generalist :3630  
    # AVES    :6459   DD:1108   Forest important  :5757  
    # MAMMALIA:1759   EN:1060   Forest unimportant:2485  
    # REPTILIA:2182   EX:   0                            
    #                 LC:7139                            
    #                 NT:1001                            
    #                 VU:1031                           

  # ---- split shapefiles into indv species ----
  # to save memory space
    pacman::p_load(tidyverse, sf, raster, rgdal)
  
    shpFiles <- c('../Data/Raw/IUCN-SpeciesDistributions/MAMMALS_TERRESTRIAL_ONLY/IntersectsTropical_filtered_EPSG54009_MAMMALS_TERRESTRIAL_ONLY.shp',
                  '../Data/Raw/IUCN-SpeciesDistributions/AMPHIBIANS/IntersectsTropical_filtered_EPSG54009_AMPHIBIANS.shp',
                  '../Data/Raw/IUCN-SpeciesDistributions/REPTILES/IntersectsTropical_filtered_EPSG54009_REPTILES.shp',
                  '../Data/Raw/BirdLifeInternational-BOTW2020/IntersectsTropical_filtered_EPSG54009_BIRDS.gpkg')
    speciesList <- read.csv('../Data/Raw/FinalForestSpeciesList.csv')
    
    # function to split the shapefile by species 
    split_shapes <- function(shapefile, taxa, path_out){
      speciesList <- unique(shapefile$binomial)
      for (i in 1:length(speciesList)){
        speciesFile <- shapefile[which(shapefile$binomial == speciesList[i]),]
        species <- paste0(strsplit(speciesList[i], " ")[[1]][1:2], collapse="_")
        fname <- paste0(path_out, '/', taxa, '-', species, ".gpkg")
        write_sf(speciesFile, fname)
      }}
    
    # run for all the 4 taxa
    for (shapefile in shpFiles){
      taxaFile <- read_sf(shapefile)
      taxa <- strsplit(strsplit(basename(shapefile), '\\_')[[1]][4], '\\.')[[1]][1]
      cat(taxa, '\n')
      path_out <- '../Data/Raw/ForestVertebrateShapefiles'
      # remove species which have already been split
      alrPresent <- unname(sapply(list.files(path_out, pattern="\\.gpkg"), function(x) gsub("_", " ", gsub("^[^-]*-([^.]+).*", "\\1", x))))
      # filter to only forest vertebrate species
      taxaFile <- taxaFile %>% 
        filter(binomial %in% speciesList$Species,
               !(binomial %in% alrPresent))
      # split the polygons by species
      speciesFile <- split_shapes(taxaFile, taxa, path_out)
      }
  # ---- try to rasterise ----
    pacman::p_load(tidyverse, sf, raster, fasterize, parallel)
    
    # ---- Wrapper around mclapply to track progress (not used) -----------------------------------
    # from here: https://stackoverflow.com/questions/10984556/is-there-way-to-track-progress-on-a-mclapply
    #' Wrapper around mclapply to track progress
    #' 
    #' Based on http://stackoverflow.com/questions/10984556
    #' 
    #' @param X         a vector (atomic or list) or an expressions vector. Other
    #'                  objects (including classed objects) will be coerced by
    #'                  ‘as.list’
    #' @param FUN       the function to be applied to
    #' @param ...       optional arguments to ‘FUN’
    #' @param mc.preschedule see mclapply
    #' @param mc.set.seed see mclapply
    #' @param mc.silent see mclapply
    #' @param mc.cores see mclapply
    #' @param mc.cleanup see mclapply
    #' @param mc.allow.recursive see mclapply
    #' @param mc.progress track progress?
    #' @param mc.style    style of progress bar (see txtProgressBar)
    #'
    #' @examples
    #' x <- mclapply2(1:1000, function(i, y) Sys.sleep(0.01))
    #' x <- mclapply2(1:3, function(i, y) Sys.sleep(1), mc.cores=1)
    #' 
    #' dat <- lapply(1:10, function(x) rnorm(100)) 
    #' func <- function(x, arg1) mean(x)/arg1 
    #' mclapply2(dat, func, arg1=10, mc.cores=2)
    
    mclapply2 <- function(X, FUN, ..., 
                          mc.preschedule = TRUE, mc.set.seed = TRUE,
                          mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
                          mc.cleanup = TRUE, mc.allow.recursive = TRUE,
                          mc.progress=TRUE, mc.style=3) 
    {
      if (!is.vector(X) || is.object(X)) X <- as.list(X)
      
      if (mc.progress) {
        f <- fifo(tempfile(), open="w+b", blocking=T)
        p <- parallel:::mcfork()
        pb <- txtProgressBar(0, length(X), style=mc.style)
        setTxtProgressBar(pb, 0) 
        progress <- 0
        if (inherits(p, "masterProcess")) {
          while (progress < length(X)) {
            readBin(f, "double")
            progress <- progress + 1
            setTxtProgressBar(pb, progress) 
          }
          cat("\n")
          parallel:::mcexit()
        }
      }
      tryCatch({
        result <- mclapply(X, ..., function(...) {
          res <- FUN(...)
          if (mc.progress) writeBin(1, f)
          res
        }, 
        mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed,
        mc.silent = mc.silent, mc.cores = mc.cores,
        mc.cleanup = mc.cleanup, mc.allow.recursive = mc.allow.recursive
        )
        
      }, finally = {
        if (mc.progress) close(f)
      })
      result
    }
    
    # ------- function to rasterise species range files ------
    
    # code from rasterizeIUCN from rasterSp package: https://github.com/RS-eco/rasterSp/blob/master/R/rasterizeIUCN.R
    # using raster package and fasterize
     rasterise_shapes <- function(listFiles, tmp, pathOut){
      # filter out species files that have already been rasterised. cant figure out a better way to do this
      alrPresent <- unname(sapply(list.files(pathOut, pattern='\\.tif'), function(x) str_split(basename(x), '\\.')[[1]][1]))
      allFiles <- unname(sapply(listFiles, function(x) str_split(basename(x), '\\.')[[1]][1]))
      toRasterise <- unname(sapply(setdiff(allFiles, alrPresent), function(x) paste0(dirname(listFiles[1]), '/', x, '.gpkg')))
      # number of species
      n <- length(toRasterise)
      # Create empty global raster 
      r <- raster(tmp, res=1000)
      
      # start parallel processing 
      noCores <- ceiling(0.5*parallel::detectCores())
      
      # if it's running on Mac
      if (.Platform$OS.type == "unix"){
        ### using FORK method
        speciesRasters <- mclapply(1:n, function(n){
          # read in shapefile
          speciesFile <- read_sf(toRasterise[n])
          species <- strsplit(basename(toRasterise[n]), '\\.')[[1]][1]
          tryCatch({
            # if there are more than one polygon for the species
            if(nrow(speciesFile) > 1){
              r_poly <- lapply(1:nrow(speciesFile), function(x){fasterize::fasterize(speciesFile[x,], r, background=0)})
              r_poly <- do.call(raster::merge, r_poly)
            } else if(nrow(speciesFile) == 1){ # if there is only one polygon
              r_poly <- fasterize::fasterize(speciesFile, r, background=0)
            }
            # write out the raster for the species
            raster::writeRaster(r_poly, filename=paste0(pathOut, '/', species, ".tif"),
                                format="GTiff")
            rm(r_poly, speciesFiles)
            gc()}, 
            error=function(cond){
              cat('Error at ', species, conditionMessage(cond), '\n')})}, 
          mc.cores=noCores)
      } else {
        ### using PSOCK method
        # Initiate cluster
        cl <- parallel::makeCluster(noCores)
        parallel::clusterEvalQ(cl, sapply(c("raster","sf", "rgdal", "fasterize"), require, char=TRUE))
        #  variables need to be shared across clusters
        parallel::clusterExport(cl, list("pathOut","toRasterise","n","r"), envir=environment())
        # start parallel processing to convert species distribution to a raster w
        speciesRasters <- parallel::parLapply(cl, 1:n, function(n){
          # read in shapefile
          speciesFile <- read_sf(toRasterise[n])
          species <- strsplit(basename(toRasterise[n]), '\\.')[[1]][1]
          tryCatch({
            # if there are more than one polygon for the species   
            if(nrow(speciesFile) > 1){
              r_poly <- lapply(1:nrow(speciesFile), function(x){fasterize::fasterize(speciesFile[x,], r, background=0)})
              r_poly$fun <- max
              r_poly <- do.call(raster::mosaic, r_poly)
            } else if(nrow(speciesFile) == 1){ # if there is only one polygon
              r_poly <- fasterize::fasterize(speciesFile, r, background=0)
            }
            # if the polygon is too small that centre of cell doesn't fall within polygon so that
            # species raster ends up being all 0s, turn polygon to lines
            if(all(values(r_poly)==0)){
              speciesLines <- st_cast(speciesFile, "MULTILINESTRING")
              r_poly <- raster::rasterize(speciesLines, tmp, field=1, background=0)
            }
            # write out the raster for the species
            raster::writeRaster(r_poly, filename=paste0(pathOut, '/', species, ".tif"),
                                format="GTiff")
            rm(r_poly, speciesFile)
            gc()}, 
            error=function(cond){
              cat('Error at ', species, conditionMessage(cond), '\n')})})
        # Close the cluster
        parallel::stopCluster(cl)
      }
     } 
    
    # faster version using terra package (doesnt work cos terra doesnt parallelise)
    terra_rasterise_shapes <- function(listFiles, tmp, pathOut){
    # filter out species files that have already been rasterised. cant figure out a better way to do this
    alrPresent <- unname(sapply(list.files(pathOut, pattern='\\.tif'), function(x) str_split(basename(x), '\\.')[[1]][1]))
    allFiles <- unname(sapply(listFiles, function(x) str_split(basename(x), '\\.')[[1]][1]))
    toRasterise <- unname(sapply(setdiff(allFiles, alrPresent), function(x) paste0(dirname(listFiles[1]), '/', x, '.gpkg')))
    # number of species
    n <- length(toRasterise)
    # Create empty global raster (should not do that)
    r <- rast(tmp, res=1000)
    
    # start parallel processing 
    noCores <- ceiling(0.5*parallel::detectCores())
    
    # if it's running on Mac
    if (.Platform$OS.type == "unix"){
      ### using FORK method
      speciesRasters <- mclapply(1:n, function(n){
        # read in shapefile
        speciesFile <- vect(toRasterise[n])
        species <- strsplit(basename(toRasterise[n]), '\\.')[[1]][1]
        r_poly <- terra::rasterize(speciesFile, tmp, background=0)
        terra::writeRaster(r_poly, filename=paste0(pathOut, '/', species, ".tif"))
        }, mc.cores=noCores)
    } else {
      ### using PSOCK method
      # Initiate cluster
      cl <- parallel::makeCluster(noCores)
      parallel::clusterEvalQ(cl, sapply(c("terra", "rgdal"), require, char=TRUE))
      #  variables need to be shared across clusters
      parallel::clusterExport(cl, list("pathOut","toRasterise","n","r"), envir=environment())
      # start parallel processing to convert species distribution to a raster w
      speciesRasters <- parallel::parLapply(cl, 1:n, function(n){
        # read in shapefile
        speciesFile <- vect(toRasterise[n])
        species <- strsplit(basename(toRasterise[n]), '\\.')[[1]][1]
        r_poly <- terra::rasterize(speciesFile, tmp, background=0)
        terra::writeRaster(r_poly, filename=paste0(pathOut, '/', species, ".tif"))
      })
      # Close the cluster
      parallel::stopCluster(cl)
      }
    } 
    #### run data ----
    
    speciesFiles <- list.files('../Data/Raw/ForestVertebrateShapefiles', '\\.gpkg', full.names=TRUE)
    alrPresentinMac <- c("AMPHIBIANS-Adenomera_coca","AMPHIBIANS-Andinobates_bombetes",
                         "AMPHIBIANS-Anodonthyla_pollicaris","AMPHIBIANS-Austrochaperina_fryi",
                         "AMPHIBIANS-Austrochaperina_mehelyi","AMPHIBIANS-Bolitoglossa_celaque", 
                         "AMPHIBIANS-Bolitoglossa_suchitanensis","AMPHIBIANS-Choerophryne_proboscidea",
                         "AMPHIBIANS-Cophyla_karenae","AMPHIBIANS-Cophyla_tetra",
                         "AMPHIBIANS-Copiula_annanoreenae","AMPHIBIANS-Craugastor_xucanebi",
                         "AMPHIBIANS-Cycloramphus_diringshofeni","AMPHIBIANS-Ecnomiohyla_fimbrimembra" ,
                         "AMPHIBIANS-Gephyromantis_malagasius","AMPHIBIANS-Ingerana_charlesdarwini",
                         "AMPHIBIANS-Ischnocnema_gualteri","AMPHIBIANS-Mantophryne_insignis",
                         "AMPHIBIANS-Niceforonia_fallaciosa","AMPHIBIANS-Oedipina_nimaso",
                         "AMPHIBIANS-Oreobates_lehri", "AMPHIBIANS-Oreophryne_crucifer",
                         "AMPHIBIANS-Oreophryne_sibilans","AMPHIBIANS-Philautus_abditus",
                         "AMPHIBIANS-Philautus_hosii" ,"AMPHIBIANS-Philautus_mjobergi",
                         "AMPHIBIANS-Phrynobatrachus_intermedius","AMPHIBIANS-Platymantis_luzonensis",
                         "AMPHIBIANS-Plethodontohyla_guentheri","AMPHIBIANS-Pristimantis_adnus" ,
                         "AMPHIBIANS-Pristimantis_celator","AMPHIBIANS-Pristimantis_condor" ,
                         "AMPHIBIANS-Pristimantis_kichwarum", "AMPHIBIANS-Pristimantis_maculosus" ,
                         "AMPHIBIANS-Pristimantis_pirrensis","AMPHIBIANS-Pristimantis_rozei",
                         "AMPHIBIANS-Pristimantis_viridis","AMPHIBIANS-Raorchestes_chlorosomma",
                         "AMPHIBIANS-Rhinella_festae","AMPHIBIANS-Rhombophryne_longicrus",
                         "AMPHIBIANS-Thorius_magnipes","AMPHIBIANS-Thorius_omiltemi",
                         "MAMMALS-Acerodon_humilis" ,"MAMMALS-Acerodon_jubatus",
                         "MAMMALS-Aeromys_thomasi","MAMMALS-Aethalops_alecto")
    pathOut <- '../Data/Raw/ForestVertebrateRasters'
    tropFor <- read_sf('../Data/Raw/TropicalForests_GADM_EPSG54009.shp')
    
    rasterise_shapes(speciesFiles, tropFor, pathOut)
    # for the files below, used terra package to rasterise instead
    # [1] "./ForestVertebrateShapefiles/BIRDS-Mitrephanes_phaeocercus.gpkg"   
    # [2] "./ForestVertebrateShapefiles/BIRDS-Otus_thilohoffmanni.gpkg"       
    # [3] "./ForestVertebrateShapefiles/MAMMALS-Crocidura_malayana.gpkg"      
    # [4] "./ForestVertebrateShapefiles/MAMMALS-Crocidura_monticola.gpkg"     
    # [5] "./ForestVertebrateShapefiles/MAMMALS-Cynopterus_nusatenggara.gpkg" 
    # [6] "./ForestVertebrateShapefiles/MAMMALS-Myotis_indochinensis.gpkg"    
    # [7] "./ForestVertebrateShapefiles/MAMMALS-Niviventer_cremoriventer.gpkg"
    # [8] "./ForestVertebrateShapefiles/MAMMALS-Nyctimene_cephalotes.gpkg"    
    # [9] "./ForestVertebrateShapefiles/MAMMALS-Petaurista_elegans.gpkg"      
    # [10] "./ForestVertebrateShapefiles/MAMMALS-Pteropus_admiralitatum.gpkg"  
    # [11] "./ForestVertebrateShapefiles/MAMMALS-Pteropus_griseus.gpkg"        
    # [12] "./ForestVertebrateShapefiles/MAMMALS-Pteropus_mariannus.gpkg"      
    # [13] "./ForestVertebrateShapefiles/MAMMALS-Sundasciurus_tenuis.gpkg"
  # ---- clip to forest 2018 extent and altitude -----
  # # 18 May: tested the function on remote desktop, but the code was not producing 
    # any output in the output folder. unsure why. to check when I get back to this bit.
    # to speed up processing, decided to trim the excess 0s that were introduced in the
    # previous function. and use elevation and forest data in 54009 crs instead of 4326
    
    # check out: https://github.com/annakrystalli/IUCNextractR
    # her suggestion is to stack all the species distribution rasters
    # mask elevation with forest and use extract
    
    # 10 Aug: decided that her suggestion wont work because I want to clip each
    # species raster to a different elevation range not the same.
    # tried using parallel with terra, terra doesnt work in parallel. 
    # terra in for loop but error with mask extent, modify to skip if error
    
    # 12 Aug: can't do in a single job cos hit memory limit. 
    # split into task arrays. see Get_AOH.R
    
    #### some prep work ####
    ## ---- failed option (changing everything to 54009) ----
    ## 1. make forest layer binary first
    forest2018 <- raster('../Data/Raw/ForestedPixels2018_Thresh25_TropForResampled.tif')
    # this shows for each 1km cell, how many 30m pixels were still forested at 25% threshold in 2018
    freq(forest2018, value=1024) # 1024 means the entire 1km cell is still forested
    # 6770378 cells.
    # let's take a cut of value of 50%. so if 50% of a 1km cell is forested it's still suitable
    # reclassify to a binary forest 2018 layer. 
    forest2018 <- reclassify(forest2018, c(0,512,0,512,1024,1), include.lowest=TRUE)
    freq(forest18)
      # value     count
      # [1,]     0 168338155
      # [2,]     1  25932245
      # [3,]    NA 163987200
    # save file
    writeRaster(forest2018, '../Data/Raw/ForestedPixels2018_Thresh25_TropForResampled_50PercBinary.tif')
    
    ## 2. reproject to 54009
    forest2 <- project(forestRaster, r, method='near')
    writeRaster(forest2, './ForestedPixels2018_Thresh25_TropForResampled_50PercBinary_54009.tif')
    
    ## 3. check altitude range for species for NAs
    speciesList <- read.csv('../Data/Raw/FinalForestSpeciesList.csv') # 11872
    speciesList %>% group_by(Class) %>% summarise(n=n())
      # Class        n
      # 1 AMPHIBIA  1472
      # 2 AVES      6459
      # 3 MAMMALIA  1759
      # 4 REPTILIA  2182
    
    # Ficetola added 300m buffer to elevational range for amphibians
    # https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-several-columns-on-a-subset-of-rows
    mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
      condition <- eval(substitute(condition), .data, envir)
      .data[condition, ] <- .data[condition, ] %>% mutate(...)
      .data
    }
    speciesList[which(speciesList$Class=='AMPHIBIA'),"Elev_lower"][1:40]
    speciesList[which(speciesList$Class=='AMPHIBIA'),"Elev_upper"][1:40]
    SL2 <- speciesList %>% 
      mutate_cond(Class=='AMPHIBIA', Elev_lower=Elev_lower-300) %>% 
      mutate_cond(Class=='AMPHIBIA', Elev_upper=Elev_upper+300)
    SL2[which(SL2$Class=='AMPHIBIA'),"Elev_lower"][1:40]
    SL2[which(SL2$Class=='AMPHIBIA'),"Elev_upper"][1:40]
    
    
    # if elev_lower=NA and upper_lower=NA, then maybe there isn't any? 
    # either assume no elevational limits. "take min and max value of species range
    missingData <- speciesList %>% 
      filter(is.na(Elev_lower) | is.na(Elev_upper)) # 5881
    missingData %>% group_by(Class) %>% summarise(n=n())
    # Class        n
    # 1 AMPHIBIA   280
    # 2 AVES      4037
    # 3 MAMMALIA   742
    # 4 REPTILIA   822
    
   # ok I'm happy with this.
    write_csv(SL2, '../Data/Raw/FinalForestSpeciesList_BufferedElevRange.csv')
    
    ## 4. reproject elev to 54009
    elev <- rast('./elevation_TropForResampled.tif')
    elev2 <- project(elevationRaster, r, method='bilinear')
    writeRaster(elev2, './elevation_TropForResampled_54009.tif')
    
    ## 5. clip and mask elevation to forest 2018 
    elev <- rast('./elevation_TropForResampled_54009.tif')
    # class       : SpatRaster 
    # dimensions  : 8344, 35563, 1  (nrow, ncol, nlyr)
    # resolution  : 1000, 1000  (x, y)
    # extent      : -17687729, 17875271, -4122605, 4221395  (xmin, xmax, ymin, ymax)
    # coord. ref. : +proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 
    # source      : elevation_TropForResampled_54009.tif 
    # name        : elevation_TropForResampled 
    # min value   :                       -412 
    # max value   :                   8428.634
    forest18 <- rast('./ForestedPixels2018_Thresh25_TropForResampled_50PercBinary_54009.tif')
    freq(elev, value=NA) # 5985579
    freq(forest18, value=NA) # 137331702
    freq(forest18, value=0) # 137776134
    elev2 <- mask(elev, forest18)
    freq(elev2, value=NA) # 137331702
    elev3 <- mask(elev2, forest18, maskvalues=0)
    freq(elev3, value=NA) # 275107836
    # class       : SpatRaster 
    # dimensions  : 8344, 35563, 1  (nrow, ncol, nlyr)
    # resolution  : 1000, 1000  (x, y)
    # extent      : -17687729, 17875271, -4122605, 4221395  (xmin, xmax, ymin, ymax)
    # coord. ref. : +proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 
    # source      : memory 
    # name        : elevation_TropForResampled 
    # min value   :                  -5.515306 
    # max value   :                   5161.833 
    writeRaster(elev3, './elevation_TropForResampled_54009_maskedForest2018.tif')
    
    ## ---- option in use: keeping everything in 4326 ----
    ## 1. make forest layer binary first
    forest2018 <- raster('../Data/Raw/ForestedPixels2018_Thresh25_TropForResampled.tif')
    # this shows for each 1km cell, how many 30m pixels were still forested at 25% threshold in 2018
    freq(forest2018, value=1024) # 1024 means the entire 1km cell is still forested
    # 6770378 cells.
    # let's take a cut of value of 50%. so if 50% of a 1km cell is forested it's still suitable
    # reclassify to a binary forest 2018 layer. 
    forest2018 <- reclassify(forest2018, c(0,512,0,512,1024,1), include.lowest=TRUE)
    freq(forest18)
    # value     count
    # [1,]     0 168338155
    # [2,]     1  25932245
    # [3,]    NA 163987200
    # save file
    writeRaster(forest2018, '../Data/Raw/ForestedPixels2018_Thresh25_TropForResampled_50PercBinary.tif')
    
    ## 2. check altitude range for species for NAs
    speciesList <- read.csv('../Data/Raw/FinalForestSpeciesList.csv') # 11872
    speciesList %>% group_by(Class) %>% summarise(n=n())
    # Class        n
    # 1 AMPHIBIA  1472
    # 2 AVES      6459
    # 3 MAMMALIA  1759
    # 4 REPTILIA  2182
    
    # Ficetola added 300m buffer to elevational range for amphibians
    # https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-several-columns-on-a-subset-of-rows
    mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
      condition <- eval(substitute(condition), .data, envir)
      .data[condition, ] <- .data[condition, ] %>% mutate(...)
      .data
    }
    speciesList[which(speciesList$Class=='AMPHIBIA'),"Elev_lower"][1:40]
    speciesList[which(speciesList$Class=='AMPHIBIA'),"Elev_upper"][1:40]
    SL2 <- speciesList %>% 
      mutate_cond(Class=='AMPHIBIA', Elev_lower=Elev_lower-300) %>% 
      mutate_cond(Class=='AMPHIBIA', Elev_upper=Elev_upper+300)
    SL2[which(SL2$Class=='AMPHIBIA'),"Elev_lower"][1:40]
    SL2[which(SL2$Class=='AMPHIBIA'),"Elev_upper"][1:40]
    
    
    # if elev_lower=NA and upper_lower=NA, then maybe there isn't any? 
    # either assume no elevational limits. "take min and max value of species range
    missingData <- speciesList %>% 
      filter(is.na(Elev_lower) | is.na(Elev_upper)) # 5881
    missingData %>% group_by(Class) %>% summarise(n=n())
    # Class        n
    # 1 AMPHIBIA   280
    # 2 AVES      4037
    # 3 MAMMALIA   742
    # 4 REPTILIA   822
    
    # ok I'm happy with this.
    write_csv(SL2, '../Data/Raw/FinalForestSpeciesList_BufferedElevRange.csv')
    
    ## 3. clip and mask elevation to forest 2018 
    elev <- rast('../Data/Raw/elevation_TropForResampled.tif')
    # class       : SpatRaster 
    # dimensions  : 8293, 43200, 1  (nrow, ncol, nlyr)
    # resolution  : 0.008333333, 0.008333333  (x, y)
    # extent      : -179.9994, 180.0006, -34.12427, 34.98406  (xmin, xmax, ymin, ymax)
    # coord. ref. : +proj=longlat +datum=WGS84 +no_defs 
    # source      : elevation_TropForResampled.tif 
    # name        : elevation_TropForResampled 
    # min value   :                       -412 
    # max value   :                   8591.077 
    # 
    forest18 <- rast('../Data/Raw/ForestedPixels2018_Thresh25_TropForResampled_50PercBinary.tif')
    freq(elev, value=NA) # 0
    freq(forest18, value=NA) # 163987200
    freq(forest18, value=0) # 168338155
    elev2 <- mask(elev, forest18)
    freq(elev2, value=NA) # 163987200
    elev3 <- mask(elev2, forest18, maskvalues=0)
    freq(elev3, value=NA) # 275107836
    # class       : SpatRaster 
    # dimensions  : 8293, 43200, 1  (nrow, ncol, nlyr)
    # resolution  : 0.008333333, 0.008333333  (x, y)
    # extent      : -179.9994, 180.0006, -34.12427, 34.98406  (xmin, xmax, ymin, ymax)
    # coord. ref. : +proj=longlat +datum=WGS84 +no_defs 
    # source      : spat_1iR5e9KwiOKUDOu.tif 
    # name        : elevation_TropForResampled 
    # min value   :                  -12.51274 
    # max value   :                   5385.128 
    writeRaster(elev3, '../Data/Raw/elevation_TropForResampled_maskedForest2018.tif')
    
    ext(elev3)
    # SpatExtent : -179.999419637, 180.000565963, -34.124274274, 34.984056295 (xmin, xmax, ymin, ymax)
    origin(elev3)
    # 0.000573163 0.000724361
    res(elev3)
    # 0.008333333 0.008333333
    # ---- parallelised function (doesnt work with terra) -----
    # terra package cannot run in parallel; see https://stackoverflow.com/questions/67445883/terra-package-returns-error-when-try-to-run-parallel-operations
    
    get_aoh <- function(rasterList, elevationRaster, speciesData){
        pathOut = paste0(dirname(rasterList[1]), '-AOH') 
        # filter out species files that have already been rasterised. cant figure out a better way to do this
        alrPresent <- unname(sapply(list.files(pathOut, pattern='\\.tif'), function(x) strsplit(basename(x), '\\.')[[1]][1]))
        allFiles <- unname(sapply(rasterList, function(x) strsplit(basename(x), '\\.')[[1]][1]))
        toDo <- unname(sapply(setdiff(allFiles, alrPresent), function(x) paste0(dirname(rasterList[1]), '/', x, '.tif')))
        # number of species
        n <- length(toDo)
        # parallel processing 
        noCores <- ceiling(0.5*parallel::detectCores())
        
        # if it's running on Mac
        if (.Platform$OS.type == "unix"){
          ### using FORK method
          parallel::mclapply(1:n, function(n){
            # read in raster
            r <- rast(toDo[n])
            r <- terra::trim(r, value=0) # cut out padding to speed up processing.
            # get the species name 
            spName <- gsub("_", " ", gsub("^[^-]*-([^.]+).*", "\\1", basename(toDo[n])))
            tryCatch({
              # mask species distribution to forest 2018 extent (which was done for elevation)
              elevMask <- elevationRaster
              elevMask <- crop(elevMask, r)
              r <- terra::mask(r, mask=elevMask, maskvalues=NA, updatevalue=0)
              # clip to altitude limits
              elevLower <- speciesData[which(speciesData$Species==spName),'Elev_lower']
              elevUpper <- speciesData[which(speciesData$Species==spName),'Elev_upper']
              # if there is lower and upper bounds
              if(!is.na(elevLower) & !is.na(elevUpper)){
                elevMask[elevMask<elevLower] <- NA
                elevMask[elevMask>elevUpper] <- NA
                r <- terra::mask(r, mask=elevMask, maskvalues=NA, updatevalue=0)
              } else if(is.na(elevLower) & !is.na(elevUpper)){
                # if there is upper bound
                elevMask[elevMask>elevUpper] <- NA
                r <- terra::mask(r, mask=elevMask, maskvalues=NA, updatevalue=0)
              } else if(!is.na(elevLower) & is.na(elevUpper)){
                # if there is lower bound
                elevMask[elevMask<elevLower] <- NA
                r <- terra::mask(r, mask=elevMask, maskvalues=NA, updatevalue=0)
              } # if lower and upper bound are NA, then leave it
              # change NAs to 0s
              r[is.na(r)] <- 0
              # write out the raster for the species
              terra::writeRaster(r, filename=paste0(pathOut, '/', basename(toDo[n])))
              # clear temporary files 
              terra::tmpFiles(remove=TRUE)
            },
            error=function(cond){
              cat('Error at ', spName, conditionMessage(cond), '\n')})
          }, mc.cores=noCores)
        } else {
          ### using PSOCK method
          # Initiate cluster
          cl <- parallel::makeCluster(noCores)
          parallel::clusterEvalQ(cl, sapply(c("terra"), require, char=TRUE))
          #  variables need to be shared across clusters
          parallel::clusterExport(cl, list("toDo", "elevationRaster", "speciesData", "n", "pathOut"), envir=environment())
          # start parallel processing to convert species distribution to a raster w
          speciesRasters <- parallel::parLapply(cl, 1:n, function(n){
            # read in raster
            r <- rast(toDo[n])
            # cut out padding to speed up processing.
            r <- terra::trim(r, value=0) 
            # get the species name 
            spName <- gsub("_", " ", gsub("^[^-]*-([^.]+).*", "\\1", basename(toDo[n])))
            # mask species distribution to forest 2018 extent (which was done for elevation)
            elevMask <- elevationRaster
            elevMask <- crop(elevMask, r)
            r <- terra::mask(r, mask=elevMask, maskvalues=NA, updatevalue=0)
            # clip to altitude limits
            elevLower <- speciesData[which(speciesData$Species==spName),'Elev_lower']
            elevUpper <- speciesData[which(speciesData$Species==spName),'Elev_upper']
            # if there is lower and upper bounds
            if(!is.na(elevLower) & !is.na(elevUpper)){
              elevMask[elevMask<elevLower] <- NA
              elevMask[elevMask>elevUpper] <- NA
              r <- terra::mask(r, mask=elevMask, maskvalues=NA, updatevalue=0)
            } else if(is.na(elevLower) & !is.na(elevUpper)){
              # if there is upper bound
              elevMask[elevMask>elevUpper] <- NA
              r <- terra::mask(r, mask=elevMask, maskvalues=NA, updatevalue=0)
            } else if(!is.na(elevLower) & is.na(elevUpper)){
              # if there is lower bound
              elevMask[elevMask<elevLower] <- NA
              r <- terra::mask(r, mask=elevMask, maskvalues=NA, updatevalue=0)
            } # if lower and upper bound are NA, then leave it
            # change NAs to 0s
            r[is.na(r)] <- 0
            # write out the raster for the species
            terra::writeRaster(r, filename=paste0(pathOut, '/', basename(toDo[n])))})
          # clear temporary files 
          terra::tmpFiles(remove=TRUE)
          # Close the cluster
          parallel::stopCluster(cl)
        }
      }
      
     
    # -------- crop_aoh function  ----
    crop_aoh <- function(rasterFile){
    # read in raster
    r <- rast(rasterFile)
    # cut out padding to speed up processing.
    r <- trim(r, value=0) 
    # get the species name 
    spName <- gsub("_", " ", gsub("^[^-]*-([^.]+).*", "\\1", basename(rasterFile)))
    # mask species distribution to forest 2018 extent (which was done for elevation)
    elevMask <- elevationRaster
    elevMask <- crop(elevMask, r)
    elevMask <- resample(elevMask, r, method='near')
    r <- mask(r, mask=elevMask, maskvalues=NA, updatevalue=0)
    # clip to altitude limits
    elevLower <- speciesData[which(speciesData$Species==spName),'Elev_lower']
    elevUpper <- speciesData[which(speciesData$Species==spName),'Elev_upper']
    # if there is lower and upper bounds
    if(!is.na(elevLower) & !is.na(elevUpper)){
      elevMask[elevMask<elevLower] <- NA
      elevMask[elevMask>elevUpper] <- NA
      r <- mask(r, mask=elevMask, maskvalues=NA, updatevalue=0)
    } else if(is.na(elevLower) & !is.na(elevUpper)){
      # if there is upper bound
      elevMask[elevMask>elevUpper] <- NA
      r <- mask(r, mask=elevMask, maskvalues=NA, updatevalue=0)
    } else if(!is.na(elevLower) & is.na(elevUpper)){
      # if there is lower bound
      elevMask[elevMask<elevLower] <- NA
      r <- mask(r, mask=elevMask, maskvalues=NA, updatevalue=0)
    } # if lower and upper bound are NA, then leave it
    # change NAs to 0s
    r[is.na(r)] <- 0
    # write out the raster for the species
    writeRaster(r, filename=paste0(pathOut, '/', basename(rasterFile)))
    # clear temporary files 
    terra::tmpFiles(current=TRUE, old=TRUE, orphan=TRUE, remove=TRUE)
    }
    ## ==== using HPC to crop AOH ====
    ## split the species files to several text files for task array
    setwd('/shared/edwards_lab1/User/bop19jss/Ch3-biodiversity')
    ## get a list of text files that specify which rasters to do
    rasterList <- list.files('./ForestVertebrateRasters', '\\.tif', full.names=TRUE) # 11872 files
    pathOut = paste0(dirname(rasterList[1]), '-AOH') 
    # filter out species files that have already been rasterised. cant figure out a better way to do this
    alrPresent <- unname(sapply(list.files(pathOut, pattern='\\.tif'), function(x) strsplit(basename(x), '\\.')[[1]][1]))
    allFiles <- unname(sapply(rasterList, function(x) strsplit(basename(x), '\\.')[[1]][1]))
    toDo <- unname(sapply(setdiff(allFiles, alrPresent), function(x) paste0(dirname(rasterList[1]), '/', x, '.tif')))
    
    # write list of files to do to separate text files 
    # first time with 260 files, then with ~20, finally with about 5
    d1 <- split(toDo, ceiling(seq_along(toDo)/5)) # split into 260 files each first time round
    outpath <- lapply(seq_along(d1), function(x) paste0("./rasterList",x,".txt"))
    lapply(seq_along(d1), function(x) writeLines(d1[[x]], outpath[[x]]))
    
    # or write list for last few files to run
    writeLines(toDo, "./rasterList.txt")
    
    ## set it to run as a task array job
    # this is the code in Get_AOH.R
    setwd('/shared/edwards_lab1/User/bop19jss/Ch3-biodiversity')
    
    elevationRaster <- rast('./elevation_TropForResampled_54009_maskedForest2018.tif')
    speciesData <- read.csv('./FinalForestSpeciesList_BufferedElevRange.csv')
    rasterList <- readLines(args[1])
    pathOut = paste0(dirname(rasterList[1]), '-AOH') 
    
    # filter out species files that have already been rasterised. 
    alrPresent <- unname(sapply(list.files(pathOut, pattern='\\.tif'), function(x) strsplit(basename(x), '\\.')[[1]][1]))
    allFiles <- unname(sapply(rasterList, function(x) strsplit(basename(x), '\\.')[[1]][1]))
    toDo <- unname(sapply(setdiff(allFiles, alrPresent), function(x) paste0(dirname(rasterList[1]), '/', x, '.tif')))
    
    cat('processing', length(toDo), 'files in this task', sep=" ", '\n')
    
    for (i in 1:length(toDo)){
      cat(i, toDo[i], sep=" ", '\n')
      # tryCatch
      tryCatch({
        crop_aoh(toDo[i])
      }, error=function(e){cat("ERROR: ", conditionMessage(e), "\n")})
      terra::tmpFiles(current=TRUE, orphan=TRUE, old=TRUE, remove=TRUE)
    }
    
    cat('finished', '\n')
    
    # see output GetAOH_out.txt
    # mask extents don't match error --> use resample after cropping elevation mask to species raster
    # trim value only 0 values error --> when rasterising from shapefile, should use mosaic fun=max not merge
    
    # for some reason, AMPHIBIANS-Acanthixalus_sonjae.tif and AMPHIBIANS-Acanthixalus_spinosus.tif are in longlat
  # ---- realised need to do 2010 and 2018 forest cover ----
    ## also realised need to project to 4326 from shapefile not raster 
    ## wrote function to rasterise from shapefile and crop to AOH together
    ## 19 Sep 2021. sometime before, was doing it indv with task array. 
    ## decided to run each task for 50 species. 151 index files for 7505 species
    ## also tried looking at Simon's stars script. 6 Oct 2021?
    ## 28 Apr 2022. finally coming back to this. dont think I had submitted the 151 task array
    ## Forest2010-AOH had 6026 files, Forest2018-AOH had 4367 files
    ## 2 May 2022. finally coming back to this. using terra because 
    ## stars is really complicated and isnt necessarily faster.
    ## 3 May 2022. 6099 files in 2010, 4397 files in 2018
    ## 4 May 2022. the task array job is still in queue... trying it
    ## manually now. 11.02am start with AMPHIBIANS-Adelophryne patamona
    ## error cos file already exists (typo in function) & read in second file and exceeded mem
    ## 5 May 2022. tried to rasterise all first (forgot elevation limits
    ## different for each species so wouldnt really work anyway)
    ## decided to go back to doing each species indv in task array
    ## set 1000 to run, finished on 7 may!
    ## 9 may. 2010 folder = 6109 files; 2018 folder = 5385
    ## 9 May. set 1001 to 7000 to run. (should be 7475 files)
    ## 10 May. finished both task arrays! 2010 folder = 11835 files; 2018 folder = 11625 files
    ## 10 May. set the remaining 247 to run again with 120GB
    ## 12 May. finished. 2010 folder = 11865 files; 2018 folder = 11865 files. 
    ## 13 may. set remaining 7 to run with 200GB
      # [1] "MAMMALS-Arctictis_binturong"     "MAMMALS-Crocidura_monticola"    
      # [3] "MAMMALS-Cynopterus_nusatenggara" "MAMMALS-Pteropus_admiralitatum" 
      # [5] "MAMMALS-Pteropus_griseus"        "MAMMALS-Pteropus_mariannus"     
      # [7] "REPTILES-Scincella_boettgeri"   
    ## 13 may. ahhh left just !ONE! un-done, Pteropus admiralitatum... gotta try 240GB?? 
    
    # -------- rasterise_AOH function --------
    rasterise_AOH <- function(species){
      # read in shapefile
      speciesFile <- vect(paste0('./ForestVertebrateShapefiles/', species, '.gpkg'))
      
      # re-project shapefile from ESRI 54009 to EPSG 4326
      shp4326 <- project(speciesFile, "epsg:4326")
      
      # rasterise shapefile
      if(nrow(shp4326) > 1){
        # if there are more than one polygon for the species
        r_poly <- lapply(1:nrow(shp4326), function(x){terra::rasterize(shp4326[x,], tmp10, touches=TRUE, background=0)})
        # using touches=TRUE so even if polygon is small such that it doesn't cross centre of cell
        # the cell will still be considered as present   
        r_poly <- terra::mosaic(src(r_poly), fun="max")
      } else if(nrow(shp4326) == 1){ 
        # if there is only one polygon
        r_poly<- terra::rasterize(shp4326, tmp10, touches=TRUE, background=0)
      }
      
      # get the species name to search in database
      spName <- gsub("_", " ", gsub("^[^-]*-([^.]+).*", "\\1", species))
      
      # species elevational limits
      elevLower <- speciesData[which(speciesData$Species==spName),'Elev_lower']
      elevUpper <- speciesData[which(speciesData$Species==spName),'Elev_upper']
      
      # clip to altitude limits for 2010
      if(!file.exists(paste0("./Forest2010-AOH/", species, ".tif"))){
        cat("clipping to 2010 AOH", "\n")
        elevMask <- tmp10
        aoh_2010 <- r_poly
        if(!is.na(elevLower) & !is.na(elevUpper)){
          # if there is lower and upper bounds
          elevMask[elevMask<elevLower] <- NA
          elevMask[elevMask>elevUpper] <- NA
          aoh_2010 <- mask(aoh_2010, mask=elevMask, maskvalues=NA, updatevalue=0)
        } else if(is.na(elevLower) & !is.na(elevUpper)){
          # if there is upper bound
          elevMask[elevMask>elevUpper] <- NA
          aoh_2010 <- mask(aoh_2010, mask=elevMask, maskvalues=NA, updatevalue=0)
        } else if(!is.na(elevLower) & is.na(elevUpper)){
          # if there is lower bound
          elevMask[elevMask<elevLower] <- NA
          aoh_2010 <- mask(aoh_2010, mask=elevMask, maskvalues=NA, updatevalue=0)
        } # if lower and upper bound are NA, then leave it
        # change NAs to 0s so I can sum up for species richness
        aoh_2010[is.na(aoh_2010)] <- 0
        # write out the raster for the species
        writeRaster(aoh_2010, filename=paste0('./Forest2010-AOH/', species, '.tif'))
      }
      
      # clip to altitude limits for 2018
      cat("clipping to 2018 AOH", "\n")
      elevMask <- tmp18
      aoh_2018 <- r_poly
      # if there is lower and upper bounds
      if(!is.na(elevLower) & !is.na(elevUpper)){
        elevMask[elevMask<elevLower] <- NA
        elevMask[elevMask>elevUpper] <- NA
        aoh_2018 <- mask(aoh_2018, mask=elevMask, maskvalues=NA, updatevalue=0)
      } else if(is.na(elevLower) & !is.na(elevUpper)){
        # if there is upper bound
        elevMask[elevMask>elevUpper] <- NA
        aoh_2018 <- mask(aoh_2018, mask=elevMask, maskvalues=NA, updatevalue=0)
      } else if(!is.na(elevLower) & is.na(elevUpper)){
        # if there is lower bound
        elevMask[elevMask<elevLower] <- NA
        aoh_2018 <- mask(aoh_2018, mask=elevMask, maskvalues=NA, updatevalue=0)
      } # if lower and upper bound are NA, then leave it
      # change NAs to 0s so I can sum up for species richness
      aoh_2018[is.na(aoh_2018)] <- 0
      # write out the raster for the species
      writeRaster(aoh_2018, filename=paste0('./Forest2018-AOH/', species, '.tif'))
    } 
    
    ## ==== using HPC to rasterise AOH ====
    ### split files up
    # This was done on the HPC 
    setwd('/shared/edwards_lab1/User/bop19jss/Ch3-biodiversity')
    
    ## get a list of text files that specify which rasters to do
    rasterList <- list.files('./ForestVertebrateShapefiles', '\\.gpkg',
                             full.names=TRUE) # 11872 files
    # filter out species files that have already been rasterised (2 May 2022)
    in2018 <- unname(sapply(list.files("./Forest2018-AOH", pattern='\\.tif'), function(x) strsplit(basename(x), '\\.')[[1]][1]))
    allFiles <- unname(sapply(rasterList, function(x) strsplit(basename(x), '\\.')[[1]][1]))
    toDo <- setdiff(allFiles, in2018) 
    # write list of files to do as Index.txt
    writeLines(toDo, "./RasterList_2022-05-13.txt")
    
    ### R script to run (Rasterise_getAOH.R) rmem=60GB then 120GB then 
    library(terra)
    
    setwd('/shared/edwards_lab1/User/bop19jss/Ch3-biodiversity')
    
    # get the index of species to do
    args <- commandArgs(trailingOnly = TRUE)
    # rasterList <- readLines("./RasterList_2022-05-04.txt")
    # easier to use RasterList as Index file in bash script
    # so task array specifies reading in one line from RasterList
    # as input argument
    
    # getting the one species from the index file
    species <- args[1]
    
    # elevation raster where cells not covered by forest in 2010 and 2018 have been masked out
    tmp10 <- rast('./elevation_TropForResampled_maskedForest2010.tif')
    tmp18 <- rast('./elevation_TropForResampled_maskedForest2018.tif')
    
    # species data to get elevation range
    speciesData <- read.csv('./FinalForestSpeciesList_BufferedElevRange.csv')
    
    cat(species, "\n")
    
    tryCatch({
      rasterise_AOH(species)}, 
      error=function(e){cat("ERROR: ", conditionMessage(e), "\n")})
    terra::tmpFiles(current=TRUE, orphan=TRUE, old=TRUE, remove=TRUE)
    
    cat("finished", "\n")
    
    ## ---- failed option in sets of 50 ----
    ### split files up
    # This was done on the HPC using task array where the list of 11872 shapefiles 
    # that needed to be processed were split up
    setwd('/shared/edwards_lab1/User/bop19jss/Ch3-biodiversity')
    
    ## get a list of text files that specify which rasters to do
    rasterList <- list.files('./ForestVertebrateShapefiles', '\\.gpkg',
                             full.names=TRUE) # 11872 files
    # filter out species files that have already been rasterised (2 May 2022)
    in2018 <- unname(sapply(list.files("./Forest2018-AOH", pattern='\\.tif'), function(x) strsplit(basename(x), '\\.')[[1]][1]))
    # 2 May. 2018 folder has 4367 files. 2010 folder has 6026 files. 
    # 3 May. 2018 has 4397 files, 2010 has 6099 files
    allFiles <- unname(sapply(rasterList, function(x) strsplit(basename(x), '\\.')[[1]][1]))
    toDo <- setdiff(allFiles, in2018) # left 7505 to do. 7475 
    # write list of files to do to separate text files w about 50 files each
    d1 <- split(toDo, ceiling(seq_along(toDo)/50)) # length 151
    outpath <- lapply(seq_along(d1), function(x) paste0("./rasterList",x,".txt"))
    lapply(seq_along(d1), function(x) writeLines(d1[[x]], outpath[[x]]))
    
    ### then submit the R script (see Rasterise_getAOH.R) with task array 
    library(terra)
    
    setwd('/shared/edwards_lab1/User/bop19jss/Ch3-biodiversity')
    
    # get the list of species
    args <- commandArgs(trailingOnly = TRUE)
    speciesList <- readLines(args[1])
    
    # elevation raster where cells not covered by forest in 2018 have been masked out
    tmp10 <- rast('./elevation_TropForResampled_maskedForest2010.tif')
    tmp18 <- rast('./elevation_TropForResampled_maskedForest2018.tif')
    
    # species data to get elevation range
    speciesData <- read.csv('./FinalForestSpeciesList_BufferedElevRange.csv')
    
    cat('processing', length(speciesList), 'files in this task', sep=" ", '\n')
    
    # for loop to do the 50 species
    for (i in 1:length(speciesList)){
      cat(i, speciesList[i], sep=" ", '\n')
      # tryCatch
      tryCatch({
        rasterise_AOH(speciesList[i])
      }, error=function(e){cat("ERROR: ", conditionMessage(e), "\n")})
      terra::tmpFiles(current=TRUE, orphan=TRUE, old=TRUE, remove=TRUE)
    }
    cat('finished processing', '\n')
    
    ## ---- failed option rasterising all ----
    # on the HPC interactive mode. started at 1230pm
    library(sf)
    library(stars)
    
    setwd('/shared/edwards_lab1/User/bop19jss/Ch3-biodiversity')
    
    tmp10 <- read_stars('./elevation_TropForResampled_maskedForest2010.tif')
    template = st_as_stars(st_bbox(tmp10), nx=43200, ny=8293)
    
    allSpecies <- read_sf("./SpeciesDistributions/TropicalForestVertebrates_EPSG4326.gpkg")
    rasts <- rep(NA, nrow(allSpecies))
    names(rasts) <- allSpecies$binomial
    
    for(i in 1:length(rasts)) {
      print(i)
      rast_temp <- st_rasterize(allSpecies[i,], template, options=c("ALL_TOUCHED=T"))
      rasts[[i]] <- rast_temp
      # stopped at 595 at 3.10pm cos lacking free disk space
    }
    # this isnt gonna work!
    save.image("./TropicalForestVertebrates_rasters.RData")
    r1to594 <- do.call(c, rasts[1:594])
    write_stars(r1to594, "./TropicalForestVertebrates_r1to594.tif")
    
##### obtain sp richness rasters ====
    # collapse all raster layers to output one raster layer of species richness (not done)
    library(terra)
    
    setwd('/shared/edwards_lab1/User/bop19jss/Ch3-biodiversity')
    
    # for 2010 
    aohFiles <- list.files('./Forest2010-AOH', pattern='\\.tif$', full.names=TRUE) # should be 11,870 species
    rr <- lapply(aohFiles, rast)
    speciesStack <- rast(rr)
    finalRaster <- calc(speciesStack, sum)
    writeRaster(finalRaster, './ForestVertebrates2010_SpRichness.tif')
    
    # for 2018
    aohFiles <- list.files('./Forest2018-AOH', pattern='\\.tif$', full.names=TRUE) # should be 11,870 species
    rr <- lapply(aohFiles, rast)
    speciesStack <- rast(rr)
    finalRaster <- calc(speciesStack, sum)
    writeRaster(finalRaster, './ForestVertebrates2018_SpRichness.tif')
    
    # or maybe not?
    rr$fun <- sum
    m <- do.call(raster::mosaic, rr)
    
    rsrc <- src(rr) # only works with terra 1.3.4 and above. 
    m <- mosaic(rsrc, fun='sum')
    
    
    ## for range-rarity species richness
    ## Range-size rarity is calculated from the area of the pixel 
    ## divided by the area of the range for each species, i.e. the 
    ## proportion of the species' range contained within the given pixel. 
    ## These values are summed across all species to show the aggregate 
    ## importance of each pixel to the species occuring there.
    ## 

    
    