#### ========= Filter species distribution shapefiles ========== ####
# Note: done on local computer, using R version 4.0.5
# Filter species distribution range files for tropical species
pacman::p_load(tidyverse, sf, raster)
## tidyverse v1.3, sf v0.9-8, raster v3.5.2, 
## GEOS v3.8.1, GDAL v3.1.4, proj4 v6.3.1

  
  # ---- for mammals, amphibians and reptiles ----
    ####### FUNCTION to filter spatial data ######
    # for trycatch: https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
    filter_sp <- function(iucn_shapefile, fn){
      out <- tryCatch(
        { # try to execute this bit (R code to be evaluated)
          shp <- read_sf(iucn_shapefile)
          cat(basename(iucn_shapefile), '\n')
          cat('no. of obs ', dim(shp)[1], '\n')
          cat('no. of sp. ', length(unique(shp$binomial)), '\n')
          
          # filter sp to relevant ones only
          cat('filtering for terrestrial, (probably) extant, native/reintroduced sp', '\n')
          shp <- shp %>% 
            dplyr::select(binomial, presence, origin, seasonal, legend, class, category, terrestial) %>% 
            filter(terrestial=='true',
                   presence %in% c(1,2,3), # Tracewski uses distribution polygons for extant (1) and probably extant (2). BOTW says probably extant (2) is discontinued.
                   origin %in% c(1,2)) 
          cat('no of filtered obs ', dim(shp)[1], '\n')
          cat('no of filtered sp. ', length(unique(shp$binomial)), '\n')
          
          # reproject to EPSG54009
          cat('reproject to EPSG54009', '\n')
          shp <- st_transform(shp, crs="ESRI:54009")
          cat('buffer shapefile', '\n')
          shp <- st_buffer(shp, 0)
          
          # extract species for relevant extent
          if (fn == 'intersects'){
            cat('keep species that intersect tropical extents', '\n')
            trop <- st_intersects(shp, tropFor) 
          } else if (fn == 'within'){
            cat('keep only species that completely fall within tropical extents', '\n')
            trop <- st_within(shp, tropFor) 
          }
          # which distributions fall inside tropical polygons
          trop_logical <- lengths(trop) > 0
          shp <- shp[trop_logical,] 
          cat('no. of tropical obs ', dim(shp)[1], '\n')
          cat('no. of tropical sp. ', length(unique(shp$binomial)), '\n') 
          
          return(shp)},
        error=function(cond){ # optional error catching
          message('Ran into an error')
          message(cond)
          # choose a return value in case of error
          return(shp)},
        warning=function(cond){ # optional warning catching
          message('Warning issued')
          message(cond)
          # choose a return value in case of warning
          return(shp)},
        finally={ # optional what should be done regardless of error or warning
          message(paste('Finished processing ',basename(iucn_shapefile)))
        }
      )
    }
    ####### EXTRACT data #######
    ### read in tropical extent
    tropFor <- read_sf('../Data/Raw/Boundaries/TropicalForests_GADM_EPSG54009.shp')
    
    ### list of species range shapefiles
    listFiles <- c('../Data/Raw/IUCN-SpeciesDistributions/AMPHIBIANS/AMPHIBIANS.shp',
                   '../Data/Raw/IUCN-SpeciesDistributions/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp',
                   '../Data/Raw/IUCN-SpeciesDistributions/REPTILES/REPTILES.shp')
    
    ### create species list for all three classes
    spList <- data.frame(Species=character(), 
                         Class=character(),
                         Category=character(),
                         stringsAsFactors = FALSE)
    
    ### for loop to extract species intersecting tropical forest extent
    for (group in listFiles){
      group_data <- filter_sp(group, 'intersects')
      # save the filtered tropical distribution polygons
      write_sf(group_data, paste0(dirname(group), '/IntersectsTropical_filtered_EPSG54009_', basename(group)))
      # extract tabular data
      spList_group <- data.frame(Species=group_data$binomial, 
                                 Class=group_data$class,
                                 Category=group_data$category, 
                                 stringsAsFactors=FALSE)
      spList <- rbind(spList, spList_group)
    }
      #### FOR ST_INTERSECTS
      # AMPHIBIANS.shp 
      # no. of obs  8598 
      # no. of sp.  7118 
      # filtering for terrestrial, (probably) extant, native/reintroduced sp 
      # no of filtered obs  7916 
      # no of filtered sp.  6888 
      # reproject to EPSG54009 
      # buffer shapefile 
      # keep only species that intersects tropical extents 
      # no. of tropical obs  6803 
      # no. of tropical sp.  5939 
      # Finished processing  AMPHIBIANS.shp
      # MAMMALS_TERRESTRIAL_ONLY.shp 
      # no. of obs  12483 
      # no. of sp.  5593 
      # filtering for terrestrial, (probably) extant, native/reintroduced sp 
      # no of filtered obs  11757 
      # no of filtered sp.  5553 
      # reproject to EPSG54009 
      # buffer shapefile 
      # keep only species that intersects tropical extents 
      # no. of tropical obs  8775 
      # no. of tropical sp.  4284 
      # Finished processing  MAMMALS_TERRESTRIAL_ONLY.shp
      # REPTILES.shp 
      # no. of obs  10890 
      # no. of sp.  7860 
      # filtering for terrestrial, (probably) extant, native/reintroduced sp 
      # no of filtered obs  10367 
      # no of filtered sp.  7715 
      # reproject to EPSG54009 
      # buffer shapefile 
      # keep only species that intersects tropical extents 
      # no. of tropical obs  6599 
      # no. of tropical sp.  5282 
      # Finished processing  REPTILES.shp
      
    ### write output
    # there will be multiple entries for single sp cos of seasonality
    spList <- unique(spList)
    write_csv(spList, '../Data/Raw/IUCN-SpeciesDistributions/SpeciesList_IntersectsTropical.csv')
    
    ### for loop to extract species within tropical forest extent
    for (group in listFiles){
      group_data <- filter_sp(group, 'within')
      # save the filtered tropical distribution polygons
      write_sf(group_data, paste0(dirname(group), '/WithinTropical_filtered_EPSG54009_', basename(group)))
      # extract tabular data
      spList_group <- data.frame(Species=group_data$binomial, 
                                 Class=group_data$class,
                                 Category=group_data$category, 
                                 stringsAsFactors=FALSE)
      spList <- rbind(spList, spList_group)
    }
      #### FOR ST WITHIN 
          # AMPHIBIANS.shp 
            # no. of obs  8598 
            # no. of sp.  7118 
            # reproject to EPSG54009 
            # filtering for terrestrial, (probably) extant, native/reintroduced sp 
            # no of filtered obs  7916 
            # no of filtered sp.  6888 
            # keep only species that completely fall within tropical extents 
            # no. of tropical obs  1488 
            # no. of tropical sp.  1384 (out of 7118)
          # MAMMALS_TERRESTRIAL_ONLY.shp 
            # no. of obs  12483 
            # no. of sp.  5593 
            # reproject to EPSG54009 
            # filtering for terrestrial, (probably) extant, native/reintroduced sp 
            # no of filtered obs  11757 
            # no of filtered sp.  5553 
            # keep only species that completely fall within tropical extents 
            # no. of tropical obs  299 
            # no. of tropical sp.  268 (out of 5593)
          # REPTILES.shp 
            # no. of obs  10890 
            # no. of sp.  7860 
            # reproject to EPSG54009 
            # filtering for terrestrial, (probably) extant, native/reintroduced sp 
            # no of filtered obs  10367 
            # no of filtered sp.  7715 
            # buffer shapefile 
            # keep only species that completely fall within tropical extents
            # no. of tropical obs  664
            # no. of tropical sp.  587 (out of 7860)
            # Finished processing  REPTILES.shp
    ### write output
    # there will be multiple entries for single sp cos of seasonality
    spList <- unique(spList)
    write_csv(spList, '../Data/Raw/IUCN-SpeciesDistributions/SpeciesList_WithinTropical.csv')

  # ---- for birds ----
    ### read in tropical forest extent
    tropFor <- read_sf('../Data/Raw/Boundaries/TropicalForests_GADM_EPSG54009.shp')
    
    ### read in list of forest habitat birds downloaded from BirdLife International Datazone search function
    birdList <- read_csv('../Data/Raw/BirdLifeInternational-BOTW2020/ForestSpecies_20210421_23675.csv')
    
    ### read in birds of the world dataset
    birds <- st_read(dsn='../Data/Raw/BirdLifeInternational-BOTW2020/BOTW.gdb',
                     layer="All_Species") 
    
    ### filter to extant (presence=1,2,3) and native (origin=1,2) for forest birds
    birds <- birds %>% 
      dplyr::select(binomial, presence, origin, seasonal) %>% 
      filter(presence %in% c(1,2,3), 
             origin %in% c(1,2), 
             binomial %in% birdList$`Scientific name`) %>% 
      st_transform(crs='ESRI:54009') %>% 
      st_buffer(0) %>% 
      st_cast('MULTIPOLYGON') %>% 
      st_buffer(0) 
      
    ### filter those that intersects tropical forest extent
      trop <- st_intersects(birds, tropFor) 
      # which distributions fall inside tropical polygons
      trop_logical <- lengths(trop) > 0
      birds <- birds[trop_logical,] 
    ### save outputs
    write_sf(birds, '../Data/Raw/BirdLifeInternational-BOTW2020/IntersectsTropical_filtered_EPSG54009_BIRDS.gpkg')
      
    ### filter those that are within tropical forest extent
    trop <- st_within(birds, tropFor) 
    # which distributions fall inside tropical polygons
    trop_logical <- lengths(trop) > 0
    birds <- birds[trop_logical,] 
    ### save outputs
    write_sf(birds, '../Data/Raw/BirdLifeInternational-BOTW2020/WithinTropical_filtered_EPSG54009_BIRDS.gpkg')
    
    ### join this species list with info from birdList as common name needed
    spList <- as_tibble(birds) %>% 
      dplyr::select(binomial) %>% 
      rename(Species = binomial) %>% 
      distinct() %>% 
      left_join(birdList, by=c('Species'='Scientific name')) %>% 
      rename(CommonName = `English name`) %>% 
      dplyr::select(-`Global IUCN Red List Category`)
    
    write_csv(spList, '../Data/Raw/BirdLifeInternational-BOTW2020/ForestSpeciesList_IntersectsTropical.csv')
    write_csv(spList, '../Data/Raw/BirdLifeInternational-BOTW2020/ForestSpeciesList_WithinTropical.csv')
    
  