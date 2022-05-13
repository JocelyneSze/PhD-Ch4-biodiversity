## Prepare species distribution files for analysis
## Using R version 4.0.5
## sf package v0.9-8, GEOS v3.8.1, GDAL v3.1.4, proj4 v6.3.1
## tidyverse package v1.3, rredlist package v0.7, rvest package v1.0

#### ========= Filter species distribution shapefiles ========== ####
  pacman::p_load(tidyverse, sf, raster)
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
    tropFor <- read_sf('../Data/Raw/TropicalForests_GADM_EPSG54009.shp')
    
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
    tropFor <- read_sf('../Data/Raw/TropicalForests_GADM_EPSG54009.shp')
    
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
      
    ### join this species list with info from birdList as common name needed
    spList <- as_tibble(birds) %>% 
      select(binomial) %>% 
      rename(Species = binomial) %>% 
      distinct() %>% 
      left_join(birdList, by=c('Species'='Scientific name')) %>% 
      rename(CommonName = `English name`) %>% 
      select(-`Global IUCN Red List Category`)
    
    write_csv(spList, '../Data/Raw/BirdLifeInternational-BOTW2020/ForestSpeciesList_IntersectsTropical.csv')
    
    
#### ========= Querying IUCN Red List API & BirdLife Datazone ======= ####
  # I need to know which species are forest specialists, and their altitude req.
  pacman::p_load(tidyverse, rredlist, rvest)
  
  ### set IUCN Red List API 
    # from here: https://www.ucsbtidytuesday.com/post/2020-05-12-gordon-blasco-api/
    # set your api key as an environmental variable so you do not upload
    # now call the variable that you set
    apikey <- Sys.getenv("IUCN_KEY")

  ###### FUNCTION to classify habitat info ######
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

  ###### FUNCTION to retrieve info from red list ######
    # from here: https://github.com/bienflorencia/rBiodiversidata/blob/master/Data%20Cleaning%20and%20Standardisation%20Scripts/retrieve_IUCN_data.R
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
    
  ###### FUNCTION to scrape birdlife website #####
    # tips from Simon's script: https://github.com/jsocolar/colombiaBeta/blob/master/bird_lists_maps_traits/birdlife_scraper.R
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
  # ---- EXTRACT data from RedList for amphibians, mammals, reptiles ######
    ### read in species list
    spList <- read_csv('../Data/Raw/IUCN-SpeciesDistributions/SpeciesList_IntersectsTropical.csv')
    
    ### retrieve info
    IUCNdata <- retrieve_info(spList)
    head(IUCNdata)
    write_csv(IUCNdata, '../Data/Raw/IUCNdata_IntersectsTropical_others.csv')

  # ---- EXTRACT data from RedList & BirdLife International for birds ######
    # species list
    spList <- read_csv('../Data/Raw/BirdLifeInternational-BOTW2020/ForestSpeciesList_IntersectsTropical.csv')
    
    ### retrieve info from Red List (not sure why needed actually. just to be consistent I guess)
    IUCNbirds <- retrieve_info(spList)
    
    ## scrape website for forest dependency data
    BirdLifeData <- scrape_birdlife(IUCNbirds)
      # some results contradict v9 data. 
      # Cercococcyx mechowi listed as 'Does not normally occur in forest.' v9 = High
      # Aramides cajaneus, Ortalis guttata, Zosterops everetti, Zosterops japonicus, Zosterops palpebrosus listed as 'unset'. use v9 values.
    
    BirdLifeData <- BirdLifeData %>% 
      replace_na(list(Elev_lower=0))
    write_csv(BirdLifeData, '../Data/Raw/BirdLifeInternational-BOTW2020/BirdLifeData_IUCNdata_IntersectsTropical_ForestBirds.csv')
    
    ## Filter to just med and high forest dependency
    BirdLifeData <- BirdLifeData %>% 
      filter(ForestDependency=='Medium'|ForestDependency=='High') 
    write_csv(BirdLifeData, '../Data/Raw/IUCNdata_IntersectsTropical_Birds.csv')
  
#### ========= Rasterise and clip maps ======= ==========
  # ---- list of forest vertebrates ----
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
  # ---- split the taxa shapefiles ------
  pacman::p_load(tidyverse, sf, raster, rgdal)
  
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
  # ---- make 2010 forest cover layer binary & mask elevation ----
  library(terra)
  forest2010 <- rast('../Data/Raw/ForestedPixels2010_Thresh25_TropForResampled.tif')
  forest2010 <- classify(forest2010, matrix(c(0,512,0,512,1024,1), ncol=3, byrow=TRUE), include.lowest=TRUE)
  writeRaster(forest2010, '../Data/Raw/ForestedPixels2010_Thresh25_TropForResampled_50PercBinary.tif')
  
  elev <- rast('../Data/Raw/elevation_TropForResampled.tif')
  elev2 <- mask(elev, forest2010)
  elev3 <- mask(elev2, forest2010, maskvalues=0)
  writeRaster(elev3, '../Data/Raw/elevation_TropForResampled_maskedForest2010.tif')
  
  # ---- make 2018 forest cover layer binary & mask elevation ----
  library(raster)
  forest2018 <- raster('../Data/Raw/ForestedPixels2018_Thresh25_TropForResampled.tif')
  # this shows for each 1km cell, how many 30m pixels (out of max 1024) were still forested at 25% threshold in 2018
  # let's take a cut of value of 50%. so if 50% of a 1km cell is forested it's still suitable
  # reclassify to a binary forest 2018 layer. 
  forest2018 <- reclassify(forest2018, c(0,512,0,512,1024,1), include.lowest=TRUE)
  writeRaster(forest2018, '../Data/Raw/ForestedPixels2018_Thresh25_TropForResampled_50PercBinary.tif')
  
  elev <- rast('../Data/Raw/elevation_TropForResampled.tif')
  forest18 <- rast('../Data/Raw/ForestedPixels2018_Thresh25_TropForResampled_50PercBinary.tif')
  elev2 <- mask(elev, forest18)
  elev3 <- mask(elev2, forest18, maskvalues=0)
  writeRaster(elev3, './elevation_TropForResampled_maskedForest2018.tif')
  # use this as template
  
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
  
  
  ###### FUNCTION to rasterise and crop to AOH #######
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
    if(!file.exists(paste0("./Forest2010-AOH",speciesList[i],".tif"))){
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
  
  # ---- extract AOH from shapefiles  ------
  ### split files up
  # This was done on the HPC (rmem=60G)
  setwd('/shared/edwards_lab1/User/bop19jss/Ch3-biodiversity')
  
  ## get a list of text files that specify which rasters to do
  rasterList <- list.files('./ForestVertebrateShapefiles', '\\.gpkg',
                           full.names=TRUE) 
  # filter out species files that have already been rasterised (2 May 2022)
  in2018 <- unname(sapply(list.files("./Forest2018-AOH", pattern='\\.tif'), function(x) strsplit(basename(x), '\\.')[[1]][1]))
  allFiles <- unname(sapply(rasterList, function(x) strsplit(basename(x), '\\.')[[1]][1]))
  toDo <- setdiff(allFiles, in2018) 
  writeLines(toDo, "./RasterList_2022-05-04.txt")
  
  ### R script to run (Rasterise_getAOH.R)
  library(terra)
  
  setwd('/shared/edwards_lab1/User/bop19jss/Ch3-biodiversity')
  
  # get the index of species to do
  args <- commandArgs(trailingOnly = TRUE)
  # getting the one species from the index file
  species <- args[1]
  
  # elevation raster where cells not covered by forest in 2010 and 2018 have been masked out
  tmp10 <- rast('./elevation_TropForResampled_maskedForest2010.tif')
  tmp18 <- rast('./elevation_TropForResampled_maskedForest2018.tif')
  
  # species data to get elevation range
  speciesData <- read.csv('./FinalForestSpeciesList_BufferedElevRange.csv')
  
  # cat('processing', length(speciesList), 'files in this task', sep=" ", '\n')
  cat(species, "\n")
  
  tryCatch({
    rasterise_AOH(species)}, 
    error=function(e){cat("ERROR: ", conditionMessage(e), "\n")})
  terra::tmpFiles(current=TRUE, orphan=TRUE, old=TRUE, remove=TRUE)
  
  cat("finished", "\n")

  # ---- create species richness maps ----
  # 
    