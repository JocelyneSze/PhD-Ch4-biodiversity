#### ========= Querying IUCN Red List API & BirdLife Datazone ======= ####
# Note: done on local computer, using R version 4.0.5
# Identify which species are forest specialists, and their altitude ranges
pacman::p_load(tidyverse, rredlist, rvest)
## tidyverse v1.3, rredlist v0.7, rvest v1.0

### set IUCN Red List API 
# from here: https://www.ucsbtidytuesday.com/post/2020-05-12-gordon-blasco-api/
# set your api key as an environmental variable so you do not upload to public domains
# now call the variable that you set
apikey <- Sys.getenv("IUCN_KEY")

###### FUNCTION to classify habitat info ######
# adapted from here: https://gist.github.com/Martin-Jung/ad250b96f07944cf5f0b
# this function takes the habitat info of a species and returns a value of 
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

