#### ============ Testing for significance ============== ####
# Note: done in HPC ShARC in interactive mode (vmem=20G)
# Test for significance between permuted difference and
# and observed difference between 
# inside IPL and outside (10km buffer/50km buffer/all outside)

library(data.table)
library(dplyr)
library(readr)
# data.table v1.14.2, dplyr v1.0.7, readr v1.4.0

setwd('/file/path')

scenario = c("SR","TS", "IR") 
countries <- c("ARG","AUS","BDI","BEN","BGD","BLZ","BOL","BRA","CAF","CHN","CIV",
          "CMR","COD","COG","COL","CRI","DMA","ECU","ETH","FJI","GAB","GTM",
          "GUF","GUY","HND","IDN","IND","KEN","KHM","LAO","LKA","MEX","MMR","MYS","NCL",
          "NGA","NIC","NPL","PAK","PAN","PER","PHL","PRY","RWA","SLB","SLV",
          "SUR","TGO","THA","TWN","TZA","UGA","USA","VEN","VNM")
vert = c("Amphibia", "Aves", "Mammalia", "Reptilia")


#### 1. calculate observed mean/median ####
## for IPL only and for All taxa only 
finalDF <- data.frame(Scenario = as.character(),
                      Country = as.character(),
                      Taxa = as.character(),
                      Area = as.character(),
                      Value = as.numeric())

for(i in 1:length(scenario)){
  cat("scenario", scenario[i], "\n")
  
  for(j in 1:length(countries)){
    cat(j, "country is", countries[j], "\n")
    countryFile <- paste0("./CountryData/All_taxa_", countries[j], ".csv")
    countryDF <- fread(countryFile)
    
    cat("for all taxa combined", "\n")
    ## FOR IPL
    iplScen <- paste0("IPL_",scenario[i],"_All")
    iplDF <- countryDF %>%
      filter(scenario==iplScen)
    DF <- data.frame(Scenario = scenario[i],
                     Country = countries[j],
                     Taxa = "All",
                     Area = "IPL",
                     Value = median(iplDF$value))
    finalDF <- finalDF %>%
      bind_rows(DF)
  }
}

fwrite(finalDF, "./ObservedMedianValue.csv")

#### 2. calculate observed difference ####
finalDF <- data.frame(Scenario = as.character(),
                      Country = as.character(),
                      Taxa = as.character(),
                      Area = as.character(),
                      Value = as.numeric())

for(i in 1:length(scenario)){
  cat("scenario", scenario[i], "\n")

  for(j in 1:length(countries)){
    cat(j, "country is", countries[j], "\n")
    countryFile <- paste0("./CountryData_2023-02-09/All_taxa_", countries[j], ".csv")
    countryDF <- fread(countryFile)
  
    #### for combined taxa ####
    cat("for all taxa combined", "\n")
    ## FOR IPL
    iplScen <- paste0("IPL_",scenario[i],"_All")
    iplDF <- countryDF %>%
      filter(scenario==iplScen)
    iplMean <- mean(iplDF$value)
    rm(iplDF)
    ## FOR 0-10 KM BUFFER
    buff10Scen <- paste0("buff10_",scenario[i],"_All")
    buff10DF <- countryDF %>%
      filter(scenario==buff10Scen)
    buff10Mean <- mean(buff10DF$value)
    DF <- data.frame(Scenario = scenario[i],
                     Country = countries[j],
                     Taxa = "All",
                     Area = "buff10",
                     Value = iplMean - buff10Mean)
    finalDF <- finalDF %>%
      bind_rows(DF)
    ## FOR 0-50 KM BUFFER
    buff50Scen <- paste0("buff50_",scenario[i],"_All")
    ## need to add buff10DF and buff50DF
    buff50DF <- countryDF %>%
      filter(scenario==buff50Scen) %>%
      bind_rows(buff10DF)
    buff50Mean <- mean(buff50DF$value)
    DF <- data.frame(Scenario = scenario[i],
                     Country = countries[j],
                     Taxa = "All",
                     Area = "buff50",
                     Value = iplMean - buff50Mean)
    finalDF <- finalDF %>%
      bind_rows(DF)
    ## FOR ALL OUTSIDE IPL
    outScen <- paste0("out_",scenario[i],"_All")
    outDF <- countryDF %>%
      filter(scenario==outScen)
    # only if there are pixels outside IPL
    if(nrow(outDF!=1)){
      # need to join with buff50
      outDF <- outDF %>%
        bind_rows(buff50DF)
      outMean <- mean(outDF$value)
      DF <- data.frame(Scenario = scenario[i],
                       Country = countries[j],
                       Taxa = "All",
                       Area = "out",
                       Value = iplMean - outMean)
      finalDF <- finalDF %>%
        bind_rows(DF)
    }
    
    #### for separate taxa
    cat("for separate taxa", "\n")
    countryFile <- paste0("./CountryData_2023-02-09/Sep_taxa_",scenario[i],'_',countries[j], ".csv")
    countryDF <- fread(countryFile)
    
    for(k in 1:length(vert)){
      cat("vertebrate", vert[k], "\n")
      ## FOR IPL 
      iplScen <- paste0("IPL_",scenario[i],'_',vert[k])
      iplDF <- countryDF %>% 
        filter(scenario==iplScen)
      iplMean <- mean(iplDF$value)
      ## FOR 0-10 KM BUFFER
      buff10Scen <- paste0("buff10_",scenario[i],'_',vert[k])
      buff10DF <- countryDF %>% 
        filter(scenario==buff10Scen)
      buff10Mean <- mean(buff10DF$value)
      DF <- data.frame(Scenario = scenario[i],
                       Country = countries[j],
                       Taxa = vert[k],
                       Area = "buff10",
                       Value = iplMean - buff10Mean)
      finalDF <- finalDF %>% 
        bind_rows(DF)
    }
  }
}
fwrite(finalDF, "./ObservedMeanDifferences_2023-02-10.csv")
  
#### 3. calculate significance ####
# write a function to compare observed vs permutated differences
calc_pValue <- function(scenario, country, taxa, area){
  observed = actualDF %>% 
    filter(Scenario == scenario,
           Country == country,
           Taxa == taxa, 
           Area == area)
  if(taxa == "All"){
    target = countryDF %>% 
      select(all_of(paste0(area, "Result")))
  } else{
    target = countryDF %>% 
      select(all_of(taxa))
  }
  # two-sided test
  pValue <- sum(abs(target[,1]) >= abs(observed$Value)) / 1000
  return(pValue)
}

actualDF <- fread("./ObservedMeanDifferences.csv")
finalDF <- data.frame(Scenario = as.character(),
                    Country = as.character(),
                    Taxa = as.character(), 
                    Area = as.character(),
                    pValue = as.numeric())

for(i in 1:length(scenario)){
  cat(scenario[i], "\n")
  
  for(j in 1:length(countries)){
    cat(countries[j], "\n")
    #### for combined vert classes
    countryDF <- fread(paste0("./CountryData/Permutation/All_taxa_",scenario[i],"_",countries[j],".csv"))
    head(countryDF)
    
    if(ncol(countryDF)==3){
      sigBuff10 <- calc_pValue(scenario[i],countries[j],"All","buff10")
      sigBuff50 <- calc_pValue(scenario[i],countries[j],"All","buff50")
      sigOut <- calc_pValue(scenario[i],countries[j],"All","out")
      DF <- data.frame(Scenario = rep(scenario[i],3),
                       Country = rep(countries[j], 3),
                       Taxa = rep("All",3),
                       Area = c("buff10","buff50","out"),
                       pValue = c(sigBuff10, sigBuff50, sigOut))
    } else{
      sigBuff10 <- calc_pValue(scenario[i],countries[j],"All","buff10")
      sigBuff50 <- calc_pValue(scenario[i],countries[j],"All","buff50")
      DF <- data.frame(Scenario = rep(scenario[i],2),
                       Country = rep(countries[j], 2),
                       Taxa = rep("All",2),
                       Area = c("buff10","buff50"),
                       pValue = c(sigBuff10, sigBuff50))
    }
    finalDF <- finalDF %>% 
      bind_rows(DF)
    #### for separate vert classes
    countryDF <- fread(paste0("./CountryData/Permutation/Sep_taxa_",scenario[i],"_",countries[j],".csv"))
    sigAmphibia <- calc_pValue(scenario[i],countries[j],"Amphibia","buff10")
    sigAves <- calc_pValue(scenario[i],countries[j],"Aves","buff10")
    sigMammalia <- calc_pValue(scenario[i],countries[j],"Mammalia","buff10")
    sigReptilia <- calc_pValue(scenario[i],countries[j],"Reptilia","buff10")
    DF <- data.frame(Scenario = rep(scenario[i],4),
                     Country = rep(countries[j], 4),
                     Taxa = c("Amphibia", "Aves", "Mammalia", "Reptilia"),
                     Area = rep("buff10",4),
                     pValue = c(sigAmphibia, sigAves, sigMammalia, sigReptilia))
    finalDF <- finalDF %>% 
      bind_rows(DF)
  }
}


# combine p value with actualdf
finalDF2 <- finalDF %>% 
  left_join(actualDF, by=c("Scenario","Country","Taxa","Area")) %>%
write_csv("./ObservedMeanDifferences_WithPValue.csv")

