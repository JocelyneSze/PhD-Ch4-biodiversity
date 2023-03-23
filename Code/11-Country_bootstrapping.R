#### =============== Country permutation tests ================= ####
# Note: done in HPC ShARC as task array for each country 
# requested mem = 50GB
# Run permutation tests to calculate difference for
# combined taxa in IPL vs 10km buffer/50km buffer/all outside, and 
# for each taxa in IPL vs 10km buffer
# for species richness, threat score, and range-size rarity
# to be used as null distribution for comparison with observed difference

library(data.table)
library(dplyr)
# data.table v1.14.2, dplyr v1.0.7

setwd('/file/path')

## one country for each task array job, so get the country index
args <- commandArgs(trailingOnly = TRUE) 
no <- as.numeric(args[1])
# args[1] is a value from 1-55 for the relevant countries
countries <- c("ARG","AUS","BDI","BEN","BGD","BLZ","BOL","BRA","CAF","CHN","CIV",
          "CMR","COD","COG","COL","CRI","DMA","ECU","ETH","FJI","GAB","GTM",
          "GUF","GUY","HND","IDN","IND","KEN","KHM","LAO","LKA","MEX","MMR","MYS","NCL",
          "NGA","NIC","NPL","PAK","PAN","PER","PHL","PRY","RWA","SLB","SLV",
          "SUR","TGO","THA","TWN","TZA","UGA","USA","VEN","VNM")
noOutside50km <- c("DMA", "LAO", "NCL", "NPL", "SLV")
cat(no, 'the country is', countries[no], '\n')

#### run permutation test for all taxa ####
scenario = c("SR", "TS", "IR")

for(i in 1:length(scenario)){ 
  cat(i, scenario[i], '\n')
  
  #### first for all taxa ####
  cat('All taxa permutation', '\n')
  countryFile <- paste0("./CountryData/All_taxa_", countries[no], ".csv")
  countryDF <- fread(countryFile)
  finalDF <- data.frame(RepNo = 1:1000)
  
  ## FOR IPL 
  iplScen <- paste0("IPL_", scenario[i], "_All")
  iplDF <- countryDF %>% 
    filter(scenario==iplScen)
  iplN <- nrow(iplDF)
  
  ## FOR 0-10 KM BUFFER
  buff10Scen <- paste0("buff10_", scenario[i], "_All")
  buff10DF <- countryDF %>% 
    filter(scenario==buff10Scen) %>% 
    bind_rows(iplDF)
  buff10Result = numeric(1000)
  cat("IPL & 10km buffer", "\n")
  # repeated 1000 times
  for(j in 1:1000){
    # draw without replacement ipl+buff10km N times 
    index = sample(nrow(buff10DF), size=iplN, replace=FALSE) 
    # and calculate mean difference between those selected & not each time
    buff10Result[j] = mean(buff10DF$value[index]) - mean(buff10DF$value[-index])
  }
  # convert to data frame 
  buff10Result <- as.data.frame(buff10Result)
  rm(iplScen, iplDF, buff10Scen, index)
  
  ## FOR 0-50 KM BUFFER
  buff50Scen <- paste0("buff50_", scenario[i], "_All")
  ## need to add buff10DF and buff50DF
  buff50DF <- countryDF %>%
    filter(scenario==buff50Scen) %>%
    bind_rows(buff10DF)
  buff50Result = numeric(1000)
  cat("IPL & 50km buffer", "\n")
  # repeated 1000 times
  for(j in 1:1000){
    # draw without replacement ipl+buff50km N times
    index = sample(nrow(buff50DF), size=iplN, replace=FALSE)
    # and calculate mean difference between those selected & not each time
    buff50Result[j] = mean(buff50DF$value[index]) - mean(buff50DF$value[-index])
  }
  buff50Result <- as.data.frame(buff50Result)
  rm(buff10DF, buff50Scen, index)
  
  ## FOR ALL OUTSIDE IPL
  # if there are no pixels outside IPL
  if(countries[no] %in% noOutside50km){
    cat("this country has no additional area outside 50km")
    ## join the buff10 and buff50 output together
    finalDF <- bind_cols(buff10Result, buff50Result)
    fwrite(finalDF, paste0("./CountryData/Permutation/All_taxa_",scenario[i],"_",countries[no],".csv"))
    rm(buff10Result, buff50Result, buff50DF, finalDF)
  } else{
    outScen <- paste0("out_",scenario[i], "_All")
    # need to join with buff50
    outDF <- countryDF %>%
      filter(scenario==outScen) %>%
      bind_rows(buff50DF)
    outResult = numeric(1000)
    cat("IPL & all outside IPL", "\n")
    # repeated 1000 times
    for(j in 1:1000){
      # draw without replacement ipl+outIPL N times
      index = sample(nrow(outDF), size=iplN, replace=FALSE)
      # and calculate mean difference between those selected & not each time
      outResult[j] = mean(outDF$value[index]) - mean(outDF$value[-index])
    }
    outResult <- as.data.frame(outResult)
    ## join the buff10, buff50, and out output together
    finalDF <- bind_cols(buff10Result, buff50Result, outResult)
    fwrite(finalDF, paste0("./CountryData/Permutation/All_taxa_",scenario[i],"_",countries[no],".csv"))
    rm(buff10Result, buff50Result, buff50DF, finalDF, outScen, outDF, outResult)
  }
  
  #### second for each taxa (but only IPL and 10k buffer) ####
  cat('Separate taxa permutation', '\n')
  countryFile <- paste0("./CountryData/Sep_taxa_",scenario[i],"_", countries[no], ".csv")
  countryDF <- fread(countryFile)
  finalDF <- data.frame(RepNo = 1:1000)
  
  # for each taxa
  vert <- c("Amphibia", "Aves", "Mammalia", "Reptilia")
  
  for(k in 1:length(vert)){
    ## FOR IPL 
    iplScen <- paste0("IPL_",scenario[i],"_",vert[k])
    iplDF <- countryDF %>% 
      filter(scenario==iplScen)
    
    ## FOR 0-10 KM BUFFER
    buff10Scen <- paste0("buff10_",scenario[i],"_",vert[k])
    buff10DF <- countryDF %>% 
      filter(scenario==buff10Scen) %>% 
      bind_rows(iplDF)
    buff10Result = numeric(1000)
    cat("IPL & 10km buffer", "\n")
    # repeated 1000 times
    for(j in 1:1000){
      # draw without replacement ipl+buff10km N times 
      index = sample(nrow(buff10DF), size=iplN, replace=FALSE) 
      # and calculate mean difference between those selected & not each time
      buff10Result[j] = mean(buff10DF$value[index]) - mean(buff10DF$value[-index])
    }
    buff10Result <- as.data.frame(buff10Result)
    names(buff10Result) <- vert[k]
    finalDF <- bind_cols(finalDF, buff10Result)
    rm(iplScen, iplDF, buff10Scen, buff10DF, buff10Result, index)
  }
  # for the 4 vert classes, save the final output
  fwrite(finalDF, paste0("./CountryData/Permutation/Sep_taxa_",scenario[i],"_",countries[no],".csv"))
}


cat('finished for this country', countries[no], "\n")