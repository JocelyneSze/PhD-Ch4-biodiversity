#### ======= Obtain forest cover 2020 ======= ####
# Note: done on local computer, R version 4.1.1
# need terra package 1.5.21 to be able to use sprc
library(terra)

#### 1. aggregate from 30m to 1km pixels ####
# set tree cover threshold for a 30 m pixel to be considered forested
thresh = 25

# function for turning tree cover 2020 file to binary based on threshold
make_binary <- function(cover, thresh, masklayer){
  # create cover layer where 30 m cells in 2020 that have tree cover of > threshold value = 1 otherwise 0
  # much faster with classify (2 mins vs 13 mins) than accessing cell values using []
  rclmat <- matrix(c(0,thresh-1,0,
                     thresh,100,1), ncol=3, byrow=TRUE)
  cover_thresh <- classify(cover, rclmat, right=NA)
  # also mask it so only land cells are considered (not no data (0) or permanent water (2))
  cover_thresh <- mask(cover_thresh, masklayer, inverse=TRUE, maskvalues=1)
  # this produces a layer where value=0 if tree cover < threshold, value=1 if tree cover > threshold,
  # value=NA if cell is no data or permanent water.
  return(cover_thresh)
}

# function for counting tree cover 2020 from lossyear file which cells still remain forested.
# lossyear file has values 0 to 20. 0 = cell where forest was not lost. 1 to 20 represent year
# (2001-2020) in which forest in that cell was lost
get_treecover2020 <- function(loss, cover_thresh, masklayer){
  rclmat2 <- matrix(c(0,0,1, 
                      1,20,0), ncol=3, byrow=TRUE)
  loss_thresh <- classify(loss, rclmat2, right=NA)
  # mask it so cells which were not forest in 2010 will not be counted
  loss_thresh <- mask(loss_thresh, cover_thresh, maskvalues=0, updatevalue=0)
  # and cells which are not land cells also wont be considered
  loss_thresh <- mask(loss_thresh, masklayer, inverse=TRUE, maskvalues=1)
  return(loss_thresh)
}

# grab files from folder
coverFiles <- list.files("../Data/Raw/GFW2020/ForestCover2010", full.names=TRUE)
lossFiles <- list.files("../Data/Raw/GFW2020/Lossyear", full.names=TRUE)
maskFiles <- list.files("../Data/Raw/GFW2020/Datamask", full.names=TRUE)

for (i in 1:length(coverFiles)){
  cat(i, basename(lossFiles[i]), "\n")
  
  # read in raster layers
  cover <- rast(coverFiles[i])
  loss <- rast(lossFiles[i])
  masklayer <- rast(maskFiles[i])
  
  # check that projection and extents are same
  if (crs(cover) != crs(loss) | crs(cover) != crs(masklayer)) {
    cat("cover file and loss file do not share same projection")
  }
  if (ext(cover) != ext(masklayer) | ext(cover) != ext(masklayer)) {
    cat("cover file and mask file do not share same extents")
  }
  
  # make tree cover 2010 layer binary based on threshold
  cover_thresh <- make_binary(cover, thresh, masklayer)
  
  # find which cells remain forested in 2020
  loss_thresh <- get_treecover2020(loss, cover_thresh, masklayer)
  
  # aggregate forestcover 2010 to ~ 1 km. agg factor 32. so each new cell contains no. of 30 m cells which have
  # tree cover of > threshold value, to a max of 1024
  cover2020_agg <- aggregate(loss_thresh, fact=32, fun="sum", na.rm=TRUE,
                             filename=paste0("../Data/Raw/GFW2020/TreeCover2020_thresh25_agg/",formatC(i, width=3, format="d", flag="0"),".tif"),
                             overwrite=TRUE)
  tmpFiles(remove=TRUE)
}

#### 2. combine 159 files into one ####
rFiles <- list.files("../Data/Raw/GFW2020/TreeCover2020_thresh25_agg", full.names=TRUE)
rlist <- lapply(rFiles, rast)
rsrc <- sprc(rlist)
m <- merge(rsrc, filename="../Data/Raw/GFW2020/TreeCover2020_thresh25_agg.tif")

#### 3. make binary ####
# make align with rest of data
forest18 <- rast('../Data/Raw/ForestedPixels2018_Thresh25_TropForResampled_50PercBinary.tif')
m <- resample(m, forest18, method='near')
rclmat <- matrix(c(0,512,0, 
                    513,1024,1), ncol=3, byrow=TRUE)
forest2020 <- classify(m, rclmat, right=NA)

#### 4. mask over elevation ####
elev <- rast('../Data/Raw/elevation_TropForResampled.tif')
elev2 <- mask(elev, forest2020)
elev3 <- mask(elev2, forest2020, maskvalues=0)
writeRaster(elev3, "../Data/Raw/elevation_TropForResampled_maskedForest2020.tif")
