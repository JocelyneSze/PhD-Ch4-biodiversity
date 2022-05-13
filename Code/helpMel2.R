install.packages("rgeos")
install.packages("rgdal")
install.packages("sf")

library(rgeos);
library(raster);
library(sp);
library(rgdal);
library(sf)

# Set wd
setwd("D:\\Dropbox\\honors projects\\2021-2\\MelanieWind")

# Read in Indo raster map. RC: 500 million cells is too much, should be less than 500k.
#indoRaster = raster("indoMollweideProj.tif")
#plot(indoRaster)



# Read in Indo Shp map 
indoProj = readOGR(dsn=getwd(), layer = "indonesiaMollweideShp")
plot(indoProj)

extent(indoProj)


#I create a coarser raster
r <- raster(as(indoProj, "Spatial"), ncols = 400, nrows = 400)
indoProj$val1 = rep(1,nrow(indoProj))
#background NA is better than 0 as we get rid of the cells for the ocean, table is much smaller
indoRaster <- rasterize(indoProj, r, field="val1", background = NA)
plot(indoRaster)

#I convert the raster into polygons
#This is the placeholder for all the species information to be stored
indoGrid = rasterToPolygons(x=indoRaster, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)

plot(indoGrid)
head(indoGrid)




############
## Mammals## 

# Read mammals shapefiles 
mammalShp = readOGR(dsn=getwd(), layer = "mammalIndo")

View(mammalShp[mammalShp$binomial == "Zaglossus attenboroughi",])

# Project to Mollweide projection
mammalsProj = spTransform(mammalShp, crs(indoRaster))

# Combine rows with same species together (because each there are duplicate rows for the same species on different islands)
aggregateMammals = aggregate(mammalsProj, c("binomial","category"))
# Plot to check. RC: nice but we lose the other columns, like threat status
plot(aggregateMammals)
#this plot shows us two additional non-indonesian islands on the left, so we need to subset to just Indonesia



###################################################################
# This is where i attempt to convert all shapefiles into rasters: #
#I use the package velox for extract that is much faster, need to install from github

#install.packages("devtools")

#library(devtools)
#install_github("hunzikp/velox")
library(velox)


list_sps = list()
#This loops through all the species, makes them a raster and puts them in a list
#Putting all the species rasters in a list makes extract work even faster doing all at once
for(i in 1:nrow(aggregateMammals)){

sps_i = aggregateMammals[i,]

sps_i = spTransform(sps_i, crs(indoGrid))

r <- raster(as(indoGrid, "Spatial"), ncols = 400, nrows = 400)
sps_i$val1 = rep(1,nrow(sps_i))
sps_i_ras <- rasterize(sps_i, r, field="val1", background = NA)

#rasters are converted to velox raster objects
list_sps[[i]] = velox(sps_i_ras)

print(i)#to see how many species have been done

}#closes species loop

#The list needs to be converted again in velox object, otherwise extract does not work
list_sps2 = velox(list_sps)


start_time <- Sys.time()

output1 <- list_sps2$extract(indoGrid, fun=mean)

end_time <- Sys.time()
end_time-start_time #takes one minute to do 283 maps for mammals


head(output1,150) 

#Each column is one mammal I put the names
colnames(output1) = aggregateMammals$binomial

#Now extracting separately for variables that are on their own like DPI wind, KBAs, PAs etc
#For instance with DPI

# Read in Wind DPI (Development Potential Indices) maps 
wind = raster("Wind_DPI\\Wind_DPI.tif")

plot(wind)

#Takes ages, trying velox
# Clip wind DPI map to indonesia only
#windIndo = mask(wind,indoGrid)

#windProj <- projectRaster(windIndo, crs=crs(indoGrid))


# Save Wind maps in Indo. RC: I think writing out and in may be creating problems.
#Let's try to use directly here the masked object.
#writeRaster(windIndo, "wind_DPI_Indo.tif")

# Read in Wind DPI maps clipped to Indonesia.
#RC: Wind DPI seems to be just ones, maybe it is a duplicate of IndoRaster
#windIndo = raster("wind_DPI_Indo.tif")
#Project is Mollweide but just in case I make sure the Indonesia map is the same projection
indoProj2 = spTransform(indoProj, crs(windIndo))

#I try to crop with velox first, quite fast
wind2 = velox(wind)

#I crop wind DPI to Indonesia using velox
wind2$crop(extent(indoProj2))


rm(wind)

#Extract and add as new variable to the output

windExtr <- wind2$extract(indoGrid, fun=mean)

#Add it as a new column
#Indonesia has very little wind farm potential, mostly NAs. Let's try with solar as well

output1$windDPI <- windExtr

output2 = data.frame(output1)

output2$windDPI <- windExtr

head(output2,150)















