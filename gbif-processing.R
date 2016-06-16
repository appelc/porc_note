### Download Erethizon sp. from GBIF

library(dismo)
library(rgdal)

aoi <- readOGR(dsn="./Shapefiles", layer="ca_AOI2")

#sp.occur <- gbif(genus='Erethizon', species='', geo=TRUE)
#write.csv(sp.occur, "gbif-all-16jun2016.csv", row.names=FALSE)

sp.occur <- read.csv("./Spreadsheets/gbif-all-16jun2016.csv")
sp.occur <- subset(sp.occur, !is.na(sp.occur$lat))
sp.occur.spdf <- SpatialPointsDataFrame(data.frame(sp.occur$lon, sp.occur$lat), 
                                        data=sp.occur)
sp.occur.spdf@proj4string <- aoi@proj4string
sp.occur.spdf$name <- over(sp.occur.spdf, aoi)$NAME
norcal.occur <- subset(sp.occur.spdf, name=="California")

plot(aoi)
points(norcal.occur)

gbif.pts <- norcal.occur@data
gbif.pts <- subset(gbif.pts)
gbif.pts$institutionCode <- factor(gbif.pts$institutionCode)
unique(gbif.pts$institutionCode)
table(gbif.pts$institutionCode)
