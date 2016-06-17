### Download Erethizon sp. from GBIF

library(dismo)
library(rgdal)

aoi <- readOGR(dsn="./Shapefiles/Admin", layer="ca_AOI2")

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

## because I can't resist making everything the same
gbif.pts$source <- rep('GBIF', nrow(gbif.pts))
gbif.pts$id <- paste('GBIF', 1:nrow(gbif.pts), sep = '')
gbif.pts$type_new <- rep(NA, nrow(gbif.pts))
gbif.pts$type_new[gbif.pts$basisOfRecord == 'HUMAN_OBSERVATION'] <- 'sighting'
gbif.pts$type_new[gbif.pts$basisOfRecord == 'PRESERVED_SPECIMEN'] <- 'specimen'

## change a couple that I know of
gbif.pts$type_new[c(2, 4, 7)] <- 'track'

## add decade
gbif.pts$decade <- rep(NA, nrow(gbif.pts))
## some don't have year (just going to fill these in by hand but should fix this)
gbif.pts$year[51] <- 1928
gbif.pts$year[52] <- 1928
gbif.pts$year[53] <- 1933
gbif.pts$year[54] <- 1928
gbif.pts$year[55] <- 1959
gbif.pts$year[56] <- 1920
gbif.pts$year[57] <- 1939
gbif.pts$year[58] <- 1929
gbif.pts$decade[gbif.pts$year > 1900 & gbif.pts$year < 1910] <- '1900s'
gbif.pts$decade[gbif.pts$year > 1910 & gbif.pts$year < 1920] <- '1910s'
gbif.pts$decade[gbif.pts$year > 1920 & gbif.pts$year < 1930] <- '1920s'
gbif.pts$decade[gbif.pts$year > 1930 & gbif.pts$year < 1940] <- '1930s'
gbif.pts$decade[gbif.pts$year > 1940 & gbif.pts$year < 1950] <- '1940s'
gbif.pts$decade[gbif.pts$year > 1950 & gbif.pts$year < 1960] <- '1950s'
gbif.pts$decade[gbif.pts$year > 1960 & gbif.pts$year < 1970] <- '1960s'
gbif.pts$decade[gbif.pts$year > 1970 & gbif.pts$year < 1980] <- '1970s'
gbif.pts$decade[gbif.pts$year > 1980 & gbif.pts$year < 1990] <- '1980s'
gbif.pts$decade[gbif.pts$year > 1990 & gbif.pts$year < 2000] <- '1990s'
gbif.pts$decade[gbif.pts$year > 2000 & gbif.pts$year < 2010] <- '2000s'
gbif.pts$decade[gbif.pts$year > 2010 & gbif.pts$year < 2020] <- '2010s'

gbif.spdf <- SpatialPointsDataFrame(data.frame(gbif.pts$lon, gbif.pts$lat),
                                    data=gbif.pts)
gbif.spdf@proj4string <- aoi@proj4string
unique(gbif.pts$institutionCode)
table(gbif.pts$institutionCode)

writeOGR(gbif.spdf, dsn = '.', layer = 'Shapefiles/GBIF_061616', driver = 'ESRI Shapefile')
write.csv(gbif.pts, "./Spreadsheets/gbif-subset-16jun2016.csv")
