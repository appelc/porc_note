
## Import Porcufinder records from Google Drive

library(googlesheets)
library(rgdal)
library(lubridate)

gs_ls()
porcufinder <- gs_title("Porcupines (Responses)")
pf <- data.frame(gs_read(ss=porcufinder, ws="Form Responses", is.na(TRUE), range=cell_cols(1:33)))
pf <- pf[-(1:3),c(2, 4, 5, 8, 19, 21, 28:33)] ## get rid of test rows & columns I don't need
colnames(pf) <- c('type_obs', 'date', 'location', 'habitat', 'info', 'observer', 'prev_sub', 'proj_notes', 
                  'credibility', 'utm_e', 'utm_n', 'type')

pf$utm_e <- as.numeric(pf$utm_e)
pf$utm_n <- as.numeric(pf$utm_n)
pf$credibility <- as.factor(pf$credibility)

pf$posix <- as.POSIXct(strptime(pf$date, "%m/%d/%Y %H:%M:%S"), tz="America/Los_Angeles")
pf$date <- as.Date(pf$posix, '%Y-%m-%d', tz = 'America/Los_Angeles')
pf$year <- year(pf$date)

## clean up a little bit
pf$source <- rep('PF', nrow(pf))
pf$id <- paste('PF', 1:nrow(pf), sep = '')

## add decade
pf$decade <- rep(NA, nrow(pf))
pf$decade[pf$date > '1970-01-01' & pf$date < '1980-01-01'] <- '1970s'
pf$decade[pf$date > '1980-01-01' & pf$date < '1990-01-01'] <- '1980s'
pf$decade[pf$date > '1990-01-01' & pf$date < '2000-01-01'] <- '1990s'
pf$decade[pf$date > '2000-01-01' & pf$date < '2010-01-01'] <- '2000s'
pf$decade[pf$date > '2010-01-01' & pf$date < '2020-01-01'] <- '2010s'

## reorder
pf <- pf[,c('source', 'id', 'type', 'date', 'year', 'decade', 'location', 'observer', 'utm_e', 
            'utm_n', 'info', 'habitat', 'prev_sub', 'type_obs', 'proj_notes', 'credibility', 'posix')] 

## keep only credibility 2 (reliable description) or 3 (photos/video)
pf <- pf[pf$credibility == '2' | pf$credibility == '3',] 

## plot
#pf.spdf <- SpatialPointsDataFrame(data.frame(pf$utm_e, pf$utm_n),
#                               data=data.frame(pf),
#                               proj4string=CRS("+proj=utm +zone=10 +datum=NAD83"))
#plot(pf.spdf)

## keep only those in AOI
#aoi <- readOGR(dsn = 'Shapefiles/Admin', layer = 'ca_AOI2', verbose = TRUE)
#aoi_utm <- spTransform(aoi, CRS('+proj=utm +zone=10 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0'))
#pf.spdf <- pf.spdf[aoi_utm,]

#writeOGR(pf.spdf, dsn = '.', layer='Shapefiles/Observations/PF_cleaned_061616', driver='ESRI Shapefile') 
#write.csv(pf.spdf@data, 'Spreadsheets/PF_cleaned_061616.csv') 

#######################################################3

## Also get the Miscellaneous records

gs_ls()
sheet <- gs_title("Misc porc records") 
misc <- data.frame(gs_read(ss=sheet, ws='Records', is.na(TRUE), range=cell_cols(1:16)))

misc$utm_e <- as.numeric(misc$utm_e)
misc$utm_n <- as.numeric(misc$utm_n)
misc$date <- as.Date(misc$date, '%m/%d/%Y')
misc$year <- year(misc$date)

## clean up a little
misc$source[misc$source == 'Flickr'] <- 'FLICKR'
misc$source[misc$source == 'iNaturalist'] <- 'INAT'
misc$id[misc$source == 'FLICKR'] <- paste('FLICKR', 1:6, sep = '')
misc$id[misc$source == 'INAT'] <- paste('INAT', 1:7, sep = '') ## combine these rows?
misc$utm_zone <- as.character(misc$utm_zone)

## add decade
misc$decade <- rep(NA, nrow(misc))
misc$decade[misc$date > '1990-01-01' & misc$date < '2000-01-01'] <- '1990s'
misc$decade[misc$date > '2000-01-01' & misc$date < '2010-01-01'] <- '2000s'
misc$decade[misc$date > '2010-01-01' & misc$date < '2020-01-01'] <- '2010s'

## reorder
misc <- misc[,c('source', 'id', 'type', 'date', 'year', 'decade', 'location', 'observer', 'utm_e', 'utm_n', 'info',
                'utm_zone', 'geotagged', 'lat', 'lon', 'link', 'proj_notes', 'proj_notes2', 'include')]

## only keep approved / relevant ones
misc <- misc[misc$include == 1,] 

#misc.spdf <- SpatialPointsDataFrame(data.frame(misc$utm_e, misc$utm_n),
#                                  data=data.frame(misc),
#                                  proj4string=CRS("+proj=utm +zone=10 +datum=NAD83"))
#plot(misc.spdf)

## keep only those in AOI
#misc.spdf <- misc.spdf[aoi_utm,]

#writeOGR(misc.spdf, dsn = '.', layer = 'Shapefiles/Observations/MISC_cleaned_061616', driver = 'ESRI Shapefile')
#write.csv(misc, 'MISC_cleaned_061616.csv')

#######################################################

## And Ric's database

gs_ls()
sheet2 <- gs_title('ERDO location database RVS 040115') 
erdo <- data.frame(gs_read(ss=sheet2, ws='Locations', is.na(TRUE), range=cell_cols(1:34)))
colnames(erdo) <- c('source', 'id', 'type1', 'year', 'month', 'date', 'county', 'city', 
                    'location', 'fs_unit_id', 'location2', 'accuracy', 'coord_uncertainty', 
                    'alt', 'source2', 'catalog_no', 'observer', 'observer_title', 'contact',
                    'type2', 'record_no', 'website', 'utm_e', 'utm_n', 'utm_zone', 'datum',
                    'lat_n', 'lon_w', 'elevation', 'documentation', 'obs_confidence', 
                    'date_entered', 'info', 'source_comments')
  
erdo$utm_e <- as.numeric(erdo$utm_e)
erdo$utm_n <- as.numeric(erdo$utm_n)
erdo$date <- as.Date(erdo$date, '%m/%d/%Y')

## clean up a little
erdo$id <- paste('ERDO', 1:nrow(erdo), sep = '')
erdo$type <- rep(NA, nrow(erdo))
erdo$type[erdo$type1 == 'Sign' | erdo$type1 == 'Excrement' | erdo$type1 == 'Other sign' | erdo$type1 == 'Other'] <- 'other_sign'
erdo$type[erdo$type1 == 'Sighting'] <- 'sighting'
erdo$type[erdo$type1 == 'Roadkill'] <- 'roadkill'
erdo$type[erdo$type1 == 'Carcass'] <- 'carcass'
erdo$type[erdo$type1 == 'Camera'] <- 'camera'
erdo$type[erdo$type1 == 'Track'] <- 'track'

## add decade
erdo$decade <- rep(NA, nrow(erdo))
erdo$decade[erdo$year >= 1910 & erdo$year < 1920] <- '1910s'
erdo$decade[erdo$year >= 1920 & erdo$year < 1930] <- '1920s'
erdo$decade[erdo$year >= 1930 & erdo$year < 1940] <- '1930s'
erdo$decade[erdo$year >= 1940 & erdo$year < 1950] <- '1940s'
erdo$decade[erdo$year >= 1950 & erdo$year < 1960] <- '1950s'
erdo$decade[erdo$year >= 1960 & erdo$year < 1970] <- '1960s'
erdo$decade[erdo$year >= 1970 & erdo$year < 1980] <- '1970s'
erdo$decade[erdo$year >= 1980 & erdo$year < 1990] <- '1980s'
erdo$decade[erdo$year >= 1990 & erdo$year < 2000] <- '1990s'
erdo$decade[erdo$year >= 2000 & erdo$year < 2010] <- '2000s'
erdo$decade[erdo$year >= 2010 & erdo$year < 2020] <- '2010s'

## reorder
erdo <- erdo[,c('source', 'id', 'type', 'date', 'decade', 'location', 'observer', 'utm_e', 'utm_n', 'info',
                'utm_zone', 'source2', 'location2', 'county', 'accuracy', 'year', 'month', 'city', 
                'fs_unit_id', 'coord_uncertainty', 'alt', 'catalog_no', 'observer_title',
                'contact', 'type1', 'type2', 'record_no', 'website', 'datum', 'lat_n', 'lon_w', 'elevation',
                'documentation', 'obs_confidence', 'date_entered', 'source_comments')]

## plot
#erdo.spdf <- SpatialPointsDataFrame(data.frame(erdo$utm_e, erdo$utm_n),
#                                    data=data.frame(erdo),
#                                    proj4string=CRS("+proj=utm +zone=10 +datum=NAD83"))

## crop to AOI
#erdo.spdf <- erdo.spdf[aoi_utm,]
#erdo.spdf$name <- over(erdo.spdf, aoi)$NAME ##CRS issue?
#erdo.spdf <- subset(erdo.spdf, name=="California")

#writeOGR(erdo.spdf, dsn = '.', layer = 'Shapefiles/Observations/ERDO_cleaned_061616', driver = 'ESRI Shapefile')
#write.csv(misc, 'Spreadsheets/ERDO_cleaned_061616.csv')

#######################################################

## CDFW records from Richard Callas / Kathryn Purcell

cdfw.shp <- readOGR(dsn = 'Shapefiles/Observations/Originals', layer='CDFW_KP', verbose=TRUE)
plot(cdfw.shp)

## made data frame with attributes & coordinates
cdfw <- data.frame(cdfw.shp@data, cdfw.shp@coords)
head(cdfw)
colnames(cdfw) <- c('id', 'date', 'observer', 'species', 'tot_obs', 'info', 'type1', 'type2', 'utm_e', 'utm_n')

## clean up
cdfw$date <- as.Date(cdfw$date, '%m/%d/%Y')
cdfw$year <- year(cdfw$date)
cdfw$source <- rep('CDFW', nrow(cdfw))
cdfw$id <- paste('CDFW', 1:nrow(cdfw), sep = '')

cdfw$type <- rep(NA, nrow(cdfw))
cdfw$type[cdfw$type1 == 'Alive'] <- 'sighting'
cdfw$type[cdfw$type1 == 'Dead'] <- 'carcass'
cdfw$type[cdfw$type1 == 'Dead' & cdfw$type2 == 'Road Kill'] <- 'roadkill'
cdfw$type[cdfw$type1 == 'Dead' & cdfw$type2 == 'Shot'] <- 'killed'

## some of these are camera or track/sign...
cdfw$type[c(25:29, 43, 56)] <- 'camera'
cdfw$type[c(78:80, 93, 95, 97, 101)] <- 'other_sign'

## one of these is duplicated in the ERDO database (from USFS NRIS): 'ERDO86'/'CDFW32'
cdfw <- cdfw[-32,] 

## decade
cdfw$decade <- rep(NA, nrow(cdfw))
cdfw$decade[cdfw$date > '1990-01-01' & cdfw$date < '2000-01-01'] <- '1990s'
cdfw$decade[cdfw$date > '2000-01-01' & cdfw$date < '2010-01-01'] <- '2000s'
cdfw$decade[cdfw$date > '2010-01-01' & cdfw$date < '2020-01-01'] <- '2010s'

## reorder
cdfw <- cdfw[,c('source', 'id', 'type', 'date', 'year', 'decade', 'observer', 'utm_e', 'utm_n',
                'info', 'species', 'tot_obs', 'type1', 'type2')]

## crop to AOI
#proj4string(aoi_utm) <- proj4string(cdfw)
#cdfw <- cdfw[aoi_utm,]

#writeOGR(cdfw, dsn = '.', layer = 'Shapefiles/Observations/CDFW_cleaned_061616', driver = 'ESRI Shapefile')
#write.csv(cdfw, 'Spreadsheets/CDFW_cleaned_061616.csv')

#######################################################

## And Yocom

yocom.shp <- readOGR(dsn = 'Shapefiles/Observations/Originals', layer='Yocom', verbose=TRUE)
plot(yocom.shp)

## made data frame with attributes & coordinates
yocom <- data.frame(yocom.shp@data, yocom.shp@coords)
head(yocom)
colnames(yocom) <- c('id', 'year1', 'type1', 'year', 'observer', 'source', 'county',
                         'location', 'info', 'utm_e', 'utm_n')
## clean up
yocom$id <- paste('YOC', 1:nrow(yocom), sep = '')
yocom$type <- rep('NA', nrow(yocom))
yocom$type[yocom$type1 == 'Sighting'] <- 'sighting'
yocom$type[yocom$type1 == 'Roadkil' | yocom$type1 == 'Roadkill'] <- 'roadkill'
yocom$type[yocom$type1 == 'Carcass'] <- 'carcass'
yocom$type[yocom$type1 == 'Killed' | yocom$type1 == 'Shot' | yocom$type1 == 'Trapped'] <- 'killed'
yocom$type[yocom$type1 == 'unknown' | yocom$type1 == 'no data'] <- 'unk'

## add decade
yocom$decade <- rep('NA', nrow(yocom))
yocom$decade[yocom$year >= 1900 & yocom$year < 1910] <- '1900s'
yocom$decade[yocom$year >= 1910 & yocom$year < 1920] <- '1910s'
yocom$decade[yocom$year >= 1920 & yocom$year < 1930] <- '1920s'
yocom$decade[yocom$year >= 1930 & yocom$year < 1940] <- '1930s'
yocom$decade[yocom$year >= 1940 & yocom$year < 1950] <- '1940s'
yocom$decade[yocom$year >= 1950 & yocom$year < 1960] <- '1950s'
yocom$decade[yocom$year >= 1960 & yocom$year < 1970] <- '1960s'

## reorder
yocom <- yocom[,c('source', 'id', 'type', 'year', 'decade', 'location', 'observer', 'utm_e', 'utm_n',
                  'info', 'type1', 'county', 'year1')]

#writeOGR(yocom, dsn = '.', layer = 'Shapefiles/Observations/Yocom_cleaned_061616', driver = 'ESRI Shapefile')
#write.csv(yocom, 'Spreadsheets/Yocom_cleaned_061616.csv')

## don't need to crop (all within AOI)

###################################################################

## Merge these 5 before adding GBIF
library(dplyr)

no_gbif <- bind_rows(pf, misc, cdfw, erdo, yocom)

## import GBIF
gbif.shp <- readOGR(dsn = 'Shapefiles/Observations', layer = 'GBIF_061616_proj', verbose = TRUE)

## make DF
gbif <- data.frame(gbif.shp@data, gbif.shp@coords)

## manipulate columns etc.
colnames(gbif)[colnames(gbif) == 'type'] <- 'type_gbif'
colnames(gbif)[colnames(gbif) == 'type_nw'] <- 'type'
colnames(gbif)[colnames(gbif) == 'coords.x1'] <- 'utm_e'
colnames(gbif)[colnames(gbif) == 'coords.x2'] <- 'utm_n'
gbif$month <- as.character(gbif$month) # sloppy but will be compatible with other dataframe

## combine all into DF
all_obs <- bind_rows(no_gbif, gbif)

## make SPDF
all_obs_sp <- SpatialPointsDataFrame(data.frame(all_obs$utm_e, all_obs$utm_n),
                                     data=data.frame(all_obs),
                                     proj4string=CRS("+proj=utm +zone=10 +datum=NAD83"))
plot(all_obs_sp)

## crop to AOI
aoi <- readOGR(dsn = 'Shapefiles/Admin', layer = 'ca_AOI2', verbose = TRUE)
aoi_utm <- spTransform(aoi, CRS('+proj=utm +zone=10 +datum=NAD83')) ## b/c all obs are UTM, NAD83
final_obs <- all_obs_sp[aoi_utm,]

writeOGR(final_obs, dsn = '.', layer='Shapefiles/Observations/all_obs_final_061716', driver='ESRI Shapefile') 
write.csv(final_obs, 'Spreadsheets/all_obs_final_061716.csv') 

