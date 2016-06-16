
## Import Porcufinder records from Google Drive

library(googlesheets)
library(rgdal)

gs_ls()
porcufinder <- gs_title("Porcupines (Responses)")
pf <- data.frame(gs_read(ss=porcufinder, ws="Form Responses", is.na(TRUE), range=cell_cols(1:33)))
pf <- pf[-(1:3),c(2, 4, 5, 8, 19, 21, 28:33)] ## get rid of test rows & columns I don't need
colnames(pf) <- c('type_obs', 'date', 'location', 'habitat', 'info', 'observor', 'prev_sub', 'proj_notes', 
                  'credibility', 'utm_e', 'utm_n', 'type')

pf$utm_e <- as.numeric(pf$utm_e)
pf$utm_n <- as.numeric(pf$utm_n)
pf$credibility <- as.factor(pf$credibility)

pf$date <- as.POSIXct(strptime(pf$date, "%m/%d/%Y %H:%M:%S"), tz="America/Los_Angeles")

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
pf <- pf[,c('source', 'id', 'type', 'date', 'decade', 'location', 'observor', 'utm_e', 
            'utm_n', 'info', 'habitat', 'prev_sub', 'type_obs', 'proj_notes', 'credibility')] 

## keep only credibility 2 (reliable description) or 3 (photos/video)
pf <- pf[pf$credibility == '2' | pf$credibility == '3',] 

## plot
pf.spdf <- SpatialPointsDataFrame(data.frame(pf$utm_e, pf$utm_n),
                               data=data.frame(pf),
                               proj4string=CRS("+proj=utm +zone=10 +datum=NAD83"))
plot(pf.spdf)

## keep only those in California
CA <- readOGR(dsn='Shapefiles/Admin', layer='CA_boundary', verbose=TRUE) ## import CA boundary shapefile
proj4string(CA) <- proj4string(pf.spdf)
pf.spdf <- pf.spdf[CA,]

writeOGR(pf.spdf, dsn = '.', layer='Shapefiles/PF_061616', driver='ESRI Shapefile') 
write.csv(pf.spdf@data, 'Spreadsheets/PF_cleaned_061616.csv') 

#######################################################3

## Also get the Miscellaneous records

gs_ls()
sheet <- gs_title("Misc porc records") 
misc <- data.frame(gs_read(ss=sheet, ws='Records', is.na(TRUE), range=cell_cols(1:16)))

misc$utm_e <- as.numeric(misc$utm_e)
misc$utm_n <- as.numeric(misc$utm_n)
misc$date <- as.Date(misc$date, '%m/%d/%Y')

## clean up a little
misc$source[misc$source == 'Flickr'] <- 'FLICKR'
misc$source[misc$source == 'iNaturalist'] <- 'INAT'
misc$id[misc$source == 'FLICKR'] <- paste('FLICKR', 1:6, sep = '')
misc$id[misc$source == 'INAT'] <- paste('INAT', 1:7, sep = '') ## combine these rows?

## add decade
misc$decade <- rep(NA, nrow(misc))
misc$decade[misc$date > '1990-01-01' & misc$date < '2000-01-01'] <- '1990s'
misc$decade[misc$date > '2000-01-01' & misc$date < '2010-01-01'] <- '2000s'
misc$decade[misc$date > '2010-01-01' & misc$date < '2020-01-01'] <- '2010s'

## reorder
misc <- misc[,c('source', 'id', 'type', 'date', 'decade', 'location', 'observor', 'utm_e', 'utm_n', 'info',
                'utm_zone', 'geotagged', 'lat', 'lon', 'link', 'proj_notes', 'proj_notes2', 'include')]

## only keep approved / relevant ones
misc1 <- misc[misc$include == 1,] 

misc.spdf <- SpatialPointsDataFrame(data.frame(misc$utm_e, misc$utm_n),
                                  data=data.frame(misc),
                                  proj4string=CRS("+proj=utm +zone=10 +datum=NAD83"))
plot(misc.spdf)

writeOGR(misc.spdf, dsn = '.', layer = 'Shapefiles/MISC_061616', driver = 'ESRI Shapefile')
write.csv(misc, 'MISC_cleaned_061616.csv')

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

## will need to clip to Northern CA but for now:
#erdo <- subset(erdo, utm_zone != '11N')

# Load Northern CA AOI
aoi <- readOGR(dsn="./Shapefiles", layer="ca_AOI2")

erdo.spdf <- SpatialPointsDataFrame(data.frame(erdo$utm_e, erdo$utm_n),
                                    data=data.frame(erdo),
                                    proj4string=CRS("+proj=utm +zone=10 +datum=NAD83"))

erdo.spdf$name <- over(erdo.spdf, aoi)$NAME

erdo.spdf <- subset(erdo.spdf, name=="California")

plot(erdo.spdf)

writeOGR(erdo.spdf, dsn = '.', layer = 'Shapefiles/ERDO_061616', driver = 'ESRI Shapefile')
write.csv(misc, 'Spreadsheets/ERDO_cleaned_061616.csv')

#######################################################

## CDFW records from Richard Callas / Kathryn Purcell

cdfw <- readOGR(dsn = 'Shapefiles', layer='CDFW_KP', verbose=TRUE)
plot(cdfw)

head(cdfw@data)
colnames(cdfw@data) <- c('id', 'date', 'observer', 'species', 'tot_obs', 'info', 'type1', 'type2')
cdfw@data$date <- as.Date(cdfw@data$date, '%m/%d/%Y')
cdfw@data$source <- rep('CDFW', nrow(cdfw@data))
cdfw@data$id <- paste('CDFW', 1:nrow(cdfw), sep = '')

cdfw@data$type <- rep(NA, nrow(cdfw@data))
cdfw@data$type[cdfw@data$type1 == 'Alive'] <- 'sighting'
cdfw@data$type[cdfw@data$type1 == 'Dead'] <- 'carcass'
cdfw@data$type[cdfw@data$type1 == 'Dead' & cdfw@data$type2 == 'Road Kill'] <- 'roadkill'

## some of these are camera or track/sign...
cdfw@data[c(25:29, 43, 56), 10] <- 'camera'
cdfw@data[c(78:80, 93, 95, 97, 101), 10] <- 'other_sign'

## decade
cdfw@data$decade <- rep(NA, nrow(cdfw@data))
cdfw@data$decade[cdfw@data$date > '1990-01-01' & cdfw@data$date < '2000-01-01'] <- '1990s'
cdfw@data$decade[cdfw@data$date > '2000-01-01' & cdfw@data$date < '2010-01-01'] <- '2000s'
cdfw@data$decade[cdfw@data$date > '2010-01-01' & cdfw@data$date < '2020-01-01'] <- '2010s'

## reorder
cdfw@data <- cdfw@data[,c('source', 'id', 'type', 'date', 'decade', 'observer', 'info',
                          'species', 'tot_obs', 'type1', 'type2')]

writeOGR(cdfw, dsn = '.', layer = 'Shapefiles/CDFW_cleaned_061616', driver = 'ESRI Shapefile')
write.csv(cdfw, 'Spreadsheets/CDFW_cleaned_061616.csv')

