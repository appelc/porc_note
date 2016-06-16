
## Import Porcufinder records from Google Drive

library(googlesheets)
library(rgdal)

gs_ls()
porcufinder <- gs_title("Porcupines (Responses)")
pf <- data.frame(gs_read(ss=porcufinder, ws="Form Responses", is.na(TRUE), range=cell_cols(1:32)))
pf <- pf[-(1:3),c(2, 4, 5, 8, 19, 21, 28:32)] ## get rid of test rows & columns I don't need
colnames(pf) <- c('type_obs', 'date', 'loc', 'habitat', 'info', 'observor', 'prev_sub', 'proj_notes', 
                  'credibility', 'utm_e', 'utm_n')

pf$utm_e <- as.numeric(pf$utm_e)
pf$utm_n <- as.numeric(pf$utm_n)
pf$credibility <- as.factor(pf$credibility)

pf$date <- as.character(pf$date)
pf$date <- as.POSIXct(strptime(pf$date, "%m/%d/%Y %H:%M:%S"), tz="America/Los_Angeles")

## keep only credibility 2 (reliable description) or 3 (photos/video)
pf <- pf[pf$credibility == '2' | pf$credibility == '3',] 

pf.spdf <- SpatialPointsDataFrame(data.frame(pf$utm_e, pf$utm_n),
                               data=data.frame(pf),
                               proj4string=CRS("+proj=utm +zone=10 +datum=NAD83"))
plot(pf.spdf)

## keep only those in California
CA <- readOGR(dsn='Shapefiles/Admin', layer='CA_boundary', verbose=TRUE) ## import CA boundary shapefile
proj4string(CA) <- proj4string(pf.spdf)
pf.spdf <- pf.spdf[CA,]

## clean up a little bit
pf.spdf@data$source <- rep('PF', nrow(pf.spdf))
pf.spdf@data$id <- paste('PF', 1:nrow(pf.spdf), sep = '')
pf.spdf@data$type <- rep(NA, nrow(pf.spdf))
pf.spdf@data$type[pf.spdf@data$type_obs == 'Live Porcupine'] <- 'live'
pf.spdf@data$type[pf.spdf@data$type_obs == 'Dead Porcupine'] <- 'dead'
pf.spdf@data$type[pf.spdf@data$type_obs == 'Live Porcupine, Quilled my dog' | pf.spdf@data$type_obs == 'Porcupine Quills in dog' | pf.spdf@data$type_obs == 'Dogs got quills in their faces. I pulled them out.' | pf.spdf@data$type_obs == 'Live Porcupine, Dog quilled by porcupine'] <- 'dog'
pf.spdf@data$type[pf.spdf@data$type_obs == 'Dying Porcupine'] <- 'live'
pf.spdf@data$type[pf.spdf@data$type_obs == 'Porcupine Tracks'] <- 'sign'
pf.spdf@data$type[pf.spdf@data$type_obs == 'History of this decline' | pf.spdf@data$type_obs == 'A story from the past and comment'] <- 'live' ## in these specific cases, UTM reference a live obs. mentioned in the general comments
pf.spdf@data$type[pf.spdf@data$type_obs == 'Live Porcupine, Dead Porcupine'] <- 'live_dead'
## reorder
pf.spdf@data <- pf.spdf@data[,c('source', 'id', 'type', 'date', 'loc', 'observor', 'utm_e', 
                            'utm_n', 'info', 'habitat', 'prev_sub', 'type_obs', 'proj_notes',
                            'credibility')]

## figure out how to add a 'decade' column that automatically figures it out

writeOGR(pf.spdf, dsn = '.', layer='Shapefiles/PF_061616', driver='ESRI Shapefile')
write.csv(pf.spdf@data, 'Spreadsheets/PF_cleaned_061616.csv')

#######################################################3

## Also get the Miscellaneous records

gs_ls()
sheet <- gs_title("Misc porc records") 
misc <- data.frame(gs_read(ss=sheet, ws='Records', is.na(TRUE), range=cell_cols(1:13)))

misc$utm_e <- as.numeric(misc$utm_e)
misc$utm_n <- as.numeric(misc$utm_n)
misc$date <- as.Date(misc$date, '%m/%d/%Y')

## clean up a little
misc$source[misc$source == 'Flickr'] <- 'FLICKR'
misc$source[misc$source == 'iNaturalist'] <- 'INAT'
misc$id[misc$source == 'FLICKR'] <- paste('FLICKR', 1:6, sep = '')
misc$id[misc$source == 'INAT'] <- paste('INAT', 1:7, sep = '') ## combine these rows?
misc <- misc[,c('source', 'id', 'type', 'date', 'loc', 'observor', 'utm_e', 'utm_n', 'info',
                'utm_zone', 'geotagged', 'lat', 'lon', 'link')]

misc.spdf <- SpatialPointsDataFrame(data.frame(misc$utm_e, misc$utm_n),
                                  data=data.frame(misc),
                                  proj4string=CRS("+proj=utm +zone=10 +datum=NAD83"))
plot(misc.spdf)
writeOGR(misc.spdf, dsn = '.', layer = 'Shapefiles/MISC_061616', driver = 'ESRI Shapefile')
