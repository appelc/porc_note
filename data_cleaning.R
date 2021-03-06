
## Import records from Google Drive: Porcufinder, Zielinski track plates, Misc, CDFW, ERDO
## Combine with Yocom and GBIF records

library(googlesheets)
library(rgdal) 
library(lubridate) ## for extracting year from dates/posix
library(extrafont) ## for ggplot2
library(dplyr)
library(ggplot2)

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

## keep only credibility 2 (reliable description) or 3 (photos/video)
table(pf$credibility) ## check first to report how many are in each
pf <- pf[pf$credibility == '2' | pf$credibility == '3',] 

## clean up a little bit
pf$source <- rep('PF', nrow(pf))
pf$id <- paste('PF', 1:nrow(pf), sep = '')

## add decade
pf$decade <- rep(NA, nrow(pf))
pf$decade[pf$date >= '1970-01-01' & pf$date < '1980-01-01'] <- '1970s'
pf$decade[pf$date >= '1980-01-01' & pf$date < '1990-01-01'] <- '1980s'
pf$decade[pf$date >= '1990-01-01' & pf$date < '2000-01-01'] <- '1990s'
pf$decade[pf$date >= '2000-01-01' & pf$date < '2010-01-01'] <- '2000s'
pf$decade[pf$date >= '2010-01-01' & pf$date < '2020-01-01'] <- '2010s'

## reorder
pf <- pf[,c('source', 'id', 'type', 'date', 'year', 'decade', 'location', 'observer', 'utm_e', 
            'utm_n', 'info', 'habitat', 'prev_sub', 'type_obs', 'proj_notes', 'credibility', 'posix')] 

## any duplicates? there shouldn't be
## no, don't remove dups based on UTM... for example, the two Healdsburg camera obs. have the same UTM
#dups <- duplicated(pf[ , c('utm_e', 'utm_n')]) 
#dups_pf <- pf[dups,] ## good, no dups

## plot
#pf.spdf <- SpatialPointsDataFrame(data.frame(pf$utm_e, pf$utm_n),
#                               data=data.frame(pf),
#                               proj4string=CRS("+proj=utm +zone=10 +datum=NAD83"))
#plot(pf.spdf)

## keep only those in AOI -- DO THIS AT THE END NOW
#aoi <- readOGR(dsn = 'Shapefiles/Admin', layer = 'ca_AOI2', verbose = TRUE)
#aoi_utm <- spTransform(aoi, CRS('+proj=utm +zone=10 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0'))
#pf.spdf <- pf.spdf[aoi_utm,]

#writeOGR(pf.spdf, dsn = '.', layer='Shapefiles/Observations/PF_cleaned_081316', driver='ESRI Shapefile') 
#write.csv(pf.spdf@data, 'Spreadsheets/PF_cleaned_061616.csv') 

#######################################################

## Now get camera & track plate records from Bill and Ric
surveys <- gs_title("CalSysSurveys")
cameras <- data.frame(gs_read(ss=surveys, ws="Camera_ERDO", is.na(TRUE), range=cell_cols(1:44)))
track_pl <- data.frame(gs_read(ss=surveys, ws="TrackPlate_ERDO", is.na(TRUE), range=cell_cols(1:50)))

## format dates
cameras$date <- as.Date(cameras$DatePhotoTaken, '%m/%d/%Y', tz = 'America/Los_Angeles')
track_pl$date <- as.Date(track_pl$TPVisitDate, '%m/%d/%Y', tz = 'America/Los_Angeles')

## only keep relevant columns
cameras <- cameras[,c(2, 8, 11, 16:18, 24, 45)]
track_pl <- track_pl[,c(3, 9, 12, 17:19, 25, 51)]

cameras$source <- rep('WJZ05', nrow(cameras))
cameras$type <- rep('camera', nrow(cameras))

track_pl$source <- rep('WJZ05', nrow(track_pl))
track_pl$type <- rep('track', nrow(track_pl))

wjz05 <- bind_rows(cameras, track_pl)
wjz05$id <- paste('WJZ05', 1:nrow(wjz05), sep = '_')
wjz05$decade[wjz05$date >= '2000-01-01' & wjz05$date < '2010-01-01'] <- '2000s'
wjz05$UtmZone <- as.character(wjz05$UtmZone)
wjz05$source <- as.character(wjz05$source)
wjz05$Year <- as.numeric(wjz05$Year)

colnames(wjz05) <- c('location', 'SurveyingAgency', 'year', 'utm_zone', 'utm_e', 'utm_n', 'datum', 'date', 
                     'source', 'type', 'id', 'decade')
wjz05 <- wjz05[,c('source', 'id' ,'type', 'date', 'year', 'decade', 'location', 'utm_e', 'utm_n', 
                  'utm_zone', 'datum', 'SurveyingAgency')]

#######################################################

## Also get the Miscellaneous records

gs_ls()
sheet <- gs_title("Misc porc records") 
misc <- data.frame(gs_read(ss=sheet, ws='Records', is.na(TRUE), range=cell_cols(1:16)))

misc$utm_e <- as.numeric(misc$utm_e)
misc$utm_n <- as.numeric(misc$utm_n)
misc$date <- as.Date(misc$date, '%m/%d/%Y')
misc$year <- year(misc$date)

## only keep approved / relevant ones
misc <- misc[misc$include == 1,] 

## clean up a little
misc$source2 <- misc$source
misc$source <- rep('MISC', nrow(misc))
misc$id <- paste('MISC', 1:nrow(misc), sep = '')
misc$utm_zone <- as.character(misc$utm_zone)

## add decade
misc$decade <- rep(NA, nrow(misc))
misc$decade[misc$date >= '1990-01-01' & misc$date < '2000-01-01'] <- '1990s'
misc$decade[misc$date >= '2000-01-01' & misc$date < '2010-01-01'] <- '2000s'
misc$decade[misc$date >= '2010-01-01' & misc$date < '2020-01-01'] <- '2010s'

## reorder
misc <- misc[,c('source', 'id', 'type', 'date', 'year', 'decade', 'location', 'observer', 'utm_e', 'utm_n', 'info', 'source2',
                'utm_zone', 'geotagged', 'lat', 'lon', 'link', 'proj_notes', 'proj_notes2', 'include')]

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

## there are 5 duplicates in here that are also in GBIF. they won't show up as dups because the coords
## are slightly different (UTM here, lat/lon in GBIF and rounded differently)
erdo_dups <- erdo[erdo$info == 'UC BERKELEY MVZ DATA.  PRIVATE PROPERTY.' | erdo$info == 'UC BERKELEY MVZ DATA.',]
erdo <- erdo[-c(28:32),] ## should be a better way but erdo <- erdo[!erdo_dups,] didn't work for data frames

## remove dups before or after assigning IDs?

## clean up a little
erdo$type <- rep(NA, nrow(erdo))
erdo$source <- 'MISC'
erdo$source[erdo$source2 == 'CROS'] <- 'CROS'
erdo$source[erdo$source2 == 'USFS NRIS'] <- 'NRIS'

erdo$id[erdo$source == 'MISC'] <- paste('MISC', (nrow(misc) + 1):(length(which(erdo$source == 'MISC')) + nrow(misc)), sep = '')
erdo$id[erdo$source == 'CROS'] <- paste('CROS', 1:(length(which(erdo$source == 'CROS'))), sep = '')
erdo$id[erdo$source == 'NRIS'] <- paste('NRIS', 1:length(which(erdo$source == 'NRIS')), sep = '')

erdo$type[erdo$type1 == 'Sign' | erdo$type1 == 'Excrement' | erdo$type1 == 'Other sign' | erdo$type1 == 'Other'] <- 'other_sign'
erdo$type[erdo$type1 == 'Sighting'] <- 'sighting'
erdo$type[erdo$type1 == 'Roadkill'] <- 'roadkill'
erdo$type[erdo$type1 == 'Carcass'] <- 'carcass'
erdo$type[erdo$type1 == 'Camera'] <- 'camera'
erdo$type[erdo$type1 == 'Track'] <- 'track'

## one should be a dog encounter
erdo$type[1] <- 'dog encounter'

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

## CDFW records from Richard Callas / Kathryn Purcell / Pete Figura

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
cdfw$decade[cdfw$date >= '1990-01-01' & cdfw$date < '2000-01-01'] <- '1990s'
cdfw$decade[cdfw$date >= '2000-01-01' & cdfw$date < '2010-01-01'] <- '2000s'
cdfw$decade[cdfw$date >= '2010-01-01' & cdfw$date < '2020-01-01'] <- '2010s'

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

## Merge these 6 before adding GBIF
no_gbif <- bind_rows(pf, misc, cdfw, erdo, yocom, wjz05)

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
gbif$source2 <- gbif$instttC

## combine all into DF
all_obs <- bind_rows(no_gbif, gbif) #unequal factor levels error is OK

## now check for duplicates
dups <- duplicated(all_obs[ , c('utm_e', 'utm_n')]) # Finds duplicates in utms
dups.all_obs <- all_obs[dups,] # Create table with duplicates only
## don't remove any of these duplicates. GBIF duplicates likely represent multiple specimens, and the
## NRIS camera duplicate is outside the AOI anyway and will be excluded in the next step
## Should have a better way of finding dups... see above for issue with NRIS/GBIF dups not having
## identical coordinates. Others I just found on my own (e.g., a couple PF/ERDO, PF/CDFW, or ERDO/CDFW)

## collapse some types
all_obs$type_new[all_obs$type == 'sighting' | all_obs$type == 'general'] <- 'sighting'
all_obs$type_new[all_obs$type == 'roadkill'] <- 'roadkill'
all_obs$type_new[all_obs$type == 'carcass'] <- 'carcass'
all_obs$type_new[all_obs$type == 'dog' | all_obs$type == 'dog encounter'] <- 'dog encounter'
all_obs$type_new[all_obs$type == 'track' | all_obs$type == 'other_sign'] <- 'track or sign'
all_obs$type_new[all_obs$type == 'camera'] <- 'remote camera'
all_obs$type_new[all_obs$type == 'killed'] <- 'killed'
all_obs$type_new[all_obs$type == 'unk'] <- 'unknown'
all_obs$type_new[all_obs$type == 'specimen'] <- 'museum specimen'

## make SPDF
all_obs_sp <- SpatialPointsDataFrame(data.frame(all_obs$utm_e, all_obs$utm_n),
                                     data=data.frame(all_obs),
                                     proj4string=CRS("+proj=utm +zone=10 +datum=NAD83"))

## crop to AOI
aoi <- readOGR(dsn = 'Shapefiles/Admin', layer = 'ca_AOI2', verbose = TRUE)
aoi_utm <- spTransform(aoi, CRS('+proj=utm +zone=10 +datum=NAD83')) ## b/c all obs are UTM, NAD83
final_obs <- all_obs_sp[aoi_utm,]

plot(final_obs)
plot(aoi_utm, add = TRUE)

writeOGR(final_obs, dsn = '.', layer='Shapefiles/Observations/all_obs_final_080216', driver='ESRI Shapefile') 
write.csv(final_obs, 'Spreadsheets/all_obs_final_080216.csv') 

########################################

## summarize observations

##  by source
table(final_obs$source) ## primary source (CDFW, CROS, GBIF, MISC, NRIS, PF, YOCOM, WJZ05)
table(final_obs$source2) ## secondary source within ERDO (iNaturalist, Flickr, etc.)
table(final_obs$instttC) ## institution codes within GBIF (CAS, MVZ, etc.)

## by type
table(final_obs$type) ## detailed
table(final_obs$type_new) ## general

## matrix (source and type)
matrix <- table(final_obs$type_new, final_obs$source)
write.csv(matrix, 'Spreadsheets/source_type_matrix_080116.csv')

## by county
counties <- readOGR(dsn = 'Shapefiles/Admin', layer = 'ca_counties_AOI', verbose = TRUE)
proj4string(counties) <- proj4string(final_obs)
final_obs$county <- over(final_obs, counties)$NAME 
table(final_obs$county)

## make a histogram by decade
hist(final_obs$year)
final_obs_df <- data.frame(final_obs@data) ## ggplot needs it as a data frame
cols <- c('#67001F', '#B2182B', '#D6604D', '#F4606D', '#FDDBC7', '#F7F7F7', '#D1E5F0', '#92C5DE',
          '#4393C3', '#2166AC', '#053061', '#001233') ## match colors to map
font_import(pattern = '[A/a]rial')
font_import(pattern = '[T/t]imes')
loadfonts(device = 'win')

ggplot(final_obs_df, aes(decade, fill = decade)) + geom_bar(colour = 'black') + 
      labs(x = 'Decade', y = 'Number of records') +
      scale_fill_manual(values = cols) +
      theme(axis.text = element_text(size = 12, colour = 'black', family = 'Arial'),
            axis.text.x = element_text(angle = 50, hjust = 1),
            axis.title = element_text(size = 14, colour = 'black', family = 'Arial'),
            axis.line.x = element_line(size = 1, colour = 'black'),
            axis.line.y = element_line(size = 1, colour = 'black'),
            axis.ticks = element_line(size = 0.5, colour = 'black'),
            axis.ticks.length = unit(.2, 'cm'),
            panel.background = element_rect(fill = 'white'),
            legend.position = 'none')

## couldn't get to work in ggplot: scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150)),
  