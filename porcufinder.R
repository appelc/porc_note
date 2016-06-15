
## Import Porcufinder records from Google Drive

library(googlesheets)
library(rgdal)

gs_ls()
porcufinder <- gs_title("Porcupines (Responses)")
pf <- data.frame(gs_read(ss=porcufinder, ws="Form Responses", is.na(TRUE), range=cell_cols(1:32)))
pf <- pf[-(1:3),c(2, 4, 5, 8, 19, 21, 28:32)] ## get rid of test rows & columns I don't need
colnames(pf) <- c('type', 'date', 'loc', 'habitat', 'info', 'observor', 'prev_sub', 'proj_notes', 
                  'credibility', 'utm_e', 'utm_n')

pf$utm_e <- as.numeric(pf$utm_e)
pf$utm_n <- as.numeric(pf$utm_n)
pf$credibility <- as.factor(pf$credibility)

pf$date <- as.character(pf$date)
pf$posix <- as.POSIXct(strptime(pf$date, "%m/%d/%Y %H:%M:%S"), tz="America/Los_Angeles")

## keep only credibility 2 (reliable description) or 3 (photos/video)
pf <- pf[pf$credibility == '2' | pf$credibility == '3',] 

pf.spdf <- SpatialPointsDataFrame(data.frame(pf$utm_e, pf$utm_n),
                               data=data.frame(pf),
                               proj4string=CRS("+proj=utm +zone=10 +datum=NAD83"))
plot(pf.spdf)

## keep only those in California
CA <- readOGR(dsn='.', layer='CA_boundary', verbose=TRUE) ## import CA boundary shapefile
proj4string(CA) <- proj4string(pf.spdf)
pf.ca.spdf <- pf.spdf[CA,]

writeOGR(pf.ca.spdf, dsn = '.', layer='pf_ca_061516', driver='ESRI Shapefile')
