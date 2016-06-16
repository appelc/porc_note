### Download Erethizon sp. from GBIF

library(dismo)
library(rgdal)

sp.occur <- gbif(genus='Erethizon', species='', geo=TRUE)
aoi <- readOGR(dsn=".", layer="ca_AOI")
