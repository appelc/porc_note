### CWHR veg data

library(rgdal)
library(ggplot2)
library(dplyr)

##### First, use WHR TYPE

## available proportions by WHR type
veg <- read.delim('Spreadsheets/WHR/avail_name.txt', sep = ',')
head(veg)
veg$prop <- veg$COUNT / sum(veg$COUNT)

## make a key for the WHR numbers, names, and codes
whr <- read.delim('Spreadsheets/WHR/whr_veg_clip.txt', sep = ',')
num <- unique(cwhr$WHRNUM)
name <- unique(cwhr$WHRNAME)
code <- unique(cwhr$WHRTYPE)
whr_type_key <- data.frame(num, name, code)
write.csv(whr_type_key, 'Spreadsheets/WHR/whr_type_key.csv')

## now look at used points by WHR TYPE
veg_pts <- readOGR(dsn = 'Shapefiles/WHR', layer = 'extract_pts_whrtype', verbose = TRUE)
used <- table(veg_pts@data$WHRTYPE)

prop_used_type <- NULL

for (i in (1:36)){
  sum <- sum(used)
  prop_i <- used[i] / sum
  df <- data.frame(names(used[i]), prop_i)
  colnames(df) <- c('CWHRTYPE', 'prop')
  prop_used_type <- rbind(prop_used_type, df)
}

head(prop_used_type)

## need to add the rest of the WHR types with 0s


#################################################################3

##### Second, use WHR TYPE

## available proportions by life form
form <- read.delim('Spreadsheets/WHR/avail_form.txt', sep = ',')
head(form)
form$prop_avail <- form$COUNT / sum(form$COUNT)

## make a key for the WHR numbers and life forms
whr_form <- read.delim('Spreadsheets/WHR/whr_veg_form.txt', sep = ',')
value <- unique(whr_form$VALUE)
life_form <- unique(whr_form$LIFE_FORM)
whr_form_key <- data.frame(value, life_form)
write.csv(whr_form_key, 'Spreadsheets/WHR/whr_form_key.csv')

## and look at used points by WHR LIFE FORM
veg_pts_form <- readOGR(dsn = 'Shapefiles/WHR', layer = 'extract_pts_whrform', verbose = TRUE)
used_form <- table(veg_pts_form@data$RASTERVALU)

prop_used_form <- NULL

for (i in (1:8)){
  sum <- sum(used_form)
  prop_i <- used_form[i] / sum
  df <- data.frame(names(used_form[i]), prop_i)
  colnames(df) <- c('life_form_code', 'prop_used')
  prop_used_form <- rbind(prop_used_form, df)
}

head(prop_used_form)

## need better way to associate the codes with the life forms
prop_used_form$LIFE_FORM <- whr_form_key$life_form
form$prop_used <- prop_used_form$prop_used
form ## have them all together but unfortunately need to reshape for ggplot

## messy but works to reshape for ggplot
form$use_avail <- rep('used', nrow(form))
prop_used_form$use_avail <- rep('avail', nrow(prop_used_form))
form$prop <- form$prop_avail
prop_used_form$prop <- prop_used_form$prop_used

form_2 <- bind_rows(form, prop_used_form)
form.df <- data.frame(form_2$LIFE_FORM, form_2$use_avail, form_2$prop)
colnames(form.df) <- c('life_form', 'use_avail', 'prop')

## plot
ggplot(form.df, aes(life_form, prop, fill = as.factor(use_avail))) +
      geom_bar(position = 'dodge', stat = 'identity')





