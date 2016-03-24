library(plyr)
library(ggmap)
library(animation)

setwd('~/Desktop/Columbia/EDAV/Project3')

########
########

data = read.csv("original_data.csv", header = TRUE)

# New columns/formatting, for ArcGis conversion compatability
data$city = 'New York City'
data$state = 'New York'
data$zip = data$ZIPCODE
data$Address <- paste(data$BUILDING, data$STREET, sep=" ")
# Remove apostrophes from violation description column (causing arcmap geocoding errors)
# data_clean <- lapply(data, gsub, pattern="'", replacement="")
data$VIOLATION_DESCRIPTION <- gsub("\032", "", data$VIOLATION.DESCRIPTION)
iconv(data_clean, "latin1", "ASCII", sub="")


# Take only latest inspections for each restaurant (after 2015-01-01)
data$dateConverted = as.Date(as.character(data$GRADE.DATE), '%d/%m/%Y')
latestData = subset(data, dateConverted > as.Date('2015-01-01'))
# Dedupe across the violations (all should have same grade)
dedupeLatestData = latestData[!duplicated(latestData[,2]),]


# Take only address columns for arcmap
address_cols = dedupeLatestData[,c('city', 'state', 'zip', 'Address')]
# Write file for arcmap
write.csv(address_cols, file='address_fixed_dedupe_just_addresses.csv')


# Vermin Counts Histogram
# Count restaurants (10688)
numRestaurants = nrow(dedupeLatestData)

# Vermin codes - 04K - Rats, 04L - Mice, 04M - Roaches, 04N - FRSA Flies
violationCounts = count(data, "VIOLATION.CODE")
verminCounts = violationCounts[(violationCounts$VIOLATION.CODE == "04K" | 
                                  violationCounts$VIOLATION.CODE == "04L" | 
                                violationCounts$VIOLATION.CODE == "04M" |
                                violationCounts$VIOLATION.CODE == "04N" ), ]
verminHist = c(verminCounts[1,2],
               verminCounts[2,2],
               verminCounts[3,2],
               verminCounts[4,2])
xlab = c('Rats', 'Mice', 'Roaches', 'Flies')
barplot(verminHist, main='Vermin violation counts since January 2015', names.arg=xlab, ylim=c(0,30000), ylab='Count', xlab='Vermin')

min(data$dateConverted, na.rm=TRUE)

# VIOLATION.CODE              freq
# Rats                04K     207
# Mice                04L     2294
# Roaches             04M     656
# Flies               04N     1359





#### Inspection Data
#### Join this dataset's lat longs with original dataset violations data
#### Subset the data to just dates where both datasets have data.

# Deduped data with one entry per restaurant
idata = read.csv('data5.csv', header=TRUE)
idata$dateFormatted = as.Date(idata$INSPECTION.DATE, "%m/%d/%Y")


# Raw Data
raw = read.csv('Rats_Rest_LL.csv', header=TRUE, fileEncoding = 'latin1')
# 3000 Rats violations:
ratsRaw = subset(raw, VIOLATION.CODE == '04K')
ratsRaw$dateFormatted = as.Date(ratsRaw$INSPECTION.DATE, "%m/%d/%Y")


# Limit rat sightings data to just Feb 06, 2012 onwards (data from inspections)
idataLatest = subset(idata, dateFormatted > '2012-02-06')


d = as.Date('2015-01-01')

map = get_map('newyork', maptype='terrain', source='google')

map <- ggmap(map, base_layer = ggplot(aes(x = signs.LONGITUDE, y = signs.LATITUDE),
                                       data = newSigns)) +
  geom_point(data = subset(newSigns, dateFormatted <= d & dateFormatted >= d-30),
           aes(x = signs.LONGITUDE, y = signs.LATITUDE, size = 1),
           alpha = 0.5, color='blue') +
  geom_point(data = subset(idata, dateFormatted <= d & dateFormatted >= d-30),
           aes(x = XCoord, y = YCoord,  size = 3),
           alpha = 0.5, color='red') +
  coord_cartesian(xlim = c(-74.10, -73.70), ylim=c(40.5, 40.95))


#############
# Animation #
#############

# Raw Data of all Rats Sightings (newSigns)
signs = read.csv('rat_signs.csv', header=TRUE, fileEncoding='latin1')
newSigns = data.frame(signs$LATITUDE, signs$LONGITUDE, signs$INSPECTION_DATE)
newSigns$dateFormatted = as.Date(newSigns$signs.INSPECTION_DATE, "%m/%d/%y")


# Raw Data of all Rats Violations
ratsViolations = read.csv('Rats_Rest_LL.csv', header=TRUE, fileEncoding = 'latin1')
ratsViolations$dateFormatted = as.Date(ratsViolations$INSPECTION.DATE, "%m/%d/%y")


cols = c('Latitude', 'Longitude')
ratsViolationsIgnoreNA = ratsViolations[!rowSums(is.na(ratsViolations[cols])), ]

ratsViolationsRemoveRowsWithNAs = ratsViolations[!ratsViolations$Latitude=='#N/A',]
ratsViolationsRemoveRowsWithNAs = ratsViolationsRemoveRowsWithNAs[!ratsViolationsRemoveRowsWithNAs$Longitude=='#N/A',]
ratsViolationsRemoveRowsWithNAs$Latitude = as.numeric(ratsViolationsRemoveRowsWithNAs$Latitude)
ratsViolationsRemoveRowsWithNAs$Longitude = as.numeric(ratsViolationsRemoveRowsWithNAs$Longitude)

map = get_map('newyork', maptype='terrain', source='google')

saveHTML({
  ani.options(interval = 0.3)
  for (d in as.Date("2015-12-01"):as.Date("2016-01-01")) {
  
    map_2 = ggplot() +
    # , base_layer = ggplot(
    #   aes(x = signs.LONGITUDE, y = signs.LATITUDE),
    #   data = newSigns)) + 
    #   
    geom_point(
      data = subset(newSigns, dateFormatted <= d & dateFormatted >= d-30), 
      aes(x = signs.LONGITUDE, y = signs.LATITUDE),
      alpha = 0.5, colour='blue') +

    geom_point(data = subset(ratsViolationsRemoveRowsWithNAs, dateFormatted <= d & dateFormatted >= d-30),
        aes(x = Longitude, y = Latitude),
        alpha = 0.5, colour='red') +
      
    coord_cartesian(xlim = c(-74.10, -73.70), ylim=c(40.5, 40.95))
    
    print(map_2)
  }
}, 
img.name = "rats", imgdir = "ian", 
htmlfile = "rats.html", 
outdir = getwd(), autobrowse = FALSE, ani.height = 680, ani.width = 800, 
verbose = FALSE, autoplay = TRUE, 
title = "General Rats sightings and Restaurant Rats Violations")

############
############
map_2 <- ggmap(map, base_layer = ggplot(
  aes(x = LONGITUDE, y = LATITUDE),
  data = ratsSightings)) + 

remove(d)

remove(causes)
remove(causeColors)
remove(x)
remove(worldMap)

