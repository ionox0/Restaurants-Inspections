library(plyr)

map <- get_googlemap('new york city',  scale = 2)

setwd('~/Desktop/Columbia/EDAV/Project3')
# Had to replace SUB unicode chars with '' ("\032" and another one)
data = read.csv("restaurants_fixed.csv", header = TRUE)
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

# Count restaurants (10688)
numRestaurants = nrow(dedupeLatestData)

# Vermin codes - 04K - Rats, 04L - Mice, 04M - Roaches, 04N - FRSA Flies
violationCounts = count(latestData, "VIOLATION.CODE")
verminCounts = violationCounts[(violationCounts$VIOLATION.CODE == "04K" | 
                                  violationCounts$VIOLATION.CODE == "04L" | 
                                violationCounts$VIOLATION.CODE == "04M" |
                                violationCounts$VIOLATION.CODE == "04N" ), ]

verminHist = c(verminCounts[1,2],
               verminCounts[2,2],
               verminCounts[3,2],
               verminCounts[4,2])
xlab = c('Rats', 'Mice', 'Roaches', 'Flies')
barplot(verminHist, names.arg=xlab, ylim=c(0,2500), ylab='Count', xlab='Vermin')



# VIOLATION.CODE              freq
# Rats                04K     207
# Mice                04L     2294
# Roaches             04M     656
# Flies               04N     1359



