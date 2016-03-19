data <- read.csv(file.choose(), header=T)
data$violation.code
table(data$violation.code)

install.packages("zipcode")
library(zipcode)
library(ggplot2)
library(plyr)

## Attach longitude and latitude correpsonding to zipcode to our 
## original dataset 
data(zipcode)
zip_ny <- data.frame()
for (i in 1:nrow(zipcode)){
  if(zipcode$state[i] == "NY"){
    zip_ny <- rbind(zip_ny, zipcode[i,])
  }
} 
data1 <- subset(data, !duplicated(data[,1])) 
rename(data1, c("ZIPCODE"="zip"))
colnames(data1)[6] <- "zip"
## This is our new dataset with lon and lat information 
data2 <- merge(data1, zip_ny, by="zip") 




