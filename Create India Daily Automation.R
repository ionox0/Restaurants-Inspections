library(ggplot2)
library(maps)

cleanGlobalFloodData = read.csv("C:\\Users\\14z\\Documents\\Github\\edavPersonal\\cleanGlobalFloodData.csv")

globFloodData = cleanGlobalFloodData
globFloodData$dateBegan = as.Date.factor(globFloodData$dateBegan, "%m/%d/%Y")
globFloodData$dateEnded = as.Date.factor(globFloodData$dateEnded, "%m/%d/%Y")


causes = c("Heavy Rain", "Monsoon", "Hurricane/Tropical Storm", 
           "Snow/Ice Melt", "Dam Failure")

causeColors = c("white", "#ffff33", "#ff7f00", "#756bb1", "#e41a1c",
                "#4daf4a") #Other color

india2007Floods = 
  cbind(subset(globFloodData, 
               centroidX > 67 & centroidX < 97 & 
                 centroidY > 5 & centroidY < 35 &
                 dateEnded >= as.Date("2007-06-01") &
                 dateBegan < as.Date("2007-11-01") &
                 !(mainCause1 %in% causes[] & 
                     mainCause2 %in% causes[] &
                     mainCause3 %in% causes[]))
        [, c("centroidX", "centroidY", "magnitude", 
             "dateBegan", "dateEnded")],
        FloodType = "Other")

for(x in 1:length(causes)){
  if(nrow(subset(globFloodData, 
                   centroidX > 67 & centroidX < 97 & 
                   centroidY > 5 & centroidY < 35 &
                   dateEnded >= as.Date("2007-06-01") &
                   dateBegan < as.Date("2007-11-01") &
                   (mainCause1 == causes[x] | 
                    mainCause2 == causes[x] |
                    mainCause3 == causes[x]))) > 0){
    
    india2007Floods = rbind(india2007Floods, 
                            cbind(subset(globFloodData, 
                                         centroidX > 67 & centroidX < 97 & 
                                           centroidY > 5 & centroidY < 35 &
                                           dateEnded >= as.Date("2007-06-01") &
                                           dateBegan < as.Date("2007-11-01") &
                                           (mainCause1 == causes[x] | 
                                              mainCause2 == causes[x] |
                                              mainCause3 == causes[x]))
                                  [, c("centroidX", "centroidY", "magnitude", 
                                       "dateBegan", "dateEnded")],
                                  FloodType = causes[x]
                            )                 
                      )  
  }
}  



library(animation)
saveHTML({
  ani.options(interval = 0.3)
  for (d in as.Date("2007-06-01"):as.Date("2007-10-31")) {
      worldMap = ggplot() +
        ggtitle(paste("2007 Indian subcontinent floods: ",
                      as.Date.numeric(d, origin = as.Date("1970-01-01")), sep="")) +
        geom_raster(data = subset(india2007.moDf, dataDate == d), 
                    aes(x = lon, y = lat, fill=geoPotHeight), interpolate=FALSE, alpha = 0.9) +
        scale_fill_continuous("Geopotential Height", limits=c(5705, 5918),
                              low = "#b2182b", high="#2166ac") +
        borders("world", colour = "black") +
        coord_cartesian(xlim = c(67, 97), ylim=c(5, 35)) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        
        geom_point(data = subset(india2007Floods, 
                                 dateBegan <= d & dateEnded >= d), 
                   aes(x = centroidX, y = centroidY, color = FloodType, size = magnitude),
                   alpha = 0.5) +
        scale_color_manual(breaks = append(causes, "Other"), 
                           limits = append(causes, "Other"), 
                           values = causeColors) +
        scale_size(trans = "exp", range=c(0, 20), 
                   limits = c(0,9), breaks=c(4, 5, 6, 7)) + 
        guides(color = guide_legend(override.aes = list(size = 10), 
                                    title = "Flood Cause"),
               size = guide_legend(title = "Flood Magnitude")) + 
        theme(legend.key = element_rect(fill = "#bdd7e7"))
      
      print(worldMap)
  }
}, img.name = "indiaDaily2007", imgdir = "indiaDaily2007", 
htmlfile = "indiaDaily2007.html", 
outdir = getwd(), autobrowse = FALSE, ani.height = 600, ani.width = 800, 
verbose = FALSE, autoplay = TRUE, 
title = "India 2007 Floods and Geopotential Height Daily")

remove(d)

remove(causes)
remove(causeColors)
remove(x)
remove(worldMap)



