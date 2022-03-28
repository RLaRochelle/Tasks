setwd('C\\Desktop\\Evolution\\Tasks\\Task_07')
install.packages(spocc)
library(spocc)
Data<-read.csv('WV monarchs.csv',stringsAsFactors=F)
head(Data)
#This is some data about Monarch butterfly sightings in WV that I downloaded from iNaturalist. 
#I probably will not be using it for my project, but it was nice to see how easy it would be to get data from this source. 
