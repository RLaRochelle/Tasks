setwd('C\\Desktop\\Evolution\\Tasks\\Task_02')
Data<-read.csv('http://jonsmitchell.com/data/beren.csv',stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
Data
length(Data) 
#12 numbers
nrow(Data)
#1363 rows
ncol(Data)
#12 columns
colnames(Data)
#tells what is in each column, like the date, time, event, etc.
head(Data)
Data[1,]
#first row
Data[2,]
#second row
Data[1:3,]
#first 3 rows
Data[1:3,4]
#first 3 rows of column 4
Data[1:5, 1:3]
#first 5 rows of first 3 columns
Data[257,1:3]
#this gives the date of the 257th observation
Feeds<- which(Data[,9]=='bottle')
berenMilk<-Data[Feeds,]
head(berenMilk)
nrow(berenMilk)
#322 rows, each one a time when he had a bottle
Feeds<-(Data[,'event']=='bottle')
berenMilk<-Data[Feeds,]
nrow(berenMilk)
Feeds<-which(Data$event=='bottle')
berenMilk<-Data[Feeds,]
nrow(berenMilk)
#all of them had the same number of rows, so they are the same
dayID<-apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID<- sapply(dayID, as.Date, format="%Y-%m-%d", origin="2019-04-18")
Data$age<-dateID-dateID[which(Data$event=='birth')]
head(Data)
beren2<-Data
beren3<- beren2[order(beren2$age),]
head(beren2)
#same as before
head(beren3)
#now they are in order by how old he was
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
#first I copied and pasted in everything from last time
#I worked with Logan Bower for multiple parts of this assignment, like what some of the code meant and why the graph was wrong
#I also posted to Reddit for help and tried to answer A Hudson's question about why the graph was wrong.
#Question 1 - They are inappropriate for the for the data we have here because Hypothesis I has the amount he eats, which is not something we have data for, and because Hypothesis II lacks a specific hypothesis and includes unclear wording like "how much"
Feeds<-which(beren3$event=="bottle")
avgMilk<-mean(beren3$value[Feeds])
avgMilk
#The average amount of milk is 2.36677 ounces. 
#The value column was used because it contains the amount of milk. 
#The square brackets tell it where to get the data to do this, so they are very important because you have to tell it which data to do this analysis on.
avgFeed<-tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
avgFeed
varFeed<- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
varFeed
totalFeed<- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
totalFeed
numFeeds<- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
numFeeds
cor(beren3$value[Feeds], beren3$age[Feeds])
?cor
cor.test(beren3$value[Feeds],beren3$age[Feeds])
berenCor<-cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenCor
berenANOVA<-aov(beren3$value[Feeds]~beren3$caregiver[Feeds])
berenANOVA
boxplot(beren3$value[Feeds]~beren3$caregiver[Feeds], xlab="who gave the bottle", ylab="amount of milk consumed (oz)")
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
?par
#las tells the type of axis that is used, mar sets the margins, mgp formats the title margins, and tck tells the length of the tick marks
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed),lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height=4, width=4)
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
#Question 2 - The problem with this graph is that it almost makes it look like he's eating less, but in reality he was starting to eat other foods not drink as much milk 
source("http://jonsmitchell.com/code/plotFxn02b.R")
pdf("r02b-cumulativeMilkByTime.pdf")
unique(beren3$event)
source("http://jonsmitchell.com/code/plotFxn02b.R")
pdf("r02b-cumulativeMilkByTime.pdf")
source("http://jonsmitchell.com/code/plotFxn02b.R")
dev.off()
#below this is the self quiz
setwd('C\\Desktop\\Evolution\\Tasks\\Task_02')
Data<-read.csv('http://jonsmitchell.com/data/beren.csv',stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
Length<- which(Data[,9]=='trait_length')
berenLength<-Data[Length,]
dayID<-apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID<- sapply(dayID, as.Date, format="%Y-%m-%d", origin="2019-04-18")
Data$age<-dateID-dateID[which(Data$event=='birth')]
beren2<-Data
beren5<- beren2[order(beren2$age),]
Length<-which(beren5$event=="trait_length")
berenLength<- tapply(beren5$value[Length], beren5$age[Length], mean)
cor.test(beren5$value[Length],beren5$age[Length])
berenLengthcor<-cor.test(beren5$value[Length], beren5$age[Length])
summary(berenLengthcor)
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(berenLength)), berenLength, type="b", pch=16, xlab="Beren's age in days", ylab="Beren's length in centimeters")
pdf("r02self-quiz-Length_over_time.pdf", height=4, width=4)
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(berenLength)), berenLength, type="b", pch=16, xlab="Beren's age in days", ylab="Beren's length in centimeters")
dev.off()
#below this is the extra credit (as much of it as I could do from the help I got and from looking up how to work with time in R)
Data<-read.csv('http://jonsmitchell.com/data/beren.csv',stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
Naps<- which(Data[,9]=='nap')
beren4<-Data[Naps,]
start<-apply(beren4, 1, function(x) paste(x[5:6], collapse=':'))
end<-apply(beren4, 1, function(x) paste(x[7:8], collapse=':'))
beren4$naptime<-end-start[which(beren4$event=='nap')]