setwd("C:\\Users\\becky\\OneDrive\\Desktop\\Evolution\\Tasks\\Project\\Data")
rawMonarch<-read.csv('butterfly data DP.csv',stringsAsFactors=F)
#My hypothesis is that in the last approximately 150 years, North American monarch butterfly wings have evolved to be larger in area through random walk evolution. 
#The data comes from a paper (https://www-pnas-org.wvu.idm.oclc.org/content/117/46/28887) comparing the populations on islands (non-mirgatory) to the migratory populations, but it includes a lot of information about each butterfly, such as sex and where it was found. 
#I used many websites for help with the code and also previous R assignments.
#https://www.statology.org/remove-columns-in-r/
#https://www.statology.org/average-across-columns-in-r/
#https://www.statmethods.net/input/missingdata.html

rawMonarch$avgArea<-rowMeans(rawMonarch[,c(21,25)],na.rm=TRUE)
NAMonarch<-which(rawMonarch[,4]=='North_America')
Monarch1<-rawMonarch[NAMonarch,]
write.csv(Monarch1,"Monarch_data.csv",quote=F,row.names=FALSE)

#install.packages(dplyr)
library(dplyr)

Monarchs<- data.frame(Monarch1)
Monarch2<-Monarchs%>% select(-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,19,20,21,22,23,24,25,26,27,27))
Monarch3<-na.omit(Monarch2)
write.csv(Monarch3,"Monarch_data_sorted.csv",quote=F,row.names=FALSE)
Males<-which(Monarch3[,2]=='male')
maleMonarchs<-Monarch3[Males,]
Females<-which(Monarch3[,2]=='female')
femaleMonarchs<-Monarch3[Females,]

#added by Dr.Mitchell
library("paleoTS")
Monarchs<-Monarch3
Monarchs[,1]<-as.numeric(sapply(Monarchs[,1],function(x) substr(x,1,4)))
mms<-tapply(Monarchs[,3],Monarchs[1],mean)
mm_all<-as.numeric(mms)
vv_all<-rep(var(Monarchs[,3]),length(mm_all))
nn_all<-as.numeric(tapply(Monarchs[,3],Monarchs[,1],length))
tt_all<-as.numeric(names(mms))
Keeps<-1:length(mm_all)
OBJ_all<-as.paleoTS(mm=mm_all[Keeps],vv=vv_all[Keeps],nn=nn_all[Keeps],tt=tt_all[Keeps])
x_URW<-fitSimple(OBJ_all,model="URW",method="Joint")
x_GRW<-fitSimple(OBJ_all,model="GRW",pool=FALSE,method="Joint")
x_Stasis<-fitSimple(OBJ_all,model="Stasis",pool=FALSE,method="Joint")
x_OU<-fitSimple(OBJ_all,model="OU",,pool=FALSE,method="Joint")
compareModels(x_OU,x_URW,x_GRW,x_Stasis)
plot(OBJ_all,modelFit=x_Stasis)