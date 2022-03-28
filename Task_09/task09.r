#Task 09
#I worked with Logan Bower, several other students in a study group, posted on Reddit, and googled things for this assignment
setwd("C:\\Users\\becky\\Desktop\\Evolution\\Tasks\\Task_08")
library(phytools)
library(ape)
tree<-force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(tree$edge.length,col='black',border='white',main="",xlab="edge lengths for the Anolis tree",ylim=c(0,50),xlim=c(0,6))
tipEdges<-which(tree$edge[,2]<=Ntip(AnolisTree))
Lengths<-tree$edge.length
names(Lengths)<-tree$tip.label
names(Lengths)[which(Lengths==min(Lengths))]
plot(tree,cex=0.25)
Labs<-sapply(tree$edge.length,round,digits=2)
edgelabels(text=Labs,cex=0.25)
plot(tree,type="fan")
tree
#question 1 - there are 82 tips in the tree and there are branch lengths present
data<-read.csv("http://jonsmitchell.com/data/svl.csv",stringsAsFactors=F,row.names=1)
dim(data)
#question 2 - it is a matrix that is 82 by 1
svl<-setNames(data$svl,rownames(data))
Ancestors<-fastAnc(tree,svl,vars=TRUE,CI=TRUE)
?fastAnc
Ancestors
#question 3 - they are stored in the ace part of the data, and CI95 is the 95% confidence interval
#question 4 - assumes the trait is continuous and that ML=MLE so you can compare all of them the same way 
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree,type="fan",lwd=2,show.tip.label=F)
tiplabels(pch=16,cex=0.25*svl[tree$tip.label])
nodelabels(pch=16,cex=0.25*Ancestors$ace)
obj<-contMap(tree,svl,plot=F)
plot(obj,type="fan",legend=0.7*max(nodeHeights(tree)),sig=2,fsize=c(0.7,0.9))
fossilData<-data.frame(svl=log(c(25.4,23.2,17.7,19.7,24,31)),tip1=c("Anolis_aliniger","Anolis_aliniger","Anolis_occultus","Anolis_ricordii","Anolis_cristatellus","Anolis_occultus"),tip2=c("Anolis_chlorocyanus","Anolis_coelestinus","Anolis_hendersoni","Anolis_cybotes","Anolis_angusticeps","Anolis_angusticeps"))
#question 5
fossilNodes<-c()
nodeN<-c()
for(i in 1:6) {
  Node<-fastMRCA(tree,fossilData[i,"tip1"],fossilData[i,"tip2"])
  fossilNodes[i]<-fossilData[i,"svl"]
  nodeN[i]<-Node
  names(fossilNodes)<-nodeN
}
Ancestors_withFossils<-fastAnc(tree,svl,anc.states=fossilNodes,CI=TRUE,var=TRUE)
Ancestors
Ancestors_withFossils
#question 7 - the estimated ancestral sizes decrease and the variance is also lower 
#questions 8-10
install.packages("geiger")
library("geiger")
fitContinuous(tree,svl,model="BM")
fitContinuous(tree,svl,model="OU")
fitContinuous(tree,svl,model="EB")
fitContinuous(tree,svl,model="rate_trend")
fitContinuous(tree,svl,model="lambda")
fitContinuous(tree,svl,model="kappa")
fitContinuous(tree,svl,model="delta")
fitContinuous(tree,svl,model="mean_trend")
fitContinuous(tree,svl,model="white")
#The model that fits best is EB, which has the lowest AICc
#This is different than the other model because it shows the rate of evolution changing over time exponentially instead of staying the same as it was at the beginning, like the other assumed.