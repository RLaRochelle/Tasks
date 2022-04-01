#Task 10
#A group of us worked together to do most of the first for loop and its questions, but I worked mainly with Logan Bower and Kayla Coffman, who helped me with much of the coding for that loop and its questions, and also a few parts of the second one and its questions later.
#There were several other people in the group for the first loop, including Megan Cozort and Spencer Cutlip, but I don't know all of their names and I mostly worked with Logan and Kayla. I also posted and discussed it on Reddit. 
library ("phytools")
#questions 1-3
trees<-list()
births<-c()
Fractions<-c()
net_div<-c()
rate<-c()
length<-c()
for (i in 1:100){
  births[i]<-runif(1)
  Fractions[i]<-runif(1)
  trees[[i]]<-pbtree(b=births[i],d=births[i]*Fractions[i],n=100)
  net_div[[i]]<-births[i]-births[i]*Fractions[i]
  rate[[i]]<-births[i]
  length[[i]]<-mean(trees[[i]]$edge.length)
}
tips<-log(sapply(trees,Ntip))
plot(net_div,tips)
plot(rate,length)
sp_rate<-unlist(rate)
lengths<-unlist(length)
cor(sp_rate,lengths)
pdf("Net diversity and tips", height=4, width=4)
plot(net_div,tips)
dev.off()
pdf("Speciation rate and length", height=4, width=4)
plot(rate,length)
dev.off()
#question 4 - as the net diversification rate increases, the log of the tips also increases
#question 5 - as the speciation rate increases, the average branch length decreases
#question 6 - the correlation between speciation rate and branch length is -0.2498617
trees
Tree<-trees[[90]]
Tree
plotTree(Tree,cex=0.25)
pdf("Tree", height=4, width=4)
plot(Tree,cex=0.25)
dev.off()
rates<-c()
traits<-c()
for (i in 1:100) {
  rates[i]<-runif(1)
  traits[[i]]<-fastBM(Tree,sig2=rates[i])
}
mean_rate[[i]]<-mean(rates[[i]])
mean_trait[[i]]<-mean(traits[[i]])
cor(mean_trait[[i]],mean_rate[[i]])
traits2[[i]]<-unlist(mean_trait[[i]])
plot(traits2[[i]],mean_rate[[i]])
pdf("correlation between rate and trait mean", height=4, width=4)
plot(traits2[[i]],mean_rate[[i]])
dev.off()
#question 8 - I was not able to figure this part out
var_rate[[i]]<-var(rates[[i]])
var_trait[[i]]<-var(traits2[[i]])
cor(var_trait[[i]],var_rate[[i]])
#question 9 - I was not able to figure this part out 
traitMat<-cbind(traits[[1]],traits[[4]])
cor(traitMat)
#question 10 - the correlation between them is 1, which is significant because it means there is a definite relationship between the traits