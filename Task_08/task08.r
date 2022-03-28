#I worked on parts of this assignment with Logan Bower (especially questions 6-10 and the extra credit) and I also discussed some parts on Reddit.
setwd("C:\\Users\\becky\\Desktop\\Evolution\\Tasks\\Task_08")
library(phytools)
library(ape)
text.string<-"(((((((cow,pig),whale),(bat(lemur,human))),(robin,iguana)),coelacanth),(gold_fish,trout)),shark);"
vert.tree<-read.tree(text=text.string)
plot(vert.tree,edge.width=2)
nodelabels(frame="circle",bg='white',cex=1)
#question 1 - gold fish are more closely related to sharks
vert.tree
#question 2 - there are no branch lengths
str(vert.tree)
tree<-read.tree(text="(((A,B),(C,D)),E);")
plotTree(tree,offset=1)
tiplabels(frame="circle",bg='lightblue',cex=1)
nodelabels(frame="circle",bg='white',cex=1)
tree$tip.label
tree$edge
AnolisTree<-force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length,col='black',border='white',main="",xlab="edge lengths for the Anolis tree",ylim=c(0,50),xlim=c(0,6))
tipEdges<-which(AnolisTree$edge[,2]<=Ntip(AnolisTree))
Lengths<-AnolisTree$edge.length
names(Lengths)<-AnolisTree$tip.label
names(Lengths)[which(Lengths==min(Lengths))]
plot(AnolisTree,cex=0.25)
Labs<-sapply(AnolisTree$edge.length,round,digits=2)
edgelabels(text=Labs,cex=0.25)
?plot.phylo
#question 3 (for 3-8, the code is below the question)
plot(AnolisTree,cex=0.25,show.tip.label=FALSE)
#question 4
plot(AnolisTree,cex=0.25,type="radial")
#question 5
plot(AnolisTree,cex=0.25,tip.color='red')
#question 6 - the shortest one is Anolis occultus
which(Lengths==min(Lengths))
AnolisTree2<-drop.tip(AnolisTree,"Anolis_occultus")
plot(AnolisTree2,cex=0.25)
ltt(AnolisTree)
abline(0,1,lwd=2,col='red',lty=2)
fit.bd(AnolisTree,rho=0.2)
pdf("Question10", height=4, width=4)
Question10<-ltt(AnolisTree)
abline(0,1,lwd=2,col='red',lty=2)
dev.off()
#The slope of the line is less positive, so there are fewer new lineages forming at the end.
#The line never goes down because the lineages keep branching into new ones so there cannot be less than there was before. 
#The slope does vary, which tells us that the rate new lineages are starting fluctuates over time.
#The number of new lineages forming at the end is lower than it was at the beginning.
#question 10 - b=0.8031, d=0
#extra credit is below this
install.packages("treebase")
library("treebase")
Huelsenbeck <- search_treebase("Huelsenbeck", by="author")
install.packages("rotl")
library("rotl")
cat_studies <- studies_find_studies(property = "ot:focalCladeOTTTaxonName",value = "Felidae", exact = TRUE)
cat_tree <- get_study_tree(study_id = cat_studies[["study_ids"]][1],
                           tree_id = cat_studies[["tree_ids"]][1])
plot(cat_tree,cex=0.25)
fit.bd(cat_tree)
library(devtools)
devtools::install_github("phylotastic/rphylotastic")
library(rphylotastic)
help(package="rphylotastic")
#I was unable to complete the extra credit because treebase didn't work and when I tried to run the fit.bd() function on trees made using other packages (I got the code from their examples because I wanted to see how they did it before I did my own) it would not work.