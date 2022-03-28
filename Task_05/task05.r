#I discussed parts of this assignment with several other classmates and also on Reddit (especially with Kayla Coffman).
setwd('C\\Desktop\\Evolution\\Tasks\\Task_05')
install.packages("learnPopGen")
library("learnPopGen")
??coalescent.plot
pdf("population1_coalescence", height=4, width=4)
coalescent.plot(n=5, ngen=10)
dev.off()
pdf("population2_coalescence", height=4, width=4)
coalescent.plot(n=5, ngen=10)
dev.off()
pdf("population3_coalescence", height=4, width=4)
coalescent.plot(n=5, ngen=10)
dev.off()
mean(c(2,0,1,0,2,0,1,0,2,2,0,2,2,1,0,2,1,1,0,1,2,2,0,0,1,0,2,1,0,2,0,3,1,1,0,1,0,1,1,2,0,1,1,3,0,0,1,0,3,1,0,3,0,1,1,1,2,1,1,0,0,0,1,4,0,1,0,2,2,0,2,2,0,1,0,1,1,1,2,0,0,2,0,0,3,2,2,0,0,1,1,1,0,3,0,2,1,1,2,0,1,0,1,3,0,1,1,2,1,0,1,2,0,0,2,0,2,2,1,0,0,2,1,1,1,0,1,3,0,1,0,1,0,0,4,2,2,1,0,0,3,1,0,0,1))
var(c(2,0,1,0,2,0,1,0,2,2,0,2,2,1,0,2,1,1,0,1,2,2,0,0,1,0,2,1,0,2,0,3,1,1,0,1,0,1,1,2,0,1,1,3,0,0,1,0,3,1,0,3,0,1,1,1,2,1,1,0,0,0,1,4,0,1,0,2,2,0,2,2,0,1,0,1,1,1,2,0,0,2,0,0,3,2,2,0,0,1,1,1,0,3,0,2,1,1,2,0,1,0,1,3,0,1,1,2,1,0,1,2,0,0,2,0,2,2,1,0,0,2,1,1,1,0,1,3,0,1,0,1,0,0,4,2,2,1,0,0,3,1,0,0,1))
#Question 1- I had each one start with 5 alleles for simplicity and clarity, and you can change that by changing n
#Question 2- I only had two of the three go to fixation in 10 generations, and both took 3 generations. The other one came very close, but then one of the individuals had 2 offspring, and then 4 offspring came from those 2.
#Question 3- The mean is 1 throughout the populations, but the variance is 0.96 because there was a range of 0 to 4, so there was a lot of variation in how many offspring each one had
#Question 4- The more fit individuals leave more offspring so their alleles become more common
#Question 5- Yes, and the allele that it has eventually becomes fixed
install.packages("assertthat")
install.packages("RcppArmadillo")
install.packages("https://cran.r-project.org/src/contrib/Archive/coala/coala_0.6.0.tar.gz", repos=NULL, type="source")
library("coala")
install.packages("phytools")
library("phytools")
model<- coal_model(sample_size=5,loci_number=10,loci_length=500,ploidy=2)+
  feat_mutation(10)+
  feat_recombination(10)+
  sumstat_trees()+
  sumstat_nucleotide_div()
stats<-simulate(model,nsim=1)
Diversity<-stats$pi
Diversity
#No, the numbers are all different because there are random mutations and recombinations
Nloci<-length(stats$trees)
t1<-read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
#The individuals are diploid so 5 individuals x 2 = 10 tips
Agel<-max(nodeHeights(t1))
#The age is 1.38
t2<-read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
Age2<-max(nodeHeights(t2))
#The age is 0.535, which is younger than the other one
#Question 7- They do not match. The first one happens over a longer period of time than the second one
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1,t2)
#The first split happened earlier for the blue one, but then the red one saw several rapid divisions before the blue one continued branching most recently
t1_1<-read.tree(text=stats$trees[[1]][1])
t1_2<-read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1,t1_2)
#Now the red and blue are very similar for one of the branches, and then they have differences for the rest, with the red generally branching earlier than the blue
for(locus in 1:Nloci) {
  ntrees<-length(stats$trees[[locus]])
  for (n in 1:ntrees){
    if(locus==1&&n==1){
      outPhy<-read.tree(text=stats$trees[[locus]][n])
    }
    else{
      outPhy<-ape:::c.phylo(outPhy,read.tree(text=stats$trees[[locus]][n]))
    }
  }
}
par(mfrow=c(1,1))
densityTree(outPhy)
model<- coal_model(sample_size=5,loci_number=10,loci_length=500,ploidy=2)+
  feat_mutation(10)+
  feat_recombination(20)+
  sumstat_trees()+
  sumstat_nucleotide_div()
#I doubled the recombination rate to 20, so I think the plot will appear much more spread out than before and take more time.
stats<-simulate(model,nsim=1)
Diversity<-stats$pi
Diversity
Nloci<-length(stats$trees)
t1<-read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
Agel<-max(nodeHeights(t1))
t2<-read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
Age2<-max(nodeHeights(t2))
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1,t2)
t1_2<-read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1,t1_2)
for(locus in 1:Nloci) {
  ntrees<-length(stats$trees[[locus]])
  for (n in 1:ntrees){
    if(locus==1&&n==1){
      outPhy<-read.tree(text=stats$trees[[locus]][n])
    }
    else{
      outPhy<-ape:::c.phylo(outPhy,read.tree(text=stats$trees[[locus]][n]))
    }
  }
}
par(mfrow=c(1,1))
densityTree(outPhy)
#The trees show some branching back at 4 now, not at 3 like in the first one, so I was correct that it would take more time.
model3<-coal_model(10,50)+
  feat_mutation(par_prior("theta", sample.int(100,1)))+
  sumstat_nucleotide_div()
stats<-simulate(model3, nsim=40)
mean_pi<-sapply(stats, function(x) mean(x$pi))
theta<-sapply(stats, function(x) x$pars[["theta"]])
line<-lm(theta~mean_pi)
plot(mean_pi,theta)
abline(line)
#practice
coalescent.plot(n=100, ngen=10)
coalescent.plot(n=50, ngen=10)
coalescent.plot(n=20, ngen=10)
coalescent.plot(n=10, ngen=10)
coalescent.plot(n=5, ngen=10)
coalescent.plot(n=2, ngen=10)
modelx<- coal_model(sample_size=5,loci_number=10,loci_length=500,ploidy=2)+
  feat_mutation(0)+
  feat_recombination(0)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
modelx<- coal_model(sample_size=100,loci_number=10,loci_length=500,ploidy=2)+
  feat_mutation(0)+
  feat_recombination(0)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
modelx<- coal_model(sample_size=20,loci_number=10,loci_length=500,ploidy=2)+
  feat_mutation(0)+
  feat_recombination(0)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
modelx<- coal_model(sample_size=5,loci_number=2,loci_length=500,ploidy=2)+
  feat_mutation(0)+
  feat_recombination(0)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
modelx<- coal_model(sample_size=5,loci_number=2,loci_length=50,ploidy=1)+
  feat_mutation(0)+
  feat_recombination(0)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
modelx<- coal_model(sample_size=2,loci_number=2,loci_length=50,ploidy=1)+
  feat_mutation(0)+
  feat_recombination(0)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
modelx<- coal_model(sample_size=5,loci_number=5,loci_length=50,ploidy=2)+
  feat_mutation(0)+
  feat_recombination(0)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
modelx<- coal_model(sample_size=5,loci_number=5,loci_length=50,ploidy=1)+
  feat_mutation(0)+
  feat_recombination(0)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
modelx<- coal_model(sample_size=10,loci_number=5,loci_length=50,ploidy=1)+
  feat_mutation(0)+
  feat_recombination(0)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
modelx<- coal_model(sample_size=10,loci_number=5,loci_length=50,ploidy=2)+
  feat_mutation(0)+
  feat_recombination(0)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
modelx<- coal_model(sample_size=10,loci_number=10,loci_length=50,ploidy=2)+
  feat_mutation(0)+
  feat_recombination(0)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
#to get number of tips = sample size X ploidy; takes longer with more loci
modelx<- coal_model(sample_size=10,loci_number=1,loci_length=50,ploidy=2)+
  feat_mutation(0)+
  feat_recombination(0)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
modelx<- coal_model(sample_size=10,loci_number=1,loci_length=50,ploidy=1)+
  feat_mutation(0)+
  feat_recombination(0)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
modelx<- coal_model(sample_size=10,loci_number=1,loci_length=50,ploidy=1)+
  feat_mutation(20)+
  feat_recombination(0)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
modelx<- coal_model(sample_size=10,loci_number=1,loci_length=50,ploidy=1)+
  feat_mutation(10)+
  feat_recombination(20)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
modelx<- coal_model(sample_size=10,loci_number=1,loci_length=50,ploidy=1)+
  feat_mutation(0)+
  feat_recombination(20)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
modelx<- coal_model(sample_size=10,loci_number=1,loci_length=50,ploidy=1)+
  feat_mutation(20)+
  feat_recombination(20)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
modelx<- coal_model(sample_size=10,loci_number=1,loci_length=50,ploidy=1)+
  feat_mutation(0)+
  feat_recombination(50)+
  sumstat_trees()+
  sumstat_nucleotide_div()
statsx<-simulate(modelx,nsim=1)
Nloci<-length(statsx$trees)
tx<-read.tree(text=statsx$trees[[1]][1])
plot(tx)
axisPhylo()
#extra credit
activate_msms(jar = NULL, java = NULL, priority = 200, download = TRUE)
modela<- coal_model(sample_size=c(5,10), loci_number=10,loci_length=500,ploidy=2)+
  feat_mutation(10)+
  feat_recombination(10)+
  feat_migration(5, 1, 2)+
  feat_migration(3, 2, 1) +
  feat_selection(strength_A = 5, population = 1, locus_group = "all", time=0)+
  feat_selection(strength_A = 2, population = 2, locus_group = "all", time=0)+
  feat_growth(1, population = 1, time = "0", locus_group = "all")+
  feat_growth(4, population = 2, time = "0", locus_group = "all")+
  sumstat_trees()+
  sumstat_nucleotide_div()
  statsa$pi
modela<- coal_model(sample_size=c(5,10), loci_number=10,loci_length=500,ploidy=2)+
  feat_mutation(10)+
  feat_recombination(10)+
  feat_migration(5, 1, 2)+
  feat_migration(7, 2, 1) +
  feat_selection(strength_A = 5, population = 1, locus_group = "all", time=0)+
  feat_selection(strength_A = 8, population = 2, locus_group = "all", time=0)+
  feat_growth(6, population = 1, time = "0", locus_group = "all")+
  feat_growth(4, population = 2, time = "0", locus_group = "all")+
  sumstat_trees()+
  sumstat_nucleotide_div()
  statsa$pi
modela<- coal_model(sample_size=c(5,15), loci_number=10,loci_length=500,ploidy=2)+
  feat_mutation(10)+
  feat_recombination(10)+
  feat_migration(2, 1, 2)+
  feat_migration(7, 2, 1) +
  feat_selection(strength_A = 1, population = 1, locus_group = "all", time=0)+
  feat_selection(strength_A = 2, population = 2, locus_group = "all", time=0)+
  feat_growth(1, population = 1, time = "0", locus_group = "all")+
  feat_growth(3, population = 2, time = "0", locus_group = "all")+
  sumstat_trees()+
  sumstat_nucleotide_div()
  statsa$pi
#I did this a few times, changing the variables each time, but it always gave me the same results. The values for pi can vary a lot for the ten loci, with some having values as low as 9.49 and others having values as high as 24.38