#I discussed parts of this assignment with Logan Bower. 
trueMean1<-5
trueSD1<-5
population1<-rnorm(1e6, trueMean1, trueSD1)
trueMean2<-4
trueSD2<-5
population2<-rnorm(1e6, trueMean2, trueSD2)
Size<-50
Sample1<-sample(population1, Size)
Sample2<-sample(population2, Size)
head(Sample1)
head(Sample2)
boxplot(Sample1, Sample2)
#The samples are slightly different from each other because the population mean was slightly different but they had close to the same amount of variation because the SD was the same.
#below this are a few times I manipulated those first tests to see what would happen
trueMean1<-5
trueSD1<-6
population1<-rnorm(1e6, trueMean1, trueSD1)
trueMean2<-5
trueSD2<-2
population2<-rnorm(1e6, trueMean2, trueSD2)
Size<-50
Sample1<-sample(population1, Size)
Sample2<-sample(population2, Size)
head(Sample1)
head(Sample2)
boxplot(Sample1, Sample2)
trueMean1<-10
trueSD1<-5
population1<-rnorm(1e6, trueMean1, trueSD1)
trueMean2<-25
trueSD2<-5
population2<-rnorm(1e6, trueMean2, trueSD2)
Size<-50
Sample1<-sample(population1, Size)
Sample2<-sample(population2, Size)
head(Sample1)
head(Sample2)
boxplot(Sample1, Sample2)
#below this is back to the assignment
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma<-makeFounder("grandma_mom")
head(MatGrandma)
nrow(MatGrandma)
MatGrandpa<-makeFounder("grandpa_mom")
head(MatGrandpa)
nrow(MatGrandpa)
PatGrandma<-makeFounder("grandma_da")
head(PatGrandma)
nrow(PatGrandma)
PatGrandpa<-makeFounder("grandpa_da")
head(PatGrandpa)
nrow(PatGrandpa)
Alan<-makeBaby(PatGrandma, PatGrandpa)
head(Alan)
nrow(Alan)
Brenda<-makeBaby(MatGrandma, MatGrandpa)
head(Brenda)
nrow(Brenda)
Focus<-makeBaby(Brenda, Alan)
head(Focus)
nrow(Focus)
#The baby should have about half of its genes from each parent
ToMom<-length(grep("mom", Focus))/length(Focus)
head(ToMom)
nrow(ToMom)
#It gives 0.5 or 50%, which is half like I had thought (and also 50% for dad)
ToDa<-length(grep("da", Focus))/length(Focus)
head(ToDa)
#The baby should have about a quarter of its genes from each grandparent
ToMomMom<-length(grep("grandma_mom", Focus))/length(Focus)
ToMomDad<-length(grep("grandpa_mom", Focus))/length(Focus)
head(ToMomMom)
head(ToMomDad)
#The baby gets about 0.1228 (12.28%) from its maternal grandma and about 0.3772 (37.72%) from its maternal grandpa. 
ToDadMom<-length(grep("grandma_da", Focus))/length(Focus)
ToDadDad<-length(grep("grandpa_da", Focus))/length(Focus)
head(ToDadMom)
head(ToDadDad)
#The baby gets about 35.09% from its paternal grandma and about 14.92% from its paternal grandpa.
#So the baby is not equally related to each maternal grandparent or each paternal grandparent. This was not what I expected.
mean(c(12.28,37.72,35.085,14.915))
#On average, the baby is 25% related to each grandparent
Sibling_01<-makeBaby(Brenda, Alan)
#I expect sibling one to share about 50% of his/her DNA with Focus.
ToSib<-length(intersect(Focus, Sibling_01))/length(Focus)
head(ToSib)
#Sibling_01 actually only shares 0.28895 (28.9%) of his/her DNA with Focus.
#I expect Focus to share about the same amount of DNA with the other siblings as with Sibling_01, so maybe around 25%.
ManySiblings<-replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan)))/length(Focus))
quantile(ManySiblings)
mean(ManySiblings)
#Focus shares on average 0.504373 (50.44%) DNA with the many siblings, which was much closer to my initial guess before Sibling_01.
plot(density(ManySiblings), main="", xlab="proportion shared genes")
#There is a range of values in these analyses because while on average you could expect there to be about 50% of shared DNA between siblings, due to random chance, that number can be much different.
#Over only a few values, like when we just did Sibling_01, the number was much lower than that, but after doing 1000 more siblings, the average was much closer to 50% like we would expect.
#The curve that is created by the plot shows this trend of most falling around 50% with only a few much higher or lower.
HWE<-function(p){
  aa<-p^2
  ab<-2*p*(1-p)
  bb<-(1-p)^2
  return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
#It's expected that aa will happen 25% of the time, ab will happen 50% of the time, and bb will happen 25% of the time.
plot(1,1,type="n",xlim=c(0,1), ylim=c(0,1), xlab="freq. allele a", ylab="geno. freq")
p<-seq(from=0, to=1, by=0.1)
GenoFreq<-t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
#As the frequency of the a allele increases, the number of aa individuals in the population also increases. As the allele becomes less frequent, there are less aa individuals. 
#Time and space are not shown on this graph, only the frequency of the allele and the genotype frequency. 
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c("aa","ab","bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")
Pop<-simPop(500)
points(Pop[,"freqa"], Pop[, "Genotypes.aa"]/500, pch=21, bg="red")
#The points are all very close to the aa line, so yes, they appear to match fairly closely to around the middle of the graph.
Pop<-simPop(50)
points(Pop[,"freqa"], Pop[, "Genotypes.aa"]/50, pch=22, bg="red")
#While the points are still close to the line most of time (but not as close), they are now more spread along it. This is because the smaller population size can undergo changes more easily than the larger population.
install.packages("learnPopGen")
library("learnPopGen")
x<-genetic.drift(Ne=200, nrep=5, pause=0.01)
x<-genetic.drift(Ne=200, nrep=5, pause=0.01)
x<-genetic.drift(Ne=200, nrep=5, pause=0.01)
x<-genetic.drift(Ne=200, nrep=5, pause=0.01)
x<-genetic.drift(Ne=200, nrep=5, pause=0.01)
x<-genetic.drift(Ne=2000, nrep=5, pause=0.01)
x<-genetic.drift(Ne=20, nrep=5, pause=0.01)
#The smaller the population, the wider the lines are spread out at the end and the faster they spread out, so more genetic drift is occurring in smaller populations.
PopSizes<-5:50
Samples<-rep(PopSizes, 5)
tExt<-sapply(Samples, function(x) nrow(simPop(x,500)))
Line<-lm(tExt~Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
Line2<-lm(tExt~Samples+0)
summary(Line2)
Line2$coef
plot(Samples, tExt)
abline(Line2)
#The +0 makes the y-intercept 0, which means when you start with 0 samples, you have 0 time to extinction, which makes more sense since if there are no individuals in the population, there cannot be an extinction. 
#As the population increases, the points begin to get further from the line, which means that the linear model may not be as good for modeling once the population gets larger.
#Below this is the extra credit. I got help from https://www.statology.org/robust-regression-in-r/. 
install.packages("MASS")
summary(Line)
library("MASS")
Line<-rlm(tExt~Samples)
Line$coef
plot(Samples, tExt)
abline(Line)
#The line does not look that much different, but the slope is sligtly less and the residual standard error is also much less. 