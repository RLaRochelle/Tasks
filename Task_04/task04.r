source("http://jonsmitchell.com/code/fxn05.R")
Pop1<-simPop(Popsize=50, nGenerations=100, initial_p=0.5, h=1, s=0)
plot(1:nrow(Pop1), Pop1[,1], ylim=c(0,1), type="l", xlab="generation", ylab="allele freq.", lwd=2)
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col='red')
legend("topleft", legend=c("a", "b"), col=c("black", "red"), lwd=2, bty="n")
plotFit(nruns=10, n=50, ngens=100, init_p=0.5, h=1, s=0)
Expectation<- c(10,10,10,10)
Observed<- c(15,15,5,5)
Chisq<- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside=T, main=bquote(chi^2~"="~.(Chisq)), legend.text=c("expected", "observed"))
Expectation<- c(10,10,10,10)
Observed<- c(5,0,0,35)
Chisq<-sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside=T, main=bquote(chi^2~"="~.(Chisq)), legend.text=c("expected", "observed"))
Expectation<- c(10,10,10,10)
Observed<- c(2,3,10,30)
Chisq<-sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside=T, main=bquote(chi^2~"="~.(Chisq)), legend.text=c("expected", "observed"))
Expectation<- c(10,10,10,10)
Observed<- c(10,10,10,10)
Chisq<-sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside=T, main=bquote(chi^2~"="~.(Chisq)), legend.text=c("expected", "observed"))
Expectation<- c(10,10,10,10)
Observed<- c(40,0,0,0)
Chisq<-sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside=T, main=bquote(chi^2~"="~.(Chisq)), legend.text=c("expected", "observed"))
Expectation<- c(10,10,10,10)
Observed<- c(20,20,0,0)
Chisq<-sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside=T, main=bquote(chi^2~"="~.(Chisq)), legend.text=c("expected", "observed"))
#The x^2 was originally 10, but then with more uneven numbers it increased to 85 and then 51.3. With all 10's, it was 0, and with all 40 in one it was 120. With two that had 20, it was 40.
#The more unevenly they are distributed and the further from the expected they are, the higher the x^2 is. 
results<-read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors=F)
write.csv(results, '112labdata.csv', quote=F)
counts<-results[,c("yellow", "red", "green", "blue", "black", "tan")]
backgrounds<- c("White", "Red", "Yellow","Green", "Blue", "Black")
backgroundCol<- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi(counts[1,])
#it is 55.2
Chisqs<- apply(counts,1,calcChi)
Chisqs
plotChis(counts)
plotChis(counts)
plotChis(counts)
#The higher chi squared values go with the more uneven bars, and lower chi squared values are with the more even bars
#This means that the higher values of chi squared tend to go with more uneven distributions and lower ones with more even distributions.
Avg<- mean(Chisqs)
#The average is 60.99081, which means that the distribution is at least somewhat uneven, meaning it is different from the expectation that there was no selection (so there was some selection).
#It is much higher than the critical value of 11.70.
backgroundAvgs<- tapply(Chisqs, results[,3], mean)
backgroundAvgs
#They are very close for black, blue, green, and yellow, but red and white are different from the others (red higher and white lower)
propSig<- length(which(Chisqs>11.70))/length(Chisqs)
percSig<- round(100*propSig)
propSig
percSig
#92% had a significant value, which is very high and did surprise me some.
#I doubt natural selection was the only thing since we probably did not follow the rules completely when doing this lab
par(las=1, mar=c(4,4,1,1), mgp=c(2,0.5,0), tck=-0.01, cex.axis=1)
hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")
par(las=1, mar=c(4,4,1,1), mgp=c(2,0.5,0), tck=-0.01, cex.axis=1)
plot(1,1, xlim=c(0,400), ylim=c(1,8.5), xlab="", ylab="", type="n", yaxt="n")
axis(2, at=1:length(backgrounds), labels=backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter<-1
for(i in backgrounds) {
  Data<-Chisqs[which(results[,3]==i)]
  addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
  counter<-counter+1
}
abline(v=11.70, lty=2, lwd=2, col='black')
#They are all mostly to the right of the line, but red had some data out to the far right, and yellow, blue, and black were more spread out. White looks the least spread out.
Simulation<-simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v=11.70, lty=2, lwd=2)
propSig<- length(which(Simulation>11.70))/length(Simulation)
percSig<- round(100*propSig)
propSig
percSig
#89% were "significant"
#The humans doing the experiment were adding in some element of selection, making the numbers so different.
Fit<-c(1,1,1,1,1,1)
names(Fit)<-1:6
Simulation2<-simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))
Fit<-c(0.1,1,1,1,1,1)
names(Fit)<-1:6
Simulation3<-simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0,0,0,0.25))
Fit<-c(0.5,0.6,0.7,1,1,1)
names(Fit)<-1:6
Simulation4<-simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0,0,0,0.25))
Fit<-c(0.1,0.2,0.3,0.4,0.5,1)
names(Fit)<-1:6
Simulation5<-simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0.25))
Fit<-c(0.1,0.1,0.1,0.1,0.1,0.1)
names(Fit)<-1:6
Simulation6<-simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0.25))
mtext(side=2, at=8, line=0, "sel. sim.")
Simulation7<-c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,1,0.25))
#I talked to Logan Bower about some of the questions below.
#The mixture looks more like the student data, so it looks like most student groups show evidence of selection. 
#In general, there was selection occurring in the student groups, which you can see by comparing it to the plot that shows "insane selection"
#I think the humans were either doing exactly what they thought should happen (to make it work) or the exact opposite (to try to mess it up) based on what I remember from the lab and what I see here.
#The students were selecting in different ways depending on the student and doing it differently each time compared to the computer doing it more consistently. 
#Because the student graphs do look somewhat similar to the mixture of the simulations, I think we can say that some selection occurred, although sometimes in different ways. 
#There was also genetic drift happening because you can see how the student data can sometimes look like the simulations that did not have as much selection occurring.
#It helps to compare them to the critical value, but I think you can see more of what was going on by comparing them to the simulations.
#This could increase the chi squared value because it could make the distribution more uneven, like at the beginning of this activity when I put in different numbers for the observed.
#below this is extra credit
#I discussed the extra credit some on Reddit
testPop<-simPop(Popsize=100, nGenerations=100, h=1, s=0, initial_p=0.5, mu = 0, twoway = TRUE, w = NULL)
mutatePop<-simPop(Popsize=100, nGenerations=100, h=1, s=0, initial_p=0.5, mu = 0.5, twoway = TRUE, w = NULL)