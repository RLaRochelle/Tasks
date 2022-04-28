#Finale
#I worked with or got help on parts of this assignment from Ashton Hudson, Kayla Canada, and Kaylea Stover. Logan Bower also helped me with the for loop for the first part. I also googled many of the parts for help.
#part 1 - the for loop
mean<-5
variance<-4
stdev<-sqrt(variance)
x<-rnorm(100,mean,stdev)
y<-5*x+2+runif(100,0,0.1)
line<-lm(y~x)
plot(x,y)
test<-lm(formula=y~x)
test
summary<-summary(test)
#The slope is 4.999
#The y-intercept is 2.055 
#The model is linear so it has a constant slope
z<-c()
slopes<-c()
for (i in 1:100){
  z[i]<-runif(1,0,1)
  w<-((x*z[i])+2)+runif(100,0,0.1)
  slope<-coef(lm(w~z[1:100]))
}
slope
plot(z,z[1:100])
#This shows that there is a linear relationship here, with the slope increasing as the random number chosen increases.

#part 2 - Monty Hall
#There are many sources on the internet that can explain this situation and give examples of code and I used one that explained it step by step (https://www.bodo-winter.net/tutorial/bw_doodling_monty_hall.pdf). 
Doors<-c("Door1","Door2","Door3")
results<-c()
for(i in 1:10000){
  fabulous_prize<-sample(Doors)[1]
  random_choose<-sample(Doors)[1]
  host_choose<-sample(Doors[which(Doors!=random_choose&Doors!=fabulous_prize)])[1]
  choose_other<-Doors[which(Doors!=random_choose&Doors!=host_choose)]
  if(random_choose==fabulous_prize){results=c(results,"stickwith")}
  if(choose_other==fabulous_prize){results=c(results,"change")}
}
stick_with_original<-length(which(results=="stickwith"))
change_your_choice<-length(which(results=="change"))
stick_with_original
MontyHall<-c(Change_doors=change_your_choice,Stick_with_first_door=stick_with_original)
barplot(MontyHall)
#The best choice would apparently be to change doors since it wins about 2/3 of the time.

#part 3 - memes
#I made three memes with pictures of my dog
install.packages("meme")
library("meme")
mmplot("teddy.jpg")
meme("teddy.jpg",upper="when your effective population is so small",lower="you're basically mating with your cousin",size=2)
meme("teddy.jpg",upper="when you've got local adaptations",lower="but then a migrant shows up",size=2.5)
mmplot("teddy2.jpg")
meme("teddy2.jpg",upper="when drift fixes genes",lower="but you miss that polymorphism",size=2.5)
