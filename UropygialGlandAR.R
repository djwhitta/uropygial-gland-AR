#Preen Gland AR paper
#Figures for revision
#Begun September 6, 2016

#set working directory
setwd("~/uropygial-gland-AR/")

#load in the data
data=as.matrix(read.csv("UropygialGlandARData.csv", header=TRUE, as.is=TRUE))
dim(data)
head(data)

####Is odor mediated by androgen sensitivity?####
par(mfrow=c(1,2))

##Figure 1A: Female preen gland AR and 1-heptadecanol
#set the variables of interest
ARF=as.numeric(data[1:15,"AR.pg.fold"])
HeptadecanolF=as.numeric(data[1:15,"Prop1Heptadecanol"])
#make a graph
plot(ARF,HeptadecanolF,pch=16,col="black",cex=1.5,main="Females",xlab="AR transcript abundance", ylab="Proportion 1-Heptadecanol")
abline(glm(HeptadecanolF~ARF))

#Figure 1B: Male preen gland AR and 1-tridecanol
ARM=as.numeric(data[16:32,"AR.pg.fold"])
TridecanolM=as.numeric(data[16:32,"Prop1Tridecanol"])
plot(ARM,TridecanolM,pch=17,col="black",cex=1.5,main="Males",xlab="AR transcript abundance", ylab="Proportion 1-Tridecanol")
abline(glm(TridecanolM~ARM))

####Does odor signal aggression levels?####
par(mfrow=c(2,2))

#Figure 2A: Female flyovers and 1-dodecanol
flyoversF=as.numeric(data[1:15,"SQRT.flyovers"])
DodecanolF=as.numeric(data[1:15,"Prop1Dodecanol"])
#make a graph
plot(flyoversF,DodecanolF,pch=16,col="black",cex=1.5,main="Females",xlab="Square root Flyovers", ylab="Proportion 1-Dodecanol")
abline(glm(DodecanolF~flyoversF))

#Figure2B: Female time within 0.25m of intruder and 1-tetradecanol
timeinproxF=as.numeric(data[1:15,"Time...1.4"])
TetradecanolF=as.numeric(data[1:15,"Prop1Tetradecanol"])
plot(timeinproxF,TetradecanolF,pch=16,col="black",cex=1.5,main="Females",xlab="Time within 0.25 m", ylab="Proportion 1-Tetradecanol")
abline(glm(TetradecanolF~timeinproxF))

#Figure2C: Male flyovers and 2-pentadecanone
flyoversM=as.numeric(data[16:32,"SQRT.flyovers"])
PentadecanoneM=as.numeric(data[16:32,"Prop2Pentadecanone"])
plot(flyoversM,PentadecanoneM,pch=17,col="black",cex=1.5,main="Males",xlab="Square root Flyovers",ylab="Proportion 2-Pentadecanone")
abline(glm(PentadecanoneM~flyoversM))

#Figure2D: Male songs and 1-tridecanol
SongsM=as.numeric(data[16:32,"songs"])
TridecanolM=as.numeric(data[16:32,"Prop1Tridecanol"])
plot(SongsM,TridecanolM,pch=17,col="black",cex=1.5,main="Males",xlab="Number of Songs",ylab="Proportion 1-Tridecanol")
abline(glm(TridecanolM~SongsM))

####Do seasonal changes in odor relate to seasonal or breeding state related changes in AR in preen gland####
par(mfrow=c(1,2))

#Figure3A: Male date and 2-tetradecanone
DateM=as.numeric(data[16:32,"Date.Collected.2"])
TetradecanoneM=as.numeric(data[16:32,"Prop2Tetradecanone"])
plot(DateM,TetradecanoneM,pch=17,col="black",cex=1.5,main="Males",xlab="Julian Date",ylab="Proportion 2-Tetradecanone")
abline(glm(TetradecanoneM~DateM))

#Figure3B: Female incubation stage and 1-heptadecanol
StageF=data[1:15,"stage.coded"]
HeptadecanolF=as.numeric(data[1:15,"Prop1Heptadecanol"])
boxplot(HeptadecanolF~StageF,xlab="Incubation Stage", ylab="Proportion 1-Heptadecanol", main="Females")
