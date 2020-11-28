# Figure in paper
# This version:  27-07-2017
# First version: 21-11-2014
# NB - Panel order has been changed in paper

#### Uncertainty intervals excess deaths ####
# Load data
load("output/reanalysis.RData")
load("output/bootstrap_estimates.RData")

# Prepare data
spec<-c("All reported deaths",
        "Excludes report if admission \n there is no death certificate",
        "Excludes report if death certificate \n claimed but not shown",
        "Excludes all reported deaths \n without death certificates")

# Upper panel (a)
centralA<-c(table[c(2,4,6,8),2]/1000)
lowerA<-c(PanelA[c(3,5,7,9),2]/1000)
upperA<-c(PanelA[c(3,5,7,9),3]/1000)

# Lower panel (b)
centralB<-c(table[c(2,4,6,8),4]/1000)
lowerB<-c(PanelB[c(3,5,7,9),2]/1000)
upperB<-c(PanelB[c(3,5,7,9),3]/1000)

y.axis<-c(length(spec):1)
x<-c(-360,860)

## Plot 
par(mar=c(5,15,2,.5),cex.axis=1.2,cex.lab=1.5,mfrow=c(2,1),las=0,bty="n")

# Panel (a)
plot(centralB,y.axis,type="n",axes=F,xlab="", ylab="",
     main="(a) Non-violent deaths",xlim=x,ylim=c(1,4.25))
rect(lowerB[4],.8,upperB[4],1.2,col="grey90",lwd=0,border=NA)
rect(lowerB[3],1.8,upperB[3],2.2,col="grey90",lwd=0,border=NA)
rect(lowerB[2],2.8,upperB[2],3.2,col="grey90",lwd=0,border=NA)
rect(lowerB[1],3.8,upperB[1],4.2,col="grey90",lwd=0,border=NA)
abline(v=0,lwd=1.5,lty=2)

segments(rep(48,4),y.axis,rep(751,4),y.axis,lwd=2.5,col="black",lend=1)
points(centralB,y.axis,type="p",xlab="", ylab="",main="", 
     pch=19,cex=2,col="black",xlim=x)

mtext("Estimated deaths (thousands)",side=1,line=3,cex=1.5)

axis(2,at=y.axis,label=spec,las=1,tick=FALSE,line=-2) 
axis(1,tick=FALSE)


# Panel (b) 
plot(centralA,y.axis,type="n",axes=F,xlab="", ylab="",
     main="(b) All deaths",xlim=x,ylim=c(1,4.25))
rect(lowerA[4],.8,upperA[4],1.2,col="grey90",lwd=0,border=NA)
rect(lowerA[3],1.8,upperA[3],2.2,col="grey90",lwd=0,border=NA)
rect(lowerA[2],2.8,upperA[2],3.2,col="grey90",lwd=0,border=NA)
rect(lowerA[1],3.8,upperA[1],4.2,col="grey90",lwd=0,border=NA)
abline(v=0,lwd=1.5,lty=2)

segments(rep(48,4),y.axis,rep(751,4),y.axis,lwd=2.5,col="black",lend=1)
points(centralA,y.axis,type="p",xlab="", ylab="",main="", 
     pch=15,cex=2,col="black",xlim=x)

mtext("Estimated deaths (thousands)",side=1,line=3,cex=1.5)
axis(2,at=y.axis,label=spec,las=1,tick=FALSE,line=-2) 
axis(1,tick=FALSE)


##
rm(list=ls()) # Clean workspace

## FIN

