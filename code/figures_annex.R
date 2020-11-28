# Figures in annex
# Data exploration and summarising results
# This version:  01-02-2017
# First version: 21-11-2014

# Load all data
deaths<-read.csv("raw_data/hh_deaths.csv",header=TRUE,stringsAsFactors=FALSE)
load("data/processed.Rdata")
require(sp)
gadm<-readShapeSpatial("raw_data/IRQ_adm1.shp")


#### Process fatality data ####
deaths<-deaths[deaths$cluster!=47 &
                 deaths$cluster!=73,] # Drop cluster 47 and 73 

# Recode missing observations on gov
deaths[deaths$cluster==87,]$gov<-6
deaths[deaths$cluster==90,]$gov<-2

# Set NAs for mod to June
#sum(is.na(deaths$mod)) # 26 obs.
#deaths$mod[is.na(deaths$mod)]<-6

# Recode cardiovascular violent death
deaths<-deaths[order(deaths$war_death),]
deaths[308,]$war_death<-"N"

# Dummy variable for war & normal deaths
# NB: one obs. coded as D (row 100), unsure about meaning.
deaths$col<-as.numeric(deaths$war_death=="Y") # war deaths
deaths$norm<-as.numeric(deaths$war_death=="N") # normal deaths

# Create variable for date
deaths$day<-"01" 
deaths$date <- paste(deaths$yod, deaths$mod, deaths$day, sep="-")
deaths$date <- as.Date(deaths$date,format="%Y-%m-%d")
deaths$day<-NULL

#### Data coverage ####

# Aggregate data per governorate
d<-merge(deaths,id<-provinces[,c(1,14)],all.x=TRUE) # Governorate names
f<-aggregate(cbind(norm,col)~gov_name+date,d,sum)
f<-f[f$col!=0 | f$norm!=0,]
f<-f[order(f$gov_name,f$date),]
governorate<-as.vector(unique(f$gov_name))

# Set colours (monochromatic)
f$Colour="grey80"
f$Colour[f$norm!=0]="grey80"
f$Colour[f$col!=0]="grey20"
f<-f[order(f$gov_name,f$date,f$Colour),]

## Plot
dev.off()
par(mar=c(8,9,2,1),cex.lab=1.5,cex.axis=1.5)
plot(f$date,factor(f$gov_name),type="n",xlab="",ylab="",axes=FALSE)
abline(h=1:18,lty=2,lwd=.5,col="dimgrey")
points(f$date,factor(f$gov_name),pch=15,cex=2,col=f$Colour)

# Axis
axis(2,at=1:18,labels=governorate,las=1,cex.axis=1.5,tick=FALSE)
axis.Date(1,at=seq(min(f$date),max(f$date),by="12 mon"), 
          format="%b-%Y",las=2,tick=FALSE)


#### Deaths over time ####
# Set NAs for 'mod' to June (as in original work)
sum(is.na(deaths$mod)) # 26 obs.
deaths$mod[is.na(deaths$mod)]<-6

# Aggregate data
d<-aggregate(cbind(col,norm)~yod,deaths,sum)
d<-d[d$yod!=2011,] # Only half a year
d$yod2<-d$yod
d[d$yod==2003,]$yod2<-2003.167

b<-cumsum(d$col)
d[d==0]<-NA
b[b==0]<-NA

# Plot data 
par(mar=c(3,5,2,2),cex.axis=1.5,cex.lab=1.5)
plot(d$yod,d$norm,axes=F,xlab="",ylab="",
     ylim=c(-.1,76),xlim=c(2000.8,2011.5),xaxs="i",type="n")
abline(h=20.5,lty=2,lwd=2,col="grey20") # Baseline
text(2010.5,18.5,"Baseline",cex=1.5)
segments(2001,24,2005,-4,lty=2,col="grey20",lwd=2) # HSR baseline
text(2004.5,3,srt=-31,"HSR Baseline",cex=1.5)
abline(v=2003.167,lty=2,lwd=2) # Start of war
text(2002.8,70,labels="Start of war",font=3,cex=1.5,srt=90)

lines(d$yod,d$norm,type="b",col="grey60",lwd=2,pch=15) # Normal deaths
lines(d$yod2,d$col,type="b",col="black",lwd=2,pch=19)  # Violent deaths
lines(d$yod2,b,type="l",col="black",lty=2,lwd=2) # Cumulative violent deaths

# Axis and labels
text(2010.7,4.5,labels="Violent \n deaths",cex=1.5)
text(2010.7,34,labels="Non-violent \n deaths",cex=1.5)
text(2010.7,75,labels="Cumulative \n violent deaths",cex=1.5)

axis(1,at=2000:2010,tick=FALSE)
axis(2,at=seq(0,80,10),las=1,tick=FALSE)
mtext("Number of deaths",side=2,line=3,cex=1.5)

#### Deaths per governorate ####
fig<-provinces[order(provinces$norm.dum,provinces$col),]
fig$gov_name<-factor(fig$gov_name,levels=fig$gov_name)

## Plot
par(mar=c(4,9,1,1),cex.axis=1.5,cex.lab=1.5)
plot(fig$norm.dum,fig$gov_name,xlim=c(0,75),axes=FALSE,cex=1.5,
     xlab="",ylab="",main="")
segments(rep(0,18),1:18,fig$norm.dum,lend=2,lty=2,lwd=.75,col="grey70")
points(fig$col,fig$gov_name,pch=15,col="black",cex=1.5)
axis(1,seq(0,75,5),tick=FALSE)
axis(2,at=1:18,label=fig$gov_name,tick=FALSE,las=1)
legend(45,5,c("Non-violent deaths","Violent deaths"),
       cex=1.7,pch=c(15,1),col=c("black","black"),
       bty="n",y.intersp=c(0.3))


#### Choropleth maps ####
# Calculate total deaths per governorate per 1000 per year
provinces$pop_before<-provinces$pop_pre/26*12
provinces$pop_during<-provinces$pop_dum/99*12

provinces$deaths_before<-provinces$pop_before/provinces$ind_q*provinces$norm.pre
provinces$deaths_during<-provinces$pop_during/provinces$ind_q*provinces$norm.dum
provinces$deaths_violent<-provinces$pop_during/provinces$ind_q*provinces$col

provinces$before_cap<-provinces$deaths_before/provinces$pop_before/26*12*1000
provinces$during_cap<-provinces$deaths_during/provinces$pop_during/99*12*1000
provinces$violent_cap<-provinces$deaths_violent/provinces$pop_during/99*12*1000

# Assign governorate name based on shapefile
df<-provinces
iraq<-as.vector(gadm$NAME_1)
iraq1<-as.vector(df$gov_name)
df$NAME_1<-c("Al-Anbar","Al-Basrah","Al-Muthannia","Al-Qadisiyah","An-Najaf",
             "Arbil","As-Sulaymaniyah","Babil","Baghdad","Dhi-Qar",
             "Diyala", "Dihok","Karbala","At-Ta'mim","Maysan",
             "Ninawa","Sala ad-Din","Wasit")
df<-df[order(df$NAME_1),]

# Add data to shapefile
gadm@data<-data.frame(gadm@data,
                      before_cap=df$before_cap,
                      during_cap=df$during,
                      violent_cap=df$violent_cap)

# Colours for plot
library(RColorBrewer)  
brks<-c(0,1,2,3,4,5,6,7)
col.regions=brewer.pal(8,"Greys")

# Plot
par(mfrow = c(1, 3),
    oma = c(5, 0, 0, 0), 
    mar = c(0, 0, 3, 0), 
    mgp = c(0, 0, 0),    
    xpd = NA)
    
plot(gadm,col=col.regions[findInterval(gadm$before_cap,brks,all.inside=TRUE)],
     axes=F,cex.main=2,
     main="Non-violent death rate before invasion")
plot(gadm,col=col.regions[findInterval(gadm$during_cap,brks,all.inside=TRUE)],
     axes=F,cex.main=2,
     main="Non-violent death rate after invasion")
plot(gadm,col=col.regions[findInterval(gadm$violent_cap,brks,all.inside=TRUE)],
     axes=F,cex.main=2,
     main="Violent death rate")
legend(39,31, legend=leglabs(brks), fill=col.regions, bty="n",cex=1.5,
       y.intersp=.5)


## FIN

