# R script to create input data for diff-in-diff regression
# This version:  25-07-2016
# First version: 07-01-2014

#### Calculate spatial lag ####
require(spdep)
require(maptools)
load("data/processed.Rdata")
gadm<-readShapeSpatial("raw_data/IRQ_adm1.shp")

# Calculate violent deaths per 100 inhabitants per province
deaths.gov$violence.gov<-(deaths.gov$col/deaths.gov$ind_q)*100  # Normal count
deaths.gov$violence.gov.re<-(deaths.gov$col.c/deaths.gov$ind_q)*100 # Reweighted

# Calculate violence neighbouring provinces
deaths.gov$NAME_1<-c("Al-Anbar","Al-Basrah","Al-Muthannia","Al-Qadisiyah",
                     "An-Najaf","Arbil","As-Sulaymaniyah","Babil","Baghdad",
                     "Dhi-Qar","Diyala","Dihok","Karbala'","At-Ta'mim",
                     "Maysan","Ninawa","Sala ad-Din","Wasit")
irq<-merge(gadm,deaths.gov)

# Spatial lag
k<-poly2nb(irq) # Create neighbour list
W<-nb2listw(k,style="B",zero.policy=TRUE) # Create weights 
irq$violence.k<-lag.listw(W,irq$violence.gov,zero.policy=TRUE) # Normal lag
irq$violence.k.rw<-lag.listw(W,irq$violence.gov.re,
                             zero.policy=TRUE) # Reweighted lag
df<-as.data.frame(irq)


#### Cluster violence level ####

# Individuals per cluster
require(plyr)
ind<-ddply(households,~cluster,summarise,
              ind.q=length(unique(id)))  

# Deaths per cluster
ind.d<-ddply(deaths,~cluster,summarise,
             violent.deaths=sum(col)) 

#  Reweighted deaths per cluster
ind.drw<-ddply(deaths,~cluster,summarise,
             violent.deaths.w=sum(col.c)) 

households<-merge(households,ind)
households<-merge(households,ind.d,all.x=TRUE)
households<-merge(households,ind.drw,all.x=TRUE)
households[is.na(households)]<-0

# Aggregate data to household level
d<-aggregate(cbind(col,norm.pre,norm.dum,norm.pre.c,norm.dum.c,col.c)~
               hh+cluster+gov,deaths,FUN=sum)
hh<-aggregate(hh_id~hh+cluster+gov+year_hh_formed+ind.q+
                violent.deaths+violent.deaths.w,households,FUN=max)
M<-merge(hh,d,by=c("hh","cluster","gov"),all.x=TRUE)

# Housekeeping
M[is.na(M)] <- 0
M<-M[order(M$gov,M$cluster,M$hh),]
M$id<-1:nrow(M)
M$hh_id<-NULL

# Calculate violence level
M$violence.cl<-(M$violent.deaths/M$ind.q)*100      # Normal count
M$violence.cl.rw<-(M$violent.deaths.w/M$ind.q)*100 # Reweighted count

# Merge household and provincial data
prov<-df[,c(10,35:39)]
M<-merge(M,prov,all.x=TRUE)

#### Split data into periods ####

# Subset data and do two melts
lh<-M[,c(14,1:10,15,16,18:21)]   # Normal death count
rh<-M[,c(14,1:3,11:13)]          # Reweighted death count

# First melt
require(reshape)
lh<-melt(lh,id=c("id","gov","cluster","hh",
                 "year_hh_formed","ind.q",
                 "col","violent.deaths","violent.deaths.w",
                 "violence.cl","violence.cl.rw",
                 "violence.gov","violence.gov.re","violence.k","violence.k.rw"))
lh$variable<-as.numeric(lh$variable=="norm.dum")
colnames(lh)<-c("id","gov","cluster","hh","year_hh_formed","ind.q",
                "col","violent.deaths","violent.deaths.w","violence.cl",
                "violence.cl.rw","violence.gov","violence.gov.re",
                "violence.k","violence.k.rw","period","norm.death")

# Second melt
rh<-melt(rh,id=c("id","gov","cluster","hh","col.c"))
rh$variable<-as.numeric(rh$variable=="norm.dum.c")
colnames(rh)<-c("id","gov","cluster","hh","col.c","period","norm.death.re")

# Merge the melted data
df<-merge(lh,rh)
df<-df[order(df$id),]

# Need to drop households formed >=2003 from before war period. 
df$d<-as.numeric(df$year_hh_formed>=2003 & df$period==0)
df<-df[df$d!=1,]

#### Create binary Diff-in-Diff indicator #### 
df$diff.gov<-as.numeric(df$violence.gov>=quantile(df$violence.gov,c(.68)))
df$diff.gov2<-as.numeric(df$violence.gov>=0.98)
df$diff.gov.re<-as.numeric(df$violence.gov.re>=quantile(df$violence.gov.re,
                                                        c(.68)))
df$diff.cluster<-as.numeric(df$violence.cl>=quantile(df$violence.cl,c(.68)))


#### Prepare data for regression #### 
source("code/functions.R")
df<-df[order(df$cluster,df$hh,df$period),]
cluster<-df$cluster
gov<-df$gov

# Outcome variables
norm.death<-df$norm.death             # Normal deaths in household
norm.death.re<-df$norm.death.re       # Normal deaths in household (reweighed)

# Explanatory variables
period<-stan(df$period)               # Period
violence.gov<-stan(df$violence.gov)   # Violence level governorate
violence.gov.re<-stan(df$violence.gov.re) # Violence level governorate reweighed
violence.cl<-stan(df$violence.cl)     # Violence level cluster
violence.k<-stan(df$violence.k)       # Violence level neighbouring governorates
diff.gov<-stan(df$diff.gov)           # Dummy violent governorates
diff.gov2<-stan(df$diff.gov2)         # Dummy violent governorates
diff.gov.re<-stan(df$diff.gov.re)     # Dummy violent governorates (reweighed)
diff.cluster<-stan(df$diff.cluster)   # Dummy violent clusters

## Libraries
library(MASS)
library(AER)
library(pscl) 
library(pROC)         

## FIN