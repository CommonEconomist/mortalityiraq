## R-script for various bootstrap estimates, whole period
# NB - running the whole script will take about 12 hours.
# This version:  25-07-2016
# First version: 15-02-2014

#### Prepare data ####
load("data/processed.Rdata")

# Aggregate data to household level
dat<-aggregate(cbind(exp0,exp1,col,col.c,col.h,
                     norm,norm.pre,norm.dum,
                     norm.pre.c,norm.dum.c,
                     norm.pre.h,norm.dum.h)
               ~hh+cluster+gov,bts,FUN=sum)
dat<-dat[order(dat$cluster),]    # Sort dataframe
cluster<-c("gov","cluster","hh") # Sample hierarchy
source("code/functions.R")       # Bootstrap functions
# NB - 'set.seed' is repeated since the script seems to reset it during steps.

#### Excess deaths with governorate resampling ####
# Pre-war exposure
set.seed(2014);exp0.g<-replicate(1000,sum(resample(dat,cluster,c(T,T,T))$exp0))
# During war exposure
set.seed(2014);exp1.g<-replicate(1000,sum(resample(dat,cluster,c(T,T,T))$exp1))

# Pre-war deaths
set.seed(2014);gov0<-replicate(1000,sum(resample(dat,cluster,c(T,T,T))$norm.pre))
# During-war deaths
set.seed(2014);gov1<-replicate(1000,sum(resample(dat,cluster,c(T,T,T))$norm.dum))
# Violent deaths
set.seed(2014);gov2<-replicate(1000,sum(resample(dat,cluster,c(T,T,T))$col))

#### Excess deaths with reweighed data ####
# Pre-war exposure
set.seed(2014);exp0.r<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$exp0))
# During war exposure
set.seed(2014);exp1.r<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$exp1))

# Pre-war deaths
set.seed(2014);r0<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$norm.pre.c))
# During-war deaths
set.seed(2014);r1<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$norm.dum.c))
# Violent deaths
set.seed(2014);r2<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$col.c))

#### Excess deaths with reweighed data (households) ####
# Pre-war deaths
set.seed(2014);rh0<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$norm.pre.h))
# During-war deaths
set.seed(2014);rh1<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$norm.dum.h))
# Violent deaths
set.seed(2014);rh2<-replicate(1000,sum(resample(dat,cluster,c(F,T,T))$col.h))

#### Excess deaths with reweighed data and governorate resampling ####
# Pre-war deaths
set.seed(2014);f0<-replicate(1000,sum(resample(dat,cluster,c(T,T,T))$norm.pre.c))
# During-war deaths
set.seed(2014);f1<-replicate(1000,sum(resample(dat,cluster,c(T,T,T))$norm.dum.c))
# Violent deaths
set.seed(2014);f2<-replicate(1000,sum(resample(dat,cluster,c(T,T,T))$col.c))

#### Certified deaths only ####
bts2<-subset(bts,bts$cert.lvl!=3 & bts$cert.lvl!=4) 
dat2<-aggregate(cbind(exp0,exp1,norm.pre,norm.dum,col,col.c,norm.pre.c,norm.dum.c)
               ~hh+cluster+gov,bts2,FUN=sum)
dat2<-dat2[order(dat2$cluster),]

# Exposure
set.seed(2014);exp0.c<-replicate(1000,sum(resample(dat2,cluster,c(F,T,T))$exp0))
set.seed(2014);exp1.c<-replicate(1000,sum(resample(dat2,cluster,c(F,T,T))$exp1))

# Pre-war deaths
set.seed(2014);d0<-replicate(1000,sum(resample(dat2,cluster,c(F,T,T))$norm.pre))
# During war deaths
set.seed(2014);d1<-replicate(1000,sum(resample(dat2,cluster,c(F,T,T))$norm.dum))
# Violent deaths
set.seed(2014);d2<-replicate(1000,sum(resample(dat2,cluster,c(F,T,T))$col))

#### Certified deaths, reweighed and governorate resampling ####
# Exposure
set.seed(2014);exp0.cf<-replicate(1000,sum(resample(dat2,cluster,c(T,T,T))$exp0))
set.seed(2014);exp1.cf<-replicate(1000,sum(resample(dat2,cluster,c(T,T,T))$exp1))

# Pre-war deaths
set.seed(2014);cf0<-replicate(1000,sum(resample(dat2,cluster,
                                                c(T,T,T))$norm.pre.c))
# During war deaths
set.seed(2014);cf1<-replicate(1000,sum(resample(dat2,cluster,
                                                c(T,T,T))$norm.dum.c))
# Violent deaths
set.seed(2014);cf2<-replicate(1000,sum(resample(dat2,cluster,c(T,T,T))$col.c))

#### Excluding deaths with no certificate ####
bts2<-subset(bts,bts$cert.lvl!=3) 
dat2<-aggregate(cbind(exp0,exp1,norm.pre,norm.dum,col)
               ~hh+cluster+gov,bts2,FUN=sum)
dat2<-dat2[order(dat2$cluster),]

# Exposure
set.seed(2014);exp0.nc<-replicate(1000,sum(resample(dat2,cluster,c(F,T,T))$exp0))
set.seed(2014);exp1.nc<-replicate(1000,sum(resample(dat2,cluster,c(F,T,T))$exp1))

# Pre-war deaths
set.seed(2014);pre.nc<-replicate(1000,sum(resample(dat2,cluster,
                                                   c(F,T,T))$norm.pre))
# During war deaths
set.seed(2014);dum.nc<-replicate(1000,sum(resample(dat2,cluster,
                                                   c(F,T,T))$norm.dum))
# Violent deaths
set.seed(2014);col.nc<-replicate(1000,sum(resample(dat2,cluster,
                                                   c(F,T,T))$col))

#### Excluding deaths with certificate not shown ####
bts2<-subset(bts,bts$cert.lvl!=4) 
dat2<-aggregate(cbind(exp0,exp1,norm.pre,norm.dum,col)
               ~hh+cluster+gov,bts2,FUN=sum)
dat2<-dat2[order(dat2$cluster),]

# Exposure
set.seed(2014);exp0.ns<-replicate(1000,sum(resample(dat2,cluster,
                                                    c(F,T,T))$exp0))
set.seed(2014);exp1.ns<-replicate(1000,sum(resample(dat2,cluster,
                                                    c(F,T,T))$exp1))
# Pre-war deaths
set.seed(2014);pre.ns<-replicate(1000,sum(resample(dat2,cluster,
                                                   c(F,T,T))$norm.pre))
# During war deaths
set.seed(2014);dum.ns<-replicate(1000,sum(resample(dat2,cluster,
                                                   c(F,T,T))$norm.dum))
# Violent deaths
set.seed(2014);col.ns<-replicate(1000,sum(resample(dat2,cluster,c(F,T,T))$col))

#### Exluding deaths with no certificate (R) ####
bts2<-subset(bts,bts$cert.lvl!=3) 
dat2<-aggregate(cbind(exp0,exp1,norm.pre.c,norm.dum.c,col.c)
               ~hh+cluster+gov,bts2,FUN=sum)
dat2<-dat2[order(dat2$cluster),]

# Exposure
set.seed(2014);exp0.nc.r<-replicate(1000,sum(resample(dat2,cluster,
                                                      c(T,T,T))$exp0))
set.seed(2014);exp1.nc.r<-replicate(1000,sum(resample(dat2,cluster,
                                                      c(T,T,T))$exp1))
# Pre-war deaths
set.seed(2014);pre.nc.r<-replicate(1000,sum(resample(dat2,cluster,
                                                     c(T,T,T))$norm.pre.c))
# During war deaths
set.seed(2014);dum.nc.r<-replicate(1000,sum(resample(dat2,cluster,
                                                     c(T,T,T))$norm.dum.c))
# Violent deaths
set.seed(2014);col.nc.r<-replicate(1000,sum(resample(dat2,cluster,
                                                     c(T,T,T))$col.c))

#### Excluding deaths with certificate not shown (R) ####
bts2<-subset(bts,bts$cert.lvl!=4) 
dat2<-aggregate(cbind(exp0,exp1,norm.pre.c,norm.dum.c,col.c)
               ~hh+cluster+gov,bts2,FUN=sum)
dat2<-dat2[order(dat2$cluster),]

# Exposure
set.seed(2014);exp0.ns.r<-replicate(1000,sum(resample(dat2,cluster,
                                                      c(T,T,T))$exp0))
set.seed(2014);exp1.ns.r<-replicate(1000,sum(resample(dat2,cluster,
                                                      c(T,T,T))$exp1))
# Pre-war deaths
set.seed(2014);pre.ns.r<-replicate(1000,sum(resample(dat2,cluster,
                                                     c(T,T,T))$norm.pre.c))
# During war deaths
set.seed(2014);dum.ns.r<-replicate(1000,sum(resample(dat2,cluster,
                                                     c(T,T,T))$norm.dum.c))
# Violent deaths
set.seed(2014);col.ns.r<-replicate(1000,sum(resample(dat2,cluster,
                                                     c(T,T,T))$col.c))

save(list=ls(all=TRUE),file="output/bootstraps.RData") # Save output


#### Tables results ####
# NB - Order of panels has changed in paper

# Calculate total population
population<-read.csv("raw_data/pop.csv")
pop<-aggregate(population~year,population,sum)
py<-sum(pop[8:14,2])+9/12*pop[7,2]+1/2*pop[15,2]
probs<-(1+c(-1,1)*0.95)/2 # Uncertainty interval

what<-vector(mode="numeric", length=0)
e.lower<-vector(mode="numeric", length=0)
e.upper<-vector(mode="numeric", length=0)
r.lower<-vector(mode="numeric", length=0)
r.upper<-vector(mode="numeric", length=0)
P<-vector(mode="numeric", length=0)

## Panel A
# Re-analysis, bootstrap with gov. resampling
what[1]<-"Governorate resampling"
a<-1000*(gov1+gov2)/exp1.g-1000*gov0/exp0.g
b<-a*py/1000
r.lower[1]<-quantile(a,.025);r.upper[1]<-quantile(a,.975)
e.lower[1]<-quantile(b,.025);e.upper[1]<-quantile(b,.975)
P[1]<-sum(a>0)/length(a)*100

# Reweighed data
what[2]<-"Reweighed data"
a<-1000*(r1+r2)/exp1.r-1000*r0/exp0.r
b<-a*py/1000
r.lower[2]<-quantile(a,.025);r.upper[2]<-quantile(a,.975)
e.lower[2]<-quantile(b,.025);e.upper[2]<-quantile(b,.975)
P[2]<-sum(a>0)/length(a)*100

# Re-analysis, excess deaths with reweighted data (households)
#a<-1000*(rh1+rh2)/exp1.r-1000*rh0/exp0.r
#b<-a*py/1000
#quantile(a,probs=probs)
#quantile(b,probs=probs)
#sum(a>0)/length(a)*100

# Reweighed data and gov. resampling
what[3]<-"Reweighed data/gov. resampling"
a<-1000*(f1+f2)/exp1.cf-1000*f0/exp0.cf
b<-a*py/1000
r.lower[3]<-quantile(a,.025);r.upper[3]<-quantile(a,.975)
e.lower[3]<-quantile(b,.025);e.upper[3]<-quantile(b,.975)
P[3]<-sum(a>0)/length(a)*100

# Excluding deaths with no certificate
what[4]<-"Excl. certificate not available"
a<-1000*(dum.nc+col.nc)/exp1.nc-1000*pre.nc/exp0.nc
b<-a*py/1000
r.lower[4]<-quantile(a,.025);r.upper[4]<-quantile(a,.975)
e.lower[4]<-quantile(b,.025);e.upper[4]<-quantile(b,.975)
P[4]<-sum(a>0)/length(a)*100

# Excluding deaths with no certificate, reweighed, gov. resampling
what[5]<-"Excl. certificate not available/reweighed/gov. resampling"
a<-1000*(dum.nc.r+col.nc.r)/exp1.nc.r-1000*pre.nc.r/exp0.nc.r
b<-a*py/1000
r.lower[5]<-quantile(a,.025);r.upper[5]<-quantile(a,.975)
e.lower[5]<-quantile(b,.025);e.upper[5]<-quantile(b,.975)
P[5]<-sum(a>0)/length(a)*100

# Excluding deaths with certificate not shown
what[6]<-"Excl. certificate not shown"
a<-1000*(dum.ns+col.ns)/exp1.ns-1000*pre.ns/exp0.ns
b<-a*py/1000
r.lower[6]<-quantile(a,.025);r.upper[6]<-quantile(a,.975)
e.lower[6]<-quantile(b,.025);e.upper[6]<-quantile(b,.975)
P[6]<-sum(a>0)/length(a)*100

# Excluding deaths with certificate not shown, reweighed, gov. resampling
what[7]<-"Excl. certificate not shown/reweighed/gov. resampling"
a<-1000*(dum.ns.r+col.ns.r)/exp1.ns.r-1000*pre.ns.r/exp0.ns.r
b<-a*py/1000
r.lower[7]<-quantile(a,.025);r.upper[7]<-quantile(a,.975)
e.lower[7]<-quantile(b,.025);e.upper[7]<-quantile(b,.975)
P[7]<-sum(a>0)/length(a)*100

# Certified only
what[8]<-"Certified only"
a<-1000*(d1+d2)/exp1.c-1000*d0/exp0.c
b<-a*py/1000
r.lower[8]<-quantile(a,.025);r.upper[8]<-quantile(a,.975)
e.lower[8]<-quantile(b,.025);e.upper[8]<-quantile(b,.975)
P[8]<-sum(a>0)/length(a)*100

# Certified only, reweighed, gov. resampling
what[9]<-"Certified only/reweighed/gov. resampling"
a<-1000*(cf1+cf2)/exp1.cf-1000*cf0/exp0.cf
b<-a*py/1000
r.lower[9]<-quantile(a,.025);r.upper[9]<-quantile(a,.975)
e.lower[9]<-quantile(b,.025);e.upper[9]<-quantile(b,.975)
P[9]<-sum(a>0)/length(a)*100

PanelA<-data.frame(specification=what,lower.estimate=e.lower,
                   upper.estimate=e.upper,lower.rate=r.lower,
                   upper.rate=r.upper,P=P)

## Panel B
# Re-analysis, bootstrap with gov. resampling
what[1]<-"Governorate resampling"
a<-1000*gov1/exp1.g-1000*gov0/exp0.g
b<-a*py/1000
r.lower[1]<-quantile(a,.025);r.upper[1]<-quantile(a,.975)
e.lower[1]<-quantile(b,.025);e.upper[1]<-quantile(b,.975)
P[1]<-sum(a>0)/length(a)*100

# Reweighed data
what[2]<-"Reweighed data"
a<-1000*r1/exp1.r-1000*r0/exp0.r
b<-a*py/1000
r.lower[2]<-quantile(a,.025);r.upper[2]<-quantile(a,.975)
e.lower[2]<-quantile(b,.025);e.upper[2]<-quantile(b,.975)
P[2]<-sum(a>0)/length(a)*100

# Re-analysis, excess deaths with reweighted data (households)
#a<-1000*rh1/exp1.r-1000*rh0/exp0.r
#b<-a*py/1000
#quantile(a,probs=probs)
#quantile(b,probs=probs)
#sum(a>0)/length(a)*100

# Reweighed data and gov. resampling
what[3]<-"Reweighed data/gov. resampling"
a<-1000*f1/exp1.cf-1000*f0/exp0.cf
b<-a*py/1000
r.lower[3]<-quantile(a,.025);r.upper[3]<-quantile(a,.975)
e.lower[3]<-quantile(b,.025);e.upper[3]<-quantile(b,.975)
P[3]<-sum(a>0)/length(a)*100

# Excluding deaths with no certificate
what[4]<-"Excl. certificate not available"
a<-1000*dum.nc/exp1.nc-1000*pre.nc/exp0.nc
b<-a*py/1000
r.lower[4]<-quantile(a,.025);r.upper[4]<-quantile(a,.975)
e.lower[4]<-quantile(b,.025);e.upper[4]<-quantile(b,.975)
P[4]<-sum(a>0)/length(a)*100

# Excluding deaths with no certificate, reweighed, gov. resampling
what[5]<-"Excl. certificate not available/reweighed/gov. resampling"
a<-1000*dum.nc.r/exp1.nc.r-1000*pre.nc.r/exp0.nc.r
b<-a*py/1000
r.lower[5]<-quantile(a,.025);r.upper[5]<-quantile(a,.975)
e.lower[5]<-quantile(b,.025);e.upper[5]<-quantile(b,.975)
P[5]<-sum(a>0)/length(a)*100

# Excluding deaths with certificate not shown
what[6]<-"Excl. certificate not shown"
a<-1000*dum.ns/exp1.ns-1000*pre.ns/exp0.ns
b<-a*py/1000
r.lower[6]<-quantile(a,.025);r.upper[6]<-quantile(a,.975)
e.lower[6]<-quantile(b,.025);e.upper[6]<-quantile(b,.975)
P[6]<-sum(a>0)/length(a)*100

# Excluding deaths with certificate not shown, reweighed, gov. resampling
what[7]<-"Excl. certificate not shown/reweighed/gov. resampling"
a<-1000*dum.ns.r/exp1.ns.r-1000*pre.ns.r/exp0.ns.r
b<-a*py/1000
r.lower[7]<-quantile(a,.025);r.upper[7]<-quantile(a,.975)
e.lower[7]<-quantile(b,.025);e.upper[7]<-quantile(b,.975)
P[7]<-sum(a>0)/length(a)*100

# Certified only
what[8]<-"Certified only"
a<-1000*d1/exp1.c-1000*d0/exp0.c
b<-a*py/1000
r.lower[8]<-quantile(a,.025);r.upper[8]<-quantile(a,.975)
e.lower[8]<-quantile(b,.025);e.upper[8]<-quantile(b,.975)
P[8]<-sum(a>0)/length(a)*100

# Certified only, reweighed, gov. resampling
what[9]<-"Certified only/reweighed/gov. resampling"
a<-1000*cf1/exp1.cf-1000*cf0/exp0.cf
b<-a*py/1000
r.lower[9]<-quantile(a,.025);r.upper[9]<-quantile(a,.975)
e.lower[9]<-quantile(b,.025);e.upper[9]<-quantile(b,.975)
P[9]<-sum(a>0)/length(a)*100

PanelB<-data.frame(specification=what,lower.estimate=e.lower,
                   upper.estimate=e.upper,lower.rate=r.lower,
                   upper.rate=r.upper,P=P)

save(list=c("PanelA","PanelB"),file="output/bootstrap_estimates.RData")


## FIN