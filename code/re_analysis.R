# Re-analysis of data for mortality study Iraq
# Account for sampling structure in governorates, 
# and permutations in certificate status. 
# This version:  01-02-2017
# First version: 09-12-2013

#### Clean data ####
# Similar to the replication file we stick as close as possible to 
# the original iPython script. 
# Deviations will be indicated with (*).
households<-read.csv("raw_data/hh_roster.csv",
                     header=TRUE,sep=",",row.names=NULL)
deaths<-read.csv("raw_data/hh_deaths.csv",header=TRUE,sep=",",row.names=NULL)

## Drop cluster 47 and 73 
deaths<-deaths[deaths$cluster!=47 & deaths$cluster!=73,] # -2 obs
households<-households[households$cluster!=47 & 
                         households$cluster!=73,] # - 203 obs.

## Recode missing observations on gov
# Based on cluster number
households[households$cluster==34,]$gov<-13
households[households$cluster==87,]$gov<-6
households[households$cluster==90,]$gov<-2
deaths[deaths$cluster==87,]$gov<-6
deaths[deaths$cluster==90,]$gov<-2

## Recode incorrectly coded clusters
# Governorate assigned based on cluster number
households[households$cluster==21 & households$hh==7,]$gov<-8
households[households$cluster==75 & households$hh==7,]$gov<-9
deaths[deaths$cluster==21 & deaths$hh==7,]$gov<-8
deaths[deaths$cluster==75 & deaths$hh==7,]$gov<-9

## Set NAs for mod to June
deaths$mod[is.na(deaths$mod)]<-6

## Recode cardiovascular violent death
deaths[deaths$cod=="cardiovascular" & deaths$war_death=="Y",]$war_death<-"N"

## Dummy variable for war & normal deaths
# NB: one obs. coded as D (row 100), unsure about meaning.
deaths$col<-as.numeric(deaths$war_death=="Y")  # War deaths
deaths$norm<-as.numeric(deaths$war_death=="N") # Normal deaths

## Create variable for date
deaths$day<-"01" 
deaths$date <- paste(deaths$yod, deaths$mod, deaths$day, sep="-")
deaths$date <- as.Date(deaths$date)
deaths$day<-NULL

## Normal deaths per period
# Note: 1 obs. in August 2011
deaths$norm.pre<-as.numeric(deaths$norm==1 & 
                              deaths$date<="2003-02-01") # Pre-war
deaths$norm.dum<-as.numeric(deaths$norm==1 & 
                              deaths$date>"2003-02-01")  # During war

## Data check
table(deaths$norm.pre) # 44
table(deaths$norm.dum) # 263
table(deaths$col)      # 75


#### Calculate exposure: households ####
# Following the procedure from the original code
## Tally person years observed for a row of hh roster for a given time period
hh<-households[,c(1:3,5,6)] 
hh$yob<-2011.5-(hh$age+0.5)
hh$hh_f<-hh$year_hh_formed+0.5

## Pre-war period
hh$t0<-2001
hh$t1<-2003.167

hh$t0<-apply(hh[,c(7,8)],1,max)
hh$t1<-apply(hh[,c(8,9)],1,max)

hh$exp0<-hh$t1-hh$yob
hh[hh$yob<=hh$t0,]$exp0<-hh[hh$yob<=hh$t0,]$t1-hh[hh$yob<=hh$t0,]$t0
hh[hh$yob>hh$t1,]$exp0<-0
sum(hh$exp0) # 14561.54 pre-war person-years

## War-period
hh$t0<-2003.167
hh$t1<-2011.5

hh$t0<-apply(hh[,c(7,8)],1,max)
hh$t1<-apply(hh[,c(8,9)],1,max)

hh$exp1<-hh$t1-hh$yob
hh[hh$yob<=hh$t0,]$exp1<-hh[hh$yob<=hh$t0,]$t1-hh[hh$yob<=hh$t0,]$t0
sum(hh$exp1) # 73282.96 during war person-years

hh.s<-hh[,c(2,1,3,10,11)]

#### Calculate exposure: deaths ####

## Create identifier and subset
deaths$id<-1:nrow(deaths)
d<-deaths[,c(1:3,5,6,13,19)]

## Before war
d$t0<-2001
d$t1<-2003.167
d$yod<-d$yod+(d$mod-0.5)/12
d$exp0<-d$t1-d$yod
d[d$yod>d$t1,]$exp0<-d[d$yod>d$t1,]$t1-d[d$yod>d$t1,]$t0
d[d$yod>=d$t0 & d$yod<=d$t1,]$exp0<-
  d[d$yod>=d$t0 & d$yod<=d$t1,]$yod-d[d$yod>=d$t0 & d$yod<=d$t1,]$t0
sum(d$exp0) # 773.7797 pre-war person-years

## During war
d$t0<-2003.167
d$t1<-2011.5
d$exp1<-d$t1-d$yod
d[d$yod<d$t0,]$exp1<-0
d[d$yod>d$t1,]$exp1<-d[d$yod>d$t1,]$t1-d[d$yod>d$t1,]$t0
d[d$yod>=d$t0 & d$yod<=d$t1,]$exp1<-
  d[d$yod>=d$t0 & d$yod<=d$t1,]$yod-d[d$yod>=d$t0 & d$yod<=d$t1,]$t0
sum(d$exp1) # 1353.554 during war person-years

## Merge with original data
deaths<-merge(deaths,d[,c(7,10,11)])

## Tally person-years
py0<-sum(hh$exp0)+sum(d$exp0) # Pre-war: 15335.32
py1<-sum(hh$exp1)+sum(d$exp1) # During: 74636.51

#### Accounting for death certificates ####
deaths$cert.lvl<-as.factor(deaths$death_cert)
deaths$cert.lvl<-as.numeric(deaths$cert.lvl)

# 1: Neonatal causes
# 2: able to see death cert 
# 3: death cert not available
# 4: told there is a death cert, but did not see

## Excluding no certificate
py0.nc<-sum(hh$exp0)+
  sum(deaths[deaths$cert.lvl!=3,]$exp0)               # Pre-war: 15269.77
py1.nc<-sum(hh$exp1)+
  sum(deaths[deaths$cert.lvl!=3,]$exp1)               # During: 74515.69

## Excluding certificate not shown
py0.ns<-sum(hh$exp0)+
  sum(deaths[deaths$cert.lvl!=4,]$exp0)               # Pre-war: 15196.59
py1.ns<-sum(hh$exp1)+
  sum(deaths[deaths$cert.lvl!=4,]$exp1)               # During: 74391.07

## Excluding no certificate and certificate not shown
py0.d<-sum(hh$exp0)+
  sum(deaths[deaths$cert.lvl!=3 & 
               deaths$cert.lvl!=4,]$exp0) # Pre-war: 15131.04
py1.d<-sum(hh$exp1)+
  sum(deaths[deaths$cert.lvl!=3 & 
               deaths$cert.lvl!=4,]$exp1) # During war: 74270.25

#### Calculate exposure per province ####
hh<-hh[,c(3,10:11)]
d<-d[,c(3,10:11)]
hh<-aggregate(.~gov,hh,FUN=sum)
d<-aggregate(.~gov,d,FUN=sum) 

colnames(d)<-c("gov","exp00","exp11")
exp<-merge(hh,d)
exp$exp0<-exp$exp0+exp$exp00
exp$exp1<-exp$exp1+exp$exp11
exp<-exp[,1:3]
rm(d,hh) # Housekeeping


#### Identify governorates ####
## Find out number of clusters and household members per governorate
require(plyr)
provinces<-ddply(households,~gov,summarise,
                 clusters=length(unique(cluster)))
table(provinces$clusters) 
# Different numbers compared to paper
# Due to removing clusters at beginning

length(unique(households$cluster)) # 98 unique clusters
sum(provinces$clusters)            # 98 clusters in total 

## Find out double-counted clusters
#households$d<-1
#double<-ddply(households,~cluster,summarise,
#                 gov_number=length(unique(gov)))
#double<-double[double$gov_number!=1,] # Double counting for 21 and 75
#a<-households[households$cluster==75 | households==21,]

## Miscoding in clusters:
# Household 7 in cluster 21 is coded for gov 7, should be 8
# Household 7 in cluster 75 is coded for gov 7, should be 9

## Calculate the proportion of the sample
provinces$cluster_pr<-provinces$clusters/sum(provinces$clusters)

# Next find out which gov. number corresponds with which specific gov. 
# Can use the data on household heads and household members for this. 
# Create an identifier for each individual & household
households$id<-1:nrow(households)
households$temp<-do.call(paste,c(households[c("gov","cluster","hh")],sep="-"))
households$hh_id<-as.numeric(c(as.factor(households$temp))) # Household
households$temp<-NULL

## Count heads of household & individuals
hhh<-ddply(households,~gov,summarise,
              hh_q=length(unique(hh_id)))
ind<-ddply(households,~gov,summarise,
              ind_q=length(unique(id)))

# 10467 individuals across 1959 households

## Merge the data
provinces<-merge(provinces,hhh)
provinces<-merge(provinces,ind)

## Governorate, Clusters, Household members (In data)
# Anbar, 7, 990; Basra, 8, 884; Muthanna, 1, 142; Qadisiyya, 4, 580;
# Najaf, 2, 200; Erbil, 9, 803; Sulaimaniya, 7, 663; Babylon, 3, 353;
# Baghdad, 23, 2347; Thi Qar, 3, 308 (2, 210); Diala, 5, 463; 
# Duhouk, 2, 284; Kerbela, 2, 221; Kirkuk, 2, 201; Maysan, 3, 311; 
# Ninevah, 13, 1298 (12,1193); Salah Al Deen, 3, 312; Wasit, 3, 310

# Based on this data we can deduce the corresponding names for each number
provinces$gov_name<-c("Anbar","Basra","Muthanna","Qadisiyya","Najaf","Erbil",
                      "Sulaimaniya","Babylon","Baghdad","Thi Qar","Diala",
                      "Duhouk","Kerbela","Kirkuk","Maysan","Ninevah",
                      "Salah Al Deen","Wasit")

## Include population estimates
provinces$pop<-c(1483359,2405434,683126,1077614,1221228,
                 1532081,1784853,1729666,6702538,1744398,
                 1371035,1072324,1013254,1325853,922890,
                 3106948,1337786,1150079)
provinces$pop_pr<-provinces$pop/sum(provinces$pop) 
provinces<-provinces[,c(6,1:5,7:8)]

## Calculate individual proportion for weights (Update 03-07-2015)
provinces$ind_pr<-provinces$ind_q/sum(provinces$ind_q)

## Calculate the cumulative population before and during war
population<-read.csv("raw_data/pop.csv",header=TRUE,sep=",",row.names=NULL)
pop<-aggregate(population~year,population,sum)
py<-sum(pop[8:14,2])+9/12*pop[7,2]+.5*pop[15,2]
py2<-sum(pop[5:6,2])+2/12*pop[7,2]
provinces$pop_dum<-provinces$pop_pr*py
provinces$pop_pre<-provinces$pop_pr*py2

## Calculate average household size per governorate
deaths$id<-1:nrow(deaths)
deaths$temp<-do.call(paste,c(deaths[c("gov","cluster","hh")],sep="-"))
deaths$hh_id<-as.numeric(c(as.factor(deaths$temp))) # Household
deaths$temp<-NULL
provinces$hh.ave<-provinces$ind_q/provinces$hh_q

## Calculated expected number of households per governorate
provinces$E.hh<-provinces$pop/provinces$hh.ave

rm(list=c("hhh","ind")) # Housekeeping

#### Reweighing the data ####
## Merge all data
deaths<-merge(deaths,provinces,all.x=TRUE)
deaths<-deaths[complete.cases(deaths),]

deaths$W<-deaths$pop_pr/deaths$ind_pr   # Individual based weight
deaths$W2<-(deaths$E.hh/deaths$hh_q)/(sum(provinces$E.hh)/sum(provinces$hh_q))

## Reweigh the deaths individual based
deaths$norm.pre.c<-deaths$norm.pre*deaths$W
deaths$norm.dum.c<-deaths$norm.dum*deaths$W
deaths$col.c<-deaths$col*deaths$W

## Reweigh the deaths household based
deaths$norm.pre.h<-deaths$norm.pre*deaths$W2
deaths$norm.dum.h<-deaths$norm.dum*deaths$W2
deaths$col.h<-deaths$col*deaths$W2

## Aggregate data
require(plyr)
deaths.gov<-ddply(deaths,.(gov),summarise,
                  norm=sum(norm),norm.pre=sum(norm.pre),norm.dum=sum(norm.dum),
                  norm.pre.c=sum(norm.pre.c),norm.dum.c=sum(norm.dum.c),
                  col=sum(col),col.c=sum(col.c),norm.pre.h=sum(norm.pre.h),
                  norm.dum.h=sum(norm.dum.h),col.h=sum(col.h))
deaths.gov<-merge(deaths.gov,exp)
deaths.gov<-merge(deaths.gov,provinces)

## Save for data for bootstrapping,regression models, and figures.
d.s<-deaths[,c("hh","cluster","gov","exp0","exp1",
               "col","norm","norm.pre","norm.dum","norm.pre",
               "norm.pre.c","norm.dum.c","col.c","norm.pre.h",
               "norm.dum.h","col.h","cert.lvl")]
bts<-rbind.fill(hh.s,d.s)
bts[is.na(bts)]<-0
provinces<-deaths.gov

save(list=c("provinces","bts","households","deaths","deaths.gov"),
     file="data/processed.Rdata")

#### Re-analysis: Central estimates ####

## Create vectors
what<-vector(mode="numeric", length=0)
exc<-vector(mode="numeric", length=0)
exc.r<-vector(mode="numeric", length=0)
exc.n<-vector(mode="numeric", length=0)
exc.r.n<-vector(mode="numeric", length=0)

## Excess deaths
what[1]<-"Re-analysis"

# Number of deaths
pre<-sum(deaths.gov$norm.pre)    # 44
during<-sum(deaths.gov$norm.dum) # 263
war<-sum(deaths.gov$col)         # 75

r.pre<-sum(deaths.gov$norm.pre.c)    # 41.3
r.during<-sum(deaths.gov$norm.dum.c) # 259.8
r.war<-sum(deaths.gov$col.c)         # 65.1

# Calculate death rates
cdr0<-1000*pre/py0               # Pre-war: 2.87
cdr1<-1000*(during+war)/py1      # During war: 4.53
exc.r[1]=cdr1-cdr0               # 1.66
exc.r.n[1]=1000*during/py1-cdr0  # 0.65

# Calculate excess deaths
exc[1]=exc.r[1]*py/1000                     # 405834.2
exc.n[1]=(1000*during/py1-cdr0)*py/1000     # 160079.4

## Re-analysis
what[2]<-"Re-analysis reweighed"
cdr0<-1000*r.pre/py0                    # Pre-war: 2.69
cdr1<-1000*(r.during+r.war)/py1         # During war: 4.35
exc.r[2]<-cdr1-cdr0                     # 1.66
exc.r.n[2]<-1000*r.during/py1-cdr0      # 0.79

exc[2]<-exc.r[2]*py/1000                # 406274
exc.n[2]<-exc.r.n[2]*py/1000            # 192996

## Certificate not available
what[3]<-"Certificate not available"
cdr0<-1000*sum(deaths[deaths$cert.lvl!=3,]$norm.pre)/py0.nc # 2.82
cdr1<-1000*(sum(deaths[deaths$cert.lvl!=3,]$norm.dum)+
                 sum(deaths[deaths$cert.lvl!=3,]$col))/py1.nc  # 4.15
exc.r[3]=cdr1-cdr0                                             # 1.33
exc[3]=exc.r[3]*py/1000                                        # 325455

# Non-violent deaths
cdr1<-1000*(sum(deaths[deaths$cert.lvl!=3,]$norm.dum))/py1.nc    # 3.19
exc.r.n[3]<-cdr1-cdr0                                            # 0.38
exc.n[3]<-exc.r.n[3]*py/1000                                     # 92430

## Certificate not available
what[4]<-"Certificate not available reweighed"

cdr0<-1000*sum(deaths[deaths$cert.lvl!=3,]$norm.pre.c)/py0.nc # 2.65
cdr1<-1000*(sum(deaths[deaths$cert.lvl!=3,]$norm.dum.c)+
                 sum(deaths[deaths$cert.lvl!=3,]$col.c))/py1.nc  # 4.00
exc.r[4]<-cdr1-cdr0                                              # 1.34
exc[4]<-exc.r[4]*py/1000                                         # 328054

# Non-violent deaths
cdr1<-1000*(sum(deaths[deaths$cert.lvl!=3,]$norm.dum.c))/py1.nc # 3.15
exc.r.n[4]<-cdr1-cdr0                                           # 0.51
exc.n[4]<-exc.r.n[4]*py/1000                                    # 123879

## Certificate not shown
what[5]<-"Certificate not shown"
cdr0<-1000*sum(deaths[deaths$cert.lvl!=4,]$norm.pre)/py0.ns    # 2.57
cdr1<-1000*(sum(deaths[deaths$cert.lvl!=4,]$norm.dum)+
                 sum(deaths[deaths$cert.lvl!=4,]$col))/py1.ns  # 3.72
exc.r[5]=cdr1-cdr0                                             # 1.16
exc[5]=exc.r[5]*py/1000                                        # 283009

# Non-violent deaths
cdr1<-1000*(sum(deaths[deaths$cert.lvl!=4,]$norm.dum))/py1.ns    # 2.98
exc.r.n[5]<-cdr1-cdr0                                            # 0.42
exc.n[5]<-exc.r.n[5]*py/1000                                     # 102194

## Certificate not shown
what[6]<-"Certificate not shown reweighed"
cdr0<-1000*sum(deaths[deaths$cert.lvl!=4,]$norm.pre.c)/py0.ns   # 2.47
cdr1<-1000*(sum(deaths[deaths$cert.lvl!=4,]$norm.dum.c)+
                 sum(deaths[deaths$cert.lvl!=4,]$col.c))/py1.ns # 3.71
exc.r[6]<-cdr1-cdr0                                             # 1.24
exc[6]<-exc.r[6]*py/1000                                        # 302844

# Non-violent deaths
cdr1<-1000*(sum(deaths[deaths$cert.lvl!=4,]$norm.dum.c))/py1.ns # 3.00
exc.r.n[6]<-cdr1-cdr0                                           # 0.53
exc.n[6]<-exc.r.n[6]*py/1000                                    # 130121

## Only deaths with certificates
what[7]<-"Certified only"
cdr0<-1000*sum(deaths[deaths$cert.lvl!=3 & 
                          deaths$cert.lvl!=4,]$norm.pre)/py0.d   # 2.51
cdr1<-1000*(sum(deaths[deaths$cert.lvl!=3 & 
                           deaths$cert.lvl!=4,]$norm.dum)+
                 sum(deaths[deaths$cert.lvl!=3 & 
                              deaths$cert.lvl!=4,]$col))/py1.d   # 3.34
exc.r[7]<-cdr1-cdr0                                              # 0.83
exc[7]<-exc.r[7]*py/1000                                         # 202441

# Non violent deaths
cdr1<-1000*(sum(deaths[deaths$cert.lvl!=3 & 
                           deaths$cert.lvl!=4,]$norm.dum))/py1.d # 2.65
exc.r.n[7]<-cdr1-cdr0                                            # 0.14
exc.n[7]<-exc.r.n[7]*py/1000                                     # 34503

## Certified only
what[8]<-"Certified only reweighed"
cdr0<-1000*sum(deaths[deaths$cert.lvl!=3 & 
                          deaths$cert.lvl!=4,]$norm.pre.c)/py0.d # 2.43
cdr1<-1000*(sum(deaths[deaths$cert.lvl!=3 &
                            deaths$cert.lvl!=4,]$norm.dum.c)+
                 sum(deaths[deaths$cert.lvl!=3 &
                              deaths$cert.lvl!=4,]$col.c))/py1.d # 3.34
exc.r[8]<-cdr1-cdr0                                              # 0.92
exc[8]<-exc.r[8]*py/1000                                         # 224408

# Non-violent deaths 
cdr1<-1000*(sum(deaths[deaths$cert.lvl!=3 & 
                           deaths$cert.lvl!=4,]$norm.dum.c))/py1.d# 2.67
exc.r.n[8]<-cdr1-cdr0                                             # 0.25
exc.n[8]<-exc.r.n[8]*py/1000                                      # 60883

## Table
table<-data.frame(specification=what,central_estimate=exc,excess_rate=exc.r,
                    non_violent_estimate=exc.n,non_violent_rate=exc.r.n)

save(list="table",file="output/reanalysis_central.RData") # Save data

## FIN