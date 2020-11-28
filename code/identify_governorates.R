# This version:  07-12-2014
# First version: 09-12-2013
# R script for cleaning data and identifying governorates

## Libraries
library(plyr)

## Load data
deaths<-read.csv("raw_data/hh_deaths.csv",header=TRUE,sep=",",row.names=NULL)
households<-read.csv("raw_data/hh_roster.csv",header=TRUE,sep=",",row.names=NULL)
population<-read.csv("raw_data/pop.csv",header=TRUE,sep=",",row.names=NULL)

#### Clean data according to original ####

# Drop cluster 47 and 73 
deaths<-deaths[deaths$cluster!=47 & deaths$cluster!=73,] # -2 obs
households<-households[households$cluster!=47 & households$cluster!=73,] # - 203 obs.

# Recode missing observations on gov based on cluster
households[households$cluster==34,]$gov<-13
households[households$cluster==87,]$gov<-6
households[households$cluster==90,]$gov<-2
deaths[deaths$cluster==87,]$gov<-6
deaths[deaths$cluster==90,]$gov<-2

# Set NAs for mod to June
sum(is.na(deaths$mod)) # 26 obs.
deaths$mod[is.na(deaths$mod)]<-6

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
deaths$date <- as.Date(deaths$date)
deaths$day<-NULL

# Normal deaths per period
deaths$norm.pre<-as.numeric(deaths$norm==1 & 
                              deaths$date<="2003-02-01") # pre-war
deaths$norm.dum<-as.numeric(deaths$norm==1 & 
                              deaths$date>"2003-02-01") # during war
# NB: 1 obs. in August 2011

table(deaths$norm.pre) # 44
table(deaths$norm.dum) # 263
table(deaths$col)      # 75

#### Identify governorates ####
# Based on data given in table 1 

## Find out number of clusters per governorate
provinces<-ddply(households,~gov,summarise,
                 clusters=length(unique(cluster))) # 18 govs. 

table(provinces$clusters) # Different numbers compared to paper

length(unique(households$cluster)) # 98 unique clusters
sum(provinces$clusters) # 100 clusters in total

# Calculate the proportion of the sample
provinces$cluster_pr<-provinces$clusters/sum(provinces$clusters)

# Next find out which gov. number corresponds with which specific gov. 
# Can use the data on household heads and household members for this. 
# Create an identifier for each individual & household
households$id<-1:nrow(households)
households$temp<-do.call(paste,c(households[c("gov","cluster","hh")],sep="-"))
households$hh_id<-as.numeric(c(as.factor(households$temp))) # Household
households$temp<-NULL

# Count heads of household & individuals
hhh<-ddply(households,~gov,summarise,
              hh_q=length(unique(hh_id)))
ind<-ddply(households,~gov,summarise,
              ind_q=length(unique(id)))

# Merge the data
provinces<-merge(provinces,hhh)
provinces<-merge(provinces,ind)

# Based on this data we can deduce the corresponding names for each number
provinces$gov_name<-c("Anbar","Basra","Muthanna","Qadisiyya","Najaf","Erbil",
                      "Sulaimaniya","Babylon","Baghdad","Thi Qar","Diala",
                      "Duhouk","Kerbela","Kirkuk","Maysan","Ninevah",
                      "Salah Al Deen","Wasit")

# Can include population estimates
provinces$pop<-c(1483359,2405434,683126,1077614,1221228,
                 1532081,1784853,1729666,6702538,1744398,
                 1371035,1072324,1013254,1325853,922890,
                 3106948,1337786,1150079)

# Calculate population proportion
provinces$pop_pr<-provinces$pop/sum(provinces$pop) 

rm(list=c("hhh","ind")) # housekeeping

# Need to calculate the population before and during war
# Note these are cumulatives
pop<-aggregate(population~year,population,sum)
py<-sum(pop[8:14,2])+9/12*pop[7,2]+.5*pop[15,2]
py2<-sum(pop[5:6,2])+2/12*pop[7,2]
provinces$pop_dum<-provinces$pop_pr*py
provinces$pop_pre<-provinces$pop_pr*py2

## Add mortality data
m<-aggregate(cbind(col,norm.pre,norm.dum)~gov,deaths,sum)
provinces<-merge(provinces,m)

## Save data
save("provinces",file="tidy_data/provinces.Rdata")


## FIN