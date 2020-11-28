## Functions

##### Bootstrap resampling ####
# Code taken from: http://biostatmatt.com/archives/2125
# NB - the code calls itself

# Bootstrap resample function
resample <- function(dat, cluster, replace) {
  
  # exit early for trivial data
  if(nrow(dat) == 1 || all(replace==FALSE))
    return(dat)
  
  # clustering factor
  cft <- dat[[cluster[1]]]
  
  # sample the clustering factor
  cls <- sample(unique(cft), replace=replace[1])
  
  # subset on the sampled clustering factors
  sub <- lapply(cls, function(b) subset(dat, cft==b))
  
  # sample lower levels of hierarchy (if any)
  if(length(cluster) > 1)
    sub <- lapply(sub, resample, cluster=cluster[-1], replace=replace[-1])
  
  # join and return samples
  do.call(rbind, sub)
}


# Robust clustered standard errors
# Source:
# http://people.su.se/~ma/mcluster.R
# http://people.su.se/~ma/clustering.pdf

# Script written by Mahmood Arai
clse <- 
  function(fm, dfcw, cluster){
    # R-codes (www.r-project.org) for computing
    # clustered-standard errors. Mahmood Arai, Jan 26, 2008.
    
    # The arguments of the function are:
    # fitted model, cluster1 and cluster2
    # You need to install libraries `sandwich' and `lmtest'
    
    # reweighting the var-cov matrix for the within model
    library(sandwich);library(lmtest)
    M <- length(unique(cluster))   
    N <- length(cluster)           
    K <- fm$rank                        
    dfc <- (M/(M-1))*((N-1)/(N-K))  
    uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)*dfcw
    coeftest(fm, vcovCL) }

#### Rescale variables ####
stan<-function(x){ (x-mean(x))/(2*sd(x)) }

## FIN
