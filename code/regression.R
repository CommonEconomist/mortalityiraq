## Model estimation
source("code/prep_regression.R")

#### Binary indicator ####
# Main model, Poisson estimation 
m1<-glm(norm.death~period+diff.gov+diff.gov*period,
        family="poisson"(link="log"))
summary(m1) 
clse(m1,1,cluster)
auc(as.numeric(norm.death>=1),fitted(m1))

# Main model Poisson estimation with FE 
m2<-glm(norm.death~period+diff.gov+diff.gov*period+factor(gov),
        family="poisson"(link="log"))
summary(m2) 
clse(m2,1,cluster)
auc(as.numeric(norm.death>=1),fitted(m2))

# Main model NB estimation 
m3<-glm.nb(norm.death~period+diff.gov+diff.gov*period)
summary(m3)
clse(m3,1,cluster)
auc(as.numeric(norm.death>=1),fitted(m3))

# Model with reweighted data, quasipoisson estimation 
m4<-glm(norm.death.re~period+diff.gov.re+diff.gov.re*period,
        family="quasipoisson"(link="log"))
summary(m4)
clse(m4,1,cluster)
auc(as.numeric(norm.death>=1),fitted(m4))

# Main model incl. violent neighbourhood measure Poisson
m5<-glm(norm.death~period+diff.gov+violence.k+diff.gov*period,
        family="poisson"(link="log"))
summary(m5)
clse(m5,1,cluster)
auc(as.numeric(norm.death>=1),fitted(m5))

# Model with violence at cluster level Poisson 
m6<-glm(norm.death~period+diff.cluster+diff.cluster*period,
        family="poisson"(link="log"))
summary(m6) 
clse(m6,1,cluster)
auc(as.numeric(norm.death>=1),fitted(m6))

#### Continuous indicator ####
# Main model, Poisson estimation 
m1<-glm(norm.death~period+violence.gov+violence.gov*period,
        family="poisson"(link="log"))
summary(m1) 
clse(m1,1,cluster)
auc(as.numeric(norm.death>=1),fitted(m1))

# Main model Poisson estimation with FE (Table 3, col.2)
m2<-glm(norm.death~period+violence.gov+violence.gov*period+factor(gov),
        family="poisson"(link="log"))
summary(m2) 
clse(m2,1,cluster)
auc(as.numeric(norm.death>=1),fitted(m2))

# Main model NB estimation 
m3<-glm.nb(norm.death~period+violence.gov+violence.gov*period)
summary(m3)
clse(m3,1,cluster)
auc(as.numeric(norm.death>=1),fitted(m3))

# Model with reweighted data, quasipoisson estimation 
m4<-glm(norm.death.re~period+violence.gov.re+violence.gov.re*period,
        family="quasipoisson"(link="log"))
summary(m4)
clse(m4,1,cluster)
auc(as.numeric(norm.death>=1),fitted(m4))

# Main model incl. violent neighbourhood measure Poisson 
m5<-glm(norm.death~period+violence.gov+violence.k+violence.gov*period,
        family="poisson"(link="log"))
summary(m5)
clse(m5,1,cluster)
auc(as.numeric(norm.death>=1),fitted(m5))

# Model with violence at cluster level Poisson 
m6<-glm(norm.death~period+violence.cl+violence.cl*period,
        family="poisson"(link="log"))
summary(m6) 
clse(m6,1,cluster)
auc(as.numeric(df$norm.death>=1),fitted(m6))

## FIN