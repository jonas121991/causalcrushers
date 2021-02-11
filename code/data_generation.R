# Data Generation
rm(list = ls())
cat("\014")

#packages
library(extraDistr)
library(sn)
library(dplyr)

# Set seed for reproducibility
set.seed(111)



#  Variable generation ----------------------------------------------------

n <- 10000

# covariates
harv <- runif(n,30,800)
area <- runif(n,100,5000)
sun <- runif(n,400,600)
rain <- runif(n,600,850)
exp <- round(runif(n,1,35))

# instrument variable
vouch <- c(rep(1,n/2),rep(0,n/2)) # 50% of individuals receive a voucher

# treatment variable
treat <- c(rbinom(n=n/2, size=1, prob=0.80), rep(0,n/2)) # 80% of the individuals receiving a voucher decide to take the treatment
  
# self-selection variables in random case
child <- round(runif(n,0,5))
educ <- round(runif(n,0,1))
dist <- runif(n,1,100)

# self-selection variables in self-selection case
child_sel <- rep(NA, n)
educ_sel <- rep(NA, n)
dist_sel <- rep(NA, n)

#x= round(rbeta(1,2,5)*5) # right skewed: more small values
#z= round(rbeta(1,5,2)*5) # left skewed: more large values

for (i in 1:length(treat)){
  if (treat[i] == 1){ # people who chose to take the treatment...
    child_sel[i] <- round(rbeta(1,2,5)*5) # have less  children
    educ_sel[i] <- round(rbeta(1,5,2)*1) # have higher education
    dist_sel[i] <- rbeta(1,2,5)*100 # have lower distance
  }else{
    child_sel[i] <- round(runif(1,0,5))
    educ_sel[i] <- round(runif(1,0,1))
    dist_sel[i] <- runif(1,1,100)    
  }
}

# treat_sel = ...

# data table for random assignment ----------------------------------------

data_table <- data.frame(harv,area,sun,rain,child,exp,educ,dist,vouch,treat)

## summary statistics
# overall
means <- data_table %>% summarise_if(is.numeric, mean, na.rm = TRUE)
obs <- dim(data_table)[1]

# treated
data_table_treat <- data_table %>% filter(treat == 1)
means_treat <- data_table_treat %>% summarise_if(is.numeric, mean, na.rm = TRUE)
obs_treat <- dim(data_table_treat)[1]

# untreated
data_table_un <- data_table %>% filter(treat == 0)
means_un <- data_table_un %>% summarise_if(is.numeric, mean, na.rm = TRUE)
obs_un <- dim(data_table_un)[1]


# data table for self-selection -------------------------------------------

data_table_sel <- data.frame(harv,area,sun,rain,child_sel,exp,educ_sel,dist_sel,vouch,treat)

## create summary statistics
# treated
data_table_sel_treat <- data_table_sel %>% filter(treat == 1)
means_sel_treat <- data_table_sel_treat %>% summarise_if(is.numeric, mean, na.rm = TRUE)
obs_sel_treat <- dim(data_table_sel_treat)[1]

# untreated
data_table_sel_un <- data_table_sel %>% filter(treat == 0)
means_sel_un <- data_table_sel_un %>% summarise_if(is.numeric, mean, na.rm = TRUE)
obs_sel_un <- dim(data_table_sel_un)[1]
