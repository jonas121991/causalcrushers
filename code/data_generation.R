#### Data Generation
rm(list = ls())
cat("\014")


# Settings ----------------------------------------------------------------

#packages
library(extraDistr) # for random distribution
library(dplyr) # for table editing
library(xtable) # for latex output
library(ggplot2) # for nice plots
library(ggpubr) # for putting multiple plots together in one window
# Set seed for reproducibility
set.seed(111)


#  Variable generation ----------------------------------------------------

# Number of observations/individuals
n <- 10000

# covariates
area <- runif(n,100,5000)
sun <- runif(n,400,600)
rain <- runif(n,600,850)
exp <- round(runif(n,1,35))

# instrument variable
vouch <- c(rep(1,n/2),rep(0,n/2)) # 50% of individuals receive a voucher

# treatment variable
treat <- c(rbinom(n=n/2, size=1, prob=0.50), rep(0,n/2)) # 50% of the individuals receiving a voucher decide to take the treatment
  
# self-selection variables
child <- round(runif(n,0,5))
educ <- round(runif(n,0,1))
dist <- runif(n,1,100)


# data table for random assignment ----------------------------------------

# create table
data_table <- data.frame(area,sun,rain,exp,dist,child,educ,vouch,treat)

# output variable
data_table <- data_table %>%
  mutate(harv = 400 + 0.05*area + 0.04*sun + 0.03*rain + 5*exp - 5*child + 10*educ + 100*treat + rnorm(1, mean=0, sd=1), .before = area)


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


# Create self-selection case ----------------------------------------------

# Redefine table
data_table_sel <- data_table

### define treatment according to self-selection variables
data_table_sel <- data_table_sel %>%
  mutate(treat = ifelse(1 - 0.4*child + 2*educ - 0.02*dist + rnorm(1, mean=0, sd=0.5) > 0,1,0))

# those that did not receive a voucher still do not take the treatment
data_table_sel <- data_table_sel %>%
  mutate(treat = ifelse(vouch == 0,0,treat))

# outcome variable  
data_table_sel <- data_table_sel %>%
  mutate(harv = 400 + 0.05*area + 0.04*sun + 0.03*rain + 1.5*exp - 5*child + 100*educ + 300*treat + rnorm(1, mean=0, sd=1))

## create summary statistics
# treated
data_table_sel_treat <- data_table_sel %>% filter(treat == 1)
means_sel_treat <- data_table_sel_treat %>% summarise_if(is.numeric, mean, na.rm = TRUE)
obs_sel_treat <- dim(data_table_sel_treat)[1]

# untreated
data_table_sel_un <- data_table_sel %>% filter(treat == 0)
means_sel_un <- data_table_sel_un %>% summarise_if(is.numeric, mean, na.rm = TRUE)
obs_sel_un <- dim(data_table_sel_un)[1]

save(data_table,data_table_sel, file = "data/data.rda")


# Linear Regression -------------------------------------------------------

#regression for random case
fit_random <- lm(harv ~ area + sun + rain + exp + child + treat, data = data_table)
summary(fit_random)
xtable(summary(fit_random)) # replace Pr... with: Pr({$>$}{$|$}t$|$) in the code

# regression for self-selection case
fit_sel <- lm(harv ~ area + sun + rain + exp + child + treat, data = data_table_sel)
summary(fit_sel)
xtable(summary(fit_sel)) # replace Pr... with: Pr({$>$}{$|$}t$|$) in the code


# T-test for balanced covariates ------------------------------------------

# p values for random case
p_val_rand <- NA
for (i in 2:length(data_table_treat)){
  res <- t.test(data_table_treat[,i], data_table_un[,i] , alternative = "two.sided", var.equal = FALSE)
  p_val_rand[i] <- res$p.value
}


# p-values for self-selection case
p_val_sel <- NA
for (i in 2:length(data_table_sel_treat)){
  res <- t.test(data_table_sel_treat[,i], data_table_sel_un[,i] , alternative = "two.sided", var.equal = FALSE)
  p_val_sel[i] <- res$p.value
}



# Create summary statistics table ----------------------------------------------------
Obs <- c(obs,obs_treat,obs_un,NA,obs_sel_treat,obs_sel_un,NA)

sum_table <- t(rbind(means,means_treat,means_un,p_val_rand,means_sel_treat,means_sel_un,p_val_sel))
sum_table <- sum_table[1:(dim(sum_table)[1]-2),]
sum_table <- rbind(sum_table,Obs)
sum_table <- data.frame(sum_table)
colnames(sum_table) <- c("Total", "Treatment","No Treatment", "P-Value", "Treatment", "No Treatment", "P-Value")

# generate latex output
addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("& &\\multicolumn{3}{c}{Random Assignment} & \\multicolumn{3}{c}{Self-Selection}\\\\\n",
                      "& Total & Treatment & No Treatment & P-Value & Treatment & No Treatment & P-Value\\\\\n")
mdat <- matrix(c(rep(2,(8*8)),rep(0,8)),nrow = 9, ncol=8, byrow=TRUE)
print(xtable(sum_table, align = c("l","r","r","r","r","r","r","r"), digits = mdat),
      add.to.row = addtorow, include.colnames = FALSE)


# Density plot ------------------------------------------------------------
par(mfrow=c(2,1))
# Histogram with density plot for random case
a <- ggplot(data_table, aes(x=harv)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = seq(000,1200,200), limits = c(000,1200)) +
  scale_y_continuous(breaks = seq(0,0.005,0.001), limits = c(0,0.005)) + 
  ggtitle("Random Case")


# Histogram with density plot for self-selection case
b <- ggplot(data_table_sel, aes(x=harv)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = seq(000,1200,200), limits = c(000,1200)) +
  scale_y_continuous(breaks = seq(0,0.005,0.001), limits = c(0,0.005)) + 
  ggtitle("Self-Selection Case")

ggarrange(a, b, ncol = 1, nrow = 2, heights = c(2,2), align = "hv")
ggsave("figures/density_total.pdf",width = 20, height = 14, units = "cm")


data_table$treat <- factor(data_table$treat, 
                           levels=c(1,0),
                           labels=c("Treated","Not Treated"))
data_table_sel$treat <- factor(data_table_sel$treat, 
                           levels=c(1,0),
                           labels=c("Treated","Not Treated"))

# Histogram with density plot for random case, color by groups
c <- ggplot(data_table, aes(x=harv, color=treat, fill=treat)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.2) +
  scale_x_continuous(breaks = seq(000,1400,200), limits = c(000,1400)) +
  scale_y_continuous(breaks = seq(0,0.005,0.001), limits = c(0,0.005)) + 
  ggtitle("Random Case")

# Histogram with density plot for self-selection case, color by groups
d <- ggplot(data_table_sel, aes(x=harv, color=treat, fill=treat)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.2) +
  scale_x_continuous(breaks = seq(000,1400,200), limits = c(000,1400)) +
  scale_y_continuous(breaks = seq(0,0.005,0.001), limits = c(0,0.005)) + 
  ggtitle("Self-Selection Case")

ggarrange(c, d, ncol = 1, nrow = 2, heights = c(2,2), align = "hv")
ggsave("figures/density_goups.pdf",width = 20, height = 14, units = "cm")
