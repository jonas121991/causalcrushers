# -------------------------------------------------------------------------
# 5) Difference in Difference

rm(list = ls())

# Settings ----------------------------------------------------------------

# packages
library(dplyr) 
library(ggplot2) 
library(stargazer)

# Set seed for reproducibility
set.seed(111)

# plots
height <- 10
width <- height * 4 / 3

# confidence level
alpha <- 0.1

# data generation ---------------------------------------------------------

# Number of observations/individuals
n <- 100

# covariates
area <- runif(n,100,5000)
sun <- runif(n,400,600)
rain <- runif(n,600,850)
expe <- round(runif(n,1,35))

# instrument variable
vouch <- c(rep(1,n/2),rep(0,n/2)) # 50% of individuals receive a voucher

# treatment variable
treat <- c(rbinom(n=n/2, size=1, prob=0.50), rep(0,n/2)) # 50% of the individuals receiving a voucher decide to take the treatment

# self-selection variables
child <- round(runif(n,0,5))
educ <- round(runif(n,0,1))
dist <- runif(n,1,100)

# initialize
n_years <- 5
df <- data.frame(year = rep(2000:(2000 + n_years - 1), n),
                 household = rep(1:n, each = n_years))

# 5 repeated cross-sections
df <- df %>%
  mutate(
    # covariates
    area = rep(area, each = n_years),
    sun = runif(n * n_years,400,600),
    rain = runif(n * n_years,600,850),
    expe = c(t(expe + t(apply(matrix(1, n, n_years), 1, cumsum)))),
    
    # self-selection variables
    child = c(t(child + t(apply(matrix(round(runif(n * n_years,-0.75,0.75)), n, n_years), 1, cumsum)))),
    child = ifelse(child < 0, 0, child),
    educ = rep(educ, each = n_years),
    dist = rep(dist, each = n_years),
    
    # treatment
    vouch = rep(vouch, each = n_years),
    treat = ifelse(1 - 0.4*child + 2*educ - 0.02*dist + rnorm(1, mean=0, sd=0.5) > 0,1,0),
    treat = ifelse(vouch == 0, 0, treat),
    group = ifelse(treat == 1, "treatment", "control"),
    treat = ifelse(year < 2003, 0, treat),
    
    # error
    eps = rnorm(n * n_years, mean=0, sd=1),
    
    # outcome (harvest)
    harv = 400 + 0.05*area + 0.04*sun + 0.03*rain + 1.5*expe - 5*child + 100*educ + 300*treat + eps
    
  )

# part 5a) DiD graph ------------------------------------------------------

df_plot <-  df %>%
  group_by(year, group) %>%
  summarise(
    harv = mean(harv, na.rm = TRUE))

ggplot(df_plot, aes(x = year, y = harv, group = group, col = group)) +
  ylab("") + xlab("year") + 
  geom_line() +
  geom_point(size = 1) +
  geom_vline(xintercept = 2002, linetype = 5) +
  labs(title = "Harvest (in kg)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title=element_blank()) +
  scale_colour_manual(values = c("#1F407A", "#A8322D"),
                      labels = c("Households not receiving seed and fertilizer package (control group)",
                                 "Households receiving seed and fertilizer package (treatment group)")) +
  guides(color = guide_legend(ncol=1))

ggsave(filename = "figures/part5a.pdf", height = height, width = width, units = "cm")



# part 5b) estimation -----------------------------------------------------


df_est <- df %>%
  mutate(
    year_treat = ifelse(year < 2003, 0, 1),
    group_treat = ifelse(group == "treatment", 1, 0),
    dummy = year_treat * group_treat)

form <- "harv ~ -1 + factor(year) + factor(household) + dummy"
did_est <- lm(as.formula(form), data = df_est)
form <- "harv ~ -1 + factor(year) + factor(household) + dummy + sun + rain + child"
did_est_exo <- lm(as.formula(form), data = df_est)

stargazer(did_est, did_est_exo, omit = c("household","year"))#, type = "text")


# part 5c) event-study ----------------------------------------------------

# create dummies
df_est_event <- df %>%
  mutate(
    d00 = ifelse(year == 2000 & group == "treatment", 1, 0),
    d01 = ifelse(year == 2001 & group == "treatment", 1, 0),
    d02 = ifelse(year == 2002 & group == "treatment", 1, 0),
    d03 = ifelse(year == 2003 & group == "treatment", 1, 0),
    d04 = ifelse(year == 2004 & group == "treatment", 1, 0)
  )  

# estimate
form <- "harv ~ -1 + factor(year) + factor(household) + d00 + d01 + d03 + d04"
did_est_event <- lm(as.formula(form), data = df_est_event)
form <- "harv ~ -1 + factor(year) + factor(household) + d00 + d01 + d03 + d04 + sun + rain + child"
did_est_event_exo <- lm(as.formula(form), data = df_est_event)

stargazer(did_est, did_est_exo, did_est_event, did_est_event_exo, omit = c("household","year"))#, type ="text")

# plot
ci <- as.data.frame(confint(did_est_event, level = 1 - alpha))
df_plot <- data.frame("2000" = c(did_est_event$coefficients["d00"], t(ci["d00",])),
                      "2001" = c(did_est_event$coefficients["d01"], t(ci["d01",])),
                      "2002" = c(0, 0, 0),
                      "2003" = c(did_est_event$coefficients["d03"], t(ci["d03",])),
                      "2004" = c(did_est_event$coefficients["d04"], t(ci["d04",])))

df_plot <- as.data.frame(t(df_plot))
names(df_plot) <- c("mean", "cilb", "ciub")
df_plot$year <- as.Date(paste0(gsub("X", "", rownames(df_plot)), "-01-01"))

ggplot(df_plot, aes(x = year, y = mean, col = "")) +
  ylab("coefficient") + xlab("year") + 
  geom_point(size = 1) + 
  geom_line() +
  geom_errorbar(aes(ymin = cilb, ymax = ciub), width = 30, size = 0.5) +
  geom_vline(xintercept = as.Date("2002-01-01"), linetype = 5) +
  labs(title = "Treatement effects") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  scale_colour_manual(values = c("#72791C")) +
  theme(legend.position = "none",
        legend.title=element_blank())
ggsave(filename = "figures/part5c.pdf", height = height, width = width, units = "cm")

# part 5d) no common trend ------------------------------------------------

# 5 repeated cross-sections
df_d <- df %>%
  mutate(
    # rain is increasing
    rain1 = runif(n * n_years,800,1000),
    rain2 = runif(n * n_years,1000,1200),
    rain3 = runif(n * n_years,1200,1400),
    rain4 = runif(n * n_years,1400,1600),
    rain = ifelse(vouch == 0 & year > 2000, rain1, rain),
    rain = ifelse(vouch == 0 & year > 2001, rain2, rain),
    rain = ifelse(vouch == 0 & year > 2002, rain3, rain),
    rain = ifelse(vouch == 0 & year > 2003, rain4, rain),
    # sunshine is increasing
    sun1 = runif(n * n_years,500,800),
    sun2 = runif(n * n_years,800,1100),
    sun3 = runif(n * n_years,1100,1400),
    sun4 = runif(n * n_years,1400,1700),
    sun = ifelse(vouch == 0 & year > 2000, sun1, sun),
    sun = ifelse(vouch == 0 & year > 2001, sun2, sun),
    sun = ifelse(vouch == 0 & year > 2002, sun3, sun),
    sun = ifelse(vouch == 0 & year > 2003, sun4, sun),
    # outcome (harvest)    
    harv = 400 + 0.05*area + 0.04*sun + 0.03*rain + 1.5*expe - 5*child + 100*educ + 300*treat + eps
)

df_plot <- df_d %>%
  group_by(year, group) %>%
  summarise(harv = mean(harv, na.rm=T)) 

ggplot(df_plot, aes(x = year, y = harv, group = group, col = group)) +
  ylab("") + xlab("year") + 
  geom_line() +
  geom_point(size = 1) +
  geom_vline(xintercept = 2002, linetype = 5) +
  labs(title = "Harvest (in kg, common trend assumption violated)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title=element_blank()) +
  scale_colour_manual(values = c("#1F407A", "#A8322D"),
                      labels = c("Households not receiving seed and fertilizer package (control group)",
                                 "Households receiving seed and fertilizer package (treatment group)")) +
  guides(color = guide_legend(ncol=1))

ggsave(filename = "figures/part5d.pdf", height = height, width = width, units = "cm")

# create dummies
df_est_event_d <- df_d %>%
  mutate(
    d00 = ifelse(year == 2000 & group == "treatment", 1, 0),
    d01 = ifelse(year == 2001 & group == "treatment", 1, 0),
    d02 = ifelse(year == 2002 & group == "treatment", 1, 0),
    d03 = ifelse(year == 2003 & group == "treatment", 1, 0),
    d04 = ifelse(year == 2004 & group == "treatment", 1, 0)
  )  

# estimate
form <- "harv ~ -1 + factor(year) + factor(household) + d00 + d01 + d03 + d04"
did_est_event_d <- lm(as.formula(form), data = df_est_event_d)
form <- "harv ~ -1 + factor(year) + factor(household) + d00 + d01 + d03 + d04 + sun + rain + child"
did_est_event_d <- lm(as.formula(form), data = df_est_event_d)

# plot
ci <- as.data.frame(confint(did_est_event_d, level = 1 - alpha))
df_plot <- data.frame("2000" = c(did_est_event_d$coefficients["d00"], t(ci["d00",])),
                      "2001" = c(did_est_event_d$coefficients["d01"], t(ci["d01",])),
                      "2002" = c(0, 0, 0),
                      "2003" = c(did_est_event_d$coefficients["d03"], t(ci["d03",])),
                      "2004" = c(did_est_event_d$coefficients["d04"], t(ci["d04",])))


df_plot <- as.data.frame(t(df_plot))
names(df_plot) <- c("mean", "cilb", "ciub")
df_plot$year <- as.Date(paste0(gsub("X", "", rownames(df_plot)), "-01-01"))

ggplot(df_plot, aes(x = year, y = mean, col = "")) +
  ylab("coefficient") + xlab("year") + 
  geom_point(size = 1) + 
  geom_line() +
  geom_errorbar(aes(ymin = cilb, ymax = ciub), width = 30, size = 0.5) +
  geom_vline(xintercept = as.Date("2002-01-01"), linetype = 5) +
  labs(title = "Treatement effects (common trend assumption violated)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  scale_colour_manual(values = c("#72791C")) +
  theme(legend.position = "none",
        legend.title=element_blank())
ggsave(filename = "figures/part5d2.pdf", height = height, width = width, units = "cm")



