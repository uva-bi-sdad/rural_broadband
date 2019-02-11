# Regression model for subscription rate by Census tract
# Compare alternate approaches to the linear model (e.g. GLM, spatial CAR)


library(rgeos)
library(ggplot2)
library(tigris)
library(dplyr)
library(tidycensus)
library(xml2)
library(acs)
library(viridis)
library(rgdal)
library(ggfortify)
library(car)
library(broom)
library(stargazer)

options(scipen = 999)

tract_data <- read.csv("data/broadband_acs_by_census_tract_2015.csv")

lm.out <- lm(subscription_continuous ~ available + rural + hs_or_less + poverty + age_65_older +
               hispanic + black + density + family + foreign,
             data=tract_data)
summary(lm.out)

# ----------------------------------------------------
# effects plot
# ----------------------------------------------------

# Figure B2:
effect1 <- lm.out$coefficients[2:11]*100
effect2 <- c(0.25,-0.11,-0.50,-0.38,-0.15,-0.06,0,0.005,0.03,0.34)*100


label <- c("Broadband Availability at 25 Mbps","Rural Neighborhood",
           "Share of Population over Age 25 with\n No More than a High School Diploma","Poverty Rate",
           "Share of Population Aged 65 or Older","Hispanic Share of Population",
           "Black Share of Population","Population Density Per Mile",
           "Families Share of Population","Foreign Born Share of Population")
library(reshape2)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
lm_dat <- melt(data.frame(label,effect2,effect1))
lm_dat$label <- ordered(lm_dat$label,levels=rev(label))
# add and organize data to git (FCC/ACS); created Brookings data frame by Census tract (include metadata)
fig_b2 <- ggplot(data=lm_dat, aes(x=label, y=value)) +
  geom_bar(stat='identity',position="dodge",width=0.7,aes(fill=variable)) +
  coord_flip() +
  theme_bw() +
  scale_fill_manual("Effect Size",values=cbPalette[2:3],labels=c("Brookings","Replication")) +
  xlab("") + ylab("") +
  scale_y_continuous(breaks=c(-40,-20,0,20,40),labels=c("-40%","-20%","0%","20%","40%")) +
  ggtitle("Estimated change in neighborhood broadband subscription rate per\n 1 percentage point increase in the variable, 2015.")
fig_b2

# ----------------------------------------------------
# linear model diagnostics
# ----------------------------------------------------

autoplot(lm.out)
# Residuals vs. fitted: A horizontal line with no pattern indicates a linear reationship. --> All-around fail.
# Normal Q-Q: Residual points following the dashed line indicate normal residual distribution. --> Fail at extremes.
# Scale-Location: Horizontal line with equal point spread indicates homoskedasticity. --> Definitely heteroskedastic. 
# Residuals vs leverage: Definitely an influential point. 

# Same as above but more+separate plots:
plot(lm.out, 1) # This suggests that we cannot assume a linear relationship between the predictors and outcome.
plot(lm.out, 2) # The normal probability plot of residuals does not follow a straight line at the extremes.
plot(lm.out, 3) # Residuals are not spread equally along the ranges of predictors. The variance of residual errors is non-constant (heteroskedasticity).
plot(lm.out, 4) # Influential values,  4/(72219-10-1)=0.00005539552 threshold
plot(lm.out, 5) # Several data points have high leverage (extreme predictor values) and exceed 2SD.  

# Identify high leverage points
# A value of hatsq statistic above 2*(10+1)/72219=0.000304629 indicates an observation with high leverage.
augmodel <- augment(lm.out)
augmodel %>% top_n(3, wt = .cooksd)

# What happens if we remove the weird outlier? Nothing, not worth it. 
#nomiss[23023, ] # This is the weird outlier on every plot

test <- tract_data[-23023, ]
lm.outtest <- lm(subscription_continuous ~ available + rural + hs_or_less + poverty + age_65_older +
                   hispanic + black + density + family + foreign,
                 data = test)
summary(lm.outtest)
summary(lm.out)
stargazer(lm.out, lm.outtest, digits = 2, type = "text",  align = TRUE, initial.zero = TRUE, no.space = TRUE, notes.align = "l", star.cutoffs = c(0.05, 0.01, 0.001))
autoplot(lm.outtest)



