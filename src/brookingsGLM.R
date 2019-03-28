library(dplyr)
library(stargazer)
library(ordinal) 
library(sure)
library(ggplot2)


#
# Read in & prepare data --------------------------------------------------------------------------------------------------------------
#

netdata <- read.csv("data/acs_property.csv")
head(netdata)

# Create a DF with no missings on model variables
nomiss <- netdata %>% select(GEOID, STATEFP, COUNTYFP, NAME, name2, available, subscription_continuous,
                              hs_or_less, age_65_older, hispanic, black, foreign, rural, poverty, density) %>% 
                      filter(!is.na(available), !is.na(subscription_continuous), !is.na(hs_or_less), !is.na(age_65_older),
                             !is.na(hispanic), !is.na(black), !is.na(foreign), !is.na(rural), !is.na(poverty), !is.na(density))


#
# Fit ordinal and OLS --------------------------------------------------------------------------------------------------------------
#

# From Josh: turn subscription quintiles into a continuous response variable by taking the center of each bin
# nomiss$subscription_continuous1 <- pmax(0, nomiss$subscription/5-0.1)

# Rescale variables
nomiss1 <- nomiss
nomiss1$available <- scale(nomiss$available, center = TRUE, scale = TRUE)
nomiss1$hs_or_less <- scale(nomiss$hs_or_less, center = TRUE, scale = TRUE)
nomiss1$age_65_older <- scale(nomiss$age_65_older, center = TRUE, scale = TRUE)
nomiss1$hispanic <- scale(nomiss$hispanic, center = TRUE, scale = TRUE)
nomiss1$black <- scale(nomiss$black, center = TRUE, scale = TRUE)
nomiss1$foreign <- scale(nomiss$foreign, center = TRUE, scale = TRUE)
nomiss1$rural <- scale(nomiss$rural, center = TRUE, scale = TRUE)
nomiss1$poverty <- scale(nomiss$poverty, center = TRUE, scale = TRUE)
nomiss1$density <- scale(nomiss$density, center = TRUE, scale = TRUE)

# Regress
fit_ols <- lm(subscription_continuous ~ available +
              hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density,
              data = nomiss1)
fit_ord_log <- clm(as.factor(subscription_continuous) ~ available +
                            hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density,
                          data = nomiss1, link = "logit")
fit_ord_prob <- clm(as.factor(subscription_continuous) ~ available +
                             hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density,
                           data = nomiss1, link = "probit")

# Compare
stargazer(fit_ols, fit_ord_log, fit_ord_prob,
          digits = 2, no.space = TRUE, type = "text", align = TRUE, df = TRUE, star.cutoffs = c(0.05, 0.01, 0.001), single.row = TRUE)

# Sidenote: MASS also works but unrecognized by Stargazer because of 0 and 1s
fit_ord_log1 <- MASS::polr(as.factor(subscription_continuous) ~ available +
                            hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density,
                          data = nomiss1, Hess = TRUE, method = "logistic")
fit_ord_prob1 <- MASS::polr(as.factor(subscription_continuous) ~ available +
                             hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density,
                           data = nomiss1, Hess = TRUE, method = "probit")

# Diagnostics
plot(fit_ols, which = 1)
olsplot_qq <- plot(fit_ols, which = 2)
plot(fit_ols, which = 3)
plot(resid(fit_ols))

logplot_fit <- autoplot(fit_ord_log, what = "fitted")
logplot_qq <- autoplot(fit_ord_log, what = "qq")
logplot_resid <- autoplot(resids(fit_ord_log))

probplot_fit <- autoplot(fit_ord_prob, what = "fitted")
probplot_qq <- autoplot(fit_ord_prob, what = "qq")
probplot_resid <- autoplot(resids(fit_ord_prob))

grid.arrange(logplot_fit, logplot_qq, logplot_resid, probplot_fit, probplot_qq, probplot_resid, nrow = 3)


