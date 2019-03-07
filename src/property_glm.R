library(dplyr)
library(naniar)
library(DHARMa)
library(boot)
library(stargazer)


# GLM
# No assumption of normality, constant variance, or additivity of effects. Instead:
# response variable is assumed to be a member of the exponential family of distributions; 
# variance is permitted to vary with the mean of the distribution; 
# the effect of covariates on the response variable is assumed to be additive on a transformed scale. 

# Link functions: GLMs require that there is a link function that guarantees additivity. LM requires that Y is additive in covariates; 
# GLM requires that there is some transformation of Y, g(Y), that is additive in the covariates.

# Gamma and inverse Gaussian are suitable for continuous, positive, right-skewed data.
# Compared to gamma, inverse Gaussian is even more skewed and heteroskedastic. 
# "... gamma distribution more strongly influenced by points on the left because the model assumes the variance increases with the square of the expected value"

# Diagnostics not necessarily interpreted the same way + not all OLS diagnostics apply given the relaxed assumptions.
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
# https://stats.stackexchange.com/questions/295340/what-to-do-with-glm-gamma-when-residuals-are-not-normally-distributed/302413#302413
# https://stats.stackexchange.com/questions/45401/how-to-validate-diagnose-a-gamma-glm-in-r
# https://stats.stackexchange.com/questions/351472/interpreting-glm-diag-plots

# Resources:
# https://www.towerswatson.com/en/Insights/IC-Types/Technical-Regulatory/2010/A-Practitioners-Guide-to-Generalized-Linear-Models
# https://stats.stackexchange.com/questions/67547/when-to-use-gamma-glms?noredirect=1&lq=1
# https://rpubs.com/kaz_yos/glm-Gamma


#
# Read in & prepare data --------------------------------------------------------------------------------------------------------------
#

propdata <- read.csv("data/acs_property.csv")
head(propdata)

# Create a DF with no missings on model variables
nomiss <- propdata %>% select(GEOID, STATEFP, COUNTYFP, NAME, name2, available, subscription_continuous,
                              hs_or_less, age_65_older, hispanic, black, foreign, rural, poverty, density, family,
                              rate_occupied, rate_owned, rate_single, median_yrbuilt,
                              median_val, rent_mediangross, rent_aggreggross) %>% 
                       filter(!is.na(available), !is.na(subscription_continuous), !is.na(hs_or_less), !is.na(age_65_older),
                              !is.na(hispanic), !is.na(black), !is.na(foreign), !is.na(rural), !is.na(poverty), !is.na(density),
                              !is.na(family), !is.na(rate_occupied), !is.na(rate_owned), !is.na(rate_single), !is.na(median_yrbuilt))


#
# GLM gamma: The outcome is continuous, right skewed, and always positive. -------------------------------------------------------------
#

# subscription_continuous excluded (available included)

# Identity, log, and inverse links are possible.
# Inverse and identity do not converge.

# Log link: Multiplicative arithmetic mean model
# Log link: effects of covariates are multiplicative; GLM estimates logs of multiplicative effects.
# Interpretation: The exponentiated coefficient of X represents the arithmetic mean ratio in the dependent variable between the X01 and X=0, aka
# exponentiated coefficients give the multiplier on the expected value of Y when X changes by 1.

# Untransformed estimates
fit_gamma1 <- glm(median_val ~ available,
                  data = nomiss, family = Gamma(link = "log"))
fit_gamma2 <- glm(median_val ~ available +
                  hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density + family,
                  data = nomiss, family = Gamma(link = "log"))
fit_gamma3 <- glm(median_val ~ available +
                  hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density + family +
                  rate_occupied + rate_owned + rate_single + median_yrbuilt,
                  data = nomiss, family = Gamma(link = "log"))

stargazer(fit_gamma1, fit_gamma2, fit_gamma3, digits = 2, no.space = TRUE, type = "text", align = TRUE, df = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))

# Exponentiated coefficients to facilitate interpretation (note: untransformed SEs!)
gcoef1 <- exp(fit_gamma1$coefficients)
gcoef2 <- exp(fit_gamma2$coefficients)
gcoef3 <- exp(fit_gamma3$coefficients)
stargazer(fit_gamma1, fit_gamma2, fit_gamma3, digits = 2, no.space = TRUE, type = "text", align = TRUE, df = TRUE, star.cutoffs = c(0.05, 0.01, 0.001),
          p.auto = F, coef = list(gcoef1, gcoef2, gcoef3))

# Interpretation example: Each 1 unit increase in proportion of population with HS education or less or less decreases the log arithmetic mean outcome by 2.37. 
# The exponentiated coefficient (exp(-2.37)) = 0.09348073 is the factor by which the arithmetic mean outcome on the original scale is multiplied for the same one unit increase;
# the arithmetic mean on the original scale is 0.09348073 times higher for the unit increase, holding all else constant.

# DIAGNOSTICS
# QQ plot: The QQ plot here is not against the normal distribution, it is against the simulation-based expected distribution of the residuals.
# That is the idea with the simulation: we do not know, theoretically, what is the null distribution of the residuals, so we approximate it by simulating from the fitted model. 
# Residuals v. predicted: residuals against expected (fitted) value, is augmented with three red lines which should (if the model is correct) be horizontal, straight, and at
# quantiles 0.25, 0.5, 0.75 (because they arebased on quantile regression of the residuals at these three Qs). Some deviations from this are to be expected by chance,
# even for a perfect model, especially if the sample size is small.
sim_gamma  <-  simulateResiduals(fit_gamma3,  n = 250, plot = TRUE)
plot(sim_gamma)

# GOF tests
# Dispersion: Over/underdispersion = residual variance is larger/smaller than expected under the fitted model. Over is more common and causes narrow CIs, 
# small p values (opposite for under). Common cause is misspecified model. 

testResiduals(sim_gamma)

testUniformity(sim_gamma)
testOutliers(sim_gamma)
testDispersion(sim_gamma) 
testZeroInflation(sim_gamma)

# Residuals
rstudent(fit_gamma3)      # jackknife
glm.diag(fit_gamma3)$rp   # standardized Pearson
glm.diag(fit_gamma3)$rd   # deviance

# Cook's distance
glm.diag(fit_gamma3)$cook
cooks.distance(fit_gamma3)

# More plots
glm.diag.plots(fit_gamma3, iden = T)


#
# GLM inverse gaussian -------------------------------------------------------------
#

# subscription_continuous excluded (available included)

# Identity, log, inverse, and 1/mu^2 links are possible.
# Identity, inverse, 1/mu^2 do not converge.

# Log link
# Untransformed estimates
fit_invg1 <- glm(median_val ~ available,
                  data = nomiss, family = inverse.gaussian(link = "log"))
fit_invg2 <- glm(median_val ~ available +
                    hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density + family,
                  data = nomiss, family = inverse.gaussian(link = "log"))
fit_invg3 <- glm(median_val ~ available +
                    hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density + family +
                    rate_occupied + rate_owned + rate_single + median_yrbuilt,
                  data = nomiss, family = inverse.gaussian(link = "log"))

stargazer(fit_invg1, fit_invg2, fit_invg3, digits = 2, no.space = TRUE, type = "text", align = TRUE, df = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))

# DIAGNOSTICS
# QQ plot: The QQ plot here is not against the normal distribution, it is against the simulation-based expected distribution of the residuals.
# That is the idea with the simulation: we do not know, theoretically, what is the null distribution of the residuals, so we approximate it by simulating from the fitted model. 
# Residuals v. predicted: residuals against expected (fitted) value, is augmented with three red lines which should (if the model is correct) be horizontal, straight, and at
# quantiles 0.25, 0.5, 0.75 (because they arebased on quantile regression of the residuals at these three Qs). Some deviations from this are to be expected by chance,
# even for a perfect model, especially if the sample size is small.
sim_invg  <-  simulateResiduals(fit_invg3,  n = 250, plot = TRUE)
plot(sim_invg)

# GOF tests
# Dispersion: Over/underdispersion = residual variance is larger/smaller than expected under the fitted model. Over is more common and causes narrow CIs, 
# small p values (opposite for under). Common cause is misspecified model. 

testResiduals(sim_invg)

testUniformity(sim_invg)
testOutliers(sim_invg)
testDispersion(sim_invg) 
testZeroInflation(sim_invg)

# Residuals
rstudent(fit_invg3)      # jackknife
glm.diag(fit_invg3)$rp   # standardized Pearson
glm.diag(fit_invg3)$rd   # deviance

# Cook's distance
glm.diag(fit_invg3)$cook
cooks.distance(fit_invg3)

# More plots
glm.diag.plots(fit_invg3, iden = T)