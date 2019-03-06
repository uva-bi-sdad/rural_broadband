library(dplyr)
library(naniar)
library(stargazer)


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
# Interpretation: The exponentiated coefficient of X represents the arithmetic mean ratio in the dependent variable between the X01 and X=0, aka
# the exponentiated coefficient is the multiplier on the expected value of Y when X changes by 1.

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

# Diagnostics
plot(fit_gamma3)
autoplot(fit_gamma3)

# Residuals vs. fitted: A horizontal line with no pattern indicates a linear reationship. --> Mildly bad?
# Normal Q-Q: Residual points following the dashed line indicate normal residual distribution. --> Fail at extremes.
# Scale-Location: Horizontal line with equal point spread indicates homoskedasticity. --> Heteroskedastic, I think.
# Residuals vs leverage: Points in the upper right and lower right corners are problematic. --> Borderline bottom right problem (1 point).

# The outlier is the same (22547) as it was in the OLS.

# GLM inverse gaussian
# Identity, log, inverse, and 1/mu^2 links are possible.

