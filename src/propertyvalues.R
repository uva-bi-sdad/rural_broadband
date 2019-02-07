library(dplyr)
library(ggplot2)
library(rgdal)
library(tidycensus)
library(tigris)
library(acs)
library(rgeos)

library(naniar)
library(stargazer)
library(broom)
library(ggfortify)
library(RCurl)
library(car)
library(jtools)
library(reshape2)

options(scipen = 999)
census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key


#
# Read in Josh's data -----------------------------------------------------------------------------------------------------------------------
#

tract_data5 <- read.csv("data/broadband_acs_by_census_tract_2015.csv")
head(tract_data5)


#
# Identify proxy property value variable -----------------------------------------------------------------------------------------------------
#

# Housing units
# View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B00002", case.sensitive=F)@results)
# View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25001", case.sensitive=F)@results)
# B00002_001 Unweighted Sample Housing Units Total
# B25001_001 Housing Units Total

# Occupancy status (Universe: Housing units)
# View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25002", case.sensitive=F)@results)
# B25002_001 Occupancy Status Total
# B25002_002 Occupancy Status: Occupied
# B25002_003 Occupancy Status: Vacant

# Tenure (Universe: Occupied housing units)
# View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25003", case.sensitive=F)@results)
# B25003_001 Tenure Total
# B25003_002 Tenure: Owner occupied
# B25003_003 Tenure: Renter occupied

# Median value ($) for owner-occupied housing units
# View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25077", case.sensitive=F)@results)
# B25077_001 Median Value (Dollars) for Owner-Occupied Housing Units

# Units in structure (Universe: Housing units)
# View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25024", case.sensitive=F)@results)
# B25024_001	Units in Structure	Total:
# B25024_002	Units in Structure	1, detached
# B25024_003	Units in Structure	1, attached
# B25024_004	Units in Structure	2
# B25024_005	Units in Structure	3 or 4
# B25024_006	Units in Structure	5 to 9
# B25024_007	Units in Structure	10 to 19
# B25024_008	Units in Structure	20 to 49
# B25024_009	Units in Structure	50 or more
# B25024_010	Units in Structure	Mobile home
# B25024_011	Units in Structure	Boat, RV, van, etc

# Median year structure built (Universe: Housing units)
# View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25035", case.sensitive=F)@results)
#B25035_001 Median year structure built

# Bedrooms (Universe: Housing units)
# View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25041", case.sensitive=F)@results)
# B25041_001	Total:
# B25041_002	No bedroom
# B25041_003	1 bedroom
# B25041_004	2 bedrooms
# B25041_005	3 bedrooms
# B25041_006	4 bedrooms
# B25041_007	5 or more bedrooms

# Contract rent (Universe: Renter-occupied housing units)
# View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25056", case.sensitive=F)@results)

# Gross rent (Universe: Renter-occupied housing units)
# View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25063", case.sensitive=F)@results)

# Median gross rent ($) (Universe: Renter-occupied housing units paying cash rent)
# View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25064", case.sensitive=F)@results)
# B25064_001 Median Gross Rent (Dollars)

# Aggregate gross rent ($)  (Universe: Renter-occupied housing units paying cash rent)
# View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25065", case.sensitive=F)@results)
# B25065_001 Aggregate gross rent (dollars)


#
# Get variable data for geography & transform -------------------------------------------------------------------------------------------------------------
#

state_fips <- unique(fips_codes$state)[1:51]

propertyvars <- c("B00002_001", "B25001_001", "B25002_001", "B25002_002", "B25002_003", "B25003_001", "B25003_002", "B25003_003", "B25077_001", 
  "B25024_001", "B25024_002", "B25024_003", "B25024_004", "B25024_005", "B25024_006", "B25024_007", "B25024_008", "B25024_009", "B25024_010", "B25024_011",
  "B25035_001", "B25041_001", "B25041_002", "B25041_003", "B25041_004", "B25041_005", "B25041_006", "B25041_007", "B25064_001", "B25065_001")

acs_est <- get_acs(geography = "tract", state=state_fips[1], variables = propertyvars, year = 2015, cache_table = TRUE, output = "wide")
for(i in 2:length(state_fips)){
  tmp <- get_acs(geography = "tract", state = state_fips[i], variables = propertyvars, year = 2015, cache_table = TRUE, output = "wide")
  acs_est <- rbind(acs_est,tmp)
}

acs_est <- acs_est %>% transmute(
  GEOID = GEOID,
  name2 = NAME,
  hunits_total_unw = B00002_001E,
  hunits_total = B25001_001E,
  occstatus_total = B25002_001E,
  occstatus_occup = B25002_002E,
  occstatus_vac = B25002_003E,
  tenure_total= B25003_001E,
  tenure_own = B25003_002E,
  tenure_rent = B25003_003E,
  median_val = B25077_001E,
  unitno_total = B25024_001E,
  unitno_1det = B25024_002E,
  unitno_1at = B25024_003E,
  unitno_2 = B25024_004E,
  unitno_3or4 = B25024_005E,
  unitno_5to9 = B25024_006E,
  unitno_10to19 = B25024_007E,
  unitno_20to49 = B25024_008E,
  unitno_50plus = B25024_009E,
  unitno_mobile = B25024_010E,
  unitno_other = B25024_011E,
  median_yrbuilt = B25035_001E,
  bed_total = B25041_001E,
  bed_none = B25041_002E,
  bed_1 = B25041_003E,
  bed_2 = B25041_004E,
  bed_3 = B25041_005E,
  bed_4 = B25041_006E,
  bed_5plus= B25041_007E,
  rent_mediangross = B25064_001E, 
  rent_aggreggross = B25065_001E)

# Compute rates
# Rate of occupied (vs vacant) / total: B25002_002/B25002_001
# Rate of owned (v. rented) / total: B25003_002/B25003_001
# Rate of single (vs. multiunit) / total: (B25024_002 + B25024_003)/B25024_001

acs_est <- acs_est %>% mutate(
  rate_occupied = occstatus_occup/occstatus_total, 
  rate_owned = tenure_own/tenure_total,
  rate_single = (unitno_1det+unitno_1at)/unitno_total
)


#
# Join data ---------------------------------------------------------------------------------------------------------------
#

acs_est$GEOID <- as.numeric(acs_est$GEOID)
tract_data5 <- tract_data5 %>% left_join(acs_est, by = "GEOID")


#
# Missingness check -------------------------------------------------------------------------------------
#

# Missingness on new variables
gg_miss_var(tract_data5)

n_miss(tract_data5$median_val) # 1647 missing on outcome variable
pct_miss(tract_data5$median_val) # 2.27% missing on outcome variable

# Complete cases
n_case_complete(tract_data5) # 69485 complete cases
pct_complete_case(tract_data5) # 95.56% cases complete

# Look at outcome
hist(tract_data5$median_val) # Right-skewed distribution. OK for outcome, but check errors post-estimation. 

# Create a DF with no missings on model variables
nomiss <- tract_data5 %>% select(GEOID, STATEFP, COUNTYFP, NAME, name2, available, subscription_continuous,
                                 hs_or_less, age_65_older, hispanic, black, foreign, rural, poverty, density, family,
                                 rate_occupied, rate_owned, rate_single, median_yrbuilt,
                                 median_val, rent_mediangross, rent_aggreggross) %>% 
                          filter(!is.na(available), !is.na(subscription_continuous), !is.na(hs_or_less), !is.na(age_65_older),
                                 !is.na(hispanic), !is.na(black), !is.na(foreign), !is.na(rural), !is.na(poverty), !is.na(density),
                                 !is.na(family), !is.na(rate_occupied), !is.na(rate_owned), !is.na(rate_single), !is.na(median_yrbuilt))


#
# Property value: Predict median property value -------------------------------------------------------------------------------------
#

reg_median1 <- lm(median_val ~ available + subscription_continuous,
                  data = nomiss)
reg_median2 <- lm(median_val ~ available + subscription_continuous + 
                  hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density + family,
                  data = nomiss)
reg_median3 <- lm(median_val ~ available + subscription_continuous + 
                  hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density + family +
                  rate_occupied + rate_owned + rate_single + median_yrbuilt,
                  data = nomiss)
stargazer(reg_median1, reg_median2, reg_median3, no.space = TRUE, digits = 2, type = "text")


#
# Property value: Diagnostics -------------------------------------------------------------------------------------
#

autoplot(reg_median3)
# Residuals vs. fitted: A horizontal line with no pattern indicates a linear reationship. --> All-around fail.
# Normal Q-Q: Residual points following the dashed line indicate normal residual distribution. --> Fail at extremes.
# Scale-Location: Horizontal line with equal point spread indicates homoskedasticity. --> Definitely heteroskedastic. 
# Residuals vs leverage: Definitely an influential point. 

# High leverage & what happens when removed: Nothing much. No reason to exclude.
augmodel <- augment(reg_median3)
augmodel %>% top_n(3, wt = .cooksd)
nomiss[22547, ] # This is the weird outlier on every plot, https://censusreporter.org/profiles/14000US17031030702-census-tract-30702-cook-il/

test <- nomiss %>% filter(name2 != "Census Tract 307.02, Cook County, Illinois")
reg_median3a <- lm(log_medianval ~ available + subscription_continuous + 
                     hs_or_less + age_65_older + hispanic + black + foreign +
                     rural + poverty + density + family,
                   data = test)
summary(reg_median3a)
stargazer(reg_median3, reg_median3a, digits = 2, type = "text")
autoplot(reg_median3a)

# Get robust standard errors
# To add robust SE to Stargazer output, see https://economictheoryblog.com/2018/05/18/robust-standard-errors-in-stargazer/
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)), envir = .GlobalEnv)

summary(reg_median3)
summary(reg_median3, robust = TRUE)

#
# Property value: Predict logged median property value -------------------------------------------------------------------------------------
#

# Run Box-Cox to figure out the best outcome transformation
bc <- MASS::boxcox(reg_median3, lambda = seq(-3, 3))
lambda <- bc$x[which.max(bc$y)]
# Lambda is close to 0 (-0.090909...) --> log transform.

nomiss$log_medianval <- log(nomiss$median_val)

reg_median1 <- lm(log_medianval ~ available + subscription_continuous,
                  data = nomiss)
reg_median2 <- lm(log_medianval ~ available + subscription_continuous + 
                  hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density + family,
                  data = nomiss)
reg_median3 <- lm(log_medianval ~ available + subscription_continuous + 
                  hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density + family +
                  rate_occupied + rate_owned + rate_single + median_yrbuilt,
                  data = nomiss)

robust_se1 <- as.vector(summary(reg_median1, robust = T)$coefficients[, "Std. Error"])
robust_se2 <- as.vector(summary(reg_median2, robust = T)$coefficients[, "Std. Error"])
robust_se3 <- as.vector(summary(reg_median3, robust = T)$coefficients[, "Std. Error"])
stargazer(reg_median1, reg_median2, reg_median3, digits = 2, no.space = TRUE, type = "text", se = list(robust_se1, robust_se2, robust_se3), star.cutoffs = c(0.05, 0.01, 0.001))

# Exponentiate the coefficient, subtract one from this number, and multiply by 100. 
# This gives the percent increase (or decrease) in the response for every one-unit increase in the independent variable. 
myfunction <- function(x){
  (exp(x)-1)*100
}

stargazer(reg_median1, reg_median2, reg_median3, digits = 2, no.space = TRUE, type = "text", se = list(robust_se1, robust_se2, robust_se3),
          apply.coef = myfunction)

# Diagnostics look better
autoplot(reg_median3)

# Outlandish estimates because of variable skewness?
hist(nomiss$black)
hist(nomiss$hispanic)
hist(nomiss$poverty)
hist(nomiss$family)
hist(nomiss$foreign)
hist(nomiss$age_65_older)
hist(nomiss$hs_or_less)
hist(nomiss$rate_owned)

# Yes, but can't do Box-Tidwell/log/... because of 0 values on all variables.
# Found one paper that suggest logit transformations for rates (http://fmwww.bc.edu/repec/bocode/t/transint.html), logit p = log (p / (1 - p))
# Tried this and diagnostics are not better.


#
# Property value: Result plots -------------------------------------------------------------------------------------
#

# Version 1
plot_summs(reg_median3, scale = TRUE) + labs(title = "Coefficients and 95%CI from OLS model \n predicting logged median property values")

# Version 2
effect_transf <- c(-9.93, 36.31, -87.66, 68.72, -17.23, -31.11, 356.91, -9.34, -61.90, 0.001, 197.91, -1.52, -33.34, 0.87, -0.19)
effect_semin <- c(-9.94, 36.29, -87.68, 68.67, -17.25, -31.12, 356.88, -9.35, -61.94, 0.001, 197.87, -1.56, -33.38, 0.84, -0.19)
effect_seplus <- c(-9.91, 36.33, -87.63, 68.77, -17.21, -31.09, 356.95, -9.33, -61.86, 0.001, 197.95, -1.49, -33.31, 0.89, -0.19)
label <- c("Broadband availability at 25 mbps", "Subscription rate", "Share of population age 25+ with\n high school diploma or less",
           "Share of population aged 65+", "Hispanic share of population", "Black share of population", "Foreign born share of population",
           "Rural neighborhood", "Poverty rate", "Population density per sq. mile", "Families share of population", "Occupied share of properties", 
           "Owned share of properties", "Single share of properties", "Property median year built")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
lm_dat <- melt(data.frame(label, effect_transf))
lm_dat$label <- ordered(lm_dat$label, levels = rev(label))
lm_dat$effect_semin <- effect_semin
lm_dat$effect_seplus <- effect_seplus

fig <- ggplot(data = lm_dat, aes(x = reorder(label, value), y = value)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, aes(fill = variable)) +
  geom_errorbar(aes(ymin = effect_semin, ymax = effect_seplus),
                size = .3,    
                width = .2,
                position = position_dodge(.9)) +
  coord_flip() +
  theme(legend.position = "none") +
  scale_fill_manual(values = cbPalette[2]) +
  labs(x = "", y = "% change", title = "Estimated % change in median property value per unit increase in independent variable", 
       caption = "Source: ACS, 2015. All variables except occupied and single rate significant at p<0.001. 95% CI shown.") +
  scale_y_continuous(breaks = seq(from = -100, to = 400, by = 30))
fig


#
# Rent: Predict median rent  -------------------------------------------------------------------------------------
#

reg_median1 <- lm(rent_mediangross ~ available + subscription_continuous,
                  data = nomiss)
reg_median2 <- lm(rent_mediangross ~ available + subscription_continuous + 
                    hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density + family,
                  data = nomiss)
reg_median3 <- lm(rent_mediangross ~ available + subscription_continuous + 
                    hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density + family +
                    rate_occupied + rate_owned + rate_single + median_yrbuilt,
                  data = nomiss)
stargazer(reg_median1, reg_median2, reg_median3, no.space = TRUE, digits = 2, type = "text")

# Quick diagnostics: pretty awful!
autoplot(reg_median3)


#
# Rent: Predict logged median rent  -------------------------------------------------------------------------------------
#

# Run Box-Cox to figure out the best outcome transformation
bc <- MASS::boxcox(reg_median3, lambda = seq(-3, 3))
lambda <- bc$x[which.max(bc$y)]
# Lambda is close to 0 (-0.272727...) --> log transform.

# OLS with depvar log transformed
nomiss$log_rentgrossmed <- log(nomiss$rent_mediangross)

reg_median1 <- lm(log_rentgrossmed ~ available + subscription_continuous,
                  data = nomiss)
reg_median2 <- lm(log_rentgrossmed ~ available + subscription_continuous + 
                    hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density + family,
                  data = nomiss)
reg_median3 <- lm(log_rentgrossmed ~ available + subscription_continuous + 
                    hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density + family +
                    rate_occupied + rate_owned + rate_single + median_yrbuilt,
                  data = nomiss)
stargazer(reg_median1, reg_median2, reg_median3, no.space = TRUE, digits = 2, type = "text")

robust_se1 <- as.vector(summary(reg_median1, robust = T)$coefficients[, "Std. Error"])
robust_se2 <- as.vector(summary(reg_median2, robust = T)$coefficients[, "Std. Error"])
robust_se3 <- as.vector(summary(reg_median3, robust = T)$coefficients[, "Std. Error"])
stargazer(reg_median1, reg_median2, reg_median3, digits = 2, no.space = TRUE, type = "text", se = list(robust_se1, robust_se2, robust_se3), star.cutoffs = c(0.05, 0.01, 0.001))

# Quick diagnostics: a bit better.
autoplot(reg_median3)

# Exponentiate the coefficient, subtract one from this number, and multiply by 100. 
# This gives the percent increase (or decrease) in the response for every one-unit increase in the independent variable. 
myfunction <- function(x){
  (exp(x)-1)*100
}

stargazer(reg_median1, reg_median2, reg_median3, digits = 2, no.space = TRUE, type = "text", se = list(robust_se1, robust_se2, robust_se3),
          apply.coef = myfunction)


#
# Rent: Result plots -------------------------------------------------------------------------------------
#

# Version 1
plot_summs(reg_median3, scale = TRUE) + labs(title = "Coefficients and 95%CI from OLS model \n predicting logged median gross rent")

# Version 2
effect_transf <- c(-1.37, 48.18, -54.79, -17.17, 11.75, 15.76, 152.29, -11.53, -39.22, 0.0004, 15.54, -9.60, 7.31, 26.23, 0.10)
effect_semin <- c(-1.37, 48.17, -54.81, -17.20, 11.73, 15.75, 152.27, -11.52, -39.24, 0.0004, 15.52, -9.62, 7.29, 26.22, 0.10)
effect_seplus <- c(-1.36, 48.19, -54.78, -17.14, 11.76, 15.77, 152.32, -11.52, -39.19, 0.0004, 15.56, -9.58, 7.32, 26.25, 0.10)
label <- c("Broadband availability at 25 mbps", "Subscription rate", "Share of population age 25+ with\n high school diploma or less",
           "Share of population aged 65+", "Hispanic share of population", "Black share of population", "Foreign born share of population",
           "Rural neighborhood", "Poverty rate", "Population density per sq. mile", "Families share of population", "Occupied share of properties", 
           "Owned share of properties", "Single share of properties", "Property median year built")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
lm_dat <- melt(data.frame(label, effect_transf))
lm_dat$label <- ordered(lm_dat$label, levels = rev(label))
lm_dat$effect_semin <- effect_semin
lm_dat$effect_seplus <- effect_seplus

fig <- ggplot(data = lm_dat, aes(x = reorder(label, value), y = value)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, aes(fill = variable)) +
  geom_errorbar(aes(ymin = effect_semin, ymax = effect_seplus),
                size = .3,    
                width = .2,
                position = position_dodge(.9)) +
  coord_flip() +
  theme(legend.position = "none") +
  scale_fill_manual(values = cbPalette[2]) +
  labs(x = "", y = "% change", title = "Estimated % change in median gross rent per unit increase in independent variable", 
       caption = "Source: ACS, 2015. All variables significant at p<0.001. 95% CI shown.") +
  scale_y_continuous(breaks = seq(from = -60, to = 200, by = 20))
fig


#
# Rent and median value on the same plot -------------------------------------------------------------------------------------
#

effect_value <- c(-9.93, 36.31, -87.66, 68.72, -17.23, -31.11, 356.91, -9.34, -61.90, 0.001, 197.91, -1.52, -33.34, 0.87, -0.19)
effect_value_semin <- c(-9.94, 36.29, -87.68, 68.67, -17.25, -31.12, 356.88, -9.35, -61.94, 0.001, 197.87, -1.56, -33.38, 0.84, -0.19)
effect_value_seplus <- c(-9.91, 36.33, -87.63, 68.77, -17.21, -31.09, 356.95, -9.33, -61.86, 0.001, 197.95, -1.49, -33.31, 0.89, -0.19)

effect_rent <- c(-1.37, 48.18, -54.79, -17.17, 11.75, 15.76, 152.29, -11.53, -39.22, 0.0004, 15.54, -9.60, 7.31, 26.23, 0.10)
effect_rent_semin <- c(-1.37, 48.17, -54.81, -17.20, 11.73, 15.75, 152.27, -11.52, -39.24, 0.0004, 15.52, -9.62, 7.29, 26.22, 0.10)
effect_rent_seplus <- c(-1.36, 48.19, -54.78, -17.14, 11.76, 15.77, 152.32, -11.52, -39.19, 0.0004, 15.56, -9.58, 7.32, 26.25, 0.10)

label <- c("25mbps broadband availability", "Subscription rate", "Age 25+ with <=HS educ(Population share)",
           "Aged 65+ (Population share)", "Hispanic (Population share)", "Black (Population share)", "Foreign born (Population share)",
           "Rural neighborhood", "Poverty rate", "Population density/sq.mile", "Families (Population share)", "Occupied (Property share)", 
           "Owned (Property share)", "Single (Property share)", "Property median year built")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

lm_dat <- melt(data.frame(label, effect_value, effect_rent))
lm_dat$label <- ordered(lm_dat$label, levels = rev(label))
lm_dat$effect_semin <- c(effect_value_semin, effect_rent_semin)
lm_dat$effect_seplus <- c(effect_value_seplus, effect_rent_seplus)

fig <- ggplot(data = lm_dat, aes(x = reorder(label, value), y = value)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, aes(fill = variable)) +
  coord_flip() +
  scale_fill_manual("Effect size", values = cbPalette[2:3], labels = c("Property value", "Gross rent")) +
  labs(x = "", y = "% change", title = "Estimated % change in median property value and median rent\n per unit increase in independent variable", 
       caption = "Source: ACS, 2015. All variables significant at p<0.001, except occupied and single rate for property value predictions.") +
  scale_y_continuous(breaks = seq(from = -100, to = 400, by = 30)) +
  theme(legend.position = c(0.8, 0.2), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))
fig

