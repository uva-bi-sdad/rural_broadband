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
View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B00002", case.sensitive=F)@results)
View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25001", case.sensitive=F)@results)
# B00002_001 Unweighted Sample Housing Units Total
# B25001_001 Housing Units Total

# Occupancy status (Universe: Housing units)
View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25002", case.sensitive=F)@results)
# B25002_001 Occupancy Status Total
# B25002_002 Occupancy Status: Occupied
# B25002_003 Occupancy Status: Vacant

# Tenure (Universe: Occupied housing units)
View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25003", case.sensitive=F)@results)
# B25003_001 Tenure Total
# B25003_002 Tenure: Owner occupied
# B25003_003 Tenure: Renter occupied

# Median value ($) for owner-occupied housing units
View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25077", case.sensitive=F)@results)
# B25077_001 Median Value (Dollars) for Owner-Occupied Housing Units

# Units in structure (Universe: Housing units)
View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25024", case.sensitive=F)@results)
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
View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25035", case.sensitive=F)@results)
#B25035_001 Median year structure built

# Bedrooms (Universe: Housing units)
View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25041", case.sensitive=F)@results)
# B25041_001	Total:
# B25041_002	No bedroom
# B25041_003	1 bedroom
# B25041_004	2 bedrooms
# B25041_005	3 bedrooms
# B25041_006	4 bedrooms
# B25041_007	5 or more bedrooms

# Contract rent (Universe: Renter-occupied housing units)
View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25056", case.sensitive=F)@results)

# Gross rent (Universe: Renter-occupied housing units)
View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25063", case.sensitive=F)@results)

# Median gross rent ($) (Universe: Renter-occupied housing units paying cash rent)
View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25064", case.sensitive=F)@results)
# B25064_001 Median Gross Rent (Dollars)

# Aggregate gross rent ($)  (Universe: Renter-occupied housing units paying cash rent)
View(acs.lookup(endyear = 2015, span = 5, dataset = "acs", table.number = "B25065", case.sensitive=F)@results)
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
# OLS -------------------------------------------------------------------------------------
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
stargazer(reg_median1, reg_median2, reg_median3, digits = 2, type = "text")


#
# OLS diagnostics -------------------------------------------------------------------------------------
#

autoplot(reg_median3)
# Residuals vs. fitted: A horizontal line with no pattern indicates a linear reationship. --> All-around fail.
# Normal Q-Q: Residual points following the dashed line indicate normal residual distribution. --> Fail at extremes.
# Scale-Location: Horizontal line with equal point spread indicates homoskedasticity. --> Definitely heteroskedastic. 
# Residuals vs leverage: Definitely an influential point. 

# Get robust standard errors
# To add robust SE to Stargazer output, see https://economictheoryblog.com/2018/05/18/robust-standard-errors-in-stargazer/
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)), envir = .GlobalEnv)

summary(reg_median3)
summary(reg_median3, robust = TRUE)

# Try depvar log transform
nomiss5$log_medianval <- log(nomiss$median_val)

reg_median1 <- lm(log_medianval ~ available + subscription_continuous,
                  data = nomiss)
reg_median2 <- lm(log_medianval ~ available + subscription_continuous + 
                  hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density + family,
                  data = nomiss)
reg_median3 <- lm(log_medianval ~ available + subscription_continuous + 
                  hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density + family +
                  rate_occupied + rate_owned + rate_single + median_yrbuilt,
                  data = nomiss)
stargazer(reg_median1, reg_median2, reg_median3, digits = 2, type = "text")

autoplot(reg_median3)

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


#
# Predict rent instead -------------------------------------------------------------------------------------
#

nomiss$log_rentgrossmed <- log(nomiss$rent_mediangross)
nomiss$log_rentaggmed <- log(nomiss$rent_aggreggross)

reg_median1 <- lm(rent_mediangross ~ available + subscription_continuous,
                  data = nomiss)
reg_median2 <- lm(rent_mediangross ~ available + subscription_continuous + 
                    hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density + family,
                  data = nomiss)
reg_median3 <- lm(rent_mediangross ~ available + subscription_continuous + 
                    hs_or_less + age_65_older + hispanic + black + foreign + rural + poverty + density + family +
                    rate_occupied + rate_owned + rate_single + median_yrbuilt,
                  data = nomiss)
stargazer(reg_median1, reg_median2, reg_median3, digits = 2, type = "text")
