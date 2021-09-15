# 5-5-21
# attach ACS summaries and margins of error by Census tract
# compute MOE for derived estimates following
# https://www.census.gov/content/dam/Census/library/publications/2020/acs/acs_general_handbook_2020_ch08.pdf

# 5-12-21 update: add a) Region,
# b) metropolitan, micropolitan, rural (metro: RUCA<=3, micro: 4<=RUCA<=6, rural: RUCA > 6)

# then compute the following summaries
# (4 regions, metro, micro, rural)
# (4 regions, metro, micro, rural) x (BIP, CCG, RCP)
# (4 regions, metro, micro, rural) x (CCG, RCP eligible, not funded)

library(sf)
library(data.table)
library(dplyr)
library(tidycensus)
library(maps)

state_fips <- c( unique(state.fips$fips), 2, 15 )
state_fips <- formatC(state_fips,width=2,flag=0)


# read in the Census tract summary table
setwd("~/git/rural_broadband/src/bipACS/")


# ------------------------------------------
# add to tract data regions and a new definition of metropolitan, micropolitan, rural
tracts_dat <- fread("broadband_tract_eligibility5-5-21.csv",
                    colClasses=c(GEOID="character"))

tracts_dat2 <- tracts_dat %>% select(-c(RUCA_RURAL,RUCA_URBAN))
tracts_dat3 <- tracts_dat2 %>% mutate(
  RUCA_METRO = case_when( RUCA_2010 <=3 ~ 1,
                          TRUE ~ 0 ),
  RUCA_MICRO = case_when( RUCA_2010 %in% c(4,5,6) ~ 1,
                          TRUE ~ 0 ),
  RUCA_RURAL = case_when( RUCA_2010 >6 ~ 1,
                          TRUE ~ 0 )
) %>% relocate(c(RUCA_METRO,RUCA_MICRO,RUCA_RURAL),.after=RUCA_2010_SECONDARY)

# add region by state
# source: https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
tracts_dat4 <- tracts_dat3 %>% mutate(
  REGION = case_when(
    STATEFP %in% c(9,23,25,33,44,50,34,36,42) ~ "NORTHEAST",
    STATEFP %in% c(18,17,26,39,55,19,20,27,29,31,38,46) ~ "MIDWEST",
    STATEFP %in% c(10,11,12,13,24,37,45,51,54,01,21,28,47,5,22,40,48) ~ "SOUTH",
    STATEFP %in% c(4,8,16,35,30,49,32,56,2,6,15,41,53) ~ "WEST"
  )
) %>% relocate(REGION, .after=TRACTCE)

# save tract summary file
fwrite(tracts_dat4,"broadband_tract_eligibility5-5-21_regions.csv")

rm(tracts_dat,tracts_dat2,tracts_dat3,tracts_dat4)

# ------------------------------------------

tracts_dat <- fread("broadband_tract_eligibility5-5-21_regions.csv",
                    colClasses=c(GEOID="character"))

census_api_key("853b2a1e71aa0aff0f3db966545e8898c73f0772")

acs_vars <- c("B15002_001","B15002_003","B15002_004","B15002_005","B15002_006","B15002_007","B15002_008","B15002_009","B15002_010","B15002_011",
              "B15002_020","B15002_021","B15002_022","B15002_023","B15002_024","B15002_025","B15002_026","B15002_027","B15002_028",
              "B17001_001","B17001_002",
              "B01001_001","B01001_020","B01001_021","B01001_022","B01001_023","B01001_024","B01001_025",
              "B01001_044","B01001_045","B01001_046","B01001_047","B01001_048","B01001_049",
              "B03003_001","B03003_003",
              "B02001_001","B02001_003",
              "B09019_002","B09019_003",
              "B05002_001","B05002_013",
              "B19013_001",
              "B25077_001",
              "B23025_003","B23025_005",
              "B25001_001", "B25002_001", "B25002_002", "B25002_003", "B25003_001", "B25003_002", "B25003_003", 
              "B25024_001", "B25024_002", "B25024_003", "B25024_004", "B25024_005", "B25024_006", "B25024_007", "B25024_008", "B25024_009", "B25024_010", "B25024_011",
              "B25035_001", "B25041_001", "B25041_002", "B25041_003", "B25041_004", "B25041_005", "B25041_006", "B25041_007", "B25064_001", "B25065_001",
              "B11001_001","B11001_002")

acs_vars_2010 <- c("B15002_001","B15002_003","B15002_004","B15002_005","B15002_006","B15002_007","B15002_008","B15002_009","B15002_010","B15002_011",
                   "B15002_020","B15002_021","B15002_022","B15002_023","B15002_024","B15002_025","B15002_026","B15002_027","B15002_028",
                   "B17001_001","B17001_002",
                   "B01001_001","B01001_020","B01001_021","B01001_022","B01001_023","B01001_024","B01001_025",
                   "B01001_044","B01001_045","B01001_046","B01001_047","B01001_048","B01001_049",
                   "B03003_001","B03003_003",
                   "B02001_001","B02001_003",
                   "B05002_001","B05002_013",
                   "B19013_001",
                   "B25077_001",
                   "B25001_001", "B25002_001", "B25002_002", "B25002_003", "B25003_001", "B25003_002", "B25003_003", 
                   "B25024_001", "B25024_002", "B25024_003", "B25024_004", "B25024_005", "B25024_006", "B25024_007", "B25024_008", "B25024_009", "B25024_010", "B25024_011",
                   "B25035_001", "B25041_001", "B25041_002", "B25041_003", "B25041_004", "B25041_005", "B25041_006", "B25041_007", "B25064_001", "B25065_001",
                   "B11001_001","B11001_002")

# get ACS variables for a given year for all US census tracts
# for 2006-2010 5-year don't include family, employment (no tables in API)
# compute estimates and MOEs
get_acs_moes <- function(year, vars){
  tmp <- list()
  for(i in 1:length(state_fips)){
    tmp[[i]] <- get_acs(geography="tract",state=state_fips[i],variables=vars,year=year,cache_table=TRUE,output="wide")
  }
  acs_est <- rbindlist(tmp)
  
  acs_estimates <- acs_est %>% transmute(
    GEOID=GEOID,
    population = B01001_001E,
    hunits_total = B25001_001E,
    hs_or_less = (B15002_003E+B15002_004E+B15002_005E+B15002_006E+B15002_007E+B15002_008E+B15002_009E+B15002_010E+B15002_011E+
                    B15002_020E+B15002_021E+B15002_022E+B15002_023E+B15002_024E+B15002_025E+B15002_026E+B15002_027E+B15002_028E) / B15002_001E,
    renters = B25003_003E / B25003_001E,
    poverty = B17001_002E / B17001_001E,
    age_65_older = (B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E+
                      B01001_044E+B01001_045E+B01001_046E+B01001_047E+B01001_048E+B01001_049E)/ B01001_001E,
    hispanic = B03003_003E / B03003_001E,
    black = B02001_003E / B02001_001E,
    family = B11001_002E / B11001_001E,
    foreign = B05002_013E / B05002_001E,
    median_income = B19013_001E,
    median_value = B25077_001E
  )
  if(year >= 2012) { acs_estimates <- cbind(acs_estimates,
                                            acs_est %>% transmute(  
                                              unemployment = B23025_005E/B23025_003E
                                            )
  )}
  names(acs_estimates)[2:length(names(acs_estimates))] <-
    paste0(names(acs_estimates)[2:length(names(acs_estimates))],"_",year)  

    
  acs_moes <- acs_est %>% transmute(
    GEOID=GEOID,
    population = B01001_001M,
    hunits_total = B25001_001M,
    hs_or_less_E = (B15002_003E+B15002_004E+B15002_005E+B15002_006E+B15002_007E+B15002_008E+B15002_009E+B15002_010E+B15002_011E+
                      B15002_020E+B15002_021E+B15002_022E+B15002_023E+B15002_024E+B15002_025E+B15002_026E+B15002_027E+B15002_028E),
    hs_or_less_M = sqrt(B15002_003M^2+B15002_004M^2+B15002_005M^2+B15002_006M^2+B15002_007M^2+B15002_008M^2+B15002_009M^2+B15002_010M^2+B15002_011M^2+
                           B15002_020M^2+B15002_021M^2+B15002_022M^2+B15002_023M^2+B15002_024M^2+B15002_025M^2+B15002_026M^2+B15002_027M^2+B15002_028M^2),
    hs_or_less = 1 / B15002_001E * sqrt( hs_or_less_M^2 - (hs_or_less_E / B15002_001E)^2 * B15002_001M^2 ),
    renters = 1 / B25003_001E * sqrt( B25003_003M^2 - (B25003_003E / B25003_001E)^2 * B25003_001M^2 ),
    poverty = 1 / B17001_001E * sqrt( B17001_002M^2 - (B17001_002E / B17001_001E)^2 * B17001_001M^2 ),
    age_65_older_E = (B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E+
                        B01001_044E+B01001_045E+B01001_046E+B01001_047E+B01001_048E+B01001_049E),
    age_65_older_M = sqrt(B01001_020M^2+B01001_021M^2+B01001_022M^2+B01001_023M^2+B01001_024M^2+B01001_025M^2+
                        B01001_044M^2+B01001_045M^2+B01001_046M^2+B01001_047M^2+B01001_048M^2+B01001_049M^2),
    age_65_older = 1 / B01001_001E * sqrt( age_65_older_M^2 - (age_65_older_E / B01001_001E)^2 * B01001_001M^2 ),
    hispanic = 1 / B03003_001E * sqrt( B03003_003M^2 - (B03003_003E / B03003_001E)^2 * B03003_001M^2 ),
    black = 1 / B02001_001E * sqrt( B02001_003M^2 - (B02001_003E / B02001_001E)^2 * B02001_001M^2 ),
    family = 1 / B11001_001E * sqrt( B11001_002M^2 - (B11001_002E / B11001_001E)^2 * B11001_001M^2 ),
    foreign = 1 / B05002_001E * sqrt( B05002_013M^2 - (B05002_013E / B05002_001E)^2 * B05002_001M^2 ),
    median_income = B19013_001M,
    median_value = B25077_001M
  ) %>% select(-c(hs_or_less_E, hs_or_less_M, age_65_older_E, age_65_older_M))
  if(year >= 2012) { acs_moes <- cbind(acs_moes,
                                            acs_est %>% transmute(  
                                              unemployment = 1 / B23025_003E * sqrt( B23025_005M^2 - (B23025_005E / B23025_003E)^2 * B23025_003M^2 )
                                            )
  )}
  names(acs_moes)[2:length(names(acs_moes))] <-
    paste0(names(acs_moes)[2:length(names(acs_moes))],"_MOE_",year)
  
  return(list(est=acs_estimates,moe=acs_moes))
}


acs_moes_2019 <- get_acs_moes(2019, acs_vars)
acs_moes_2019$est
acs_moes_2019$moe
tracts_dat2 <- tracts_dat %>% left_join(acs_moes_2019$est, by="GEOID")
tracts_dat2 <- tracts_dat2 %>% left_join(acs_moes_2019$moe, by="GEOID")

acs_moes_2018 <- get_acs_moes(2018, acs_vars)
tracts_dat3 <- tracts_dat2 %>% left_join(acs_moes_2018$est, by="GEOID")
tracts_dat3 <- tracts_dat3 %>% left_join(acs_moes_2018$moe, by="GEOID")

acs_moes_2015 <- get_acs_moes(2015, acs_vars)
tracts_dat4 <- tracts_dat3 %>% left_join(acs_moes_2015$est, by="GEOID")
tracts_dat4 <- tracts_dat4 %>% left_join(acs_moes_2015$moe, by="GEOID")

acs_moes_2013 <- get_acs_moes(2013, acs_vars)
tracts_dat5 <- tracts_dat4 %>% left_join(acs_moes_2013$est, by="GEOID")
tracts_dat5 <- tracts_dat5 %>% left_join(acs_moes_2013$moe, by="GEOID")

acs_moes_2010 <- get_acs_moes(2010, acs_vars_2010)
tracts_dat6 <- tracts_dat5 %>% left_join(acs_moes_2010$est, by="GEOID")
tracts_dat6 <- tracts_dat6 %>% left_join(acs_moes_2010$moe, by="GEOID")

# -----------------------------------------------------------------------------------

# adjust all income and home values to 2019 dollars
# (using BLS CPI data at https://www.usinflationcalculator.com/)
adj_2019 <- 1
adj_2018 <- 1.018
adj_2015 <- 1.079
adj_2013 <- 1.097
adj_2010 <- 1.172

tracts_dat6$median_income_adj_2019 <- round(tracts_dat6$median_income_2019 * adj_2019,0)
tracts_dat6$median_value_adj_2019 <- round(tracts_dat6$median_value_2019 * adj_2019,0)
tracts_dat6$median_income_adj_2018 <- round(tracts_dat6$median_income_2018 * adj_2018,0)
tracts_dat6$median_value_adj_2018 <- round(tracts_dat6$median_value_2018 * adj_2018,0)
tracts_dat6$median_income_adj_2015 <- round(tracts_dat6$median_income_2015 * adj_2015,0)
tracts_dat6$median_value_adj_2015 <- round(tracts_dat6$median_value_2015 * adj_2015,0)
tracts_dat6$median_income_adj_2013 <- round(tracts_dat6$median_income_2013 * adj_2013,0)
tracts_dat6$median_value_adj_2013 <- round(tracts_dat6$median_value_2013 * adj_2013,0)
tracts_dat6$median_income_adj_2010 <- round(tracts_dat6$median_income_2010 * adj_2010,0)
tracts_dat6$median_value_adj_2010 <- round(tracts_dat6$median_value_2010 * adj_2010,0)

fwrite(tracts_dat6,"broadband_tract_data_moe_5_5_21.csv")

# -----------------------------------------------------------------------------------

# note: just join the ACS data by GEOID if we don't need to run the above again
#tracts_dat_new <- fread("broadband_tract_eligibility5-5-21_regions.csv",
#                    colClasses=c(GEOID="character"))
#tracts_dat_orig <- fread("broadband_tract_data_moe_5_5_21.csv",
#                         colClasses=c(GEOID="character"))
#tracts_dat_orig <- tracts_dat_orig[,c(1,40:177)]
#tracts_dat <- tracts_dat_new %>% left_join(tracts_dat_orig,by="GEOID")
#fwrite(tracts_dat,"broadband_tract_data_moe_5_5_21.csv")
#rm(tracts_dat_new, tracts_dat_orig)

# -----------------------------------------------------------------------------------
# propagate errors to get MOEs for national estimates; add to table
# save each variable as a separate .csv under summary/varname.csv
# column = ACS year, row = subpopulation
tracts_dat <- fread("broadband_tract_data_moe_5_5_21.csv")

tracts_dat <- tracts_dat %>% mutate(
  NORTHEAST=1*(REGION=="NORTHEAST"),
  MIDWEST=1*(REGION=="MIDWEST"),
  SOUTH=1*(REGION=="SOUTH"),
  WEST=1*(REGION=="WEST")
)

# general formula is:
# as.numeric(df %>% filter(!is.na(varname)) %>% summarize(varname=sum(weights*varname)/sum(weights)))
sumcompute <- function(df, varname, moename, weights){
  # weights is a character vector of columns to multiply
  df_filter <- df[ c(!is.na(df[,..varname]) & !is.na(df[,..moename])), ]
  for(i in 1:length(weights)){
    if(i==1) {
      nam <- weights[1]
      w <- unlist( df_filter[,..nam] )
    } else{
      nam <- weights[i]      
      w <- w * unlist( df_filter[,..nam] )
    }
  }
  var <- unlist( df_filter[,..varname] )
  err <- unlist( df_filter[,..moename] )
  return( data.frame(
    est=sum(w*var)/sum(w),
    moe=sqrt(sum(w^2*err^2))/sum(w))
  )
}

# compute income and value in $1000 to avoid numeric instability
tracts_dat$median_income_2019 <- tracts_dat$median_income_adj_2019 / 1000
tracts_dat$median_value_2019 <- tracts_dat$median_value_adj_2019 / 1000

tracts_dat$median_income_2018 <- tracts_dat$median_income_adj_2018 / 1000
tracts_dat$median_value_2018 <- tracts_dat$median_value_adj_2018 / 1000

tracts_dat$median_income_2015 <- tracts_dat$median_income_adj_2015 / 1000
tracts_dat$median_value_2015 <- tracts_dat$median_value_adj_2015 / 1000

tracts_dat$median_income_2013 <- tracts_dat$median_income_adj_2013 / 1000
tracts_dat$median_value_2013 <- tracts_dat$median_value_adj_2013 / 1000

tracts_dat$median_income_2010 <- tracts_dat$median_income_adj_2010 / 1000
tracts_dat$median_value_2010 <- tracts_dat$median_value_adj_2010 / 1000


#sumcompute(tracts_dat, "renters_2019", "renters_MOE_2019", weights = "population_2019")
#sumcompute(tracts_dat, "poverty_2019", "poverty_MOE_2019", weights = "population_2019")
#sumcompute(tracts_dat, "poverty_2010", "poverty_MOE_2010", weights = "population_2010")
#sumcompute(tracts_dat, "median_value_2019", "median_value_MOE_2019", weights = "hunits_total_2019")
#sumcompute(tracts_dat, "median_income_2019", "median_income_MOE_2019", weights = "hunits_total_2019")

# function to take varname as input and call sumcompute for all years, subpopulation
sumyear <- function(varname, year, type){
  if(type == "person") weightname <- 'population_'
  if(type == "household") weightname <- 'hunits_total_'
  return(rbind(
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = paste0(weightname,year)),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('RUCA_METRO',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('RUCA_MICRO',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('RUCA_RURAL',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('NORTHEAST',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('MIDWEST',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('SOUTH',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('WEST',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('BIP_PROP_AREA',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('BIP_PROP_5MI',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('BIP_PROP_10MI',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('BIP_PROP_25MI',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('BIP_PROP_50MI',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('IN_CC',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2014_CC',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2014_CC_FUNDED',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2014_CC_NOFUNDS',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_CC',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_CC_FUNDED',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_CC_NOFUNDS',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('IN_RC',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_RC',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_RC_FUNDED',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_RC_NOFUNDS',paste0(weightname,year))),
    # BIP interactions (7)
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('BIP_PROP_AREA','RUCA_METRO',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('BIP_PROP_AREA','RUCA_MICRO',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('BIP_PROP_AREA','RUCA_RURAL',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('BIP_PROP_AREA','NORTHEAST',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('BIP_PROP_AREA','MIDWEST',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('BIP_PROP_AREA','SOUTH',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('BIP_PROP_AREA','WEST',paste0(weightname,year))),
    # CC interactions (7)
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('IN_CC','RUCA_METRO',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('IN_CC','RUCA_MICRO',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('IN_CC','RUCA_RURAL',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('IN_CC','NORTHEAST',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('IN_CC','MIDWEST',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('IN_CC','SOUTH',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('IN_CC','WEST',paste0(weightname,year))),
    # CC eligible nonfunded 2014 interactions (7)
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2014_CC_NOFUNDS','RUCA_METRO',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2014_CC_NOFUNDS','RUCA_MICRO',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2014_CC_NOFUNDS','RUCA_RURAL',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2014_CC_NOFUNDS','NORTHEAST',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2014_CC_NOFUNDS','MIDWEST',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2014_CC_NOFUNDS','SOUTH',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2014_CC_NOFUNDS','WEST',paste0(weightname,year))),
    # RC eligible nonfunded 2018 interactions (7)
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_CC_NOFUNDS','RUCA_METRO',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_CC_NOFUNDS','RUCA_MICRO',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_CC_NOFUNDS','RUCA_RURAL',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_CC_NOFUNDS','NORTHEAST',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_CC_NOFUNDS','MIDWEST',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_CC_NOFUNDS','SOUTH',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_CC_NOFUNDS','WEST',paste0(weightname,year))),
    # RC interactions (7)
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('IN_RC','RUCA_METRO',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('IN_RC','RUCA_MICRO',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('IN_RC','RUCA_RURAL',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('IN_RC','NORTHEAST',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('IN_RC','MIDWEST',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('IN_RC','SOUTH',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('IN_RC','WEST',paste0(weightname,year))),
    # RC eligible nonfunded 2018 interactions (7)
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_RC_NOFUNDS','RUCA_METRO',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_RC_NOFUNDS','RUCA_MICRO',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_RC_NOFUNDS','RUCA_RURAL',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_RC_NOFUNDS','NORTHEAST',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_RC_NOFUNDS','MIDWEST',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_RC_NOFUNDS','SOUTH',paste0(weightname,year))),
    sumcompute(tracts_dat, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('ELIGIBLE_2018_RC_NOFUNDS','WEST',paste0(weightname,year)))
  ))
}

# format and write as .csv
sumtable <- function(varname, type){
  df <- data.frame(source = c("National",
                              "Metropolitan (RUCA 1-3)", "Micropolitan (RUCA 4-6)", "Rural (RUCA 7-10)",
                              "Northeast", "Midwest", "South", "West",
                              "BIP Regions", "BIP 5MI Donut", "BIP 10MI Donut", "BIP 25MI Donut", "BIP 50MI Donut",
                              "Community Connect Regions",
                              "Eligible (2014) for Community Connect",
                              "Eligible (2014) CC, received funding",
                              "Eligible (2014) CC, did not receive funding",
                              "Eligible (2018) for Community Connect",
                              "Eligible (2018) CC, received funding",
                              "Eligible (2018) CC, did not receive funding",
                              "ReConnect Regions",
                              "Eligible (2018) for ReConnect",
                              "Eligible (2018) RC, received funding",
                              "Eligible (2018) RC, did not receive funding",
                              "BIP Metropolitan",
                              "BIP Micropolitan",
                              "BIP Rural",
                              "BIP Northeast",
                              "BIP Midwest",
                              "BIP South",
                              "BIP West",
                              "Community Connect Metropolitan",
                              "Community Connect Micropolitan",
                              "Community Connect Rural",
                              "Community Connect Northeast",
                              "Community Connect Midwest",
                              "Community Connect South",
                              "Community Connect West",
                              "CC Eligible Nonfunded (2014) Metropolitan",
                              "CC Eligible Nonfunded (2014) Micropolitan",
                              "CC Eligible Nonfunded (2014) Rural",
                              "CC Eligible Nonfunded (2014) Northeast",
                              "CC Eligible Nonfunded (2014) Midwest",
                              "CC Eligible Nonfunded (2014) South",
                              "CC Eligible Nonfunded (2014) West",
                              "CC Eligible Nonfunded (2018) Metropolitan",
                              "CC Eligible Nonfunded (2018) Micropolitan",
                              "CC Eligible Nonfunded (2018) Rural",
                              "CC Eligible Nonfunded (2018) Northeast",
                              "CC Eligible Nonfunded (2018) Midwest",
                              "CC Eligible Nonfunded (2018) South",
                              "CC Eligible Nonfunded (2018) West",
                              "ReConnect Metropolitan",
                              "ReConnect Micropolitan",
                              "ReConnect Rural",
                              "ReConnect Northeast",
                              "ReConnect Midwest",
                              "ReConnect South",
                              "ReConnect West",
                              "RC Eligible Nonfunded (2018) Metropolitan",
                              "RC Eligible Nonfunded (2018) Micropolitan",
                              "RC Eligible Nonfunded (2018) Rural",
                              "RC Eligible Nonfunded (2018) Northeast",
                              "RC Eligible Nonfunded (2018) Midwest",
                              "RC Eligible Nonfunded (2018) South",
                              "RC Eligible Nonfunded (2018) West"
  ))
  if(varname %in% c('unemployment')){
    df$ACS2010_est <- NA
    df$ACS2010_moe <- NA
  } else{
    df$ACS_2010_est = signif( sumyear(varname, '2010', type), 3 )[,1]
    df$ACS_2010_moe = signif( sumyear(varname, '2010', type), 3 )[,2]    
  }
  df$ACS_2013_est = signif( sumyear(varname, '2013', type), 3 )[,1]
  df$ACS_2013_moe = signif( sumyear(varname, '2013', type), 3 )[,2]    
  df$ACS_2015_est = signif( sumyear(varname, '2015', type), 3 )[,1]
  df$ACS_2015_moe = signif( sumyear(varname, '2015', type), 3 )[,2]    
  df$ACS_2018_est = signif( sumyear(varname, '2018', type), 3 )[,1]
  df$ACS_2018_moe = signif( sumyear(varname, '2018', type), 3 )[,2]    
  df$ACS_2019_est = signif( sumyear(varname, '2019', type), 3 )[,1]
  df$ACS_2019_moe = signif( sumyear(varname, '2019', type), 3 )[,2]    
  names(df) <- c("","2006-2010 ACS Est","2006-2010 ACS MOE",
                 "2009-2013 ACS Est", "2009-2013 ACS MOE",
                 "2011-2015 ACS Est", "2011-2015 ACS MOE",
                 "2014-2018 ACS Est", "2014-2018 ACS MOE",
                 "2015-2019 ACS Est", "2015-2019 ACS MOE")
  return(df)
}

#test <- sumtable('renters','person')
#sumyear('renters', '2010', 'person')

fwrite( sumtable('renters','person'), file='summaryMOE/renters.csv' )
fwrite( sumtable('hs_or_less','person'), file='summaryMOE/hs_or_less.csv' )
fwrite( sumtable('poverty','person'), file='summaryMOE/poverty.csv' )
fwrite( sumtable('age_65_older','person'), file='summaryMOE/age_65_older.csv' )
fwrite( sumtable('hispanic','person'), file='summaryMOE/hispanic.csv' )
fwrite( sumtable('black','person'), file='summaryMOE/black.csv' )
fwrite( sumtable('foreign','person'), file='summaryMOE/foreign.csv' )
fwrite( sumtable('unemployment','person'), file='summaryMOE/unemployment.csv' )

# NOTES: for family, income, value use housing weights instead of person-weights
fwrite( sumtable('family','household'), file='summaryMOE/family.csv' )

medinc <- sumtable('median_income','household')
medinc[,c(2,4,6,8,10)] <- 1000*medinc[,c(2,4,6,8,10)]
fwrite( medinc, file='summaryMOE/median_income.csv' )

medval <- sumtable('median_value','household')
medval[,c(2,4,6,8,10)] <- 1000*medval[,c(2,4,6,8,10)]
fwrite( medval, file='summaryMOE/median_value.csv' )

