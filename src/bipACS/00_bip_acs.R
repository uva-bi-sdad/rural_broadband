# update 4/13/20 JG
# summarize ACS variables by Census Tract
# include %area covered by BIP regions, %within buffer distance (5, 10, 25, 50 mi), RUCC and RUCA codes

# update:
# other shapefiles for RUS programs? what data do we have?
# link BIP shapefiles to RUS program data; join program information

library(DBI)
library(maps)
library(sf)
library(data.table)
library(dplyr)

options(scipen=999)
setwd("~/git/rural_broadband/src/bipACS")
# load("~/git/rural_broadband/src/bipACS/bip_acs.RData")

# --------------------------------------------------------------------------------------------
# read in Census tracts
# --------------------------------------------------------------------------------------------

get_db_conn <-
  function(db_name = "sdad",
           db_host = "postgis1",
           db_port = "5432",
           db_user = Sys.getenv("db_usr"),
           db_pass = Sys.getenv("db_pwd")) {
    RPostgreSQL::dbConnect(
      drv = RPostgreSQL::PostgreSQL(),
      dbname = db_name,
      host = db_host,
      port = db_port,
      user = db_user,
      password = db_pass
    )
  }

con <- get_db_conn()

# continental + AK, HI state fips
state_fips <- c( unique(state.fips$fips), 2, 15 )
state_fips <- formatC(state_fips,width=2,flag=0)
tracts <- list()
for(i in 1:length(state_fips)){
  tracts[[i]] <- sf::st_read(con,c("gis_census_cb",paste0("cb_2018_",state_fips[i],"_tract_500k")))
}

tracts_us <- do.call(rbind, tracts)

# --------------------------------------------------------------------------------------------
# read in BIP shapefiles (nationally)
# --------------------------------------------------------------------------------------------

load("BIP_New.rds")
bip_all <- newbip_all
bip <- newbip_union

# use st_buffer to get area around shapefiles; then st_difference to remove the area in BIP
m_per_mi <- 1609.34 # converting meters to miles

bip_5mi_buffer <- st_buffer(bip_all, dist = 5*m_per_mi)
bip_5mi <- st_difference(bip_5mi_buffer, bip_all)

bip_10mi_buffer <- st_buffer(bip_all, dist = 10*m_per_mi)
bip_10mi <- st_difference(bip_10mi_buffer, bip_all)

bip_25mi_buffer <- st_buffer(bip_all, dist = 25*m_per_mi)
bip_25mi <- st_difference(bip_25mi_buffer, bip_all)

bip_50mi_buffer <- st_buffer(bip_all, dist = 50*m_per_mi)
bip_50mi <- st_difference(bip_50mi_buffer, bip_all)

# --------------------------------------------------------------------------------------------
# get %area intersection between BIP and tracts
# --------------------------------------------------------------------------------------------

tracts_us <- st_transform(tracts_us, st_crs(bip))

# find which intersect and get the proportional area
ind_intersect <- which(lengths(st_intersects(tracts_us,bip)) > 0)
prop_area_intersect <- as.numeric( st_area( st_intersection(tracts_us[ind_intersect,],bip_all) ) / st_area( tracts_us[ind_intersect,] ) )

tracts_us$BIP_IN <- "FALSE"; tracts_us$BIP_IN[ind_intersect] <- "TRUE"
tracts_us$BIP_PROP_AREA <- 0; tracts_us$BIP_PROP_AREA[ind_intersect] <- prop_area_intersect

# repeat for buffers (5, 10, 25, 50 miles)
ind_5mi <- which(lengths(st_intersects(tracts_us,st_buffer(bip, dist = 5*m_per_mi))) > 0)
intersect_5mi <- st_intersection(tracts_us[ind_5mi,],bip_5mi)
prop_area_5mi <- as.numeric( st_area(intersect_5mi) / st_area( tracts_us %>% filter(GEOID %in% intersect_5mi$GEOID) ) )
tracts_us$BIP_PROP_5MI <- 0; tracts_us$BIP_PROP_5MI[tracts_us$GEOID %in% intersect_5mi$GEOID] <- prop_area_5mi

ind_10mi <- which(lengths(st_intersects(tracts_us,st_buffer(bip, dist = 10*m_per_mi))) > 0)
intersect_10mi <- st_intersection(tracts_us[ind_10mi,],bip_10mi)
prop_area_10mi <- as.numeric( st_area(intersect_10mi) / st_area( tracts_us %>% filter(GEOID %in% intersect_10mi$GEOID) ) )
tracts_us$BIP_PROP_10MI <- 0; tracts_us$BIP_PROP_10MI[tracts_us$GEOID %in% intersect_10mi$GEOID] <- prop_area_10mi

ind_25mi <- which(lengths(st_intersects(tracts_us,st_buffer(bip, dist = 25*m_per_mi))) > 0)
intersect_25mi <- st_intersection(tracts_us[ind_25mi,],bip_25mi)
prop_area_25mi <- as.numeric( st_area(intersect_25mi) / st_area( tracts_us %>% filter(GEOID %in% intersect_25mi$GEOID) ) )
tracts_us$BIP_PROP_25MI <- 0; tracts_us$BIP_PROP_25MI[tracts_us$GEOID %in% intersect_25mi$GEOID] <- prop_area_25mi

ind_50mi <- which(lengths(st_intersects(tracts_us,st_buffer(bip, dist = 50*m_per_mi))) > 0)
intersect_50mi <- st_intersection(tracts_us[ind_50mi,],bip_50mi)
prop_area_50mi <- as.numeric( st_area(intersect_50mi) / st_area( tracts_us %>% filter(GEOID %in% intersect_50mi$GEOID) ) )
tracts_us$BIP_PROP_50MI <- 0; tracts_us$BIP_PROP_50MI[tracts_us$GEOID %in% intersect_50mi$GEOID] <- prop_area_50mi


# save.image("~/git/rural_broadband/src/bipACS/bip_acs.RData")

# --------------------------------------------------------------------------------------------
# attach county-level RUCC code, tract-level RUCA code
# --------------------------------------------------------------------------------------------

# join RUCC codes by county
rucc2013 <- fread("~/git/rural_broadband/dat/RUCC2013/ruralurbancodes2013.csv")

tracts_us$GEOID5 <- as.numeric( substr(tracts_us$GEOID,1,5) )
tracts_us <- tracts_us %>% left_join(rucc2013 %>% dplyr::select(FIPS,RUCC_2013), by=c("GEOID5"="FIPS"))
# note 4 tracts do not join; County geography changes from 2013 to 2018 (02158, 46102)

ruca2010 <- fread("~/git/rural_broadband/dat/RUCA2010/ruca2010revised.csv",
                  colClasses = c(GEOID="character"))
ruca2010 <- ruca2010 %>% dplyr::select(GEOID,RUCA_2010=RUCA2010,RUCA_2010_SECONDARY='Secondary RUCA2010')
tracts_us <- tracts_us %>% left_join(ruca2010, by=c("GEOID"))

# --------------------------------------------------------------------------------------------
# call several sets of ACS estimates for Census tract by year
# --------------------------------------------------------------------------------------------

library(tidycensus)

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
              "B25035_001", "B25041_001", "B25041_002", "B25041_003", "B25041_004", "B25041_005", "B25041_006", "B25041_007", "B25064_001", "B25065_001")

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
                   "B25035_001", "B25041_001", "B25041_002", "B25041_003", "B25041_004", "B25041_005", "B25041_006", "B25041_007", "B25064_001", "B25065_001")

# get ACS variables for a given year for all US census tracts
# for 2006-2010 5-year don't include family, employment (no tables in API)
get_acs_vars <- function(year, vars){
  tmp <- list()
  for(i in 1:length(state_fips)){
    tmp[[i]] <- get_acs(geography="tract",state=state_fips[i],variables=vars,year=year,cache_table=TRUE,output="wide")
  }
  acs_est <- rbindlist(tmp)

  acs_estimates <- acs_est %>% transmute(
    GEOID=GEOID,
    population = B01001_001E,
    hs_or_less = (B15002_003E+B15002_004E+B15002_005E+B15002_006E+B15002_007E+B15002_008E+B15002_009E+B15002_010E+B15002_011E+
                    B15002_020E+B15002_021E+B15002_022E+B15002_023E+B15002_024E+B15002_025E+B15002_026E+B15002_027E+B15002_028E) / B15002_001E,
    poverty = B17001_002E / B17001_001E,
    age_65_older = (B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E+
                      B01001_044E+B01001_045E+B01001_046E+B01001_047E+B01001_048E+B01001_049E)/ B01001_001E,
    hispanic = B03003_003E / B03003_001E,
    black = B02001_003E / B02001_001E,
    foreign = B05002_013E / B05002_001E,
    median_income = B19013_001E,
    median_value = B25077_001E,
    renters = B25003_003E / B25003_001E,
    median_yrbuilt = B25035_001E,
    bed_total = B25041_001E,
    bed_none = B25041_002E,
    bed_1 = B25041_003E,
    bed_2 = B25041_004E,
    bed_3 = B25041_005E,
    bed_4 = B25041_006E,
    bed_5plus= B25041_007E,
    rent_mediangross = B25064_001E, 
    rent_aggreggross = B25065_001E,
    hunits_total = B25001_001E,
    occstatus_total = B25002_001E,
    occstatus_occup = B25002_002E,
    occstatus_vac = B25002_003E,
    tenure_total= B25003_001E,
    tenure_own = B25003_002E,
    tenure_rent = B25003_003E,
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
    unitno_other = B25024_011E
  )
  if(year >= 2012) { acs_estimates <- cbind(acs_estimates,
                                            acs_est %>% transmute(  
                                              family = B09019_003E / B09019_002E,
                                              unemployment = B23025_005E/B23025_003E
                                            )
  )}
  if(year >= 2019) {
    acs_estimates$family <- B09019_003E / B09019_002E
  }
  names(acs_estimates)[2:length(names(acs_estimates))] <-
    paste0(names(acs_estimates)[2:length(names(acs_estimates))],"_",year)
  return(acs_estimates)  
}

acs_estimates_2018 <- get_acs_vars(2018, acs_vars)
acs_estimates_2015 <- get_acs_vars(2015, acs_vars)
acs_estimates_2013 <- get_acs_vars(2013, acs_vars)
acs_estimates_2010 <- get_acs_vars(2010, acs_vars_2010)


tracts_us2 <- tracts_us %>% left_join(acs_estimates_2010, by="GEOID")
tracts_us2 <- tracts_us2 %>% left_join(acs_estimates_2013, by="GEOID")
tracts_us2 <- tracts_us2 %>% left_join(acs_estimates_2015, by="GEOID")
tracts_us2 <- tracts_us2 %>% left_join(acs_estimates_2018, by="GEOID")
tracts_us2 <- dplyr::select(as.data.frame(tracts_us2), -geometry) # convert to data frame
tracts_us2[is.na(tracts_us2)] <- NA # convert NaN to NA for consistency

# adjust all income and home values to 2019 dollars
# (using BLS CPI data at https://www.usinflationcalculator.com/)
adj_2019 <- 1
adj_2018 <- 1.018
adj_2015 <- 1.079
adj_2013 <- 1.097
adj_2010 <- 1.172

tracts_us2$median_income_adj_2019 <- round(tracts_us2$median_income_2019 * adj_2019,0)
tracts_us2$median_value_adj_2019 <- round(tracts_us2$median_value_2019 * adj_2019,0)
tracts_us2$median_income_adj_2018 <- round(tracts_us2$median_income_2018 * adj_2018,0)
tracts_us2$median_value_adj_2018 <- round(tracts_us2$median_value_2018 * adj_2018,0)
tracts_us2$median_income_adj_2015 <- round(tracts_us2$median_income_2015 * adj_2015,0)
tracts_us2$median_value_adj_2015 <- round(tracts_us2$median_value_2015 * adj_2015,0)
tracts_us2$median_income_adj_2013 <- round(tracts_us2$median_income_2013 * adj_2013,0)
tracts_us2$median_value_adj_2013 <- round(tracts_us2$median_value_2013 * adj_2013,0)
tracts_us2$median_income_adj_2010 <- round(tracts_us2$median_income_2010 * adj_2010,0)
tracts_us2$median_value_adj_2010 <- round(tracts_us2$median_value_2010 * adj_2010,0)


# save.image("~/git/rural_broadband/src/bipACS/bip_acs.RData")

# --------------------------------------------------------------------------------------------
# join FCC-ACS congruence
# --------------------------------------------------------------------------------------------

# NOTE: replace with Teja's new results (ADD 3 VARIABLES AS PER NOTES)

fcc_acs_congruent <- fread("~/git/rural_broadband/src/bipACS/fcc_acs_congruent.csv",
                           colClasses = c(GEOID="character"))
tracts_us3 <- tracts_us2 %>% left_join(fcc_acs_congruent, by="GEOID")

#table(tracts_us3$acs_within_fcc)
#0     1 
#32203 40510 

# --------------------------------------------------------------------------------------------
# join FCC-ACS subscription rates (at 200 kbps)
# --------------------------------------------------------------------------------------------

fcc2011_subscription <- fread("~/git/rural_broadband/src/bipACS/fcc2011_subscription.csv",
                              colClasses = c(GEOID="character"))
fcc2016_subscription <- fread("~/git/rural_broadband/src/bipACS/fcc2016_subscription.csv",
                              colClasses = c(GEOID="character"))

tracts_us4 <- tracts_us3 %>% left_join(fcc2011_subscription, by="GEOID")
tracts_us5 <- tracts_us4 %>% left_join(fcc2016_subscription, by="GEOID")

# --------------------------------------------------------------------------------------------
# attach 2011 provider counts
# --------------------------------------------------------------------------------------------

fcc2011_providers <- fread("~/git/rural_broadband/src/bipACS/fcc2011_providers.csv",
                           colClasses = c(GEOID="character"))

# total_residential_prov (200kbps), total_residential_prov_nbp (3Mbps)
# note: skip provider counts for 2016 (hard to come up with a consistent
#   tract-level definition from 2011 to 2016 with available data)
# note: '1' is coded as 1 to 3
tracts_us6 <- tracts_us5 %>% left_join(fcc2011_providers, by="GEOID")

# --------------------------------------------------------------------------------------------
# attach eligibility
# --------------------------------------------------------------------------------------------

# simple criterion for eligibility by tract:
#   providers (3Mbps) in 2011 is < 3, and the tract is rural (RUCA >=4, non-metropolitan)
#   see e.g. https://www.rd.usda.gov/programs-services/rural-broadband-access-loan-and-loan-guarantee

# load eligible %area by tract from 04_bip_eligibility.R
eligible_broadband <- tracts_us6$RUCA_2010 >=4 & tracts_us6$fcc2011_providers_3 <= 2
table(eligible_broadband)

eligible_infrastructure <- tracts_us6$RUCA_2010 >=4 & tracts_us6$fcc2011_providers_3 <= 1
table(eligible_infrastructure)

#tracts_us7$eligible_broadband <- tracts_us7$RUS_PROP_URBAN==0 & tracts_us7$fcc2011_providers_3 <= 2
#tracts_us7$eligible_infrastructure <- tracts_us7$RUS_PROP_URBAN==0 & tracts_us7$fcc2011_providers_3 <= 1
tracts_us7 <- tracts_us6

tracts_us7$RUCA_RURAL <- 1*(tracts_us7$RUCA_2010 >= 7)
tracts_us7$RUCA_URBAN <- 1*(tracts_us7$RUCA_2010 <= 6)
tracts_us7 <- tracts_us7 %>% filter(!is.na(RUCA_2010))

# --------------------------------------------------------------------------------------------
# format and save final data
# --------------------------------------------------------------------------------------------

broadband_tract_data <- tracts_us7 %>% dplyr::select(
  STATEFP,
  COUNTYFP,
  TRACTCE,
  GEOID,
  ALAND,
  AWATER,
  BIP_IN,
  BIP_PROP_AREA,
  BIP_PROP_5MI,
  BIP_PROP_10MI,
  BIP_PROP_25MI,
  BIP_PROP_50MI,
  RUCC_2013=RUCC_2013,
  RUCA_2010=RUCA_2010,
  RUCA_2010_SECONDARY=RUCA_2010_SECONDARY,
  RUCA_RURAL,
  RUCA_URBAN,
  acs_within_fcc200,
  acs_within_fcc10,
  acs_within_fcc,
  fcc2011_200min,
  fcc2011_200max,
  fcc2016_200min,
  fcc2016_200max,
  fcc2011_providers_200,
  fcc2011_providers_3,

  population_2010,
  hunits_total_2010,
  hs_or_less_2010,
  poverty_2010,
  age_65_older_2010,
  hispanic_2010,
  black_2010,
  foreign_2010,
  median_income_2010,
  median_value_2010,
  median_income_adj_2010,
  median_value_adj_2010,

  population_2013,
  hunits_total_2013,
  hs_or_less_2013,
  poverty_2013,
  age_65_older_2013,
  hispanic_2013,
  black_2013,
  family_2013,
  foreign_2013,
  unemployment_2013,
  median_income_2013,
  median_value_2013,
  median_income_adj_2013,
  median_value_adj_2013,
  
  population_2015,
  hunits_total_2015,
  hs_or_less_2015,
  poverty_2015,
  age_65_older_2015,
  hispanic_2015,
  black_2015,
  family_2015,
  foreign_2015,
  unemployment_2015,
  median_income_2015,
  median_value_2015,
  median_income_adj_2015,
  median_value_adj_2015,
  
  population_2018,
  hunits_total_2018,
  hs_or_less_2018,
  poverty_2018,
  age_65_older_2018,
  hispanic_2018,
  black_2018,
  family_2018,
  foreign_2018,
  unemployment_2018,
  median_income_2018,
  median_value_2018
)

fwrite(broadband_tract_data, file="broadband_tract_data.csv")

# save.image("~/git/rural_broadband/src/bipACS/bip_acs.RData")

# --------------------------------------------------------------------------------------------
# check completeness
# --------------------------------------------------------------------------------------------

sort( colSums(is.na(broadband_tract_data)) )

# --------------------------------------------------------------------------------------------
# save summary tables
# --------------------------------------------------------------------------------------------

# save each variable as a separate .csv under summary/varname.csv
# column = ACS year, row = subpopulation

# general formula is:
# as.numeric(df %>% filter(!is.na(varname)) %>% summarize(varname=sum(weights*varname)/sum(weights)))

sumcompute <- function(df, varname, weights){
  # weights is a character vector of columns to multiply
  df_filter <- df[ !is.na(df[,varname]), ]
  for(i in 1:length(weights)){
    if(i==1) {
      w <- df_filter[,weights[1]]
    } else{
      w <- w*df_filter[,weights[i]]
    }
  }
  var <- df_filter[,varname]
  return( sum(w*var)/sum(w) )
}
#sumcompute(df = tracts_us7, varname = 'unemployment_2013', weights = 'population_2013')

# function to take varname as input and call sumcompute for all years, subpopulation
sumyear <- function(varname, year, type){
  if(type == "person") weightname <- 'population_'
  if(type == "household") weightname <- 'hunits_total_'
  return(c(
    sumcompute(tracts_us7, paste0(varname,"_",year), weights = paste0(weightname,year)),
    sumcompute(tracts_us7, paste0(varname,"_",year), weights = c('BIP_PROP_AREA',paste0(weightname,year))),
    sumcompute(tracts_us7, paste0(varname,"_",year), weights = c('BIP_PROP_5MI',paste0(weightname,year))),
    sumcompute(tracts_us7, paste0(varname,"_",year), weights = c('BIP_PROP_10MI',paste0(weightname,year))),
    sumcompute(tracts_us7, paste0(varname,"_",year), weights = c('BIP_PROP_25MI',paste0(weightname,year))),
    sumcompute(tracts_us7, paste0(varname,"_",year), weights = c('BIP_PROP_50MI',paste0(weightname,year))),
    sumcompute(tracts_us7, paste0(varname,"_",year), weights = c('RUCA_RURAL',paste0(weightname,year))),
    sumcompute(tracts_us7, paste0(varname,"_",year), weights = c('RUCA_URBAN',paste0(weightname,year)))
  ))
}

# format and write as .csv
sumtable <- function(varname, type){
  df <- data.frame(source = c("National", "BIP Regions", "BIP 5MI Donut", "BIP 10MI Donut",
                   "BIP 25MI Donut", "BIP 50MI Donut", "Rural (RUCA 7-10)", "Urban (RUCA 1-6)"))
  
  if(varname %in% c('unemployment','family')){
    df$ACS2010 <- NA
  } else{ df$ACS_2010 = signif( sumyear(varname, '2010', type), 3 ) }
  df$ACS_2013 = signif( sumyear(varname, '2013', type), 3 )
  df$ACS_2015 = signif( sumyear(varname, '2015', type), 3 )
  df$ACS_2018 = signif( sumyear(varname, '2018', type), 3 )
  names(df) <- c("","2006-2010 ACS", "2009-2013 ACS", "2011-2015 ACS", "2014-2018 ACS")
  return(df)
}

fwrite( sumtable('hs_or_less','person'), file='summary/hs_or_less.csv' )
fwrite( sumtable('poverty','person'), file='summary/poverty.csv' )
fwrite( sumtable('age_65_older','person'), file='summary/age_65_older.csv' )
fwrite( sumtable('hispanic','person'), file='summary/hispanic.csv' )
fwrite( sumtable('black','person'), file='summary/black.csv' )
fwrite( sumtable('foreign','person'), file='summary/foreign.csv' )
fwrite( sumtable('unemployment','person'), file='summary/unemployment.csv' )
# NOTES: for family, income, value use housing weights instead of person-weights
fwrite( sumtable('family','household'), file='summary/family.csv' )
fwrite( sumtable('median_income_adj','household'), file='summary/median_income.csv' )
fwrite( sumtable('median_value_adj','household'), file='summary/median_value.csv' )










# ----------------------------------------------------------------

library(DBI)
library(maps)
library(sf)
library(data.table)
library(dplyr)

options(scipen=999)
setwd("~/git/rural_broadband/src/bipACS")

library(tidycensus)
census_api_key("853b2a1e71aa0aff0f3db966545e8898c73f0772")

# add 2019 ACS data
load("bip_acs.RData")

acs_estimates_2019 <- get_acs_vars(2019, acs_vars)

tract_data_new <- tracts_us7 %>% left_join(acs_estimates_2019, by="GEOID")

# adjustments for inflation to 2019 dollars
adj_2019 <- 1
adj_2018 <- 1.018
adj_2015 <- 1.079
adj_2013 <- 1.097
adj_2010 <- 1.172

tract_data_new$median_income_adj_2019 <- round(tract_data_new$median_income_2019 * adj_2019,0)
tract_data_new$median_value_adj_2019 <- round(tract_data_new$median_value_2019 * adj_2019,0)
tract_data_new$median_income_adj_2018 <- round(tract_data_new$median_income_2018 * adj_2018,0)
tract_data_new$median_value_adj_2018 <- round(tract_data_new$median_value_2018 * adj_2018,0)
tract_data_new$median_income_adj_2015 <- round(tract_data_new$median_income_2015 * adj_2015,0)
tract_data_new$median_value_adj_2015 <- round(tract_data_new$median_value_2015 * adj_2015,0)
tract_data_new$median_income_adj_2013 <- round(tract_data_new$median_income_2013 * adj_2013,0)
tract_data_new$median_value_adj_2013 <- round(tract_data_new$median_value_2013 * adj_2013,0)
tract_data_new$median_income_adj_2010 <- round(tract_data_new$median_income_2010 * adj_2010,0)
tract_data_new$median_value_adj_2010 <- round(tract_data_new$median_value_2010 * adj_2010,0)



broadband_tract_data <- tract_data_new %>% dplyr::transmute(
  STATEFP,
  COUNTYFP,
  TRACTCE,
  GEOID,
  ALAND,
  AWATER,
  BIP_IN,
  BIP_PROP_AREA,
  BIP_PROP_5MI,
  BIP_PROP_10MI,
  BIP_PROP_25MI,
  BIP_PROP_50MI,
  RUCC_2013=RUCC_2013,
  RUCA_2010=RUCA_2010,
  RUCA_2010_SECONDARY=RUCA_2010_SECONDARY,
  RUCA_RURAL,
  RUCA_URBAN,
  acs_within_fcc200,
  acs_within_fcc10,
  acs_within_fcc,
  fcc2011_200min,
  fcc2011_200max,
  fcc2016_200min,
  fcc2016_200max,
  fcc2011_providers_200,
  fcc2011_providers_3,
  
  population_2010,
  hunits_total_2010,
  hs_or_less_2010,
  renters_2010 = tenure_rent_2010 / tenure_total_2010,
  poverty_2010,
  age_65_older_2010,
  hispanic_2010,
  black_2010,
  foreign_2010,
  median_income_2010,
  median_value_2010,
  median_income_adj_2010,
  median_value_adj_2010,
  
  population_2013,
  hunits_total_2013,
  hs_or_less_2013,
  renters_2013 = tenure_rent_2013 / tenure_total_2013,
  poverty_2013,
  age_65_older_2013,
  hispanic_2013,
  black_2013,
  family_2013,
  foreign_2013,
  unemployment_2013,
  median_income_2013,
  median_value_2013,
  median_income_adj_2013,
  median_value_adj_2013,
  
  population_2015,
  hunits_total_2015,
  hs_or_less_2015,
  renters_2015 = tenure_rent_2015 / tenure_total_2015,
  poverty_2015,
  age_65_older_2015,
  hispanic_2015,
  black_2015,
  family_2015,
  foreign_2015,
  unemployment_2015,
  median_income_2015,
  median_value_2015,
  median_income_adj_2015,
  median_value_adj_2015,
  
  population_2018,
  hunits_total_2018,
  hs_or_less_2018,
  renters_2018 = tenure_rent_2018 / tenure_total_2018,
  poverty_2018,
  age_65_older_2018,
  hispanic_2018,
  black_2018,
  family_2018,
  foreign_2018,
  unemployment_2018,
  median_income_2018,
  median_value_2018,
  median_income_adj_2018,
  median_value_adj_2018,

  population_2019,
  hunits_total_2019,
  hs_or_less_2019,
  renters_2019 = tenure_rent_2019 / tenure_total_2019,
  poverty_2019,
  age_65_older_2019,
  hispanic_2019,
  black_2019,
  family_2019,
  foreign_2019,
  unemployment_2019,
  median_income_2019,
  median_value_2019,
  median_income_adj_2019,
  median_value_adj_2019
)

fwrite(broadband_tract_data, file="broadband_tract_data.csv")


