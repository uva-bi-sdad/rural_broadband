# summarize ACS variables by Census Tract
# include %area covered by BIP regions, %within buffer distance (5, 10, 25, 50 mi), RUCC and RUCA codes

library(DBI)
library(maps)
library(sf)
library(data.table)
library(dplyr)

options(scipen=999)
# load("~/git/rural_broadband/src/BIPsummaries/bip_acs.RData")

# --------------------------------------------------------------------------------------------
# read in Census tracts
# --------------------------------------------------------------------------------------------

con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                      dbname = "gis",
                      host = "postgis_1",
                      port = "5432",
                      user = "jrg3bs",
                      password = "jrg3bs")

# continental + AK, HI state fips
state_fips <- c( unique(state.fips$fips), 2, 15 )
state_fips <- formatC(state_fips,width=2,flag=0)
tracts <- list()
for(i in 1:length(state_fips)){
  tracts[[i]] <- sf::st_read(con,c("census_cb",paste0("cb_2018_",state_fips[i],"_tract_500k")))
}

# tracts_us <- rbind(tracts) # failed?
tracts_us <- tracts[[1]]
for(i in 2:length(tracts)){
  tracts_us <- rbind(tracts_us,tracts[[i]])
}

# --------------------------------------------------------------------------------------------
# read in BIP shapefiles (nationally)
# --------------------------------------------------------------------------------------------

bip <- readRDS("~/git/rural_broadband/data/working/BIP_working/BIP_Approved.RDS")
# length(unique(bip$APPLICATIO))
# 217 unique applications
bip_all <- st_union(bip)

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

# --------------------------------------------------------------------------------------------
# also attach county-level RUCC code, tract-level RUCA code
# --------------------------------------------------------------------------------------------

# join RUCC codes by county
rucc2013 <- fread("~/git/rural_broadband/data/RUCC2013/ruralurbancodes2013.csv")

tracts_us$GEOID5 <- as.numeric( substr(tracts_us$GEOID,1,5) )
tracts_us <- tracts_us %>% left_join(rucc2013 %>% dplyr::select(FIPS,RUCC_2013), by=c("GEOID5"="FIPS"))
# note 4 tracts do not join; County geography changes from 2013 to 2018 (02158, 46102)

ruca2010 <- fread("~/git/rural_broadband/data/RUCA2010/ruca2010revised.csv",
                  colClasses = c(GEOID="character"))
ruca2010 <- ruca2010 %>% dplyr::select(GEOID,RUCA_2010=RUCA2010,RUCA_2010_SECONDARY='Secondary RUCA2010')
tracts_us <- tracts_us %>% left_join(ruca2010, by=c("GEOID"))

# --------------------------------------------------------------------------------------------
# call 2017 5-year ACS variables (centered at 2015)
# -----------------------

# state_fips <- unique(fips_codes$state)[1:51]

# see brookingsOLS.R, propertyvalues.R
# additional variables:
# B19013 MEDIAN INCOME (1 column)
# B25077 MEDIAN VALUE (dollars) of Owner Occupied Housing Units (1 column)
# B23025 EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER (003=civilian labor force, 005=unemployed)
library(tidycensus)

census_api_key("853b2a1e71aa0aff0f3db966545e8898c73f0772")

acs_vars <- c("B15003_001","B15003_002","B15003_003","B15003_004","B15003_005","B15003_006","B15003_007","B15003_008","B15003_009",
              "B15003_010","B15003_011","B15003_012","B15003_013","B15003_014","B15003_015","B15003_016","B15003_017","B15003_018",
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

# get a the string with all ACS variables and call them at the tract level; loop over states
#test <- get_acs(geography="tract",state=state_fips[1],variables=acs_vars,year=2017,cache_table=TRUE,output="wide")
#test <- get_acs(geography="tract",state=state_fips[1],variables=acs_vars,year=2012,cache_table=TRUE,output="wide")
# these variables work for both periods
tmp <- list()
for(i in 1:length(state_fips)){
  tmp[[i]] <- get_acs(geography="tract",state=state_fips[i],variables=acs_vars,year=2017,cache_table=TRUE,output="wide")
}
acs_est_2015 <- rbindlist(tmp)

acs_estimates_2015 <- acs_est_2015 %>% transmute(
  GEOID=GEOID,
  population = B01001_001E,
  hs_or_less = (B15003_002E+B15003_003E+B15003_004E+B15003_005E+B15003_006E+B15003_007E+B15003_008E+B15003_009E+B15003_010E+
                  B15003_011E+B15003_012E+B15003_013E+B15003_014E+B15003_015E+B15003_016E+B15003_017E+B15003_018E) / B15003_001E,
  poverty = B17001_002E / B17001_001E,
  age_65_older = (B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E+
                    B01001_044E+B01001_045E+B01001_046E+B01001_047E+B01001_048E+B01001_049E)/ B01001_001E,
  hispanic = B03003_003E / B03003_001E,
  black = B02001_003E / B02001_001E,
  family = B09019_003E / B09019_002E,
  foreign = B05002_013E / B05002_001E,
  median_income = B19013_001E,
  unemployment = B23025_005E/B23025_003E,
  median_value = B25077_001E,
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

# --------------------------------------------------------------------------------------------
# call 2012 5-year ACS variables (centered at 2010)
# --------------------------------------------------------------------------------------------

# same code as above but with year=2012
tmp <- list()
for(i in 1:length(state_fips)){
  tmp[[i]] <- get_acs(geography="tract",state=state_fips[i],variables=acs_vars,year=2012,cache_table=TRUE,output="wide")
}
acs_est_2010 <- rbindlist(tmp)

acs_estimates_2010 <- acs_est_2010 %>% transmute(
  GEOID=GEOID,
  population = B01001_001E,
  hs_or_less = (B15003_002E+B15003_003E+B15003_004E+B15003_005E+B15003_006E+B15003_007E+B15003_008E+B15003_009E+B15003_010E+
                  B15003_011E+B15003_012E+B15003_013E+B15003_014E+B15003_015E+B15003_016E+B15003_017E+B15003_018E) / B15003_001E,
  poverty = B17001_002E / B17001_001E,
  age_65_older = (B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E+
                    B01001_044E+B01001_045E+B01001_046E+B01001_047E+B01001_048E+B01001_049E)/ B01001_001E,
  hispanic = B03003_003E / B03003_001E,
  black = B02001_003E / B02001_001E,
  family = B09019_003E / B09019_002E,
  foreign = B05002_013E / B05002_001E,
  median_income = B19013_001E,
  unemployment = B23025_005E/B23025_003E,
  median_value = B25077_001E,
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

# join both to tracts_us (left join on GEOID)
names(acs_estimates_2015)[2:length(names(acs_estimates_2015))] <-
  paste0(names(acs_estimates_2015)[2:length(names(acs_estimates_2015))],"_2015")
names(acs_estimates_2010)[2:length(names(acs_estimates_2010))] <-
  paste0(names(acs_estimates_2010)[2:length(names(acs_estimates_2010))],"_2010")

tracts_us2 <- tracts_us %>% left_join(acs_estimates_2010, by="GEOID")
# adjust 2012 5-year data on income, property value to 2017 dollars
# (https://www.usinflationcalculator.com/; 1.068)
tracts_us2$median_income_2010_adj <- round(tracts_us2$median_income * 1.068,0)
tracts_us2$median_value_2010_adj <- round(tracts_us2$median_value * 1.068,0)

tracts_us2 <- tracts_us2 %>% left_join(acs_estimates_2015, by="GEOID")
tracts_us2 <- dplyr::select(as.data.frame(tracts_us2), -geometry) # convert to data frame
tracts_us2[is.na(tracts_us2)] <- NA # convert NaN to NA for consistency

# --------------------------------------------------------------------------------------------
# summarize ACS tract distributions:
# 2015 BIP, nonBIP, close to BIP (5,10,25,50), >75% rural, <75% rural, RUCC
# 2010 BIP, nonBIP, close to BIP (5,10,25,50), >75% rural, <75% rural, RUCC
# --------------------------------------------------------------------------------------------

computeACSsummary2010 <- function(df,source,weights,weights_hh){
  df$weights <- weights
  df$weights_hh <- weights_hh
  df <- df %>% filter(!is.na(weights),!is.na(weights_hh))
  summaryvars <- data.frame(
    source=source,
    hs_or_less_2010 = as.numeric(df %>% filter(!is.na(hs_or_less_2010)) %>% summarize(hs_or_less_2010=sum(weights*hs_or_less_2010)/sum(weights))),
    poverty_2010 = as.numeric(df %>% filter(!is.na(poverty_2010)) %>% summarize(poverty_2010=sum(weights*poverty_2010)/sum(weights))),
    age_65_older_2010 = as.numeric(df %>% filter(!is.na(age_65_older_2010)) %>% summarize(age_65_older_2010=sum(weights*age_65_older_2010)/sum(weights))),
    hispanic_2010 = as.numeric(df %>% filter(!is.na(hispanic_2010)) %>% summarize(hispanic_2010=sum(weights*hispanic_2010)/sum(weights))),
    black_2010 = as.numeric(df %>% filter(!is.na(black_2010)) %>% summarize(black_2010=sum(weights*black_2010)/sum(weights))),
    foreign_2010 = as.numeric(df %>% filter(!is.na(foreign_2010)) %>% summarize(foreign_2010=sum(weights*foreign_2010)/sum(weights))),
    unemployment_2010 = as.numeric(df %>% filter(!is.na(unemployment_2010)) %>% summarize(unemployment_2010=sum(weights*unemployment_2010)/sum(weights))),
    family_2010 = as.numeric(df %>% filter(!is.na(family_2010)) %>% summarize(family_2010=sum(weights_hh*family_2010)/sum(weights_hh))),
    median_income_2010 = as.numeric(df %>% filter(!is.na(median_income_2010_adj)) %>% summarize(median_income_2010_adj=sum(weights_hh*median_income_2010_adj)/sum(weights_hh))),
    median_value_2010 = as.numeric(df %>% filter(!is.na(median_value_2010_adj)) %>% summarize(median_value_2010_adj=sum(weights_hh*median_value_2010_adj)/sum(weights_hh)))
  )
  summaryvars
}

computeACSsummary2015 <- function(df,source,weights,weights_hh){
  df$weights <- weights
  df$weights_hh <- weights_hh
  df <- df %>% filter(!is.na(weights),!is.na(weights_hh))
  summaryvars <- data.frame(
    source=source,
    hs_or_less_2015 = as.numeric(df %>% filter(!is.na(hs_or_less_2015)) %>% summarize(hs_or_less_2015=sum(weights*hs_or_less_2015)/sum(weights))),
    poverty_2015 = as.numeric(df %>% filter(!is.na(poverty_2015)) %>% summarize(poverty_2015=sum(weights*poverty_2015)/sum(weights))),
    age_65_older_2015 = as.numeric(df %>% filter(!is.na(age_65_older_2015)) %>% summarize(age_65_older_2015=sum(weights*age_65_older_2015)/sum(weights))),
    hispanic_2015 = as.numeric(df %>% filter(!is.na(hispanic_2015)) %>% summarize(hispanic_2015=sum(weights*hispanic_2015)/sum(weights))),
    black_2015 = as.numeric(df %>% filter(!is.na(black_2015)) %>% summarize(black_2015=sum(weights*black_2015)/sum(weights))),
    foreign_2015 = as.numeric(df %>% filter(!is.na(foreign_2015)) %>% summarize(foreign_2015=sum(weights*foreign_2015)/sum(weights))),
    unemployment_2015 = as.numeric(df %>% filter(!is.na(unemployment_2015)) %>% summarize(unemployment_2015=sum(weights*unemployment_2015)/sum(weights))),
    family_2015 = as.numeric(df %>% filter(!is.na(family_2015)) %>% summarize(family_2015=sum(weights_hh*family_2015)/sum(weights_hh))),
    median_income_2015 = as.numeric(df %>% filter(!is.na(median_income_2015)) %>% summarize(median_income_2015=sum(weights_hh*median_income_2015)/sum(weights_hh))),
    median_value_2015 = as.numeric(df %>% filter(!is.na(median_value_2015)) %>% summarize(median_value_2015=sum(weights_hh*median_value_2015)/sum(weights_hh)))
  )
  summaryvars
}

# compute ACS summaries; weight by person or household variables
acs_2010_summaries <- rbind(
  computeACSsummary2010(df=tracts_us2, source="National",
                        weights=tracts_us2$population_2010,
                        weights_hh=tracts_us2$hunits_total_2010
  ),
  computeACSsummary2010(df=tracts_us2, source="BIP Regions",
                        weights=tracts_us2$population_2010 *(tracts_us2$BIP_PROP_AREA),
                        weights_hh=tracts_us2$hunits_total_2010*(tracts_us2$BIP_PROP_AREA)
  ),
  computeACSsummary2010(df=tracts_us2, source="BIP Within 5MI",
                        weights=tracts_us2$population_2010 *(tracts_us2$BIP_PROP_5MI),
                        weights_hh=tracts_us2$hunits_total_2010*(tracts_us2$BIP_PROP_5MI)
  ),
  computeACSsummary2010(df=tracts_us2, source="BIP Within 10MI",
                        weights=tracts_us2$population_2010 *(tracts_us2$BIP_PROP_10MI),
                        weights_hh=tracts_us2$hunits_total_2010*(tracts_us2$BIP_PROP_10MI)
  ),
  computeACSsummary2010(df=tracts_us2, source="BIP Within 25MI",
                        weights=tracts_us2$population_2010 *(tracts_us2$BIP_PROP_25MI),
                        weights_hh=tracts_us2$hunits_total_2010*(tracts_us2$BIP_PROP_25MI)
  ),
  computeACSsummary2010(df=tracts_us2, source="BIP Within 50MI",
                        weights=tracts_us2$population_2010 *(tracts_us2$BIP_PROP_50MI),
                        weights_hh=tracts_us2$hunits_total_2010*(tracts_us2$BIP_PROP_50MI)
  ),
  computeACSsummary2010(df=tracts_us2, source="Rural (RUCC 7-9)",
                        weights=tracts_us2$population_2010 *(tracts_us2$RUCC_2013 >= 7),
                        weights_hh=tracts_us2$hunits_total_2010*(tracts_us2$RUCC_2013 >= 7)
  ),
  computeACSsummary2010(df=tracts_us2, source="Rural (RUCA 7-10)",
                        weights=tracts_us2$population_2010 *(tracts_us2$RUCA_2010 >= 7),
                        weights_hh=tracts_us2$hunits_total_2010*(tracts_us2$RUCA_2010 >= 7)
  ),
  computeACSsummary2010(df=tracts_us2, source="Urban (RUCC 1-6)",
                        weights=tracts_us2$population_2010 *(tracts_us2$RUCC_2013 <= 6),
                        weights_hh=tracts_us2$hunits_total_2010*(tracts_us2$RUCC_2013 <= 6)
  ),
  computeACSsummary2010(df=tracts_us2, source="Urban (RUCA 1-6)",
                        weights=tracts_us2$population_2010 *(tracts_us2$RUCA_2010 <= 6),
                        weights_hh=tracts_us2$hunits_total_2010*(tracts_us2$RUCA_2010 <= 6)
  )
)

acs_2015_summaries <- rbind(
  computeACSsummary2015(df=tracts_us2, source="National",
                        weights=tracts_us2$population_2015,
                        weights_hh=tracts_us2$hunits_total_2015
  ),
  computeACSsummary2015(df=tracts_us2, source="BIP Regions",
                        weights=tracts_us2$population_2015 *(tracts_us2$BIP_PROP_AREA),
                        weights_hh=tracts_us2$hunits_total_2015*(tracts_us2$BIP_PROP_AREA)
  ),
  computeACSsummary2015(df=tracts_us2, source="BIP Within 5MI",
                        weights=tracts_us2$population_2015 *(tracts_us2$BIP_PROP_5MI),
                        weights_hh=tracts_us2$hunits_total_2015*(tracts_us2$BIP_PROP_5MI)
  ),
  computeACSsummary2015(df=tracts_us2, source="BIP Within 10MI",
                        weights=tracts_us2$population_2015 *(tracts_us2$BIP_PROP_10MI),
                        weights_hh=tracts_us2$hunits_total_2015*(tracts_us2$BIP_PROP_10MI)
  ),
  computeACSsummary2015(df=tracts_us2, source="BIP Within 25MI",
                        weights=tracts_us2$population_2015 *(tracts_us2$BIP_PROP_25MI),
                        weights_hh=tracts_us2$hunits_total_2015*(tracts_us2$BIP_PROP_25MI)
  ),
  computeACSsummary2015(df=tracts_us2, source="BIP Within 50MI",
                        weights=tracts_us2$population_2015 *(tracts_us2$BIP_PROP_50MI),
                        weights_hh=tracts_us2$hunits_total_2015*(tracts_us2$BIP_PROP_50MI)
  ),
  computeACSsummary2015(df=tracts_us2, source="Rural (RUCC 7-9)",
                        weights=tracts_us2$population_2015 *(tracts_us2$RUCC_2013 >= 7),
                        weights_hh=tracts_us2$hunits_total_2015*(tracts_us2$RUCC_2013 >= 7)
  ),
  computeACSsummary2015(df=tracts_us2, source="Rural (RUCA 7-10)",
                        weights=tracts_us2$population_2015 *(tracts_us2$RUCA_2010 >= 7),
                        weights_hh=tracts_us2$hunits_total_2015*(tracts_us2$RUCA_2010 >= 7)
  ),
  computeACSsummary2015(df=tracts_us2, source="Urban (RUCC 1-6)",
                        weights=tracts_us2$population_2015 *(tracts_us2$RUCC_2013 <= 6),
                        weights_hh=tracts_us2$hunits_total_2015*(tracts_us2$RUCC_2013 <= 6)
  ),
  computeACSsummary2015(df=tracts_us2, source="Urban (RUCA 1-6)",
                        weights=tracts_us2$population_2015 *(tracts_us2$RUCA_2010 <= 6),
                        weights_hh=tracts_us2$hunits_total_2015*(tracts_us2$RUCA_2010 <= 6)
  )
)

acs_2010_summaries[,2:ncol(acs_2010_summaries)] <- signif(acs_2010_summaries[,2:ncol(acs_2010_summaries)],3)
acs_2015_summaries[,2:ncol(acs_2015_summaries)] <- signif(acs_2015_summaries[,2:ncol(acs_2015_summaries)],3)

tracts_us3 <- tracts_us2
tracts_us3[,11:ncol(tracts_us3)] <- round(tracts_us3[,11:ncol(tracts_us3)],3)
tracts_us3 <- tracts_us3[,c(1:30,59:71)] %>% select(-c("NAME","GEOID5","LSAD","AFFGEOID"))

fwrite(acs_2010_summaries,file="~/git/rural_broadband/src/BIPsummaries/acs_2010_summaries.csv")
fwrite(acs_2015_summaries,file="~/git/rural_broadband/src/BIPsummaries/acs_2015_summaries.csv")
fwrite(tracts_us3,file="~/git/rural_broadband/src/BIPsummaries/ACS_tracts.csv")

# save.image("~/git/rural_broadband/src/BIPsummaries/bip_acs.RData")
