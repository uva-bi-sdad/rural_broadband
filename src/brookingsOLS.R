# Regression model for subscription rate by Census tract
# Replicating the 2017 Brookings report

# 72,222 tracts had data for all fields (no report of missingness)
# 2017 total of 73,056 Census tracts (50 states + DC)

# Response = subscription rate (FCC)
# Covariates:
# 1. Availability @25 Mbps (FCC)s
# 2. Rural neighborhood y/n; Brookings used Census Metropolitan Statstical Areas
# https://www.census.gov/geo/maps-data/data/cbf/cbf_msa.html
# rural Census tracts are defined as those falling outside of any MSA
# 3. Share with at least a HS diploma (add ACS table #)
# 4. Poverty rate
# 5. Share age 65 or older
# 6. Hispanic share of population
# 7. Black share of population
# 8. Population density
# 9. Families share of population
# 10. Foreign born share of population

library(rgeos)
library(ggplot2)
library(tigris)
library(dplyr)
library(tidycensus)
library(xml2)
library(acs)
library(viridis)
library(rgdal)

options(scipen = 999)

#setwd("~/Desktop/Rural_Broadband/brookingsOLS/")

census_api_key("853b2a1e71aa0aff0f3db966545e8898c73f0772")

#load("brookings_fit.RData")

# ----------------------------------------------------
# a) create Census tract geometry and data frame
# ----------------------------------------------------
state_fips <- unique(fips_codes$state)[1:51]

census_tracts <- tracts(state=state_fips[1],year=2016)
for(i in 2:length(state_fips)){
  tmp <- tracts(state=state_fips[i],year=2016)
  census_tracts <- rbind(census_tracts,tmp)
}

census_tracts_df <- fortify(census_tracts,region="GEOID")
tract_data <- census_tracts@data %>% select(GEOID,STATEFP,COUNTYFP,NAME)

# save.image("census_tracts.RData")

# ----------------------------------------------------
# b) read in FCC data
# ----------------------------------------------------

# Fixed Broadband Deployment Data FCC Form 477, latest data as of December 31, 2016
# data at https://www.fcc.gov/general/broadband-deployment-data-fcc-form-477 [need for 25 Mbps availability by block]
# read in the availability dataset for December 2015 to match the Brookings report (US - Fixed with Satellite - Dec 15v2)
fcc_full <- read.csv("fcc_availability/fbd_us_with_satellite_dec2015_v2.csv")
# metadata at https://www.fcc.gov/general/explanation-broadband-deployment-data
# BlockCode is Census block code
# Provider_Id is filing number for internet provider
# MaxAdDown is Maximum advertised downstream speed/bandwidth offered by the provider in the block for Consumer service
# MaxAdUp is Maximum advertised upstream speed/bandwidth offered by the provider in the block for Consumer service
# Business(0/1) where 1 = Provider can or does offer business/government service in the block (not important; only care about consumer)
# MaxCIRDown, MaxCIRUp is Maximum contractual downstream/upstream bandwith for business service (also not important for consumers)

# using this data, aggregate 25 Mbps availability by Census block (#providers per block)
# fcc_availability data frame; names should be blockcode, providercount25
fcc_availability <- fcc_full %>% group_by(blockcode=BlockCode) %>%
  summarize(providercount10 = sum(MaxAdDown >= 10),
            providercount25 = sum(MaxAdDown >= 25))
# table(fcc_availability$providercount25) # at 25 Mbps there are 4.7 million blockgroups without a provider
# table(fcc_availability$providercount10) # at 10 Mbps there are only 119,274 blockgroups without a provider
#   (but only 18,000 in 2016)


# Residential Fixed Internet Access Service; quintiles of broadband subscription
# Residential connections with at least 10 Mbps downstream/1 Mbps upstream
# raw data at https://www.fcc.gov/internet-access-services-reports
# data by year, 2016:
# https://www.fcc.gov/reports-research/maps/residential-fixed-internet-access-service-connections-per-1000-households-by-census-tract-dec-2016/
# data by year, 2015:
# https://www.fcc.gov/reports-research/maps/residential-fixed-connections-10-mbps-1-mbps-december-2015/

fcc_subscription <- read.csv("fcc_subscription/tract_map_dec_2015.csv")
# metadata: (73,767 rows)
# tractcode is Census Tract code
# pcat_all is quinitles of Residential Fixed High-Speed Connections over 200kbps
# pcat_10x1 is quintiles of Residential Fixed High-Speed Connections at least 10 Mbps downatream and 1 Mbps upstream


# ----------------------------------------------------
# c) attach Broadband subscription (quintiles) by Census tract
# ----------------------------------------------------

tract_data$GEOID <- as.numeric(tract_data$GEOID)
tract_data <- tract_data %>% left_join(fcc_subscription %>% select(GEOID=tractcode,subscription=pcat_10x1), by="GEOID")

# table(tract_data$subscription,useNA = "always") # 342 are missing; these have NAME=9900
# View(tract_data[is.na(tract_data$subscription),])
# these are census tracts deliniated specifically to cover large bodies of water; filter them out
# see https://www.census.gov/geo/reference/gtc/gtc_ct.html
tract_data2 <- tract_data %>% filter(!is.na(subscription)) # 72,714


# ----------------------------------------------------
# d) define Broadband availability (>50%) by Census tract
# need to attach Census Tract population estimates
# unlike Brookings I will use 2016 ACS population estimates rather than decennial 2010 counts
# ----------------------------------------------------
# i. join tract to block data
# ii. get (population where rpcount10x1 is 0)/(total population) by tract
# iii. set available = TRUE whenever this is >50%, available = FALSE otherwise

#fcc_availability$blockcode[1:10]
#tract_data$GEOID[1:10]
# go from block code to census tract
fcc_availability$GEOID <- substr(fcc_availability$blockcode,1,nchar(fcc_availability$blockcode)-4)

# get decennial Census block estiamtes and join to fcc_availability by blockcode
# message: Error: At the moment, block data is only available for 2010.
#   I recommend using NHGIS (http://www.nhgis.org) and the ipumsr package for block data for other years.
# loop over state_code, county_code in fips_codes dataframe

fips2010 <- read.table("https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",sep=",",quote="")
names(fips2010) <- c("state","state_code","county_code","county","class")
fips2010 <- fips2010 %>% filter(state_code <= 56) # only include 50 states + DC

block_pop <- get_decennial(geography="block",county=fips2010$county_code[1],state=fips2010$state_code[1],variables="P001001",year=2010,cache_table=TRUE)
for(i in 2:nrow(fips2010)){
  tmp <- get_decennial(geography="block",county=fips2010$county_code[i],state=fips2010$state_code[i],variables="P001001",year=2010,cache_table=TRUE)
  block_pop <- rbind(block_pop,tmp)
  if(i%%10==0){print(i)}
}
#tmp <- get_decennial(geography="block",county=fips2010$county_code[i],state=fips2010$state_code[i],variables="P001001",year=2010,cache_table=TRUE)
# block_pop[nrow(block_pop),]
# fips2010[i-1,]
# fips2010[i,]
# save.image("census_tracts.RData")

# join block_pop to fcc_availability by blockcode
block_pop$GEOID <- as.numeric(block_pop$GEOID)
fcc_availability2 <- fcc_availability %>% left_join((block_pop %>% select(blockcode=GEOID,pop=value)), by="blockcode")

# ii. by Census tract: (population where rpcount10x1 is 0)/(total population)
tract_availabilty <- fcc_availability2 %>% group_by(GEOID) %>%
  summarize( percent_unavailable = sum(pop*(providercount25==0),na.rm=T) / sum(pop,na.rm=T) )

# iii. set available = FALSE whenever this is >50%, available = TRUE otherwise
tract_availabilty$available <- TRUE; tract_availabilty$available[tract_availabilty$percent_unavailable > 0.5] <- FALSE
#hist(tract_availabilty$percent_unavailable)
#table(tract_availabilty$available)
# 67793 available, 6071 unavailable Census tracts using the 25 Mbps definition for a majority of the population

# join percent and available y/n to tract_data
tract_availabilty$GEOID <- as.numeric(tract_availabilty$GEOID)
tract_data3 <- tract_data2 %>% left_join(tract_availabilty,by="GEOID")

# write.csv(tract_data3,file="broadband_availability_by_census_tract_2015.csv",row.names=F)
# write.csv(block_pop,file="population_by_census_block_2010.csv",row.names=F)
# save.image("brookings_fit.RData")

# ----------------------------------------------------
# e) add eight 2015 5-year ACS variable estimates by Census tract
# ----------------------------------------------------
# They downloaded data through the National Historical Geographic Information System (IPUMS NHGIS; https://data2.nhgis.org).
# I will get data through the Census API

# add ACS table # and variable #'s of interest
# 3. 'Share of population over 25 with no more than a HS diploma'
View( acs.lookup(endyear=2015, span = 5, dataset = "acs", table.name = "education", case.sensitive=F)@results )
# B15003_001 total
# B15003_002 - B15003_016 no HS diploma
# B15003_017 HS diploma
# B15003_018 GED or alternative credential
# B15003_019 - B15003_020 some college no degree
# B15003_021 some college no degree
# B15003_022 Bachelor's degree
# B15003_023 Master's degree
# B15003_024 Professional school degree
# B15003_025 Doctorate degree
# not clear how exactly Brookings defined this; I will take sum(002-018)/001

# 4. Poverty rate
View( acs.lookup(endyear=2015, span = 5, dataset = "acs", table.name = "poverty", case.sensitive=F)@results %>% group_by(table.number) %>% slice(1) )
# Poverty Status in the Past 12 Months by Age (B17020)
View( acs.lookup(endyear=2015, span = 5, dataset = "acs", table.number = "B17020", case.sensitive=F)@results )
# B17020_001 total
# B17020_002 income below the poverty level

# 5. Share age 65 or older
View( acs.lookup(endyear=2015, span = 5, dataset = "acs", table.name = "age", case.sensitive=F)@results %>% group_by(table.number) %>% slice(1) )
View( acs.lookup(endyear=2015, span = 5, dataset = "acs", table.number = "B01001", case.sensitive=F)@results )
# B01001_001 total
# B01001_020:025 male older than 65
# B01001_044:049 female older than 65

# 6. Hispanic share of population
View( acs.lookup(endyear=2015, span = 5, dataset = "acs", table.name = "hispanic", case.sensitive=F)@results %>% group_by(table.number) %>% slice(1) )
View( acs.lookup(endyear=2015, span = 5, dataset = "acs", table.number = "B03003", case.sensitive=F)@results )
# B03003_001 total
# B03003_003 Hispanic or Latino

# 7. Black share of population
View( acs.lookup(endyear=2015, span = 5, dataset = "acs", table.name = "race", case.sensitive=F)@results %>% group_by(table.number) %>% slice(1) )
View( acs.lookup(endyear=2015, span = 5, dataset = "acs", table.number = "B02001", case.sensitive=F)@results )
# B02001_001 total
# B02001_003 Black or African American alone

# 8. Population density (per square mile)
# B01001_001 total
# divide by area of Census tracts
# to convert square meters to square miles, divide by 2,589,988 (https://www.census.gov/quickfacts/fact/note/US/LND110210)
library(raster)
tract_area <- data.frame(GEOID=census_tracts@data$GEOID, area_sqmi = area(census_tracts)/2589988)

# 9. Families share of population (by household)
View( acs.lookup(endyear=2015, span = 5, dataset = "acs", table.name = "family", case.sensitive=F)@results %>% group_by(table.number) %>% slice(1) )
View( acs.lookup(endyear=2015, span = 5, dataset = "acs", table.number = "B09019", case.sensitive=F)@results )
# B09019_001 total
# B09019_002 in households
# B09019_003 in households: in family households
# B09019_038 in group quarters
# no estimates of families in group quarters; I will take 003 / 002. Not sure how Brookings defines family share.

# 10. Foreign born share of population
View( acs.lookup(endyear=2015, span = 5, dataset = "acs", table.name = "birth", case.sensitive=F)@results %>% group_by(table.number) %>% slice(1) )
View( acs.lookup(endyear=2015, span = 5, dataset = "acs", table.number = "B05002", case.sensitive=F)@results )
# B05002_001 total
# B05002_013 foreign born

acs_vars <- c("B15003_001","B15003_002","B15003_003","B15003_004","B15003_005","B15003_006","B15003_007","B15003_008","B15003_009",
              "B15003_010","B15003_011","B15003_012","B15003_013","B15003_014","B15003_015","B15003_016","B15003_017","B15003_018",
              "B17020_001","B17020_002",
              "B01001_001","B01001_020","B01001_021","B01001_022","B01001_023","B01001_024","B01001_025",
              "B01001_044","B01001_045","B01001_046","B01001_047","B01001_048","B01001_049",
              "B03003_001","B03003_003",
              "B02001_001","B02001_003",
              "B09019_002","B09019_003",
              "B05002_001","B05002_013"
)


# get a the string with all ACS variables and call them at the tract level; loop over states
acs_est <- get_acs(geography="tract",state=state_fips[1],variables=acs_vars,year=2015,cache_table=TRUE,output="wide")
for(i in 2:length(state_fips)){
  tmp <- get_acs(geography="tract",state=state_fips[i],variables=acs_vars,year=2015,cache_table=TRUE,output="wide")
  acs_est <- rbind(acs_est,tmp)
}

# compute rates (% of population) for each of the 8 variables; compute population density by joining tract_area
acs_est2 <- acs_est %>% left_join(tract_area,by="GEOID")
acs_estimates <- acs_est2 %>% transmute(
  GEOID=GEOID,
  population = B01001_001E,
  hs_or_less = (B15003_002E+B15003_003E+B15003_004E+B15003_005E+B15003_006E+B15003_007E+B15003_008E+B15003_009E+B15003_010E+
                 B15003_011E+B15003_012E+B15003_013E+B15003_014E+B15003_015E+B15003_016E+B15003_017E+B15003_018E) / B15003_001E,
  poverty = B17020_002E / B17020_001E,
  age_65_older = (B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E+
                    B01001_044E+B01001_045E+B01001_046E+B01001_047E+B01001_048E+B01001_049E)/ B01001_001E,
  hispanic = B03003_003E / B03003_001E,
  black = B02001_003E / B02001_001E,
  density = B01001_001E / area_sqmi,
  family = B09019_003E / B09019_002E,
  foreign = B05002_013E / B05002_001E
)

# join final ACS estimates to tract_data4 by GEOID
acs_estimates$GEOID <- as.numeric(acs_estimates$GEOID)
tract_data4 <- tract_data3 %>% left_join(acs_estimates,by="GEOID")

# write.csv(tract_data4,file="acs_estimates_by_census_tract_2015.csv",row.names=F)
# save.image("brookings_fit.RData")

# ----------------------------------------------------
# f) define Metropolitan/non-Metropolitan (rural) by Census tract
# ----------------------------------------------------

# Brookings uses 381 MSAs defined by the Office of Management and Budget in 2013
# To be more consistent in time, I will use the 388 MSAs defined for 2015

# use 2015 Combined Statistical Areas; includes Metropolitan + Micropolitan
# definitions at https://www.census.gov/geo/reference/gtc/gtc_cbsa.html
#   core urban area of at least 10,000 plus adjacent counties having high social and economic integration with the core
# data at https://www.census.gov/geo/maps-data/data/cbf/cbf_msa.html (cb_2015_us_cbsa_500k.zip)
# use only Metropolitan Statistical Areass; that is, at least one urbanized area has a population of at least 50,000

# read in CBSA shapefile
# use over() to determine: does each Census tract intersect with any CBSA?
# if so, define that Census tract as metropolitan, if not, define it as rural
cbsa_shape <- readOGR(dsn="cbsa_shapefiles/cb_2015_us_cbsa_500k/", layer="cb_2015_us_cbsa_500k")
# filter only Metropolitan Statistical Areas
table(cbsa_shape@data$LSAD) # 388 are 'M1' Metropolitan; 541 are 'M2' Micropolitan
msa_shape <- cbsa_shape[cbsa_shape@data$LSAD=="M1",]

metro_tracts <- sp::over(census_tracts,msa_shape)
rural_tract <- data.frame(GEOID=census_tracts@data$GEOID,rural=is.na(metro_tracts$NAME))
table(rural_tract$rural)
#FALSE  TRUE 
#64346  8710


# join metropolitan/rural binary variable to tract_data5
rural_tract$GEOID <- as.numeric(paste(rural_tract$GEOID))
tract_data5 <- tract_data4 %>% left_join(rural_tract,by="GEOID")

# ----------------------------------------------------
# g) run the Brookings OLS regression; Broadband subscription on 10 covariates
# ----------------------------------------------------

# what % of data are missing at the Census tract level?
table( rowSums(is.na(tract_data5)) ) # 72,210 Census tracts are compeltely observed (Brookings had 72,222)
colSums(is.na(tract_data5))/nrow(tract_data5) # missingness from ACS variables [for very small tracts?]


# turn subscription quintiles into a continuous response variable by taking the center of each bin
tract_data5$subscription_continuous <- pmax(0,tract_data5$subscription/5-0.1)
# fit the OLS regression
lm.out <- lm(subscription_continuous ~ available + rural + hs_or_less + poverty + age_65_older +
               hispanic + black + density + family + foreign,
             data=tract_data5)
summary(lm.out)

#write.csv(tract_data5,file="broadband_acs_by_census_tract_2015.csv",row.names=F)
#save.image("brookings_fit.RData")

# ----------------------------------------------------
# ----------------------------------------------------

# estimates, standard errors, p-values; compare to Brookings 2017
# make the table in figure B1 with bars for both Brookings + my estimates
# add asterisks or bold to significant effects; all but Black share at p < 0.01

# Figure B2:
effect1 <- c(0.28,-0.09,-0.52,-0.37,-0.15,-0.028,0,0.005,-0.02,0.37)*100
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

png("figB2.png",width=800,height=400)
fig_b2
dev.off()

# ----------------------------------------------------
# DATA ISSUES
# ----------------------------------------------------

# problem: fcc_full is not distinct by Provider_Id, BlockCode
# why are some duplicated? TechCode; technology of transmission used to offer broadband service
nrow( fcc_full %>% distinct(Provider_Id, BlockCode, TechCode) ) # 61776109
# (61904691 - 61776109)/61904691 # still 0.2% duplicated [128,582 records]
# get unique counts by Provider_Id, BlockCode, TechCode
test <- fcc_full %>% group_by(Provider_Id, BlockCode, TechCode) %>% summarize(count=n())
# look at records where counts > 1; where is there still duplication in FCC records?
as.numeric( test[which(test$count > 1)[1],] )
View( fcc_full %>% filter(Provider_Id==15821,BlockCode==271119606002093,TechCode==70) )
# differs by: DBAName ('doing business as'), MaxCIRDown, MaxCIRUp
# e.g. abetterwireless.com directs to abwmn.com; but they are listed as separate DBANames
# **many of these may be data errors like this (128,582 records)

# In this analysis we we will count different technologies, different 'DBA' names as multiple providers
# (this is fine since we only care if providercount25 is 0; if we care about exact provider count, consider carefully)



# map data for availability (only 10Mbps) at
# https://www.fcc.gov/reports-research/maps/residential-fixed-internet-access-service-providers-by-census-block-dec-2016/
# BUT we prefer to use the full 477 data to get availability at 25Mbps
# fcc_old <- read.csv("fcc_old/block_map_dec_2016.csv")
# metadata: (~10.6 million rows)
# blockcode is Census Block code
# rpcount is #providors with internet access greater than 200 kpbs to at least one residence
# rpcount10x1 is #providors with at least 10 Mbps downstream and 1 Mbps upstream to at least one residence



