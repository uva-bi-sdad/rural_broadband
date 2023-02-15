# Broadband subscription fact for the paper form ACS
# packages
library(maps)
library(sf)
library(sp)
library(data.table)
library(dplyr)
library(rgeos)

##########################################################################################################
# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

# get ACS broadband vars
# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

# state FIPS 
state_fips <- c( unique(state.fips$fips), 2, 15 )
state_fips <- formatC(state_fips,width=2,flag=0)

# acs_vars <- c("B28002_004", # BB of anytype 
#              "B28002_007") # BB subscription such as cable, fiber optic or DSL (no Satelite/wireless)

acs_vars <- c("B28011_004") # BB subscription such as cable, fiber optic or DSL (no Satelite/wireless)
acs_vars <- c("B28011_004", "B28011_001") 

tmp <- list()
for(i in 1:length(state_fips)){
  tmp[[i]] <- get_acs(geography="tract",
                      state=state_fips[i],
                      variables=acs_vars,
                      year=2020,
                      cache_table=TRUE,
                      output="wide")}

acs_est <- rbindlist(tmp)
acs_estimates <- acs_est %>% transmute(
  geoid=GEOID,
  hh_units = B28011_001E,
  bb_units = B28011_004E,
  per_bb =B28011_004E / B28011_001E)

# join in RUCA codes 
ruca2010 <- fread("~/git/rural_broadband/src/ruca2010r.csv",
                  colClasses = c(State_County_Tract_FIPS="character"))

ruca2010 <- ruca2010 %>% dplyr::select(State_County_Tract_FIPS,RUCA_2010='Primary_RUCA_2010',
                                       RUCA_2010_SECONDARY='Secondary_RUCA_2010', State)

acs_estimates$geoid <- as.character(as.numeric(acs_estimates$geoid))
acs_estimates <- acs_estimates %>% left_join(ruca2010, by=c("geoid"="State_County_Tract_FIPS"))


# define urban and rural areas based on RUCC and RUCA codes values
# beased on https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/
acs_estimates <- acs_estimates %>% mutate(
  RUCA_METRO = case_when( RUCA_2010 <=3 ~ 1,
                          TRUE ~ 0 ),
  RUCA_MICRO = case_when( RUCA_2010 %in% c(4,5,6) ~ 1,
                          TRUE ~ 0 ),
  RUCA_RURAL = case_when( RUCA_2010 >6 ~ 1,
                          TRUE ~ 0 ))

mean(acs_estimates$per_bb[acs_estimates$RUCA_RURAL ==1], na.rm = T)
mean(acs_estimates$per_bb[acs_estimates$RUCA_METRO ==1], na.rm = T)

mean(acs_estimates$bb_units[acs_estimates$RUCA_RURAL ==1], na.rm = T)/mean(acs_estimates$hh_units[acs_estimates$RUCA_RURAL ==1], na.rm = T)
