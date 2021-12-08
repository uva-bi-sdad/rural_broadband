#Tthis script computes intersections between ReConnect program shape files 
# and ACS census tracts and creates summary tables for 2019

# created: 18 Sept 21
# updated: 30 Sept 21: Remove estmates for technology type, round of applications and application status
# Re-compute the natinal level and region/area level estimates to include all tracts (not only in ReConnect Program)
# updated: Oct 4 21: Added Second Chance Approved application status from Round_2 table into the summary tables

# packages 
library(data.table)
library(sf)
library(dplyr)
library(plyr)
library(readxl)
library(tigris)
library(maps)
library(tidycensus)
`%>%` <- magrittr::`%>%`

# work dir
setwd("~/git/rural_broadband/src/")

# ReConnect shapefiles
project_data <- read_excel("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_RC_Shapefiles_August2021/Round 1 and 2 Submitted ReConnect Applications_8-6-2021.xlsx")

round1 <- st_read("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_RC_Shapefiles_August2021/ReConnect_984_PFSAs/","USDARD_PFSA_ReConnect",
                  int64_as_string = TRUE)
round2 <- st_read("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_RC_Shapefiles_August2021/ReConnect_Round_2_Final/","ReConnect_Round_2_Final",
                  int64_as_string = TRUE)
round2_program <- round2 %>% select(Applicat_2,Program__1)


# the PFSA identifiers in each shapefile (Applicat_1, 146 total, Applicat_2, 172 total)
# match perfectly with the excel file (Applicat_2, 318 total)
pfsa1 <- unique(round1$Applicat_1)
pfsa2 <- unique(round2$Applicat_2)
pfsa <- unique(project_data$Applicat_2)

# census tract shapes
state_fips <- c( unique(state.fips$fips), 2, 15 )
state_fips <- formatC(state_fips,width=2,flag=0)
tractgeo_list <- list()
for (state in state_fips){
  state_tract <- tigris::tracts(state)
  #tractgeo <- append(tractgeo, tigris::tracts(state))}
  i <- which(state_fips == state)
  tractgeo_list[[i]] <- state_tract }
tractgeo_df <- do.call(rbind, tractgeo_list)
tractgeo <- tractgeo_df %>% select(GEOID,  geometry)
# remove big files from the env
rm(tractgeo_df, tractgeo_list)

# get US counties shapefile
counties <- st_as_sf(counties())

# --------------------------------------------------
# loop over pfsa1, pfsa2
# select all shapes in the PFSA; then intersect with 
# Census tracts to get %area of each block

############################## ROUND 1 ###################################
tract_intersect_round1 <- list()

# loop across all IDs in the first round shapefile
for(i in 1:length(pfsa1)){
  print(i)
  currID <- pfsa1[i]
  currshape <- round1[round1$Applicat_1 %in% currID,]
  currshape <- st_buffer( st_union(st_buffer(currshape,0)), 0 )
  currshape <- st_transform(currshape,st_crs(counties))
  
  # first get counties where there is an intersection and subset to these counties
  county_intersect <- st_intersects(counties,currshape,sparse=T)
  county_intersect <- counties$GEOID[ sapply(county_intersect,length)>0 ]
  tractgeo_sub <- tractgeo[substr(tractgeo$GEOID,1,5) %in% county_intersect, ]
  
  # then get the tracts that actually intersect with currshape and subset to these tracts
  tracts_intersect <- st_intersects(tractgeo_sub,currshape,sparse=T)
  tracts_intersect <- tractgeo_sub$GEOID[ sapply(tracts_intersect,length)>0]
  tractgeo_sub2 <- tractgeo_sub[tractgeo_sub$GEOID %in% tracts_intersect, ]
  
  prop_area <- rep(NA,nrow(tractgeo_sub2))
  # compute proportion area of intersection with each block in a loop
  for(j in 1:nrow(tractgeo_sub2)){
    currblock <- tractgeo_sub2[j,]
    prop_area[j] <- st_area(st_intersection(currblock,currshape))/st_area(currblock)
  }
  # add to data frame of all aera intersections
  tract_intersect_round1[[i]] <- data.frame(pfsa=currID,
                                            GEOID10=tractgeo_sub2$GEOID,
                                            prop_area=prop_area)
}

####################### ROUND 2 ###################################
# loop across all IDs in the second round shapefile
tract_intersect_round2 <- list()

# loop across all IDs in the first round shapefile
for(i in 1:length(pfsa2)){
  currID <- pfsa2[i]
  currshape <- round2[round2$Applicat_2 %in% currID,]
  currshape <- st_union(st_buffer(currshape,0))
  currshape <- st_buffer( st_transform(currshape,st_crs(counties)), 0 )
  
  # first get counties where there is an intersection and subset to these counties
  county_intersect <- st_intersects(counties,currshape,sparse=T)
  county_intersect <- counties$GEOID[ sapply(county_intersect,length)>0 ]
  tractgeo_sub <- tractgeo[ substr(tractgeo$GEOID,1,5) %in% county_intersect, ]
  
  # then get the blocks that actually intersect with currshape and subset to these blocks
  tracts_intersect <- st_intersects(tractgeo_sub,currshape,sparse=T)
  tracts_intersect <- tractgeo_sub$GEOID[sapply(tracts_intersect,length)>0]
  tractgeo_sub2 <- tractgeo_sub[ tractgeo_sub$GEOID %in% tracts_intersect, ]
  
  prop_area <- rep(NA,nrow(tractgeo_sub2))
  # compute proportion area of intersection with each block in a loop
  for(j in 1:nrow(tractgeo_sub2)){
    currblock <- tractgeo_sub2[j,]
    intersect_area <- st_area(st_intersection(currblock,currshape))
    if(length(intersect_area)==0){
      prop_area[j] <- 0
    } else{
      prop_area[j] <- intersect_area/st_area(currblock)
    }
  }
  # add to data frame of all aera intersections
  tract_intersect_round2[[i]] <- data.frame(pfsa=currID,
                                            GEOID10=tractgeo_sub2$GEOID,
                                            prop_area=prop_area)
}
  
# to dataframes
tract_intersect_round1_df <- rbindlist(tract_intersect_round1)
# add round indicator
tract_intersect_round1_df$round <- "1"
tract_intersect_round1_df$Program__1<- 0
tract_intersect_round2_df <- rbindlist(tract_intersect_round2)
# add round indicator
tract_intersect_round2_df$round <- "2"

round2_df <- left_join(tract_intersect_round2_df,round2_program, by=c('pfsa'='Applicat_2'))
round2_df <- round2_df %>% select(pfsa, GEOID10, prop_area, round, Program__1)
round2_df <- unique(round2_df)
# save
tract_intersect_df <- rbind(tract_intersect_round1_df, round2_df, fill=TRUE)
saveRDS(tract_intersect_df,file="tract_intersect_df.RDS")


######################## URBAN/RURAL AREAS AND REGIONS #####################
######################## REDUNDANT #############################
# read in tract intersection files
tract_intersect_df <- readRDS("tract_intersect_df.RDS")

# set small values to zeros
tract_intersect_df$prop_area <- sprintf("%.5f", tract_intersect_df$prop_area)

# join RUCC codes by county
rucc2013 <- read_excel("~/git/rural_broadband/src/ruralurbancodes2013.xls")
tract_intersect_df$GEOID5 <-  substr(tract_intersect_df$GEOID10,1,5) 
tract_intersect_df <- tract_intersect_df %>% left_join(rucc2013 %>% 
                                                         dplyr::select(FIPS,RUCC_2013), by=c("GEOID5"="FIPS"))
# join in RUCA codes 
ruca2010 <- fread("~/git/rural_broadband/src/ruca2010r.csv",
                  colClasses = c(State_County_Tract_FIPS="character"))

ruca2010 <- ruca2010 %>% dplyr::select(State_County_Tract_FIPS,RUCA_2010='Primary_RUCA_2010',
                                       RUCA_2010_SECONDARY='Secondary_RUCA_2010', State)
tract_intersect_df <- tract_intersect_df %>% left_join(ruca2010, by=c("GEOID10"="State_County_Tract_FIPS"))

# define urban and rural areas based on RUCC and RUCA codes values
# beased on https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/
tract_intersect_df <- tract_intersect_df %>% mutate(
  RUCA_METRO = case_when( RUCA_2010 <=3 ~ 1,
                          TRUE ~ 0 ),
  RUCA_MICRO = case_when( RUCA_2010 %in% c(4,5,6) ~ 1,
                          TRUE ~ 0 ),
  RUCA_RURAL = case_when( RUCA_2010 >6 ~ 1,
                          TRUE ~ 0 ))

# define regions by state
# based on https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
tract_intersect_df <- tract_intersect_df %>% mutate(
  REGION = case_when(
    State %in% c("ME","VT", "NH", "MA", "CT", "RI", "NJ", "NY", "PA") ~ "NORTHEAST",
    State %in% c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "MI", "IN", "OH") ~ "MIDWEST",
    State %in% c("OK", "TX", "AR", "LA", "MS", "TN", "AL", "KY", "GA", "FL", "SC", "NC", "VA", "DC", "MD", "DE", "WV") ~ "SOUTH",
    State %in% c("WA", "OR", "ID", "MT", "CA", "NV", "WY", "UT", "AZ", "CO", "NM", "AK", "HI") ~ "WEST"
  ))

tract_intersect_df <- tract_intersect_df %>% mutate(
  NORTHEAST=1*(REGION=="NORTHEAST"),
  MIDWEST=1*(REGION=="MIDWEST"),
  SOUTH=1*(REGION=="SOUTH"),
  WEST=1*(REGION=="WEST")
)

# save
saveRDS(tract_intersect_df,file="tract_intersect_df.RDS")

############################# ADD ACS VARIABLES ###########################
# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

# acs vars search help 
#vars_19 <- tidycensus::load_variables(year = 2019,
#                       dataset = "acs5",
#                       cache = TRUE)

acs_vars <- c("B15002_001","B15002_003","B15002_004","B15002_005",
              "B15002_006","B15002_007","B15002_008","B15002_009",
              "B15002_010","B15002_011",
              "B15002_020","B15002_021","B15002_022","B15002_023",
              "B15002_024","B15002_025","B15002_026","B15002_027",
              "B15002_028",
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
              "B25001_001", "B25002_001", "B25002_002", "B25002_003", 
              "B25003_001", "B25003_002", "B25003_003", 
              "B25024_001", "B25024_002", "B25024_003", 
              "B25024_004", "B25024_005", "B25024_006", 
              "B25024_007", "B25024_008", "B25024_009", "B25024_010", "B25024_011",
              "B25035_001", "B25041_001", "B25041_002", 
              "B25041_003", "B25041_004", "B25041_005", 
              "B25041_006", "B25041_007", "B25064_001", "B25065_001",
              "B11001_001","B11001_002")

tmp <- list()
for(i in 1:length(state_fips)){
  tmp[[i]] <- get_acs(geography="tract",
                      state=state_fips[i],
                      variables=acs_vars,
                      year=2019,
                      cache_table=TRUE,
                      output="wide")}

acs_est <- rbindlist(tmp)

acs_estimates <- acs_est %>% transmute(
  GEOID=GEOID,
  population = B01001_001E,
  hunits_total = B25001_001E,
  hs_or_less = (B15002_003E+B15002_004E+B15002_005E+B15002_006E+
                  B15002_007E+B15002_008E+B15002_009E+B15002_010E+
                  B15002_011E+B15002_020E+B15002_021E+B15002_022E+
                  B15002_023E+B15002_024E+B15002_025E+B15002_026E+
                  B15002_027E+B15002_028E) / B15002_001E,
  renters = B25003_003E / B25003_001E,
  poverty = B17001_002E / B17001_001E,
  age_65_older = (B01001_020E+B01001_021E+B01001_022E+B01001_023E+
                  B01001_024E+B01001_025E+B01001_044E+B01001_045E+
                  B01001_046E+B01001_047E+B01001_048E+B01001_049E)/ 
                  B01001_001E,
  hispanic = B03003_003E / B03003_001E,
  black = B02001_003E / B02001_001E,
  family = B11001_002E / B11001_001E,
  foreign = B05002_013E / B05002_001E,
  median_income = B19013_001E,
  median_value = B25077_001E,
  unemployment = B23025_005E/B23025_003E
)
# rename colums
names(acs_estimates)[2:length(names(acs_estimates))] <-
  paste0(names(acs_estimates)[2:length(names(acs_estimates))],"_",2019)

# compute MOEs 
# (based on Census Manual available at
# https://www.census.gov/content/dam/Census/library/publications/2020/acs/acs_general_handbook_2020_ch08.pdf)
acs_moes <- acs_est %>% transmute(
  GEOID=GEOID,
  population = B01001_001M,
  hunits_total = B25001_001M,
  hs_or_less_E = (B15002_003E+B15002_004E+B15002_005E+B15002_006E+B15002_007E+B15002_008E
                  +B15002_009E+B15002_010E+B15002_011E+B15002_020E+B15002_021E+B15002_022E
                  +B15002_023E+B15002_024E+B15002_025E+B15002_026E+B15002_027E+B15002_028E),
  
  hs_or_less_M = sqrt(B15002_003M^2+B15002_004M^2+B15002_005M^2+B15002_006M^2+B15002_007M^2+
                        B15002_008M^2+B15002_009M^2+B15002_010M^2+B15002_011M^2+B15002_020M^2+
                        B15002_021M^2+B15002_022M^2+B15002_023M^2+B15002_024M^2+B15002_025M^2+
                        B15002_026M^2+B15002_027M^2+B15002_028M^2),
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
  median_value = B25077_001M,
  unemployment = 1 / B23025_003E * sqrt( B23025_005M^2 - (B23025_005E / B23025_003E)^2 * B23025_003M^2))

# rename columns
names(acs_moes)[2:length(names(acs_moes))] <-
  paste0(names(acs_moes)[2:length(names(acs_moes))],"_MOE_",2019)

# join acs estimates and MOEs 
acs_all <- acs_estimates %>% left_join(acs_moes, by=c("GEOID" = "GEOID"))
  
# add rural/urban ares to acs estimates and MOEs
acs_all <- acs_all %>% left_join(ruca2010, by=c("GEOID"="State_County_Tract_FIPS"))

# define urban and rural areas based on RUCC and RUCA codes values
# beased on https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/
acs_all <- acs_all %>% mutate(
  RUCA_METRO = case_when( RUCA_2010 <=3 ~ 1,
                          TRUE ~ 0 ),
  RUCA_MICRO = case_when( RUCA_2010 %in% c(4,5,6) ~ 1,
                          TRUE ~ 0 ),
  RUCA_RURAL = case_when( RUCA_2010 >6 ~ 1,
                          TRUE ~ 0 ))

# add regions
acs_all$state_id <- substr(acs_all$GEOID,1,2)
acs_all <- acs_all %>% mutate(
  REGION = case_when(
    state_id %in% c('09','23','25','33','44','50','34','36','42') ~ "NORTHEAST",
    state_id %in% c('18','17','26','39','55','19','20','27','29','31','38','46') ~ "MIDWEST",
    state_id %in% c('10','11','12','13','24','37','45','51','54','01','21','28','47','05','22','40','48') ~ "SOUTH",
    state_id %in% c('04','08','16','35','30','49','32','56','02','06','15','41','53') ~ "WEST"
  ))

acs_all <- acs_all %>% mutate(
  NORTHEAST=1*(REGION=="NORTHEAST"),
  MIDWEST=1*(REGION=="MIDWEST"),
  SOUTH=1*(REGION=="SOUTH"),
  WEST=1*(REGION=="WEST")
)

# join tracts intersections to acs estimates and MOEs 
tract_intersect_df <- readRDS("tract_intersect_df.RDS")
tract_intersect_df <- tract_intersect_df %>% left_join(acs_estimates, by=c("GEOID10" = "GEOID"))
tract_intersect_df <- tract_intersect_df %>% left_join(acs_moes, by=c("GEOID10" = "GEOID"))

# merge to project data to add info on withdrawn/rejected applications, technolody and round 
tract_intersect_df <- tract_intersect_df %>% left_join(project_data, by=c("pfsa"="Applicat_2"))

# save the final version ready for the summary tables
saveRDS(tract_intersect_df,file="tract_intersect_df.RDS")
#write.csv(tract_intersect_df,'tract_intersect_df.csv')

###################################### SUMMARY TABLES ###############################
# read in tract intersection files
tract_intersect_df <- readRDS("tract_intersect_df.RDS")

# summary fnx
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
    est=sum(w*var, na.rm=TRUE)/sum(w, na.rm=TRUE),
    moe=sqrt(sum(w^2*err^2, na.rm=TRUE))/sum(w, na.rm=TRUE))
  )
}


#test_2 <- sumcompute(acs_all, 'renters_2019', 'renters_MOE_2019', 
#                     weights = c('NORTHEAST', 'population_2019'))

# compute income and value in $1000 to avoid numeric instability
tract_intersect_df$median_income_2019 <- tract_intersect_df$median_income_2019 / 1000
tract_intersect_df$median_value_2019 <- tract_intersect_df$median_value_2019 / 1000

# proportion area to numeric
tract_intersect_df$prop_area <- sapply(tract_intersect_df[,'prop_area'], as.numeric)

# year/round dummies
tract_intersect_df <- tract_intersect_df %>% mutate(
  ROUND_1 = 1*(round=="1"), 
  ROUND_2 = 1*(round=="2"))

# dummies for technology type
tract_intersect_df <- tract_intersect_df %>% mutate(
  TECH = case_when(
    Technology %in% c('Fiber-to-the-Premises') ~ "FIBER",
    Technology %in% c('Fiber-to-the-Premises; Fixed Wireless - Licensed', 
                      'Fiber-to-the-Premises; Fixed Wireless - Licensed; Fixed Wireless - Unlicensed', 
                      'Fiber-to-the-Premises; Fixed Wireless - Unlicensed',
                      'Fiber-to-the-Premises; Hybrid-Fiber-Coax',
                      'Fiber-to-the-Premises; Hybrid-Fiber-Coax; Fixed Wireless - Licensed; Other',
                      'Fiber-to-the-Premises; Hybrid-Fiber-Coax; Fixed Wireless - Unlicensed',
                      'Fiber-to-the-Premises; Other') ~ "FIBER_OTHER",
    Technology %in% c('Fixed Wireless - Licensed', 'Fixed Wireless - Licensed; Fixed Wireless - Unlicensed',
                      'Fixed Wireless - Licensed; Fixed Wireless - Unlicensed; Other',
                      'Fixed Wireless - Unlicensed',
                      'Hybrid-Fiber-Coax; Fixed Wireless - Unlicensed',
                      'Hybrid-Fiber-Coax; Other',
                      'Other') ~ "NON-FIBER"))

tract_intersect_df <- tract_intersect_df %>% mutate(
  FIBER = 1*(TECH=="FIBER"), 
  FIBER_OTHER = 1*(TECH=="FIBER_OTHER"), 
  NON_FIBER= 1*(TECH=="NON-FIBER"))

# application status
tract_intersect_df <- tract_intersect_df %>% mutate(
  APPROVED = 1*(`Completion Status`=="Approved"), 
  REJECTED = 1*(`Completion Status`=="Rejected"), 
  WITHDRAWN= 1*(`Completion Status`=="Withdrawn"))

# application status
tract_intersect_df <- tract_intersect_df %>% mutate(
  SEC_CHANCE = 1*(Program__1 == "ReConnect Second Chance - FY 2020")
)

# function to take varname as input and call sumcompute for all years, subpopulation
sumyear <- function(varname, year, type){
  if(type == "person") weightname <- 'population_'
  if(type == "household") weightname <- 'hunits_total_'
  return(rbind(
    sumcompute(acs_all, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = paste0(weightname,year)),
    sumcompute(acs_all, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('RUCA_METRO',paste0(weightname,year))),
    sumcompute(acs_all, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('RUCA_MICRO',paste0(weightname,year))),
    sumcompute(acs_all, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('RUCA_RURAL',paste0(weightname,year))),
    sumcompute(acs_all, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('NORTHEAST',paste0(weightname,year))),
    sumcompute(acs_all, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('MIDWEST',paste0(weightname,year))),
    sumcompute(acs_all, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('SOUTH',paste0(weightname,year))),
    sumcompute(acs_all, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('WEST',paste0(weightname,year))),
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area',paste0(weightname,year))),
    # interactions
    # with area/region
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area','RUCA_METRO',paste0(weightname,year))),
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area','RUCA_MICRO',paste0(weightname,year))),
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area','RUCA_RURAL',paste0(weightname,year))),
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area','NORTHEAST',paste0(weightname,year))),
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area','MIDWEST',paste0(weightname,year))),
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area','SOUTH',paste0(weightname,year))),
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area', 'WEST',paste0(weightname,year))),
    # with year/round
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area','ROUND_1',paste0(weightname,year))),
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area','ROUND_2',paste0(weightname,year))),
    # with technology type
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area','FIBER',paste0(weightname,year))),
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area','FIBER_OTHER',paste0(weightname,year))),
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area','NON_FIBER',paste0(weightname,year))),
    # with application status
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area','APPROVED',paste0(weightname,year))),
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area','REJECTED',paste0(weightname,year))),
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area','WITHDRAWN',paste0(weightname,year))),
    sumcompute(tract_intersect_df, paste0(varname,"_",year), paste0(varname,"_MOE_",year), weights = c('prop_area','SEC_CHANCE',paste0(weightname,year)))
    
    ))
}

# test_1 <- sumyear('renters', '2019', 'person')

# format and write as .csv
sumtable <- function(varname, type){
  df <- data.frame(source = c("National",
                              "Metropolitan (RUCA 1-3)", "Micropolitan (RUCA 4-6)", "Rural (RUCA 7-10)",
                              "Northeast", "Midwest", "South", "West",
                              "ReConnect Regions",
                              "ReConnect Metropolitan",
                              "ReConnect Micropolitan",
                              "ReConnect Rural",
                              "ReConnect Northeast",
                              "ReConnect Midwest",
                              "ReConnect South",
                              "ReConnect West",
                              "ReConnect Round 1", "ReConnect Round 2",
                              "In RC and Fiber", "In RC and Fiber plus", "In RC and Non-fiber",
                              "In RC Approved", "In RC Rejected", "In RC Withdrawn", "In RC 2nd Chance"
                              ))
  df$ACS_2019_est = signif(sumyear(varname, '2019', type), 3 )[,1]
  df$ACS_2019_moe = signif(sumyear(varname, '2019', type), 3 )[,2]
  names(df) <- c("Level",
                 paste0(varname," 2019 ACS Est"), paste0(varname, " 2019 ACS MOE"))
  return(df)
}

#test <- sumtable('renters','person')

renters <- sumtable('renters','person')
hs_or_less <- sumtable('hs_or_less','person')
poverty <- sumtable('poverty','person')
age_65_older <- sumtable('age_65_older','person')
hispanic <- sumtable('hispanic','person')
black <- sumtable('black','person')
foreign <- sumtable('foreign','person')
unemp <- sumtable('unemployment','person')

# NOTES: for family, income, value use housing weights instead of person-weights
family <- sumtable('family','household')

medinc <- sumtable('median_income','household')
#fwrite( medinc, file='median_income.csv' )

medval <- sumtable('median_value','household')
#fwrite(medval, file="median_value.csv")

################################ STACK THE TABLES ###################################
tables <- join_all(list(renters, hs_or_less, poverty,
                        age_65_older, hispanic, black,
                        foreign, unemp, family,
                        medinc, medval), by="Level")
fwrite(tables, file="acs_2019_ReConnect_summaries.csv")