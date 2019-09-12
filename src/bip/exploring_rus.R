######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### BIP PENDER SHAPEFILES  ###### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 

rucc <- read.csv("~/git/dspg19broadband/data/working/ruralurban2013.csv")
fcc_processed_25 <- read.csv("data/working/fcc_processed_25.csv")
fcctract25 <- read.csv('~/git/dspg19broadband/data/working/fcc_processed_tract_25.csv')
approved_dbf <- foreign::read.dbf("~/git/rural_broadband/data/rus_broadband_servicearea/RD_BB_ApprovedSA.dbf")

"B19001"


# YOU have to do three things
# ACS for variables of interest - do I need years to do this? 
# BIP areas of grantees
# FCC coverage of these areas

######### LOADING PENDER SHAPEFILES
approved <- foreign::read.dbf("~/git/rural_broadband/data/rus_broadband_servicearea/RD_BB_ApprovedSA.dbf")
clec <- foreign::read.dbf("~/git/rural_broadband/data/rus_broadband_servicearea/RD_BB_InfraBorrCLEC.dbf")
borr <- foreign::read.dbf("~/git/rural_broadband/data/rus_broadband_servicearea/RD_BB_InfrastructureBorr.dbf")

######### COMPARING PENDER SHAPEFILES
pender_files <- tibble("Dataset" = c("Approved", "CLEC", "Borr"),
                       "Unique_RUSIDs" = c(length(unique(approved_dbf$RUS_ID)), length(unique(clec$RUSID)), length(unique(borr$RUSID))),
                       "Rows" = c(length((approved_dbf$RUS_ID)), length((clec$RUSID)), length((borr$RUSID))),
                       "Shared_RUSIDs" = c("shares 5 with Borr", "none", "shares 5 with Approved"),
                       "Other" = c("1 VA , 50 not located??", " ", "1 VA ")
)

pender_files

######### OVERLAP BETWEEN PENDER SHAPEFILES
#approved[which(approved$RUS_ID %in% unique(borr$RUSID)),] # approved %>% filter(RUS_ID == "VA0532") # borr %>% filter(RUSID == "VA0532") # borr %>% filter(STATE == "IA")
#borr[which(borr$RUSID %in% unique(approved$RUS_ID)),]

######### LOADING & FILTERING FOR APPROVED RUS APPLICATIONS - FROM PENDER SHAPE FILES
library(sf)
library(stringr)
approved_shp <- read_sf("~/git/rural_broadband/data/rus_broadband_servicearea/RD_BB_ApprovedSA.shp")

#head(as.data.frame(approved_shp)) # SAMPLE OF APPROVED RUS APPLICATIONS - FROM PENDER SHAPE FILES

bip_approved <- approved_shp %>% 
  filter(STATUS == "Approved" & 
           APPSTATUSS == "Approved" & 
           str_detect(PROGRAMTYP, pattern = "BIP") == TRUE &
           lubridate::year(PUBLISHEDD) > 2000   
  )

plot(bip_approved[5]) # NATIONAL MAP OF APPROVED SUBSET OF RUS APPLICATIONS BY STATUS 

######### SELECT VIRGINIA APPLICATION - GRAB OTHER DATA
virginia_BIP_application <- approved_shp %>% filter(RUS_ID == "VA0532") %>% 
  select(-OBJECTID, -SPEED_CODE, -TECHTYPE, -DL_SPEED, -UL_SPEED, -PCT_HISPEE) %>% 
  left_join(borr, by = c("RUS_ID" = "RUSID", 
                         "COMPANY" = "BORROWER", 
                         "SUM_HOUSIN" = "HOUSING_UN", 
                         "SUM_HOUSEH" = "HOUSEHOLDS", 
                         "SUM_TOTAL_" = "TOTAL_POPU",
                         "SHAPE_Leng", "SHAPE_Area" ))

plot(virginia_BIP_application)

virginia_BIP_application

######### 
state_fips <- 51
con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                      dbname = "gis",
                      host = "postgis_1",
                      port = "5432",
                      user = Sys.getenv("db_userid"),
                      password = Sys.getenv("db_pwd"))

geo = st_read(con, c("census_cb", sprintf("cb_2018_%s_bg_500k", state_fips)))

DBI::dbDisconnect(con)

plot(geo[2])


######### 
library(rgdal)
states <- readOGR(dsn="~/git/rural_broadband/data/census_geo_cb/",layer="cb_2017_us_state_20m")
state_VA <- states[states@data$NAME=="Virginia",]

proj4string=CRS('+proj=longlat +ellps=WGS84')
ggplot(data = state_VA) +
  geom_polygon(aes(x=long, y=lat)) + ##, group=group, fill=id)) #+
  geom_polygon(data =virginia_BIP_application)


plot(virginia_BIP_application)
class(virginia_BIP_application)




library(sf)
library(sp)
library(rgdal)
library(tigris)
library(tidycensus)
library(dplyr)
library(naniar)
library(gt) # devtools::install_github('rstudio/gt')
library(tidyr)
library(ggplot2)

options(tigris_use_cache = TRUE)
census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key


#
# Get VA state map ----------------------------------------------------------------------------------------------
#
#devtools::install_version("dplyr", version = "0.5", repos = "http://cran.us.r-project.org")
stateVA <- get_acs(geography = "tract", variables = "B01003_001", 
                   state = "Virginia",keep_geo_vars = TRUE, geometry = TRUE, 
                   output = "wide")

table(st_geometry_type(stateVA)) # 1907 multipolygons
plot(st_geometry(stateVA))


######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### FCC BB Providers    ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 

#'%!in%' <- function(x,y)!('%in%'(x,y))

library(data.table)
library(dplyr)
library(maditr)
setwd("~/git/dspg19broadband/data/working/FCC/")

fcc2010_small <- fread("NBM-CBLOCK-CSV-December-2010.csv",
                       fill=TRUE,
                       select = c("PROVNAME", "FULLFIPSID", "MAXADDOWN", "MAXADUP")) # FULLFIPSID(7), MAXADDOWN(9)
fcc2010_small <- fcc2010_small %>%
  dt_filter(MAXADDOWN %in% 3:11)

saveRDS(fcc2010_small, "~/git/rural_broadband/data/working/FCC_working/FCC_SM_FIPSwBBPROVIDERS.RDS")
# fcc_small_GEOID00 <-  fcc2010_small$FULLFIPSID[fcc2010_small$MAXADDOWN %in% 3:11]
# fcc_small_provcount <- tapply(X = fcc2010_small$PROVNAME[1:20],
#             INDEX = fcc2010_small$FULLFIPSID[1:20],
#             FUN = unique)
# 
#  <- data.table(FULLFIPSID = names(x = x[1:2]), PROVNAME = x[1:2])
# fcc_small_provcount <- fcc2010_small %>% 
#   group_by(FULLFIPSID, PROVNAME) %>% 
#   summarise(rows = n()) %>% select(-rows)
# rm(fcc2010_small); gc(reset = TRUE)
#fcc_small_provcount2 <- fcc_small_provcount %>% group_by(FULLFIPSID) %>% summarise(provid_ct = n())

fcc2010_large1 <- fread("NBM-Address-Street-CSV-December-2010.csv",fill=TRUE, select = c(5, 10, 13, 14)) # breaks at line 7739714
fcc2010_large2 <- fread("NBM-Address-Street-CSV-December-2010.csv",fill=TRUE,skip=7739715, select = c(5, 10, 13, 14))
fcc2010_large2 <- fcc2010_large2 %>%
  setNames(nm = names(fcc2010_large1))
names(fcc2010_large2)
names(fcc2010_large1)
fcc2010_large <- rbind(fcc2010_large1,fcc2010_large2)
rm(fcc2010_large1); rm(fcc2010_large2)
fcc2010_large <- fcc2010_large %>%
  dt_filter(MAXADDOWN %in% 3:11)
saveRDS(fcc2010_large, "~/git/rural_broadband/data/working/FCC_working/FCC_LG_FIPSwBBPROVIDERS.RDS")
rm(fcc2010_large)
#fcc_large_provcount2 <- fcc_large_provcount %>% group_by(FULLFIPSID) %>% summarise(provid_ct = n())

fcc2010_wireless <- fread("NBM-Wireless-CSV-December-2010.csv",fill=TRUE, select = c(2, 6, 9, 10) ) # CENSUSBLOCK_FIPS, MAXADDOWN
fcc2010_wireless <- fcc2010_wireless %>%
  dt_filter(maxaddown %in% 3:11)
saveRDS(fcc2010_wireless, "~/git/rural_broadband/data/working/FCC_working/FCC_WL_FIPSwBBPROVIDERS.RDS")
setwd("~/git/rural_broadband/data/working/FCC_working/")
fcc2010_large <- readRDS("FCC_LG_FIPSwBBPROVIDERS.RDS")
fcc2010_small <- readRDS("FCC_SM_FIPSwBBPROVIDERS.RDS")

fcc_GEOID_bb <- data.frame(GEOID00 = unique(c(fcc_small_GEOID00, fcc_large_GEOID00, fcc_wireless_GEOID00)))
fcc_GEOID_bb

length(unique(fcc2010_small$FULLFIPSID))
length(unique(fcc2010_large$FULLFIPSID))
length(unique(fcc2010_wireless$censusblock_fips))

length(intersect(unique(fcc2010_small$FULLFIPSID), unique(fcc2010_large$FULLFIPSID))) # 1594 in common
length(intersect(unique(fcc2010_wireless$censusblock_fips), unique(fcc2010_large$FULLFIPSID))) #24974 in common
length(intersect(unique(fcc2010_small$FULLFIPSID), unique(fcc2010_wireless$censusblock_fips))) # 1,817,139 in common

largesmall_shared <- intersect(unique(fcc2010_small$FULLFIPSID), unique(fcc2010_large$FULLFIPSID))
head(largesmall_shared, 5)
FCC_FIPSwBBPROV_LGSMjoin <- full_join(fcc2010_small, fcc2010_large, "FULLFIPSID")
FCC_FIPSwBBPROV_LGSMjoin 
#setwd("~/git/rural_broadband/")
#saveRDS(fcc_GEOID_bb, "~/git/rural_broadband/data/working/FCC_working/FCC_FIPSwBB.RDS")


FCC_LG_FIPSwBBPROVIDERS <- data.table(distinct(readRDS("~/git/rural_broadband/data/working/FCC_working/FCC_LG_FIPSwBBPROVIDERS.RDS")))
FCC_SM_FIPSwBBPROVIDERS <- data.table(distinct(readRDS("~/git/rural_broadband/data/working/FCC_working/FCC_SM_FIPSwBBPROVIDERS.RDS")))
FCC_WL_FIPSwBBPROVIDERS <- data.table(distinct(readRDS("~/git/rural_broadband/data/working/FCC_working/FCC_WL_FIPSwBBPROVIDERS.RDS")))


###SIDE QUESTION - why do small/large FCC files overlap? wireless overlaps as well - but that's expected? 
lg_sm_intersect_fips <- intersect(unique(FCC_LG_FIPSwBBPROVIDERS$FULLFIPSID), unique(FCC_SM_FIPSwBBPROVIDERS$FULLFIPSID)) #1594
wl_size_intersect_fips <- intersect(lg_sm_intersect_fips, unique(FCC_WL_FIPSwBBPROVIDERS$censusblock_fips)) #262
lg_wl_intersect_fips <- intersect(unique(FCC_LG_FIPSwBBPROVIDERS$FULLFIPSID), unique(FCC_WL_FIPSwBBPROVIDERS$censusblock_fips)) #24,974
sm_wl_intersect_fips <- intersect(unique(FCC_SM_FIPSwBBPROVIDERS$FULLFIPSID), unique(FCC_WL_FIPSwBBPROVIDERS$censusblock_fips)) #1,817,139

LG_SM_JOIN <- maditr::dt_inner_join(FCC_LG_FIPSwBBPROVIDERS, FCC_SM_FIPSwBBPROVIDERS, by = "FULLFIPSID")
LG_SM_JOIN <- LG_SM_JOIN %>% setNames(nm = c("FULLFIPSID", "LG_PROVNAME", "LG_MAXADDOWN", "LG_MAXADUP", "SM_PROVNAME", "SM_MAXADDOWN", "SM_MAXADUP"))
LG_SM_JOIN <- LG_SM_JOIN %>% mutate(ADDOWN_AGREE = ifelse(LG_MAXADDOWN == SM_MAXADDOWN, 1, 0),
                                    ADUP_AGREE = ifelse(LG_MAXADUP == SM_MAXADUP, 1, 0))

table(LG_SM_JOIN$ADDOWN_AGREE)  #2370 vs 1716
table(LG_SM_JOIN$ADUP_AGREE) #2135 vs 1951
table(LG_SM_JOIN$ADDOWN_AGREE)/nrow(LG_SM_JOIN) # 58% no
table(LG_SM_JOIN$ADUP_AGREE)/nrow(LG_SM_JOIN) # 52 % no

covered <- data.table("GEOID00" = unique(c(FCC_LG_FIPSwBBPROVIDERS$FULLFIPSID, FCC_SM_FIPSwBBPROVIDERS$FULLFIPSID, FCC_WL_FIPSwBBPROVIDERS$censusblock_fips)))

head(covered, 10)
saveRDS(FCC_LG_FIPSwBBPROVIDERS, "~/git/rural_broadband/data/working/FCC_working/FCC_LG_FIPSwBBPROVIDERS_distinct.RDS")
saveRDS(FCC_SM_FIPSwBBPROVIDERS, "~/git/rural_broadband/data/working/FCC_working/FCC_SM_FIPSwBBPROVIDERS_distinct.RDS")
saveRDS(FCC_WL_FIPSwBBPROVIDERS, "~/git/rural_broadband/data/working/FCC_working/FCC_WL_FIPSwBBPROVIDERS_distinct.RDS")
saveRDS(covered, "~/git/rural_broadband/data/working/FCC_working/block_with_786kbps.RDS")

rm(FCC_LG_FIPSwBBPROVIDERS)
rm(FCC_SM_FIPSwBBPROVIDERS)
rm(FCC_WL_FIPSwBBPROVIDERS)
rm(covered)
######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### POPULATION    ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 

FCC_LG_FIPSwBBPROVIDERS <- readRDS("~/git/rural_broadband/data/working/FCC_working/FCC_LG_FIPSwBBPROVIDERS_distinct.RDS")
FCC_SM_FIPSwBBPROVIDERS <- readRDS("~/git/rural_broadband/data/working/FCC_working/FCC_SM_FIPSwBBPROVIDERS_distinct.RDS")
FCC_WL_FIPSwBBPROVIDERS <- readRDS("~/git/rural_broadband/data/working/FCC_working/FCC_WL_FIPSwBBPROVIDERS_distinct.RDS")

blocks_w786kb <- readRDS("~/git/rural_broadband/data/working/FCC_working/block_with_786kbps.RDS")
setwd("~/git/dspg19broadband/data/working/FCC/")
block_pop <- fread("population_by_census_block_2010.csv",colClasses = c(GEOID = "character"))
census_2000_to_2010 <- fread("nhgis_blk2000_blk2010_ge.csv",colClasses = c(GEOID00 = "character", GEOID10 = "character"))
census_2000_to_2010$available <- 1*(census_2000_to_2010$GEOID00 %in% blocks_w786kb$GEOID00)

rm(blocks_w786kb)

block_with_786kbps_2010 <-census_2000_to_2010 %>% group_by(GEOID10) %>% summarize(percent_available=sum(WEIGHT*available)/sum(WEIGHT))
block_with_786kbps_2010$percent_available[is.na(block_with_786kbps_2010$percent_available)] <- 0

block_pop2 <- (block_pop %>% dplyr::select(GEOID10 = GEOID, population = value)) %>%
  maditr::dt_left_join(block_with_786kbps_2010,by="GEOID10")

hist(block_pop2$percent_available)
rm(block_pop)
# compute availability by Census Tract; sum(percent_available * population)/sum(population)
block_pop2$TRACTID <- substr(block_pop$GEOID,1,11)

fcc2010_tract <- block_pop2 %>% group_by(TRACTID) %>%
  summarize(available786kb = sum(percent_available*population)/sum(population))
fcc2010_tract$available786kb[is.na(fcc2010_tract$available786kb)] <- 0
hist(fcc2010_tract$available786kb)
setwd("~/git/rural_broadband/data/working/FCC_working/")
write.csv(fcc2010_tract,file="fcc2010tract.csv",row.names=F)
