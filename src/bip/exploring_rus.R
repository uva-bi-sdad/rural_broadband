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

fcc2010_small <- fread("NBM-CBLOCK-CSV-December-2010.csv",fill=TRUE) # FULLFIPSID(7), MAXADDOWN(9)
fcc_small_GEOID00 <-  fcc2010_small$FULLFIPSID[fcc2010_small$MAXADDOWN %in% 8:11]
rm(fcc2010_small)

fcc2010_large1 <- fread("NBM-Address-Street-CSV-December-2010.csv",fill=TRUE) # breaks at line 7739714
fcc2010_large2 <- fread("NBM-Address-Street-CSV-December-2010.csv",fill=TRUE,skip=7739715)
fcc2010_large2 <- fcc2010_large2 %>%
  setNames(nm = names(fcc2010_large1))
names(fcc2010_large2)
names(fcc2010_large1)
fcc2010_large <- rbind(fcc2010_large1,fcc2010_large2)
rm(fcc2010_large1); rm(fcc2010_large2)
fcc_large_GEOID00 <- fcc2010_large$FULLFIPSID[fcc2010_large$MAXADDOWN %in% 8:11]
fcc_large_provcount <- fcc2010_large[fcc2010_large$FULLFIPSID %in% fcc_large_GEOID00] %>% 
  group_by(FULLFIPSID, PROVNAME) %>% 
  summarise(rows = n()) %>% select(-rows)
rm(fcc2010_large)
fcc_large_provcount2 <- fcc_large_provcount %>% group_by(FULLFIPSID) %>% summarise(provid_ct = n())

fcc2010_wireless <- fread("NBM-Wireless-CSV-December-2010.csv",fill=TRUE) # CENSUSBLOCK_FIPS, MAXADDOWN
fcc_wireless_GEOID00 <- fcc2010_wireless$censusblock_fips[fcc2010_wireless$maxaddown %in% 8:11]
fcc_wireless_provcount <- fcc2010_wireless[fcc2010_wireless$censusblock_fips %in% fcc_wireless_GEOID00] %>% 
  group_by(censusblock_fips, PROVNAME) %>% 
  summarise(rows = n()) %>% select(-rows)
rm(fcc2010_wireless)
fcc_wireless_provcount2 <- fcc_wireless_provcount %>% group_by(censusblock_fips) %>% summarise(provid_ct = n())




