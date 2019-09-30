######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### BIP PENDER SHAPEFILES  ###### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 

rucc <- read.csv("~/git/dspg19broadband/data/working/ruralurban2013.csv")
fcc_processed_25 <- read.csv("data/working/fcc_processed_25.csv")
fcctract25 <- read.csv('~/git/dspg19broadband/data/working/fcc_processed_tract_25.csv')
approved_dbf <- foreign::read.dbf("~/git/rural_broadband/data/rus_broadband_servicearea/RD_BB_ApprovedSA.dbf")



## "B19001"

View(approved_dbf)


# YOU have to do three things
# ACS for variables of interest - do I need years to do this? 
# BIP areas of grantees
# FCC coverage of these areas

######### LOADING PENDER SHAPEFILES
approved <- foreign::read.dbf("~/git/rural_broadband/data/rus_broadband_servicearea/RD_BB_ApprovedSA.dbf")
clec <- foreign::read.dbf("~/git/rural_broadband/data/rus_broadband_servicearea/RD_BB_InfraBorrCLEC.dbf")
borr <- foreign::read.dbf("~/git/rural_broadband/data/rus_broadband_servicearea/RD_BB_InfrastructureBorr.dbf")

######### COMPARING PENDER SHAPEFILES
pender_files <- tibble::tibble("Dataset" = c("Approved", "CLEC", "Borr"),
                       "Unique_RUSIDs" = c(length(unique(approved$RUS_ID)), length(unique(clec$RUSID)), length(unique(borr$RUSID))),
                       "Rows" = c(length((approved$RUS_ID)), length((clec$RUSID)), length((borr$RUSID))),
                       "Shared_RUSIDs" = c("shares 5 with Borr", "none", "shares 5 with Approved"),
                       "Other" = c("1 VA , 50 not located??", " ", "1 VA ")
)

pender_files

######### OVERLAP BETWEEN PENDER SHAPEFILES
#approved[which(approved$RUS_ID %in% unique(borr$RUSID)),] # approved %>% filter(RUS_ID == "VA0532") # borr %>% filter(RUSID == "VA0532") # borr %>% filter(STATE == "IA")
#borr[which(borr$RUSID %in% unique(approved$RUS_ID)),]

######### LOADING & FILTERING FOR APPROVED RUS APPLICATIONS - FROM PENDER SHAPE FILES
#remotes::install_github("r-lib/rlang")

library(sf)
library(stringr)
approved_shp <- read_sf("~/git/rural_broadband/data/rus_broadband_servicearea/RD_BB_ApprovedSA.shp")

#head(as.data.frame(approved_shp)) # SAMPLE OF APPROVED RUS APPLICATIONS - FROM PENDER SHAPE FILES
library(dplyr)
 #remotes::install_github("r-lib/rlang")
library(rlang, lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")
library(rgdal)
bip_approved <- approved_shp %>%
  filter(STATUS == "Approved" & 
           APPSTATUSS == "Approved" & 
           str_detect(PROGRAMTYP, pattern = "BIP") == TRUE &
           lubridate::year(PUBLISHEDD) > 2000   
  )

plot(bip_approved[5]) # NATIONAL MAP OF APPROVED SUBSET OF RUS APPLICATIONS BY STATUS 


######### JOIN TO STATES - WRITE - SELECT VIRGINIA APPROVED AREAS FOR FUN
states <- sf::st_as_sf(readOGR(dsn="~/git/rural_broadband/data/census_geo_cb/",layer="cb_2017_us_state_20m"))
st_crs(bip_approved)
st_crs(states)
states <- states %>% st_transform(st_crs(bip_approved))
bip_approved_join  <- st_join(bip_approved, states)

#devtools::install_github("r-spatial/lwgeom")
library("lwgeom")
bip_approved_multistring <- st_cast(bip_approved, "MULTILINESTRING")
bip_split <- st_split(bip_approved_multistring, states)
library(data.table)
head(as.data.table(bip_split))
head(as.data.table(bip_approved_join))

virginia_bip_approved <- bip_approved_join %>% filter(NAME == "Virginia") 
plot(virginia_bip_approved["SERVICETYP"])

saveRDS(bip_approved_join, "data/working/BIP_working/BIP_Approved.RDS")

thing <- bip_approved_join %>% 
  mutate(state = str_extract(RUS_ID, "[:alpha:]{2}"),
         state_match = ifelse(STUSPS == state, 1, 0)) %>% 
  filter(STUSPS == "VA"|state == "VA")



test <- st_intersects(bip_approved_multistring, states)



