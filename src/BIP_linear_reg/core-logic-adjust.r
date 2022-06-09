# 6-1-22
# adjust CoreLogic data - make sure we are getting all sales within 20MI

# packages
library(sf)
library(dplyr)
library(readr)
library(readxl)
library(writexl)
library(data.table)

setwd("~/git/rural_broadband/src/")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/BIP_linear_data/"

# BIP shapes
load(paste0(path, "BIP_New.rds"))

# property Corelogic data
if(!"state_data.keep" %in% ls()) load(paste0(path, "usda_er_bb_data1 (1).rdata"))
# save(state_data.keep, file="usda_er_bb_data1.RData")

# run for all rusids
# use sf to create a 20mi buffer around each project area
# find all latlongs within the buffer and compute distance (inside becomes negative)

#library(readr)
#dataset <- read_csv(paste0(path, "bip_all_projects.csv"))
rusid_include <- unique(newbip_all$ProjectID)

# project in meters to compute distance, lat/lon to check against CoreLogic
# Albers: 5070
# WGS84: 4326
newbip_all <- st_transform(newbip_all, 5070)

coords_CL <- st_as_sf( data.frame(latitude=state_data.keep$property_centroid_latitude,
                                  longitude=state_data.keep$property_centroid_longitude),
                       coords=1:2,crs=4326
)
coords_CL <- st_transform(coords_CL, 5070)

# get CoreLogic lat/lons that fall within the boundary and a 20mi radius outside
CL_inside <- list()
CL_outside <- list()
for(k in 1:length(rusid_include)){
  rus_boundary <- subset(newbip_all, ProjectID==rusid_include[k])
  rus_outer <- st_difference( st_buffer(rus_boundary, dist=1609.34*20), st_buffer(rus_boundary,0) )
  
  pts_inside <- st_within(coords_CL, rus_boundary)
  ind_pts_inside <- which( sapply(pts_inside,length) > 0 )
  CL_inside[[k]] <- state_data.keep[which( sapply(pts_inside,length) > 0 ),]
  
  pts_outside <- st_within(coords_CL,rus_outer,sparse=TRUE)
  CL_outside[[k]] <- state_data.keep[which( sapply(pts_outside,length) > 0 ),]
}
saveRDS(CL_inside,"BIP_linear_reg/CL_inside.RDS")
saveRDS(CL_outside,"BIP_linear_reg/CL_outside.RDS")

# add a rusid column, add bip=1 if inside, bip=0 if outside
# compute distance to boundary for each point (inside: distance*-1)
# then rbind and save as data.BIP

CL_inside <- readRDS("BIP_linear_reg/CL_inside.RDS")
CL_outside <- readRDS("BIP_linear_reg/CL_outside.RDS")

for(k in 1:length(rusid_include)){
  if(nrow(CL_inside[[k]]) > 0){ CL_inside[[k]]$bip <- 1 }
  if(nrow(CL_outside[[k]]) > 0){ CL_outside[[k]]$bip <- 0 }
}

# compute distance to boundary and convert to miles (negative if interior to boundary)
for(k in 1:length(rusid_include)){
  if(nrow(CL_inside[[k]]) > 0){
    coords_CL <- st_as_sf( data.frame(latitude=CL_inside[[k]]$property_centroid_latitude,
                                      longitude=CL_inside[[k]]$property_centroid_longitude),
                           coords=1:2,crs=4326
    )
    coords_CL <- st_transform(coords_CL, 5070)
    CL_inside[[k]]$dist_bip <- -1 * st_distance(coords_CL, st_cast( newbip_all %>% filter(ProjectID == rusid_include[k]), "MULTILINESTRING" ) ) / 1609.34
  }
}

for(k in 1:length(rusid_include)){
  if(nrow(CL_outside[[k]]) > 0){
    coords_CL <- st_as_sf( data.frame(latitude=CL_outside[[k]]$property_centroid_latitude,
                                      longitude=CL_outside[[k]]$property_centroid_longitude),
                           coords=1:2,crs=4326
    )
    coords_CL <- st_transform(coords_CL, 5070)
    CL_outside[[k]]$dist_bip <- st_distance(coords_CL, newbip_all %>% filter(ProjectID == rusid_include[k]) ) / 1609.34
  }
}

for(k in 1:length(rusid_include)){
  if(nrow(CL_inside[[k]]) > 0) CL_inside[[k]]$rusid <- rusid_include[k]
  if(nrow(CL_outside[[k]]) > 0) CL_outside[[k]]$rusid <- rusid_include[k]
}

rusid_keep <- which( sapply(CL_inside,nrow) > 50 & sapply(CL_outside,nrow) > 50 ) # 112 projects
data.BIP <- rbind( rbindlist(CL_inside[rusid_keep]), rbindlist(CL_outside[rusid_keep]) )
data.BIP$dist_bip <- as.numeric(data.BIP$dist_bip)

###########################################
# PROPERTY DATA CLEANING ACCORDING TO JP
###########################################
# tarnsaction codes:
transaction_code <- list("1", "3")
data.BIP$transaction_type <- as.character(data.BIP$transaction_type)

# building code:
bldg_code_ext <- list("A0G","A0Z", "AB0", "AY0", "AYA", "AYG", "M00",
                      "M02", "M03", "M04", "M05", "M06", "M0A", "M0B", "M0M", "M0T",
                      "M40", "MA0", "MAA", "MAB", "MAC", "MAS", "MAT", "MC0", "MCA", "MCB",
                      "MCE", "MCM", "MCT", "MCV", "MD0", "MDC", "MDE", "MDF", "MRD", "MS0", "MST", "MT0",
                      "R00", "R0C", "R0F", "R0Q", "R10", "R20", "R30", "R40", "R60", "R80",
                      "RC0", "RCA", "RG0", "RM0", "RM1", "RM2", "RMB", "RQ0", "RS0", "RSF", "RT0", "RU0",
                      "RW0", "RY0", "X01", "X0M", "X0N", "X0Z", "XF0", "XH0", "XNM", "XRM", "Y1L",
                      "YCR", "YDA", "YOR", "YQ1", "YQB", "YQR", "YQS", "YRA", "YSA", "YSR")

#building_code <- list("RS0", "R00")
data.BIP$bldg_code <- as.character(data.BIP$bldg_code)

# primary category code:
data.BIP$pri_cat_code <- as.character(data.BIP$pri_cat_code)

# age non-negative
data.BIP <- data.BIP %>% filter(transaction_type %in% transaction_code) %>% 
  filter(bldg_code %in% bldg_code_ext) %>%
  filter(pri_cat_code == "A") %>%
  filter(age >= 0)
########################
# PROCEED AS USUAL:
########################

data.BIP <- data.BIP[!is.na(data.BIP$age),]
data.BIP[is.na(data.BIP$bedrooms),"bedrooms"] <- 0
data.BIP[is.na(data.BIP$sqft_ratio),"sqft_ratio"] <- 1
data.BIP$sale_year <- sapply(as.character(data.BIP$sale_date), function(x) strsplit(x,split="-")[[1]][1])


# SAVE HERE
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"
saveRDS(data.BIP,file=paste0(path,"BIP_linear_models/housing_BIP_060622.RDS"))
fwrite(data.BIP,file=paste0(path,"BIP_linear_models/housing_BIP_060622.csv"),row.names=FALSE)

########################
# JOIN PROJECT DATA:
########################

# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"
data.BIP <- readRDS(file=paste0(path,"BIP_linear_models/housing_BIP_060622.RDS"))

# load in project data spreadsheet
bip_project = readxl::read_xlsx(paste0(path,"BIP_linear_models/BIP_R1-R2AwardeeMaster_10-29-10_updNetObl_7-3-19.xlsx"), sheet = 1)
bip_project$rusid <- paste0(bip_project$`RUS Award No.`, "-", bip_project$`RUS\r\nLoan-Grant No.`)

# compute quantiles of $net award / households served
bip_project$Households <- as.numeric(bip_project$Households)
bip_project$`Budget/HH` <- bip_project$`Net Award` / bip_project$Households

qcut2 <- function(x, n) findInterval(x, quantile(x, seq(0, 1, length = n + 1), na.rm=TRUE), all.inside = T)
bip_project$quantiles <- qcut2(bip_project$`Budget/HH`, 3)

# DTA file with new technology definitions
tech_descr <- read_dta(paste0(path, "BIP_linear_data/bip_completed_infrastructure_projects_master_file.dta"))
tech <- tech_descr %>% select(projectid, ftth_gpon, ftth_rfog, ftth_ptp, ftth, fixed_wireless, adsl,
                              vdsl, mobile_wireless, hfc_cable, power_line)

tech["wireless"] <- tech$fixed_wireless + tech$mobile_wireless
tech["dsl"] <- 0
tech$dsl[(tech$adsl ==1 | tech$vdsl == 1)] <- 1

tech <- tech %>% select(c(projectid,ftth,wireless,dsl))


unique_rusid <- unique(data.BIP$rusid)
# several rusid in data.BIP are NOT in bip_project
unique_rusid[! unique_rusid %in% bip_project$rusid ]

data.BIP2 <- left_join(data.BIP, bip_project[,c("rusid", "quantiles")])
data.BIP2 <- left_join(data.BIP2, tech,  by = c("rusid" = "projectid"))
data.BIP2 <- data.BIP2 %>% filter(sale_year >= 2005)


saveRDS(data.BIP2,file=paste0(path,"BIP_linear_models/housing_BIP_060622.RDS"))
fwrite(data.BIP2,file=paste0(path,"BIP_linear_models/housing_BIP_060622.csv"),row.names=FALSE)

