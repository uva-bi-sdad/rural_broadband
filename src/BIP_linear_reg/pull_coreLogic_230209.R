# 2/9/23
# pull CoreLogic data from corelogic_usda.broadband_variables_tax_2020_06_27_unq_prog
# filter on:
#   transaction_type (TRNTP) 1 = resale, 3 = subdivision / new construction
#   property_indicator (PRIND) 10 = single family residene
# save as usda_er_bb_data_230209.RDS

setwd("~/git/rural_broadband/src/")

library(RPostgreSQL)
library(sf)

get_db_conn <-
  function(db_name = "sdad",
           db_host = "postgis1",
           db_port = "5432",
           db_user = Sys.getenv("db_usr"),
           db_pass = Sys.getenv("db_pwd")
  ) {
    RPostgreSQL::dbConnect(
      drv = RPostgreSQL::PostgreSQL(),
      dbname = db_name,
      host = db_host,
      port = db_port,
      user = db_user,
      password = db_pass
    )
  }

sql <- "
  SELECT *
  FROM corelogic_usda.broadband_variables_tax_2020_06_27_unq_prog
  WHERE property_indicator = '10'
    AND transaction_type IN('1','3')
"

con <- get_db_conn()
state_data.keep <- st_read(con, query = sql)
dbDisconnect(con)

path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/BIP_linear_reg/"
saveRDS(state_data.keep,file=paste0(path,"usda_er_bb_data_230209.RDS"))




# missingness and exploratory plots
# add +/- 1.5 IQR to bars in exploratory plots of #bedrooms, #bathrooms, sqft_ratio VS log house price

# add distance to nearest BIP project
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

# run for all rusids
# use sf to create a 20mi buffer around each project area
# find all latlongs within the buffer and compute distance (inside becomes negative)

#library(readr)
#dataset <- read_csv(paste0(path, "bip_all_projects.csv"))
#rusid_include <- unique(newbip_all$ProjectID)
rusid_include <- unique(newbip_union$ProjectID)

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

for(k in 1:length(CL_inside)){
  if(nrow(CL_inside[[k]]) > 0){ CL_inside[[k]]$bip <- 1 }
  if(nrow(CL_outside[[k]]) > 0){ CL_outside[[k]]$bip <- 0 }
}

# compute distance to boundary and convert to miles (negative if interior to boundary)
for(k in 1:length(CL_inside)){
  if(nrow(CL_inside[[k]]) > 0){
    coords_CL <- st_as_sf( data.frame(latitude=CL_inside[[k]]$property_centroid_latitude,
                                      longitude=CL_inside[[k]]$property_centroid_longitude),
                           coords=1:2,crs=4326
    )
    coords_CL <- st_transform(coords_CL, 5070)
    CL_inside[[k]]$dist_bip <- -1 * st_distance(coords_CL, st_cast( newbip_all %>% filter(ProjectID == rusid_include[k]), "MULTILINESTRING" ) ) / 1609.34
  }
}

for(k in 1:length(CL_inside)){
  if(nrow(CL_outside[[k]]) > 0){
    coords_CL <- st_as_sf( data.frame(latitude=CL_outside[[k]]$property_centroid_latitude,
                                      longitude=CL_outside[[k]]$property_centroid_longitude),
                           coords=1:2,crs=4326
    )
    coords_CL <- st_transform(coords_CL, 5070)
    CL_outside[[k]]$dist_bip <- st_distance(coords_CL, newbip_all %>% filter(ProjectID == rusid_include[k]) ) / 1609.34
  }
}

for(k in 1:length(CL_inside)){
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
  filter(pri_cat_code == "A")

data.BIP$sale_year <- sapply(as.character(data.BIP$sale_date), function(x) strsplit(x,split="-")[[1]][1])

# save this file
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"
saveRDS(data.BIP,file=paste0(path,"BIP_linear_models/housing_BIP_021623.RDS"))


# --------------------------------------
# BATHROOMS - complete when combining nbaths and total_baths
# --------------------------------------

data.BIP.orig <- data.BIP

# fix bathrooms: when nbaths is 0, set to total_baths
data.BIP$nbaths[data.BIP$nbaths==0] <- NA

table(data.BIP$nbaths, useNA="always")
# 973,930 (34%) are missing from nbaths
sum(is.na(data.BIP$nbaths) & !is.na(data.BIP$total_baths))
# ALL of these are available in the total_baths column
data.BIP$nbaths[is.na(data.BIP$nbaths)] <- data.BIP$total_baths[is.na(data.BIP$nbaths)]

# --------------------------------------
# BEDROOMS - still 33% missing; investigate original CL data for an additional column?
# --------------------------------------

table( data.BIP$bedrooms, useNA="always" )
# 943,979 (33%) are missing

# --------------------------------------
# AGE - 0.6% missing, 2.7% negative; how was this computed?
# --------------------------------------

sum( is.na(data.BIP$age ))/nrow(data.BIP)
# 16848 (0.6%) missing
sum( data.BIP$age < 0, na.rm=T )/nrow(data.BIP)
# 77518 (2.7%) is negative; sale date is earlier than year built
# exclude these from the analysis

# CHECK FOR NEW CONSTRUCTION; SET TO 0; REMOVE OUTLIERS/NOT NEW CONSTRUCTION

# --------------------------------------
# SQFT_RATIO - 13.3% missing
# --------------------------------------

sum(is.na(data.BIP$sqft_ratio)) / nrow(data.BIP)
# 378,344 (13.3%) missing, mostly building square feet
sum(is.na(data.BIP$living_square_feet)) / nrow(data.BIP) # 0.2%
sum(is.na(data.BIP$building_square_feet)) / nrow(data.BIP) # 13.1%

# --------------------------------------
# PO BOX in address
# --------------------------------------

# USE SITUS ADDRESS INSTEAD OF ADDRESS
# FOR MISSING ADDRESS, GEOCODE?

table( grepl( "PO BOX", data.BIP$address, fixed = TRUE) )
# FALSE    TRUE 
#2735303  114143
# 114143 addresses (4%) are PO boxes; omit these from analysis

data.BIP <- data.BIP %>% filter(age >= 0) %>%
  filter( !grepl( "PO BOX", address, fixed = TRUE) )


path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"
saveRDS(data.BIP,file=paste0(path,"BIP_linear_models/housing_BIP_021623.RDS"))



########################
# JOIN PROJECT DATA:
########################

# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"
data.BIP <- readRDS(file=paste0(path,"BIP_linear_models/housing_BIP_021623.RDS"))

# load in project data spreadsheet
bip_project = readxl::read_xlsx(paste0(path,"BIP_linear_models/BIP_R1-R2AwardeeMaster_10-29-10_updNetObl_7-3-19.xlsx"), sheet = 1)
bip_project$rusid <- paste0(bip_project$`RUS Award No.`, "-", bip_project$`RUS\r\nLoan-Grant No.`)

# compute quantiles of $net award / households served
bip_project$Households <- as.numeric(bip_project$Households)
bip_project$`Budget/HH` <- bip_project$`Net Award` / bip_project$Households

qcut2 <- function(x, n) findInterval(x, quantile(x, seq(0, 1, length = n + 1), na.rm=TRUE), all.inside = T)
bip_project$quantiles <- qcut2(bip_project$`Budget/HH`, 3)

# DTA file with new technology definitions
library(haven)
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

data.BIP <- data.BIP %>% select(-BIP,-CC,-RC,-TCF,-TCI) # remove extraneous columns
data.BIP2 <- left_join(data.BIP, bip_project[,c("rusid", "quantiles")])
data.BIP2 <- left_join(data.BIP2, tech,  by = c("rusid" = "projectid"))
data.BIP3 <- data.BIP2 %>% filter(sale_year >= 2005)

saveRDS(data.BIP3,file=paste0(path,"BIP_linear_models/housing_BIP_021623.RDS"))



# Note on rusid: rusid indexes the project area
#   data.BIP contains all properties inside and within 20 miles outside of each rusid 
#   this means that properties are duplicated; by property ID and sale date
test <- data.BIP3 %>% group_by(sale_date,p_id_iris_frmtd) %>% filter( n() > 1)

# Note on geography: "WI" projects are right next to "OK" projects; this is accurate (state of corporate HQ)
# plot(newbip_all$geometry[newbip_all$ProjectID %in% c("OK1110-F40","WI1129-A39")], col=c("red","blue"))


########################
# Exploratory plots to flag outliers
########################

library(ggplot2)

ggplot(data=data.BIP3, aes(x=bedrooms,y=log(sale_price))) +
  geom_point()

median(data.BIP3$bedrooms,na.rm=T)
IQR(data.BIP3$bedrooms,na.rm=T)


