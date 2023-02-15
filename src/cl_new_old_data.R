# For one BIP project area compare the number of properties within 20mi radius
# in old and new Corelogic data

# packages
library(readr)
library(readxl)
library(tidyverse)
library(DBI)
library(RPostgreSQL)
library(stringr)
library(fedmatch)
library(sf)
library(gridExtra)

# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

# all old CL data
load(paste0(path, "BIP_linear_data/usda_er_bb_data1 (1).rdata"))

# old CL in the 20mi radius
old_CL <- readRDS(paste0(path, "BIP_linear_models/housing_BIP_060622.RDS"))

# slect a project area
# BIP VA1108 GA1109 TX1120
old_bip <- old_CL %>% filter(BIP == "TX1120")

# bip shapes
load(paste0(path, "BIP_linear_data/BIP_New.rds"))
# VA1108-A39 GA1109-C40 TX1120-A40
shape <- newbip_union[newbip_union$ProjectID == "TX1120-A40",] 

# new corelogic data
con <- dbConnect(PostgreSQL(), 
                 dbname = "sdad",
                 host = "postgis1", 
                 port = 5432, 
                 password = Sys.getenv("db_pwd"))

qry <- "SELECT * 
FROM corelogic_usda.combined_property_basic_selected_typed_clean"
 
new_CL <- dbGetQuery(con, qry)

dbDisconnect(con)

# save the new CL data
# save(new_CL, file = paste0(path,"new_corelogic_property_data_clean_all.RData"))
load(paste0(path, "new_corelogic_property_data_clean_all.RData"))

# limit to above year 2005 
new_CL <- new_CL %>% filter(sale_year >=2005)

# project in meters to compute distance, lat/lon to check against CoreLogic
# Albers: 5070
# WGS84: 4326
shape <- st_transform(shape, 5070)

# drop if coordinates are missing
new_CL <- new_CL[!is.na(new_CL$property_centroid_latitude),]
new_CL <- new_CL[!is.na(new_CL$property_centroid_longitude),]

coords_CL <- new_CL %>%
  st_as_sf(coords = c("property_centroid_longitude", "property_centroid_latitude"), crs = 4326)
coords_CL <- st_transform(coords_CL, 5070)

# get CoreLogic lat/lons that fall within the boundary and a 20mi radius outside
CL_inside <- list()
CL_outside <- list()

# 20 mi outer boundary
outer_shape <- st_difference(st_buffer(shape, dist=1609.34*20), 
                             st_buffer(shape,0) )

# find pts inside the project area boundary
pts_inside <- st_within(coords_CL$geometry, shape$geometry)
CL_inside <- new_CL[which( sapply(pts_inside,length) > 0 ),]
CL_inside["bip"] <- 1

# find pts within 20miof the boundary
pts_outside <- st_within(coords_CL,outer_shape,sparse=TRUE)
CL_outside <- new_CL[which( sapply(pts_outside,length) > 0 ),]
CL_outside['bip'] <- 0

new_bip <- rbind(CL_inside, CL_outside)

# plots
shape <- st_transform(shape, 4326)
outer_shape <- st_transform(outer_shape, 4326)


plt1 <- ggplot() +
  geom_sf(data = shape$geometry,fill="white", col = "black") +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA) +
  geom_point(data=new_bip[new_bip$bip==1,], aes(x=property_centroid_longitude,
                                    y=property_centroid_latitude), color="green", size=0.5) +
  geom_point(data=new_bip[new_bip$bip==0,], aes(x=property_centroid_longitude, 
                                  y=property_centroid_latitude), color="orange", size=0.5)

plt2 <-ggplot() +
  geom_sf(data = shape$geometry, col = "black") +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA) +
  geom_point(data=old_bip[old_bip$bip == 1,], 
             aes(x=property_centroid_latitude, y=property_centroid_longitude), 
             color="green", size=0.5) +
  geom_point(data=old_bip[old_bip$bip == 0,], 
             aes(x=property_centroid_latitude, y=property_centroid_longitude), 
             color="orange", size=0.5) 

pdf(paste0(path, "/BIP_linear_data/", shape$ProjectID, ".pdf"))
grid.arrange(plt1, plt2)
dev.off()

