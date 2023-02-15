# Geocode address in the new Cl data for one program
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
library(tidygeocoder)
library(stringi) 
library(gridExtra)

# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

old_CL <- readRDS(paste0(path, "BIP_linear_models/housing_BIP_060622.RDS"))

# slect a project area
# BIP VA1108 VA1109 VA1110 VA1112 TN1102 TN1103
old_bip <- old_CL %>% filter(BIP == "VA1108")

# which states?
old_bip_states <- substr(old_bip$geoid_cnty, 1, 2)
table(old_bip_states)

###############
# NEW CL DATA
###############

# establish connection to DB
con <- dbConnect(PostgreSQL(), 
                 dbname = "sdad",
                 host = "postgis1", 
                 port = 5432, 
                 password = Sys.getenv("db_pwd")) 

# all cols in the new full table
new_full_col_names <- dbGetQuery(con,"SELECT column_name FROM information_schema.columns
                                 WHERE table_schema = 'corelogic_usda'
                                 AND table_name   = 'combined_propertybasic_full'")


qry <- "SELECT clip, fips_code, zoning_code, zoning_code_description,
                property_indicator_code, block_level_latitude, block_level_longitude,
parcel_level_longitude, parcel_level_latitude, sale_date, sale_amount,
transaction_type_code, sale_recording_date, acres, land_square_footage,
year_built, effective_year_built, bedrooms_all_buildings,
total_bathrooms_all_buildings, number_of_bathrooms, half_baths_all_buildings, 
\"1qtr_baths_all_buildings\", \"3qtr_baths_all_buildings\",
                building_code, building_square_feet, living_square_feet_all_buildings,
                situs_house_number, situs_house_number_suffix, situs_house_number_2, 
                situs_direction, situs_street_name, situs_mode, situs_quadrant, 
                situs_unit_number, situs_city, situs_state, situs_zip_code
FROM corelogic_usda.combined_propertybasic_full
WHERE ((parcel_level_latitude IS NOT NULL AND parcel_level_longitude IS NOT NULL) OR
        block_level_latitude IS NOT NULL AND block_level_longitude IS NOT NULL)
AND  property_indicator_code = '10'
AND (sale_date IS NOT NULL OR sale_recording_date IS NOT NULL)
AND sale_amount IS NOT NULL
AND transaction_type_code != '9'
AND (building_square_feet IS NOT NULL OR living_square_feet_all_buildings IS NOT NULL)
AND (acres IS NOT NULL OR land_square_footage IS NOT NULL)
AND (year_built IS NOT NULL OR effective_year_built IS NOT NULL)
AND bedrooms_all_buildings IS NOT NULL
AND fips_code LIKE '51%'"

new_CL <- dbGetQuery(con, qry)


dbDisconnect(con)

# put address together
# full address
new_CL$full_address <- paste(
  str_trim(new_CL$situs_house_number),
  str_trim(new_CL$situs_house_number_suffix),
  str_trim(new_CL$situs_house_number_2),
  str_trim(new_CL$situs_direction),
  str_trim(new_CL$situs_street_name),
  str_trim(new_CL$situs_mode),
  str_trim(new_CL$situs_quadrant),
  str_trim(new_CL$situs_unit_number),
  str_trim(new_CL$situs_city),
  str_trim(new_CL$situs_state),
  str_trim(new_CL$situs_zip_code)
  )

# edit zip to 5 digits
new_CL$prop_address <- gsub(new_CL$full_address, 
                                 pattern="-[0-9]{0,9}", replacement = "")
new_CL$zip_code_5 <- stri_extract_last_regex(
                          new_CL$full_address, "\\d{5}+")
new_CL$full_address = substr(new_CL$full_address,1,
                                  nchar(new_CL$full_address)-9)
new_CL$full_address <- paste(new_CL$full_address, 
                             new_CL$zip_code_5)

# remove white spaces
new_CL$full_address <- str_squish(new_CL$full_address)

# program shapes
load(paste0(path, "BIP_linear_data/BIP_New.rds"))
# select a shape VA1108-A39 VA1110-A40 VA1109-A39 TN1102-A40 	
# TN1103-A40
shape <- newbip_union[newbip_union$ProjectID == "VA1108-A39",] 

# project in meters to compute distance, lat/lon to check against CoreLogic
# Albers: 5070
# WGS84: 4326
shape <- st_transform(shape, 5070)

new_CL$property_long <- as.numeric(new_CL$parcel_level_longitude)
new_CL$property_lat <- as.numeric(new_CL$parcel_level_latitude)

new_CL <-new_CL[!is.na(new_CL$parcel_level_longitude),]
new_CL <-new_CL[!is.na(new_CL$parcel_level_latitude),]

new_CL <-new_CL[new_CL$parcel_level_longitude != "",]
new_CL <-new_CL[new_CL$parcel_level_latitude != "",]


coords_CL <- new_CL %>%
  st_as_sf(coords = c("parcel_level_longitude", "parcel_level_latitude"), crs = 4326)

# transform into the same projection as program shape
coords_CL <- st_transform(coords_CL, 5070)

# get CoreLogic lat/lons that fall within the boundary and a 10mi radius outside
# lists to save data
CL_inside <- list()
CL_outside <- list()

# extend the shape bounsary by 10 mi
outer_shape <- st_difference(st_buffer(shape, dist=1609.34*20), st_buffer(shape,0) )
outer_shape <- st_transform(outer_shape, 5070)

pts_inside <- st_within(coords_CL, shape$geometry)
ind_pts_inside <- which( sapply(pts_inside,length) > 0 )

CL_inside <- new_CL[which( sapply(pts_inside,length) > 0 ),]
CL_inside['bip'] <- 1

pts_outside <- st_within(coords_CL,outer_shape,sparse=TRUE)
CL_outside <- new_CL[which( sapply(pts_outside,length) > 0 ),]

CL_outside['bip'] <- 0 

new_bip <-  rbind(CL_inside, CL_outside)

shape <- st_transform(shape, 4326)
outer_shape <- st_transform(outer_shape, 4326)

plt1 <- ggplot() +
  geom_sf(data = shape$geometry,fill="white", col = "black") +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA) +
  geom_point(data=new_bip[new_bip$bip==1,], aes(x=parcel_level_longitude,
                                                y=parcel_level_latitude), color="green", size=0.5) +
  geom_point(data=new_bip[new_bip$bip==0,], aes(x=parcel_level_longitude, 
                                                y=parcel_level_latitude), color="orange", size=0.5) +
  coord_sf(xlim = c(min(new_bip$parcel_level_longitude), 
                    max(new_bip$parcel_level_longitude)), 
           ylim = c(min(new_bip$parcel_level_latitude),
                    max(new_bip$parcel_level_latitude)), expand = T)

plt2 <-ggplot() +
  geom_sf(data = shape$geometry, col = "black") +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA) +
  geom_point(data=old_bip[old_bip$bip == 1,], 
             aes(x=property_centroid_latitude, y=property_centroid_longitude), 
             color="green", size=0.5) +
  geom_point(data=old_bip[old_bip$bip == 0,], 
             aes(x=property_centroid_latitude, y=property_centroid_longitude), 
             color="orange", size=0.5) +
  coord_sf(xlim = c(min(old_bip$property_centroid_latitude), 
                    max(old_bip$property_centroid_latitude)), 
           ylim = c(min(old_bip$property_centroid_longitude),
                    max(old_bip$property_centroid_longitude)), expand = T)


pdf(paste0(path, "/BIP_linear_data/", shape$ProjectID, ".pdf"))
grid.arrange(plt1, plt2)
dev.off()

# geocode addresses
# installed google api key
readRenviron("~/.Renviron")
Sys.getenv("GOOGLEGEOCODE_API_KEY")


#geocode the addresses
new_bip_lonlat <- new_bip %>%
  geocode(full_address,
          method = 'google',
          lat = latitude ,
          long = longitude,
          full_results = FALSE)

# project in meters to compute distance, lat/lon to check against CoreLogic
# Albers: 5070
# WGS84: 4326
shape <- st_transform(shape, 5070)

new_bip_lonlat <-new_bip_lonlat[!is.na(new_bip_lonlat$longitude),]
new_bip_lonlat <-new_bip_lonlat[!is.na(new_bip_lonlat$latitude),]

new_bip_lonlat <-new_bip_lonlat[new_bip_lonlat$longitude != "",]
new_bip_lonlat <-new_bip_lonlat[new_bip_lonlat$latitude != "",]


coords_CL <- new_bip_lonlat %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# transform into the same projection as program shape
coords_CL <- st_transform(coords_CL, 5070)

# get CoreLogic lat/lons that fall within the boundary and a 10mi radius outside
# lists to save data
CL_inside <- list()
CL_outside <- list()

# extend the shape bounsary by 10 mi
outer_shape <- st_difference(st_buffer(shape, dist=1609.34*20), st_buffer(shape,0) )
outer_shape <- st_transform(outer_shape, 5070)

pts_inside <- st_within(coords_CL, shape$geometry)
ind_pts_inside <- which( sapply(pts_inside,length) > 0 )

CL_inside <- new_bip_lonlat[which( sapply(pts_inside,length) > 0 ),]
CL_inside['bip'] <- 1

pts_outside <- st_within(coords_CL,outer_shape,sparse=TRUE)
CL_outside <- new_bip_lonlat[which( sapply(pts_outside,length) > 0 ),]

CL_outside['bip'] <- 0 

new_bip_geocoded <-  rbind(CL_inside, CL_outside)
shape <- st_transform(shape, 4326)
outer_shape <- st_transform(outer_shape, 4326)

ggplot() +
  geom_sf(data = shape$geometry,fill="white", col = "black") +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA) +
  geom_point(data=new_bip_geocoded[new_bip_geocoded$bip==1,],
             aes(x=longitude,
                y=latitude), color="green", size=0.5) +
  geom_point(data=new_bip_geocoded[new_bip_geocoded$bip==0,], 
             aes(x=longitude, 
                y=latitude), color="orange", size=0.5) 
