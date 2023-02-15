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

# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

old_CL <- readRDS(paste0(path, "BIP_linear_models/housing_BIP_060622.RDS"))

# slect a project area
# BIP VA1108 VA1109 VA1110 VA1112
old_bip <- old_CL %>% filter(BIP == "VA1108")

# which states?
# old_bip_states <- substr(old_bip$geoid_cnty, 1, 2)
# table(old_bip_states)


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
AND fips_code LIKE '24%'"
 
new_CL <- dbGetQuery(con, qry)

dbDisconnect(con)

##################################################################
# drop if sale amount is NA
new_CL <- new_CL[!is.na(new_CL$sale_amount),]

# drop if sale year is later than 2005
new_CL['sale_year'] <- substr(new_CL$sale_date, 1,4)
new_CL$sale_year[is.na(new_CL$sale_year) == T] <- substr(new_CL$sale_recording_date, 1,4)
new_CL <- new_CL %>% filter(sale_year != "") %>% filter(as.numeric(sale_year) >=2005)

# if parcel level coordinates are missing replace with block level
new_CL['property_long'] <- new_CL$parcel_level_longitude
new_CL$property_long[is.na(new_CL$property_long)==T] <- new_CL$block_level_longitude
new_CL['property_lat'] <- new_CL$parcel_level_latitude
new_CL$property_lat[is.na(new_CL$property_lat)==T] <- new_CL$block_level_latitude
new_CL <- new_CL[!is.na(new_CL$property_long),]
new_CL <- new_CL[!is.na(new_CL$property_lat),]
new_CL <- new_CL[new_CL$property_long != "",]
new_CL <- new_CL[new_CL$property_lat != "",]

###############################################
load(paste0(path, "BIP_linear_data/BIP_New.rds"))

# select a shape VA1108-A39 VA1110-A40 VA1109-A39
shape <- newbip_union[newbip_union$ProjectID == "VA1109-A39",] 

# project in meters to compute distance, lat/lon to check against CoreLogic
# Albers: 5070
# WGS84: 4326
shape <- st_transform(shape, 5070)

new_CL$property_long <- as.numeric(new_CL$property_long)
new_CL$property_lat <- as.numeric(new_CL$property_lat)

coords_CL <- new_CL %>%
  st_as_sf(coords = c("property_long", "property_lat"), crs = 4326)

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

########################
# match addresses
########################

new_bip['full_address'] <- paste(
  str_trim(new_bip$situs_house_number),
  str_trim(new_bip$situs_house_number_suffix),
  str_trim(new_bip$situs_house_number_2),
  str_trim(new_bip$situs_direction),
  str_trim(new_bip$situs_street_name),
  str_trim(new_bip$situs_mode),
  str_trim(new_bip$situs_quadrant),
  str_trim(new_bip$situs_unit_number),
  str_trim(new_bip$situs_city),
  str_trim(new_bip$situs_state),
  str_trim(new_bip$situs_zip_code))

new_bip$full_address <- str_squish(new_bip$full_address)

new_bip['clip_fips'] <- paste0(new_bip$clip, "_", new_bip$fips_code)

adr_match <- merge_plus(
  data1 = new_bip,
  match_type = "fuzzy",
  data2 = old_bip, by.x = "full_address", by.y = "address",
  unique_key_1 = "clip_fips", unique_key_2 = "p_id_iris_frmtd",
  fuzzy_settings = build_fuzzy_settings(maxDist = .01))

match_adrs <- adr_match$matches
match_adrs <- match_adrs %>% select(c("p_id_iris_frmtd", "clip_fips", "address", "full_address"))

########### 
# plots

new_bip$property_long <- as.numeric(new_bip$property_long)
new_bip$property_lat <- as.numeric(new_bip$property_lat)

mat = matrix(1:2, nr = 1, nc = 2, byrow = T)

layout(mat,
       widths = c(3, 3),
       heights = c(3, 3))

#ggplot() + 
#  geom_sf(data = shape$geometry, col = "black") +
#  geom_sf(data = outer_shape$geometry, col="black", fill=NA) +
#  geom_point(data=new_bip, aes(x=property_long, y=property_lat), color="grey60", size=0.5) 

# get US state shapefile
states <- st_as_sf(states())

ggplot() +  geom_sf(data = states$geometry[states$GEOID == "24"]) +
  geom_sf(data = shape$geometry, col = "black") +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA) +
  geom_point(data=new_bip[new_bip$bip==1,], aes(x=property_long, y=property_lat), color="green", size=0.5) +
  geom_point(data=new_bip[new_bip$bip==0,], aes(x=property_long, y=property_lat), color="orange", size=0.5) +
  coord_sf(xlim = c(min(new_bip$property_long-1), max(new_bip$property_long)+1), 
           ylim = c(min(new_bip$property_lat)-1,max(new_bip$property_lat)+1), expand = TRUE)


ggplot() +  geom_sf(data = states$geometry[states$GEOID == "24"]) +
  geom_sf(data = shape$geometry, col = "black") +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA) +
  geom_point(data=old_bip[old_bip$bip == 1,], 
             aes(x=property_centroid_latitude, y=property_centroid_longitude), 
             color="green", size=0.5) +
  geom_point(data=old_bip[old_bip$bip == 0,], 
             aes(x=property_centroid_latitude, y=property_centroid_longitude), 
             color="orange", size=0.5) +
  coord_sf(xlim = c(min(old_bip$property_centroid_latitude)-1, max(old_bip$property_centroid_latitude)+1), 
           ylim = c(min(old_bip$property_centroid_longitude)-1, max(old_bip$property_centroid_longitude)+1), expand = FALSE)

