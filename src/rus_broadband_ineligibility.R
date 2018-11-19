library(sf)
library(dplyr)


# Note: Data files from http://www2.census.gov/geo/tiger/GENZ2016/shp/ (cb_2016_us_state_20m) 
# and https://www.sc.egov.usda.gov/data/data_files.html (RD_BROADBAND_IELG_PRODUCTION). 
# Not on GitLab. Change to local paths.


#
#------------------------------------------- Read in data
#

#ineligibility <- st_read("~/Broadband_Ineligible/RD_BROADBAND_IELG_PRODUCTION.shp", 
#                         stringsAsFactors = FALSE)

ineligibility <- st_read("data/rus_broadband_ineligibility/RD_BROADBAND_IELG_PRODUCTION.shp", 
                         stringsAsFactors = FALSE)

#us_states <- st_read("~/cb_2016_us_state_20m/cb_2016_us_state_20m.shp", 
#                     stringsAsFactors = FALSE)

us_states <- st_read("data/census_geo_cb/cb_2016_us_state_20m.shp", 
                     stringsAsFactors = FALSE)


#
#------------------------------------------- Match projections
#

project <- st_crs(us_states)
ineligibility <- st_transform(ineligibility, project)


#
#------------------------------------------- Map ineligibility by state
#

# Notes: This runs and *looks* like it makes sense (cf. ilegibility map and state maps separately), but I still get the following warning: 
# although coordinates are longitude/latitude, st_intersection assumes that they are planar
# Warning message:
#  attribute variables are assumed to be spatially constant throughout all geometries 

# Virignia
va <- us_states[us_states$NAME == "Virginia", ]
va <- va %>% select(geometry)
intersection <- st_intersection(va, ineligibility)

plot(va, reset = FALSE)
plot(intersection, add = TRUE)

#plot(st_intersection(va, ineligibility), add=T)

# Georgia
ga <- us_states[us_states$NAME == "Georgia", ]
ga <- ga %>% select(geometry)

plot(st_intersection(ga, ineligibility))