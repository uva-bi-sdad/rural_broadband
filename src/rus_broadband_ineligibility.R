library(sf)
library(dplyr)
library(ggplot2)

#
#------------------------------------------- Read in data
#


ineligibility <- st_read("data/rus_broadband_ineligibility/RD_BROADBAND_IELG_PRODUCTION.shp", 
                         stringsAsFactors = FALSE)

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

# Base R
plot(va, reset = FALSE)
plot(intersection, add = TRUE)

# Ggplot
ggplot() +
  geom_sf(data = va) +
  geom_sf(data = st_intersection(va, ineligibility), fill = "orange") + 
  labs(title = "Map of RUS program ineligibility in VA")
