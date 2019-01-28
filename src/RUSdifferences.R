library(sf)
library(tigris)
library(tidycensus)


#
# Get VA state map ----------------------------------------------------------------------------------------------
#

stateVA <- get_acs(geography = "tract", variables = "B01003_001", 
          state = "Virginia", geometry = TRUE, keep_geo_vars = TRUE,
          output = "wide")

plot(st_geometry(stateVA))


#
# Get VA urbanized areas map (ineligible), adapted from Josh --------------------------------------------------------------------------
#

urbanized_areas <- readOGR(dsn = "data/census_2010_urbanized_areas/", layer = "tl_2010_us_uac10")

# filter only Urbanized Areas, exclude Urban Clusters
urbanized_areas <- urbanized_areas[urbanized_areas@data$UATYP10=="U", ]

# filter Urbanized Areas in VA
urbanized_areas <- st_as_sf(urbanized_areas)

# Transform/match CRS
st_crs(stateVA)
urbanized_areas <- st_transform(urbanized_areas, st_crs(stateVA))
st_crs(urbanized_areas)

urban <- st_intersection(stateVA, urbanized_areas)
plot(st_geometry(urban))


#
# Get places over 2K (ineligible), adapted from Josh ----------------------------------------------------------------------------
#

# Get places and GEOIDs
proj4string <- CRS('+proj=longlat +ellps=WGS84')
places <- readOGR(dsn = "data/census_2010_places/VA/", layer = "gz_2010_51_160_00_500k")
places <- spTransform(places, proj4string)

place_pop <- get_decennial(geography = "place", state = "VA", variables = "P001001", year = 2010, cache_table = TRUE)
places_VA <- places@data
places_VA$GEO_ID <- paste(places_VA$GEO_ID)
places_VA$GEOID <- substr(places_VA$GEO_ID, nchar(places_VA$GEO_ID)-6, nchar(places_VA$GEO_ID))
places_VA2 <- places_VA %>% left_join(place_pop, by = "GEOID")

# Filter places with population > 20,000
places_over20k <- places[places_VA2$value > 20000, ]

# Transform/match CRS
over20k <- st_as_sf(places_over20k)
st_crs(over20k)
st_crs(stateVA)

over20k <- st_transform(over20k, st_crs(stateVA))


#
# Get eligible and ineligible areas --------------------------------------------------------------------------------------------
#

# Join 2 ineligible into 1
ineligible <- st_join(urban, over20k)

plot(st_geometry(stateVA))
plot(st_geometry(ineligible), add = TRUE, col = "blue")

# Invert to get eligible
eligible <- st_difference(stateVA, st_union(ineligible))

plot(st_geometry(stateVA))
plot(st_geometry(eligible), add = TRUE, col = "blue")


#
# Check ----------------------------------------------------------------------------------------------------------------------------
#

summary(stateVA)
summary(ineligible) 
summary(eligible) 

str(stateVA)
str(ineligible)
str(eligible)

names(stateVA) # AFFGEOID, GEOID
names(ineligible) # AFFGEOID, GEOID, GEOID10, GEO_ID
names(eligible) # AFFGEOID, GEOID



#
# Get demographics ----------------------------------------------------------------------------------------------------------------------------
#
