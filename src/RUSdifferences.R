# devtools::install_github('rstudio/gt')
library(sf)
library(sp)
library(rgdal)
library(tigris)
library(tidycensus)
library(dplyr)
library(naniar)
library(gt)

options(tigris_use_cache = TRUE)
census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key

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

# Join 2 ineligible into 1: 
# ineligible <- st_join(urban, over20k) --> this looks good on a map, but how is it possible I'm getting 1731 rows? (55 + 1448 = 1503). 
# I don't understand exactly what happens, but this version must be incorrect.

# Join using the "remove and rejoin geometry" workaround since there is no bind_rows for sp or sf objects
ineligible_join <- bind_rows(data.frame(urban), data.frame(over20k))
ineligible_join$geometry <- c(urban$geometry, over20k$geometry)
ineligible <- st_as_sf(ineligible_join)

plot(st_geometry(stateVA))
plot(st_geometry(ineligible), add = TRUE, col = "blue")

# Invert to get eligible
# A couple of places are getting lost here (see map). Must be something about the type of geometry or st_difference. Figure out! There is clearly
# something I don't get about geometry operations.
eligible <- st_difference(stateVA, st_union(ineligible))

plot(st_geometry(stateVA))
plot(st_geometry(eligible), add = TRUE, col = "blue")
plot(st_geometry(ineligible), add = TRUE, col = "red")

#
# Check ----------------------------------------------------------------------------------------------------------------------------
#

# stateVA: 1907 rows
# ineligible (urban+over20k): 1448 + 55 = 1503 rows
# eligible (stateVA-ineligible): 953 rows -- this does not match (1907-1503=404), see note about inverting to get eligible above, BUT it looks right on a map?!

plot(st_geometry(eligible), add = TRUE, col = "black")
plot(st_geometry(ineligible), add = TRUE, col = "red")

summary(stateVA)
summary(ineligible) 
summary(eligible) 

str(stateVA)
str(ineligible)
str(eligible)

names(stateVA) # AFFGEOID, GEOID
names(ineligible) # AFFGEOID, GEOID, GEOID10, GEO_ID
names(eligible) # AFFGEOID, GEOID

class(stateVA)
class(ineligible)
class(eligible)


#
# Get sociodemographics, adapted from Josh ----------------------------------------------------------------------------------------
#

# Get variables
acs_vars <- c("B15003_001","B15003_002","B15003_003","B15003_004","B15003_005","B15003_006","B15003_007","B15003_008","B15003_009",
              "B15003_010","B15003_011","B15003_012","B15003_013","B15003_014","B15003_015","B15003_016","B15003_017","B15003_018",
              "B17020_001","B17020_002",
              "B01001_001","B01001_020","B01001_021","B01001_022","B01001_023","B01001_024","B01001_025",
              "B01001_044","B01001_045","B01001_046","B01001_047","B01001_048","B01001_049",
              "B03003_001","B03003_003",
              "B02001_001","B02001_003",
              "B09019_002","B09019_003",
              "B05002_001","B05002_013")

acs_est <- get_acs(geography = "tract", state = "Virginia", variables = acs_vars, year = 2015, cache_table = TRUE,
                    geometry = TRUE, keep_geo_vars = TRUE, output = "wide")

# Compute rates
acs_estimates <- acs_est %>% transmute(
                    STATEFP = STATEFP,
                    COUNTYFP = COUNTYFP,
                    TRACTCE = TRACTCE,
                    AFFGEOID = AFFGEOID, 
                    GEOID = GEOID,
                    population = B01001_001E,
                    hs_or_less = (B15003_002E+B15003_003E+B15003_004E+B15003_005E+B15003_006E+B15003_007E+B15003_008E+B15003_009E+B15003_010E+
                                    B15003_011E+B15003_012E+B15003_013E+B15003_014E+B15003_015E+B15003_016E+B15003_017E+B15003_018E) / B15003_001E,
                    poverty = B17020_002E / B17020_001E,
                    age_65_older = (B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E+
                                      B01001_044E+B01001_045E+B01001_046E+B01001_047E+B01001_048E+B01001_049E)/ B01001_001E,
                    hispanic = B03003_003E / B03003_001E,
                    black = B02001_003E / B02001_001E,
                    family = B09019_003E / B09019_002E,
                    foreign = B05002_013E / B05002_001E)

summary(acs_estimates)

# Join with areas, with geography: This still looks correct on a map after the join, but how come there are so many more rows? Must mean multiple rows from ACS go with
# a single row from ineligible/eligible?
ineligible_acs <- st_join(ineligible, acs_estimates, left = TRUE)
eligible_acs <- st_join(eligible, acs_estimates, left = TRUE)

plot(st_geometry(ineligible_acs))
plot(st_geometry(eligible_acs))

# Join with areas, without geography
# head(ineligible$GEOID)
# head(eligible$GEOID)
# head(acs_estimates$GEOID)
# 
# names(ineligible)
# names(eligible)
# 
# ineligible1 <- ineligible
# st_geometry(ineligible1) <- NULL
# eligible1 <- eligible
# st_geometry(eligible1) <- NULL
# acs_estimates1 <- acs_estimates
# st_geometry(acs_estimates1) <- NULL
# 
# ineligible_acs <- ineligible %>% left_join(acs_estimates1, by = "GEOID")
# eligible_acs <- eligible %>% left_join(acs_estimates1, by = "GEOID")


#
# Inspect ----------------------------------------------------------------------------------------
#

# Baseline missingness
gg_miss_var(acs_estimates)
gg_miss_var(ineligible)
gg_miss_var(eligible)

# Join missingness - makes sense
gg_miss_var(ineligible_acs)
gg_miss_var(eligible_acs)

# Test plot
plot(eligible_acs["family"])
plot(ineligible_acs["family"])


#
# Comparison ----------------------------------------------------------------------------------------
#


ineligible_comp <- ineligible_acs %>% st_set_geometry(NULL) %>% 
  select(population, hs_or_less, poverty, age_65_older, hispanic, black, family, foreign) %>% 
  summarize_all(c("mean", "sd", "min", "max"), na.rm = TRUE) %>% 
  round(2) %>%
  gt()

eligible_comp <- eligible_acs %>% st_set_geometry(NULL) %>% 
  select(population, hs_or_less, poverty, age_65_older, hispanic, black, family, foreign) %>% 
  summarize_all(c("mean", "sd", "min", "max"), na.rm = TRUE) %>% 
  round(2) %>%
  gt()






