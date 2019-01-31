library(sf)
library(sp)
library(rgdal)
library(tigris)
library(tidycensus)
library(dplyr)
library(naniar)
library(gt) # devtools::install_github('rstudio/gt')
library(tidyr)
library(ggplot2)

options(tigris_use_cache = TRUE)
census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key


#
# Get VA state map ----------------------------------------------------------------------------------------------
#

stateVA <- get_acs(geography = "tract", variables = "B01003_001", 
          state = "Virginia", geometry = TRUE, keep_geo_vars = TRUE,
          output = "wide")

table(st_geometry_type(stateVA)) # 1907 multipolygons
plot(st_geometry(stateVA))


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

acs_estimates <- acs_estimates %>% st_set_geometry(NULL) 


#
# Join ACS with VA --------------------------------------------------------------------------------------------------------------------
#

stateVAacs <- merge(stateVA, acs_estimates, all.x = TRUE)

# Test plot
plot(stateVAacs["family"])


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

urban <- st_intersection(stateVAacs, urbanized_areas)
plot(st_geometry(urban))

# Extract only polygons
#test1 <- st_collection_extract(urban, type = "POLYGON")
#plot(st_geometry(test1))
#test2 <- st_collection_extract(urban, type = "POINT")
#plot(st_geometry(test2))
#plot(st_geometry(test1), col = "blue")
#plot(st_geometry(test2), add = TRUE, col = "red")

table(st_geometry_type(urban)) # 4 points, 1147 polygons, 81 multipolygons, 215 geometry collection
#test <- st_collection_extract(urban, type = "POLYGON")
#plot(st_geometry(urban))
#plot(st_geometry(test), add = TRUE, col = "red", border = "red")

urban1 <- urban
urban <- st_collection_extract(urban, type = "POLYGON")


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
# Get places with at least 10 Mbps downstream/1 Mbps upstream ---------------------------------------
#

fcc_subscription <- read.csv("data/fcc_subscription/tract_map_dec_2015.csv")
colnames(fcc_subscription)[colnames(fcc_subscription) == "tractcode"] <- "GEOID"

test <- fcc_subscription$GEOID %in% eligible$GEOID
fcc_subscription$test <- fcc_subscription$GEOID %in% eligible$GEOID
fcc_subscription <- fcc_subscription %>% filter(test == TRUE)
  
test <- eligible %>% filter(GEOID %in% fcc_subscription$GEOID)  
# I think everyone has at least 10mbps/1...


#
# Get eligible and ineligible areas --------------------------------------------------------------------------------------------
#

# Join using the "remove and rejoin geometry" workaround since there is no bind_rows for sp or sf objects
ineligible_join <- bind_rows(data.frame(urban), data.frame(over20k))
ineligible_join$geometry <- c(urban$geometry, over20k$geometry)
ineligible <- st_as_sf(ineligible_join)

# test <- st_union(urban, over20k) --> This works too but has a ridiculous number of rows. 
# test <- st_union(st_union(urban), st_union(over20k)) --> this just gives a multipolygon with no other data
# test <- st_union(urban, st_union(over20k)) --> This works but is extremely slow.

# This looks okay, but why are there so many rows regardless of whether I do this with st_join or st_union or dplyr joins?
plot(st_geometry(stateVA))
plot(st_geometry(over20k), add = TRUE, col = "blue")
plot(st_geometry(urban), add = TRUE, col = "red")

plot(st_geometry(stateVA))
plot(st_geometry(ineligible), add = TRUE, col = "blue", border = "blue")

test <- duplicated(ineligible$geometry)
table(test)

# Invert to get eligible: same problem as above with row #s.
eligible <- st_difference(stateVAacs, st_union(ineligible))

# test1 <- st_join(stateVAacs, st_union(ineligible), join = st_difference)

plot(st_geometry(stateVA))
plot(st_geometry(eligible), add = TRUE, col = "blue")
plot(st_geometry(ineligible), add = TRUE, col = "red", border = "red")


#
# Check ----------------------------------------------------------------------------------------------------------------------------
#

plot(st_geometry(eligible), col = "blue")
plot(st_geometry(ineligible), col = "red")

summary(stateVAacs)
summary(ineligible) 
summary(eligible) 

str(stateVAacs)
str(ineligible)
str(eligible)

names(stateVAacs) # AFFGEOID, GEOID
names(ineligible) # AFFGEOID, GEOID, GEOID10, GEO_ID
names(eligible) # AFFGEOID, GEOID

class(stateVAacs)
class(ineligible)
class(eligible)


#
# Inspect ----------------------------------------------------------------------------------------
#

# Baseline missingness
gg_miss_var(acs_estimates)
gg_miss_var(ineligible)
gg_miss_var(eligible)

# Join missingness - makes sense
gg_miss_var(ineligible)
gg_miss_var(eligible)

# Attributes/relation of data to geometry
# relation_to_geometry notes how attributes relate to the geometry: 
# constant (field), aggregated over the geometry (lattice), identify individual entities (buildings, parcels etc.)?
attributes(stateVAacs)
attributes(eligible)
attributes(ineligible)

# Test plot
plot(eligible["family"])
plot(ineligible["family"])

plot(eligible["population"])
plot(ineligible["population"])


#
# Pretty map ----------------------------------------------------------------------------------------
#

ggplot() +
  geom_sf(data = eligible, aes(color = "Eligible")) +
  geom_sf(data = ineligible, aes(color = "Ineligible")) +
  labs(title = "Virginia tracts by RUS grant eligibility", color = "Status", 
       caption = "Note: Ineligible areas defined as rural areas (over 20,000 inhabitants or 2010 Census urbanized areas.") +
  theme_bw()


#
# Comparison ----------------------------------------------------------------------------------------
#

# Tables
inel <- ineligible %>% st_set_geometry(NULL) %>% 
  select(population, hs_or_less, poverty, age_65_older, hispanic, black, family, foreign) %>% 
  summarize_all(c("mean", "median", "sd", "min", "max"), na.rm = TRUE) %>% 
  round(2) %>%
  gather("variable", "value_inel")

el <- eligible %>% st_set_geometry(NULL) %>% 
  select(population, hs_or_less, poverty, age_65_older, hispanic, black, family, foreign) %>% 
  summarize_all(c("mean", "median", "sd", "min", "max"), na.rm = TRUE) %>% 
  round(2) %>%
  gather("variable", "value_el")

comparison <- merge(inel, el, by = "variable", sort = FALSE)

# Tables when population != 0
inel_nozero <- ineligible %>% st_set_geometry(NULL) %>% 
  filter(population != 0) %>%
  select(population, hs_or_less, poverty, age_65_older, hispanic, black, family, foreign) %>% 
  summarize_all(c("mean", "median", "sd", "min", "max"), na.rm = TRUE) %>% 
  round(2) %>%
  gather("variable", "value_inel")

el_nozero <- eligible %>% st_set_geometry(NULL) %>% 
  filter(population != 0) %>%
  select(population, hs_or_less, poverty, age_65_older, hispanic, black, family, foreign) %>% 
  summarize_all(c("mean", "median", "sd", "min", "max"), na.rm = TRUE) %>% 
  round(2) %>%
  gather("variable", "value_el")

comparison <- merge(inel_nozero, el_nozero, by = "variable", sort = FALSE)

