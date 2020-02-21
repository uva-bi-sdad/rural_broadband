# BIP funded areas

# REFERENCE SHAPES
# 1 - All US States
# 2 - Counties *  
# 3 - Census Tracts * 
# 4 - Census Blocks * 
# 5 - Zip codes

# NOTE: zipcodes more efficient than counties 
# harder to find zipcode shapes (in time?) and also (maybe) ERS will want to analyze at county level


# functions for placing in shapes (means - state & donut holes) per BIP area 
### function house_whichstates (bipgeometry)
### get back df of states x subcomponent shapes
###
### function house_donutgenerator ( state, subcomponent shapes)
### get back subcomponent shapes x donut shapes

library(sf)
library(dplyr)

get_states_for_weird_shape <- function(weird_shape) {
  states <- sf::st_as_sf(tigris::states())
  weird_shape <- sf::st_transform(weird_shape, crs=sf::st_crs(states))
  shape_states <- sf::st_intersection(weird_shape, states)
  shape_states <- select(shape_states, c(1:ncol(weird_shape), "STUSPS"))
  shape_states
}

View(tibble(1:ncol(BIP_Approved), colnames(BIP_Approved)))
BIP_Approved <- readRDS("~/BIP_Approved.RDS")
our_weird_shape <- BIP_Approved[1,c(4:10, 37)] 
states <- tigris::states() %>% sf::st_as_sf()
our_weird_shape <- our_weird_shape %>% sf::st_transform(sf::st_crs(states))
shape_in_states <- get_states_for_weird_shape(our_weird_shape)
shape_in_states
cali <- states %>% filter(STUSPS == "CA")
plot(cali$geometry)
plot(shape_in_states$geometry,add=TRUE,col="red")
plot(our_weird_shape$geometry,add=TRUE,col="red")

get_donuts <- function(weird_shape, d) {
  # 1 mile = 1609.34
  # 5 mile = 8046.72
  # 10 mile = 16093.4
  # 25 mile = 40233.6
  #d <- recode(d, `1` = 1609.34, `5` = 8046.72, `10` = 16093.4, `25` = 40233.6)
  d <- d*1609.333333333333333333333333333333
  weird_shape <- weird_shape %>% st_transform(st_crs(3857))
  donut <- sf::st_buffer(weird_shape, dist = d, endCapStyle = "ROUND") 
  print("Note - Results have CRS 3857 so units of meters can be used")
  donut
}


BIP_Approved

donuts1 <- get_donuts(weird_shape = shape_in_states, d = 1)
donuts2 <- get_donuts(weird_shape = shape_in_states, d = 5)
donuts3 <- get_donuts(weird_shape = shape_in_states, d = 10)
donuts4 <- get_donuts(weird_shape = shape_in_states, d = 25)

plot(cali$geometry)
plot(donuts4, add = TRUE, col= "blue")
plot(donuts3, add = TRUE, col= "green")
plot(donuts2, add = TRUE, col= "yellow")
plot(donuts1, add = TRUE, col= "red")

place_house_in_bip_or_donut <- function(coords, shape) {
  
}


usa <- tigris::states()
usa <- usa %>% sf::st_as_sf()
usa <- usa %>% filter(GEOID != 78 & GEOID != 69 & GEOID != 66 & GEOID != 60)
plot(usa[2])
usa <- usa %>% st_transform(st_crs(3857))
usa48 <- usa %>% filter(STUSPS != "PR" & STUSPS != "HI" & STUSPS != "AK")

BIP_Approved <- readRDS("~/BIP_Approved.RDS")
# bip_1mile <- readRDS("donuts/bip_1miledonuts.RDS")
# bip_5mile <- readRDS("donuts/bip_5miledonuts.RDS")
# bip_10mile <- readRDS("donuts/bip_10miledonuts.RDS")
# bip_25mile <- readRDS("donuts/bip_25miledonuts.RDS")
mile1donuts <- readRDS("donuts/bip_1miledonuts.RDS")
mile5donuts <- readRDS("donuts/bip_5miledonuts.RDS")
mile10donuts <- readRDS("donuts/bip_10miledonuts.RDS")
mile25donuts <- readRDS("donuts/bip_25miledonuts.RDS")

plot(usa48$geometry)
plot(mile25donuts$geometry, add = TRUE, col= "blue")
plot(mile10donuts$geometry, add = TRUE, col= "green")
plot(mile5donuts$geometry, add = TRUE, col= "yellow")
plot(mile1donuts$geometry, add = TRUE, col= "red")

mile1donuts <- get_donuts(BIP_Approved, d = 1)
mile5donuts <- get_donuts(BIP_Approved, d = 5)
mile10donuts <- get_donuts(BIP_Approved, d = 10)
mile25donuts <- get_donuts(BIP_Approved, d = 25)

saveRDS(mile1donuts, "donuts/bip_1miledonuts.RDS")
saveRDS(mile5donuts, "donuts/bip_5miledonuts.RDS")
saveRDS(mile10donuts, "donuts/bip_10miledonuts.RDS")
saveRDS(mile25donuts, "donuts/bip_25miledonuts.RDS")
