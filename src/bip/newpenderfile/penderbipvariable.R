#library(haven)
#sasdata <- read_dta("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deed_tax_merge_all.dta")

library(dplyr)
library(sf)
library(tigris)
deedtaxVA <- readRDS("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxmergeall_va.RDS")
deedtaxVA <- deedtaxVA %>% select(apnparcelnumberunformatted, state, propertylevellatitude, propertylevellongitude)
bip_approved_join <- readRDS("~/git/rural_broadband/data/working/BIP_working/BIP_Approved.RDS")
VAgeom <- states()
VAgeom <- VAgeom[VAgeom$STATEFP=="51",]
VAgeom <- st_as_sf(VAgeom)
bip_shape <- st_transform(bip_approved_join, crs=st_crs(VAgeom))
test <- st_intersects(bip_shape, VAgeom)
bipVA <- bip_shape[lengths(test)>0,]
plot(VAgeom$geometry)
plot(bipVA$geometry,add=TRUE,col="red")
deedtaxVA2 <- st_as_sf(deedtaxVA, coords = c("propertylevellongitude", "propertylevellatitude"))
st_crs(deedtaxVA2) <- st_crs(bipVA)
test <- st_within(deedtaxVA2, bipVA)
table(lengths(test))
#0        2      6
#585123    231     64

deedtaxVA2$bip <- st_within(deedtaxVA2, bipVA) %>% lengths >0
table(deedtaxVA2$bip)
# FALSE - 585123
# TRUE - 295

#deedtaxall <- readRDS("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deed_tax_merge_all.RDS")
deedtaxall <- st_as_sf(deedtaxall, coords = c("propertylevellongitude", "propertylevellatitude"))
st_crs(deedtaxall) <- st_crs(bip_shape)
deedtaxall$bip <- st_within(deedtaxall, bip_shape) %>% lengths >0


#saveRDS(deedtaxall, "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxall_bipvariable.RDS")


deedtaxall <- readRDS("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxall_bipvariable.RDS")
table(deedtaxall$bip) #7,262,049 FALSE 77,387 TRUE
tibble(Column = c(, "Price", "Bedrooms"))

summary(deedtaxall$)

colnames(deedtaxall)
colnames(deedtaxall)[stringr::str_detect(colnames(deedtaxall), "bath") == TRUE]
table(deedtaxall)

deedtax_VA_penderbipvariable <- deedtaxall %>% 
  filter(state == "VA") %>% 
  select(apnparcelnumberunformatted, city, state, year, taxyear, assessedyear, # property-year identifiers
         metro2013,
         saleamount, assdtotalvalue, mkttotalvalue, # property values
         acres, landsquarefootage, buildingsquarefeet, livingsquarefeet, grosssquarefeet, # area
         yearbuilt, age,
         bedrooms, 
         totalbathscalculated, totalbaths, fullbaths, halfbaths, bathfixtures, 
         bip, 
         geometry)
library(sf)
#saveRDS(deedtax_VA_penderbipvariable, "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxmergeall_va_bipvariable.RDS")
rm(deedtaxall)

summary(deedtax_VA_penderbipvariable) 

biptrueva <- deedtax_VA_penderbipvariable %>% filter(bip == TRUE)
plot(VAgeom$geometry)
plot(biptrueva$geometry,add=TRUE,col="red")
plot(biptrueva[5])

colnames(biptrueva)
tibble("Column" = c("Sale Amount",),
       "Min" = c(min(biptrueva$saleamount)),
       "Max" = c(max(biptrueva$saleamount)),
       "Mean" = c(mean(biptrueva$saleamount)),
       "StDev" = c(sd(biptrueva$saleamount)))

tibble("Column" = c("Metro 2013"),
       "Values" = unique(biptrueva$metro2013),
       "Max" = c(287, 8))
       
       
table(biptrueva$assessedyear)

plot(VAgeom$geometry)
plot(virginia_bip_sites, add=TRUE, col = "red")

virginia_bip_sites <- bip_approved_join %>% filter(STUSPS == "VA")
isochrome5 <- sf::st_buffer(virginia_bip_sites, dist = 5)
isochrome15 <- sf::st_buffer(virginia_bip_sites, dist = 15)
isochrome50 <- sf::st_buffer(virginia_bip_sites, dist = 50)

isochrome5 <- sf::st_buffer(virginia_bip_sites, dist = 5)
isochrome15 <- sf::st_buffer(virginia_bip_sites, dist = 15)
isochrome50 <- sf::st_buffer(virginia_bip_sites, dist = 50)

plot(st_buffer(virginia_bip_sites, dist = .0001, endCapStyle = "ROUND")[1])
plot(st_buffer(st_centroid(virginia_bip_sites), dist = 3, endCapStyle = "ROUND")[1])
plot(virginia_bip_sites[1])


##########
library(units)
library(tidyverse)
library(sf)
library(mapview)
library(units)

# define nautical miles (as per ICAO notation)
NM <- make_unit("NM")
install_conversion_constant("NM", "km", 1.852)

# DUB/EIDW location, see
# https://skyvector.com/airport/EIDW/Dublin-Airport
# Coordinates:
#   N53°25.28' / W6°16.20' (Degrees Decimal Minutes (DDM) format)
#   (-6.27, 53.421333) (lon/lat Decimal Degrees (DD))
# Elevation: 242.0 feet (MSL)
dub_lon <- -6.27
dub_lat <- 53.421333
dub_elv <- set_units(242.0, ft)
dub <- st_point(x = c(dub_lon, dub_lat, dub_elv), dim = "XYZ")
dub <- dub %>% st_sfc(crs = 4326)

# define radious of interest, i.e. 110 NM
r110 <- set_units(110, NM) %>% set_units(km) %>% set_units(m)

# change to Irish grid, which uses meters
dub <- st_transform(dub, 29902)
dub_buffer <-  st_buffer(dub, r110)

# eventually convert back to WSG84 if needed for other purposes
dub <- st_transform(dub, 4326)
dub_buffer <- st_transform(dub_buffer, 4326)
mapview(dub_buffer)
