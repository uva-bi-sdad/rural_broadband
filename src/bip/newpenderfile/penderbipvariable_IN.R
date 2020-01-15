### POSSIBLE DATASETS
# ALL PENDER ORIGINAL CORELOGIC MERGED DATA
#sasdata <- read_dta("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deed_tax_merge_all.dta")
# ALL PROPERTIES NO BIP VARIABLE "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxall_bipvariable.RDS"
deedtaxall <- readRDS("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deed_tax_merge_all.RDS")
# ALL PROPERTIES W BIP VARIABLE "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxall_bipvariable.RDS"
#deedtaxall <- readRDS("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxall_bipvariable.RDS")
# VIRGINIA PROPERTIES W BIP VARIABLE "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxmergeall_va_bipvariable.RDS"
#deedtax_VA_penderbipvariable <- readRDS("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxmergeall_va_bipvariable.RDS")

#deedtaxallIN <- deedtaxall %>% filter(state == "IN")

### PACKAGES
library(dplyr)
library(sf)
library(tigris)
#library(haven)

### READ PROPERTIES IN 
deedtaxallIN <- readRDS("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxmergeall_in.RDS")
#deedtaxallIN <- deedtaxallIN %>% select(apnparcelnumberunformatted, state, propertylevellatitude, propertylevellongitude)
### READ BIP SHAPES IN 
bip_approved_join <- readRDS("~/git/rural_broadband/data/working/BIP_working/BIP_Approved.RDS") # EPSG: 3857 
### READ STATE BOUNDARIES IN
INgeom <- states()
INgeom <- INgeom[INgeom$STATEFP=="18",]
INgeom <- st_as_sf(INgeom) # EPSG: 4269 
### TRANSFORM BIP SHAPES TO STATE BOUNDARIES CRS
bip_shape <- st_transform(bip_approved_join, crs=st_crs(INgeom))
### IDENTIFY BIP SHAPES INSIDE STATE BOUNDARIES
test <- st_intersects(bip_shape, INgeom)
bipIN <- bip_shape[lengths(test)>0,]
plot(INgeom$geometry)
plot(bipIN$geometry,add=TRUE,col="red")
### TRANSFORM PROPERTIES TO SF 
deedtaxIN2 <- st_as_sf(deedtaxallIN, coords = c("propertylevellongitude", "propertylevellatitude"))
### TRANSFORM PROPERTIES TO BIP SHAPES CRS
st_crs(deedtaxIN2) <- st_crs(bipIN)
### IDENTIFY PROPERTIES INSIDE BIP SHAPES
test <- st_within(deedtaxIN2, bipIN)
table(lengths(test))
#0        1     2      3       5
#355735   763   23840  167    5
### IDENTIFY PROPERTIES INSIDE BIP SHAPES - VIRGINIA
deedtaxIN2$bip <- st_within(deedtaxIN2, bipIN) %>% lengths >0
table(deedtaxIN2$bip)
# FALSE - 355735
# TRUE - 24775

# # MAKE VA SUBSET
# Search for columns
# colnames(deedtaxall)[stringr::str_detect(colnames(deedtaxall), "bath") == TRUE]
#
# deedtax_VA_penderbipvariable <- deedtaxall %>% 
#   filter(state == "VA") %>% 
#   select(apnparcelnumberunformatted, city, state, year, taxyear, assessedyear, # property-year identifiers
#          metro2013,
#          saleamount, assdtotalvalue, mkttotalvalue, # property values
#          acres, landsquarefootage, buildingsquarefeet, livingsquarefeet, grosssquarefeet, # area
#          yearbuilt, age,
#          bedrooms, 
#          totalbathscalculated, totalbaths, fullbaths, halfbaths, bathfixtures, 
#          bip, 
#          geometry)


plot(INgeom$geometry)
#plot(test$geometry, add = TRUE, col = "red")
plot(bipIN$geometry, add = TRUE)
plot(deedtaxIN2$geometry, add = TRUE, col = "red")
deedtax_IN_penderbipvariable <- deedtaxIN2
#saveRDS(deedtaxIN2, "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxmergeall_in_bipvariable.RDS")

# Which ones are your properties?  DON'T NEED these: bip_va_houses <- deedtax_VA_penderbipvariable %>% filter(bip == TRUE)
head(deedtax_IN_penderbipvariable)
st_crs(deedtax_IN_penderbipvariable)

## REFERENCE SHAPES
indiana_bip_sites <- bipIN %>% filter(STUSPS == "IN") %>% st_transform(st_crs(3857))
INgeom <- INgeom %>% st_transform(st_crs(3857))
deedtax_IN_penderbipvariable <- deedtax_IN_penderbipvariable %>% st_transform(st_crs(3857))
bipIN <- bipIN %>% st_transform(st_crs(3857))

buffer_1mi <- st_buffer(indiana_bip_sites, dist = 1609.34, endCapStyle = "ROUND") #1 mile
buffer_5mi <- st_buffer(indiana_bip_sites, dist = 8046.72, endCapStyle = "ROUND") #5 miles
buffer_10mi <- st_buffer(indiana_bip_sites, dist = 16093.4, endCapStyle = "ROUND") #10 miles
buffer_25mi <- st_buffer(indiana_bip_sites, dist = 40233.6, endCapStyle = "ROUND") #25 miles
#buffer_50mi <- st_buffer(indiana_bip_sites, dist = 80467.2, endCapStyle = "ROUND") #50 miles

plot(INgeom$geometry)
plot(bipIN$geometry, add = TRUE)
plot(buffer_1mi$geometry, add = TRUE)
plot(buffer_5mi$geometry, add = TRUE)
plot(buffer_10mi$geometry, add = TRUE)
plot(buffer_25mi$geometry, add = TRUE)
#plot(buffer_50mi$geometry, add = TRUE)
plot(deedtax_IN_penderbipvariable$geometry, add = TRUE, col = "red")

st_crs(buffer_5mi)
st_crs(deedtax_IN_penderbipvariable)
head(deedtax_IN_penderbipvariable$geometry)
head(buffer_5mi$geometry)

head(deedtax_IN_penderbipvariable$bip)
test <- deedtax_VA_penderbipvariable %>% filter(bip == TRUE)
deedtax_IN_penderbipvariable$bip_1mi <- st_within(deedtax_IN_penderbipvariable, buffer_1mi) %>% lengths >0
deedtax_IN_penderbipvariable$bip_5mi <- st_within(deedtax_IN_penderbipvariable, buffer_5mi) %>% lengths >0
deedtax_IN_penderbipvariable$bip_10mi <- st_within(deedtax_IN_penderbipvariable, buffer_10mi) %>% lengths >0
deedtax_IN_penderbipvariable$bip_25mi <- st_within(deedtax_IN_penderbipvariable, buffer_25mi) %>% lengths >0
#deedtax_IN_penderbipvariable$bip_50mi <- st_within(deedtax_IN_penderbipvariable, buffer_50mi) %>% lengths >0

table(deedtax_IN_penderbipvariable$bip_1mi)
table(deedtax_IN_penderbipvariable$bip_5mi)
table(deedtax_IN_penderbipvariable$bip_10mi)
table(deedtax_IN_penderbipvariable$bip_25mi)
#table(deedtax_IN_penderbipvariable$bip_50mi)

#saveRDS(deedtax_VA_penderbipvariable, "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxmergeall_va_bipvariables.RDS")

deedtax_IN_penderbipvariable <- deedtax_IN_penderbipvariable %>% 
  mutate(bip_c = paste(stringr::str_extract(bip, "^.{1}"),
                       stringr::str_extract(bip_1mi, "^.{1}"),
                       stringr::str_extract(bip_5mi, "^.{1}"),
                       stringr::str_extract(bip_10mi, "^.{1}"),
                       stringr::str_extract(bip_25mi, "^.{1}")),
         bip_n = stringr::str_count(bip_c, "T"), 
         bip_distance = recode(bip_n, `1` = "25 miles", `2` = "10 miles", `3` = "5 miles", `4` = "1 mile", `5` = "BIP region", `0` = "Non-BIP region"))

#saveRDS(deedtax_IN_penderbipvariable, "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxmergeall_in_bipvariables.RDS") 


in_near_bip_homes <- deedtax_IN_penderbipvariable %>% filter(bip_distance !="Non-BIP region") 

plot(INgeom$geometry)
plot(bipIN$geometry, add = TRUE)
plot(buffer_1mi$geometry, add = TRUE)
plot(buffer_5mi$geometry, add = TRUE)
plot(buffer_10mi$geometry, add = TRUE)
plot(buffer_25mi$geometry, add = TRUE)
#plot(buffer_50mi$geometry, add = TRUE)
plot(in_near_bip_homes$geometry, add = TRUE, col = "red")

deedtax_IN_penderbipvariable_nogeom <- deedtax_IN_penderbipvariable
st_geometry(deedtax_IN_penderbipvariable_nogeom) <- NULL

deedtax_IN_penderbipvariable_nogeom2 <- deedtax_IN_penderbipvariable_nogeom %>%
  select(apnparcelnumberunformatted, formattedapn, #property identifier
         transactiontype, pricatcode, #transaction identifier? 
         city, state, censustract, zipcode, # geography
         year, taxyear, assessedyear, # time
         metro2013, #rurality
         saleamount, assdtotalvalue, mkttotalvalue, totalvaluecalculated, totalvaluecalculatedind, # property values
         acres, landsquarefootage, buildingsquarefeet, livingsquarefeet, grosssquarefeet, universalbuildingsquarefeet, # area
         yearbuilt, age,
         bedrooms,
         totalbathscalculated, 
         bip_distance)

st_geometry(deedtax_IN_penderbipvariable_nogeom2) <- NULL

haven::write_dta(deedtax_IN_penderbipvariable_nogeom2, "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deed_tax_merge_in_bipvariables.dta")

in_bip_homes <- deedtax_IN_penderbipvariable %>% filter(bip_distance == "BIP region") 
in_1mi_homes <- deedtax_IN_penderbipvariable %>% filter(bip_distance == "1 mile") 
in_5mi_homes <- deedtax_IN_penderbipvariable %>% filter(bip_distance == "5 miles") 
in_10mi_homes <- deedtax_IN_penderbipvariable %>% filter(bip_distance == "10 miles") 
in_25mi_homes <- deedtax_IN_penderbipvariable %>% filter(bip_distance == "25 miles") 
in_notbip_homes <- deedtax_IN_penderbipvariable %>% filter(bip_distance == "Non-BIP region") 
table(deedtax_IN_penderbipvariable$bip_distance)


in_notbip_homes %>% 
  select(apnparcelnumberunformatted, formattedapn, #property identifier
         transactiontype, pricatcode, #transaction identifier? 
         city, state, censustract, zipcode, # geography
         year, taxyear, assessedyear, # time
         metro2013, #rurality
         saleamount, assdtotalvalue, mkttotalvalue, totalvaluecalculated, totalvaluecalculatedind, # property values
         acres, landsquarefootage, buildingsquarefeet, livingsquarefeet, grosssquarefeet, universalbuildingsquarefeet, # area
         yearbuilt, age,
         bedrooms,
         totalbathscalculated, 
         bip_distance) %>% summary()
