### POSSIBLE DATASETS
# ALL PENDER ORIGINAL CORELOGIC MERGED DATA
#sasdata <- read_dta("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deed_tax_merge_all.dta")
# ALL PROPERTIES NO BIP VARIABLE "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxall_bipvariable.RDS"
#deedtaxall <- readRDS("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deed_tax_merge_all.RDS")
# ALL PROPERTIES W BIP VARIABLE "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxall_bipvariable.RDS"
#deedtaxall <- readRDS("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxall_bipvariable.RDS")
# VIRGINIA PROPERTIES W BIP VARIABLE "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxmergeall_va_bipvariable.RDS"
deedtax_VA_penderbipvariable <- readRDS("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxmergeall_va_bipvariable.RDS")

### PACKAGES
library(dplyr)
library(sf)
library(tigris)
#library(haven)

### READ PROPERTIES IN 
deedtaxVA <- readRDS("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxmergeall_va.RDS")
deedtaxVA <- deedtaxVA %>% select(apnparcelnumberunformatted, state, propertylevellatitude, propertylevellongitude)
### READ BIP SHAPES IN 
bip_approved_join <- readRDS("~/git/rural_broadband/data/working/BIP_working/BIP_Approved.RDS") # EPSG: 3857 
### READ STATE BOUNDARIES IN
VAgeom <- states()
VAgeom <- VAgeom[VAgeom$STATEFP=="51",]
VAgeom <- st_as_sf(VAgeom) # EPSG: 4269 
### TRANSFORM BIP SHAPES TO STATE BOUNDARIES CRS
bip_shape <- st_transform(bip_approved_join, crs=st_crs(VAgeom))
### IDENTIFY BIP SHAPES INSIDE STATE BOUNDARIES
test <- st_intersects(bip_shape, VAgeom)
bipVA <- bip_shape[lengths(test)>0,]
plot(VAgeom$geometry)
plot(bipVA$geometry,add=TRUE,col="red")
### TRANSFORM PROPERTIES TO SF 
deedtaxVA2 <- st_as_sf(deedtaxVA, coords = c("propertylevellongitude", "propertylevellatitude"))
### TRANSFORM PROPERTIES TO BIP SHAPES CRS
st_crs(deedtaxVA2) <- st_crs(bipVA)
### IDENTIFY PROPERTIES INSIDE BIP SHAPES
test <- st_within(deedtaxVA2, bipVA)
table(lengths(test))
#0        2      6
#585123    231     64
### IDENTIFY PROPERTIES INSIDE BIP SHAPES - VIRGINIA
deedtaxVA2$bip <- st_within(deedtaxVA2, bipVA) %>% lengths >0
table(deedtaxVA2$bip)
# FALSE - 585123
# TRUE - 295
### IDENTIFY PROPERTIES INSIDE BIP SHAPES - NATIONALLY
deedtaxall <- st_as_sf(deedtaxall, coords = c("propertylevellongitude", "propertylevellatitude"))
st_crs(deedtaxall) <- st_crs(bip_shape)
deedtaxall$bip <- st_within(deedtaxall, bip_shape) %>% lengths >0
table(deedtaxall$bip) 
# FALSE - 7,262,049 
# TRUE  - 77,387 

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


plot(VAgeom$geometry)
plot(test$geometry, add = TRUE, col = "red")
plot(bipVA$geometry, add = TRUE)

# Which ones are your properties?  DON'T NEED these: bip_va_houses <- deedtax_VA_penderbipvariable %>% filter(bip == TRUE)
head(deedtax_VA_penderbipvariable)
st_crs(deedtax_VA_penderbipvariable)

# What are your reference shapes? note - this repeats? 
### READ BIP SHAPES IN 
bip_approved_join <- readRDS("~/git/rural_broadband/data/working/BIP_working/BIP_Approved.RDS") # EPSG: 3857 
### READ STATE BOUNDARIES IN
VAgeom <- states()
VAgeom <- VAgeom[VAgeom$STATEFP=="51",]
VAgeom <- st_as_sf(VAgeom) # EPSG: 4269 
### TRANSFORM BIP SHAPES TO STATE BOUNDARIES CRS
VAgeom <- st_transform(VAgeom, crs=st_crs(bip_approved_join))
### IDENTIFY BIP SHAPES INSIDE STATE BOUNDARIES
test <- st_intersects(bip_approved_join, VAgeom)
bipVA <- bip_approved_join[lengths(test)>0,]
bipVA

virginia_bip_sites <- bipVA %>% filter(STUSPS == "VA")

#buffer_1mi <- st_buffer(virginia_bip_sites, dist = 1609.34, endCapStyle = "ROUND") #1 mile
buffer_5mi <- st_buffer(virginia_bip_sites, dist = 8046.72, endCapStyle = "ROUND") #5 miles
buffer_10mi <- st_buffer(virginia_bip_sites, dist = 16093.4, endCapStyle = "ROUND") #10 miles
buffer_25mi <- st_buffer(virginia_bip_sites, dist = 40233.6, endCapStyle = "ROUND") #25 miles
buffer_50mi <- st_buffer(virginia_bip_sites, dist = 80467.2, endCapStyle = "ROUND") #50 miles

plot(VAgeom$geometry)
#plot(buffer_1mi$geometry, add = TRUE)
plot(buffer_5mi$geometry, add = TRUE)
plot(buffer_10mi$geometry, add = TRUE)
plot(buffer_25mi$geometry, add = TRUE)
plot(buffer_50mi$geometry, add = TRUE)
plot(deedtax_VA_penderbipvariable$geometry, add = TRUE, col = "red")

st_crs(buffer_5mi)
st_crs(deedtax_VA_penderbipvariable)
head(deedtax_VA_penderbipvariable$geometry)
head(buffer_5mi$geometry)
deedtax_VA_penderbipvariable <- deedtax_VA_penderbipvariable %>% st_transform(st_crs(buffer_5mi))

head(deedtax_VA_penderbipvariable$bip)
test <- deedtax_VA_penderbipvariable %>% filter(bip == TRUE)
deedtax_VA_penderbipvariable$bip_5mi
deedtax_VA_penderbipvariable$bip_5mi <- st_within(deedtax_VA_penderbipvariable, buffer_5mi) %>% lengths >0
deedtax_VA_penderbipvariable$bip_10mi <- st_within(deedtax_VA_penderbipvariable, buffer_10mi) %>% lengths >0
deedtax_VA_penderbipvariable$bip_25mi <- st_within(deedtax_VA_penderbipvariable, buffer_25mi) %>% lengths >0
deedtax_VA_penderbipvariable$bip_50mi <- st_within(deedtax_VA_penderbipvariable, buffer_50mi) %>% lengths >0


table(deedtax_VA_penderbipvariable$bip_5mi)
table(deedtax_VA_penderbipvariable$bip_10mi)
table(deedtax_VA_penderbipvariable$bip_25mi)
table(deedtax_VA_penderbipvariable$bip_50mi)

#saveRDS(deedtax_VA_penderbipvariable, "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxmergeall_va_bipvariables.RDS")

deedtax_VA_penderbipvariable <- deedtax_VA_penderbipvariable %>% 
  mutate(bip_c = paste(stringr::str_extract(bip, "^.{1}"),
                       stringr::str_extract(bip_5mi, "^.{1}"),
                       stringr::str_extract(bip_10mi, "^.{1}"),
                       stringr::str_extract(bip_25mi, "^.{1}"),
                       stringr::str_extract(bip_50mi, "^.{1}")),
         bip_n = stringr::str_count(bip_c, "T"), 
         bip_distance = recode(bip_n, `1` = "50 miles", `2` = "25 miles", `3` = "10 miles", `4` = "5 miles", `5` = "BIP region", `0` = "Non-BIP region"))

va_near_bip_homes <- deedtax_VA_penderbipvariable %>% filter(bip_distance !="Non-BIP region") 

plot(VAgeom$geometry)
plot(buffer_5mi$geometry, add = TRUE)
plot(buffer_10mi$geometry, add = TRUE)
plot(buffer_25mi$geometry, add = TRUE)
plot(buffer_50mi$geometry, add = TRUE)
plot(va_near_bip_homes$geometry, add = TRUE, col = "red")
st_geometry(deedtax_VA_penderbipvariable_nogeom) <- NULL
haven::write_dta(deedtax_VA_penderbipvariable_nogeom, "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deed_tax_merge_va_bipvariables.dta")



deedtax_VA_penderbipvariable <- readRDS("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxmergeall_va_bipvariables.RDS")

va_bip_homes <- deedtax_VA_penderbipvariable %>% filter(bip_distance == "BIP region") 
va_5mi_homes <- deedtax_VA_penderbipvariable %>% filter(bip_distance == "5 miles") 
va_10mi_homes <- deedtax_VA_penderbipvariable %>% filter(bip_distance == "10 miles") 
va_25mi_homes <- deedtax_VA_penderbipvariable %>% filter(bip_distance == "25 miles") 
va_50mi_homes <- deedtax_VA_penderbipvariable %>% filter(bip_distance == "50 mile") 
va_notbip_homes <- deedtax_VA_penderbipvariable %>% filter(bip_distance == "Non-BIP region") 
table(deedtax_VA_penderbipvariable$bip_distance)


va_bip_homes %>% 
  select(apnparcelnumberunformatted, #formattedapn, #property identifier
         #transactiontype, pricatcode, #transaction identifier? 
         city, state, # censustract, zipcode, # geography
         year, taxyear, assessedyear, # time
         metro2013, #rurality
         saleamount, assdtotalvalue, mkttotalvalue, # totalvaluecalculated, totalvaluecalculatedind, # property values
         acres, landsquarefootage, buildingsquarefeet, livingsquarefeet, grosssquarefeet, # universalbuildingsquarefeet, # area
         yearbuilt, age,
         bedrooms,
         totalbathscalculated, 
         bip_distance) %>% summary()
