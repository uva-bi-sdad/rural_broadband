library(tigris)
library(sf)
deedtax_IN_penderbipvariables <- readRDS("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxmergeall_in_bipvariables.RDS")
colnames(deedtax_IN_penderbipvariables)[stringr::str_detect(colnames(deedtax_IN_penderbipvariables), "fips") == TRUE]
head(deedtax_IN_penderbipvariables$fipscode)
lakecty <- block_groups(18, county = "Lake", cb = TRUE, year = 2010) %>% st_as_sf()
portercty <- block_groups(18, county = "Porter", cb = TRUE, year = 2010) %>% st_as_sf()
newtoncty <- block_groups(18, county = "Newton", cb = TRUE, year = 2010) %>% st_as_sf()

plot(lakecty)
plot(portercty, add = TRUE)
plot(newtoncty, add = TRUE)


indianaNW <- rbind(lakecty, portercty, newtoncty)
plot(indianaNW[3])


indiana_cty_bg <- block_groups(18, cb = TRUE, year = 2010)
indiana_cty_bg <- indiana_cty_bg %>% st_as_sf()
indiana_cty_bg2010 <- indiana_cty_bg

deedtax_IN_penderbipvariables <- deedtax_IN_penderbipvariables %>% st_transform(st_crs(indiana_cty_bg))

deedtax_IN_bipvarsandcensus <- deedtax_IN_penderbipvariables %>% st_join(indiana_cty_bg) 
deedtax_IN_bipvarsandcensus_nogeom <- deedtax_IN_bipvarsandcensus
st_geometry(deedtax_IN_bipvarsandcensus_nogeom) <- NULL
haven::write_dta(deedtax_IN_bipvarsandcensus_nogeom, "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtax_IN_bipvarsandcensus.dta")
saveRDS(deedtax_IN_bipvarsandcensus, "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtax_IN_bipvarsandcensus.RDS")

#View(head(deedtax_IN_bipvarsandcensus %>% select(apnparcelnumberunformatted, censustract, county_name, city, state, GEO_ID, STATE, COUNTY, TRACT, BLKGRP, NAME, LSAD, CENSUSAREA, COUNTYFP, STATEFP))

check2000s <- deedtax_IN_penderbipvariables %>% 
   filter(assessedyear < 2010)
  #select(apnparcelnumberunformatted, censustract, county_name, city, state, GEO_ID, STATE, COUNTY, TRACT, BLKGRP, NAME, LSAD, CENSUSAREA, COUNTYFP, STATEFP) %>% 
  
check2010s <- deedtax_IN_penderbipvariables %>% 
  filter(assessedyear > 2009)

indiana_cty_bg2000 <- block_groups(18, cb = TRUE, year = 2000) %>% st_as_sf()

nrow(check2000s) # 46,167
nrow(check2010s)# 353,656

check2010s <- check2010s %>% st_join(indiana_cty_bg2010) 
check2000s <- check2000s %>% st_join(indiana_cty_bg2000)

bg2000scheck <- check2000s %>%
  select(censustract, STATE, COUNTY, TRACT, BLKGROUP, NAME) %>% 
  mutate(fakegeoid = paste0(STATE, COUNTY, TRACT, BLKGROUP),
         check_CLinGID = ifelse(stringr::str_detect(fakegeoid, pattern = censustract) ==TRUE, 1, 0),
         checkGIDinCL_tract = ifelse(stringr::str_detect(censustract, pattern = TRACT) == TRUE, 1, 0),
         checkGIDinCL_bkgp = ifelse(stringr::str_detect(censustract, pattern = paste0(TRACT, BLKGROUP)) == TRUE, 1, 0))

bg2000scheck
table(bg2000scheck$check_CLinGID) # 800x where CL is inside GeoID but 45K where CL is NOT inside GeoID
table(bg2000scheck$checkGIDinCL_tract) # 44K where tract is inside CL but 2K where tract is not inside CL
table(bg2000scheck$checkGIDinCL_bkgp) # 42K where block group is inside CL but 3.5K where block group is not inside CL
# this tells me that corelogic isn't reliable...? but tract is at least closer than block group

tibble::tibble("IN/OUT" = c("IN", "OUT", "Percent"), 
               "2000_CLinGID" = c(757, 45061, 757/46167),
               "2000_TRACTinCL" = c(44203, 1920, 44203/46167),
               "2000_BGPinCL" = c(42589, 3578, 42589/46167), 
               "2010_CLinGID" = c(3910, 349192, 3910/353656),
               "2010_TRACTinCL" = c(344123, 9413, 344123/353656),
               "2010_BGPinCL" = c(338602, 15054, 338602/353656))

#deedtax_IN_bipvarsandcensus_check <- rbind

bg2010scheck <- check2010s %>%
  select(censustract, GEO_ID, STATE, COUNTY, TRACT, BLKGRP, NAME) %>% 
  mutate(check_CLinGID = ifelse(stringr::str_detect(GEO_ID, pattern = censustract) ==TRUE, 1, 0),
         checkGIDinCL_tract = ifelse(stringr::str_detect(censustract, pattern = TRACT) == TRUE, 1, 0),
         checkGIDinCL_bkgp = ifelse(stringr::str_detect(censustract, pattern = paste0(TRACT, BLKGRP)) == TRUE, 1, 0))

bg2010scheck
table(bg2010scheck$check_CLinGID) # 4K where CL is inside GeoID but 350K where CL is NOT inside GeoID
table(bg2010scheck$checkGIDinCL_tract) # 344K where tract is inside CL but 9K where tract is not inside CL
table(bg2010scheck$checkGIDinCL_bkgp) # 338K where block group is inside CL but 15K where block group is not inside CL
# this tells me that corelogic isn't reliable...? but tract is at least closer than block group

# indianaNWblocks <- tigris::blocks(18, county = c("Lake", "Porter", "Newton"), year = 2010)
# indianaNWblocks <- indianaNWblocks %>% st_as_sf()
# 
# testset <- deedtax_IN_bipvarsandcensus %>% filter(county_name %in% c("Lake County", "Porter County", "Newton County")) 
# testset <- testset %>% st_transform(st_crs(indianaNWblocks))
# thing <- st_join(testset, indianaNWblocks)
# 
# blockcheck <- thing %>% transmute(
#   CLcensustractorig = censustract, 
#   SDADcensustract = TRACTCE10,
#   CLcensustractcut = stringr::str_extract(censustract, "^.{0,6}"),              
#   CLblockcheckstring = paste0(TRACTCE10, BLOCKCE10),
#   CLtractcheck = ifelse(CLcensustractcut == SDADcensustract, 1, 0),
#   CLblockcheck = ifelse(censustract == CLblockcheckstring, 1, 0))
# 
# table(blockcheck$CLtractcheck)
# table(blockcheck$CLblockcheck)
# 
# 
# testset2 <- testset %>% filter(taxyear < 2010)
# indianaNWblocks2 <- tigris::blocks(18, county = c("Lake", "Porter", "Newton"), year = 2000)
# indianaNWblocks2 <- indianaNWblocks2 %>% st_as_sf()
# 
# thing2 <- st_join(testset2, indianaNWblocks2)
# 
# blockcheck2 <- thing2 %>% transmute(
#   CLcensustractorig = censustract, 
#   SDADcensustract = TRACTCE00,
#   SDADblock = BLOCKCE00,
#   CLcensustractcut = stringr::str_extract(censustract, "^.{0,6}"),              
#   CLblockcheckstring = paste0(TRACTCE00, BLOCKCE00),
#   CLtractcheck = ifelse(CLcensustractcut == SDADcensustract, 1, 0),
#   CLblockcheck = ifelse(censustract == CLblockcheckstring, 1, 0), 
#   onemorecheck = ifelse(stringr::str_detect(CLcensustractorig, patt = SDADcensustract) == TRUE, 1, 0),
#   onenothercheck = ifelse(stringr::str_detect(CLcensustractorig, patt = SDADblock)==TRUE, 1, 0))
# 
# table(blockcheck2$CLtractcheck)
# table(blockcheck2$CLblockcheck)
# table(blockcheck2$onemorecheck)
# table(blockcheck2$onenothercheck)
nrow(deedtax_IN_penderbipvariables) #380510
length(unique(deedtax_IN_penderbipvariables$id)) #378903
length(unique(deedtax_IN_penderbipvariables$apnparcelnumberunformatted)) #323857
colnames(deedtax_IN_penderbipvariables)[stringr::str_detect(colnames(deedtax_IN_penderbipvariables), "year") == TRUE]

timecombosindiana <- deedtax_IN_penderbipvariables %>% 
  group_by(taxyear, assessedyear, year) %>% 
  summarise(n = n()) #%>% group_by(n) %>% summarise(n = n())

timecombosindiana %>% 
  transmute(assessedyear = stringr::str_extract(assessedyear, "^.{0,4}"),
            year = year, n = n) %>% 
  group_by(assessedyear, year) %>%
  summarise(n = sum(n)) %>% 
  arrange(assessedyear, year)
st_geometry(timecombosindiana)  <- NULL
timecombosindiana <- as.data.frame(timecombosindiana)

########
deedtax_IN_bipvarsandcensus <- readRDS("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtax_IN_bipvarsandcensus.RDS")
deedtax_IN3CT_bipvars_10bgps <- deedtax_IN_bipvarsandcensus %>% filter(COUNTY %in% c("089", "127", "111"))
rm(deedtax_IN_bipvarsandcensus)

lakecty <- tigris::blocks(18, county = "Lake", year = 2000) %>% st_as_sf()
portercty <- tigris::blocks(18, county = "Porter", year = 2000) %>% st_as_sf()
newtoncty <- tigris::blocks(18, county = "Newton", year = 2000) %>% st_as_sf()

indianaNW_2000blocks <- rbind(lakecty, portercty, newtoncty)

deedtax_IN3CT_bipvars_10bgps_00blks <- deedtax_IN3CT_bipvars_10bgps %>% st_join(indianaNW_2000blocks)
head(deedtax_IN3CT_bipvars_10bgps_00blks, 1)

fcc2010_providers_by_block <- readr::read_csv("git/rural_broadband/data/fcc2010_providers_by_block.csv")
colnames(fcc2010_providers_by_block)
head(fcc2010_providers_by_block, 1)

deedtax_IN3CT_bipvars_10bgps_00blks_fcc <- deedtax_IN3CT_bipvars_10bgps_00blks  %>% left_join(fcc2010_providers_by_block, by = c("BLKIDFP00" = "GEOID00"))

table(deedtax_IN3CT_bipvars_10bgps_00blks_fcc$providers_768kbps)
sum(dataplumbr::var.is_blank(deedtax_IN3CT_bipvars_10bgps_00blks_fcc$providers_768kbps))
table(deedtax_IN3CT_bipvars_10bgps_00blks_fcc$providers_3mpbs)
sum(dataplumbr::var.is_blank(deedtax_IN3CT_bipvars_10bgps_00blks_fcc$providers_3mpbs))
table(deedtax_IN3CT_bipvars_10bgps_00blks_fcc$providers_10mpbs)
sum(dataplumbr::var.is_blank(deedtax_IN3CT_bipvars_10bgps_00blks_fcc$providers_10mpbs))
table(deedtax_IN3CT_bipvars_10bgps_00blks_fcc$providers_25mpbs)
sum(dataplumbr::var.is_blank(deedtax_IN3CT_bipvars_10bgps_00blks_fcc$providers_25mpbs))

deedtax_IN3CT_bipvars_10bgps_00blks_fcc_nogeom <- deedtax_IN3CT_bipvars_10bgps_00blks_fcc
st_geometry(deedtax_IN3CT_bipvars_10bgps_00blks_fcc_nogeom) <- NULL
colnames(deedtax_IN3CT_bipvars_10bgps_00blks_fcc_nogeom)[colnames(deedtax_IN3CT_bipvars_10bgps_00blks_fcc_nogeom)=="COUNTYFP.x"] <- "COUNTYFP10"
colnames(deedtax_IN3CT_bipvars_10bgps_00blks_fcc_nogeom)[colnames(deedtax_IN3CT_bipvars_10bgps_00blks_fcc_nogeom)=="STATEFP.x"] <- "STATEFP10"
colnames(deedtax_IN3CT_bipvars_10bgps_00blks_fcc_nogeom)[colnames(deedtax_IN3CT_bipvars_10bgps_00blks_fcc_nogeom)=="COUNTYFP.y"] <- "COUNTYFP00"
colnames(deedtax_IN3CT_bipvars_10bgps_00blks_fcc_nogeom)[colnames(deedtax_IN3CT_bipvars_10bgps_00blks_fcc_nogeom)=="STATEFP.y"] <- "STATEFP00"
colnames(deedtax_IN3CT_bipvars_10bgps_00blks_fcc_nogeom)
haven::write_dta(deedtax_IN3CT_bipvars_10bgps_00blks_fcc_nogeom, "~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtax_IN3CT_bipvarsandcensusandfcc.dta")


