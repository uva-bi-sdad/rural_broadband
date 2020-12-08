library(dplyr)
library(sf)

## LOAD DATA

# FIPS STATE CODES (FOR EASE)
fips_  <- tigris::fips_codes  %>% select(state, state_code) %>% distinct()
# USDA - NONRURALITY FILE
shps_e <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RECONNECT_NON_RURAL_working/final_nonruralareas_USDAfile_translatedblocks_df_NOV182020.RDS")
# CENSUS - ALL USA BLOCKS
shapes_e <- readRDS("/project/biocomplexity/sdad/projects_data/usda/bb/original/censusblocks/blocks_TIGER2018_sf_RDS_USA_SF_wmissing.RDS")
# FCC - SPEED FILE (NOT ALL US BLOCKS REPRESENTED)
fcc_2018_block_max <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/fcc_availability/fcc_2018_block_maxspdelig.RDS")


### 1) JOIN ALL USA BLOCKS TO FCC SPEED DATA

fcc_2018_block_max$BlockCode <- as.character(fcc_2018_block_max$BlockCode)
fcc_2018_block_max <- fcc_2018_block_max %>% mutate(BlockCode2 = ifelse(nchar(BlockCode) == 14, paste0("0", BlockCode), BlockCode))

shapes_e_fcc <- shapes_e %>% 
  mutate(FULL_GEOID = paste0(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10)) %>% 
  left_join(fcc_2018_block_max, by = c("FULL_GEOID" = "BlockCode2")) %>% 
  left_join(fips_, by = c("STATEFP10" = "state_code"))


### 2) JOIN ALL USA BLOCKS TO USDA NONRURALITY (BLOCK TRANSLATION) FILE 
blocks_rurality_TF <- shps_e %>% as.data.frame() %>% select(GEOID10, nonruralTF)
usa_blocks_speed_rural <- shapes_e_fcc %>% left_join(blocks_rurality_TF, by = c("GEOID10"))


### 3) INTERSECT WITH PROJECTS 
#### PROT BORR
protborr <- sf::st_read("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/rus_broadband_servicearea/ProtectedBorrowers04222019.shp")
protborr <- protborr %>% st_transform(st_crs(usa_blocks_speed_rural))
blocks_criteriaXprotborr_VECTOR <- st_intersects(usa_blocks_speed_rural, protborr) 
usa_blocks_speed_rural$protborr_inelg_vec <- blocks_criteriaXprotborr_VECTOR %>% lengths >0 

#### CAF II
caf2auctionwinners <- st_read("/project/biocomplexity/sdad/projects_data/usda/bb/original/Protected_bb_borrower_service_areas/Auction903_April2019.shp")
caf2auctionwinners <- caf2auctionwinners %>% st_transform(st_crs(usa_blocks_speed_rural))
blocks_criteriaXcaf2winners_VECTOR <- st_intersects(usa_blocks_speed_rural, caf2auctionwinners) 
usa_blocks_speed_rural$caf2_inelg_vec <- blocks_criteriaXcaf2winners_VECTOR %>% lengths >0 

# save out just in case
# saveRDS(usa_blocks_speed_rural, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2018_draft_eligibility_sf_18NOV2020v1.RDS")

usa_blocks_speed_rural_projects_final_2018 <- usa_blocks_speed_rural %>% 
  mutate(disqual1 = ifelse(speed_inelig != 1|is.na(speed_inelig), "eligible", "speed")) %>%
  mutate(disqual2 = ifelse(nonruralTF != 1|is.na(nonruralTF), "eligible", "nonrural")) %>%
  mutate(disqual3 = ifelse(protborr_inelg_vec == TRUE|caf2_inelg_vec == TRUE, "projects", "eligible")) %>%
  mutate(eligibility_final = ifelse(stringr::str_count(paste(disqual1, disqual2, disqual3), pattern = "eligible") == 3, "eligible", "ineligible" ))



# saveRDS(usa_blocks_speed_rural_projects_final_2018, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2018_final_eligibility_sf_18NOV2020v2.RDS")
# usa_blocks_speed_rural_projects_final_2018 %>% as.data.frame() %>% count(disqual1, disqual2, disqual3, eligibility_final)
# usa_blocks_speed_rural_projects_final_2018 %>% as.data.frame() %>% count(eligibility_final)




##################


# FCC - SPEED FILE (NOT ALL US BLOCKS REPRESENTED)
fcc_2014_block_max_ <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/fcc_availability/fcc_2014_block_maxspdelig_threshold_total3.RDS")

### 1) JOIN ALL USA BLOCKS TO FCC SPEED DATA

fcc_2014_block_max$BlockCode <- as.character(fcc_2014_block_max$BlockCode)
fcc_2014_block_max <- fcc_2014_block_max %>% mutate(BlockCode2 = ifelse(nchar(BlockCode) == 14, paste0("0", BlockCode), BlockCode))

shapes_e_fcc <- shapes_e %>% 
  mutate(FULL_GEOID = paste0(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10)) %>% 
  left_join(fcc_2014_block_max, by = c("FULL_GEOID" = "BlockCode2")) %>% 
  left_join(fips_, by = c("STATEFP10" = "state_code"))


### 2) JOIN ALL USA BLOCKS TO USDA NONRURALITY (BLOCK TRANSLATION) FILE 
blocks_rurality_TF <- shps_e %>% as.data.frame() %>% select(GEOID10, nonruralTF)
usa_blocks_speed_rural <- shapes_e_fcc %>% left_join(blocks_rurality_TF, by = c("GEOID10"))

### 3) INTERSECT WITH PROJECTS 
#### PROT BORR
protborr <- sf::st_read("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/rus_broadband_servicearea/ProtectedBorrowers04222019.shp")
protborr <- protborr %>% st_transform(st_crs(usa_blocks_speed_rural))
blocks_criteriaXprotborr_VECTOR <- st_intersects(usa_blocks_speed_rural, protborr) 
usa_blocks_speed_rural$protborr_inelg_vec <- blocks_criteriaXprotborr_VECTOR %>% lengths >0 

#### CAF II
caf2auctionwinners <- st_read("/project/biocomplexity/sdad/projects_data/usda/bb/original/Protected_bb_borrower_service_areas/Auction903_April2019.shp")
caf2auctionwinners <- caf2auctionwinners %>% st_transform(st_crs(usa_blocks_speed_rural))
blocks_criteriaXcaf2winners_VECTOR <- st_intersects(usa_blocks_speed_rural, caf2auctionwinners) 
usa_blocks_speed_rural$caf2_inelg_vec <- blocks_criteriaXcaf2winners_VECTOR %>% lengths >0 

# save out just in case
# saveRDS(usa_blocks_speed_rural, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2018_draft_eligibility_sf_18NOV2020v1.RDS")

usa_blocks_speed_rural_projects_final_2014 <- usa_blocks_speed_rural %>% 
  mutate(disqual1 = ifelse(speed_inelig != 1|is.na(speed_inelig), "eligible", "speed")) %>%
  mutate(disqual2 = ifelse(nonruralTF != 1|is.na(nonruralTF), "eligible", "nonrural")) %>%
  mutate(disqual3 = ifelse(protborr_inelg_vec == TRUE|caf2_inelg_vec == TRUE, "projects", "eligible")) %>%
  mutate(eligibility_final = ifelse(stringr::str_count(paste(disqual1, disqual2, disqual3), pattern = "eligible") == 3, "eligible", "ineligible" ))



# saveRDS(usa_blocks_speed_rural_projects_final_2014, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2014_final_eligibility_sf_18NOV2020v2.RDS")
# usa_blocks_speed_rural_projects_final_2014 %>% as.data.frame() %>% count(disqual1, disqual2, disqual3, eligibility_final)
# usa_blocks_speed_rural_projects_final_2014 %>% as.data.frame() %>% count(eligibility_final)



########################

usa_blocks_speed_rural_projects_final_2014 <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2014_final_eligibility_sf_18NOV2020v2.RDS")
usa_blocks_speed_rural_projects_final_2018 <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2018_final_eligibility_sf_18NOV2020v2.RDS")

# BIP, TC does not have a date
# CC has a date - 2013-2020
# RC has a date - 2019-2020
bip_areas <- sf::st_read("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_BIP_Shapefiles_April2020/200409_BIP_ServAr_ID.shp") %>% sf::st_transform(sf::st_crs(usa_blocks_speed_rural_projects_final_2018))
cc_areas <- sf::st_read("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_CC_Shapefiles_April2020/CC 2013_2019_83 04272020.shp") %>% sf::st_transform(sf::st_crs(usa_blocks_speed_rural_projects_final_2018))
cc_areas_2014andearlier <- cc_areas %>% filter(OBLFY <= 2014)
cc_areas_2018andearlier <- cc_areas %>% filter(OBLFY <= 2018)
rc_areas <- sf::st_read("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_RC_Shapefiles_April2020/ReConnect R1 594 PFSA 05012020.shp") %>% sf::st_transform(sf::st_crs(usa_blocks_speed_rural_projects_final_2018))
rc_areas_2014andearlier <- rc_areas %>% filter(as.numeric(Obligation) <= 2014)
rc_areas_2018andearlier <- rc_areas %>% filter((Obligation) <= 2018)
tc_fb_areas <- sf::st_read("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_TC_Shapefiles_June2020/USDARD_RUS_TELCO_FARMBILL_06042020/USDARD_RUS_TELCO_FARMBILL_06042020.shp") %>% sf::st_transform(sf::st_crs(usa_blocks_speed_rural_projects_final_2018))
tc_inf_areas <- sf::st_read("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_TC_Shapefiles_June2020/USDARD_RUS_TELCO_INFRA_06042020/USDARD_RUS_TELCO_INFRA_06042020.shp") %>% sf::st_transform(sf::st_crs(usa_blocks_speed_rural_projects_final_2018))


eligblocks_bip_intersect14 <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2014, bip_areas)
eligblocks_cc14_intersect <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2014, cc_areas_2014andearlier)
eligblocks_rc14_intersect <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2014, rc_areas_2014andearlier)
eligblocks_tc_fb_intersect14 <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2014, tc_fb_areas)
eligblocks_tc_inf_intersect14 <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2014, tc_inf_areas)

eligblocks_bip_intersect18 <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2018, bip_areas)
eligblocks_cc18_intersect <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2018, cc_areas_2018andearlier)
eligblocks_rc18_intersect <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2018, rc_areas_2018andearlier)
eligblocks_tc_fb_intersect18 <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2018, tc_fb_areas)
eligblocks_tc_inf_intersect18 <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2018, tc_inf_areas)


usa_blocks_speed_rural_projects_final_2014$BIP_PROJECTID <- bip_areas$ProjectID[ sapply(eligblocks_bip_intersect, first) ]
usa_blocks_speed_rural_projects_final_2014$CC_RUSID <- cc_areas_2014andearlier$RUSID[ sapply(eligblocks_cc14_intersect, first) ]
usa_blocks_speed_rural_projects_final_2014$RC_RUSID <- rc_areas_2014andearlier$RUS_ID[ sapply(eligblocks_rc14_intersect, first) ]
usa_blocks_speed_rural_projects_final_2014$TC_FB_RUSID <- tc_fb_areas$RUS_ID[ sapply(eligblocks_tc_fb_intersect14, first) ]
usa_blocks_speed_rural_projects_final_2014$TC_INF_RUSID <- tc_inf_areas$RUSID[ sapply(eligblocks_tc_inf_intersect14, first) ]

usa_blocks_speed_rural_projects_final_2018$BIP_PROJECTID <- bip_areas$ProjectID[ sapply(eligblocks_bip_intersect, first) ]
usa_blocks_speed_rural_projects_final_2018$CC_RUSID <- cc_areas_2018andearlier$RUSID[ sapply(eligblocks_cc18_intersect, first) ]
usa_blocks_speed_rural_projects_final_2018$RC_RUSID <- rc_areas_2018andearlier$RUS_ID[ sapply(eligblocks_rc18_intersect, first) ]
usa_blocks_speed_rural_projects_final_2018$TC_FB_RUSID <- tc_fb_areas$RUS_ID[ sapply(eligblocks_tc_fb_intersect18, first) ]
usa_blocks_speed_rural_projects_final_2018$TC_INF_RUSID <- tc_inf_areas$RUSID[ sapply(eligblocks_tc_inf_intersect18, first) ]

# saveRDS(usa_blocks_speed_rural_projects_final_2018, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2018_final_eligibility_sf_24NOV2020v3.RDS")
# table(is.na(usa_blocks_speed_rural_projects_final_2014$BIP_PROJECTID))
# table(is.na(usa_blocks_speed_rural_projects_final_2014$CC_RUSID))
# table(is.na(usa_blocks_speed_rural_projects_final_2014$RC_RUSID))
# table(is.na(usa_blocks_speed_rural_projects_final_2014$TC_FB_RUSID))
# table(is.na(usa_blocks_speed_rural_projects_final_2014$TC_INF_RUSID))
rm(eligblocks_rc14_intersect)


