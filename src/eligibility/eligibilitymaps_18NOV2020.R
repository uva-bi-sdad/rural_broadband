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
fcc_2014_block_max <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/fcc_availability/fcc_2014_block_maxspdelig_threshold_total3.RDS")

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



# saveRDS(usa_blocks_speed_rural_projects_final_2014, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2014_final_eligibility_sf_08DEC2020v3.RDS")
# usa_blocks_speed_rural_projects_final_2014 %>% as.data.frame() %>% count(disqual1, disqual2, disqual3, eligibility_final)
# usa_blocks_speed_rural_projects_final_2014 %>% as.data.frame() %>% count(eligibility_final)



########################

usa_blocks_speed_rural_projects_final_2014 <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2014_final_eligibility_sf_08DEC2020v3.RDS")
usa_blocks_speed_rural_projects_final_2018 <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2018_final_eligibility_sf_18NOV2020v2.RDS")

# BIP, TC does not have a date
# CC has a date - 2013-2020
# RC has a date - 2019-2020
# bip_areas <- sf::st_read("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_BIP_Shapefiles_April2020/200409_BIP_ServAr_ID.shp") %>% sf::st_transform(sf::st_crs(usa_blocks_speed_rural_projects_final_2014))
cc_areas <- sf::st_read("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_CC_Shapefiles_April2020/CC 2013_2019_83 04272020.shp") %>% sf::st_transform(sf::st_crs(usa_blocks_speed_rural_projects_final_2014))
cc_areas_2014andearlier <- cc_areas %>% filter(OBLFY <= 2014)
cc_areas_2018andearlier <- cc_areas %>% filter(OBLFY <= 2018 & OBLFY > 2014)
# tc_fb_areas <- sf::st_read("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_TC_Shapefiles_June2020/USDARD_RUS_TELCO_FARMBILL_06042020/USDARD_RUS_TELCO_FARMBILL_06042020.shp") %>% sf::st_transform(sf::st_crs(usa_blocks_speed_rural_projects_final_2014))
# tc_inf_areas <- sf::st_read("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_TC_Shapefiles_June2020/USDARD_RUS_TELCO_INFRA_06042020/USDARD_RUS_TELCO_INFRA_06042020.shp") %>% sf::st_transform(sf::st_crs(usa_blocks_speed_rural_projects_final_2014))

## CC 2014 - Block at CC Project Boundaries
eligblocks_cc14_crossbound <- sf::st_crosses(usa_blocks_speed_rural_projects_final_2014, st_boundary(cc_areas_2014andearlier))
usa_blocks_speed_rural_projects_final_2014$CC_14_RUSID_boundary <- cc_areas_2014andearlier$RUSID[ sapply(eligblocks_cc14_crossbound, first) ]
## CC 2014 - Block within CC Project Boundaries
eligblocks_cc14_within <- sf::st_within(usa_blocks_speed_rural_projects_final_2014,cc_areas_2014andearlier)
usa_blocks_speed_rural_projects_final_2014$CC_14_RUSID_inside <- cc_areas_2014andearlier$RUSID[ sapply(eligblocks_cc14_within, first) ]
## CC 2014 - Block intersects CC Project 
eligblocks_cc14_intersect <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2014, cc_areas_2014andearlier)
usa_blocks_speed_rural_projects_final_2014$CC_14_RUSID_intersect <- cc_areas_2014andearlier$RUSID[ sapply(eligblocks_cc14_intersect, first) ]

# saveRDS(usa_blocks_speed_rural_projects_final_2014, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2014_final_eligibility_sf_14DEC2020v5.RDS")
usa_blocks_speed_rural_projects_final_2014 <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2014_final_eligibility_sf_14DEC2020v5.RDS")

##########


## CC 2018 - Block at CC Project Boundaries
eligblocks_cc18_crossbound <- sf::st_crosses(usa_blocks_speed_rural_projects_final_2018, st_boundary(cc_areas_2018andearlier))
usa_blocks_speed_rural_projects_final_2018$CC_18_RUSID_boundary <- cc_areas_2018andearlier$RUSID[ sapply(eligblocks_cc18_crossbound, first) ]
## CC 2018 - Block within CC Project Boundaries
eligblocks_cc18_within <- sf::st_within(usa_blocks_speed_rural_projects_final_2018,cc_areas_2018andearlier)
usa_blocks_speed_rural_projects_final_2018$CC_18_RUSID_inside <- cc_areas_2018andearlier$RUSID[ sapply(eligblocks_cc18_within, first) ]
## CC 2018 - Block intersects CC Project 
eligblocks_cc18_intersect <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2018, cc_areas_2018andearlier)
usa_blocks_speed_rural_projects_final_2018$CC_18_RUSID_intersect <- cc_areas_2018andearlier$RUSID[ sapply(eligblocks_cc18_intersect, first) ]

# saveRDS(usa_blocks_speed_rural_projects_final_2018, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2018_final_eligibility_sf_14DEC2020v5.RDS")
usa_blocks_speed_rural_projects_final_2014 <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2014_final_eligibility_sf_14DEC2020v5.RDS")
usa_blocks_speed_rural_projects_final_2018 <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2018_final_eligibility_sf_14DEC2020v5.RDS")

rc_areas <- sf::st_read("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_RC_Shapefiles_April2020/ReConnect R1 594 PFSA 05012020.shp") %>% sf::st_transform(sf::st_crs(usa_blocks_speed_rural_projects_final_2018))

## RC 2018 - Block at RC Project Boundaries
eligblocks_rc18_crossbound <- sf::st_crosses(usa_blocks_speed_rural_projects_final_2018, st_boundary(rc_areas))
usa_blocks_speed_rural_projects_final_2018$RC_18_RUSID_boundary <- rc_areas$RUS_ID[ sapply(eligblocks_rc18_crossbound, first) ]
# saveRDS(usa_blocks_speed_rural_projects_final_2018, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/eligibility_summary_percentages/intermediates_delete/")
## RC 2018 - Block within RC Project Boundaries
eligblocks_rc18_within <- sf::st_within(usa_blocks_speed_rural_projects_final_2018,rc_areas)
usa_blocks_speed_rural_projects_final_2018$RC_18_RUSID_inside <- rc_areas$RUS_ID[ sapply(eligblocks_rc18_within, first) ]
## RC 2018 - Block intersects RC Project 
eligblocks_rc18_intersect <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2018, rc_areas)
usa_blocks_speed_rural_projects_final_2018$RC_18_RUSID_intersect <- rc_areas$RUS_ID[ sapply(eligblocks_rc18_intersect, first) ]

# saveRDS(usa_blocks_speed_rural_projects_final_2018, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/eligibility_summary_percentages/intermediates_delete/usa_blocks_speed_rural_projects_final_2018WRC_sf.RDS")

join_18_rc <- usa_blocks_speed_rural_projects_final_2018 %>% 
  as.data.frame() %>% 
  select(GEOID10, BlockCode, RC_18_RUSID_boundary, RC_18_RUSID_inside, RC_18_RUSID_intersect)

####


join14 <- usa_blocks_speed_rural_projects_final_2014 %>% 
  as.data.frame() %>%
  transmute(FULL_GEOID, Speed14 = disqual1, Nonrural14 = disqual2, Projects14 = disqual3, CC_14_RUSID_boundary, CC_14_RUSID_inside, CC_14_RUSID_intersect)
# select(FULL_GEOID, state, disqual1, disqual2, disqual3, CC_14_RUSID_boundary, CC_14_RUSID_inside, CC_14_RUSID_intersect) %>% 
# `colnames<-`(c("FULL_GEOID", "State", "Speed14", "Nonrural14", "Projects14", "CC_14_RUSID_boundary", "CC_14_RUSID_inside", "CC_14_RUSID_intersect", "geometry"))

join18  <- usa_blocks_speed_rural_projects_final_2018 %>% 
  as.data.frame() %>%
  transmute(FULL_GEOID, Speed18 = disqual1, Nonrural18 = disqual2, Projects18 = disqual3, CC_18_RUSID_boundary, CC_18_RUSID_inside, CC_18_RUSID_intersect)


rm(usa_blocks_speed_rural_projects_final_2014, usa_blocks_speed_rural_projects_final_2018)

final_eligibility_df <- join14 %>% left_join(join18, by = "FULL_GEOID")

# saveRDS(final_eligibility_df, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2014_18_final_eligibility_df_14DEC2020.RDS")
final_eligibility_df <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2014_18_final_eligibility_df_14DEC2020.RDS")

final_eligibility_df_ <- final_eligibility_df %>% 
  mutate(cc_elig_14 = ifelse(Speed14 == "eligible" & Nonrural14 == "eligible", "eligible", "ineligible"), 
         cc_elig_18 = ifelse(Speed18 == "eligible" & Nonrural18 == "eligible", "eligible", "ineligible"), 
         areas_14_ANY = ifelse(is.na(CC_14_RUSID_intersect), paste0(cc_elig_14, ", no funds"), paste0(cc_elig_14, ", funded")), 
         areas_18_ANY = ifelse(is.na(CC_18_RUSID_intersect), paste0(cc_elig_18, ", no funds"), paste0(cc_elig_18, ", funded")),
         areas_14_IN = ifelse(is.na(CC_14_RUSID_inside), paste0(cc_elig_14, ", no funds"), paste0(cc_elig_14, ", funded")), 
         areas_18_IN = ifelse(is.na(CC_18_RUSID_inside), paste0(cc_elig_18, ", no funds"), paste0(cc_elig_18, ", funded")),
         areas_14_BOUND = ifelse(is.na(CC_14_RUSID_boundary), paste0(cc_elig_14, ", no funds"), paste0(cc_elig_14, ", funded")), 
         areas_18_BOUND = ifelse(is.na(CC_18_RUSID_boundary), paste0(cc_elig_18, ", no funds"), paste0(cc_elig_18, ", funded")))

block_pop_2010 <- read.csv("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/population_by_census_block_2010.csv")
block_pop_2010$GEOID <- ifelse(nchar(block_pop_2010$GEOID) < 15, paste0(0, block_pop_2010$GEOID), block_pop_2010$GEOID)

final_eligibility_df_ <- final_eligibility_df_ %>% left_join(block_pop_2010 %>% transmute(GEOID, pop_2010 = value), by = c("FULL_GEOID" = "GEOID"))

write.csv(final_eligibility_df_, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/eligibility_summary_percentages/USA_2014_18_final_eligibility_df_15DEC2020.csv")

final_eligibility_df <- final_eligibility_df_ %>% left_join(join_18_rc %>% select(-BlockCode), by = c("FULL_GEOID" = "GEOID10"))


final_eligibility_df <- final_eligibility_df %>% 
  mutate(rc_elig_18 = ifelse(Speed18 == "eligible" & Nonrural18 == "eligible" & Projects14 == "eligible", "eligible", "ineligible"), 
         rc_areas_18_ANY = ifelse(is.na(RC_18_RUSID_intersect), paste0(rc_elig_18, ", no funds"), paste0(rc_elig_18, ", funded")),
         rc_areas_18_IN = ifelse(is.na(RC_18_RUSID_inside), paste0(rc_elig_18, ", no funds"), paste0(rc_elig_18, ", funded")),
         rc_areas_18_BOUND = ifelse(is.na(RC_18_RUSID_boundary), paste0(rc_elig_18, ", no funds"), paste0(rc_elig_18, ", funded")))

colnames(final_eligibility_df)

final_eligibility_df <- final_eligibility_df %>%
  transmute(FULL_GEOID, pop_2010,
            # Eligibility criteria
            CC_ELIG_14 = cc_elig_14, CC_ELIG_18 = cc_elig_18, 
            # Eligibility bins
            CC_14_elig_fund_ANY = areas_14_ANY, CC_14_elig_fund_INSIDE = areas_14_IN, CC_14_elig_fund_BOUND = areas_14_BOUND, 
            # CC Project areas 
            CC_14_RUSID_intersect, CC_14_RUSID_boundary, CC_14_RUSID_inside, 
            CC_18_RUSID_intersect, CC_18_RUSID_boundary, CC_18_RUSID_inside, 
            # Eligibility criteria
            rc_elig_18,
            # Eligibility bins
            RC_14_elig_fund_ANY = rc_areas_18_ANY, RC_14_elig_fund_INSIDE = areas_14_IN, RC_14_elig_fund_BOUND = areas_14_BOUND, 
            # RC Project areas 
            RC_18_RUSID_intersect = RC_18_RUSID_intersect.x, RC_18_RUSID_inside = RC_18_RUSID_inside.x, RC_18_RUSID_boundary = RC_18_RUSID_boundary.x
            )

write.csv(final_eligibility_df, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/eligibility_summary_percentages/USA_2014_18_final_CCRC_eligibility_fundedareas_df_15DEC2020.csv")

summary_pop <- rbind(final_eligibility_df_ %>% group_by(bucket = areas_14_ANY) %>% summarise(pop = sum(pop_2010, na.rm = TRUE)) %>% mutate(set = "areas_14_ANY"),
                     final_eligibility_df_ %>% group_by(bucket = areas_18_ANY) %>% summarise(pop = sum(pop_2010, na.rm = TRUE)) %>% mutate(set = "areas_18_ANY"),
                     final_eligibility_df_ %>% group_by(bucket = areas_14_IN) %>% summarise(pop = sum(pop_2010, na.rm = TRUE)) %>% mutate(set = "areas_14_IN"),
                     final_eligibility_df_ %>% group_by(bucket = areas_18_IN) %>% summarise(pop = sum(pop_2010, na.rm = TRUE)) %>% mutate(set = "areas_18_IN"),
                     final_eligibility_df_ %>% group_by(bucket = areas_14_BOUND) %>% summarise(pop = sum(pop_2010, na.rm = TRUE)) %>% mutate(set = "areas_14_BOUND"),
                     final_eligibility_df_ %>% group_by(bucket = areas_18_BOUND) %>% summarise(pop = sum(pop_2010, na.rm = TRUE)) %>% mutate(set = "areas_18_BOUND"))

summary_bc <- rbind(final_eligibility_df_ %>% count(bucket = areas_14_ANY) %>% mutate(set = "areas_14_ANY"),
                    final_eligibility_df_ %>% count(bucket = areas_18_ANY)  %>% mutate(set = "areas_18_ANY"),
                    final_eligibility_df_ %>% count(bucket = areas_14_IN)  %>% mutate(set = "areas_14_IN"),
                    final_eligibility_df_ %>% count(bucket = areas_18_IN)  %>% mutate(set = "areas_18_IN"),
                    final_eligibility_df_ %>% count(bucket = areas_14_BOUND)  %>% mutate(set = "areas_14_BOUND"),
                    final_eligibility_df_ %>% count(bucket = areas_18_BOUND)  %>% mutate(set = "areas_18_BOUND"))

make_proportion <- function(x) { round(sum(x, na.rm = TRUE)/308745538, 3) }
make_proportion2 <- function(x) { round(sum(x, na.rm = TRUE)/11078297, 3) }

summary <- rbind(summary_pop %>% reshape2::dcast(set~bucket, value.var = "pop", fun.aggregate = sum) %>% mutate(metric = "pop"),
                 summary_pop %>% reshape2::dcast(set~bucket, value.var = "pop", fun.aggregate = make_proportion) %>% mutate(metric = "pop"),
                 summary_bc %>% reshape2::dcast(set~bucket, value.var = "n", fun.aggregate = sum) %>% mutate(metric = "bc"),
                 summary_bc %>% reshape2::dcast(set~bucket, value.var = "n", fun.aggregate = make_proportion2) %>% mutate(metric = "bc"))

write.csv(summary, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/eligibility_summary_percentages/elig_types1418_popbc.csv")



### CHECK
usa_blocks_speed_rural_projects_final_2014 %>% as.data.frame() %>% count( is.na(CC_14_RUSID_intersect), is.na(CC_14_RUSID_boundary), is.na(CC_14_RUSID_inside))

## CHECK PLOT
intersect_lookatme <- usa_blocks_speed_rural_projects_final_2014 %>% filter(!is.na(CC_14_RUSID_intersect))
intersect_lookatme_OK <- intersect_lookatme %>% filter(state == "OK")

leaflet(intersect_lookatme_OK %>% filter(!is.na(CC_14_RUSID_inside))) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons() %>%
  addPolygons(data = intersect_lookatme_OK %>% filter(!is.na(CC_14_RUSID_boundary)), color = "red") %>%
  addPolygons(data = cc_areas_2014andearlier %>% filter(RUSID == "OK1410-B23"), color = "black", fill = FALSE)

usa_blocks_speed_rural_projects_final_2018 %>% as.data.frame() %>% count( is.na(CC_18_RUSID_intersect), is.na(CC_18_RUSID_boundary), is.na(CC_18_RUSID_inside))
intersect_lookatme <- usa_blocks_speed_rural_projects_final_2014 %>% filter(!is.na(CC_14_RUSID_intersect))
intersect_lookatme_OK <- intersect_lookatme %>% filter(state == "OK")

leaflet(intersect_lookatme_OK %>% filter(!is.na(CC_14_RUSID_inside))) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons() %>%
  addPolygons(data = intersect_lookatme_OK %>% filter(!is.na(CC_14_RUSID_boundary)), color = "red") %>%
  addPolygons(data = cc_areas_2014andearlier %>% filter(RUSID == "OK1410-B23"), color = "black", fill = FALSE)

## JOIN? 
usa_blocks_speed_rural_projects_final_2018 <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2018_final_eligibility_sf_14DEC2020v5.RDS")


####


# eligblocks_bip_intersect14 <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2014, bip_areas)
# eligblocks_cc14_intersect <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2014, cc_areas_2014andearlier)
# eligblocks_rc14_intersect <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2014, rc_areas_2014andearlier)
# eligblocks_tc_fb_intersect14 <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2014, tc_fb_areas)
# eligblocks_tc_inf_intersect14 <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2014, tc_inf_areas)
# 
# eligblocks_bip_intersect18 <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2018, bip_areas)
# eligblocks_cc18_intersect <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2018, cc_areas_2018andearlier)
# eligblocks_rc18_intersect <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2018, rc_areas_2018andearlier)
# eligblocks_tc_fb_intersect18 <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2018, tc_fb_areas)
# eligblocks_tc_inf_intersect18 <- sf::st_intersects(usa_blocks_speed_rural_projects_final_2018, tc_inf_areas)
# 
# 
# usa_blocks_speed_rural_projects_final_2014$BIP_PROJECTID <- bip_areas$ProjectID[ sapply(eligblocks_bip_intersect14, first) ]
# usa_blocks_speed_rural_projects_final_2014$CC_RUSID <- cc_areas_2014andearlier$RUSID[ sapply(eligblocks_cc14_intersect, first) ]
# usa_blocks_speed_rural_projects_final_2014$RC_RUSID <- rc_areas_2014andearlier$RUS_ID[ sapply(eligblocks_rc14_intersect, first) ]
# usa_blocks_speed_rural_projects_final_2014$TC_FB_RUSID <- tc_fb_areas$RUS_ID[ sapply(eligblocks_tc_fb_intersect14, first) ]
# usa_blocks_speed_rural_projects_final_2014$TC_INF_RUSID <- tc_inf_areas$RUSID[ sapply(eligblocks_tc_inf_intersect14, first) ]
# 
# usa_blocks_speed_rural_projects_final_2018$BIP_PROJECTID <- bip_areas$ProjectID[ sapply(eligblocks_bip_intersect18, first) ]
# usa_blocks_speed_rural_projects_final_2018$CC_RUSID <- cc_areas_2018andearlier$RUSID[ sapply(eligblocks_cc18_intersect, first) ]
# usa_blocks_speed_rural_projects_final_2018$RC_RUSID <- rc_areas_2018andearlier$RUS_ID[ sapply(eligblocks_rc18_intersect, first) ]
# usa_blocks_speed_rural_projects_final_2018$TC_FB_RUSID <- tc_fb_areas$RUS_ID[ sapply(eligblocks_tc_fb_intersect18, first) ]
# usa_blocks_speed_rural_projects_final_2018$TC_INF_RUSID <- tc_inf_areas$RUSID[ sapply(eligblocks_tc_inf_intersect18, first) ]
# 
# # saveRDS(usa_blocks_speed_rural_projects_final_2014, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2014_final_eligibility_sf_08DEC2020v4.RDS")
# saveRDS(usa_blocks_speed_rural_projects_final_2018, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2018_final_eligibility_sf_08DEC2020v3.RDS")
# # table(is.na(usa_blocks_speed_rural_projects_final_2014$BIP_PROJECTID))
# # table(is.na(usa_blocks_speed_rural_projects_final_2014$CC_RUSID))
# # table(is.na(usa_blocks_speed_rural_projects_final_2014$RC_RUSID))
# # table(is.na(usa_blocks_speed_rural_projects_final_2014$TC_FB_RUSID))
# # table(is.na(usa_blocks_speed_rural_projects_final_2014$TC_INF_RUSID))
# rm(eligblocks_rc14_intersect)


