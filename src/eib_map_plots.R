# EIB paper maps

# packages
library(ggplot2)
library(sf)
library(tidycensus)
library(readxl)
library(tigris)
library(dplyr)
options(tigris_use_cache = TRUE)
# set a working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

# BIP data
BIP_shapes <- load(paste0(path, "BIP_linear_data/BIP_New.rds"))
bip_ak <- newbip_union[substr(newbip_union$ProjectID, 1,2) == "AK",]
bip_hi <- newbip_union[substr(newbip_union$ProjectID, 1,2) == "HI",]
bip_pr <- newbip_union[substr(newbip_union$ProjectID, 1,2) == "PR",]

bip_shapes <- newbip_union[substr(newbip_union$ProjectID, 1,2) != "AK",]
bip_shapes <- bip_shapes[substr(bip_shapes$ProjectID, 1,2) != "HI",]
bip_shapes <- bip_shapes[substr(bip_shapes$ProjectID, 1,2) != "PR",]

# load states shapes
states <- get_acs(
  geography = "state", 
  variables = "B19013_001",
  year = 2018,
  geometry = TRUE
)

ak_state <- states[states$GEOID == "02",]
hi_state <- states[states$GEOID == "15",]
pr_state <- states[states$GEOID == "72",]

fill = ("BIP areas" = "#D55E00")

ak_map <- ggplot() + 
  geom_sf(data=ak_state, aes(geometry=geometry), fill = "cornsilk", color="grey50", lwd=0.1) +
  geom_sf(data=bip_ak, aes(geometry=geometry, fill = "BIP areas", color = "BIP areas"), lwd=0.5) +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA) +
  guides(fill="none", color="none") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())

hi_map <- ggplot() + 
  geom_sf(data=hi_state, aes(geometry=geometry), fill = "cornsilk", color="grey50", lwd=0.1) +
  geom_sf(data=bip_hi, aes(geometry=geometry, fill = "BIP areas", color = "BIP areas"), lwd=0.5) +
  coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE, datum = NA) +
  guides(fill="none", color="none") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())

mainland <- ggplot() + 
  geom_sf(data=states, aes(geometry=geometry), fill = "cornsilk", color="grey50", lwd=0.1) +
  geom_sf(data=bip_shapes, aes(geometry=geometry, fill = "BIP areas", color = "BIP areas"), lwd=0.5) +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000)) + 
  guides(color="none") + labs(fill = "Legend") + scale_fill_manual(values = fill) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())

mainland +
  annotation_custom(
    grob = ggplotGrob(ak_map),
    xmin = -2750000,
    xmax = -2750000 + (1600000 - (-2400000))/2.5,
    ymin = -2450000,
    ymax = -2450000 + (2500000 - 200000)/2.5
  ) +
  annotation_custom(
    grob = ggplotGrob(hi_map),
    xmin = -1250000,
    xmax = -1250000 + (-154 - (-161))*120000,
    ymin = -2450000,
    ymax = -2450000 + (23 - 18)*120000
  )

# save the figure
#ggsave("bip_usmap_eib_update.png")


#####################
# Community Connect
#####################

# CCG data
CCG_2013_20 <- st_read(paste0(path, "RUS_CC_Shapefiles_Nov2021/CC_2013-2019-83_09042020/"),"CC 2013_2019_83 09042020",
                       int64_as_string = TRUE)
ccg21 <- st_read(paste0(path,"RUS_CC_Shapefiles_Nov2021/FY21_CC_Approved_PFSA_Layer/"),"FY21_CC_Approved_PFSA_Layer",
                 int64_as_string = TRUE)
CCG_2013_20$APPFY <- as.numeric(as.character(CCG_2013_20$APPFY)) 
ccg20 <- CCG_2013_20[CCG_2013_20$APPFY >= 2018,]

ccg20_ak <- ccg20[ccg20$STATE == "AK",] 
ccg20_hi <- ccg20[ccg20$STATE == "HI",] 
ccg20_pr <- ccg20[ccg20$STATE == "PR",] 

ccg20_shapes <- ccg20[!(ccg20$STATE %in% c("AK", "HI", "PR")),]

ccg21_ak <- ccg21[substr(ccg21$RUSID, 1,2) == "AK",]
ccg21_hi <- ccg21[substr(ccg21$RUSID, 1,2) == "HI",]
ccg21_pr <- ccg21[substr(ccg21$RUSID, 1,2) == "PR",]

ccg21_shapes <- ccg21[substr(ccg21$RUSID, 1,2) != "AK",]
ccg21_shapes <- ccg21_shapes[substr(ccg21_shapes$RUSID, 1,2) != "HI",]
ccg21_shapes <- ccg21_shapes[substr(ccg21_shapes$RUSID, 1,2) != "PR",]

# eligibility data
#elig_data <- read_csv(paste0(path, "Community_Connect_Summaries/CC2010_census_block_eligibility.csv"))
#elig_data <- elig_data[,c("GEOID10", "tract", "eligibile2018")]
#elig_data <- elig_data %>% filter(eligibile2018 == "eligible")
#saveRDS(elig_data, paste0(path, "BIP_linear_data/only_elig_blocks2018.rds")) 

# load eligible only blocks
elig_data <- readRDS(paste0(path, "BIP_linear_data/only_elig_blocks2018.rds"))

# block shapes
#blocks10 <- readRDS("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/block2010_geometry.RDS")
blocks_list <- list()
for (s in states$GEOID){
  print(s)
  blocks <- blocks(state=s, year=2018)
  i <- which(states$GEOID == s)
  blocks <- blocks[blocks$GEOID10 %in% elig_data$GEOID10,]
  blocks_list[[i]] <- blocks 
}
blocks <- do.call(rbind, blocks_list)
#st_write(blocks, paste0(path, "BIP_linear_data/elig_blocks.shp"))
blocks <- blocks[,c("GEOID10", "ALAND10", "AWATER10", "geometry")]

#elig_data <- left_join(elig_data, alt_block, by="GEOID10")
#saveRDS(elig_data, paste0(path, "BIP_linear_data/elig_blocks2018_geometry.rds")) 

# load in eligible blocks in 2018 
#elig_data <- readRDS(paste0(path, "BIP_linear_data/elig_blocks2018_geometry.rds"))

blocks_ak <- blocks[substr(blocks$GEOID10, 1,2) == "02",]
blocks_hi <- blocks[substr(blocks$GEOID10, 1,2) == "15",]
blocks_pr <- blocks[substr(blocks$GEOID10, 1,2) == "72",]

#elig_ak <- elig_data[substr(elig_data$GEOID10, 1,2) == "02",]
#elig_hi <- elig_data[substr(elig_data$GEOID10, 1,2) == "15",]
#elig_pr <- elig_data[substr(elig_data$GEOID10, 1,2) == "72",]

#elig_blocks <- elig_data[substr(elig_data$GEOID10, 1,2) != "02",]
#elig_blocks <- elig_blocks[substr(elig_blocks$GEOID10, 1,2) != "15",]
#elig_blocks <- elig_blocks[substr(elig_blocks$GEOID10, 1,2) != "72",]

block_shapes <- blocks[substr(blocks$GEOID10, 1,2) != "02",]
block_shapes <- block_shapes[substr(block_shapes$GEOID10, 1,2) != "15",]
block_shapes <- block_shapes[substr(block_shapes$GEOID10, 1,2) != "72",]

fill = c("CCG approved in FY2018-2021" = "#D55E00", "Eligible areas without CCG project" = "#009E73")

ak_map <- ggplot() + 
  geom_sf(data=ak_state, aes(geometry=geometry), fill = "cornsilk", color="grey50", lwd=0.1) +
  geom_sf(data=blocks_ak[blocks_ak$ALAND10 != 0,], aes(geometry=geometry, fill="Eligible areas without CCG project", alpha=0.5), lwd=0) +
  geom_sf(data=ccg20_ak, aes(geometry=geometry, fill = "CCG approved in FY2018-2021", 
                             color = "CCG approved in FY2018-2021"), lwd=1) +
  geom_sf(data=ccg21_ak, aes(geometry=geometry, fill = "CCG approved in FY2018-2021", 
                             color = "CCG approved in FY2018-2021"), lwd=1) +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA) +
  guides(fill="none", color="none", alpha="none") + scale_fill_manual(values = fill) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())

hi_map <- ggplot() + 
  geom_sf(data=hi_state, aes(geometry=geometry), fill = "cornsilk", color="grey50", lwd=0.1) +
  coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE, datum = NA) +
  guides(fill="none", color="none") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())

mainland <- ggplot() + 
  geom_sf(data=states, aes(geometry=geometry), fill = "cornsilk", color="grey50", lwd=0.1) +
  geom_sf(data=block_shapes[block_shapes$ALAND10 != 0,], aes(geometry=geometry, fill="Eligible areas without CCG project", alpha=0.5), lwd=0) +
  geom_sf(data=ccg20_shapes, aes(geometry=geometry, fill = "CCG approved in FY2018-2021", 
                                 color = "CCG approved in FY2018-2021"), lwd=1) +
  geom_sf(data=ccg21_shapes, aes(geometry=geometry, fill = "CCG approved in FY2018-2021", 
                                 color = "CCG approved in FY2018-2021"), lwd=1) +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000)) + 
  guides(color="none", alpha="none") + labs(fill = "Legend") + scale_fill_manual(values = fill) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())

mainland +
  annotation_custom(
    grob = ggplotGrob(ak_map),
    xmin = -2750000,
    xmax = -2750000 + (1600000 - (-2400000))/2.5,
    ymin = -2450000,
    ymax = -2450000 + (2500000 - 200000)/2.5
  ) +
  annotation_custom(
    grob = ggplotGrob(hi_map),
    xmin = -1250000,
    xmax = -1250000 + (-154 - (-161))*120000,
    ymin = -2450000,
    ymax = -2450000 + (23 - 18)*120000
  )

# save the figure
ggsave("ccg_usmap_eib_update.png")

#####################
# ReConnect Map
#####################

# RCP data
RCP_round1 <- st_read(paste0(path, "RUS_RC_Shapefiles_August2021/ReConnect_984_PFSAs/"),"USDARD_PFSA_ReConnect",
                      int64_as_string = TRUE, stringsAsFactors = F)
RCP_round2 <- st_read(paste0(path, "RUS_RC_Shapefiles_August2021/ReConnect_Round_2_Final/"),"ReConnect_Round_2_Final",
                      int64_as_string = TRUE)

project_data <- read_excel("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_RC_Shapefiles_August2021/Round 1 and 2 Submitted ReConnect Applications_8-6-2021.xlsx")

# add program obligation dates 
project_data$Applicat_2 <- as.numeric(project_data$Applicat_2)
round2 <- left_join(RCP_round2, project_data[,c("Applicat_2", "Completion Status")], by= "Applicat_2")
RCP_round1$Applicat_1 <- as.numeric(RCP_round1$Applicat_1)
round1 <- left_join(RCP_round1, project_data[,c("Applicat_2", "Completion Status")], by=c("Applicat_1"="Applicat_2"))

rcp1_ak <- round1[substr(round1$CongDistri, 1,2) == "AK",] 
rcp1_hi <- round1[substr(round1$CongDistri, 1,2) == "HI",] 
rcp1_pr <- round1[substr(round1$CongDistri, 1,2) == "PR",] 

rcp1_shapes <- round1[substr(round1$CongDistri, 1,2) != "AK",] 
rcp1_shapes <- rcp1_shapes[substr(rcp1_shapes$CongDistri, 1,2) != "HI",] 
rcp1_shapes <- rcp1_shapes[substr(rcp1_shapes$CongDistri, 1,2) != "PR",] 

# initilize list to save results:
state_round2 <- list()
round2$geometry <- st_transform(round2$geometry,st_crs(states))
# sf settings to avoid Loop 0 error
sf_use_s2(TRUE)
# indeces of counties which contain a geopoint
inds <- st_intersects(round2$geometry, states$geometry, sparse=T)
round2['stateID'] <- NA
for (i in 1:length(inds)){
  if (identical(states$NAME[inds[[i]]],character(0))){
    round2$stateID[i] <- NA}
  else{
    round2$stateID[i] <- list(states$GEOID[inds[[i]]])
  }}

rcp2_ak <- round2[substr(round2$stateID, 1,2) == "02",] 
rcp2_hi <- round2[substr(round2$stateID, 1,2) == "15",] 
rcp2_pr <- round2[substr(round2$stateID, 1,2) == "72",] 

rcp2_shapes <- round2[substr(round2$stateID, 1,2) != "02",] 
rcp2_shapes <- rcp2_shapes[substr(rcp2_shapes$stateID, 1,2) != "15",] 
rcp2_shapes <- rcp2_shapes[substr(rcp2_shapes$stateID, 1,2) != "72",] 

fill = c("Accepted" = "#D55E00", "Applied, not approved" = "#999999", "Eligible areas without RCP" = "#009E73")

ak_map <- ggplot() + 
  geom_sf(data=ak_state, aes(geometry=geometry), fill = "cornsilk", color="grey50", lwd=0.1) +
  geom_sf(data=blocks_ak[blocks_ak$ALAND10 != 0,], 
          aes(geometry=geometry, fill="Eligible areas without RCP", alpha=0.5), lwd=0) +
  geom_sf(data=rcp1_ak[rcp1_ak$`Completion Status` == "Rejected",], 
          aes(geometry=geometry, fill = "Applied, not approved", color = "Applied, not approved"), lwd=1) +
  geom_sf(data=rcp2_ak[rcp2_ak$`Completion Status` == "Rejected",], 
          aes(geometry=geometry, fill = "Applied, not approved", color = "Applied, not approved"), lwd=1) +
  geom_sf(data=rcp1_ak[rcp1_ak$`Completion Status` == "Approved",], 
          aes(geometry=geometry, fill = "Accepted", color = "Accepted"), lwd=1) +
  geom_sf(data=rcp2_ak[rcp2_ak$`Completion Status` == "Approved",], 
          aes(geometry=geometry, fill = "Accepted", color = "Accepted"), lwd=1) +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA) +
  guides(fill="none", color="none", alpha="none") + scale_fill_manual(values = fill) + scale_color_manual(values = fill) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())

hi_map <- ggplot() + 
  geom_sf(data=hi_state, aes(geometry=geometry), fill = "cornsilk", color="grey50", lwd=0.1) +
  coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE, datum = NA) +
  guides(fill="none", color="none") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())

mainland <- ggplot() + 
  geom_sf(data=states, aes(geometry=geometry), fill = "cornsilk", color="grey50", lwd=0.1) +
  geom_sf(data=block_shapes[block_shapes$ALAND10 != 0,], 
          aes(geometry=geometry, fill="Eligible areas without RCP", alpha=0.5), lwd=0) +
  geom_sf(data=rcp1_shapes[rcp1_shapes$`Completion Status` == "Rejected",], 
          aes(geometry=geometry, fill = "Applied, not approved", color = "Applied, not approved"), lwd=1) +
  geom_sf(data=rcp2_shapes[rcp2_shapes$`Completion Status` == "Rejected",], 
          aes(geometry=geometry, fill = "Applied, not approved", color = "Applied, not approved"), lwd=1) +
  geom_sf(data=rcp1_shapes[rcp1_shapes$`Completion Status` == "Approved",], 
          aes(geometry=geometry, fill = "Accepted", color = "Accepted"), lwd=1) +
  geom_sf(data=rcp2_shapes[rcp2_shapes$`Completion Status` == "Approved",], 
          aes(geometry=geometry, fill = "Accepted", color = "Accepted"), lwd=1) +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000)) + 
  guides(color="none", alpha="none") + labs(fill = "Legend") + 
  scale_fill_manual(values = fill) + scale_color_manual(values = fill) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())

mainland +
  annotation_custom(
    grob = ggplotGrob(ak_map),
    xmin = -2750000,
    xmax = -2750000 + (1600000 - (-2400000))/2.5,
    ymin = -2450000,
    ymax = -2450000 + (2500000 - 200000)/2.5
  ) +
  annotation_custom(
    grob = ggplotGrob(hi_map),
    xmin = -1250000,
    xmax = -1250000 + (-154 - (-161))*120000,
    ymin = -2450000,
    ymax = -2450000 + (23 - 18)*120000
  )

# save the figure
ggsave("rcp_usmap_eib_update.png")


#####################
# OHIO TEST
#####################


elig_oh <- elig_blocks[substr(elig_blocks$GEOID10, 1,2) == "39",]

# get water areas
county_oh <- counties(state="OH", year=2018)
water_list <- list()
for (c in county_oh$COUNTYFP){
  print(c)
  water_oh <- area_water(state="OH", county=c, year=2018)
  i <- which(county_oh$COUNTYFP == c)
  water_list[[i]] <- water_oh 
}
water_oh <- do.call(rbind, water_list)

ggplot() + 
  geom_sf(data=states[states$GEOID == "39",], aes(geometry=geometry), fill = "cornsilk", color="grey50", lwd=0.1) +
  geom_sf(data=elig_oh, aes(geometry=geometry.x, fill="#F0E442"), lwd=0) +
  geom_sf(data=water_oh, aes(geometry=geometry), lwd=0, fill="cornsilk") +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA) +
  guides(fill="none", color="none") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())
