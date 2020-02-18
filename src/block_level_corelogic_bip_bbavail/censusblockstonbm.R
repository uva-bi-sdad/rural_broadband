library(tidyverse)
library(sf)

censusblocks2010_folder <- "/project/biocomplexity/sdad/projects_data/usda/bb/original/censusblocks/blocks_TIGER2018_zips_shapes/"
censusblocks2010_folder <- "/project/biocomplexity/sdad/projects_data/usda/bb/original/censusblocks/blocks_TIGER2018_sf_RDS/"
censusblocks2010_paths <- list.files(censusblocks2010_folder)

#fipscodes <- stringr::str_extract(censusblocks2010_paths, "_\\d\\d_") %>% stringr::str_remove_all(pattern = "_")
texaspath <- censusblocks2010_paths[stringr::str_detect(censusblocks2010_paths, "48")]

texas <- st_read("/project/biocomplexity/sdad/projects_data/usda/bb/original/censusblocks/blocks_TIGER2018_zips_shapes/texas_blocks/tl_2018_48_tabblock10.shp")
texas_all_centroid <- st_centroid(texas)


bip_approved_join <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/BIP_working/BIP_Approved.RDS")
#texas_bip <- bip_approved_join %>% filter(NAME == "Texas")

states <- tigris::states() %>% st_as_sf()
texas_outline <- states %>% filter(NAME == "Texas") %>% st_transform(st_crs(bip_approved_join))

texas_all_centroid <- texas_all_centroid %>% st_transform(st_crs(bip_approved_join))

#borders_url <- "http://users.econ.umn.edu/~holmes/data/BORDLIST.html"

borders <- read_csv("/project/biocomplexity/sdad/projects_data/usda/bb/original/stateborder.csv")

 get_centroids_distance <- function(source_centroids, ref_bips, state_abbrev)

texas_borders <- borders %>% filter(ST1 == "TX" | ST2 == "TX") %>% mutate(ST3 = ifelse(ST1 == "TX", ST2, ST1))

texas_bip <- bip_approved_join %>% filter(STUSPS %in% c("TX", texas_borders$ST3))

texas_distances_txonly <- st_distance(texas_bip, texas_all_centroid) 

mindist <- apply(X = texas_distances_txonly, MARGIN = 2, FUN = min)

texas_all_centroid$bip_distance <- mindist
# (round(texas_all_centroid$bip_distance, digits = -4))
plot(texas_outline$geometry)
plot(texas_bip$geometry, add = TRUE, col = "black")
plot(texas_all_centroid$geometry, add = TRUE, col = texas_all_centroid$bip_distance, pch = 15)

saveRDS(texas_all_centroid, "~/texas_all_centroid_dist_to4BIPs.RDS")



################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

allstatesbuttx <- paste0(censusblocks2010_folder, censusblocks2010_paths)

saveRDS(allstatesbuttx_rds, "~/allstatesbuttx_rds.RDS") #"/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/BIP_working/allstatesbuttx_rds.RDS")

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

allstatesbuttx <- paste0(censusblocks2010_folder, censusblocks2010_paths)

allstatesbuttx_rds <- allstatesbuttx %>%
  map(readRDS) 

saveRDS(allstatesbuttx_rds, "~/allstatesbuttx_rds.RDS") #"/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/BIP_working/allstatesbuttx_rds.RDS")
saveRDS(texas, "~/tx_rds.RDS")

allstatesbuttx_rds <- readRDS("~/allstatesbuttx_rds.RDS") #"/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/BIP_working/allstatesbuttx_rds.RDS")
texas <- readRDS("~/git/rural_broadband/ /tx_rds.RDS")

allstatesbuttx_rds2 <- do.call(rbind, allstatesbuttx_rds)


allstates_withtexas <- rbind(allstatesbuttx_rds, texas)
allstatesbuttx_rds[1,]
unique(allstatesbuttx_rds$STATEFP10)

alabamageometry <- allstatesbuttx_rds[[1]]$geometry
alaskageometry <- allstatesbuttx_rds[[2]]$geometry
test <- allstatesbuttx_rds[1:2] 

st_geometry(test[[1]]) <- NULL
st_geometry(test[[2]]) <- NULL
test <- rbind(test[[1]], test[[2]])
data.table::setDT(test)
test[test$STATEFP10 == "01", geometry := alabamageometry] 
st(test)


