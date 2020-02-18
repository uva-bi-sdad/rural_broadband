library(readr)
library(tidyverse)
library(stringr)
library(sf)
 
#censusblocksfolder <- "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"
datapath <- "/project/biocomplexity/sdad/projects_data/usda/bb/"
censusblocksfolder <- "/project/biocomplexity/sdad/projects_data/usda/bb/original/censusblocks/blocks_TIGER2018_zips_shapes/"
zip_contents <- list.files(censusblocksfolder)
shapepaths <- zip_contents[str_detect(zip_contents, ".shp$")]
shapepaths <- data.frame(files = shapepaths, fips = str_remove_all(str_extract(shapepaths, "_\\d\\d_"), "_"))

states <- read.csv("/project/biocomplexity/sdad/projects_data/dtn2ep_homedir_overflow/usastatefipscodes.csv",  colClasses = c(rep("character",6))) %>% select(1:3) 
colnames(states) <- dataplumbr::name.standard_col_names(colnames(states))
states <- states %>% mutate(fips_code = ifelse(nchar(fips_code) == 1, paste0("0", fips_code), fips_code)) %>% 
  left_join(shapepaths, by = c("fips_code" = "fips"))

borders <- read_csv(paste0(datapath, "original/stateborder.csv")) %>% select(2:3)

number <- 2
state_fips <-  states$fips_code[number]
state_abbrev <- states$state_abbreviation[number]
neighbors <- borders %>% filter(ST1 == state_abbrev | ST2 == state_abbrev) %>% mutate(neighbors = ifelse(ST1 == state_abbrev, ST2, ST1))
neighbors <- c(neighbors$neighbors, state_abbrev)

usa_bip_shapes <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/BIP_working/BIP_Approved.RDS")
nearest_bips <- usa_bip_shapes[usa_bip_shapes$STUSPS %in% neighbors,]
plot(nearest_bips$geometry)

state_blocks <- st_read(paste0(censusblocksfolder, states$files[number])) %>% st_transform(st_crs(usa_bip_shapes))
state_centroids <- st_centroid(state_blocks)

test <- st_distance(head(state_centroids, 2), nearest_bips)




