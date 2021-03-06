library(readr)
library(tidyverse)
library(stringr)
library(sf)
library(tigris)

rappdirs::user_cache_dir()
 
options(tigris_use_cache = FALSE)

#censusblocksfolder <- "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"
datapath <- "/project/biocomplexity/sdad/projects_data/usda/bb/"
fcc2011_200 <- readRDS(paste0(datapath, "working/fcc2011_200byblock_count.RDS")) 
fcc2011_3 <- readRDS(paste0(datapath, "working/fcc2011_3byblock_count.RDS"))  

fcc2016_200 <- readRDS(paste0(datapath, "working/fcc2016_200byblock_count.RDS"))
fcc2016_3 <- readRDS(paste0(datapath, "working/fcc2016_3byblock_count.RDS")) 

censusblocksfolder <- "/project/biocomplexity/sdad/projects_data/usda/bb/original/censusblocks/blocks_TIGER2018_zips_shapes/"
zip_contents <- list.files(censusblocksfolder)
shapepaths <- zip_contents[str_detect(zip_contents, ".shp$")]
shapepaths <- data.frame(files = shapepaths, fips = str_remove_all(str_extract(shapepaths, "_\\d\\d_"), "_"))

states <- read.csv("/project/biocomplexity/sdad/projects_data/dtn2ep_homedir_overflow/usastatefipscodes.csv",  colClasses = c(rep("character",6))) %>% select(1:3) 
colnames(states) <- dataplumbr::name.standard_col_names(colnames(states))
states <- states %>% mutate(fips_code = ifelse(nchar(fips_code) == 1, paste0("0", fips_code), fips_code)) %>% 
  left_join(shapepaths, by = c("fips_code" = "fips"))

borders <- read_csv(paste0(datapath, "original/stateborder.csv")) %>% select(2:3)

states <- states %>% filter(!str_detect(state_abbreviation, "AL|CA|CO|FL|GA|OH|NC|PA|TX"))

number <- 9
state_fips <-  states$fips_code[number]
state_abbrev <- states$state_abbreviation[number]
state_abbrev
neighbors <- borders %>% filter(ST1 == state_abbrev | ST2 == state_abbrev) %>% mutate(neighbors = ifelse(ST1 == state_abbrev, ST2, ST1))
neighbors <- c(neighbors$neighbors, state_abbrev)

datapath2 <- "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"
usa_bip_shapes <- readRDS(paste0(datapath2, "working/BIP_working/BIP_Approved.RDS"))
nearest_bips <- usa_bip_shapes[usa_bip_shapes$STUSPS %in% neighbors,]

state_outline <- tigris::counties(state_abbrev, refresh = TRUE) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))

state_blocks <- st_read(paste0(censusblocksfolder, states$files[number])) %>% st_transform(st_crs(usa_bip_shapes))
state_centroids <- st_centroid(state_blocks)

alldistances <- st_distance(nearest_bips, state_centroids) #rows = centroids #cols = BIPS
mindist <- apply(alldistances, MARGIN=2, min)
rm(alldistances)

state_centroids$bip_dist = mindist

hist(mindist[0 < mindist & mindist < 500])
inbip <- state_centroids %>% filter(bip_dist <500 & bip_dist > 0)

plot(state_outline$geometry)
plot(nearest_bips$geometry, col = "red", add = TRUE)
plot(inbip$geometry, add = TRUE, col = "blue", pch = 0)

state_bipdist_nbm11fcc16 <- state_centroids %>% 
  left_join(fcc2011_200, by = c("GEOID10" = "GEOID")) %>% 
  left_join(fcc2011_3, by = c("GEOID10" = "GEOID")) %>% 
  left_join(fcc2016_200, by = c("GEOID10" = "GEOID")) %>% 
  left_join(fcc2016_3, by = c("GEOID10" = "GEOID"))

# table(is.na(state_bipdist_nbm11fcc16$fcc2011_providers_200))
# table(is.na(state_bipdist_nbm11fcc16$fcc2011_providers_3))
# table(is.na(state_bipdist_nbm11fcc16$fcc2016_providers_200))
# table(is.na(state_bipdist_nbm11fcc16$fcc2016_providers_3))
# state_bipdist_nbm11fcc16 %>% filter(is.na(fcc2011_providers_200))
# fcc2011_200____actuals <- readRDS(paste0(datapath, "working/fcc2011_200byblock.RDS")) 
# 
# state_bipdist_nbm11fcc16 %>% filter(GEOID10 == "010890113001161")
# fcc2011_200____actuals %>% filter(GEOID == "010890113001161")
# fcc2011_200____actuals %>% filter(str_detect(GEOID, "^01089011300116"))

saveRDS(state_bipdist_nbm11fcc16, paste0(datapath2, "working/state_blocks_centroids_bipdist_2011_16/", "centroids_manip_", state_abbrev, ".RDS"))

check <- list.files("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/state_blocks_centroids_bipdist_2011_16/")
check <- str_remove_all(str_extract(check, "_[:alpha:][:alpha:].RDS"), "_|.RDS")
length(check)
nrow(states)
setdiff(states$state_abbreviation, check)
