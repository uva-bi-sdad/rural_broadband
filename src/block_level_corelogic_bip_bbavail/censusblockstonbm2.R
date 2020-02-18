library(stringr)
library(dplyr)

usastatefipscodes <- read.csv("~/usastatefipscodes.csv") %>% select(1:3)

censusblocks2010_folder <- "/project/biocomplexity/sdad/projects_data/usda/bb/original/censusblocks/blocks_TIGER2018_zips_shapes/"
censusblocks2010_paths <- list.files(censusblocks2010_folder)
shapefiles <- censusblocks2010_paths[ str_detect(censusblocks2010_paths, ".shp$")]
fipsstates <- str_remove_all(str_extract(shapefiles, "_\\d\\d_"), "_")
shapefiles_fullpaths <- paste0(censusblocks2010_folder, shapefiles)

library(sf)

#all_us <- do.call(rbind, lapply(shapefiles_fullpaths, st_read ))
changeme <- 1
state_full <- usastatefipscodes[changeme,]
state_fips <- ifelse(nchar(state_full$FIPS.Code) == 1, paste0("0", state_full$FIPS.Code), as.character(state_full$FIPS.Code))
state_abbrev <- 

path <- shapefiles_fullpaths[str_detect(string = shapefiles_fullpaths, pattern = paste0("_", state_fips, "_"))]

bip_approved_join <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/BIP_working/BIP_Approved.RDS")
borders <- read.csv("/project/biocomplexity/sdad/projects_data/usda/bb/original/stateborder.csv") %>% 
  filter(ST1 == "TX" | ST2 == "TX") %>% 
  mutate(ST3 = ifelse(ST1 == "TX", ST2, ST1))

state <- st_read(path)
state_centroid <- st_centroid(state)