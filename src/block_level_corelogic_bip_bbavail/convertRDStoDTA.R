library(dplyr)
bip_block_coverage_fold <- "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/state_blocks_centroids_bipdist_2011_16/"
bip_block_coverage_fold_dta <- "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/state_blocks_centroids_bipdist_2011_16/DTA files/"
bip_block_coverage_files <- list.files(bip_block_coverage_fold)

bip_block_coverage_paths <- paste0(bip_block_coverage_fold, bip_block_coverage_files)

length(bip_block_coverage_paths) # 55

for (i in 37:length(bip_block_coverage_paths)) {
  file <- readRDS(bip_block_coverage_paths[i])
  file_few <- file %>% select(1,2,3,4,5,6,16:20) 
  haven::write_dta(file_few, paste0(bip_block_coverage_fold_dta, stringr::str_remove(bip_block_coverage_files[i], ".RDS"), ".dta"))
}

rm(i)

zip(zipfile = "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/state_blocks_centroids_bipdist_2011_16/DTA files/zippedcentroidBIPdistBBcoverage.zip", 
    files = paste0(bip_block_coverage_fold_dta, list.files(bip_block_coverage_fold_dta)) )
