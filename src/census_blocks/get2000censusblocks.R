library(tigris)

### DOWNLOAD DATA
## 1- Get all FIPS CODES
## 2- USE TIGRIS WITH YEAR SPECIED TO DOWNLOAD BLOCKS
## 3- ITERATE UPON FAILURE LOL..
## 4- COMMENT OUT!! 
# usastatefipscodes <- read.csv("usastatefipscodes.csv")
# blocks00wehave <- list.files("censusblocks/blocks_tigris2000/")
# missingblocks00 <- setdiff(usastatefipscodes$FIPS.Code, as.numeric(stringr::str_remove_all(blocks00wehave, "tigris2000_|.RDS")))
# missingblocks00
# for (i in 2:length(missingblocks00)) {
#   state_blocks <- tigris::blocks(state = missingblocks00[i], year = 2000)
#   saveRDS(state_blocks, file = paste0("censusblocks/blocks_tigris2000/tigris2000_", missingblocks00[i],  ".RDS"))
# }
# 
# rm(state_blocks)
# rm(i)
# # 48 didn't work in the loop for some reason
# state_blocks <- tigris::blocks(state = 48, year = 2000)
# saveRDS(state_blocks, file = paste0("censusblocks/blocks_tigris2000/tigris2000_", "48",  ".RDS"))


## Gonna assume that we got the correct file sizes because of TIGRIS API? 