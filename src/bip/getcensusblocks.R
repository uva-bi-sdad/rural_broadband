library(tigris)

virginia_blocks <- blocks(state = 51, year = 2010)

saveRDS(virginia_blocks, "~/git/rural_broadband/data/working/Census Shapefiles/virginia2010censusblocks.RDS")
