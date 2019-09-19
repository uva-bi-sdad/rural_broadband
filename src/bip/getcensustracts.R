library(tigris)

virginia_tracts <- tracts(state = 51, year = 2010)

saveRDS(virginia_tracts, "~/git/rural_broadband/data/working/Census Shapefiles/virginia2010censustracts.RDS")
