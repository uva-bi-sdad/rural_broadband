# packakge
library(RPostgreSQL)
library(readr)

# connect to database
con <- dbConnect(PostgreSQL(), 
                 dbname = "sdad",
                 host = "postgis1", 
                 port = 5432, 
                 user = "hc2cc", # <---- user name here 
                 password = "hc2cchc2cc") # <---- password here

rcp1_distances_1 <- dbGetQuery(con, "SELECT * FROM usda_bb.rcp_round1_distance_01_06")
rcp1_distances_2 <- dbGetQuery(con, "SELECT * FROM usda_bb.rcp_round1_distance_08_18")
rcp1_distances_3 <- dbGetQuery(con, "SELECT * FROM usda_bb.rcp_round1_distance_19_28")
rcp1_distances_4 <- dbGetQuery(con, "SELECT * FROM usda_bb.rcp_round1_distance_29_38")
rcp1_distances_5 <- dbGetQuery(con, "SELECT * FROM usda_bb.rcp_round1_distance_39_49")
rcp1_distances_6 <- dbGetQuery(con, "SELECT * FROM usda_bb.rcp_round1_distance_50_78")

rcp1_10_1 <- dbGetQuery(con, "SELECT * FROM usda_bb.rcp_round1_distance_cs2010_01_25")
rcp1_10_2 <- dbGetQuery(con, "SELECT * FROM usda_bb.rcp_round1_distance_cs2010_26_47")

rcp2_distances_2 <- dbGetQuery(con, "SELECT * FROM usda_bb.rcp_round2_distance_31_78")
rcp2_distances_1 <- dbGetQuery(con, "SELECT * FROM usda_bb.rcp_round2_distance_01_30")

rcp2_10_1 <- dbGetQuery(con, "SELECT * FROM usda_bb.rcp_round2_distance_cs2010_01_25")
rcp2_10_2 <- dbGetQuery(con, "SELECT * FROM usda_bb.rcp_round2_distance_cs2010_26_47")

ccg_2013_20_2 <- dbGetQuery(con, "SELECT * FROM usda_bb.ccg_2013_20_distance_31_78")
ccg_2013_20_1 <- dbGetQuery(con, "SELECT * FROM usda_bb.ccg_2013_20_distance_01_30")

ccg1_10_1 <- dbGetQuery(con, "SELECT * FROM usda_bb.ccg_2013_20_distance_cs2010_01_25")
ccg1_10_2 <- dbGetQuery(con, "SELECT * FROM usda_bb.ccg_2013_20_distance_cs2010_26_47")

ccg_2021_2 <- dbGetQuery(con, "SELECT * FROM usda_bb.ccg_2021_distance_31_78")
ccg_2021_1 <- dbGetQuery(con, "SELECT * FROM usda_bb.ccg_2021_distance_01_30")

ccg2_10_1 <- dbGetQuery(con, "SELECT * FROM usda_bb.ccg_2021_distance_cs2010_01_25")
ccg2_10_2 <- dbGetQuery(con, "SELECT * FROM usda_bb.ccg_2021_distance_cs2010_26_47")

dbDisconnect(con)

rcp1_dists20 <- do.call(rbind, c(rcp1_distances_1, rcp1_distances_2, rcp1_distances_3))
rcp1_dists20 <- rbind(rcp1_distances_1, rcp1_distances_2)
rcp1_dists20 <- rbind(rcp1_dists20, rcp1_distances_3)
rcp1_dists20 <- rbind(rcp1_dists20, rcp1_distances_4)
rcp1_dists20 <- rbind(rcp1_dists20, rcp1_distances_5)
rcp1_dists20 <- rbind(rcp1_dists20, rcp1_distances_6)
rcp1_dists20["dist_mi"] <- rcp1_dists20$dist/1609.34 

rcp2_dists20 <- rbind(rcp2_distances_2, rcp2_distances_1)
rcp2_dists20["dist_mi"] <- rcp2_dists20$dist/1609.34 

ccg_2013_20_dists20 <- rbind(ccg_2013_20_1, ccg_2013_20_2)
ccg_2013_20_dists20["dist_mi"] <- ccg_2013_20_dists20$dist/1609.34

ccg_2021_dists20 <- rbind(ccg_2021_1, ccg_2021_2)
ccg_2021_dists20["dist_mi"] <- ccg_2021_dists20$dist/1609.34

rcp1_dists10 <- rbind(rcp1_10_1, rcp1_10_2)
rcp1_dists10["dist_mi"] <- rcp1_dists10$dist/1609.34

rcp2_dists10 <- rbind(rcp2_10_1, rcp2_10_2)
rcp2_dists10["dist_mi"] <- rcp2_dists10$dist/1609.34

ccg_2013_20_dists10 <- rbind(ccg1_10_1, ccg1_10_2)
ccg_2013_20_dists10["dist_mi"] <- ccg_2013_20_dists10$dist/1609.34

ccg_2021_dists10 <- rbind(ccg2_10_1, ccg2_10_2)
ccg_2021_dists10["dist_mi"] <- ccg_2021_dists10$dist/1609.34


# to a Rivanna folder
write_csv(rcp1_dists10, "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/distances_to_blocks_RCP_CCG/census_2010_blk/RCP_round1_distances.csv")
write_csv(rcp2_dists10, "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/distances_to_blocks_RCP_CCG/census_2010_blk/RCP_round2_distances.csv")

write_csv(rcp1_dists20, "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/distances_to_blocks_RCP_CCG/census_2020_blk/RCP_round1_distances.csv")
write_csv(rcp2_dists20, "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/distances_to_blocks_RCP_CCG/census_2020_blk/RCP_round2_distances.csv")

write_csv(ccg_2013_20_dists20, "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/distances_to_blocks_RCP_CCG/census_2020_blk/CCG_2013_20_distances.csv")
write_csv(ccg_2021_dists20, "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/distances_to_blocks_RCP_CCG/census_2020_blk/CCG_2021_distances.csv")

write_csv(ccg_2013_20_dists10, "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/distances_to_blocks_RCP_CCG/census_2010_blk/CCG_2013_20_distances.csv")
write_csv(ccg_2021_dists10, "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/distances_to_blocks_RCP_CCG/census_2010_blk/CCG_2021_distances.csv")


#path = "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/distances_to_blocks_RCP_CCG/census_2020_blk/RCP"