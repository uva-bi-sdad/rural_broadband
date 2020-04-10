joinfold <- "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/state_blocks_centroids_bipdist_2011_16/"
completedbip_fcc_blockjoins <- list.files(joinfold)
completedbip_fcc_blockjoins <- paste0(joinfold, completedbip_fcc_blockjoins)

library(sf)
library(dplyr)

#datapath <- "/project/biocomplexity/sdad/projects_data/usda/bb/"

california <- readRDS(completedbip_fcc_blockjoins[2])

# datapath2 <- "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"
# readRDS(paste0(datapath2, "working/state_blocks_centroids_bipdist_2011_16/", "centroids_manip_", state_abbrev, ".RDS"))

california
library(RPostgreSQL)

# connect to postgresql to get data (in rivanna)
conn <- dbConnect(drv = PostgreSQL(), dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = "dtn2ep", password = "dtn2ep")
#user = Sys.getenv("db_userid"), 
#password = Sys.getenv("db_pwd"))

#dbListTables(conn, table_schema="corelogic_sdad")
tables <-  dbListTables(conn) 
tables <- tables[stringr::str_detect(tables, "state8")]
#corelogic <- dbGetQuery(conn, "SELECT fips_code, appr_total_value, acres, assessed_year FROM corelogic_sdad.tax_hist_1 WHERE fips_code = '01011' LIMIT 100")
california_db <- dbGetQuery(conn, "SELECT * FROM corelogic_usda.state8_fixed_join WHERE fips LIKE '06%';")
# disconnect from postgresql
dbDisconnect(conn); rm(conn)

