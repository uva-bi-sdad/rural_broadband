datapath2 <- "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"
alabama_bipdist_nbm11fcc16 <- readRDS(paste0(datapath2, "working/state_blocks_centroids_bipdist_2011_16/", "centroids_manip_", state_abbrev, ".RDS"))


alabama_bipdist_nbm11fcc16 
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
tables <- tables[str_detect(tables, "tax")]
#corelogic <- dbGetQuery(conn, "SELECT fips_code, appr_total_value, acres, assessed_year FROM corelogic_sdad.tax_hist_1 WHERE fips_code = '01011' LIMIT 100")
alabama_corelogic <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_1_01 LIMIT 100")
# disconnect from postgresql
dbDisconnect(conn); rm(conn)



