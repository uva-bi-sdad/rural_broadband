# read in the CoreLogic data

# set up environmental vars (DON'T push actual values to github)
Sys.setenv(db_usr = "dtn2ep")
Sys.setenv(db_pwd = "dtn2ep")

get_db_conn <-
  function(db_name = "sdad",
           db_host = "postgis1",
           db_port = "5432",
           db_user = Sys.getenv("db_usr"), # requires you to setup environmental vars (above)
           db_pass = Sys.getenv("db_pwd")) {
    RPostgreSQL::dbConnect(
      drv = RPostgreSQL::PostgreSQL(),
      dbname = db_name,
      host = db_host,
      port = db_port,
      user = db_user,
      password = db_pass
    )
  }

list_db_schemas <- function(db_con) {
  result <- DBI::dbGetQuery(db_con, "select schema_name from information_schema.schemata")
  DBI::dbDisconnect(db_con)
  return(result)
}

list_schema_tables <- function(db_con, db_schema) {
  result <- DBI::dbGetQuery(db_con, paste0("SELECT table_name FROM information_schema.tables
                                           WHERE table_schema='", db_schema, "'"))
  DBI::dbDisconnect(db_con)
  return(result)
}

list_table_columns <- function(db_con, db_schema, db_table) {
  result <- DBI::dbGetQuery(db_con, paste0("SELECT table_schema, table_name, column_name, data_type
                                           FROM information_schema.columns
                                           WHERE table_schema = '", db_schema, "'",
                                           " AND table_name = '", db_table, "'"))
  DBI::dbDisconnect(db_con)
  return(result)
}

# EXAMPLES ----
# List Schemas
con <- get_db_conn()
list_db_schemas(con)
# you should see CoreLogic data in this list

# List Tables in a Schema
con <- get_db_conn()
list_schema_tables(con, "corelogic_prop_tax_2003_2019")

# List Columns in a Table
con <- get_db_conn()
list_table_columns(con, "corelogic_prop_tax_2003_2019", "va_pl_prop_tax")

# Get Some Data
con <- get_db_conn()
va_subset <- DBI::dbGetQuery(con, 'SELECT * FROM corelogic_prop_tax_2003_2019.va_pl_prop_tax LIMIT 200')
class(va_subset$fips_code)

# look at variable names
names(va_subset)

con <- get_db_conn()

ffx <- DBI::dbGetQuery(con, statement = paste(
  #"SELECT fips_code, unformatted_apn, formatted_apn, census_tract, lot_number, township, property_indicator, assd_total_value, mkt_total_value, tax_amount, tax_year, assessed_year, acres, land_square_footage, building__square_feet, living_square_feet, year_built, effective_year_built, bedrooms, total_baths, full_baths, half_baths",
  "SELECT  situs_house_number, situs_street_name, situs_mode, situs_city, situs_state, situs_zip_code",
  # "SELECT tax_year, assessed_year", 
  "FROM corelogic_prop_tax_2003_2019.va_pl_prop_tax",
  "WHERE fips_code='51059'"))

ffx$addr <- paste0(ffx$situs_house_number, " ",
                   ffx$situs_street_name, " ",
                   ffx$situs_mode, ", ", 
                   ffx$situs_city, ", ",  
                   ffx$situs_state, " ",
                   ffx$situs_zip_code)

library(tidygeocoder)
library(dplyr)

test_1 <- head(ffx, 1000)

ffx %>% nrow()
test <- tidygeocoder::geocode(test_1, addr, "census")
table(is.na(test$lat))

# DBI::dbDisconnect(con)