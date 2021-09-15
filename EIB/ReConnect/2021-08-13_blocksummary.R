# compute area share of Census blocks for ReConnect PFSAs

library(data.table)
library(sf)
library(dplyr)
library(tidyverse)
library(readxl)
library(tigris)

setwd("~/git/rural_broadband/EIB/ReConnect/")

# read in ReConnect shapefiles
project_data <- read_excel("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_RC_Shapefiles_August2021/Round 1 and 2 Submitted ReConnect Applications_8-6-2021.xlsx")

round1 <- st_read("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_RC_Shapefiles_August2021/ReConnect_984_PFSAs/","USDARD_PFSA_ReConnect",
                  int64_as_string = TRUE)
round2 <- st_read("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/RUS_RC_Shapefiles_August2021/ReConnect_Round_2_Final/","ReConnect_Round_2_Final",
                  int64_as_string = TRUE)

# the PFSA identifiers in each shapefile (Applicat_1, 146 total, Applicat_2, 172 total)
# match perfectly with the excel file (Applicat_2, 318 total)
pfsa1 <- unique(round1$Applicat_1)
pfsa2 <- unique(round2$Applicat_2)
pfsa <- unique(project_data$Applicat_2)


# get US counties shapefile
counties <- st_as_sf(counties())

# load shapefiles for all Census blocks
blockgeo <- readRDS("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/censusblocks_shape.RDS")
blockgeo$GEOID10 <- as.character(blockgeo$GEOID10)

# add Puerto Rico Census blocks to blockgeo (for one FPSA)
pr_blocks <- blocks(state="72")
pr_blocks_sf <- st_as_sf(pr_blocks)
st_crs(pr_blocks_sf) <- st_crs(blockgeo)
pr_blocks_df <- pr_blocks_sf %>% select(GEOID10,geometry)
blockgeo <- rbind(blockgeo, pr_blocks_df)


# --------------------------------------------------
# loop over pfsa1, pfsa2
# select all shapes in the PFSA; then intersect with Census blocks to get %area of each block

block_intersect_round1 <- list()

# loop across all IDs in the first round shapefile
for(i in 1:length(pfsa1)){
  currID <- pfsa1[i]
  currshape <- round1[round1$Applicat_1 %in% currID,]
  currshape <- st_buffer( st_union(st_buffer(currshape,0)), 0 )
  currshape <- st_transform(currshape,st_crs(counties))
  
  # first get counties where there is an intersection and subset to these counties
  county_intersect <- st_intersects(counties,currshape,sparse=T)
  county_intersect <- counties$GEOID[ sapply(county_intersect,length)>0 ]
  blockgeo_sub <- blockgeo[ substr(blockgeo$GEOID10,1,5) %in% county_intersect, ]
  
  # then get the blocks that actually intersect with currshape and subset to these blocks
  blocks_intersect <- st_intersects(blockgeo_sub,currshape,sparse=T)
  blocks_intersect <- blockgeo_sub$GEOID10[ sapply(blocks_intersect,length)>0]
  blockgeo_sub2 <- blockgeo_sub[ blockgeo_sub$GEOID10 %in% blocks_intersect, ]

  prop_area <- rep(NA,nrow(blockgeo_sub2))
  # compute proportion area of intersection with each block in a loop
  for(j in 1:nrow(blockgeo_sub2)){
    currblock <- blockgeo_sub2[j,]
    prop_area[j] <- st_area(st_intersection(currblock,currshape))/st_area(currblock)
  }
  
  # add to data frame of all aera intersections
  block_intersect_round1[[i]] <- data.frame(pfsa=currID,
                                            GEOID10=blockgeo_sub2$GEOID10,
                                            prop_area=prop_area)
}



block_intersect_round1_df <- rbindlist(block_intersect_round1)
saveRDS(block_intersect_round1_df,file="block_intersect_round1_df.RDS")


# block_intersect_round1_df <- readRDS("block_intersect_round1_df.RDS")
#hist( log10( sapply(block_intersect_round1,nrow) ), xlab="Log Census Blocks", ylab="PFSAs", main="Number of Blocks in Round 1 PFSAs" )
#hist( block_intersect_round1[[1]]$prop_area, main="Share of Area Covered by\n Census Blocks in PFSA '101000016'", xlab="Share of Area", ylab="Census Blocks")


# loop across all IDs in the second round shapefile
block_intersect_round2 <- list()

# loop across all IDs in the first round shapefile
for(i in 1:length(pfsa2)){
  currID <- pfsa2[i]
  currshape <- round2[round2$Applicat_2 %in% currID,]
  currshape <- st_union(st_buffer(currshape,0))
  currshape <- st_buffer( st_transform(currshape,st_crs(counties)), 0 )
  
  # first get counties where there is an intersection and subset to these counties
  county_intersect <- st_intersects(counties,currshape,sparse=T)
  county_intersect <- counties$GEOID[ sapply(county_intersect,length)>0 ]
  blockgeo_sub <- blockgeo[ substr(blockgeo$GEOID10,1,5) %in% county_intersect, ]
  
  # then get the blocks that actually intersect with currshape and subset to these blocks
  blocks_intersect <- st_intersects(blockgeo_sub,currshape,sparse=T)
  blocks_intersect <- blockgeo_sub$GEOID10[ sapply(blocks_intersect,length)>0]
  blockgeo_sub2 <- blockgeo_sub[ blockgeo_sub$GEOID10 %in% blocks_intersect, ]
  
  prop_area <- rep(NA,nrow(blockgeo_sub2))
  # compute proportion area of intersection with each block in a loop
  for(j in 1:nrow(blockgeo_sub2)){
    currblock <- blockgeo_sub2[j,]
    intersect_area <- st_area(st_intersection(currblock,currshape))
    if(length(intersect_area)==0){
      prop_area[j] <- 0
    } else{
      prop_area[j] <- intersect_area/st_area(currblock)
    }
  }
  
  # add to data frame of all aera intersections
  block_intersect_round2[[i]] <- data.frame(pfsa=currID,
                                            GEOID10=blockgeo_sub2$GEOID10,
                                            prop_area=prop_area)
}

# OMIT two of the rejected applications in round 2

# skipping i=133; "101000464" this has an error when intersect_area is empty...
#currshape <- round2[round2$Applicat_2 %in% pfsa2[133],]
#plot(currshape[1,]$geometry,main="PFSA 101000464")

# error in i=169: this FPSA is in Peurto Rico, shapefiles not in "blockgeo"
# to fix I added Puerto Rico blocks from tigris to blockgeo
#currshape <- round2[round2$Applicat_2 %in% pfsa2[169],]
#plot(currshape[1,]$geometry,main="PFSA 101000463")
# saveRDS(block_intersect_round2,file="block_intersect_round2.RDS")

block_intersect_round2_df <- rbindlist(block_intersect_round2)
saveRDS(block_intersect_round2_df,file="block_intersect_round2_df.RDS")

# combine intersection into one data frame and save
block_intersect_df <- rbind( block_intersect_round1_df, block_intersect_round2_df )
saveRDS(block_intersect_df,file="block_intersect_df.RDS")


# --------------------------------------------------
# add population, race, ethnicity by Census block
block_intersect_df <- readRDS("block_intersect_df.RDS")


library(tidycensus)

census_api_key("853b2a1e71aa0aff0f3db966545e8898c73f0772")

census_vars <-
  c("P001001",
    "P003001",
    "P003002",
    "P003003",
    "P003004",
    "P003005",
    "P003006",
    "P003007",
    "P003008",
    "P004001",
    "P004002",
    "P004003")

varnames <-
  c("Total	TOTAL POPULATION",
    "Total	RACE",
    "Total!!White alone RACE",
    "Total!!Black or African American alone RACE",
    "Total!!American Indian and Alaska Native alone RACE",
    "Total!!Asian alone	RACE",
    "Total!!Native Hawaiian and Other Pacific Islander alone RACE",
    "Total!!Some Other Race alone RACE",
    "Total!!Two or More Race RACE",
    "Total	HISPANIC OR LATINO ORIGIN	",
    "Total!!Not Hispanic or Latino	HISPANIC OR LATINO ORIGIN",
    "Total!!Hispanic or Latino	HISPANIC OR LATINO ORIGIN")

state_fips <- c( unique(state.fips$fips), 2, 15 )
state_fips <- formatC(state_fips,width=2,flag=0)


# loop over states to get estimates for all US Census blocks
blockdat <-get_decennial(geography = "block",
                         state = state_fips[1],
                         variables = census_vars,
                         output = "wide",
                         year = 2010) 

for(j in 2:length(state_fips)){
  blockdat <- rbind(blockdat, get_decennial(geography = "block",
                                            state = state_fips[j],
                                            variables = census_vars,
                                            output = "wide",
                                            year = 2010)
  )
}

# join to block_intersect_df by GEOID10 to GEOID
block_intersect_df2 <- block_intersect_df %>% left_join(blockdat, by=c("GEOID10"="GEOID"))

# join application data by PFSA
block_intersect_df3 <- block_intersect_df2 %>% left_join(project_data, by=c("pfsa"="Applicat_2"))
saveRDS(block_intersect_df3,file="block_intersect_final.RDS")


# --------------------------------------------------

block_intersect_final <- readRDS("block_intersect_final.RDS")
# compute race/ethnicity (as %population) by
#   technology (fiber, fiber+other, nonfiber),
#   application status (approved/rejected/withdrawn, "second chance" approved),
#   year (round 1 2019, round 2 2020)

# compare to national race/ethnicity share (again using decennial Census)


# --------------------------------------------------

# new file: ACS 1-year tract-level estimates + MOE by technology, status, year
#   downcast using decennial Census block population to weight population within each tract
#   (produce estimates with and without the decennial pop. block-weighting and compare them)






# --------------------------------------------------
# note: we don't have Census block shapefiles in the SDAD database
#get_db_conn <-
#  function(db_name = "sdad",
#           db_host = "postgis1",
#           db_port = "5432",
#           db_user = Sys.getenv("db_usr"),
#           db_pass = Sys.getenv("db_pwd")) {
#    RPostgreSQL::dbConnect(
#      drv = RPostgreSQL::PostgreSQL(),
#      dbname = db_name,
#      host = db_host,
#      port = db_port,
#      user = db_user,
#      password = db_pass
#    )
#  }
#
#list_db_schemas <- function(db_con) {
#  result <- DBI::dbGetQuery(db_con, "select schema_name from information_schema.schemata")
#  DBI::dbDisconnect(db_con)
#  return(result)
#}
#
#list_schema_tables <- function(db_con, db_schema) {
#  result <- DBI::dbGetQuery(db_con, paste0("SELECT table_name FROM information_schema.tables
#                                           WHERE table_schema='", db_schema, "'"))
#  DBI::dbDisconnect(db_con)
#  return(result)
#}
#
#list_table_columns <- function(db_con, db_schema, db_table) {
#  result <- DBI::dbGetQuery(db_con, paste0("SELECT table_schema, table_name, column_name, data_type
#                                 FROM information_schema.columns
#                                 WHERE table_schema = '", db_schema, "'",
#                                           " AND table_name = '", db_table, "'"))
#  DBI::dbDisconnect(db_con)
#  return(result)
#}
#
#con <- get_db_conn()
#list_db_schemas(con)
#
#con <- get_db_conn()
#list_schema_tables(con, "gis_census_cb")






# then get blocks within those counties and compute area of intersection for each block
#blocks_intersect <- character(0)
#for(j in 1:length(county_intersect)){
#  blocks <- st_as_sf( blocks(county_intersect[j]) )
#  block_intersect <- st_intersects(blocks,currshape,sparse=T)
#  blocks_intersect <- c(blocks_intersect, tracts$GEOID[ sapply(block_intersect,length)==1 ])
#}

# problem: "blocks" downloads by state
#blocks <- st_as_sf( blocks(state=substr(county_intersect[1],1,2),
#                           county=substr(county_intersect[1],3,5) ) )
#block_intersect <- st_intersects(blocks,currshape,sparse=T)

# then get tracts within those blocks where there is an intersection
#tracts_intersect <- character(0)
#for(j in 1:length(county_intersect)){
#  tracts <- st_as_sf( tracts(county_intersect[j]) )
#  tract_intersect <- st_intersects(tracts,currshape,sparse=T)
#  tracts_intersect <- c(tracts_intersect, tracts$GEOID[ sapply(tract_intersect,length)==1 ])
#}
