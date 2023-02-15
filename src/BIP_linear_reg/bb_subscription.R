# Description

# packages
library(maps)
library(sf)
library(sp)
library(data.table)
library(dplyr)
library(rgeos)

##########################################################################################################
# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/BIP_linear_data/"

# BIP shapes
load(paste0(path, "BIP_New.rds"))

# census tract shapes
state_fips <- c( unique(state.fips$fips), 2, 15 )
state_fips <- formatC(state_fips,width=2,flag=0)
tractgeo_list <- list()
for (state in state_fips){
  state_tract <- tigris::tracts(state)
  #tractgeo <- append(tractgeo, tigris::tracts(state))}
  i <- which(state_fips == state)
  tractgeo_list[[i]] <- state_tract }
tractgeo_df <- do.call(rbind, tractgeo_list)
tractgeo <- tractgeo_df %>% select(GEOID,  geometry)
# remove big files from the env
rm(tractgeo_df, tractgeo_list)


tracts_us <- st_transform(tractgeo, st_crs(newbip_union))
tracts_us$AREA <- as.numeric( st_area(tracts_us))


inter_tracts <- st_intersection(newbip_union,tracts_us)

plot(tracts_us$geometry[substr(tracts_us$GEOID,1,2)=="51"], axes = TRUE)
plot(newbip_union$geometry, add = TRUE, col="green")
plot(inter_tracts$geometry[substr(inter_tracts$GEOID,1,2)=="51"], add = TRUE, col = 'red')

# add in areas in m2
attArea <- inter_tracts %>% 
  mutate(area = st_area(.) %>% as.numeric())
prop_area <- attArea %>% 
  as_tibble() %>% 
  group_by(GEOID, ProjectID) %>% 
  summarize(area = sum(area)/AREA * 100)


# tracts with no intersection
diff_tracts <- tracts_us %>% filter(!(tracts_us$GEOID %in% prop_area$GEOID))
# tracts within 10 mi programs
# USA geographies shapes 
usa_shape <- raster::getData("GADM",country="USA", level=1)
# create a 10 mi buffer arounf program areas
proj_crs <- CRS(" +proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")
newbip_all <- sf::st_transform(newbip_union,crs=sf::st_crs(usa_shape))
dat.p <- Polygon(newbip_all$geometry[1])
dat.ps <- Polygons(list(dat.p), 1)
dat.sps <- SpatialPolygons(list(dat.ps))
dat.sps.enlarged <- rgeos::gBuffer(newbip_all$geometry[1], byid = T,width = 0.1) 
##########################################################################################################

# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/BIP_linear_data/"

dataset <- read_csv(paste0(path, "bip_all_projects.csv"))
# tracts
dataset['geoid_tr'] <- substr(dataset$geoid_blk,1,11)
dat_tracts <- dataset %>% group_by(geoid_tr) %>% 
  summarise(BIPsum = sum(bip),
            prop_sum = n())
dat_tracts <- dat_tracts %>% 
  mutate(outside = case_when(BIPsum==0 ~ 0, # all properties are outside
                                          BIPsum>0 & BIPsum==prop_sum ~ 1, # all properties are outside
                                          BIPsum>0 & BIPsum<prop_sum ~ 2)) # partially inside 


# get ACS broadband vars
# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

# state FIPS 
state_fips <- c( unique(state.fips$fips), 2, 15 )
state_fips <- formatC(state_fips,width=2,flag=0)

# acs_vars <- c("B28002_004", # BB of anytype 
#              "B28002_007") # BB subscription such as cable, fiber optic or DSL (no Satelite/wireless)

acs_vars <- c("B28011_004") # BB subscription such as cable, fiber optic or DSL (no Satelite/wireless)
acs_vars <- c("B28011_004") 

tmp <- list()
for(i in 1:length(state_fips)){
  tmp[[i]] <- get_acs(geography="tract",
                      state=state_fips[i],
                      variables=acs_vars,
                      year=2010,
                      cache_table=TRUE,
                      output="wide")}
