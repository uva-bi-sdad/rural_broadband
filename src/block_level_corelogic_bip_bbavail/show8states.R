joinfold <- "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/state_blocks_centroids_bipdist_2011_16/"
completedbip_fcc_blockjoins <- list.files(joinfold)
completedbip_fcc_blockjoins <- paste0(joinfold, completedbip_fcc_blockjoins)

library(sf)
library(dplyr)
options(tigris_use_cache = FALSE)

datapath <- "/project/biocomplexity/sdad/projects_data/usda/bb/"
borders <- read.csv(paste0(datapath, "original/stateborder.csv")) %>% select(2:3)

datapath2 <- "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"
usa_bip_shapes <- readRDS(paste0(datapath2, "working/BIP_working/BIP_Approved.RDS"))

###### ALABAMA
abbrev <- "AL"
alabama <- readRDS(completedbip_fcc_blockjoins[1])
alabama_500mpoints <- alabama[alabama$bip_dist < 500 & alabama$bip_dist > 0,  ]
alabama_outline <- tigris::counties(abbrev) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))

alabama_neighbors <- borders %>% filter(ST1 == abbrev | ST2 == abbrev) %>% mutate(neighbors = ifelse(ST1 == abbrev, as.character(ST2), as.character(ST1)))
alabama_neighbors <- c(alabama_neighbors$neighbors, abbrev)
alabama_nearest_bips <- usa_bip_shapes[usa_bip_shapes$STUSPS %in% alabama_neighbors,]

plot(alabama_outline$geometry)
plot(alabama_nearest_bips$geometry, add = TRUE, col = "red")
plot(alabama_500mpoints$geometry, add = TRUE, col = "blue", pch = 15)


###### CALIFORNIA
abbrev <- "CA"
california <- readRDS(completedbip_fcc_blockjoins[2])
california_500mpoints <- california[california$bip_dist < 500 & california$bip_dist > 0,  ]
california_outline <- tigris::counties(abbrev) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))

california_neighbors <- borders %>% filter(ST1 == abbrev | ST2 == abbrev) %>% mutate(neighbors = ifelse(ST1 == abbrev, as.character(ST2), as.character(ST1)))
california_neighbors <- c(california_neighbors$neighbors, abbrev)
california_nearest_bips <- usa_bip_shapes[usa_bip_shapes$STUSPS %in% california_neighbors,]

plot(california_outline$geometry)
plot(california_nearest_bips$geometry, add = TRUE, col = "red")
plot(california_500mpoints$geometry, add = TRUE, col = "blue", pch = 15)


###### COLORADO
abbrev <- "CO"
colorado <- readRDS(completedbip_fcc_blockjoins[3])
colorado_500mpoints <- colorado[colorado$bip_dist < 500 & colorado$bip_dist > 0,  ]
colorado_outline <- tigris::counties(abbrev) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))

colorado_neighbors <- borders %>% filter(ST1 == abbrev | ST2 == abbrev) %>% mutate(neighbors = ifelse(ST1 == abbrev, as.character(ST2), as.character(ST1)))
colorado_neighbors <- c(colorado_neighbors$neighbors, abbrev)
colorado_nearest_bips <- usa_bip_shapes[usa_bip_shapes$STUSPS %in% colorado_neighbors,]

plot(colorado_outline$geometry)
plot(colorado_nearest_bips$geometry, add = TRUE, col = "red")
plot(colorado_500mpoints$geometry, add = TRUE, col = "blue", pch = 15)




florida <- readRDS(completedbip_fcc_blockjoins[4])
georgia <- readRDS(completedbip_fcc_blockjoins[5])
ncarolina <- readRDS(completedbip_fcc_blockjoins[6])
ohio <- readRDS(completedbip_fcc_blockjoins[7])
pennsylvania <- readRDS(completedbip_fcc_blockjoins[7])
texas <- readRDS(completedbip_fcc_blockjoins[9])



california_outline <- tigris::counties("CA") %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))
colorado_outline <- tigris::counties("CO") %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))
florida_outline <- tigris::counties("FL") %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))
state_outline <- tigris::counties(state_abbrev) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))
state_outline <- tigris::counties(state_abbrev) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))
state_outline <- tigris::counties(state_abbrev) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))
state_outline <- tigris::counties(state_abbrev) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))
