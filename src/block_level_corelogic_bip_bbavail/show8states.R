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


###### FLORIDA
abbrev <- "FL"
florida <- readRDS(completedbip_fcc_blockjoins[4])
florida_500mpoints <- florida[florida$bip_dist < 500 & florida$bip_dist > 0,  ]
florida_outline <- tigris::counties(abbrev) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))

florida_neighbors <- borders %>% filter(ST1 == abbrev | ST2 == abbrev) %>% mutate(neighbors = ifelse(ST1 == abbrev, as.character(ST2), as.character(ST1)))
florida_neighbors <- c(florida_neighbors$neighbors, abbrev)
florida_nearest_bips <- usa_bip_shapes[usa_bip_shapes$STUSPS %in% florida_neighbors,]

plot(florida_outline$geometry)
plot(florida_nearest_bips$geometry, add = TRUE, col = "red")
plot(florida_500mpoints$geometry, add = TRUE, col = "blue", pch = 15)


###### georgia
abbrev <- "GA"
georgia <- readRDS(completedbip_fcc_blockjoins[5])
georgia_500mpoints <- georgia[georgia$bip_dist < 500 & georgia$bip_dist > 0,  ]
georgia_outline <- tigris::counties(abbrev) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))

georgia_neighbors <- borders %>% filter(ST1 == abbrev | ST2 == abbrev) %>% mutate(neighbors = ifelse(ST1 == abbrev, as.character(ST2), as.character(ST1)))
georgia_neighbors <- c(georgia_neighbors$neighbors, abbrev)
georgia_nearest_bips <- usa_bip_shapes[usa_bip_shapes$STUSPS %in% georgia_neighbors,]

plot(georgia_outline$geometry)
plot(georgia_nearest_bips$geometry, add = TRUE, col = "red")
plot(georgia_500mpoints$geometry, add = TRUE, col = "blue", pch = 15)


###### NORTH CAROLINA
abbrev <- "NC"
ncarolina <- readRDS(completedbip_fcc_blockjoins[6])
ncarolina_500mpoints <- ncarolina[ncarolina$bip_dist < 500 & ncarolina$bip_dist > 0,  ]
ncarolina_outline <- tigris::counties(abbrev) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))

ncarolina_neighbors <- borders %>% filter(ST1 == abbrev | ST2 == abbrev) %>% mutate(neighbors = ifelse(ST1 == abbrev, as.character(ST2), as.character(ST1)))
ncarolina_neighbors <- c(ncarolina_neighbors$neighbors, abbrev)
ncarolina_nearest_bips <- usa_bip_shapes[usa_bip_shapes$STUSPS %in% ncarolina_neighbors,]

plot(ncarolina_outline$geometry)
plot(ncarolina_nearest_bips$geometry, add = TRUE, col = "red")
plot(ncarolina_500mpoints$geometry, add = TRUE, col = "blue", pch = 15)

###### OHIO
abbrev <- "OH"
ohio <- readRDS(completedbip_fcc_blockjoins[7])
ohio_500mpoints <- ohio[ohio$bip_dist < 500 & ohio$bip_dist > 0,  ]
ohio_outline <- tigris::counties(abbrev) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))

ohio_neighbors <- borders %>% filter(ST1 == abbrev | ST2 == abbrev) %>% mutate(neighbors = ifelse(ST1 == abbrev, as.character(ST2), as.character(ST1)))
ohio_neighbors <- c(ohio_neighbors$neighbors, abbrev)
ohio_nearest_bips <- usa_bip_shapes[usa_bip_shapes$STUSPS %in% ohio_neighbors,]

plot(ohio_outline$geometry)
plot(ohio_nearest_bips$geometry, add = TRUE, col = "red")
plot(ohio_500mpoints$geometry, add = TRUE, col = "blue", pch = 15)


###### PENNSYLVANIA
abbrev <- "PA"
pennsylvania <- readRDS(completedbip_fcc_blockjoins[8])
pennsylvania_500mpoints <- pennsylvania[pennsylvania$bip_dist < 500 & pennsylvania$bip_dist > 0,  ]
pennsylvania_outline <- tigris::counties(abbrev) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))

pennsylvania_neighbors <- borders %>% filter(ST1 == abbrev | ST2 == abbrev) %>% mutate(neighbors = ifelse(ST1 == abbrev, as.character(ST2), as.character(ST1)))
pennsylvania_neighbors <- c(pennsylvania_neighbors$neighbors, abbrev)
pennsylvania_nearest_bips <- usa_bip_shapes[usa_bip_shapes$STUSPS %in% pennsylvania_neighbors,]

plot(pennsylvania_outline$geometry)
plot(pennsylvania_nearest_bips$geometry, add = TRUE, col = "red")
plot(pennsylvania_500mpoints$geometry, add = TRUE, col = "blue", pch = 15)
 

###### TEXAS
abbrev <- "TX"
texas <- readRDS(completedbip_fcc_blockjoins[9])
texas_500mpoints <- texas[texas$bip_dist < 500 & texas$bip_dist > 0,  ]
texas_outline <- tigris::counties(abbrev) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))

texas_neighbors <- borders %>% filter(ST1 == abbrev | ST2 == abbrev) %>% mutate(neighbors = ifelse(ST1 == abbrev, as.character(ST2), as.character(ST1)))
texas_neighbors <- c(texas_neighbors$neighbors, abbrev)
texas_nearest_bips <- usa_bip_shapes[usa_bip_shapes$STUSPS %in% texas_neighbors,]

plot(texas_outline$geometry)
plot(texas_nearest_bips$geometry, add = TRUE, col = "red")
plot(texas_500mpoints$geometry, add = TRUE, col = "blue", pch = 15)




