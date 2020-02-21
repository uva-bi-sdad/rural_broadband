---
title: "Untitled"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

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

completedbip_fcc_blockjoins
```

## Alaska

```{r cars}
abbrev <- "AR"
state <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/state_blocks_centroids_bipdist_2011_16/centroids_manip_AR.RDS")
state_500mpoints <- state[state$bip_dist < 500 & state$bip_dist > 0,  ]
state_outline <- tigris::counties(abbrev) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))

state_neighbors <- borders %>% filter(ST1 == abbrev | ST2 == abbrev) %>% mutate(neighbors = ifelse(ST1 == abbrev, as.character(ST2), as.character(ST1)))
state_neighbors <- c(state_neighbors$neighbors, abbrev)
state_nearest_bips <- usa_bip_shapes[usa_bip_shapes$STUSPS %in% state_neighbors,]

plot(state_outline$geometry)
plot(state_nearest_bips$geometry, add = TRUE, col = "red")
plot(state_500mpoints$geometry, add = TRUE, col = "blue", pch = 15)

```

## Arkansas

```{r cars}
abbrev <- "AR"
state <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/state_blocks_centroids_bipdist_2011_16/centroids_manip_AR.RDS")
state_500mpoints <- state[state$bip_dist < 500 & state$bip_dist > 0,  ]
state_outline <- tigris::counties(abbrev) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))

state_neighbors <- borders %>% filter(ST1 == abbrev | ST2 == abbrev) %>% mutate(neighbors = ifelse(ST1 == abbrev, as.character(ST2), as.character(ST1)))
state_neighbors <- c(state_neighbors$neighbors, abbrev)
state_nearest_bips <- usa_bip_shapes[usa_bip_shapes$STUSPS %in% state_neighbors,]

plot(state_outline$geometry)
plot(state_nearest_bips$geometry, add = TRUE, col = "red")
plot(state_500mpoints$geometry, add = TRUE, col = "blue", pch = 15)

```

## Arizona

```{r pressure, echo=FALSE}
abbrev <- "AZ"
state <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/state_blocks_centroids_bipdist_2011_16/centroids_manip_AZ.RDS")
state_500mpoints <- state[state$bip_dist < 500 & state$bip_dist > 0,  ]
state_outline <- tigris::counties(abbrev) %>% st_as_sf() %>% st_transform(st_crs(usa_bip_shapes))

state_neighbors <- borders %>% filter(ST1 == abbrev | ST2 == abbrev) %>% mutate(neighbors = ifelse(ST1 == abbrev, as.character(ST2), as.character(ST1)))
state_neighbors <- c(state_neighbors$neighbors, abbrev)
state_nearest_bips <- usa_bip_shapes[usa_bip_shapes$STUSPS %in% state_neighbors,]

plot(state_outline$geometry)
plot(state_nearest_bips$geometry, add = TRUE, col = "red")
plot(state_500mpoints$geometry, add = TRUE, col = "blue", pch = 15)
```


