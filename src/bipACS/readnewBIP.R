# read new BIP data, subset to complete records
library(sf)
library(dplyr)
library(data.table)

setwd("~/git/rural_broadband/src/bipACS/")

newbip <- st_read(dsn="200409_BIP_ArcGIS",layer="200409_BIP_ServAr_ID")
newbip_data <- fread("tblProject.csv")

newbip_join <- newbip %>% left_join(newbip_data, by="ProjectID") %>% filter(`Has Project Shapefile`=="Yes")

newbip_union <- newbip_join %>% dplyr::select(ProjectID, geometry) %>%
  aggregate(by=list(newbip_join$ProjectID), first) %>% dplyr::select(-Group.1)

newbip_all <- newbip_join %>% dplyr::select(ProjectID, geometry) %>%
  aggregate(by=list(rep(1,nrow(newbip_join))), FUN=first) %>% dplyr::select(-Group.1)

save(list=c("newbip_union", "newbip_all"), file="BIP_New.rds")
