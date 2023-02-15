# packages
library(readr)
library(readxl)
library(tidyverse)
library(DBI)
library(RPostgreSQL)
library(stringr)
library(fedmatch)
library(sf)
library(gridExtra)

# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

# all old CL data
# load(paste0(path, "BIP_linear_data/usda_er_bb_data1 (1).rdata"))

# old CL in the 20mi radius
old_CL <- readRDS(paste0(path, "BIP_linear_data/housing_BIP_clean_011122.RDS"))

# number of unique rusids selected 
bip_ids <- unique(old_CL$rusid)

# table number of inside and out
table(old_CL$bip, old_CL$rusid)

# slect a project area
# BIP VA1108 GA1109 TX1120 MT1104-B40 MO1108-A40 WA1106-A40 VT1103-A40 OK1110-D40
old_bip <- old_CL %>% filter(rusid == "OK1110-D40")

# bip shapes
load(paste0(path, "BIP_linear_data/BIP_New.rds"))
# VA1108-A39 GA1109-C40 TX1120-A40
shape <- newbip_union[newbip_union$ProjectID == "OK1110-D40",] 

# project in meters to compute distance, lat/lon to check against CoreLogic
# Albers: 5070
# WGS84: 4326
shape <- st_transform(shape, 5070)

# 20 mi outer boundary
outer_shape <- st_difference(st_buffer(shape, dist=1609.34*20), 
                             st_buffer(shape,0) )

# plots
shape <- st_transform(shape, 4326)
outer_shape <- st_transform(outer_shape, 4326)

ggplot() +
  geom_sf(data = shape$geometry, col = "black") +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA) +
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) 
ggsave("BIP_linear_reg/inside_out_OK1.png",height=4.5,width=6)

ggplot() +
  geom_sf(data = shape$geometry, col = "black") +
  geom_sf(data = outer_shape$geometry, col="black", fill=NA) +
  geom_point(data=old_bip[old_bip$bip == 1,], 
             aes(x=property_centroid_latitude, y=property_centroid_longitude), 
             color="grey60", size=0.5) +
  geom_point(data=old_bip[old_bip$bip == 0,], 
             aes(x=property_centroid_latitude, y=property_centroid_longitude), 
             color="grey90", size=0.5) +
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) 
ggsave("BIP_linear_reg/inside_out_OKbw.png",height=4.5,width=6)


