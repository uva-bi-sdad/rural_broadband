#library(haven)
#sasdata <- read_dta("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deed_tax_merge_all.dta")

library(dplyr)
library(sf)

top1000merged <- read.csv("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deed_tax_merge_top1000.csv")

tryme <- top1000merged %>% select(apnparcelnumberunformatted, state, propertylevellatitude, propertylevellongitude)

tryme <- st_as_sf(tryme, coords = c("propertylevellatitude", "propertylevellongitude")) 

bip_approved_join <- readRDS("~/git/rural_broadband/data/working/BIP_working/BIP_Approved.RDS") 

bip_approved_join_comb <- st_union(bip_approved_join)

tryme$bip <- st_within(tryme, bip_approved_join_comb) %>% lengths > 0
tryme %>% filter(bip == TRUE)

