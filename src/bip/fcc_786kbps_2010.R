######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### FCC BB Providers    ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 

#'%!in%' <- function(x,y)!('%in%'(x,y))

library(data.table)
library(dplyr)
library(maditr)
setwd("~/git/dspg19broadband/data/working/FCC/")

fcc2010_small <- fread("NBM-CBLOCK-CSV-December-2010.csv",
                       fill=TRUE) %>%
  dt_filter(MAXADDOWN %in% 3:11) %>% distinct()

saveRDS(fcc2010_small, "~/git/rural_broadband/data/working/FCC_working/FCC_SM_FIPSwBBPROVIDERS.RDS")

fcc2010_large1 <- fread("NBM-Address-Street-CSV-December-2010.csv",fill=TRUE, select = c(5, 10, 13, 14)) # breaks at line 7739714
fcc2010_large2 <- fread("NBM-Address-Street-CSV-December-2010.csv",fill=TRUE,skip=7739715, select = c(5, 10, 13, 14))
fcc2010_large2 <- fcc2010_large2 %>%
  setNames(nm = names(fcc2010_large1))
names(fcc2010_large2)
names(fcc2010_large1)
fcc2010_large <- rbind(fcc2010_large1,fcc2010_large2)
rm(fcc2010_large1); rm(fcc2010_large2)
fcc2010_large <- fcc2010_large %>%
  dt_filter(MAXADDOWN %in% 3:11)
saveRDS(fcc2010_large, "~/git/rural_broadband/data/working/FCC_working/FCC_LG_FIPSwBBPROVIDERS.RDS")
rm(fcc2010_large)
#fcc_large_provcount2 <- fcc_large_provcount %>% group_by(FULLFIPSID) %>% summarise(provid_ct = n())

fcc2010_wireless <- fread("NBM-Wireless-CSV-December-2010.csv",fill=TRUE, select = c(2, 6, 9, 10) ) # CENSUSBLOCK_FIPS, MAXADDOWN
fcc2010_wireless <- fcc2010_wireless %>%
  dt_filter(maxaddown %in% 3:11)
saveRDS(fcc2010_wireless, "~/git/rural_broadband/data/working/FCC_working/FCC_WL_FIPSwBBPROVIDERS.RDS")
setwd("~/git/rural_broadband/data/working/FCC_working/")
fcc2010_large <- readRDS("FCC_LG_FIPSwBBPROVIDERS.RDS")
fcc2010_small <- readRDS("FCC_SM_FIPSwBBPROVIDERS.RDS")

fcc_GEOID_bb <- data.frame(GEOID00 = unique(c(fcc_small_GEOID00, fcc_large_GEOID00, fcc_wireless_GEOID00)))
fcc_GEOID_bb

length(unique(fcc2010_small$FULLFIPSID))
length(unique(fcc2010_large$FULLFIPSID))
length(unique(fcc2010_wireless$censusblock_fips))

length(intersect(unique(fcc2010_small$FULLFIPSID), unique(fcc2010_large$FULLFIPSID))) # 1594 in common
length(intersect(unique(fcc2010_wireless$censusblock_fips), unique(fcc2010_large$FULLFIPSID))) #24974 in common
length(intersect(unique(fcc2010_small$FULLFIPSID), unique(fcc2010_wireless$censusblock_fips))) # 1,817,139 in common

largesmall_shared <- intersect(unique(fcc2010_small$FULLFIPSID), unique(fcc2010_large$FULLFIPSID))
head(largesmall_shared, 5)
FCC_FIPSwBBPROV_LGSMjoin <- full_join(fcc2010_small, fcc2010_large, "FULLFIPSID")
FCC_FIPSwBBPROV_LGSMjoin 
#setwd("~/git/rural_broadband/")
#saveRDS(fcc_GEOID_bb, "~/git/rural_broadband/data/working/FCC_working/FCC_FIPSwBB.RDS")


FCC_LG_FIPSwBBPROVIDERS <- data.table(distinct(readRDS("~/git/rural_broadband/data/working/FCC_working/FCC_LG_FIPSwBBPROVIDERS.RDS")))
FCC_SM_FIPSwBBPROVIDERS <- data.table(distinct(readRDS("~/git/rural_broadband/data/working/FCC_working/FCC_SM_FIPSwBBPROVIDERS.RDS")))
FCC_WL_FIPSwBBPROVIDERS <- data.table(distinct(readRDS("~/git/rural_broadband/data/working/FCC_working/FCC_WL_FIPSwBBPROVIDERS.RDS")))


###SIDE QUESTION - why do small/large FCC files overlap? wireless overlaps as well - but that's expected? 
lg_sm_intersect_fips <- intersect(unique(FCC_LG_FIPSwBBPROVIDERS$FULLFIPSID), unique(FCC_SM_FIPSwBBPROVIDERS$FULLFIPSID)) #1594
wl_size_intersect_fips <- intersect(lg_sm_intersect_fips, unique(FCC_WL_FIPSwBBPROVIDERS$censusblock_fips)) #262
lg_wl_intersect_fips <- intersect(unique(FCC_LG_FIPSwBBPROVIDERS$FULLFIPSID), unique(FCC_WL_FIPSwBBPROVIDERS$censusblock_fips)) #24,974
sm_wl_intersect_fips <- intersect(unique(FCC_SM_FIPSwBBPROVIDERS$FULLFIPSID), unique(FCC_WL_FIPSwBBPROVIDERS$censusblock_fips)) #1,817,139

LG_SM_JOIN <- maditr::dt_inner_join(FCC_LG_FIPSwBBPROVIDERS, FCC_SM_FIPSwBBPROVIDERS, by = "FULLFIPSID")
LG_SM_JOIN <- LG_SM_JOIN %>% setNames(nm = c("FULLFIPSID", "LG_PROVNAME", "LG_MAXADDOWN", "LG_MAXADUP", "SM_PROVNAME", "SM_MAXADDOWN", "SM_MAXADUP"))
LG_SM_JOIN <- LG_SM_JOIN %>% mutate(ADDOWN_AGREE = ifelse(LG_MAXADDOWN == SM_MAXADDOWN, 1, 0),
                                    ADUP_AGREE = ifelse(LG_MAXADUP == SM_MAXADUP, 1, 0))

table(LG_SM_JOIN$ADDOWN_AGREE)  #2370 vs 1716
table(LG_SM_JOIN$ADUP_AGREE) #2135 vs 1951
table(LG_SM_JOIN$ADDOWN_AGREE)/nrow(LG_SM_JOIN) # 58% no
table(LG_SM_JOIN$ADUP_AGREE)/nrow(LG_SM_JOIN) # 52 % no

covered <- data.table("GEOID00" = unique(c(FCC_LG_FIPSwBBPROVIDERS$FULLFIPSID, FCC_SM_FIPSwBBPROVIDERS$FULLFIPSID, FCC_WL_FIPSwBBPROVIDERS$censusblock_fips)))

head(covered, 10)
saveRDS(FCC_LG_FIPSwBBPROVIDERS, "~/git/rural_broadband/data/working/FCC_working/FCC_LG_FIPSwBBPROVIDERS_distinct.RDS")
saveRDS(FCC_SM_FIPSwBBPROVIDERS, "~/git/rural_broadband/data/working/FCC_working/FCC_SM_FIPSwBBPROVIDERS_distinct.RDS")
saveRDS(FCC_WL_FIPSwBBPROVIDERS, "~/git/rural_broadband/data/working/FCC_working/FCC_WL_FIPSwBBPROVIDERS_distinct.RDS")
saveRDS(covered, "~/git/rural_broadband/data/working/FCC_working/block_with_786kbps.RDS")

rm(FCC_LG_FIPSwBBPROVIDERS)
rm(FCC_SM_FIPSwBBPROVIDERS)
rm(FCC_WL_FIPSwBBPROVIDERS)
rm(covered)
######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### POPULATION    ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### ######### 

FCC_LG_FIPSwBBPROVIDERS <- readRDS("~/git/rural_broadband/data/working/FCC_working/FCC_LG_FIPSwBBPROVIDERS_distinct.RDS")
FCC_SM_FIPSwBBPROVIDERS <- readRDS("~/git/rural_broadband/data/working/FCC_working/FCC_SM_FIPSwBBPROVIDERS_distinct.RDS")
FCC_WL_FIPSwBBPROVIDERS <- readRDS("~/git/rural_broadband/data/working/FCC_working/FCC_WL_FIPSwBBPROVIDERS_distinct.RDS")

blocks_w786kb <- readRDS("~/git/rural_broadband/data/working/FCC_working/block_with_786kbps.RDS")
setwd("~/git/dspg19broadband/data/working/FCC/")
block_pop <- fread("population_by_census_block_2010.csv",colClasses = c(GEOID = "character"))
census_2000_to_2010 <- fread("nhgis_blk2000_blk2010_ge.csv",colClasses = c(GEOID00 = "character", GEOID10 = "character"))
census_2000_to_2010$available <- 1*(census_2000_to_2010$GEOID00 %in% blocks_w786kb$GEOID00)

rm(blocks_w786kb)

block_with_786kbps_2010 <-census_2000_to_2010 %>% group_by(GEOID10) %>% summarize(percent_available=sum(WEIGHT*available)/sum(WEIGHT))
block_with_786kbps_2010$percent_available[is.na(block_with_786kbps_2010$percent_available)] <- 0

block_pop2 <- (block_pop %>% dplyr::select(GEOID10 = GEOID, population = value)) %>%
  maditr::dt_left_join(block_with_786kbps_2010,by="GEOID10")

hist(block_pop2$percent_available)
rm(block_pop)
# compute availability by Census Tract; sum(percent_available * population)/sum(population)
block_pop2$TRACTID <- substr(block_pop$GEOID,1,11)

fcc2010_tract <- block_pop2 %>% group_by(TRACTID) %>%
  summarize(available786kb = sum(percent_available*population)/sum(population))
fcc2010_tract$available786kb[is.na(fcc2010_tract$available786kb)] <- 0
hist(fcc2010_tract$available786kb)
setwd("~/git/rural_broadband/data/working/FCC_working/")
write.csv(fcc2010_tract,file="fcc2010tract.csv",row.names=F)

read.csv("~/git/rural_broadband/data/working/FCC_working/fcc2010tract.csv")
