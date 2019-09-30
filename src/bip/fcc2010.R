# use FCC data from Fall 2010 (2009 data may exist by Census Tract -- where is it?)
# PROBLEM: only 63% of FIPS ids for 2010 data are in the decennial block population FIPS
# because this data is using *2000* US Census Block 15 Character number
# need to crosswalk from 2000 to 2010 Census Block IDs;
# use IPUMS crosswalks plus interpolation weights at https://www.nhgis.org/user-resources/geographic-crosswalks#download
# notes on method: interpolation weights, derived from advanced models,
# to support the allocation of summary data from one census's units (the "source zones")
# to another (the "target zones"). Each interpolation weight indicates the approximate proportion
# of a source zone's characteristics that should be allocated to a given target zone.
# use a hybrid of dasymetric and TDW interpolation models
# (see https://www.nhgis.org/documentation/time-series/2000-blocks-to-2010-geog)

library(data.table)
library(dplyr)

options(scipen=999)

# files stored separately for small and large Census blocks, Wireless
# fcc2010_small <- fread("NBM-CBLOCK-CSV-December-2010.csv",fill=TRUE) # FULLFIPSID(7), MAXADDOWN(9)
# fcc2010_large1 <- fread("NBM-Address-Street-CSV-December-2010.csv",fill=TRUE) # breaks at line 7739714
# fcc2010_large2 <- fread("NBM-Address-Street-CSV-December-2010.csv",fill=TRUE,skip=7739715)
# names(fcc2010_large2) <- names(fcc2010_large1)
# fcc2010_large <- rbind(fcc2010_large1,fcc2010_large2)
# rm(fcc2010_large1); rm(fcc2010_large2)
# fcc2010_wireless <- fread("NBM-Wireless-CSV-December-2010.csv",fill=TRUE) # CENSUSBLOCK_FIPS, MAXADDOWN
#save(fcc2010_small,fcc2010_large,fcc2010_wireless,file="~/Desktop/Rural_Broadband/fcc2010/fcc2010.RData")
load("fcc2010.RData")

# get unique Census Block FIPS ID (15 digit) from *any* source with MAXADDOWN >= 25 Mbps [8:11]
class(fcc2010_small$FULLFIPSID)
class(fcc2010_large$FULLFIPSID)
class(fcc2010_wireless$censusblock_fips)
block_with_25Mbps_all <- data.frame(GEOID00 =
                                  unique( c(fcc2010_small$FULLFIPSID[fcc2010_small$MAXADDOWN %in% 8:11],
                                            fcc2010_large$FULLFIPSID[fcc2010_large$MAXADDOWN %in% 8:11],
                                            fcc2010_wireless$censusblock_fips[fcc2010_wireless$maxaddown %in% 8:11])
                                  )
)
block_with_25Mbps <- block_with_25Mbps_all %>% filter(nchar(paste(GEOID00))==15)

# https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html
# Block: 15 digits [FULLFIPSID]
# Block Group: 12 digits
# Census Tract: 11 digits

# are any available at 25 Mbps? filter by MAXADDOWN
# from https://www2.ntia.doc.gov/June-2010-datasets
###########MaxAdDown###########
# Code	Description
# 2	Greater than 200 kbps and less than 768 kbps
# 3	Greater than 768 kbps and less than 1.5 mbps
# 4	Greater than 1.5 mbps and less than 3 mbps
# 5	Greater than 3 mbps and less than 6 mbps
# 6	Greater than 6 mbps and less than 10 mbps
# 7	Greater than 10 mbps and less than 25 mbps
# 8	Greater than 25 mbps and less than 50 mbps
# 9	Greater than 50 mbps and less than 100 mbps
# 10	Greater than 100 mbps and less than 1 gbps
# 11	Greater than 1 gbps
# available at 25 Mbps if MAXADDOWN %in% c(7,8,9,10,11)
# sum(fcc2010v3$MAXADDOWN %in% 1:11) # 41923753
# sum(fcc2010v3$MAXADDOWN %in% 7:11) # 599424

# get 2010 block population and join to FCC data by FIPS ID
block_pop <- fread("population_by_census_block_2010.csv",colClasses = c(GEOID = "character"))

# nrow(block_with_25Mbps)/nrow(block_pop)
# 2,629,624 Census blocks (24%) have providers above 25Mbps

# must convert from 2000 to 2010 Census blocks, in order to compare to later FCC data
census_2000_to_2010 <- fread("nhgis_blk2000_blk2010_ge.csv",colClasses = c(GEOID00 = "character", GEOID10 = "character"))
# metadata:
# GEOID00: GEOID for 2000 Census
# GEOID10: GEOID for 2010 Census
# WEIGHT: interpolation weights to allocate portions of 2000 block counts to 2010 blocks
# PAREA: proportion of 2000 block's land area in each 2010 block

census_2000_to_2010$available <- 1*(census_2000_to_2010$GEOID00 %in% block_with_25Mbps$GEOID00)
# use interpolation weights by GEOID10 to get proportion availability by 2010 Census block (note: most of these weights are '1')
block_with_25Mbps_2010 <-census_2000_to_2010 %>% group_by(GEOID10) %>% summarize(percent_available=sum(WEIGHT*available)/sum(WEIGHT))
block_with_25Mbps_2010$percent_available[is.na(block_with_25Mbps_2010$percent_available)] <- 0

# join to 2010 block population
block_pop2 <- (block_pop %>% dplyr::select(GEOID10 = GEOID, population = value)) %>%
  left_join(block_with_25Mbps_2010,by="GEOID10")

# compute availability by Census Tract; sum(percent_available * population)/sum(population)
block_pop2$TRACTID <- substr(block_pop$GEOID,1,11)

fcc2010_tract <- block_pop2 %>% group_by(TRACTID) %>%
  summarize(available25 = sum(percent_available*population)/sum(population))
fcc2010_tract$available25[is.na(fcc2010_tract$available25)] <- 0

write.csv(fcc2010_tract,file="fcc2010tract.csv",row.names=F)
#block_pop$available25 <- 1*(block_pop$GEOID %in% block_with_25Mbps$GEOID)

# note the huge majority are '0' or '1' by Census Tract
# -----------------------------------------------
# plots; comparison to 2015 FCC data
#Wfcc2015_tract <- read.csv("../brookingsOLS/broadband_availability_by_census_tract_2015.csv")
# load availability by tract for 2015; comparisons (0 counts)
#sum(fcc2010_tract$available25==0) / nrow(fcc2010_tract) # 33% unavailable by tract at 25 Mbps
#sum((1-fcc2015_tract$percent_unavailable)==0,na.rm=T) / nrow(fcc2015_tract) # only 2% totally unavailable by tract at 25 Mbps

# -----------------------------------------------
# save as a separate .csv, and push under data/working/FCC





# fixed: block population GEOID was missing leading zeroes
# table(nchar(block_pop$GEOID))
# block_pop$GEOID[nchar(block_pop$GEOID)==14] <- paste0("0",block_pop$GEOID[nchar(block_pop$GEOID)==14])
# table(nchar(block_pop$GEOID))
# write.csv(block_pop,file="~/Desktop/Rural_Broadband/brookingsOLS/population_by_census_block_2010.csv")
