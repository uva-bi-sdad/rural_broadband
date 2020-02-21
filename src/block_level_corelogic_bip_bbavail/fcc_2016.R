# 2/6/20
# FCC availability in 2016
# https://www.fcc.gov/form-477-broadband-deployment-data-december-2016-version-2
# get #unique providers per Census Tract
# do not include Satellite providers

library(data.table)
library(dplyr)

#setwd("~/Desktop/USDA Broadband/Rural_Broadband/fcc_availability/")
fcc_folder <- "/project/biocomplexity/sdad/projects_data/usda/bb/original/FCC_2016/"
bbd_folder <- "/project/biocomplexity/sdad/projects_data/usda/bb/"

# get counts of #providers per tract at 200kbps, 3Mbps
fcc2016 <- fread(paste0(fcc_folder, "fbd_us_without_satellite_dec2016_v2.csv"),
                 colClasses = c(BlockCode="character"))
# make tract into GEOID; block = 15, tract = 11
fcc2016$GEOID <- fcc2016$BlockCode #substr(fcc2016$BlockCode,1,11)

# counts by tract:
#Providers of Residential Fixed High-Speed Connections over 200 kbps in at least one direction

# filter download or upload > 200 kbps
# MaxAdDown >= 0.2 OR MaxAdUp >= 0.2
fcc2016_200 <- fcc2016 %>% filter(MaxAdDown >= 0.2 | MaxAdUp >= 0.2)
# get counts of *unique* providers by Tract (Provider_Id)
fcc2016_200_counts <- fcc2016_200 %>% group_by(GEOID) %>% summarize(fcc2016_providers_200=length(unique(Provider_Id)))

#Providers of Residential Fixed Connections at least 3 Mbps downstream and at least 768 kbps upstream

# filter download > 3Mbps, upload > 768 kbps
# MaxAdDown >= 3 AND MaxAdUp >= 0.768
fcc2016_3 <- fcc2016 %>% filter(MaxAdDown >= 3, MaxAdUp >= 0.768)
# get counts of *unique* providers by Tract (Provider_Id)
fcc2016_3_counts <- fcc2016_3 %>% group_by(GEOID) %>% summarize(fcc2016_providers_3=length(unique(Provider_Id)))

saveRDS(fcc2016_200, paste0(bbd_folder, "working/fcc2016_200byblock.RDS"))
saveRDS(fcc2016_3, paste0(bbd_folder, "working/fcc2016_3byblock.RDS"))



# metadata: (https://www.fcc.gov/general/explanation-broadband-deployment-data)
#LogRecNo: A logical record number created to relate the broadband deployment tables to the Imputations Table
#Provider_Id: filing number (assigned by FCC)
#FRN: FCC registration number
#ProviderName: Provider name
#DBAName: "Doing business as" name
#HoldingCompanyName: Holding company name (as filed on Form 477)
#HocoNum: Holding company number (assigned by FCC)
#HocoFinal: Holding company name (attribution by FCC)
#StateAbbr: 2-letter state abbreviation used by the US Postal Service
#BlockCode: 15-digit census block code used in the 2010 US Census
#TechCode: 2-digit code indicating the Technology of Transmission used to offer broadband service
#Consumer: (0/1) where 1 = Provider can or does offer consumer/mass market/residential service in the block
#MaxAdDown: Maximum advertised downstream speed/bandwidth offered by the provider in the block for Consumer service (Mbps)
#MaxAdUp: Maximum advertised upstream speed/bandwidth offered by the provider in the block for Consumer service (Mbps)
#Business: (0/1) where 1 = Provider can or does offer business/government service in the block
#MaxCIRDown: Maximum contractual downstream bandwidth offered by the provider in the block for Business service (filer directed to report 0 if the contracted service is sold on a "best efforts" basis without a guaranteed data-throughput rate)
#MaxCIRUp: Maximum contractual upstream bandwidth offered by the provider in the block for Business service (filer directed to report 0 if the contracted service is sold on a "best efforts" basis without a guaranteed data-throughput rate)

#TechCode:
#10	Asymmetric xDSL
#11	ADSL2, ADSL2+
#12	VDSL
#20	Symmetric xDSL*
#30	Other Copper Wireline (all copper-wire based technologies other than xDSL; Ethernet over copper and T-1 are examples)
#40	Cable Modem other than DOCSIS 1, 1.1, 2.0, 3.0, or 3.1
#41	Cable Modem – DOCSIS 1, 1.1 or 2.0
#42	Cable Modem – DOCSIS 3.0
#43	Cable Modem – DOCSIS 3.1
#50	Optical Carrier / Fiber to the end user (Fiber to the home or business end user, does not include “fiber to the curb”)
#60	Satellite
#70	Terrestrial Fixed Wireless
#90	Electric Power Line
#0	All Other

#> table(fcc2016$TechCode)
#0      10      11      12      20      30      40      41      42      43      50      70 
#1384 4237468 3016999 1727376  217149  690157   90842  122687 5697710    2340 2554705 7056007

# *include all but Satellite (no 60 codes); includes Wireless, Electric Power Line, Other



