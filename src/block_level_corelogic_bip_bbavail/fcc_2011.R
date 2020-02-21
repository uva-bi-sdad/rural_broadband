# 2/6/20
# FCC availability in 2011 (available from NBM)
# https://www2.ntia.doc.gov/Dec-2011-datasets; "US Broadband Availability Data by CSV"
# get #unique providers per Census Tract
# do not include Satellite providers

library(data.table)
library(dplyr)

#setwd("~/Desktop/USDA Broadband/Rural_Broadband/fcc_availability/")
bbd_folder <- "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"
nbm_folder <- "/project/biocomplexity/sdad/projects_data/usda/bb/original/NBM_2011/"
bbd_folder <- "/project/biocomplexity/sdad/projects_data/usda/bb/"

# StateAbbreviation-NBM-CBLOCK-CSV-December-2011.zip --- Broadband Availibility data for Census Blocks less than 2 square miles
# StateAbbreviation-NBM-Wireless-CSV-December-2011.zip --- Broadband Availibility data for Wireless
# StateAbbreviation-NBM-Address-Street-CSV-December-2011.zip --- Broadband Availibility data for Large Blocks - Census blocks over 2 square miles

# include all three files
# metadata:
# StateAbbreviation-NBM-CBLOCK-CSV-December-2011
#FRN - FCC Registration Number
#PROVNAME - Provider Name
#DBANAME - Doing Business As Name
#HOCONUM - Holding Company Unique Number 
#HOCONAME - Holding Company Name 
#STATEABBR - 2 character State Abbreviation
#FULLFIPSID - 2010 US Census Block 15 Character number
#TRANSTECH - Technology Code (See below for valid values)
#MAXADDOWN - Maximum Advertised Download Speed (see below for valid values) from record level
#MAXADUP - Maximum Advertised Upload Speed (see below for valid values) from record level
#TYPICDOWN - Typical Download Speed (see below for valid values)
#TYPICUP - Typical Upload Speed (see below for valid values)
#DOWNLOADSPEED - Maximum Advertised Download Speed if provided from Oviewview table
#UPLOADSPEED - Maximum Advertised Upload Speed if provided from Overview table
#OBJECTID - These values are just a record count from the database and should be ignored

# StateAbbreviation-NBM-Wireless-CSV-December-2011
#DATASOURCE - Source of availability data (Originally submitted to NTIA in Shapefiles)
#FRN - FCC Registration Number
#PROVNAME - Provider Name
#DBANAME - Doing Business As Name
#HOCONUM - Holding Company Unique Number 
#HOCONAME - Holding Company Name 
#CENSUSBLOCK_FIPS - 2010 US Census Block 15 Character number
#PCT_BLK_IN_SHAPE - the percent area of the wireless polygon overlay of the Census block
#TRANSTECH - Technology Code (See below for valid values)
#MAXADDOWN - Maximum Advertised Download Speed (see below for valid values) from record level
#MAXADUP - Maximum Advertised Upload Speed (see below for valid values) from record level
#TYPICDOWN - Typical Download Speed (see below for valid values)
#TYPICUP - Typical Upload Speed (see below for valid values)
#OBJECTID - These values are just a record count from the database and should be ignored
#SPECTRUM - Spectrum used (not included in this data round)

# StateAbbreviation-NBM-Address-Street-CSV-December-2011
#DATASOURCE - Source of availability data (Originally submitted to NTIA as Road Segment or Address Points)
#FRN - FCC Registration Number
#PROVNAME - Provider Name
#DBANAME - Doing Business As Name
#HOCONUM - Holding Company Unique Number 
#HOCONAME - Holding Company Name 
#STATEABBR - 2 character State Abbreviation
#FULLFIPSID - 2010 US Census Block 15 Character number
#COUNTY_FIPS - The 5 character State + County 2010 US Census FIPS ID
#TRANSTECH - Technology Code (See below for valid values)
#MAXADDOWN - Maximum Advertised Download Speed (see below for valid values) from record level
#MAXADUP - Maximum Advertised Upload Speed (see below for valid values) from record level
#TYPICDOWN - Typical Download Speed (see below for valid values)
#TYPICUP - Typical Upload Speed (see below for valid values)
#DOWNLOADSPEED - Maximum Advertised Download Speed if provided from Overview table
#UPLOADSPEED - Maximum Advertised Upload Speed if provided from Overview table
#OBJECTID - These values are just a record count from the database and should be ignored
#RANDOM_PT_OBJECTID - These values are just a record count from the database and should be ignored

###########Data_Dictionary###########
###########TransTech###########
#Code	Description
#10	Asymmetric xDSL
#20	Symmetric xDSL
#30	Other Copper Wire
#40	Cable Modem - DOCSIS 3.0 Down
#41	Cable Model - Other
#50	Optical Carrier/Fiber to the End User
#60	Satellite
#70	Terrestrial Fixed - Unlicensed
#71	Terrestrial Fixed - Licensed
#80	Terrestrial Mobile Wireless
#90	Electric Power Line
#0	All Other

############MaxAdDown (Same as Downloadspeed)###########
#Code	Description
#2	Greater than 200 kbps and less than 768 kbps
#3	Greater than 768 kbps and less than 1.5 mbps
#4	Greater than 1.5 mbps and less than 3 mbps
#5	Greater than 3 mbps and less than 6 mbps
#6	Greater than 6 mbps and less than 10 mbps
#7	Greater than 10 mbps and less than 25 mbps
#8	Greater than 25 mbps and less than 50 mbps
#9	Greater than 50 mbps and less than 100 mbps
#10	Greater than 100 mbps and less than 1 gbps
#11	Greater than 1 gbps
#
############MaxAdUp (Same as Uploadspeed)###########
#Code	Description
#2	Greater than 200 kbps and less than 768 kbps
#3	Greater than 768 kbps and less than 1.5 mbps
#4	Greater than 1.5 mbps and less than 3 mbps
#5	Greater than 3 mbps and less than 6 mbps
#6	Greater than 6 mbps and less than 10 mbps
#7	Greater than 10 mbps and less than 25 mbps
#8	Greater than 25 mbps and less than 50 mbps
#9	Greater than 50 mbps and less than 100 mbps
#10	Greater than 100 mbps and less than 1 gbps
#11	Greater than 1 gbps

# read in three files
fcc2011_small <- fread(paste0(nbm_folder, "NBM-CBLOCK-CSV-December-2011.csv"),
                       colClasses = c(fullfipsid="character"))

fcc2011_wireless <- fread(paste0(nbm_folder, "NBM-Wireless-CSV-December-2011.csv"),
                          colClasses = c(censusblock_fips="character"))

fcc2011_large <- fread(paste0(nbm_folder, "ALL-NBM-Address-Street-CSV-DEC-2011.csv"),
                       colClasses = c(fullfipsid="character"))

#table(fcc2011_wireless$transtech)
#70       71       80 
#6987585  2183713 72004604 
# includes 70, 71, 80 (Terrestrial Fixed Liscenced/Unliscenced, Terrestrial Mobile Wireless); remove 71, 80

#table(fcc2011_small$transtech)
#10      20      30      40      41      50      90 
#8254248 1046154 2196109 4427509 1498950 1154734    2219 
#table(fcc2011_large$transtech)
#10      20      30      40      41      50 
#6137737  146614  603452 3088029 1483969  541714 

# filter out Satellite data (TRANSTECH = 60), Terrestrial Mobile Wireless (TRANSTECH = 80), Unliscenced Terrestrial Fixed Wireless (71)
fcc2011_wireless <- fcc2011_wireless %>% filter(transtech == 70)

# make tract into GEOID (FULLFIPSID, CENSUSBLOCK_FIPS, FULLFIPSID; first 11)
fcc2011_small$GEOID <- fcc2011_small$fullfipsid #substr(fcc2011_small$fullfipsid,1,11)
fcc2011_large$GEOID <- fcc2011_large$fullfipsid #substr(fcc2011_large$fullfipsid,1,11)
fcc2011_wireless$GEOID <- fcc2011_wireless$censusblock_fips #substr(fcc2011_wireless$censusblock_fips,1,11)

fcc2011_wireless$frn <- as.integer(fcc2011_wireless$frn)
# rbind these three files by: FRN, GEOID, TRANSTECH, MAXADDOWN, MAXADUP
fcc2011 <- rbind( fcc2011_small %>% dplyr::select(frn, GEOID, transtech, maxaddown, maxadup),
                  fcc2011_large %>% dplyr::select(frn, GEOID, transtech, maxaddown, maxadup) ,
                  fcc2011_wireless %>% dplyr::select(frn, GEOID, transtech, maxaddown, maxadup)
)

# counts by tract:
#Providers of Residential Fixed High-Speed Connections over 200 kbps in at least one direction

# filter download or upload > 200 kbps
# MaxAdDown >= 2 OR MaxAdUp >= 2
fcc2011_200 <- fcc2011 %>% filter(maxaddown >= 2 | maxadup >= 2)
# get counts of *unique* providers by Tract (FRN)
fcc2011_200_counts <- fcc2011_200 %>% group_by(GEOID) %>% summarize(fcc2011_providers_200=length(unique(frn)))


#Providers of Residential Fixed Connections at least 3 Mbps downstream and at least 768 kbps upstream

# filter download > 3Mbps, upload > 768 kbps
# MaxAdDown >= 5 AND MaxAdUp >= 3
fcc2011_3 <- fcc2011 %>% filter(maxaddown >= 5, maxadup >= 3)
# get counts of *unique* providers by Tract (FRN)
fcc2011_3_counts <- fcc2011_3 %>% group_by(GEOID) %>% summarize(fcc2011_providers_3=length(unique(frn)))

saveRDS(fcc2011_200, paste0(bbd_folder, "working/fcc2011_200byblock.RDS"))
saveRDS(fcc2011_3, paste0(bbd_folder, "working/fcc2011_3byblock.RDS"))


#table(fcc2011_200_counts$fcc2011_providers_200)
#1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17 
#845 11707 18399 16332 12055  7065  3738  1879   998   414   164    70    44    23     6     4     2 
#table(fcc2016_200_counts$fcc2016_providers_200)
#1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17    18    19    20    28 
#653 17857 21615 15605  8777  4969  2419  1039   424   204   112    60    36    23    15    14     3     1     4     1     1 
#table(fcc2011_3_counts$fcc2011_providers_3)
#1     2     3     4     5     6     7     8     9    10    11    12    13    14    15 
#2224 16738 21188 17038  9629  3819  1564   677   345   149    45    18    15     4     1 
#table(fcc2016_3_counts$fcc2016_providers_3)
#1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    18    19    27 
#1132 22505 21700 13535  7511  4093  1866   807   301   162    76    46    24    17    10     9     2     3     1 

#sum(fcc2011_200_counts$fcc2011_providers_200)
#305495
#sum(fcc2016_200_counts$fcc2016_providers_200)
#271816
#sum(fcc2011_3_counts$fcc2011_providers_3)
#260411
#sum(fcc2016_3_counts$fcc2016_providers_3)
#253694

# there are more providers in 2011 than in 2016 - consolidation

# write GEOID w/ 200 kbps, 3 Mbps counts for 2011 and 2016
# start with full tract dataset by GEOID and join
fcc_availability <- fread("tracts_us.csv", colClasses = c(GEOID="character"))

fcc_availability <- fcc_availability %>% left_join(fcc2011_200_counts, by="GEOID")
fcc_availability <- fcc_availability %>% left_join(fcc2011_3_counts, by="GEOID")
fcc_availability <- fcc_availability %>% left_join(fcc2016_200_counts, by="GEOID")
fcc_availability <- fcc_availability %>% left_join(fcc2016_3_counts, by="GEOID")
fcc_availability[is.na(fcc_availability)] <- 0

fwrite(fcc_availability,file="fcc_availability.csv")

