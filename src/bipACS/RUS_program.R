# 1/24/20 JG
# RUS program information; join to shapefiles

library(sf)

# Protected Broadband Borrower Service Areas - https://www.usda.gov/reconnect/eligibility-area-map-datasets
#This layer includes the service areas of entities that received a Telecommunications Infrastructure loan, Farm Bill Broadband loan (Farm Bill),
#or Broadband Initiatives Program (BIP) loan in or after Fiscal Year (FY) 2000. Service areas of projects that were approved but were subsequently 
#de-obligated are not protected and have been omitted from this layer. The current dataset is dated September 3, 2019. A new dataset will be 
#available for download prior to the opening of the application window.

# 2019 updated shapefile:
RUS_servicearea <- st_read(dsn="~/git/rural_broadband/data/rus_broadband_servicearea/", layer="ProtectedBorrowers04222019")

# RUS program information:
RUS_program <- read.csv("~/git/rural_broadband/data/rus_broadband_servicearea/RUS_programs.csv")

table(RUS_program$ProgramID)
#BIP CCG FBB INF RCP 
#325 128  15 227   1 

#BIP	Broadband Initiatives Program
#CCG	Community Connect Grant Program
#FBB	Broadband Program
#INF	Telecom Loan Program
#RCP	ReConnect Program

# NOTE: looks like we can only join based only on the company (BorrowerID), not on the specific project (ProjectID)

table( RUS_program$BorrowerID %in% RUS_servicearea$RUSID_1 ) # 366
table( RUS_program$BorrowerID %in% RUS_servicearea$RUSID_2 ) # 102
table( RUS_program$BorrowerID %in% RUS_servicearea$RUSID_3 ) # 21

RUS_program$has_shapefile <- RUS_program$BorrowerID %in% RUS_servicearea$RUSID_1 | RUS_program$BorrowerID %in% RUS_servicearea$RUSID_2 |
  RUS_program$BorrowerID %in% RUS_servicearea$RUSID_3
table(RUS_program$has_shapefile) # 472 / 696 programs have service area shapefiles by RUSID
table(RUS_program$has_shapefile, RUS_program$ProgramID)
#      BIP CCG FBB INF RCP
#FALSE 151  55   6  11   1
#TRUE  174  73   9 216   0

# only 174/325 BIP projects in program data have associated shapefiles (RUSID to BorrowerID)


