library(sf)
library(dplyr)

auction <- st_read("/project/biocomplexity/sdad/projects_data/usda/bb/original/Protected_bb_borrower_service_areas/Auction903_April2019.dbf")
prot_borr_serv_area_sf <- st_read("/project/biocomplexity/sdad/projects_data/usda/bb/original/Protected_bb_borrower_service_areas/USDARD_ProtectedApproved02052020.dbf")
approved_dbf_sf <- st_read("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/rus_broadband_servicearea/RD_BB_ApprovedSA.dbf")
infraborrclec <- st_read("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/rus_broadband_servicearea/RD_BB_InfraBorrCLEC.dbf")
infraborr <- st_read("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/rus_broadband_servicearea/RD_BB_InfrastructureBorr.dbf")

nrow(auction) #16902 - POLYGON
nrow(prot_borr_serv_area_sf) #1886 - POLYGON
nrow(approved_dbf_sf) #1557 - POLYGON

# prot_borr_serv_area_sf %>% st_set_geometry(NULL) %>% write.csv("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/Prot_Borr.csv")
# approved_dbf_sf %>% st_set_geometry(NULL) %>% write.csv("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/Approved_SA.csv")
# infraborrclec %>% st_set_geometry(NULL) %>% write.csv("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/InfraborrCLEC.csv")
# infraborrclec %>% st_set_geometry(NULL) %>% write.csv("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/Infraborr.csv")

#######  DATE QUESTION   ##########################################  

prot_borr_serv_area_sf

#######  PROGRAM QUESTION   ##########################################  

table(approved_dbf_sf$PROGRAMTYP) # BIP, CC, FB, Telecom

prot_prog_breakdown <- prot_borr_serv_area_sf %>% st_set_geometry(NULL) %>% count(BB, BIPLOAN, BIPGRANT, CCGRANT, PROGRAM, SUBPROGRAM) %>% data.table::as.data.table()

prot_borr_serv_area_sf %>% st_set_geometry(NULL) %>% count(PROGRAM, SUBPROGRAM) # 1370 without prog/subprog
prot_borr_serv_area_sf %>% st_set_geometry(NULL) %>% count(PROGRAM, SUBPROGRAM, INF, BB, BIPLOAN, BIPGRANT, CCGRANT) %>% View() 
# 750 have some combination loan/grant info, but 620 still dont

#######  GEOGRAPHY QUESTION   ##########################################  

usonly_thenewone <- prot_borr_serv_area_sf %>% filter(stringr::str_detect(RUSID_1, "^AK|^HI") == FALSE & stringr::str_detect(RUSID_2, "^AK|^HI") == FALSE)
usonly_approved_dbf_sf <- approved_dbf_sf %>% filter(stringr::str_detect(RUS_ID, "^AK|^HI|^AS") == FALSE)

#approved_dbf_sf %>% filter(stringr::str_detect(RUS_ID, "^AS"))
#table(stringr::str_extract(usonly_approved_dbf_sf$RUS_ID, "^[:alpha:][:alpha:]"))

plot(usonly_thenewone$geometry, col = "grey")
plot(usonly_approved_dbf_sf$geometry, col = "red")
plot(usonly_approved_dbf_sf$geometry, col = "red", add = TRUE)

plot(auction$geometry)

#######  RUS ID QUESTION   ##########################################  

prot_rus_ids <- c(as.character(prot_borr_serv_area_sf$RUSID_1), as.character(prot_borr_serv_area_sf$RUSID_2), as.character(prot_borr_serv_area_sf$RUSID_3))
orig_rus_ids <- as.character(approved_dbf_sf$RUS_ID)

length(prot_rus_ids)
length(unique(prot_rus_ids))
length(orig_rus_ids)
length(unique(orig_rus_ids))

length(intersect(prot_rus_ids, orig_rus_ids))
length(setdiff(prot_rus_ids, orig_rus_ids))
length(setdiff(orig_rus_ids, prot_rus_ids))


prot_borr_serv_area_sf%>% st_set_geometry(NULL)  %>% count(BB, BIPLOAN, BIPGRANT, CCGRANT, PROGRAM, SUBPROGRAM, is.na(prot_borr_serv_area_sf$RUSID_1), is.na(prot_borr_serv_area_sf$RUSID_2), is.na(prot_borr_serv_area_sf$RUSID_3)) %>% View()


library(sf)
library(dplyr)

approved_dbf_sf <- st_read("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/rus_broadband_servicearea/RD_BB_ApprovedSA.dbf"  )
masterfile <- readxl::read_excel("/project/biocomplexity/sdad/projects_data/usda/bb/original/RUS bb projects_2009-2019.xlsx")
prot_borr_serv_area_sf <- st_read("/project/biocomplexity/sdad/projects_data/usda/bb/original/Protected_bb_borrower_service_areas/USDARD_ProtectedApproved02052020.dbf")

# as.character(approved_dbf_sf$RUS_ID[1])
# prot_borr_serv_area_sf$RUSID_1

prot_rus_ids <- unique(c(as.character(prot_borr_serv_area_sf$RUSID_1), as.character(prot_borr_serv_area_sf$RUSID_2), as.character(prot_borr_serv_area_sf$RUSID_3)))
orig_rus_ids <- as.character(unique(approved_dbf_sf$RUS_ID))

prot_rus_ids
head(masterfile, 1)
masterfile_borrowers <- unique(masterfile$BorrowerID)

length(orig_rus_ids)
length(masterfile_borrowers)

length(intersect(orig_rus_ids, masterfile_borrowers))

length(prot_rus_ids)
length(intersect(prot_rus_ids, masterfile_borrowers))


prot_borr_serv_area_sf %>% st_set_geometry(NULL) %>% inner_join(masterfile, by = c("RUSID_1" = "BorrowerID")) %>% nrow()

prot_borr_serv_area_sf %>% filter(RUSID_1 == "WA0534")
masterfile %>% filter(BorrowerID == "WA0534")

table(approved_dbf_sf$PROGRAMSER)
table(approved_dbf_sf$PROGRAMTYP)

# prot_borr_serv_area_sf  %>% st_set_geometry(NULL) %>% count(INF, BB, BIPLOAN, BIPGRANT, CCGRANT, PROGRAM, SUBPROGRAM) %>% View()

#   filter(STATUS == "Approved" & 
#          APPSTATUSS == "Approved" & 
#          str_detect(PROGRAMTYP, pattern = "BIP") == TRUE &
#          lubridate::year(PUBLISHEDD) > 2000   
# )


orig_bips_unfiltered <- approved_dbf_sf %>% filter(stringr::str_detect(PROGRAMTYP, "BIP"))
new_borrower_bips <- prot_borr_serv_area_sf %>% filter(!is.na(BIPLOAN)|!is.na(BIPGRANT))

table(orig_bips_unfiltered$APPSTATUSS)
table(new_borrower_bips$APPSTATUS)


biprusIDs_orig <- unique(orig_bips_unfiltered$RUS_ID)
biprusIDS_borrowers <- unique(c(as.character(new_borrower_bips$RUSID_1), as.character(new_borrower_bips$RUSID_2), as.character(new_borrower_bips$RUSID_3)))

length(biprusIDs_orig)
length(biprusIDS_borrowers)

length(intersect(biprusIDs_orig, biprusIDS_borrowers))
length(setdiff(biprusIDs_orig, biprusIDS_borrowers))
length(setdiff(biprusIDS_borrowers, biprusIDs_orig))

orig_bips_unfiltered  %>% st_set_geometry(NULL) %>% count(PROGRAMTYP, PROGRAMSER)
new_borrower_bips %>% st_set_geometry(NULL) %>% count(PROGRAM, SUBPROGRAM, INF, BB, BIPLOAN, BIPGRANT, CCGRANT) %>% select(-n)






plot(orig_bips_unfiltered$geometry, col = "red")
plot(new_borrower_bips$geometry, col = "grey")

`%nin%` = Negate(`%in%`)

orig_bips_unfiltered2 <- orig_bips_unfiltered %>% filter(stringr::str_extract(RUS_ID, "^[:alpha:][:alpha:]") %nin% c("HI", "AK", "PR", "AS", "GU" ))
new_borrower_bips2 <- new_borrower_bips %>% 
  filter(stringr::str_extract(RUSID_1, "^[:alpha:][:alpha:]") %nin% c("HI", "AK", "PR", "AS", "GU" )) %>% 
  filter(stringr::str_extract(RUSID_2, "^[:alpha:][:alpha:]") %nin% c("HI", "AK", "PR", "AS", "GU" )) %>%
  filter(stringr::str_extract(RUSID_3, "^[:alpha:][:alpha:]") %nin% c("HI", "AK", "PR", "AS", "GU" ))

plot(orig_bips_unfiltered2$geometry, col = "red")
plot(new_borrower_bips2$geometry, col = "grey")

table(masterfile$ProgramID)
masterfile_bips <- masterfile %>% filter(ProgramID == "BIP")

length(unique(masterfile_bips$BorrowerID)) # 265 unique IDs out of 1196

masterfile_bips_cts  <- masterfile_bips %>% count(BorrowerID, ProgramID, StateID, oblg_year = lubridate::year(ObligationDate))

#new_borrower_bips %>% left_join(masterfile_bips_cts, by = c())
length(intersect(as.character(unique(new_borrower_bips$RUSID_1)), masterfile_bips_cts$BorrowerID))
length(setdiff(as.character(unique(new_borrower_bips$RUSID_1)), masterfile_bips_cts$BorrowerID))
length(setdiff(masterfile_bips_cts$BorrowerID, as.character(unique(new_borrower_bips$RUSID_1))))

new_borrower_bips %>% filter(!is.na(RUSID_1)) %>% 
  inner_join(masterfile_bips_cts, by = c("RUSID_1" = "BorrowerID")) %>% nrow()

new_borrower_bips %>% st_set_geometry(NULL) %>% count(RUSID_1) %>% arrange(desc(n))

# INDEPENDENT 
# RUSID_1 = inner join - 299, anti join - 309, adds up to 608 (created 20 rows - bc of NAs?)
# RUSID_2 = inner join - 366, anti join - 281, adds up to 647 (created 59 rows)
# RUSID_3 = inner join - 299, anti join - 309, adds up to 608 (created 20 rows)






