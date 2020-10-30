# update 7/22/20
# "Create a variable for the share of the tract area within a BIP-only service area.

# For census tracts with no tract area within a BIP-only service area, the distance (up to 50 miles) to the nearest BIP-only service area.
# If the distance can be easily measured as the shortest distance between the boundary of the tract and the boundary of 
# the nearest BIP-only service area, that would be preferable to measuring distances between centroids of polygons.
# But if that can’t be easily done, I’ll live with distances based on centroids.  

# The Project ID code of the BIP project area that the tract either overlaps or is nearest to (up to 50 miles), if the Project ID 
# is available for that BIP project area.  If the Project ID code is not available for the nearest BIP project area, 
# please provide the RUS ID code (which provides the identity of the awardee but not the project).

# Variables for the shares of the tract area served by a combination of BIP and/or programs (whichever combinations are found in the data) 
# or by other single programs (i.e., BIP-CCG, BIP-FBB, BIP-TIL, CCG-FBB, CCG-TIL, FBB-TIL, BIP-CCG-FBB, BIP-CCG-TIL, CCG-FBB-TIL, 
# BIP-CCG-FBB-TIL, CCG only, FBB only, TIL only). I think these 13 variables exhaust the possible combinations of these programs,
# though I think we won’t find some combinations in the data, so fewer than 13 variables should be necessary.

#The Project ID’s associated with the program service areas included in these combinations, if available.
# (There will be potentially one Project ID for each program included in a combination area). If the Project ID is not available 
# for a program (we don’t have them for FBB or TIL) provide the RUS ID."

library(DBI)
library(maps)
library(sf)
library(data.table)
library(dplyr)

options(scipen=999)
setwd("~/git/rural_broadband/src/bipACS")

# --------------------------------------------------------------------------------------------
# read in Census tracts
# --------------------------------------------------------------------------------------------

get_db_conn <-
  function(db_name = "sdad",
           db_host = "postgis1",
           db_port = "5432",
           db_user = Sys.getenv("db_usr"),
           db_pass = Sys.getenv("db_pwd")) {
    RPostgreSQL::dbConnect(
      drv = RPostgreSQL::PostgreSQL(),
      dbname = db_name,
      host = db_host,
      port = db_port,
      user = db_user,
      password = db_pass
    )
  }

con <- get_db_conn()

# continental + AK, HI state fips
state_fips <- c( unique(state.fips$fips), 2, 15 )
state_fips <- formatC(state_fips,width=2,flag=0)
tracts <- list()
for(i in 1:length(state_fips)){
  tracts[[i]] <- sf::st_read(con,c("gis_census_cb",paste0("cb_2018_",state_fips[i],"_tract_500k")))
}

tracts_us <- do.call(rbind, tracts)

# --------------------------------------------------------------------------------------------
# read in BIP shapefiles (use the April shapefiles under projects/usda/rural_broadband)
# --------------------------------------------------------------------------------------------

bip_shape <- st_read(dsn="200409_BIP_ArcGIS",layer="200409_BIP_ServAr_ID")

# when ProjectID is missing, fill in the RUSID instead
bip_shape$ProjectID[is.na(bip_shape$ProjectID)] <- bip_shape$RUS_ID[is.na(bip_shape$ProjectID)]

# aggregate geometry by ProjectID (1168 shapes, 228 unique ProjectIDs, 20 additional RUSIDs)
program_BIP <- bip_shape %>% dplyr::select(ProjectID, geometry) %>%
  aggregate(by=list(bip_shape$ProjectID), first) %>% dplyr::select(-Group.1) %>% st_buffer(0)

# --------------------------------------------------------------------------------------------
# read in the protected borrower service area file
# --------------------------------------------------------------------------------------------

# this is the July file
program_shape <- st_read(dsn="USDARD_Protected",layer="USDARD_ProtectedBorrowers")

# other versions of the protected borrowers file:
# Apr 2019
#program_shape <- st_read(dsn="~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/rus_reconnect/protected_borrower",layer="ProtectedBorrowers04222019")
# Feb 2020
#program_shape <- st_read(dsn="reconnect-eligibility-protected-borrower",layer="USDARD_ProtectedApproved02052020")

table(program_shape$INF,useNA="always")
# INF <NA> 
# 968  465

table(program_shape$BB,useNA="always")
#BB <NA> 
#138 1295

table(program_shape$CCGRANT,useNA="always")
#CC     CC Grant CC Grant (2)         <NA> 
#54          111            3         1265

table(program_shape$BIPLOAN,useNA="always")
#BIP Loan  BIPLoan     <NA> 
#  542       11     880

table(program_shape$BIPGRANT,useNA="always")
#BIP Grant  BIPGrant      <NA> 
#  503        11      919

table(program_shape$PROGRAM)
#BIP            BIPBTOP  Community Connect     Infrastructure Telecommunications 
#1                 11                 20                  2                 45
table(program_shape$SUBPROGRAM)
#Infrastructure 
#46 

# subset to TIL; add PROGRAM="Infrastructure","Telecommunications", SUBPROGRAM="Infrastructure"
program_TIL_all <- program_shape %>% filter(!is.na(INF) | PROGRAM %in% c("Infrastructure","Telecommunications") |
                                              SUBPROGRAM == "Infrastructure",!is.na(RUSID_1))
# aggregate shapes by RUSID_1 (998 shapes, 358 RUSIDs)
program_TIL <- program_TIL_all %>% st_buffer(0) %>% dplyr::select(RUSID_1, geometry) %>%
  aggregate(by=list(program_TIL_all$RUSID_1), first) %>% dplyr::select(-Group.1) %>% st_buffer(0)

# subset to FBB (138 shapes, 28 RUSIDs)
program_FBB_all <- program_shape %>% filter(!is.na(BB),!is.na(RUSID_1))
program_FBB <- program_FBB_all %>% st_buffer(0) %>% dplyr::select(RUSID_1, geometry) %>%
  aggregate(by=list(program_FBB_all$RUSID_1), first) %>% dplyr::select(-Group.1) %>% st_buffer(0)

# subset to CCG (168 shapes, 36 RUSIDs); add PROGRAM="Community Connect"
program_CCG_all <- program_shape %>% filter(!is.na(CCGRANT) | PROGRAM=="Community Connect",!is.na(RUSID_1))
program_CCG <- program_CCG_all %>% st_buffer(0) %>% dplyr::select(RUSID_1, geometry) %>%
  aggregate(by=list(program_CCG_all$RUSID_1), first) %>% dplyr::select(-Group.1) %>% st_buffer(0)

# simplify all polygons at a reasonable tolerance (500m) to speed up computation
library(rgeos)
s_BIP <- st_as_sf( gSimplify(as_Spatial(program_BIP), tol=500) )
s_TIL <- st_as_sf( gSimplify(as_Spatial(program_TIL), tol=500) )
s_FBB <- st_as_sf( gSimplify(as_Spatial(program_FBB), tol=500) )
s_CCG <- st_as_sf( gSimplify(as_Spatial(program_CCG), tol=500) )

s_BIP$ProjectID <- program_BIP$ProjectID

# --------------------------------------------------------------------------------------------
# get the BIP ProjectID (or RUSID) that intersects with each tract; ProjectID_BIP
# --------------------------------------------------------------------------------------------

tracts_us <- st_transform(tracts_us, st_crs(program_BIP))
tracts_us$AREA <- as.numeric( st_area(tracts_us) )

BIP_tract_intersect <- st_intersects(tracts_us, s_BIP)
tracts_us$ProjectID_BIP <- program_BIP$ProjectID[ sapply(BIP_tract_intersect, first) ]

# --------------------------------------------------------------------------------------------
# get the RUSID that intersects with each tract for each of TIL, FBB, CCG;
# RUSID_TIL, RUSID_FBB, RUSID_CCG
# --------------------------------------------------------------------------------------------

TIL_tract_intersect <- st_intersects(tracts_us, s_TIL)
tracts_us$RUSID_TIL <- program_TIL$RUSID_1[ sapply(TIL_tract_intersect, first) ]

FBB_tract_intersect <- st_intersects(tracts_us, s_FBB)
tracts_us$RUSID_FBB <- program_FBB$RUSID_1[ sapply(FBB_tract_intersect, first) ]

CCG_tract_intersect <- st_intersects(tracts_us, s_CCG)
tracts_us$RUSID_CCG <- program_CCG$RUSID_1[ sapply(CCG_tract_intersect, first) ]

# --------------------------------------------------------------------------------------------
# intersect tracts with each program
# --------------------------------------------------------------------------------------------
#save.image("bip_intersections.RData")

tract_intersections <- function(tract,s_BIP,s_TIL,s_FBB,s_CCG){
  library(sf)
  library(data.table)
  library(dplyr)
  tract_areas <- data.frame( GEOID = tract$GEOID,
                             PROP_AREA_none = NA,
                             PROP_AREA_BIP_only= NA,
                             PROP_AREA_TIL_only= NA,
                             PROP_AREA_FBB_only= NA,
                             PROP_AREA_CCG_only= NA,
                             PROP_AREA_BIP_TIL = NA,
                             PROP_AREA_BIP_FBB = NA,
                             PROP_AREA_BIP_CCG = NA,
                             PROP_AREA_TIL_FBB = NA,
                             PROP_AREA_TIL_CCG = NA,
                             PROP_AREA_FBB_CCG = NA,
                             PROP_AREA_BIP_TIL_FBB = NA,
                             PROP_AREA_BIP_TIL_CCG = NA,
                             PROP_AREA_BIP_FBB_CCG = NA, 
                             PROP_AREA_TIL_FBB_CCG = NA,
                             PROP_AREA_BIP_TIL_FBB_CCG = NA
  )
  
  allintersect <- function(tract,op1,op2,op3,op4,
                           shape1=s_BIP,shape2=s_TIL,shape3=s_FBB,shape4=s_CCG){
    if(op1=="i"){
      out1 <- st_union(st_intersection(tract,shape1))
    } else if(op1=="d"){
      shape1 <- st_union( shape1[unlist2(st_intersects(tract,shape1)),] )
      out1 <- st_difference(tract,shape1)
    }
    
    if(op2=="i"){
      out2 <- st_union(st_intersection(out1,shape2))
    } else if(op2=="d"){
      shape2 <- st_union( shape2[unlist2(st_intersects(out1,shape2)),] )
      out2 <- st_union(st_difference(out1,shape2))
    }
    
    if(op3=="i"){
      out3 <- st_union(st_intersection(out2,shape3))
    } else if(op3=="d"){
      shape3 <- st_union( shape3[unlist2(st_intersects(out2,shape3)),] )
      out3 <- st_union(st_difference(out2,shape3))
    }
    
    if(op4=="i"){
      out4 <- st_union(st_intersection(out3,shape4))
    } else if(op4=="d"){
      shape4 <- st_union( shape4[unlist2(st_intersects(out3,shape4)),] )
      out4 <- st_union(st_difference(out3,shape4))
    }
    return(out4)
  }
  
  numtozero <- function(x){
    if(length(x)==0){
      return(0)
    } else{ return(as.numeric(x)) }
  }
  
  unlist2 <- function(x){
    y <- unlist(x)
    ifelse(length(y)==0, return(NA),return(y))
  }
  
  area <- st_area(tract)
  tract_areas$PROP_AREA_none <- numtozero( st_area(allintersect(tract,"d","d","d","d") ) ) / area
  tract_areas$PROP_AREA_BIP_only <- numtozero( st_area(allintersect(tract,"i","d","d","d") ) ) / area
  tract_areas$PROP_AREA_TIL_only <- numtozero( st_area( allintersect(tract,"d","i","d","d") ) ) / area
  tract_areas$PROP_AREA_FBB_only <- numtozero( st_area( allintersect(tract,"d","d","i","d") ) )/ area
  tract_areas$PROP_AREA_CCG_only <- numtozero( st_area( allintersect(tract,"d","d","d","i") ) )/ area
  tract_areas$PROP_AREA_BIP_TIL <- numtozero( st_area( allintersect(tract,"i","i","d","d") ) )/ area
  tract_areas$PROP_AREA_BIP_FBB <- numtozero( st_area( allintersect(tract,"i","d","i","d") ) )/ area
  tract_areas$PROP_AREA_BIP_CCG <- numtozero( st_area( allintersect(tract,"i","d","d","i") ) )/ area
  tract_areas$PROP_AREA_TIL_FBB <- numtozero( st_area( allintersect(tract,"d","i","i","d") ) )/ area
  tract_areas$PROP_AREA_TIL_CCG <- numtozero( st_area( allintersect(tract,"d","i","d","i") ) )/ area
  tract_areas$PROP_AREA_FBB_CCG <- numtozero( st_area( allintersect(tract,"d","d","i","i") ) )/ area
  tract_areas$PROP_AREA_BIP_TIL_FBB <- numtozero( st_area( allintersect(tract,"i","i","i","d") ) )/ area
  tract_areas$PROP_AREA_BIP_TIL_CCG <- numtozero( st_area( allintersect(tract,"i","i","d","i") ) )/ area
  tract_areas$PROP_AREA_BIP_FBB_CCG <- numtozero( st_area( allintersect(tract,"i","d","i","i") ) )/ area
  tract_areas$PROP_AREA_TIL_FBB_CCG <- numtozero( st_area( allintersect(tract,"d","i","i","i") ) )/ area
  tract_areas$PROP_AREA_BIP_TIL_FBB_CCG <- numtozero( st_area( allintersect(tract,"i","i","i","i") ) )/ area
  
  return(tract_areas)
}



library(doParallel)
cl<-makeCluster(30)
registerDoParallel(cl)

#start time
strt<-Sys.time()

# parallel loop
tract_areas1 <- foreach(k=1:20000, .combine=rbind) %dopar% {
  tract_intersections(tract=tracts_us[k,],
                      s_BIP=s_BIP,
                      s_TIL=s_TIL,
                      s_FBB=s_FBB,
                      s_CCG=s_CCG)
}

tract_areas2 <- foreach(k=20001:40000, .combine=rbind) %dopar% {
  tract_intersections(tract=tracts_us[k,],
                      s_BIP=s_BIP,
                      s_TIL=s_TIL,
                      s_FBB=s_FBB,
                      s_CCG=s_CCG)
}

tract_areas3 <- foreach(k=40001:58000, .combine=rbind) %dopar% {
  tract_intersections(tract=tracts_us[k,],
                      s_BIP=s_BIP,
                      s_TIL=s_TIL,
                      s_FBB=s_FBB,
                      s_CCG=s_CCG)
}


ind33 <- (58001:60000)[-1446]
tract_areas33 <- foreach(k=ind33, .combine=rbind) %dopar% {
  tract_intersections(tract=tracts_us[k,],
                      s_BIP=s_BIP,
                      s_TIL=s_TIL,
                      s_FBB=s_FBB,
                      s_CCG=s_CCG)
}
# topologyException in this range; remove the tract

tract_areas4 <- foreach(k=60001:nrow(tracts_us), .combine=rbind) %dopar% {
  tract_intersections(tract=tracts_us[k,],
                      s_BIP=s_BIP,
                      s_TIL=s_TIL,
                      s_FBB=s_FBB,
                      s_CCG=s_CCG)
}

print(Sys.time()-strt)
# turn off clusters
stopCluster(cl)

tract_areas <- rbind(tract_areas1, tract_areas2, tract_areas3, tract_areas33, tract_areas4)

save.image("bip_intersections.RData")


# --------------------------------------------------------------------------------------------
# investigate the single tract with an error; due to precision issues when doing polygon intersection
badtract <- tracts_us[59446,]

fixed <- tract_intersections(
  tract=st_set_precision(badtract,1000),
  s_BIP=s_BIP,
  s_TIL=s_TIL,
  s_FBB=s_FBB,
  s_CCG=s_CCG
)
tract_areas <- rbind(tract_areas, fixed)


# --------------------------------------------------------------------------------------------
# distance of the tract to the nearest BIP-only service area (either border-to-border, or centroids)
# --------------------------------------------------------------------------------------------

# get BIP only shapefiles and rerun (loop over s_BIP shapes and take st_difference)
TIL_FBB_CCG <- st_union( st_union(s_TIL), st_union(s_FBB), st_union(s_CCG) )

s_BIPonly <- s_BIP
ind_remove <- numeric(0)

for(i in 1:nrow(s_BIP)){
  poly <- s_BIP[i,]
  polydiff <- st_difference(poly, TIL_FBB_CCG)
  if(nrow(polydiff)==0){
    ind_remove <- c(ind_remove,i)
  } else{ s_BIPonly$geometry[i] <- polydiff$geometry }
}
s_BIPonly <- s_BIPonly[-ind_remove,]

# loop over all tracts, using st_nearest_feature and st_distance
# (see https://stackoverflow.com/questions/53854803/calculate-minimum-distance-between-multiple-polygons-with-r)
distanceBIP <- function(tract, s_BIP){
  library(sf)
  library(dplyr)
  index <- st_nearest_feature(x = tract, y = s_BIP)
  nearestBIP <- s_BIP %>% slice(index)

  m_per_mi <- 1609.34 # converting meters to miles
  tract_distance <- data.frame(GEOID=tract$GEOID,
                               distanceBIP=as.numeric( st_distance(x = tract, y= nearestBIP, by_element = TRUE) ) / m_per_mi,
                               nearestProjectID=nearestBIP$ProjectID[1])
  return(tract_distance)
}

library(doParallel)
cl<-makeCluster(30)
registerDoParallel(cl)

strt<-Sys.time()

tract_distance <- foreach(k=1:nrow(tracts_us), .combine=rbind) %dopar% {
  distanceBIP(tract=tracts_us[k,], s_BIP=s_BIPonly)
}

print(Sys.time()-strt)
# turn off clusters
stopCluster(cl)

save.image("bip_intersections.RData")

# --------------------------------------------------------------------------------------------
# join tracts to area of intersection and distance to nearest BIP
# --------------------------------------------------------------------------------------------

tract_areas[,2:17] <- round(tract_areas[,2:17],6)

tract_data <- tracts_us %>% select(STATEFP, COUNTYFP, TRACTCE, GEOID, ALAND, AWATER,
                                   ProjectID_BIP, RUSID_TIL, RUSID_FBB, RUSID_CCG) %>% as.data.frame(.) %>% select(-geometry)
tract_data <- tract_data %>% left_join(tract_distance, by="GEOID")
tract_data <- tract_data %>% left_join(tract_areas, by="GEOID")

fwrite(tract_data,file="tract_areas2.csv")

# add some checks:
#   proportions of all areas sum to 1 to 8 decimal places
table( rowSums(tract_areas[,-1]) )
#   histogram of areas > 0 (xlim from 0 to 1)
par(mfrow=c(4,4))
for(i in 1:16){
  tract_areas[,i+1] <- as.numeric(tract_areas[,i+1])
  hist(tract_areas[,(i+1)][tract_areas[,(i+1)] > 0], xlim=c(0,1),
       main=names(tract_areas)[i+1], xlab="", ylab="")
}
#   histogram of distance to nearest BIP
hist(tract_data$distanceBIP, xlab="Distance to Nearest BIP Region", ylab="Number of Tracts", main="")
# large distances are in Alaska + Hawaii


