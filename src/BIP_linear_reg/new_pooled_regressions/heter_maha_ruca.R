# Heterogenous effects in Mahalanobis matching: RUCA codes

# packages
library(data.table)
library(readr)
library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
library(haven)
library(Matching)
library(tibble)
library(purrr)

# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"
dataset <- readRDS(paste0(path, "BIP_linear_data/housing_BIP_clean_011122.RDS"))
dataset <- dataset[, 1:47]

#################
# RUCA areas
#################

# join in RUCA codes 
ruca2010 <- read_csv("~/git/rural_broadband/src/ruca2010r.csv",
                     col_types = c(State_County_Tract_FIPS="character"))

ruca2010 <- ruca2010 %>% dplyr::select(State_County_Tract_FIPS,RUCA_2010='Primary_RUCA_2010',
                                       RUCA_2010_SECONDARY='Secondary_RUCA_2010', State)

# census tract geoids
geoid_tr  <- substr(dataset$geoid_blk,1,11)
dataset <- cbind(dataset, geoid_tr)

# add rural/urban ares to acs estimates and MOEs
dataset <- dataset %>% left_join(ruca2010, by=c("geoid_tr"="State_County_Tract_FIPS"))

# define urban and rural areas based on RUCC and RUCA codes values
# beased on https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/
dataset <- dataset %>% mutate(
  RUCA_METRO = case_when( RUCA_2010 <=3 ~ 1,
                          TRUE ~ 0 ),
  RUCA_MICRO = case_when( RUCA_2010 %in% c(4,5,6) ~ 1,
                          TRUE ~ 0 ),
  RUCA_RURAL = case_when( RUCA_2010 >6 ~ 1,
                          TRUE ~ 0 ))
######################
# METRO
######################

metro <- dataset %>% filter(RUCA_METRO == 1)

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(metro$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

# Mahalanobis matching with exact matching on rusid, bedrooms, baths,
# non-exact matching on age, sqft_ratio, land_square_footage, living_area_feet (Mahalanobis distance, Weight=2)
dist_bip <- metro$dist_bip
Y <- log(metro$sale_price)
Tr <- metro$bip
Xmatch <- metro %>% dplyr::select(rusid, bedrooms, nbaths, age, sqft_ratio, land_square_footage, living_square_feet)
Xmatch$rusid <- as.numeric( unclass(factor(Xmatch$rusid)) ) # convert to numeric for exact matching

close_filter0 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==0 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )
close_filter1 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==1 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )
close_filter2 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==2 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )
close_filter3 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==3 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )
close_filter4 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==4 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )
close_filter5 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==5 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )

med_filter0 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==0 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )
med_filter1 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==1 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )
med_filter2 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==2 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )
med_filter3 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==3 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )
med_filter4 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==4 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )
med_filter5 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==5 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )

far_filter0 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==0 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )
far_filter1 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==1 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )
far_filter2 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==2 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )
far_filter3 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==3 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )
far_filter4 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==4 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )
far_filter5 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==5 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )

filters <- list(close_filter0,close_filter1,close_filter2,close_filter3,close_filter4,close_filter5,
                med_filter0,med_filter1,med_filter2,med_filter3,med_filter4,med_filter5,
                far_filter0,far_filter1,far_filter2,far_filter3,far_filter4,far_filter5)

matchfun <- function(k){
  m.out <- Match(Y=Y[filters[[k]]], Tr=Tr[filters[[k]]], X=Xmatch[filters[[k]],],
                 exact=c(rep('TRUE',3),rep('FALSE',4)), Weight=2,
                 ties=FALSE, replace=FALSE)
  return(m.out)
}

model_match <- list()

for(k in 1:length(filters)){
  print(paste0("Working on filter: ",k))
  model_match[[k]] <- matchfun(k)
  saveRDS(model_match,"BIP_linear_reg/heter_metro_match.RDS")
}

model_match_df <- data.frame(
  dist = rep(c("0_10mi","5_15mi","10_20mi"),each=6),
  year = rep(c("05_06","07_08","09_10","11_12","13_14","15+"),3),
  est = sapply( model_match, function(x){x$est} ),
  #ai_se = sapply( model_match, function(x){x$se} ),
  se = sapply( model_match, function(x){x$se.standard} ), 
  wnobs = sapply( model_match, function(x){x$wnobs} ),
  nobs = sapply( model_match, function(x){x$nobs})
)
fwrite(model_match_df,"BIP_linear_reg/metro_match.csv",row.names = FALSE)

########################
# MICRO
########################

micro <- dataset %>% filter(RUCA_MICRO == 1)

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(micro$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

# Mahalanobis matching with exact matching on rusid, bedrooms, baths,
# non-exact matching on age, sqft_ratio, land_square_footage, living_area_feet (Mahalanobis distance, Weight=2)
dist_bip <- micro$dist_bip
Y <- log(micro$sale_price)
Tr <- micro$bip
Xmatch <- micro %>% dplyr::select(rusid, bedrooms, nbaths, age, sqft_ratio, land_square_footage, living_square_feet)
Xmatch$rusid <- as.numeric( unclass(factor(Xmatch$rusid)) ) # convert to numeric for exact matching

close_filter0 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==0 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )
close_filter1 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==1 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )
close_filter2 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==2 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )
close_filter3 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==3 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )
close_filter4 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==4 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )
close_filter5 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==5 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )

med_filter0 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==0 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )
med_filter1 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==1 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )
med_filter2 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==2 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )
med_filter3 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==3 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )
med_filter4 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==4 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )
med_filter5 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==5 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )

far_filter0 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==0 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )
far_filter1 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==1 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )
far_filter2 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==2 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )
far_filter3 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==3 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )
far_filter4 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==4 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )
far_filter5 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==5 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )

filters <- list(close_filter0,close_filter1,close_filter2,close_filter3,close_filter4,close_filter5,
                med_filter0,med_filter1,med_filter2,med_filter3,med_filter4,med_filter5,
                far_filter0,far_filter1,far_filter2,far_filter3,far_filter4,far_filter5)

model_match <- list()

for(k in 1:length(filters)){
  print(paste0("Working on filter: ",k))
  model_match[[k]] <- matchfun(k)
  saveRDS(model_match,"BIP_linear_reg/heter_micro_match.RDS")
}

model_match_df <- data.frame(
  dist = rep(c("0_10mi","5_15mi","10_20mi"),each=6),
  year = rep(c("05_06","07_08","09_10","11_12","13_14","15+"),3),
  est = sapply( model_match, function(x){x$est} ),
  #ai_se = sapply( model_match, function(x){x$se} ),
  se = sapply( model_match, function(x){x$se.standard} ), 
  wnobs = sapply( model_match, function(x){x$wnobs} ),
  nobs = sapply( model_match, function(x){x$nobs})
)
fwrite(model_match_df,"BIP_linear_reg/micro_match.csv",row.names = FALSE)

##########################
# RURAL
##########################

rural <- dataset %>% filter(RUCA_RURAL == 1)

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(rural$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

# Mahalanobis matching with exact matching on rusid, bedrooms, baths,
# non-exact matching on age, sqft_ratio, land_square_footage, living_area_feet (Mahalanobis distance, Weight=2)
dist_bip <- rural$dist_bip
Y <- log(rural$sale_price)
Tr <- rural$bip
Xmatch <- rural %>% dplyr::select(rusid, bedrooms, nbaths, age, sqft_ratio, land_square_footage, living_square_feet)
Xmatch$rusid <- as.numeric( unclass(factor(Xmatch$rusid)) ) # convert to numeric for exact matching

close_filter0 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==0 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )
close_filter1 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==1 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )
close_filter2 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==2 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )
close_filter3 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==3 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )
close_filter4 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==4 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )
close_filter5 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==5 & (dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10)) )

med_filter0 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==0 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )
med_filter1 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==1 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )
med_filter2 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==2 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )
med_filter3 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==3 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )
med_filter4 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==4 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )
med_filter5 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==5 & (dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15)) )

far_filter0 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==0 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )
far_filter1 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==1 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )
far_filter2 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==2 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )
far_filter3 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==3 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )
far_filter4 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==4 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )
far_filter5 <- which( rowSums(is.na(Xmatch)) == 0 & year.class==5 & (dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20)) )

filters <- list(close_filter0,close_filter1,close_filter2,close_filter3,close_filter4,close_filter5,
                med_filter0,med_filter1,med_filter2,med_filter3,med_filter4,med_filter5,
                far_filter0,far_filter1,far_filter2,far_filter3,far_filter4,far_filter5)

model_match <- list()

for(k in 1:length(filters)){
  print(paste0("Working on filter: ",k))
  model_match[[k]] <- matchfun(k)
  saveRDS(model_match,"BIP_linear_reg/heter_rural_match.RDS")
}

model_match_df <- data.frame(
  dist = rep(c("0_10mi","5_15mi","10_20mi"),each=6),
  year = rep(c("05_06","07_08","09_10","11_12","13_14","15+"),3),
  est = sapply( model_match, function(x){x$est} ),
  #ai_se = sapply( model_match, function(x){x$se} ),
  se = sapply( model_match, function(x){x$se.standard} ), 
  wnobs = sapply( model_match, function(x){x$wnobs} ),
  nobs = sapply( model_match, function(x){x$nobs})
)
fwrite(model_match_df,"BIP_linear_reg/rural_match.csv",row.names = FALSE)

