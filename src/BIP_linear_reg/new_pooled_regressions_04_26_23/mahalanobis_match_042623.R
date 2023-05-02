# 04/26/23
# re-fit Mahalanobis models using completed BIP regions
# then run DID regressions on the matched sample


# data with complete BIP regions (Xin):
# /project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/BIP_linear_data/houseBIP_rusid1_v2.RDS

# code for Mahalanobis matching analysis (Hanna):

# src/BIP_linear_reg/new_pooled_regressions
# Files List: 
#  did_with_fe_new.R DID regressions with tract FEs for 10mi, 15 and 20mi radius
#  maha_matching_new.R Matching estimator for 10, 15 and 20mi, also saves results for DID regressions on matched sample

# pooled_on_matched_10mi.R DID regression on matched sample with tract FEs 10 mi 
# pooled_on_matched_15mi.R --- 15 mi
# pooled_on_matched_20mi.R --- 20 mi

# heter_didfe_award.R DID regressions with tract FEs for 10, 15, 20mi by award quantile
# heter_didfe_ruca.R --- by rurality 
# heter_didfe_tech.R --- by technology type

# heter_maha_award.R matching estimator for 10, 15, 20 mi by award quantiles
# heter_maha_ruca.R --- rurality
# heter_maha_tech.R --- technology type

# heter_pool_10_award.R DID regression on matched sample 10 mi by award quantiles
# heter_pool_10_ruca.R --- rurality
# heter_pool_10_tech.R --- technology

library(sf)
library(dplyr)
library(Matching)
library(data.table)

setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

# complete BIPs dataset (50 BIPs)
dataset <- readRDS(paste0(path,"BIP_linear_data/houseBIP_rusid1_v2.RDS"))

# rejoin dist_bip from old file by rusid (project id), p_id_iris_frmtd (property indicator), sale_date
old_dataset <- readRDS(paste0(path, "BIP_linear_data/housing_BIP_clean_011122.RDS"))
old_dataset <- old_dataset %>% dplyr::select(p_id_iris_frmtd, sale_date, rusid, dist_bip, geoid_blk)
dataset$sale_date <- as.Date(dataset$sale_date, format="%Y%m%d")
dataset <- dataset %>% left_join(old_dataset,by=c("p_id_iris_frmtd", "sale_date", "rusid"))



# Mahalanobis matching with exact matching on rusid, bedrooms, baths,
# non-exact matching on age, sqft_ratio, land_square_footage, living_area_feet (Mahalanobis distance, Weight=2)
dist_bip <- dataset$dist_bip
Y <- log(dataset$sale_price)
Tr <- dataset$bip
Xmatch <- dataset %>% dplyr::select(rusid, bedrooms, nbaths, age, sqft_ratio, land_square_footage, living_square_feet)
Xmatch$rusid <- as.numeric( unclass(factor(Xmatch$rusid)) ) # convert to numeric for exact matching

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(dataset$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

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


matchfun <- function(k){
  m.out <- Match(Y=Y[filters[[k]]], Tr=Tr[filters[[k]]], X=Xmatch[filters[[k]],],
                 exact=c(rep('TRUE',3),rep('FALSE',4)), Weight=2,
                 ties=FALSE, replace=FALSE)
  return(m.out)
}


for(k in 1:length(filters)){
  print(paste0("Working on filter: ",k))
  model_match[[k]] <- matchfun(k)
  saveRDS(model_match,"BIP_linear_reg/new_pooled_regressions_04_26_23/model_match.RDS")
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
fwrite(model_match_df,"BIP_linear_reg/new_pooled_regressions_04_26_23/model_match_df.csv",row.names = FALSE)


