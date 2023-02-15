# Pooled regression on the matched housing data

# packages
library(readr)
library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
library(haven)
library(tidycensus)
library(tigris)
library(miceadds)
library(lmtest)
library(sandwich)

# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

# load housing data
dataset <- readRDS(paste0(path, "BIP_linear_data/housing_BIP_clean_011122.RDS"))
dataset <- dataset[, 1:47]

# year-groupings
year.class = unlist(sapply(dataset$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

# create the filters 
close_filter0 <- which( year.class==0 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 0 & dataset$dist_bip <= 10)) )
close_filter1 <- which( year.class==1 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 0 & dataset$dist_bip <= 10)) )
close_filter2 <- which( year.class==2 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 0 & dataset$dist_bip <= 10)) )
close_filter3 <- which( year.class==3 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 0 & dataset$dist_bip <= 10)) )
close_filter4 <- which( year.class==4 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 0 & dataset$dist_bip <= 10)) )
close_filter5 <- which( year.class==5 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 0 & dataset$dist_bip <= 10)) )

med_filter0 <- which( year.class==0 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 5 & dataset$dist_bip <= 15)) )
med_filter1 <- which( year.class==1 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 5 & dataset$dist_bip <= 15)) )
med_filter2 <- which( year.class==2 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 5 & dataset$dist_bip <= 15)) )
med_filter3 <- which( year.class==3 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 5 & dataset$dist_bip <= 15)) )
med_filter4 <- which( year.class==4 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 5 & dataset$dist_bip <= 15)) )
med_filter5 <- which( year.class==5 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 5 & dataset$dist_bip <= 15)) )

far_filter0 <- which( year.class==0 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 10 & dataset$dist_bip <= 20)) )
far_filter1 <- which( year.class==1 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 10 & dataset$dist_bip <= 20)) )
far_filter2 <- which( year.class==2 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 10 & dataset$dist_bip <= 20)) )
far_filter3 <- which( year.class==3 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 10 & dataset$dist_bip <= 20)) )
far_filter4 <- which( year.class==4 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 10 & dataset$dist_bip <= 20)) )
far_filter5 <- which( year.class==5 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 10 & dataset$dist_bip <= 20)) )

filters <- list(close_filter0,close_filter1,close_filter2,close_filter3,close_filter4,close_filter5,
                med_filter0,med_filter1,med_filter2,med_filter3,med_filter4,med_filter5,
                far_filter0,far_filter1,far_filter2,far_filter3,far_filter4,far_filter5)


# load the matched model
model_match <- readRDS("BIP_linear_reg/model_match.RDS")

match_data <- list()

# for each model get treated and control sample
#for(k in 1:length(filters)){
for(k in 1:6){ 
  data_f <- dataset[filters[[k]]]
  ind_treat <- model_match[[k]]$index.treated
  ind_cont <- model_match[[k]]$index.control
  data_treat <- cbind(data_f[ind_treat], paste0(ind_treat, "+", ind_cont))
  data_cont <- cbind(data_f[ind_cont], paste0(ind_treat, "+", ind_cont))
  #data <- rbind(data_f[ind_treat],data_f[ind_cont])
  data <- rbind(data_treat,data_cont, fill=T)
  match_data <- rbind(match_data, data)
}

names(match_data)[48] <- "match_id"

# add match pair id
match_data <- match_data %>%
  group_by(match_id) %>%
  dplyr::mutate(match_group = cur_group_id())
# drop if there is no pair
no_pairs <- match_data %>% group_by(match_id) %>% summarise(count_obs = n()) %>% subset(count_obs <2)
match_data <- match_data[!(match_data$match_id %in% no_pairs$match_id),]

############################
# NEW CONTROLS MATRIX
############################

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(match_data$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

Xmat <- cbind(1,
              match_data$bip,
              match_data$ftth, 
              match_data$wireless)
cnames <- c() 

levels.x <- names(table(year.class, useNA = "ifany"))
id.yr = length(levels.x)
flag.in = 0 %in% levels.x

# year dummies
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <- cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(year.class)==x)
  })) 
  cnames <- c(cnames,paste("year.cl:",levels.x[-1],sep=""))
}

# transaction type dummies
levels.x <- names(table(as.character(match_data$transaction_type), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(match_data$transaction_type)==x)
  })) 
  cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
}

# primary sale price code
levels.x <- names(table(as.character(match_data$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(match_data$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

# award quantiles 
levels.x <- names(table(as.character(match_data$quantiles), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(match_data$quantiles)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("quantiles:",levels.x[-1],sep=""))
}

# building codes 
levels.x <- names(table(as.character(match_data$bldg_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(match_data$bldg_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("bldg_code:",levels.x[-1],sep=""))
}

# interactions of program dummies with year dummies:

if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(Xmat[, 5 : (5 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(match_data$bip) * Xmat[, 5 : (5 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 5 : (5 + id.yr - 2)], 2, function(k) as.numeric(match_data$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 5 : (5 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(match_data$bip) * Xmat[, 5 : (5 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 5 : (5 + id.yr - 2)], 2, function(k) as.numeric(match_data$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip",
                    "ftth", "wireless", cnames)

#########################
# FIXED EFFECTS
#########################

# with census tract dummies
match_data$geoid_tr <- substr(match_data$geoid_blk,1,11)

# length(unique(match_data$geoid_tr))

# restrict to tracts that have at least 10 sales inside and outside the BIP
#dataset_tract <- match_data %>% group_by(geoid_tr) %>% summarize(n_in=sum(bip==1),n_out=sum(bip==0))
#tracts_include <- (dataset_tract %>% filter(n_in >=10 & n_out >=10))$geoid_tr # only 461 tracts
#ind_include <- which(match_data$geoid_tr %in% tracts_include)
#dataset2 <- match_data[ind_include,]
#Xmat2 <- Xmat[ind_include,]

dist_bip <- match_data$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )


dataset_f <- match_data[close_filter,]; Xmat_f <- Xmat[close_filter,]

model_tract = lm(data=dataset_f, log(dataset_f$sale_price) ~ -1 + Xmat_f + dataset_f$geoid_tr-1)

# clustered standard errors
model_cl <- coeftest(model_tract, vcov = vcovCL, cluster = ~match_group)
# confidence intervals
model_cis <- coefci(model_tract, vcov = vcovCL,level = 0.90,
                 cluster = ~match_group)
model_cl <- as.data.frame(model_cl[,1:4])
model_cis <- as.data.frame(model_cis[,1:2])
model_res <- cbind(model_cl, model_cis)

# another approach with a wrapper function // gives the same result
# fit <- miceadds::lm.cluster(data=dataset_f, 
#                     log(dataset_f$log_sale_price) ~ -1 + Xmat_f + dataset_f$geoid_tr-1, 
#                     cluster=dataset_f$match_group)

# number of observations
nobs <- c("Nobs", nobs(model_tract), rep("", 5))
model_res <- rbind(model_res, nobs)
  
saveRDS(model_res,file="BIP_linear_reg/lm_fe_matched_robust10_bldg_codes.RDS")




