# Heter Effects DID FEs: Tech Types

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
library(foreach)

# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

dataset <- readRDS(paste0(path, "BIP_linear_models/housing_BIP_060622.RDS"))

##########################
# TECH SAMPLE REGRESSIONS
##########################

###########################
# FTTH
###########################

ftth_df <- dataset %>% filter(ftth == 1)

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(ftth_df$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

Xmat <- cbind(1,
              ftth_df$bip,
              ftth_df$age, # age of the house
              ftth_df$nbaths, # number of baths
              ftth_df$sqft_ratio, # living square ft/building square ft.
              log(ftth_df$living_square_feet), # living sq ft 
              log(ftth_df$land_square_footage), # land suqare footage
              ftth_df$bedrooms)
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
levels.x <- names(table(as.character(ftth_df$transaction_type), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(ftth_df$transaction_type)==x)
  })) 
  cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
}

# primary sale price code
levels.x <- names(table(as.character(ftth_df$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(ftth_df$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

# award quantiles 
levels.x <- names(table(as.character(ftth_df$quantiles), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(ftth_df$quantiles)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("quantiles:",levels.x[-1],sep=""))
}

# interactions of program dummies with year dummies:

if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(Xmat[, 9 : (9 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(ftth_df$bip) * Xmat[, 9 : (9 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 9 : (9 + id.yr - 2)], 2, function(k) as.numeric(ftth_df$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 9 : (9 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(ftth_df$bip) * Xmat[, 9 : (9 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 9 : (9 + id.yr - 2)], 2, function(k) as.numeric(ftth_df$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip","age","nbaths","sqft_ratio", "land_square_footage", 
                    "living_area_ft", "bedrooms", cnames)

#############################
# FIXED EFFECTS
#############################

# with census tract dummies
ftth_df$geoid_tr <- substr(ftth_df$geoid_blk,1,11)

# length(unique(dataset$geoid_tr))
# 7011 tracts

# restrict to tracts that have at least 10 sales inside and outside the BIP
#dataset_tract <- ftth_df %>% group_by(geoid_tr) %>% summarize(n_in=sum(bip==1),n_out=sum(bip==0))
#tracts_include <- (dataset_tract %>% filter(n_in >=10 & n_out >=10))$geoid_tr # only 461 tracts
#ind_include <- which(ftth_df$geoid_tr %in% tracts_include)
#dataset2 <- ftth_df[ind_include,]
#Xmat2 <- Xmat[ind_include,]

dist_bip <- ftth_df$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )

get_lmfixedresults <- function(filter){
  dataset_f <- ftth_df[filter,]; Xmat_f <- Xmat[filter,]
  model_tract = try(lm(log(dataset_f$sale_price) ~ -1 + Xmat_f + dataset_f$geoid_tr-1), silent = T)
  model_summ_tr = coef(summary(model_tract))
  rownames(model_summ_tr) = sapply(rownames(model_summ_tr), function(k) substr(k, start = 7, stop = nchar(k)))
  model_summ_tr <- cbind(" "=rownames(model_summ_tr), model_summ_tr)
  confint <- as.data.frame(confint(model_tract, level = 0.90))
  model_summ_tr <- as.data.frame(model_summ_tr)
  model_tr_res <- list(model_summ_tr, confint) %>% 
    map(~ .x %>% 
          as.data.frame %>%
          rownames_to_column('rn')) %>% 
    reduce(left_join, by = 'rn') %>%
    column_to_rownames('rn')
  nobs <- c("Nobs", nobs(model_tract))
  model_tr_res <- rbind(model_tr_res, nobs)
  return(model_tr_res)
}
lm_fixedresults <- list()

lm_fixedresults[[1]] <- get_lmfixedresults(close_filter)
lm_fixedresults[[2]] <- get_lmfixedresults(med_filter)
lm_fixedresults[[3]] <- get_lmfixedresults(far_filter)

saveRDS(lm_fixedresults,file="BIP_linear_reg/lm_fe_ftth.RDS")

############
# WIRELESS
############

wireless_df <- dataset %>% filter(wireless == 1)

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(wireless_df$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

Xmat <- cbind(1,
              wireless_df$bip,
              wireless_df$age, # age of the house
              wireless_df$nbaths, # number of baths
              wireless_df$sqft_ratio, # living square ft/building square ft.
              log(wireless_df$living_square_feet), # living sq ft 
              log(wireless_df$land_square_footage), # land suqare footage
              wireless_df$bedrooms)
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
levels.x <- names(table(as.character(wireless_df$transaction_type), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(wireless_df$transaction_type)==x)
  })) 
  cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
}

# primary sale price code
levels.x <- names(table(as.character(wireless_df$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(wireless_df$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

# award quantiles 
levels.x <- names(table(as.character(wireless_df$quantiles), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(wireless_df$quantiles)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("quantiles:",levels.x[-1],sep=""))
}

# interactions of program dummies with year dummies:

if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(Xmat[, 9 : (9 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(wireless_df$bip) * Xmat[, 9 : (9 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 9 : (9 + id.yr - 2)], 2, function(k) as.numeric(wireless_df$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 9 : (9 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(wireless_df$bip) * Xmat[, 9 : (9 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 9 : (9 + id.yr - 2)], 2, function(k) as.numeric(wireless_df$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip","age","nbaths","sqft_ratio", "land_square_footage", 
                    "living_area_ft", "bedrooms", cnames)

#############################
# FIXED EFFECTS
#############################

# with census tract dummies
wireless_df$geoid_tr <- substr(wireless_df$geoid_blk,1,11)


# restrict to tracts that have at least 10 sales inside and outside the BIP
#dataset_tract <- wireless_df %>% group_by(geoid_tr) %>% summarize(n_in=sum(bip==1),n_out=sum(bip==0))
#tracts_include <- (dataset_tract %>% filter(n_in >=10 & n_out >=10))$geoid_tr # only 461 tracts
#ind_include <- which(wireless_df$geoid_tr %in% tracts_include)
#dataset2 <- wireless_df[ind_include,]
#Xmat2 <- Xmat[ind_include,]

dist_bip <- wireless_df$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )

get_lmfixedresults <- function(filter){
  dataset_f <- wireless_df[filter,]; Xmat_f <- Xmat[filter,]
  model_tract = try(lm(log(dataset_f$sale_price) ~ -1 + Xmat_f + dataset_f$geoid_tr-1), silent = T)
  model_summ_tr = coef(summary(model_tract))
  rownames(model_summ_tr) = sapply(rownames(model_summ_tr), function(k) substr(k, start = 7, stop = nchar(k)))
  model_summ_tr <- cbind(" "=rownames(model_summ_tr), model_summ_tr)
  confint <- as.data.frame(confint(model_tract, level = 0.90))
  model_summ_tr <- as.data.frame(model_summ_tr)
  model_tr_res <- list(model_summ_tr, confint) %>% 
    map(~ .x %>% 
          as.data.frame %>%
          rownames_to_column('rn')) %>% 
    reduce(left_join, by = 'rn') %>%
    column_to_rownames('rn')
  nobs <- c("Nobs", nobs(model_tract))
  model_tr_res <- rbind(model_tr_res, nobs)
  return(model_tr_res)
}
lm_fixedresults <- list()

lm_fixedresults[[1]] <- get_lmfixedresults(close_filter)
lm_fixedresults[[2]] <- get_lmfixedresults(med_filter)
lm_fixedresults[[3]] <- get_lmfixedresults(far_filter)

saveRDS(lm_fixedresults,file="BIP_linear_reg/lm_fe_wireless.RDS")

################
# DSL
################

dsl_df <- dataset %>% filter(dsl == 1)

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(dsl_df$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

Xmat <- cbind(1,
              dsl_df$bip,
              dsl_df$age, # age of the house
              dsl_df$nbaths, # number of baths
              dsl_df$sqft_ratio, # living square ft/building square ft.
              log(dsl_df$living_square_feet), # living sq ft 
              log(dsl_df$land_square_footage), # land suqare footage
              dsl_df$bedrooms)
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
levels.x <- names(table(as.character(dsl_df$transaction_type), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dsl_df$transaction_type)==x)
  })) 
  cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
}

# primary sale price code
levels.x <- names(table(as.character(dsl_df$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dsl_df$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

# award quantiles 
levels.x <- names(table(as.character(dsl_df$quantiles), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dsl_df$quantiles)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("quantiles:",levels.x[-1],sep=""))
}

# interactions of program dummies with year dummies:

if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(Xmat[, 9 : (9 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(dsl_df$bip) * Xmat[, 9 : (9 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 9 : (9 + id.yr - 2)], 2, function(k) as.numeric(dsl_df$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 9 : (9 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(dsl_df$bip) * Xmat[, 9 : (9 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 9 : (9 + id.yr - 2)], 2, function(k) as.numeric(dsl_df$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip","age","nbaths","sqft_ratio", "land_square_footage", 
                    "living_area_ft", "bedrooms", cnames)

#####################
# FIXED EFFECTS
#####################

# with census tract dummies
dsl_df$geoid_tr <- substr(dsl_df$geoid_blk,1,11)

# length(unique(dataset$geoid_tr))
# 7011 tracts

# restrict to tracts that have at least 10 sales inside and outside the BIP
#dataset_tract <- dsl_df %>% group_by(geoid_tr) %>% summarize(n_in=sum(bip==1),n_out=sum(bip==0))
#tracts_include <- (dataset_tract %>% filter(n_in >=10 & n_out >=10))$geoid_tr # only 461 tracts
#ind_include <- which(dsl_df$geoid_tr %in% tracts_include)
#dataset2 <- dsl_df[ind_include,]
#Xmat2 <- Xmat[ind_include,]

dist_bip <- dsl_df$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )

get_lmfixedresults <- function(filter){
  dataset_f <- dsl_df[filter,]; Xmat_f <- Xmat[filter,]
  model_tract = try(lm(log(dataset_f$sale_price) ~ -1 + Xmat_f + dataset_f$geoid_tr-1), silent = T)
  model_summ_tr = coef(summary(model_tract))
  rownames(model_summ_tr) = sapply(rownames(model_summ_tr), function(k) substr(k, start = 7, stop = nchar(k)))
  model_summ_tr <- cbind(" "=rownames(model_summ_tr), model_summ_tr)
  confint <- as.data.frame(confint(model_tract, level = 0.90))
  model_summ_tr <- as.data.frame(model_summ_tr)
  model_tr_res <- list(model_summ_tr, confint) %>% 
    map(~ .x %>% 
          as.data.frame %>%
          rownames_to_column('rn')) %>% 
    reduce(left_join, by = 'rn') %>%
    column_to_rownames('rn')
  nobs <- c("Nobs", nobs(model_tract))
  model_tr_res <- rbind(model_tr_res, nobs)
  return(model_tr_res)
}
lm_fixedresults <- list()

lm_fixedresults[[1]] <- get_lmfixedresults(close_filter)
lm_fixedresults[[2]] <- get_lmfixedresults(med_filter)
lm_fixedresults[[3]] <- get_lmfixedresults(far_filter)

saveRDS(lm_fixedresults,file="BIP_linear_reg/lm_fe_dsl.RDS")
