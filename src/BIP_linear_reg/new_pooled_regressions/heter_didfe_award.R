# Heter Effects DID FEs: Award Amount

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

#####################################
# AWARD QUANTILES SAMPLE REGRESSIONS
#####################################

#######################
# first quantile
#######################

quant1 <- dataset %>% filter(quantiles == 1)

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(quant1$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

Xmat <- cbind(1,
              quant1$bip,
              quant1$age, # age of the house
              quant1$nbaths, # number of baths
              quant1$sqft_ratio, # living square ft/building square ft.
              log(quant1$living_square_feet), # living sq ft 
              log(quant1$land_square_footage), # land suqare footage
              quant1$bedrooms,
              quant1$ftth,
              quant1$wireless)
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
levels.x <- names(table(as.character(quant1$transaction_type), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(quant1$transaction_type)==x)
  })) 
  cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
}

# primary sale price code
levels.x <- names(table(as.character(quant1$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(quant1$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

# interactions of program dummies with year dummies:
if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(quant1$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(quant1$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(quant1$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(quant1$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip","age","nbaths","sqft_ratio", "land_square_footage", 
                    "living_area_ft", "bedrooms", "ftth", "wireless", cnames)

####################################
# FIXED EFFECTS
####################################
# with census tract dummies
quant1$geoid_tr <- substr(quant1$geoid_blk,1,11)

# restrict to tracts that have at least 10 sales inside and outside the BIP
#dataset_tract <- quant1 %>% group_by(geoid_tr) %>% summarize(n_in=sum(bip==1),n_out=sum(bip==0))
#tracts_include <- (dataset_tract %>% filter(n_in >=10 & n_out >=10))$geoid_tr # only 461 tracts
#ind_include <- which(quant1$geoid_tr %in% tracts_include)
#dataset2 <- quant1[ind_include,]
#Xmat2 <- Xmat[ind_include,]

dist_bip <- quant1$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )

get_lmfixedresults <- function(filter){
  dataset_f <- quant1[filter,]; Xmat_f <- Xmat[filter,]
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

saveRDS(lm_fixedresults,file="BIP_linear_reg/lm_fe_quant1.RDS")

##################
# second quantile
##################

quant2 <- dataset %>% filter(quantiles == 2)

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(quant2$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

Xmat <- cbind(1,
              quant2$bip,
              quant2$age, # age of the house
              quant2$nbaths, # number of baths
              quant2$sqft_ratio, # living square ft/building square ft.
              log(quant2$living_square_feet), # living sq ft 
              log(quant2$land_square_footage), # land suqare footage
              quant2$bedrooms,
              quant2$ftth,
              quant2$wireless)
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
levels.x <- names(table(as.character(quant2$transaction_type), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(quant2$transaction_type)==x)
  })) 
  cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
}

# primary sale price code
levels.x <- names(table(as.character(quant2$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(quant2$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

# interactions of program dummies with year dummies:
if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(quant2$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(quant2$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(quant2$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(quant2$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip","age","nbaths","sqft_ratio", "land_square_footage", 
                    "living_area_ft", "bedrooms", "ftth", "wireless", cnames)

####################################
# FIXED EFFECTS
####################################
# with census tract dummies
quant2$geoid_tr <- substr(quant2$geoid_blk,1,11)

# restrict to tracts that have at least 10 sales inside and outside the BIP
#dataset_tract <- quant2 %>% group_by(geoid_tr) %>% summarize(n_in=sum(bip==1),n_out=sum(bip==0))
#tracts_include <- (dataset_tract %>% filter(n_in >=10 & n_out >=10))$geoid_tr # only 461 tracts
#ind_include <- which(quant2$geoid_tr %in% tracts_include)
#dataset2 <- quant2[ind_include,]
#Xmat2 <- Xmat[ind_include,]

dist_bip <- quant2$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )

get_lmfixedresults <- function(filter){
  dataset_f <- quant2[filter,]; Xmat_f <- Xmat[filter,]
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

saveRDS(lm_fixedresults,file="BIP_linear_reg/lm_fe_quant2.RDS")

#######################
# third award quantile
#######################

quant3 <- dataset %>% filter(quantiles == 3)

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(quant3$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

Xmat <- cbind(1,
              quant3$bip,
              quant3$age, # age of the house
              quant3$nbaths, # number of baths
              quant3$sqft_ratio, # living square ft/building square ft.
              log(quant3$living_square_feet), # living sq ft 
              log(quant3$land_square_footage), # land suqare footage
              quant3$bedrooms,
              quant3$ftth,
              quant3$wireless)
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
levels.x <- names(table(as.character(quant3$transaction_type), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(quant3$transaction_type)==x)
  })) 
  cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
}

# primary sale price code
levels.x <- names(table(as.character(quant3$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(quant3$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

# interactions of program dummies with year dummies:
if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(quant3$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(quant3$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(quant3$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(quant3$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip","age","nbaths","sqft_ratio", "land_square_footage", 
                    "living_area_ft", "bedrooms", "ftth", "wireless", cnames)

####################################
# FIXED EFFECTS
####################################

# with census tract dummies
quant3$geoid_tr <- substr(quant3$geoid_blk,1,11)

# restrict to tracts that have at least 10 sales inside and outside the BIP
#dataset_tract <- quant3 %>% group_by(geoid_tr) %>% summarize(n_in=sum(bip==1),n_out=sum(bip==0))
#tracts_include <- (dataset_tract %>% filter(n_in >=10 & n_out >=10))$geoid_tr # only 461 tracts
#ind_include <- which(quant3$geoid_tr %in% tracts_include)
#dataset2 <- quant3[ind_include,]
#Xmat2 <- Xmat[ind_include,]

dist_bip <- quant3$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )

get_lmfixedresults <- function(filter){
  dataset_f <- quant3[filter,]; Xmat_f <- Xmat[filter,]
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

saveRDS(lm_fixedresults,file="BIP_linear_reg/lm_fe_quant3.RDS")

