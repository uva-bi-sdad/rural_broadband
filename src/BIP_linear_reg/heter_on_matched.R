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

# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

# load housing data
dataset <- readRDS(paste0(path, "BIP_linear_models/housing_BIP_060622.RDS"))

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
dist_bip <- dataset$dist_bip
Y <- log(dataset$sale_price)
Tr <- dataset$bip
Xmatch <- dataset %>% dplyr::select(rusid, bedrooms, nbaths, age, sqft_ratio, land_square_footage, 
                                    living_square_feet)
Xextra <- dataset %>% dplyr::select(geoid_blk, sale_year, ftth, wireless, dsl, transaction_type,
                                    pri_cat_code, quantiles)
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

# load the matched model
model_match <- readRDS("BIP_linear_reg/model_match.RDS")

match_data <- list()
# for each model get treated and control sample
for(k in 1:length(filters)){
  data_f <- cbind(Y[filters[[k]]],Tr[filters[[k]]], dist_bip[filters[[k]]], Xextra[filters[[k]]])
  ind_treat <- model_match[[k]]$index.treated
  ind_cont <- model_match[[k]]$index.control
  data <- rbind(data_f[ind_treat], data_f[ind_cont])
  match_data <- rbind(match_data, data)
}
match_data <-  match_data[rowSums(is.na(match_data)) == 0]

names(match_data)[1] <- "log_sale_price"
names(match_data)[2] <- "inside_bip"
names(match_data)[3] <- "dist_bip"

#######################
# FTTH
#######################
ftth_df <- match_data %>% filter(ftth == 1)

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
              ftth_df$inside_bip)
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
    if(is.null(ncol(Xmat[, 5 : (5 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(ftth_df$inside_bip) * Xmat[, 5 : (5 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 5 : (5 + id.yr - 2)], 2, function(k) as.numeric(ftth_df$inside_bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 5 : (5 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(ftth_df$inside_bip) * Xmat[, 5 : (5 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 5 : (5 + id.yr - 2)], 2, function(k) as.numeric(ftth_df$inside_bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip", cnames)

# with census tract dummies
ftth_df$geoid_tr <- substr(ftth_df$geoid_blk,1,11)

# length(unique(ftth_df$geoid_tr))

# restrict to tracts that have at least 10 sales inside and outside the BIP
dataset_tract <- ftth_df %>% group_by(geoid_tr) %>% summarize(n_in=sum(inside_bip==1),n_out=sum(inside_bip==0))
tracts_include <- (dataset_tract %>% filter(n_in >=10 & n_out >=10))$geoid_tr # only 461 tracts
ind_include <- which(ftth_df$geoid_tr %in% tracts_include)
dataset2 <- ftth_df[ind_include,]
Xmat2 <- Xmat[ind_include,]

dist_bip <- dataset2$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )

get_lmfixedresults <- function(filter){
  dataset_f <- dataset2[filter,]; Xmat_f <- Xmat2[filter,]
  model_tract = try(lm(log(dataset_f$log_sale_price) ~ -1 + Xmat_f + dataset_f$geoid_tr-1), silent = T)
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

saveRDS(lm_fixedresults,file="BIP_linear_reg/fe_matched_ftth.RDS")

######################
# WIRELESS
######################

wire_df <- match_data %>% filter(wireless == 1)

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(wire_df$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

Xmat <- cbind(1,
              wire_df$inside_bip)
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
levels.x <- names(table(as.character(wire_df$transaction_type), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(wire_df$transaction_type)==x)
  })) 
  cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
}

# primary sale price code
levels.x <- names(table(as.character(wire_df$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(wire_df$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

# award quantiles 
levels.x <- names(table(as.character(wire_df$quantiles), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(wire_df$quantiles)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("quantiles:",levels.x[-1],sep=""))
}

# interactions of program dummies with year dummies:

if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(Xmat[, 5 : (5 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(wire_df$inside_bip) * Xmat[, 5 : (5 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 5 : (5 + id.yr - 2)], 2, function(k) as.numeric(wire_df$inside_bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 5 : (5 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(wire_df$inside_bip) * Xmat[, 5 : (5 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 5 : (5 + id.yr - 2)], 2, function(k) as.numeric(wire_df$inside_bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip", cnames)

# with census tract dummies
wire_df$geoid_tr <- substr(wire_df$geoid_blk,1,11)

# length(unique(wire_df$geoid_tr))

# restrict to tracts that have at least 10 sales inside and outside the BIP
dataset_tract <- wire_df %>% group_by(geoid_tr) %>% summarize(n_in=sum(inside_bip==1),n_out=sum(inside_bip==0))
tracts_include <- (dataset_tract %>% filter(n_in >=10 & n_out >=10))$geoid_tr # only 461 tracts
ind_include <- which(ftth_df$geoid_tr %in% tracts_include)
dataset2 <- ftth_df[ind_include,]
Xmat2 <- Xmat[ind_include,]

dist_bip <- dataset2$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )

get_lmfixedresults <- function(filter){
  dataset_f <- dataset2[filter,]; Xmat_f <- Xmat2[filter,]
  model_tract = try(lm(log(dataset_f$log_sale_price) ~ -1 + Xmat_f + dataset_f$geoid_tr-1), silent = T)
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

saveRDS(lm_fixedresults,file="BIP_linear_reg/fe_matched_wire.RDS")

######################
# DSL
######################

dsl_df <- match_data %>% filter(dsl == 1)

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
              dsl_df$inside_bip)
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
    if(is.null(ncol(Xmat[, 5 : (5 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(dsl_df$inside_bip) * Xmat[, 5 : (5 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 5 : (5 + id.yr - 2)], 2, function(k) as.numeric(dsl_df$inside_bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 5 : (5 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(dsl_df$inside_bip) * Xmat[, 5 : (5 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 5 : (5 + id.yr - 2)], 2, function(k) as.numeric(dsl_df$inside_bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip", cnames)

# with census tract dummies
dsl_df$geoid_tr <- substr(dsl_df$geoid_blk,1,11)

# length(unique(dsl_df$geoid_tr))

# restrict to tracts that have at least 10 sales inside and outside the BIP
dataset_tract <- dsl_df %>% group_by(geoid_tr) %>% summarize(n_in=sum(inside_bip==1),n_out=sum(inside_bip==0))
tracts_include <- (dataset_tract %>% filter(n_in >=10 & n_out >=10))$geoid_tr # only 461 tracts
ind_include <- which(dsl_df$geoid_tr %in% tracts_include)
dataset2 <- dsl_df[ind_include,]
Xmat2 <- Xmat[ind_include,]

dist_bip <- dataset2$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )

get_lmfixedresults <- function(filter){
  dataset_f <- dataset2[filter,]; Xmat_f <- Xmat2[filter,]
  model_tract = try(lm(log(dataset_f$log_sale_price) ~ -1 + Xmat_f + dataset_f$geoid_tr-1), silent = T)
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

saveRDS(lm_fixedresults,file="BIP_linear_reg/fe_matched_dsl.RDS")

#####################
# QUANTILE 1
#####################

quant1 <- match_data %>% filter(quantiles == 1)

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
              quant1$inside_bip,
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
    if(is.null(ncol(Xmat[, 5 : (5 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(quant1$inside_bip) * Xmat[, 5 : (5 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 5 : (5 + id.yr - 2)], 2, function(k) as.numeric(quant1$inside_bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 5 : (5 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(quant1$inside_bip) * Xmat[, 5 : (5 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 5 : (5 + id.yr - 2)], 2, function(k) as.numeric(quant1$inside_bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip",
                    "ftth", "wireless", cnames)

# with census tract dummies
quant1$geoid_tr <- substr(quant1$geoid_blk,1,11)

# length(unique(quant1$geoid_tr))

# restrict to tracts that have at least 10 sales inside and outside the BIP
dataset_tract <- quant1 %>% group_by(geoid_tr) %>% summarize(n_in=sum(inside_bip==1),n_out=sum(inside_bip==0))
tracts_include <- (dataset_tract %>% filter(n_in >=10 & n_out >=10))$geoid_tr # only 461 tracts
ind_include <- which(quant1$geoid_tr %in% tracts_include)
dataset2 <- quant1[ind_include,]
Xmat2 <- Xmat[ind_include,]

dist_bip <- dataset2$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )

get_lmfixedresults <- function(filter){
  dataset_f <- dataset2[filter,]; Xmat_f <- Xmat2[filter,]
  model_tract = try(lm(log(dataset_f$log_sale_price) ~ -1 + Xmat_f + dataset_f$geoid_tr-1), silent = T)
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

saveRDS(lm_fixedresults,file="BIP_linear_reg/fe_matched_quant1.RDS")

################
# QUANTILE 2
################

quant2 <- match_data %>% filter(quantiles == 2)

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
              quant2$inside_bip,
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
    if(is.null(ncol(Xmat[, 5 : (5 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(quant2$inside_bip) * Xmat[, 5 : (5 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 5 : (5 + id.yr - 2)], 2, function(k) as.numeric(quant2$inside_bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 5 : (5 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(quant2$inside_bip) * Xmat[, 5 : (5 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 5 : (5 + id.yr - 2)], 2, function(k) as.numeric(quant2$inside_bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip",
                    "ftth", "wireless", cnames)

# with census tract dummies
quant2$geoid_tr <- substr(quant2$geoid_blk,1,11)

# length(unique(quant2$geoid_tr))

# restrict to tracts that have at least 10 sales inside and outside the BIP
dataset_tract <- quant2 %>% group_by(geoid_tr) %>% summarize(n_in=sum(inside_bip==1),n_out=sum(inside_bip==0))
tracts_include <- (dataset_tract %>% filter(n_in >=10 & n_out >=10))$geoid_tr # only 461 tracts
ind_include <- which(quant2$geoid_tr %in% tracts_include)
dataset2 <- quant2[ind_include,]
Xmat2 <- Xmat[ind_include,]

dist_bip <- dataset2$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )

get_lmfixedresults <- function(filter){
  dataset_f <- dataset2[filter,]; Xmat_f <- Xmat2[filter,]
  model_tract = try(lm(log(dataset_f$log_sale_price) ~ -1 + Xmat_f + dataset_f$geoid_tr-1), silent = T)
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

saveRDS(lm_fixedresults,file="BIP_linear_reg/fe_matched_quant2.RDS")

########################
# QUANTILE 3
########################

quant3 <- match_data %>% filter(quantiles == 3)

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
              quant3$inside_bip,
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
    if(is.null(ncol(Xmat[, 5 : (5 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(quant3$inside_bip) * Xmat[, 5 : (5 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 5 : (5 + id.yr - 2)], 2, function(k) as.numeric(quant3$inside_bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 5 : (5 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(quant3$inside_bip) * Xmat[, 5 : (5 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 5 : (5 + id.yr - 2)], 2, function(k) as.numeric(quant3$inside_bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip",
                    "ftth", "wireless", cnames)

# with census tract dummies
quant3$geoid_tr <- substr(quant3$geoid_blk,1,11)

# length(unique(quant3$geoid_tr))

# restrict to tracts that have at least 10 sales inside and outside the BIP
dataset_tract <- quant3 %>% group_by(geoid_tr) %>% summarize(n_in=sum(inside_bip==1),n_out=sum(inside_bip==0))
tracts_include <- (dataset_tract %>% filter(n_in >=10 & n_out >=10))$geoid_tr # only 461 tracts
ind_include <- which(quant3$geoid_tr %in% tracts_include)
dataset2 <- quant3[ind_include,]
Xmat2 <- Xmat[ind_include,]

dist_bip <- dataset2$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )

get_lmfixedresults <- function(filter){
  dataset_f <- dataset2[filter,]; Xmat_f <- Xmat2[filter,]
  model_tract = try(lm(log(dataset_f$log_sale_price) ~ -1 + Xmat_f + dataset_f$geoid_tr-1), silent = T)
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

saveRDS(lm_fixedresults,file="BIP_linear_reg/fe_matched_quant3.RDS")


