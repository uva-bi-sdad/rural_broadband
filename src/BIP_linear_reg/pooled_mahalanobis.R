# Pooled regression using 1-1 Mahalnobis matching

# Anil’s idea to try using propensity score matching to select similar houses inside and outside the BIP PSA,
# in addition to OLS regressions. For example, you could use 1:1 or n:1 matching (controls : treated) and then run
# the regression on the matched sample, or use matching with auxiliary regressions.  Both of those methods account for
# bias due to imperfect matches. There are some technical problems in using the PSM approach followed by regressions on
# the matched sample. In a two-stage estimation approach like that, the standard errors should reflect the two-stage design
# of the analysis, which is difficult to do analytically. Typically researchers will use bootstrapping to numerically estimate
# standard errors in such two stage estimation. But Abadie and Imbens have proven that bootstrapping does not yield a consistent
# estimator in these models. They have an alternative estimator that does not use propensity score matching, but uses Mahalanobis
# metric (and some other metrics) matching and has a bias correction based on auxiliary regressions to address bias caused by
# imperfect matches on observed covariates. The standard errors using Abadie and Imbens’ bias correcting matching estimator are
# supposed to be correct.

# methods described by Abadie and Imbens (2006, 2016) and implemented in the "Matching" R package:
# Covariate matching including propensity score, Mahalanobis and inverse variance matching. The function is intended
# to be used in conjunction with the MatchBalance function which determines the extent to which Match has been able to
# achieve covariate balance. In order to do propensity score matching, one should estimate the propensity model before calling
# Match, and then send Match the propensity score to use.

# Match arguments
# Y = outcome (log property value)
# Tr = treatment (in-out of BIP)
# X = matrix of variables to match on (observed housing covariates)
# Z = matrix of covariates to make bias adjustments
# V = matrix of covariates for which variance of the causal effect may vary
# estimand: 'ATE' = average treatment effect
# ties=FALSE, replace=FALSE (speeds up calculation)
# exact = logical vector for X matrix; TRUE for exact matching
# Weight: 1 for inverse variance, 2 for Mahalanobis distance

# Match outputs
# est = average treatment effect
# se = Abadie-Imbens standard error

# Look at windows 0-10 miles, 5-15 miles, 10-20 miles of service area for comparators
# Show the results (row=year, col=interaction coefficient with BIP + s.e.)
# Regression
# Regression, tract fixed effects
# Mahalanobis matching with exact matching on county (add this), bedrooms, baths,
    # non-exact matching on age, sqft_ratio, land_square_footage, living_area_feet (Mahalanobis distance, Weight=2)
# standard errors calculated according to Abadie-Imbens
# produce tables + plots (9 models total)


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

dataset <- readRDS(paste0(path, "BIP_linear_models/housing_BIP_060622.RDS"))

#hist(dataset$dist_bip, main="Distance to BIP", xlab="dist(mi)")
#sum(dataset$dist_bip < 10) # 1.05m sales observed under 10 miles from BIP (vs 300k previously)

#########################################
# DESIGN MATRIX
#########################################

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(dataset$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

Xmat <- cbind(1,
              dataset$bip,
              dataset$age, # age of the house
              dataset$nbaths, # number of baths
              dataset$sqft_ratio, # living square ft/building square ft.
              log(dataset$living_square_feet), # living sq ft 
              log(dataset$land_square_footage), # land suqare footage
              dataset$bedrooms, 
              dataset$ftth, 
              dataset$wireless)
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
levels.x <- names(table(as.character(dataset$transaction_type), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dataset$transaction_type)==x)
  })) 
  cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
}

# primary sale price code
levels.x <- names(table(as.character(dataset$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dataset$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

# award quantiles 
levels.x <- names(table(as.character(dataset$quantiles), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dataset$quantiles)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("quantiles:",levels.x[-1],sep=""))
}

# interactions of program dummies with year dummies:

if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(dataset$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(dataset$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(dataset$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(dataset$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}


# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip","age","nbaths","sqft_ratio", "land_square_footage", "living_area_ft", "bedrooms",
                    "ftth", "wireless", cnames)


####################################
# REGRESSION MODELS
####################################

####################################
# LINEAR MODEL
####################################

dist_bip <- dataset$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )

get_lmresults <- function(filter){
  dataset_f <- dataset[filter,]; Xmat_f <- Xmat[filter,]
  # pooled regression with technology and award quantiles, no acs, no zoning and no bldg code, no prog ids, no census tract dummies
  model = try(lm(log(dataset_f$sale_price) ~ -1 + Xmat_f))
  model_summ = coef(summary(model))
  rownames(model_summ) = sapply(rownames(model_summ), function(k) substr(k, start = 5, stop = nchar(k)))
  model_summ <- cbind(" "=rownames(model_summ), model_summ)
  model_summ <- cbind(model_summ, confint(model, level = 0.90))
  return(model_summ)  
}
lm_results <- list()
lm_results[[1]] <- get_lmresults(close_filter)
lm_results[[2]] <- get_lmresults(med_filter)
lm_results[[3]] <- get_lmresults(far_filter)

saveRDS(lm_results,file="BIP_linear_reg/lm_results.RDS")

####################################
# LINEAR MODEL w/ fixed tract effects
####################################

# with census tract dummies
dataset$geoid_tr <- substr(dataset$geoid_blk,1,11)

# length(unique(dataset$geoid_tr))
# 7011 tracts

# restrict to tracts that have at least 10 sales inside and outside the BIP
dataset_tract <- dataset %>% group_by(geoid_tr) %>% summarize(n_in=sum(bip==1),n_out=sum(bip==0))
tracts_include <- (dataset_tract %>% filter(n_in >=10 & n_out >=10))$geoid_tr # only 461 tracts
ind_include <- which(dataset$geoid_tr %in% tracts_include)
dataset2 <- dataset[ind_include,]
Xmat2 <- Xmat[ind_include,]

dist_bip <- dataset2$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )

get_lmfixedresults <- function(filter){
  dataset_f <- dataset2[filter,]; Xmat_f <- Xmat2[filter,]
  model_tract = try(lm(log(dataset_f$sale_price) ~ -1 + Xmat_f + dataset_f$geoid_tr-1), silent = T)
  model_summ_tr = coef(summary(model_tract))
  rownames(model_summ_tr) = sapply(rownames(model_summ_tr), function(k) substr(k, start = 5, stop = nchar(k)))
  model_summ_tr <- cbind(" "=rownames(model_summ_tr), model_summ_tr)
  confint <- as.data.frame(confint(model_tract, level = 0.90))
  model_summ_tr <- as.data.frame(model_summ_tr)
  model_tr_res <- list(model_summ_tr, confint) %>% 
    map(~ .x %>% 
          as.data.frame %>%
          rownames_to_column('rn')) %>% 
    reduce(left_join, by = 'rn') %>%
    column_to_rownames('rn')
  return(model_tr_res)
}
lm_fixedresults <- list()
lm_fixedresults[[1]] <- get_lmfixedresults(close_filter)
lm_fixedresults[[2]] <- get_lmfixedresults(med_filter)
lm_fixedresults[[3]] <- get_lmfixedresults(far_filter)

saveRDS(lm_fixedresults,file="BIP_linear_reg/lm_fixedresults.RDS")

#write_xlsx(list("model" = as.data.frame(model_summ), "model_tract" = as.data.frame(model_tr_res)), 
#           paste0(path, "pool_res_10mi_052022.xlsx"))

####################################
# MATCHING
####################################

# Mahalanobis matching with exact matching on rusid, bedrooms, baths,
# non-exact matching on age, sqft_ratio, land_square_footage, living_area_feet (Mahalanobis distance, Weight=2)
dist_bip <- dataset$dist_bip
Y <- log(dataset$sale_price)
Tr <- dataset$bip
Xmatch <- dataset %>% dplyr::select(rusid, bedrooms, nbaths, age, sqft_ratio, land_square_footage, living_square_feet)
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
#model_match <- list()
model_match <- readRDS("BIP_linear_reg/model_match.RDS")

matchfun <- function(k){
  m.out <- Match(Y=Y[filters[[k]]], Tr=Tr[filters[[k]]], X=Xmatch[filters[[k]],],
                            exact=c(rep('TRUE',3),rep('FALSE',4)), Weight=2,
                            ties=FALSE, replace=FALSE,version="fast")
  return(m.out)
}

# 12, 18 have too many samples causing matching to hang; random sample 300,000 from these
filters[[12]] <- filters[[12]][sample(x=1:length(filters[[12]]), size=3e5, replace=FALSE)]
filters[[18]] <- filters[[18]][sample(x=1:length(filters[[18]]), size=3e5, replace=FALSE)]

#system.time( model_match[[2]] <- matchfun(2) )
#saveRDS(model_match,"BIP_linear_reg/model_match.RDS")
for(k in 12:length(filters)){
  model_match[[k]] <- matchfun(k)
  saveRDS(model_match,"BIP_linear_reg/model_match.RDS")
}

#model_match <- readRDS("BIP_linear_reg/model_match.RDS")
model_match_df <- data.frame(
  dist = rep(c("0_10mi","5_15mi","10_20mi"),each=6),
  year = rep(c("05_06","07_08","09_10","11_12","13_14","15+"),3),
  est = sapply( model_match, function(x){x$est} ),
  se = sapply( model_match, function(x){x$se.standard} )
)
fwrite(model_match_df,"BIP_linear_reg/model_match.csv",row.names = FALSE)






