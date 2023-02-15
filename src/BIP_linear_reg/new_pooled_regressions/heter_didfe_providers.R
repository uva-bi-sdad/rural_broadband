# Heter Effects DID FEs: Number of Providers 

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
# get the tract ID
dataset$geoid_tract <- substr(dataset$geoid_blk,1,11)
dataset$geoid_tract <- as.character(as.numeric(dataset$geoid_tract))

#elig_df <- read_csv(paste0(path, "Community_Connect_Summaries/CC2010_census_block_eligibility.csv"))

elig_df <- read_csv(paste0(path, "BIP_linear_data/broadband_tract_eligibility12-15-20.csv"))
elig_df <- elig_df[,c("GEOID", "fcc2011_providers_200", "fcc2011_providers_3")]
elig_df$GEOID <- as.character(elig_df$GEOID)

# merge to the dataset 
dataset <- left_join(dataset, elig_df, by=c("geoid_tract" = "GEOID"))

############################################
# NO PROVIDERS WITH MORE THAN 3MBTS IN 2011 
############################################

#provider0 <- dataset %>% filter(fcc2011_providers_200 == 0 & fcc2011_providers_3 == 0)
#provider200 <- dataset %>% filter(fcc2011_providers_200 == 0)
provider3 <- dataset %>% filter(fcc2011_providers_3 == 0)

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(provider3$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

Xmat <- cbind(1,
              provider3$bip,
              provider3$age, # age of the house
              provider3$nbaths, # number of baths
              provider3$sqft_ratio, # living square ft/building square ft.
              log(provider3$living_square_feet), # living sq ft 
              log(provider3$land_square_footage), # land suqare footage
              provider3$bedrooms,
              provider3$ftth,
              provider3$wireless)
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
levels.x <- names(table(as.character(provider3$transaction_type), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(provider3$transaction_type)==x)
  })) 
  cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
}

# primary sale price code
levels.x <- names(table(as.character(provider3$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(provider3$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

# award quantiles
levels.x <- names(table(as.character(provider3$quantiles), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(provider3$quantiles)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("quantiles:",levels.x[-1],sep=""))
}


# interactions of program dummies with year dummies:
if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(provider3$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(provider3$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(provider3$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(provider3$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip","age","nbaths","sqft_ratio", "land_square_footage", 
                    "living_area_ft", "bedrooms", "ftth", "wireless", cnames)

#########################
# FIXED EFFECTS
#########################

dist_bip <- provider3$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )

get_lmfixedresults <- function(filter){
  dataset_f <- provider3[filter,]; Xmat_f <- Xmat[filter,]
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

saveRDS(lm_fixedresults,file="BIP_linear_reg/lm_fe_provider3.RDS")

#######################
# 1 PROVIDER
