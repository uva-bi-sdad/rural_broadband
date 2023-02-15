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
Xextra <- dataset %>% dplyr::select(geoid_blk, sale_year, ftth, wireless, transaction_type,
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

close_filters <- list(close_filter0,close_filter1,close_filter2,close_filter3,close_filter4,close_filter5)
med_filters <- list(med_filter0,med_filter1,med_filter2,med_filter3,med_filter4,med_filter5)
far_filters <- list(far_filter0,far_filter1,far_filter2,far_filter3,far_filter4,far_filter5)

# load the matched model
model_match <- readRDS("BIP_linear_reg/model_match.RDS")

match_data <- list()

# for each model get treated and control sample
for(k in 1:length(close_filters)){
  data_f <- cbind(Y[close_filters[[k]]],Tr[close_filters[[k]]], dist_bip[close_filters[[k]]], Xextra[close_filters[[k]]])
  ind_treat <- model_match[[k]]$index.treated
  ind_cont <- model_match[[k]]$index.control
  data_treat <- cbind(data_f[ind_treat], paste0(ind_treat, "+", ind_cont))
  data_cont <- cbind(data_f[ind_cont], paste0(ind_treat, "+", ind_cont))
  #data <- rbind(data_f[ind_treat],data_f[ind_cont])
  data <- rbind(data_treat,data_cont, fill=T)
  match_data <- rbind(match_data, data)
}
match_data <-  match_data[rowSums(is.na(match_data)) == 0]

names(match_data)[1] <- "log_sale_price"
names(match_data)[2] <- "inside_bip"
names(match_data)[3] <- "dist_bip"
names(match_data)[11] <- "match_id"

# add macth pair id
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
              match_data$inside_bip,
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

# interactions of program dummies with year dummies:

if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(Xmat[, 5 : (5 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(match_data$inside_bip) * Xmat[, 5 : (5 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 5 : (5 + id.yr - 2)], 2, function(k) as.numeric(match_data$inside_bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 5 : (5 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(match_data$inside_bip) * Xmat[, 5 : (5 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 5 : (5 + id.yr - 2)], 2, function(k) as.numeric(match_data$inside_bip) * k))
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
dataset_tract <- match_data %>% group_by(geoid_tr) %>% summarize(n_in=sum(inside_bip==1),n_out=sum(inside_bip==0))
tracts_include <- (dataset_tract %>% filter(n_in >=10 & n_out >=10))$geoid_tr # only 461 tracts
ind_include <- which(match_data$geoid_tr %in% tracts_include)
dataset2 <- match_data[ind_include,]
Xmat2 <- Xmat[ind_include,]

dist_bip <- dataset2$dist_bip
close_filter <- which( dist_bip <= 0 | ( dist_bip >= 0 & dist_bip <= 10) )
med_filter <- which( dist_bip <= 0 | ( dist_bip >= 5 & dist_bip <= 15) )
far_filter <- which( dist_bip <= 0 | ( dist_bip >= 10 & dist_bip <= 20) )


dataset_f <- dataset2[close_filter,]; Xmat_f <- Xmat2[close_filter,]

model_tract = lm(data=dataset_f, log(dataset_f$log_sale_price) ~ -1 + Xmat_f + dataset_f$geoid_tr-1)

# clustered standard errors
model_cl <- coeftest(model_tract, vcov = vcovCL, cluster = ~match_group)
# confidence intervals
model_cis <- coefci(model_tract, vcov = vcovCL,
                 cluster = ~match_group)
model_cl <- as.data.frame(model_cl[,1:4])
model_cis <- as.data.frame(model_cis[,1:2])

# another approach with a wrapper function // gives the same result
# fit <- miceadds::lm.cluster(data=dataset_f, 
#                     log(dataset_f$log_sale_price) ~ -1 + Xmat_f + dataset_f$geoid_tr-1, 
#                     cluster=dataset_f$match_group)

# coeficients
model_summ_tr = coef(summary(model_tract))
rownames(model_summ_tr) = sapply(rownames(model_summ_tr), function(k) substr(k, start = 7, stop = nchar(k)))
model_summ_tr <- cbind(" "=rownames(model_summ_tr), model_summ_tr)
  
# confidence intervals
confint <- as.data.frame(confint(model_tract, level = 0.90))
model_summ_tr <- as.data.frame(model_summ_tr)
  
  #model_tr_res <- list(model_summ_tr, confint) %>% 
  #map(~ .x %>% 
  #        as.data.frame %>%
  #        rownames_to_column('rn')) %>% 
  #  reduce(left_join, by = 'rn') %>%
  #  column_to_rownames('rn')
  
  # number of observations
  nobs <- c("Nobs", nobs(model_tract), rep("", 5))
  model_tr_res <- rbind(model_tr_res, nobs)
  
lm_fixedresults <- list()

lm_fixedresults[[1]] <- get_lmfixedresults(close_filter)
lm_fixedresults[[2]] <- get_lmfixedresults(med_filter)
lm_fixedresults[[3]] <- get_lmfixedresults(far_filter)

saveRDS(lm_fixedresults,file="BIP_linear_reg/lm_fe_matched.RDS")



