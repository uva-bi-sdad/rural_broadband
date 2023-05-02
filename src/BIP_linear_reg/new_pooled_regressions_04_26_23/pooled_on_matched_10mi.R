# 4/26/23 Pooled regression on the matched housing data

# load the matched model
model_match <- readRDS("model_match.RDS")

match_data <- list()

# for each model get treated and control sample
#for(k in 1:length(filters)){
for(k in 1:6){ 
  data_f <- dataset[filters[[k]],]
  ind_treat <- model_match[[k]]$index.treated
  ind_cont <- model_match[[k]]$index.control
  data_treat <- cbind(data_f[ind_treat,], paste0(ind_treat, "+", ind_cont))
  data_cont <- cbind(data_f[ind_cont,], paste0(ind_treat, "+", ind_cont))
  #data <- rbind(data_f[ind_treat],data_f[ind_cont])
  #data <- rbind(data_treat,data_cont, fill=T)
  data <- rbind(data_treat,data_cont)
  match_data <- rbind(match_data, data)
}

names(match_data)[43] <- "match_id"

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

old_dataset <- readRDS(paste0(path, "BIP_linear_data/housing_BIP_clean_011122.RDS"))
old_dataset <- old_dataset %>% dplyr::select(p_id_iris_frmtd, sale_date, rusid, geoid_blk)
match_data <- match_data %>% left_join(old_dataset,by=c("p_id_iris_frmtd", "sale_date", "rusid"))

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

library(lmtest)

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
  
saveRDS(model_res,file="BIP_linear_reg/lm_fe_matched_robust10.RDS")




