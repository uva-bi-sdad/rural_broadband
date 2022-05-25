# Pooled regression with new tech definitions

# packages
library(readr)
library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
library(haven)

# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/BIP_linear_data/"

dataset <- read_csv(paste0(path, "bip_all_projects.csv"))

dataset <- dataset %>% filter(dist_bip <= 10)

##########################
# TECH SAMPLE REGRESSIONS
##########################

###########################
# FTTH

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

# pooled regression FFTH tech regression
ftth_reg = try(lm(log(ftth_df$sale_price) ~ -1 + Xmat))

ftth_reg_summ = coef(summary(ftth_reg))
rownames(ftth_reg_summ) = sapply(rownames(ftth_reg_summ), function(k) substr(k, start = 5, stop = nchar(k)))
ftth_reg_summ <- cbind(" "=rownames(ftth_reg_summ), ftth_reg_summ)

# with census tract dummies
ftth_df['geoid_tr'] <- substr(ftth_df$geoid_blk,1,11)
model_tract = try(lm(log(ftth_df$sale_price) ~ -1 + Xmat + ftth_df$geoid_tr-1), silent = T)

model_summ_tr = coef(summary(model_tract))
rownames(model_summ_tr) = sapply(rownames(model_summ_tr), function(k) substr(k, start = 5, stop = nchar(k)))
model_summ_tr <- cbind(" "=rownames(model_summ_tr), model_summ_tr)

####################
# wireless 

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

# pooled regression FTTH tech regression
wireless_reg = try(lm(log(wireless_df$sale_price) ~ -1 + Xmat))

wireless_reg_summ = coef(summary(wireless_reg))
rownames(wireless_reg_summ) = sapply(rownames(wireless_reg_summ), function(k) substr(k, start = 5, stop = nchar(k)))
wireless_reg_summ <- cbind(" "=rownames(wireless_reg_summ), wireless_reg_summ)

# with census tract dummies
wireless_df['geoid_tr'] <- substr(wireless_df$geoid_blk,1,11)
wireless_tract = try(lm(log(wireless_df$sale_price) ~ -1 + Xmat + wireless_df$geoid_tr-1), silent = T)

wireless_summ_tr = coef(summary(wireless_tract))
rownames(wireless_summ_tr) = sapply(rownames(wireless_summ_tr), function(k) substr(k, start = 5, stop = nchar(k)))
wireless_summ_tr <- cbind(" "=rownames(wireless_summ_tr), wireless_summ_tr)

#########################
# DSL

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

# pooled regression DSL tech regression
dsl_reg = try(lm(log(dsl_df$sale_price) ~ -1 + Xmat))

dsl_reg_summ = coef(summary(dsl_reg))
rownames(dsl_reg_summ) = sapply(rownames(dsl_reg_summ), function(k) substr(k, start = 5, stop = nchar(k)))
dsl_reg_summ <- cbind(" "=rownames(dsl_reg_summ), dsl_reg_summ)

# with census tract dummies
dsl_df['geoid_tr'] <- substr(dsl_df$geoid_blk,1,11)
dsl_tract = try(lm(log(dsl_df$sale_price) ~ -1 + Xmat + dsl_df$geoid_tr-1), silent = T)

dsl_summ_tr = coef(summary(dsl_tract))
rownames(dsl_summ_tr) = sapply(rownames(dsl_summ_tr), function(k) substr(k, start = 5, stop = nchar(k)))
dsl_summ_tr <- cbind(" "=rownames(dsl_summ_tr), dsl_summ_tr)

write_xlsx(list("ftth" = as.data.frame(ftth_reg_summ), "ftth_tract" = as.data.frame(model_summ_tr),
                "wireless" = as.data.frame(wireless_reg_summ), "wireless_tract" = as.data.frame(wireless_summ_tr),
                "dsl" = as.data.frame(dsl_reg_summ), "dsl_tract" = as.data.frame(dsl_summ_tr)), 
           paste0(path, "tech_res_10mi_051022.xlsx"))

#######################
# QUANTILE REGRESSIONS
#######################

#######################
# first quantile

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

# pooled regression DSL tech regression
quant1_reg = try(lm(log(quant1$sale_price) ~ -1 + Xmat))

quant1_reg_summ = coef(summary(quant1_reg))
rownames(quant1_reg_summ) = sapply(rownames(quant1_reg_summ), function(k) substr(k, start = 5, stop = nchar(k)))
quant1_reg_summ <- cbind(" "=rownames(quant1_reg_summ), quant1_reg_summ)

# with census tract dummies
quant1['geoid_tr'] <- substr(quant1$geoid_blk,1,11)
quant1_tract = try(lm(log(quant1$sale_price) ~ -1 + Xmat + quant1$geoid_tr-1), silent = T)

quant1_summ_tr = coef(summary(quant1_tract))
rownames(quant1_summ_tr) = sapply(rownames(quant1_summ_tr), function(k) substr(k, start = 5, stop = nchar(k)))
quant1_summ_tr <- cbind(" "=rownames(quant1_summ_tr), quant1_summ_tr)

#######################
# quantile 2

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

# pooled regression DSL tech regression
quant2_reg = try(lm(log(quant2$sale_price) ~ -1 + Xmat))

quant2_reg_summ = coef(summary(quant2_reg))
rownames(quant2_reg_summ) = sapply(rownames(quant2_reg_summ), function(k) substr(k, start = 5, stop = nchar(k)))
quant2_reg_summ <- cbind(" "=rownames(quant2_reg_summ), quant2_reg_summ)

# with census tract dummies
quant2['geoid_tr'] <- substr(quant2$geoid_blk,1,11)
quant2_tract = try(lm(log(quant2$sale_price) ~ -1 + Xmat + quant2$geoid_tr-1), silent = T)

quant2_summ_tr = coef(summary(quant2_tract))
rownames(quant2_summ_tr) = sapply(rownames(quant2_summ_tr), function(k) substr(k, start = 5, stop = nchar(k)))
quant2_summ_tr <- cbind(" "=rownames(quant2_summ_tr), quant2_summ_tr)


#######################
# quantile 3

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

# pooled regression DSL tech regression
quant3_reg = try(lm(log(quant3$sale_price) ~ -1 + Xmat))

quant3_reg_summ = coef(summary(quant3_reg))
rownames(quant3_reg_summ) = sapply(rownames(quant3_reg_summ), function(k) substr(k, start = 5, stop = nchar(k)))
quant3_reg_summ <- cbind(" "=rownames(quant3_reg_summ), quant3_reg_summ)

# with census tract dummies
quant3['geoid_tr'] <- substr(quant3$geoid_blk,1,11)
quant3_tract = try(lm(log(quant3$sale_price) ~ -1 + Xmat + quant3$geoid_tr-1), silent = T)

quant3_summ_tr = coef(summary(quant3_tract))
rownames(quant3_summ_tr) = sapply(rownames(quant3_summ_tr), function(k) substr(k, start = 5, stop = nchar(k)))
quant3_summ_tr <- cbind(" "=rownames(quant3_summ_tr), quant3_summ_tr)


write_xlsx(list("quantile1" = as.data.frame(quant1_reg_summ), "quantile1_tract" = as.data.frame(quant1_summ_tr),
                "quantile2" = as.data.frame(quant2_reg_summ), "quantile2_tract" = as.data.frame(quant2_summ_tr),
                "quantile3" = as.data.frame(quant3_reg_summ), "quantile3_tract" = as.data.frame(quant3_summ_tr)), 
           paste0(path, "quant_res_10mi_051022.xlsx"))

#################
# RUCA areas
#################

# join in RUCA codes 
ruca2010 <- read_csv("~/git/rural_broadband/src/ruca2010r.csv",
                     col_types = c(State_County_Tract_FIPS="character"))

ruca2010 <- ruca2010 %>% dplyr::select(State_County_Tract_FIPS,RUCA_2010='Primary_RUCA_2010',
                                       RUCA_2010_SECONDARY='Secondary_RUCA_2010', State)

# census tract geoids
dataset['geoid_tr'] <- substr(dataset$geoid_blk,1,11)

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
# Metro

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

Xmat <- cbind(1,
              metro$bip,
              metro$age, # age of the house
              metro$nbaths, # number of baths
              metro$sqft_ratio, # living square ft/building square ft.
              log(metro$living_square_feet), # living sq ft 
              log(metro$land_square_footage), # land suqare footage
              metro$bedrooms,
              metro$ftth,
              metro$wireless)
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
levels.x <- names(table(as.character(metro$transaction_type), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(metro$transaction_type)==x)
  })) 
  cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
}

# primary sale price code
levels.x <- names(table(as.character(metro$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(metro$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

# award quantiles
levels.x <- names(table(as.character(metro$quantiles), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(metro$quantiles)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("quantiles:",levels.x[-1],sep=""))
}


# interactions of program dummies with year dummies:
if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(metro$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(metro$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(metro$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(metro$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip","age","nbaths","sqft_ratio", "land_square_footage", 
                    "living_area_ft", "bedrooms", "ftth", "wireless", cnames)

# pooled regression DSL tech regression
metro_reg = try(lm(log(metro$sale_price) ~ -1 + Xmat))

metro_reg_summ = coef(summary(metro_reg))
rownames(metro_reg_summ) = sapply(rownames(metro_reg_summ), function(k) substr(k, start = 5, stop = nchar(k)))
metro_reg_summ <- cbind(" "=rownames(metro_reg_summ), metro_reg_summ)

# with census tract dummies
metro['geoid_tr'] <- substr(metro$geoid_blk,1,11)
metro_tract = try(lm(log(metro$sale_price) ~ -1 + Xmat + metro$geoid_tr-1), silent = T)

metro_summ_tr = coef(summary(metro_tract))
rownames(metro_summ_tr) = sapply(rownames(metro_summ_tr), function(k) substr(k, start = 5, stop = nchar(k)))
metro_summ_tr <- cbind(" "=rownames(metro_summ_tr), metro_summ_tr)


##########################
# micro

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

Xmat <- cbind(1,
              micro$bip,
              micro$age, # age of the house
              micro$nbaths, # number of baths
              micro$sqft_ratio, # living square ft/building square ft.
              log(micro$living_square_feet), # living sq ft 
              log(micro$land_square_footage), # land suqare footage
              micro$bedrooms,
              micro$ftth,
              micro$wireless)
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
levels.x <- names(table(as.character(micro$transaction_type), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(micro$transaction_type)==x)
  })) 
  cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
}

# primary sale price code
levels.x <- names(table(as.character(micro$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(micro$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

# award quantiles
levels.x <- names(table(as.character(micro$quantiles), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(micro$quantiles)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("quantiles:",levels.x[-1],sep=""))
}

# interactions of program dummies with year dummies:
if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(micro$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(micro$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(micro$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(micro$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip","age","nbaths","sqft_ratio", "land_square_footage", 
                    "living_area_ft", "bedrooms", "ftth", "wireless", cnames)

# pooled regression DSL tech regression
micro_reg = try(lm(log(micro$sale_price) ~ -1 + Xmat))

micro_reg_summ = coef(summary(micro_reg))
rownames(micro_reg_summ) = sapply(rownames(micro_reg_summ), function(k) substr(k, start = 5, stop = nchar(k)))
micro_reg_summ <- cbind(" "=rownames(micro_reg_summ), micro_reg_summ)

# with census tract dummies
micro['geoid_tr'] <- substr(micro$geoid_blk,1,11)
micro_tract = try(lm(log(micro$sale_price) ~ -1 + Xmat + micro$geoid_tr-1), silent = T)

micro_summ_tr = coef(summary(micro_tract))
rownames(micro_summ_tr) = sapply(rownames(micro_summ_tr), function(k) substr(k, start = 5, stop = nchar(k)))
micro_summ_tr <- cbind(" "=rownames(micro_summ_tr), micro_summ_tr)

##########################
# rural

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

Xmat <- cbind(1,
              rural$bip,
              rural$age, # age of the house
              rural$nbaths, # number of baths
              rural$sqft_ratio, # living square ft/building square ft.
              log(rural$living_square_feet), # living sq ft 
              log(rural$land_square_footage), # land suqare footage
              rural$bedrooms,
              rural$ftth,
              rural$wireless)
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
levels.x <- names(table(as.character(rural$transaction_type), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(rural$transaction_type)==x)
  })) 
  cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
}

# primary sale price code
levels.x <- names(table(as.character(rural$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(rural$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

# award quantiles
levels.x <- names(table(as.character(rural$quantiles), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(rural$quantiles)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("quantiles:",levels.x[-1],sep=""))
}

# interactions of program dummies with year dummies:
if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(rural$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(rural$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 11 : (11 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(rural$bip) * Xmat[, 11 : (11 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 11 : (11 + id.yr - 2)], 2, function(k) as.numeric(rural$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}

# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip","age","nbaths","sqft_ratio", "land_square_footage", 
                    "living_area_ft", "bedrooms", "ftth", "wireless", cnames)

# pooled regression DSL tech regression
rural_reg = try(lm(log(rural$sale_price) ~ -1 + Xmat))

rural_reg_summ = coef(summary(rural_reg))
rownames(rural_reg_summ) = sapply(rownames(rural_reg_summ), function(k) substr(k, start = 5, stop = nchar(k)))
rural_reg_summ <- cbind(" "=rownames(rural_reg_summ), rural_reg_summ)

# with census tract dummies
rural['geoid_tr'] <- substr(rural$geoid_blk,1,11)
rural_tract = try(lm(log(rural$sale_price) ~ -1 + Xmat + rural$geoid_tr-1), silent = T)

rural_summ_tr = coef(summary(rural_tract))
rownames(rural_summ_tr) = sapply(rownames(rural_summ_tr), function(k) substr(k, start = 5, stop = nchar(k)))
rural_summ_tr <- cbind(" "=rownames(rural_summ_tr), rural_summ_tr)

write_xlsx(list("metro" = as.data.frame(metro_reg_summ), "metro_tract" = as.data.frame(metro_summ_tr),
                "micro" = as.data.frame(micro_reg_summ), "micro_tract" = as.data.frame(micro_summ_tr),
                "rural" = as.data.frame(rural_reg_summ), "rural_tract" = as.data.frame(rural_summ_tr)), 
           paste0(path, "ruca_res_10mi_051022.xlsx"))

