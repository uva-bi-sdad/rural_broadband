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

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(dataset$sale_year, function(k){
  if(k >= 2005 & k < 2007) return(0)
  if(k >= 2007 & k < 2009) return(1)
  if(k >= 2009 & k < 2011) return(2)
  if(k >= 2011 & k < 2013) return(3)
  if(k >= 2013 & k < 2015) return(4)
  if(k >= 2015) return(5)
}))

# check all the covars for missiningness
table(is.na(dataset$bip))
table(is.na(dataset$age))
table(is.na(dataset$nbaths))
table(is.na(dataset$sqft_ratio))
table(is.na(log(dataset$living_square_feet)))
dataset <- dataset %>% filter(!is.na(log(living_square_feet)))
table(is.na(log(dataset$land_square_footage)))
dataset <- dataset %>% filter(!is.na(log(land_square_footage)))
table(is.na(dataset$bedrooms))
table(is.na(dataset$ftth))
table(is.na(dataset$wireless))


levels.x <- names(table(year.class, useNA = "ifany"))
id.yr = length(levels.x)
flag.in = 0 %in% levels.x

cnames <- colnames(dataset)
# year dummies
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  dataset <- cbind(dataset,sapply(levels.x[-1], function(x){
    as.numeric(as.character(year.class)==x)
  })) 
  cnames <- c(cnames,paste("year.cl:",levels.x[-1],sep=""))
}

# transaction type dummies
levels.x <- names(table(as.character(dataset$transaction_type), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  dataset <-cbind(dataset,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dataset$transaction_type)==x)
  })) 
  cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
}

# primary sale price code
levels.x <- names(table(as.character(dataset$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  dataset <-cbind(dataset,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dataset$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

# award quantiles 
levels.x <- names(table(as.character(dataset$quantiles), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  dataset <-cbind(dataset,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dataset$quantiles)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("quantiles:",levels.x[-1],sep=""))
}

if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(dataset[, 48 : (48 + id.yr - 2)]))){
      dataset <- cbind(dataset, as.numeric(dataset$bip) * dataset[, 48 : (48 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[48 : 52],"xbip", sep = ""))
    }else{
      dataset <- cbind(dataset, apply(dataset[, 48 : (48 + id.yr - 2)], 2, function(k) as.numeric(dataset$bip) * k))
      cnames <- c(cnames, paste(cnames[48 : 52],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(dataset[, 48 : (48 + id.yr - 2)]))){
      dataset <- cbind(dataset, as.numeric(dataset$bip) * dataset[, 48 : (48 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[48 : 52],"xbip", sep = "")) 
    }else{
      dataset <- cbind(dataset, apply(dataset[, 48 : (48 + id.yr - 2)], 2, function(k) as.numeric(dataset$bip) * k))
      cnames <- c(cnames, paste(cnames[48 : 52],"xbip", sep = ""))  
    }
  }
}
# add names to covar matrix col
colnames(dataset) <- c(cnames)

# check all the covars for missiningness
table(is.na(dataset$`year.cl:1`))
table(is.na(dataset$`year.cl:2`))
table(is.na(dataset$`year.cl:3`))
table(is.na(dataset$`year.cl:4`))
table(is.na(dataset$`year.cl:5`))
table(is.na(dataset$`transaction_type:3`))
table(is.na(dataset$`quantiles:2`))
table(is.na(dataset$`quantiles:3`))
table(is.na(dataset$`year.cl:1xbip`))
table(is.na(dataset$`year.cl:2xbip`))
table(is.na(dataset$`year.cl:3xbip`))
table(is.na(dataset$`year.cl:4xbip`))
table(is.na(dataset$`year.cl:5xbip`))

table(is.na(dataset$geoid_blk))
dataset <- dataset %>% filter(!is.na(dataset$geoid_blk))

# save the new dataset 
saveRDS(dataset, paste0(path, "BIP_linear_data/housing_BIP_clean_011122.RDS"))
