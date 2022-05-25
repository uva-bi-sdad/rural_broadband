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

# BIP shapes
load(paste0(path, "BIP_New.rds"))

load("~/R/broadband/bip-state.rdata")
# load(paste0(path,"bip-state.RData"))

# load in project data spreadsheet
bip_project = readxl::read_xlsx("BIP_linear_reg/BIP_R1-R2AwardeeMaster_10-29-10_updNetObl_7-3-19 (1).xlsx", sheet = 2)
bip_project['rusid'] <- paste0(bip_project$`RUS Award No.`, "-", bip_project$`RUS\r\nLoan-Grant No.`)

qcut2 <- function(x, n) {
  findInterval(x, quantile(x, seq(0, 1, length = n + 1)), all.inside = T)
}
bip_project['quantiles'] <- qcut2(bip_project$`Budget/HH`, 3)

# DTA file with new technology definitions
tech_descr <- read_dta(paste0(path, "bip_completed_infrastructure_projects_master_file.dta"))
tech <- tech_descr %>% select(projectid, ftth_gpon, ftth_rfog, ftth_ptp, ftth, fixed_wireless, adsl,
                              vdsl, mobile_wireless, hfc_cable, power_line)

# use if required <- ACS eligibility data
if(!"bb_eligibility"%in% ls())bb_eligibility <- read.csv("~/git/rural_broadband/src/BIP_analysis/broadband_tract_eligibility12-15-20.csv",  
                                                         stringsAsFactors = FALSE)

p <- paste0(path, "housing_BIP_extended_bldg_codes_no_acs/")

# append all the data together
for (file_name in list.files(p)){
  print(file_name)
  # create the first data if no data exist yet
  if (!exists("dataset")){
    dataset <- read_xlsx(paste0(p, file_name), sheet = "data")
    bip_name_rusid <- strsplit(file_name, "[.]")[[1]][1]
    dataset['rusid'] <- bip_name_rusid
  }
  
  # if data already exist, then append it together
  if (exists("dataset")){
    tempory <-read_xlsx(paste0(p, file_name), sheet = "data")
    bip_name_rusid <- strsplit(file_name, "[.]")[[1]][1]
    tempory['rusid'] <- bip_name_rusid
    dataset <-unique(rbind(dataset, tempory))
    rm(tempory)
  }
}

# check if inside and out program are coded 0 and 1 correctly
#dataset %>% group_by(bip) %>% summarise(mean_dist = mean(dist_bip))

dataset <- left_join(dataset, bip_project[,c("rusid", "quantiles")])
dataset <- left_join(dataset, tech,  by = c("rusid" = "projectid"))

# drop sales before 2005
dataset <- dataset[dataset$sale_year >= 2005,]

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
              dataset$ftth_gpon, 
              #dataset$ftth_rfog,
              dataset$ftth_ptp,
              dataset$ftth,
              dataset$fixed_wireless,
              dataset$adsl,
              dataset$vdsl,
              dataset$mobile_wireless, 
              dataset$hfc_cable,
              dataset$power_line)
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
    if(is.null(ncol(Xmat[, 18 : (18 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(dataset$bip) * Xmat[, 18 : (18 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 18 : (18 + id.yr - 2)], 2, function(k) as.numeric(dataset$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 18 : (18 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(dataset$bip) * Xmat[, 18 : (18 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 18 : (18 + id.yr - 2)], 2, function(k) as.numeric(dataset$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}


# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip","age","nbaths","sqft_ratio", "land_square_footage", "living_area_ft", "bedrooms",
                    "ftth_grop", "ftth_ptp", "ftth", "fixed_wireless", 
                    "adsl", "vdsl", "mobile_wireless", "hfc_cable", "power_line", cnames)


####################################
# REGRESSION MODELS
####################################

# pooled regression with technology and award quantiles, no acs, no zoning and no bldg code, no prog ids, no census tract dummies
model = try(lm(log(dataset$sale_price) ~ -1 + Xmat))

model_summ = coef(summary(model))
rownames(model_summ) = sapply(rownames(model_summ), function(k) substr(k, start = 5, stop = nchar(k)))
model_summ <- cbind(" "=rownames(model_summ), model_summ)

# with census tract dummies
dataset['geoid_tr'] <- substr(dataset$geoid_blk,1,11)
model_tract = try(lm(log(dataset$sale_price) ~ -1 + Xmat + dataset$geoid_tr-1), silent = T)

model_summ_tr = coef(summary(model_tract))
rownames(model_summ_tr) = sapply(rownames(model_summ_tr), function(k) substr(k, start = 5, stop = nchar(k)))
model_summ_tr <- cbind(" "=rownames(model_summ_tr), model_summ_tr)

write_xlsx(list("model" = as.data.frame(model_summ), "model_tract" = as.data.frame(model_summ_tr)), 
           paste0(path, "pool_res_402222.xlsx"))

# save the residuals 
omit <- model$na.action
omit_dataset <- dataset[-c(omit), ]
omit_dataset["residuals"] <- residuals(summary(model))
# residuals by tract
omit_dataset <- left_join(omit_dataset, tractgeo, by= c("geoid_tr" = "GEOID"))
write_csv(omit_dataset, paste0(path, "base_residuals_402522.csv"))

# tract model
omit <- model_tract$na.action
omit_dataset <- dataset[-c(omit), ]
omit_dataset["residuals"] <- residuals(summary(model_tract))
# residuals by tract
omit_dataset <- left_join(omit_dataset, tractgeo, by= c("geoid_tr" = "GEOID"))
write_csv(omit_dataset, paste0(path, "base_tr_residuals_402522.csv"))


# model with tech interactions

Xmat <- cbind(Xmat, as.numeric(dataset$bip) * Xmat[, 9 : 17])
colnames(Xmat[,31:39]) <- paste0(colnames(Xmat[,9:17]),"xbip")

model_tech <- try(lm(log(dataset$sale_price) ~ -1 + Xmat))

model_summ_tech = coef(summary(model_tech))

rownames(model_summ_tech) = sapply(rownames(model_summ_tech), function(k) substr(k, start = 5, stop = nchar(k)))
model_summ_tech <- cbind(" "=rownames(model_summ_tech), model_summ_tech)


# tech with census tract

model_tech_tract = try(lm(log(dataset$sale_price) ~ -1 + Xmat + dataset$geoid_tr-1), silent = T)

model_summ_tech_tr = coef(summary(model_tech_tract))
rownames(model_summ_tech_tr) = sapply(rownames(model_summ_tech_tr), function(k) substr(k, start = 5, stop = nchar(k)))
model_summ_tech_tr <- cbind(" "=rownames(model_summ_tech_tr), model_summ_tech_tr)

write_xlsx(list("model_tech" = as.data.frame(model_summ_tech), "model_tech_tract" = as.data.frame(model_summ_tech_tr)), 
           paste0(path, "pool_res_tech_inter_402522.xlsx"))

# tech model
omit <- model_tech$na.action
omit_dataset <- dataset[-c(omit), ]
omit_dataset["residuals"] <- residuals(summary(model_tech))
# residuals by tract
omit_dataset <- left_join(omit_dataset, tractgeo, by= c("geoid_tr" = "GEOID"))
write_csv(omit_dataset, paste0(path, "tech_residuals_402522.csv"))

# tech with tracts model
omit <- model_tech_tract$na.action
omit_dataset <- dataset[-c(omit), ]
omit_dataset["residuals"] <- residuals(summary(model_tech_tract))
# residuals by tract
omit_dataset <- left_join(omit_dataset, tractgeo, by= c("geoid_tr" = "GEOID"))
write_csv(omit_dataset, paste0(path, "tech_tr_residuals_402522.csv"))

