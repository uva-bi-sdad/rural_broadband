# BIP pooled regression

# packages
# packages
library(readr)
library(readxl)
library(writexl)

# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/BIP_linear_data/"

# BIP shapes
load(paste0(path, "BIP_New.rds"))

load("~/R/broadband/bip-state.rdata")
# load(paste0(path,"bip-state.RData"))

# use if required <- ACS eligibility data
if(!"bb_eligibility"%in% ls())bb_eligibility <- read.csv("~/git/rural_broadband/src/BIP_analysis/broadband_tract_eligibility12-15-20.csv",  
                                                         stringsAsFactors = FALSE)

p <- paste0(path, "housing_BIP/")

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

# add program IDs
dataset <- transform(dataset, prog_ID = as.numeric(factor(rusid)))

# add inside by factor
dataset['bip_new'] <- dataset$bip * dataset['prog_ID']

#########################################
# DESIGN MATRIX
#########################################

# YEAR-GROUPPINGS DUMMIES
year.class = unlist(sapply(dataset$sale_year, function(k){
  if(k < 2010) return(0)
  if(k >= 2010 & k < 2012) return(1)
  if(k >= 2012 & k < 2014) return(2)
  if(k >= 2014 & k < 2016) return(3)
  if(k >= 2016) return(4)
}))

Xmat <- cbind(1,
              dataset$bip_new,
              dataset$age, # age of the house
              dataset$nbaths, # number of baths
              dataset$sqft_ratio, # living square ft/building square ft.
              #log(dataset$acres), # property acreage
              log(dataset$land_square_footage), # land suqare footage
              dataset$bedrooms)
cnames <- c() 

levels.x <- names(table(year.class, useNA = "ifany"))
id.yr = length(levels.x)
flag.in = 0 %in% levels.x


if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <- cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(year.class)==x)
  })) 
  cnames <- c(cnames,paste("year.cl:",levels.x[-1],sep=""))
}

levels.x <- names(table(as.character(dataset$transaction_type), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dataset$transaction_type)==x)
  })) 
  cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
}

levels.x <- as.character(names(table(as.character(dataset$bldg_code),useNA = "ifany")))
if(levels.x != "" & !is.na(levels.x)  & length(levels.x) != 1){
  Xmat <- cbind(Xmat,sapply(levels.x[!is.na(levels.x)], function(x){
    as.numeric(as.character(dataset$bldg_code)==x)
  }))            
  cnames <- c(cnames,paste("bldg_code:",levels.x[!is.na(levels.x)],sep=""))
}

levels.x <- names(table(as.character(dataset$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dataset$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

levels.x <- names(table(as.character(dataset$zoning), useNA = "ifany" ))
#levels.x <- levels.x[!is.na(levels.x)]
if(levels.x != "" & !is.na(levels.x)  & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[!is.na(levels.x)], function(x){
    as.numeric(as.character(dataset$zoning)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("zoning:",levels.x[!is.na(levels.x)],sep=""))
}

levels.x <- names(table(as.character(dataset$prog_ID), useNA = "ifany" ))
if(levels.x != "" & !is.na(levels.x)  & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dataset$prog_ID)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("prog_ID:",levels.x[-1],sep=""))
}

# interactions of program dummies with year dummies:
if(id.yr != 1){
  if(!flag.in){
    if(is.null(ncol(Xmat[, 8 : (8 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(dataset$bip) * Xmat[, 8 : (8 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 8 : (8 + id.yr - 2)], 2, function(k) as.numeric(dataset$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }
  }else{
    if(is.null(ncol(Xmat[, 8 : (8 + id.yr - 2)]))){
      Xmat <- cbind(Xmat, as.numeric(dataset$bip) * Xmat[, 8 : (8 + id.yr - 2)])
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
    }else{
      Xmat <- cbind(Xmat, apply(Xmat[, 8 : (8 + id.yr - 2)], 2, function(k) as.numeric(dataset$bip) * k))
      cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
    }
  }
}


# add names to covar matrix col
colnames(Xmat) <- c("(intercept)","bip","age","nbaths","sqft_ratio","land_square_footage","bedrooms",cnames)

###########################################
# ACS VARIABLES
###########################################

geoid.bip <- as.numeric(substr(dataset$geoid_blk,1,11))
geoid.acs <- bb_eligibility$GEOID

id.acs <- match(geoid.bip,geoid.acs)
acs_data <- list()
for(j in 1:nrow(dataset)){
  if(is.na(dataset$sale_year[j])){
    acs_data[[j]] <- as.numeric(bb_eligibility[id.acs[j],c("hs_or_less_2010",
                                                           "renters_2010",
                                                           "poverty_2010",
                                                           "age_65_older_2010",
                                                           "hispanic_2010",
                                                           "black_2010",
                                                           "family_2010",
                                                           "foreign_2010")])
  } else if(dataset$sale_year[j]<=2010){
    acs_data[[j]] <- as.numeric(bb_eligibility[id.acs[j],c("hs_or_less_2010",
                                                           "renters_2010",
                                                           "poverty_2010",
                                                           "age_65_older_2010",
                                                           "hispanic_2010",
                                                           "black_2010",
                                                           "family_2010",
                                                           "foreign_2010")])
  }else{
    acs_data[[j]] <- as.numeric(bb_eligibility[id.acs[j],c("hs_or_less_2019",
                                                           "renters_2019",
                                                           "poverty_2019",
                                                           "age_65_older_2019",
                                                           "hispanic_2019",
                                                           "black_2019",
                                                           "family_2019",
                                                           "foreign_2019")])
  }
}
acs_data <- do.call(rbind,acs_data)
colnames(acs_data) <- c("hs_or_less",
                        "renters",
                        "poverty",
                        "age_65_older",
                        "hispanic",
                        "black",
                        "family",
                        "foreign")
id.na  = apply(acs_data, 2, function(k) sum(!is.na(k)))

if(sum(!is.na(acs_data)) == 0) cat("NO ACS DATA FOUND!", "\n") else Xmat <- cbind(Xmat,acs_data)

####################################
# REGRESSION MODELS
####################################

# base model with acs variables
model_acs = try(lm(log(dataset$sale_price) ~ -1 + Xmat))

model_acs_summ = coef(summary(model_acs))
rownames(model_acs_summ) = sapply(rownames(model_acs_summ), function(k) substr(k, start = 5, stop = nchar(k)))
model_acs_summ <- cbind(" "=rownames(model_acs_summ), model_acs_summ)

# with block dummies --> results in a crash
model_blk = try(lm(log(dataset$sale_price) ~ -1 + Xmat + dataset$geoid_blk-1), silent = T)

if(class(model_blk) == "try-error"){
  model_blk = model_acs
}

model_blk_summ = coef(summary(model_blk))
rownames(model_blk_summ) = sapply(rownames(model_blk_summ), function(k) substr(k, start = 5, stop = nchar(k)))
model_blk_summ <- cbind(" "=rownames(model_blk_summ), model_blk_summ)

write_xlsx(list("model_acs" = as.data.frame(model_acs_summ)), 
           paste0(path, "pooled_reg_res_acs_cat.xlsx"))

write_xlsx(list("model_acs" = as.data.frame(model_acs_summ), "model_blk" = as.data.frame(model_blk_summ)), 
           paste0(path, bip_name_rusid, "_pooled_res.xlsx"))



