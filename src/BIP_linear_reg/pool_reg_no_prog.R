# Pooled regression with tract dummies, no acs, no program dummies, no zoning, plus program chars

# packages
library(readr)
library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)


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


dataset <- left_join(dataset, bip_project[,c("rusid", "Technology Type\r\n(Simplified)", "quantiles")])
dataset <- dataset %>% rename("technology" = "Technology Type\r\n(Simplified)")


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
              dataset$bip,
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

# building code dummies
# levels.x <- as.character(names(table(as.character(dataset$bldg_code),useNA = "ifany")))
# if(levels.x != "" & !is.na(levels.x)  & length(levels.x) != 1){
#   Xmat <- cbind(Xmat,sapply(levels.x[!is.na(levels.x)], function(x){
#     as.numeric(as.character(dataset$bldg_code)==x)
#   }))            
#   cnames <- c(cnames,paste("bldg_code:",levels.x[!is.na(levels.x)],sep=""))
# }

# primary sale price code
levels.x <- names(table(as.character(dataset$pri_cat_code), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dataset$pri_cat_code)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
}

# technology type
levels.x <- names(table(as.character(dataset$technology), useNA = "ifany"))
if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
  Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
    as.numeric(as.character(dataset$technology)==x)
  })) 
  if(levels.x != "") cnames <- c(cnames,paste("technology:",levels.x[-1],sep=""))
}

# technology type
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
colnames(Xmat) <- c("(intercept)","bip","age","nbaths","sqft_ratio","living_area_ft", "land_square_footage","bedrooms",cnames)

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
           paste0(path, "pooled_res_401222.xlsx"))


############################
# ERROR BAR PLOT
############################
est_df <- data.frame(matrix(nrow = 4))

est_df['var'] <- c("BIPxYEAR2010-11", "BIPxYEAR2012-13", "BIPxYEAR2014-15", "BIPxYEAR2016plus")
est_df['est_model'] <- as.numeric(model_summ[17:20, "Estimate"])
est_df['std_model'] <- as.numeric(model_summ[17:20, "Std. Error"])
est_df['est_model_tr'] <- as.numeric(model_summ_tr[17:20, "Estimate"])
est_df['std_model_tr'] <- as.numeric(model_summ_tr[17:20, "Std. Error"])

ggplot(est_df, aes(x=var, y=est_model, group=1)) + 
  geom_line () +
  geom_point()+
  geom_errorbar(aes(ymin=est_model-std_model, ymax=est_model+std_model), width=.2,
                position=position_dodge(0.05)) +
  geom_line(est_df, aes(x=var, y=est_model_r, group=1))



ggplot(est_df, aes(x=var)) + 
  geom_line(aes(y = est_model, group=1), color = "darkred") + 
  geom_point(aes(y = est_model, group=1), color = "darkred") +
  geom_errorbar(aes(ymin=est_model-std_model, ymax=est_model+std_model), width=.2,
                position=position_dodge(0.05), color = "darkred") +
  geom_line(aes(y = est_model_tr, group=1), color="steelblue") +
  geom_point(aes(y = est_model_tr, group=1), color="steelblue") +
  geom_errorbar(aes(ymin=est_model_tr-std_model_tr, ymax=est_model_tr+std_model_tr), width=.2,
                position=position_dodge(0.05),color="steelblue") +
  labs(x="Year Group and BIP Indicator Interaction", y = "Estimate")+
  theme_classic()

