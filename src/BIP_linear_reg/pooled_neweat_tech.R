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

# pooled regression with technology and award quantiles, no acs, no zoning and no bldg code, no prog ids, no census tract dummies
model = try(lm(log(dataset$sale_price) ~ -1 + Xmat))

model_summ = coef(summary(model))
rownames(model_summ) = sapply(rownames(model_summ), function(k) substr(k, start = 5, stop = nchar(k)))
model_summ <- cbind(" "=rownames(model_summ), model_summ)
confint <- as.data.frame(confint(model, level = 0.90))
model_summ <- as.data.frame(model_summ)
model_res <- list(model_summ, confint) %>% 
  map(~ .x %>% 
        as.data.frame %>%
        rownames_to_column('rn')) %>% 
  reduce(left_join, by = 'rn') %>%
  column_to_rownames('rn')


# with census tract dummies
dataset['geoid_tr'] <- substr(dataset$geoid_blk,1,11)
model_tract = try(lm(log(dataset$sale_price) ~ -1 + Xmat + dataset$geoid_tr-1), silent = T)

model_summ_tr = coef(summary(model_tract))
rownames(model_summ_tr) = sapply(rownames(model_summ_tr), function(k) substr(k, start = 5, stop = nchar(k)))
model_summ_tr <- cbind(" "=rownames(model_summ_tr), model_summ_tr)
confint <- as.data.frame(confint(model_tract, level = 0.90))
model_summ_tr <- as.data.frame(model_summ_tr)
model_res_tr <- list(model_summ_tr, confint) %>% 
  map(~ .x %>% 
        as.data.frame %>%
        rownames_to_column('rn')) %>% 
  reduce(left_join, by = 'rn') %>%
  column_to_rownames('rn')


#write_xlsx(list("model" = as.data.frame(model_res), "model_tract" = as.data.frame(model_res_tr)), 
#           paste0(path, "pool_res_052322.xlsx"))

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

