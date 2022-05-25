# BIP linear regression

# packages
library(readr)
library(readxl)
library(writexl)

# your working directory
setwd("~/git/rural_broadband/src")

# geographic plots function
source('~/git/rural_broadband/src/BIP_analysis/spPlot.r')

# USA geographies shapes 
usa_shape <- raster::getData("GADM",country="USA", level=1)

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/BIP_linear_data/"

# BIP shapes
load(paste0(path, "BIP_New.rds"))

load("~/R/broadband/bip-state.rdata")
# load(paste0(path,"bip-state.RData"))

proj_crs <- CRS(" +proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")
newbip_all <- sf::st_transform(newbip_union,crs=sf::st_crs(usa_shape))

# use if required <- ACS eligibility data
if(!"bb_eligibility"%in% ls()) bb_eligibility <- read.csv(paste0(path,"broadband_tract_eligibility12-15-20.csv"))

# lists to store data
count_lst <- c()

p <- paste0(path, "housing_BIP/")

#for (i in 42:42){
for (i in 1:length(list.files(p))){
  
  file_name <- list.files(p)[i]
  
  bip_name_rusid <- strsplit(file_name, "[.]")[[1]][1]
  print(paste(i, "Program", bip_name_rusid))
  
  state.symb <- substr(bip_name_rusid, start = 1, stop = 2)
  state <- state.name[grep(state.symb, state.abb)]
  fp_code <- fips_state[[i]]
  
  data.BIP <- read_xlsx(paste0(p, file_name), sheet = "data")
  
  
  #########################################
  # DESIGN MATRIX
  #########################################
  
  # YEAR-GROUPPINGS DUMMIES
  year.class = unlist(sapply(data.BIP$sale_year, function(k){
    if(k < 2010) return(0)
    if(k >= 2010 & k < 2012) return(1)
    if(k >= 2012 & k < 2014) return(2)
    if(k >= 2014 & k < 2016) return(3)
    if(k >= 2016) return(4)
  }))
  
  Xmat <- cbind(1,
                data.BIP$bip,
                data.BIP$age, # age of the house
                data.BIP$nbaths, # number of baths
                data.BIP$sqft_ratio, # living square ft/building square ft.
                #log(data.BIP$acres), # property acreage
                log(data.BIP$land_square_footage), # land suqare footage
                data.BIP$bedrooms)
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
  
  levels.x <- names(table(as.character(data.BIP$transaction_type), useNA = "ifany"))
  if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
    Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
      as.numeric(as.character(data.BIP$transaction_type)==x)
    })) 
    cnames <- c(cnames,paste("transaction_type:",levels.x[-1],sep=""))
  }
  
  levels.x <- as.character(names(table(as.character(data.BIP$bldg_code),useNA = "ifany")))
  if(levels.x != "" & !is.na(levels.x)  & length(levels.x) != 1){
    Xmat <- cbind(Xmat,sapply(levels.x[!is.na(levels.x)], function(x){
      as.numeric(as.character(data.BIP$bldg_code)==x)
    }))            
    cnames <- c(cnames,paste("bldg_code:",levels.x[!is.na(levels.x)],sep=""))
  }
  
  levels.x <- names(table(as.character(data.BIP$pri_cat_code), useNA = "ifany"))
  if(levels.x != "" & !is.na(levels.x) & length(levels.x) != 1){
    Xmat <-cbind(Xmat,sapply(levels.x[-1], function(x){
      as.numeric(as.character(data.BIP$pri_cat_code)==x)
    })) 
    if(levels.x != "") cnames <- c(cnames,paste("pri_cat_code:",levels.x[-1],sep=""))
  }
  
  levels.x <- names(table(as.character(data.BIP$zoning), useNA = "ifany" ))
  #levels.x <- levels.x[!is.na(levels.x)]
  if(levels.x != "" & !is.na(levels.x)  & length(levels.x) != 1){
    Xmat <-cbind(Xmat,sapply(levels.x[!is.na(levels.x)], function(x){
      as.numeric(as.character(data.BIP$zoning)==x)
    })) 
    if(levels.x != "") cnames <- c(cnames,paste("zoning:",levels.x[!is.na(levels.x)],sep=""))
  }

  
  # interactions of program dummies with year dummies:
  if(id.yr != 1){
    if(!flag.in){
      if(is.null(ncol(Xmat[, 8 : (8 + id.yr - 2)]))){
        Xmat <- cbind(Xmat, as.numeric(data.BIP$bip) * Xmat[, 8 : (8 + id.yr - 2)])
        cnames <- c(cnames, paste(cnames[1 : (id.yr - 2)],"xbip", sep = ""))
      }else{
        Xmat <- cbind(Xmat, apply(Xmat[, 8 : (8 + id.yr - 2)], 2, function(k) as.numeric(data.BIP$bip) * k))
        cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
      }
    }else{
      if(is.null(ncol(Xmat[, 8 : (8 + id.yr - 2)]))){
        Xmat <- cbind(Xmat, as.numeric(data.BIP$bip) * Xmat[, 8 : (8 + id.yr - 2)])
        cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = "")) 
      }else{
        Xmat <- cbind(Xmat, apply(Xmat[, 8 : (8 + id.yr - 2)], 2, function(k) as.numeric(data.BIP$bip) * k))
        cnames <- c(cnames, paste(cnames[1 : (id.yr - 1)],"xbip", sep = ""))  
      }
    }
  }
  
  # add names to covar matrix col
  colnames(Xmat) <- c("(intercept)","bip","age","nbaths","sqft_ratio","land_square_footage","bedrooms",cnames)

  #########################################
  # no. obs and no. block dummies per prog
  #########################################
  
  count_lst = rbind(count_lst, c(bip_name_rusid,
                           nrow(Xmat),
                           length(unique(data.BIP$geoid_blk))))
  
  ###########################################
  # ACS VARIABLES
  ###########################################
  
  
  geoid.bip <- as.numeric(substr(data.BIP$geoid_blk,1,11))
  geoid.acs <- bb_eligibility$GEOID
  id.acs <- match(geoid.bip,geoid.acs)
  acs_data <- list()
  for(j in 1:nrow(data.BIP)){
    if(is.na(data.BIP$sale_year[j])){
      acs_data[[j]] <- as.numeric(bb_eligibility[id.acs[j],c("hs_or_less_2010",
                                                             "renters_2010",
                                                             "poverty_2010",
                                                             "age_65_older_2010",
                                                             "hispanic_2010",
                                                             "black_2010",
                                                             "family_2010",
                                                             "foreign_2010")])
    } else if(data.BIP$sale_year[j]<=2010){
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
  
  # SAVE THE RAW DATA AND A DESIGN MATRIX:
  write_xlsx(list("data" = data.BIP, "matrix" = as.data.frame(Xmat)), paste0(path, "housing_BIP_with_ACS/", bip_name_rusid, ".xlsx"))
  
  ####################################
  # REGRESSION MODELS
  ####################################
  
  # base model with acs variables
  model_acs = try(lm(log(data.BIP$sale_price) ~ -1 + Xmat))
  # if ACS are missing too much don't use
  if(class(model_acs) == "try-error"){
    cat("ACS DATA TOO SPARSE!", "\n")
    # OLS model
    model_acs = try(lm(log(data.BIP$sale_price) ~ -1 + Xmat[,-((ncol(Xmat) - 7):ncol(Xmat))]))
  }
  
  model_acs_summ = coef(summary(model_acs))
  rownames(model_acs_summ) = sapply(rownames(model_acs_summ), function(k) substr(k, start = 5, stop = nchar(k)))
  model_acs_summ <- cbind(" "=rownames(model_acs_summ), model_acs_summ)
  
  # with block dummies
  #if(length(data.BIP$geoid_blk)>1 & length(data.BIP$geoid_blk[is.na(data.BIP$geoid_blk)]) != length(data.BIP$geoid_blk)){
    model_blk = try(lm(log(data.BIP$sale_price) ~ -1 + Xmat + data.BIP$geoid_blk-1), silent = T)
  #}
    
  if(class(model_blk) == "try-error"){
    model_blk = model_acs
  }
    
  model_blk_summ = coef(summary(model_blk))
  rownames(model_blk_summ) = sapply(rownames(model_blk_summ), function(k) substr(k, start = 5, stop = nchar(k)))
  model_blk_summ <- cbind(" "=rownames(model_blk_summ), model_blk_summ)
    
  
  write_xlsx(list("model_acs" = as.data.frame(model_acs_summ), "model_blk" = as.data.frame(model_blk_summ)), 
             paste0(path, "ind_reg_results/", bip_name_rusid, ".xlsx"))

}


colnames(count_lst) <- c("BIP_rusid", "NO_obs", "NO_blk_dummies")
write.csv(count_lst, file = paste0(path, "blk_dummy_counts.csv"))
