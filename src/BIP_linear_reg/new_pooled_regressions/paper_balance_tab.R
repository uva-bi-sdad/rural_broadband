# Balance table for a matched sample 
# packages
library(readr)
library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
library(haven)
library(tidycensus)
library(tigris)
library(cobalt)

# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

# load housing data
dataset <- readRDS(paste0(path, "BIP_linear_data/housing_BIP_clean_011122.RDS"))
# dataset restrcit to 10mi
dataset10 <- dataset[dataset$dist_bip <= 10 & dataset$dist_bip >= 0,]

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
Xextra <- dataset %>% dplyr::select(geoid_blk, p_id_iris_frmtd, sale_year, ftth, wireless, dsl, transaction_type,
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

filters <- list(close_filter0,close_filter1,close_filter2,close_filter3,close_filter4,close_filter5,
                med_filter0,med_filter1,med_filter2,med_filter3,med_filter4,med_filter5,
                far_filter0,far_filter1,far_filter2,far_filter3,far_filter4,far_filter5)

filters <- list(close_filter0,close_filter1,close_filter2,close_filter3,close_filter4,close_filter5)

# load the matched model
model_match <- readRDS("BIP_linear_reg/model_match.RDS")

match_data <- list()
# for each model get treated and control sample
for(k in 1:length(filters)){
  data_f <- cbind(Y[filters[[k]]],Tr[filters[[k]]], dist_bip[filters[[k]]], Xmatch[filters[[k]]])
  ind_treat <- model_match[[k]]$index.treated
  ind_cont <- model_match[[k]]$index.control
  data <- rbind(data_f[ind_treat], data_f[ind_cont])
  match_data <- rbind(match_data, data)
}
match_data <-  match_data[rowSums(is.na(match_data)) == 0]

names(match_data)[1] <- "log_sale_price"
names(match_data)[2] <- "inside_bip"
names(match_data)[3] <- "dist_bip"

#nodup_macth_data <- match_data[!duplicated(match_data)]

varnames <- c("Age", "Bedrooms", "Bathrooms", "Living/Building sq. ft.", "Land sq. ft.",
              "Living sq. ft.", "Observations")


out_tab <- as.data.frame(round(c(t.test(match_data$age[match_data$inside_bip == 0], 
       match_data$age[match_data$inside_bip == 1], 
       alternative = "two.sided", var.equal = T)$estimate[[1]], 
  t.test(match_data$bedrooms[match_data$inside_bip == 0], 
         match_data$bedrooms[match_data$inside_bip == 1], 
         alternative = "two.sided", var.equal = T)$estimate[[1]],
  t.test(match_data$nbaths[match_data$inside_bip == 0], 
         match_data$nbaths[match_data$inside_bip == 1], 
         alternative = "two.sided", var.equal = T)$estimate[[1]],
  t.test(match_data$sqft_ratio[match_data$inside_bip == 0], 
         match_data$sqft_ratio[match_data$inside_bip == 1], 
         alternative = "two.sided", var.equal = T)$estimate[[1]],
  t.test(match_data$land_square_footage[match_data$inside_bip == 0], 
         match_data$land_square_footage[match_data$inside_bip == 1], 
         alternative = "two.sided", var.equal = T)$estimate[[1]],
  t.test(match_data$living_square_feet[match_data$inside_bip == 0], 
         match_data$living_square_feet[match_data$inside_bip == 1], 
         alternative = "two.sided", var.equal = T)$estimate[[1]]
  ), 3))

out_tab <- cbind(out_tab,
  round(c(t.test(match_data$age[match_data$inside_bip == 0], 
           match_data$age[match_data$inside_bip == 1], 
           alternative = "two.sided", var.equal = T)$estimate[[2]], 
    t.test(match_data$bedrooms[match_data$inside_bip == 0], 
           match_data$bedrooms[match_data$inside_bip == 1], 
           alternative = "two.sided", var.equal = T)$estimate[[2]],
    t.test(match_data$nbaths[match_data$inside_bip == 0], 
           match_data$nbaths[match_data$inside_bip == 1], 
           alternative = "two.sided", var.equal = T)$estimate[[2]],
    t.test(match_data$sqft_ratio[match_data$inside_bip == 0], 
           match_data$sqft_ratio[match_data$inside_bip == 1], 
           alternative = "two.sided", var.equal = T)$estimate[[2]],
    t.test(match_data$land_square_footage[match_data$inside_bip == 0], 
           match_data$land_square_footage[match_data$inside_bip == 1], 
           alternative = "two.sided", var.equal = T)$estimate[[2]],
    t.test(match_data$living_square_feet[match_data$inside_bip == 0], 
           match_data$living_square_feet[match_data$inside_bip == 1], 
           alternative = "two.sided", var.equal = T)$estimate[[2]])
  ,3)
)

out_tab <- cbind(out_tab,
                 round(c(t.test(match_data$age[match_data$inside_bip == 0], 
                                match_data$age[match_data$inside_bip == 1], 
                                alternative = "two.sided", var.equal = T)$p.val[[1]], 
                         t.test(match_data$bedrooms[match_data$inside_bip == 0], 
                                match_data$bedrooms[match_data$inside_bip == 1], 
                                alternative = "two.sided", var.equal = T)$p.val[[1]],
                         t.test(match_data$nbaths[match_data$inside_bip == 0], 
                                match_data$nbaths[match_data$inside_bip == 1], 
                                alternative = "two.sided", var.equal = T)$p.val[[1]],
                         t.test(match_data$sqft_ratio[match_data$inside_bip == 0], 
                                match_data$sqft_ratio[match_data$inside_bip == 1], 
                                alternative = "two.sided", var.equal = T)$p.val[[1]],
                         t.test(match_data$land_square_footage[match_data$inside_bip == 0], 
                                match_data$land_square_footage[match_data$inside_bip == 1], 
                                alternative = "two.sided", var.equal = T)$p.val[[1]],
                         t.test(match_data$living_square_feet[match_data$inside_bip == 0], 
                                match_data$living_square_feet[match_data$inside_bip == 1], 
                                alternative = "two.sided", var.equal = T)$p.val[[1]])
                       ,3)
)

out_tab <- rbind(out_tab, c(table(match_data$inside_bip)[[1]], table(match_data$inside_bip)[[2]], ""))

colnames(out_tab) <- c("Outside", "Inside", "p-value")
rownames(out_tab) <- varnames




