# Balance table for a matched sample: 5-15 mi
# packages
library(readr)
library(dplyr)
library(tigris)


# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

# load housing data
dataset <- readRDS(paste0(path, "BIP_linear_data/housing_BIP_clean_011122.RDS"))

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
close_filter0 <- which( year.class==0 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 0 & dataset$dist_bip <= 10)) )
close_filter1 <- which( year.class==1 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 0 & dataset$dist_bip <= 10)) )
close_filter2 <- which( year.class==2 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 0 & dataset$dist_bip <= 10)) )
close_filter3 <- which( year.class==3 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 0 & dataset$dist_bip <= 10)) )
close_filter4 <- which( year.class==4 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 0 & dataset$dist_bip <= 10)) )
close_filter5 <- which( year.class==5 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 0 & dataset$dist_bip <= 10)) )

med_filter0 <- which( year.class==0 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 5 & dataset$dist_bip <= 15)) )
med_filter1 <- which( year.class==1 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 5 & dataset$dist_bip <= 15)) )
med_filter2 <- which( year.class==2 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 5 & dataset$dist_bip <= 15)) )
med_filter3 <- which( year.class==3 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 5 & dataset$dist_bip <= 15)) )
med_filter4 <- which( year.class==4 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 5 & dataset$dist_bip <= 15)) )
med_filter5 <- which( year.class==5 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 5 & dataset$dist_bip <= 15)) )

far_filter0 <- which( year.class==0 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 10 & dataset$dist_bip <= 20)) )
far_filter1 <- which( year.class==1 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 10 & dataset$dist_bip <= 20)) )
far_filter2 <- which( year.class==2 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 10 & dataset$dist_bip <= 20)) )
far_filter3 <- which( year.class==3 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 10 & dataset$dist_bip <= 20)) )
far_filter4 <- which( year.class==4 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 10 & dataset$dist_bip <= 20)) )
far_filter5 <- which( year.class==5 & (dataset$dist_bip <= 0 | ( dataset$dist_bip >= 10 & dataset$dist_bip <= 20)) )

filters <- list(close_filter0,close_filter1,close_filter2,close_filter3,close_filter4,close_filter5,
                med_filter0,med_filter1,med_filter2,med_filter3,med_filter4,med_filter5,
                far_filter0,far_filter1,far_filter2,far_filter3,far_filter4,far_filter5)

#filters <- list(med_filter0,med_filter1,med_filter2,med_filter3,med_filter4,med_filter5)

# load the matched model
model_match <- readRDS("BIP_linear_reg/model_match.RDS")

match_data <- list()
# for each model get treated and control sample
# filters 1-6 10 mi and below
# filters 7-12 5-15 mi 
# filters 13-18 10-20 mi
# filters 12 and 18 have both treated and untreated observations in treated and control samples! - FIXED

#for(k in 1:length(filters)){ 
for(k in 13:18){ 
  #data_f <- cbind(Y[filters[[k]]],Tr[filters[[k]]], dist_bip[filters[[k]]], Xmatch[filters[[k]]])
  data_f <- dataset[filters[[k]]]
  ind_treat <- model_match[[k]]$index.treated
  ind_cont <- model_match[[k]]$index.control
  data_cont <- data_f[ind_cont]
  data_treat <- data_f[ind_treat]
  data <- rbind(data_f[ind_treat], data_f[ind_cont])
  match_data <- rbind(match_data, data)
}
table(match_data$bip)
table(data_treat$bip)
table(data_cont$bip)

#nodup_macth_data <- match_data[!duplicated(match_data)]
#table(nodup_macth_data$bip)
varnames <- c("Age", "Bedrooms", "Bathrooms", "Living/Building sq. ft.", "Land sq. ft.",
              "Living sq. ft.", "Observations")


out_tab <- as.data.frame(round(c(t.test(match_data$age[match_data$bip == 0], 
       match_data$age[match_data$bip == 1], 
       alternative = "two.sided", var.equal = T)$estimate[[1]], 
  t.test(match_data$bedrooms[match_data$bip == 0], 
         match_data$bedrooms[match_data$bip == 1], 
         alternative = "two.sided", var.equal = T)$estimate[[1]],
  t.test(match_data$nbaths[match_data$bip == 0], 
         match_data$nbaths[match_data$bip == 1], 
         alternative = "two.sided", var.equal = T)$estimate[[1]],
  t.test(match_data$sqft_ratio[match_data$bip == 0], 
         match_data$sqft_ratio[match_data$bip == 1], 
         alternative = "two.sided", var.equal = T)$estimate[[1]],
  t.test(match_data$land_square_footage[match_data$bip == 0], 
         match_data$land_square_footage[match_data$bip == 1], 
         alternative = "two.sided", var.equal = T)$estimate[[1]],
  t.test(match_data$living_square_feet[match_data$bip == 0], 
         match_data$living_square_feet[match_data$bip == 1], 
         alternative = "two.sided", var.equal = T)$estimate[[1]]
  ), 3))

out_tab <- cbind(out_tab,
  round(c(t.test(match_data$age[match_data$bip == 0], 
           match_data$age[match_data$bip == 1], 
           alternative = "two.sided", var.equal = T)$estimate[[2]], 
    t.test(match_data$bedrooms[match_data$bip == 0], 
           match_data$bedrooms[match_data$bip == 1], 
           alternative = "two.sided", var.equal = T)$estimate[[2]],
    t.test(match_data$nbaths[match_data$bip == 0], 
           match_data$nbaths[match_data$bip == 1], 
           alternative = "two.sided", var.equal = T)$estimate[[2]],
    t.test(match_data$sqft_ratio[match_data$bip == 0], 
           match_data$sqft_ratio[match_data$bip == 1], 
           alternative = "two.sided", var.equal = T)$estimate[[2]],
    t.test(match_data$land_square_footage[match_data$bip == 0], 
           match_data$land_square_footage[match_data$bip == 1], 
           alternative = "two.sided", var.equal = T)$estimate[[2]],
    t.test(match_data$living_square_feet[match_data$bip == 0], 
           match_data$living_square_feet[match_data$bip == 1], 
           alternative = "two.sided", var.equal = T)$estimate[[2]])
  ,3)
)

out_tab <- cbind(out_tab,
                 round(c(t.test(match_data$age[match_data$bip == 0], 
                                match_data$age[match_data$bip == 1], 
                                alternative = "two.sided", var.equal = T)$p.val[[1]], 
                         t.test(match_data$bedrooms[match_data$bip == 0], 
                                match_data$bedrooms[match_data$bip == 1], 
                                alternative = "two.sided", var.equal = T)$p.val[[1]],
                         t.test(match_data$nbaths[match_data$bip == 0], 
                                match_data$nbaths[match_data$bip == 1], 
                                alternative = "two.sided", var.equal = T)$p.val[[1]],
                         t.test(match_data$sqft_ratio[match_data$bip == 0], 
                                match_data$sqft_ratio[match_data$bip == 1], 
                                alternative = "two.sided", var.equal = T)$p.val[[1]],
                         t.test(match_data$land_square_footage[match_data$bip == 0], 
                                match_data$land_square_footage[match_data$bip == 1], 
                                alternative = "two.sided", var.equal = T)$p.val[[1]],
                         t.test(match_data$living_square_feet[match_data$bip == 0], 
                                match_data$living_square_feet[match_data$bip == 1], 
                                alternative = "two.sided", var.equal = T)$p.val[[1]])
                       ,3)
)

out_tab <- rbind(out_tab, c(table(match_data$bip)[[1]], table(match_data$bip)[[2]], ""))

colnames(out_tab) <- c("Outside", "Inside", "p-value")
rownames(out_tab) <- varnames




