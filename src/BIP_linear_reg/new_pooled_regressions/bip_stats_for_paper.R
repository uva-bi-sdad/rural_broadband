# packages / install if necessary 
library(readxl)
library(sf)
library(sp)
library(tigris)
library(geosphere)
library(dplyr)
library(ggplot2)
library(RPostgreSQL)

# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/BIP_linear_data/"

# load in project data spreadsheet
bip_project = readxl::read_xlsx("BIP_linear_reg/BIP_R1-R2AwardeeMaster_10-29-10_updNetObl_7-3-19 (1).xlsx", sheet = 2)
bip_project['rusid'] <- paste0(bip_project$`RUS Award No.`, "-", bip_project$`RUS\r\nLoan-Grant No.`)

# DTA file with new technology definitions
tech_descr <- read_dta(paste0(path, "bip_completed_infrastructure_projects_master_file.dta"))
tech <- tech_descr %>% select(projectid, ftth_gpon, ftth_rfog, ftth_ptp, ftth, fixed_wireless, adsl,
                              vdsl, mobile_wireless, hfc_cable, power_line)

tech["wireless"] <- tech$fixed_wireless + tech$mobile_wireless
tech["dsl"] <- 0
tech$dsl[(tech$adsl ==1 | tech$vdsl == 1)] <- 1

tech <- tech %>% select(c(projectid,ftth,wireless,dsl))

bip_project <- left_join(bip_project, tech, by=c("rusid"="projectid"))

# BIP 

bip_project <- bip_project[bip_project$rusid %in% bip_ids,]

varnames <- c("No. households", "No. businesses", "Award per HH (USD)", "FTTH", "Wireless", "DSL")
 

# min
out_tab <-  as.data.frame(round(c(min(bip_project$Households), min(bip_project$Businesses), min(bip_project$`Budget/HH`),
  min(bip_project$ftth), min(bip_project$wireless), min(bip_project$dsl)),3))
# mean 
out_tab <- cbind(out_tab, round(c(mean(bip_project$Households), mean(bip_project$Businesses), mean(bip_project$`Budget/HH`),
        mean(bip_project$ftth), mean(bip_project$wireless), mean(bip_project$dsl)),3))
# sd 
out_tab <- cbind(out_tab, round(c(sd(bip_project$Households), sd(bip_project$Businesses), sd(bip_project$`Budget/HH`),
                                  sd(bip_project$ftth), sd(bip_project$wireless), sd(bip_project$dsl)),3))
# max
out_tab <- cbind(out_tab, round(c(max(bip_project$Households), max(bip_project$Businesses), max(bip_project$`Budget/HH`),
                                  max(bip_project$ftth), max(bip_project$wireless), max(bip_project$dsl)),3))
colnames(out_tab) <- c("Min", "Mean", "SD", "Max")
rownames(out_tab) <- varnames
