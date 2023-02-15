# Figure 3 plot: DID on matched estimated coefficients with 95% CIs 

# packages
library(readr)
library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
library(haven)
library(tidycensus)
library(tigris)

# your working directory
setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

##########################
# DID ON MATCHED  FIG 3
##########################

# load in files: DID on matched property sales
lmfixed_match <- readRDS("BIP_linear_reg/lm_fe_matched_robust10.RDS")

yearnames <- c("07-08","09-10","11-12","13-14","15+")

lmma_10 <- as.data.frame(lmfixed_match[13:17,c(1,5,6)])
rownames(lmma_10) <- yearnames
names(lmma_10) <- c("est2","lower2","upper2")
lmma_10$years <- as.character(yearnames)

lmma_10$est2 <- as.numeric(lmma_10$est2)*100
lmma_10$lower2 <- as.numeric(lmma_10$lower2)*100
lmma_10$upper2 <- as.numeric(lmma_10$upper2)*100

shapes <- c('DID on Matched' = 16)

ggplot(data=lmma_10) + 
  geom_pointrange(aes(x=years, y=est2, ymin=upper2, ymax=lower2, shape="DID on Matched"), 
                  color="black", position = position_nudge(x = 0.1)) + 
  geom_hline(yintercept=0, linetype="dashed", color="grey70") +
  geom_vline(xintercept ="09-10", linetype="dashed", color="grey70") +
  scale_shape_manual(name = "Legend", values = shapes) + 
  labs(#title = "Estimated Coefficients on the Interactions between BIP and 2-year Period Dummies", 
    x="BIP x 2-Year Period Indicators", 
    y = "Estimate, %")+
  theme_classic()
ggsave("BIP_linear_reg/did_match_fig3.png",height=4.5,width=6)

##########################
# DID FULL SAMPLE FIG 8
##########################

# load in files: did full sample
lmfixed_fit <- readRDS("BIP_linear_reg/lm_fixedresults_10mi.RDS")
lmfixed_fit[["Estimate"]] <- as.numeric(levels(lmfixed_fit[["Estimate"]]))[lmfixed_fit[["Estimate"]]]
lmfixed_fit[["Pr(>|t|)"]] <- as.numeric(levels(lmfixed_fit[["Pr(>|t|)"]]))[lmfixed_fit[["Pr(>|t|)"]]]
lmfixed_fit[["Std. Error"]] <- as.numeric(levels(lmfixed_fit[["Std. Error"]]))[lmfixed_fit[["Std. Error"]]]


yearnames <- c("07-08","09-10","11-12","13-14","15+")

lm_10 <- as.data.frame(lmfixed_fit[19:23,c(2,6,7)])
rownames(lm_10) <- yearnames
names(lm_10) <- c("est1","lower1","upper1")
lm_10$years <- as.character(yearnames)

lm_10$est1 <- as.numeric(lm_10$est1)*100
lm_10$lower1 <- as.numeric(lm_10$lower1)*100
lm_10$upper1 <- as.numeric(lm_10$upper1)*100

shapes <- c('DID Full Sample' = 16)

ggplot(data=lm_10) + 
  geom_pointrange(aes(x=years, y=est1, ymin=upper1, ymax=lower1, shape="DID Full Sample"), 
                  color="black", position = position_nudge(x = 0.1)) + 
  geom_hline(yintercept=0, linetype="dashed", color="grey70") +
  geom_vline(xintercept ="09-10", linetype="dashed", color="grey70") +
  scale_shape_manual(name = "Legend", values = shapes) + 
  labs(#title = "Estimated Coefficients on the Interactions between BIP and 2-year Period Dummies", 
    x="BIP x 2-Year Period Indicators", 
    y = "Estimate, %")+
  theme_classic()
ggsave("BIP_linear_reg/did_full_fig8.png",height=4.5,width=6)

#################
# MATCHING FIG 9
#################

match_fit <- fread("BIP_linear_reg/model_match_df.csv")
yearnames <- c("05-06", "07-08","09-10","11-12","13-14","15+")
ma_10 <- match_fit[1:6,]
ma_10$lower <- ma_10$est - 1.96*ma_10$se
ma_10$upper <- ma_10$est + 1.96*ma_10$se
ma_10 <- ma_10 %>% dplyr::select("est","lower","upper")
ma_10$years <- as.character(yearnames)

ma_10$est <- as.numeric(ma_10$est)*100
ma_10$lower <- as.numeric(ma_10$lower)*100
ma_10$upper <- as.numeric(ma_10$upper)*100

shapes <- c('Matching' = 16)

ggplot(data=ma_10) + 
  geom_pointrange(aes(x=years, y=est, ymin=upper, ymax=lower, shape="Matching"), 
                  color="black", position = position_nudge(x = 0.1)) + 
  geom_hline(yintercept=0, linetype="dashed", color="grey70") +
  geom_vline(xintercept ="09-10", linetype="dashed", color="grey70") +
  scale_shape_manual(name = "Legend", values = shapes) + 
  labs(#title = "Estimated Coefficients on the Interactions between BIP and 2-year Period Dummies", 
    x="BIP x 2-Year Period Indicators", 
    y = "Estimate, %")+
  theme_classic()
ggsave("BIP_linear_reg/macthing_fig9.png",height=4.5,width=6)
