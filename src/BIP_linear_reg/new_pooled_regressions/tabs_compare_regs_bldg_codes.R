# Compare the results of regression with building codes

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

# load in files: linear model, linear model w/ fixed tract effects, mahalanobis matching
lmfixed_fit <- readRDS("BIP_linear_reg/lm_fixedresults_10mi.RDS")
lm_bldg <- readRDS("BIP_linear_reg/lm_fixedresults_10mi_bldg_codes.RDS")
lm_bldg_sel <- readRDS("BIP_linear_reg/lm_fixedresults_10mi_bldg_codes_sel.RDS")

varnames <- c("BIP", "Year 2007-08", "Year 2009-10", "Year 2011-12", "Year 2013-14", "Year 2015+",
              "BIP x Year 2007-08", "BIP Year 2009-10", "BIP x Year 2011-12", 
              "BIP x Year 2013-14", "BIP x Year 2015+", "Observations")

####################
# TABLES
####################

lmfixed_fit[["Estimate"]] <- as.numeric(levels(lmfixed_fit[["Estimate"]]))[lmfixed_fit[["Estimate"]]]
lmfixed_fit[["Pr(>|t|)"]] <- as.numeric(levels(lmfixed_fit[["Pr(>|t|)"]]))[lmfixed_fit[["Pr(>|t|)"]]]
lmfixed_fit[["Std. Error"]] <- as.numeric(levels(lmfixed_fit[["Std. Error"]]))[lmfixed_fit[["Std. Error"]]]

lmfe_10 <- as.data.frame(lmfixed_fit[c(2, 11:15, 19:23), c(2,3,5)])
lmfe_10$`Std. Error` <- round(lmfe_10$`Std. Error`, 3)
lmfe_10$Estimate <- round(lmfe_10$Estimate, 3)
lmfe_10 <- lmfe_10 %>% mutate(est_sig = case_when(`Pr(>|t|)` <= 0.01 ~ paste0(round(as.numeric(lmfe_10$Estimate), 3), "***"),
                                                  `Pr(>|t|)`   <= 0.05 &  `Pr(>|t|)` > 0.01 ~  paste0(round(as.numeric(lmfe_10$Estimate), 3), "**"),
                                                  `Pr(>|t|)`   <= 0.1 &  `Pr(>|t|)` > 0.05 ~  paste0(round(as.numeric(lmfe_10$Estimate), 3), "*"),
                                                  `Pr(>|t|)`   > 0.1 ~  paste0(round(as.numeric(lmfe_10$Estimate), 3))
))
lmfe_10 <- rbind(lmfe_10, lmfixed_fit[["5 %"]][length(lmfixed_fit[["5 %"]])])

lm_bldg[["Estimate"]] <- as.numeric(levels(lm_bldg[["Estimate"]]))[lm_bldg[["Estimate"]]]
lm_bldg[["Pr(>|t|)"]] <- as.numeric(levels(lm_bldg[["Pr(>|t|)"]]))[lm_bldg[["Pr(>|t|)"]]]
lm_bldg[["Std. Error"]] <- as.numeric(levels(lm_bldg[["Std. Error"]]))[lm_bldg[["Std. Error"]]]

lm_bldg_10 <- as.data.frame(lm_bldg[c(2, 11:15, 66:70), c(2,3,5)])
lm_bldg_10$`Std. Error` <- round(lm_bldg_10$`Std. Error`, 3)
lm_bldg_10$Estimate <- round(lm_bldg_10$Estimate, 3)
lm_bldg_10 <- lm_bldg_10 %>% mutate(est_sig = case_when(`Pr(>|t|)` <= 0.01 ~ paste0(round(as.numeric(lm_bldg_10$Estimate), 3), "***"),
                                                  `Pr(>|t|)`   <= 0.05 &  `Pr(>|t|)` > 0.01 ~  paste0(round(as.numeric(lm_bldg_10$Estimate), 3), "**"),
                                                  `Pr(>|t|)`   <= 0.1 &  `Pr(>|t|)` > 0.05 ~  paste0(round(as.numeric(lm_bldg_10$Estimate), 3), "*"),
                                                  `Pr(>|t|)`   > 0.1 ~  paste0(round(as.numeric(lm_bldg_10$Estimate), 3))
))
lm_bldg_10 <- rbind(lm_bldg_10, lm_bldg[["5 %"]][length(lm_bldg[["5 %"]])])

lm_bldg_sel[["Estimate"]] <- as.numeric(levels(lm_bldg_sel[["Estimate"]]))[lm_bldg_sel[["Estimate"]]]
lm_bldg_sel[["Pr(>|t|)"]] <- as.numeric(levels(lm_bldg_sel[["Pr(>|t|)"]]))[lm_bldg_sel[["Pr(>|t|)"]]]
lm_bldg_sel[["Std. Error"]] <- as.numeric(levels(lm_bldg_sel[["Std. Error"]]))[lm_bldg_sel[["Std. Error"]]]

lm_bldgsel_10 <- as.data.frame(lm_bldg_sel[c(2, 11:15, 35:39), c(2,3,5)])
lm_bldgsel_10$`Std. Error` <- round(lm_bldgsel_10$`Std. Error`, 3)
lm_bldgsel_10$Estimate <- round(lm_bldgsel_10$Estimate, 3)
lm_bldgsel_10 <- lm_bldgsel_10 %>% mutate(est_sig = case_when(`Pr(>|t|)` <= 0.01 ~ paste0(round(as.numeric(lm_bldgsel_10$Estimate), 3), "***"),
                                                        `Pr(>|t|)`   <= 0.05 &  `Pr(>|t|)` > 0.01 ~  paste0(round(as.numeric(lm_bldgsel_10$Estimate), 3), "**"),
                                                        `Pr(>|t|)`   <= 0.1 &  `Pr(>|t|)` > 0.05 ~  paste0(round(as.numeric(lm_bldgsel_10$Estimate), 3), "*"),
                                                        `Pr(>|t|)`   > 0.1 ~  paste0(round(as.numeric(lm_bldgsel_10$Estimate), 3))
))
lm_bldgsel_10 <- rbind(lm_bldgsel_10, lm_bldg_sel[["5 %"]][length(lm_bldg_sel[["5 %"]])])


table_main <- as.data.frame(lmfe_10[,c(1,2)])
table_main <- cbind(table_main, as.data.frame(lm_bldg_10[,c(1,2)]))
table_main <- cbind(table_main, as.data.frame(lm_bldgsel_10[,c(1,2)]))

rownames(table_main) <- varnames
names(table_main) <- rep(c("Estimate", "SE"),2)

###################
# PLOT 
###################

yearnames <- c("07-08","09-10","11-12","13-14","15+")

lm_10 <- as.data.frame(lmfixed_fit[19:23,c(2,6,7)])
rownames(lm_10) <- yearnames
names(lm_10) <- c("est1","lower1","upper1")
lm_10$years <- as.character(yearnames)

lm_bldg_10 <- as.data.frame(lm_bldg[66:70,c(2,6,7)])
rownames(lm_bldg_10) <- yearnames
names(lm_bldg_10) <- c("est2","lower2","upper2")

lm_bldgsel_10 <- as.data.frame(lm_bldg_sel[35:39,c(2,6,7)])
rownames(lm_bldgsel_10) <- yearnames
names(lm_bldgsel_10) <- c("est3","lower3","upper3")


plotmodels_lm <- cbind(lm_10,lm_bldg_10,lm_bldgsel_10)

plotmodels_lm$est3 <- as.numeric(plotmodels_lm$est3)*100
plotmodels_lm$est1 <- as.numeric(plotmodels_lm$est1)*100
plotmodels_lm$est2 <- as.numeric(plotmodels_lm$est2)*100

plotmodels_lm$lower3 <- as.numeric(plotmodels_lm$lower3)*100
plotmodels_lm$lower1 <- as.numeric(plotmodels_lm$lower1)*100
plotmodels_lm$lower2 <- as.numeric(plotmodels_lm$lower2)*100

plotmodels_lm$upper3 <- as.numeric(plotmodels_lm$upper3)*100
plotmodels_lm$upper1 <- as.numeric(plotmodels_lm$upper1)*100
plotmodels_lm$upper2 <- as.numeric(plotmodels_lm$upper2)*100

shapes <- c('Base' = 16, "All Bldg Codes" = 15, "Selected Bldg Codes" = 17)

ggplot(data=plotmodels_lm) + 
  geom_pointrange(aes(x=years, y=est1, ymin=upper1, ymax=lower1, shape="Base"), 
                  color="black",position = position_nudge(x = -0.1)) + 
  geom_pointrange(aes(x=years, y=est2, ymin=upper2, ymax=lower2, shape="All Bldg Codes"), 
                  color="black", position = position_nudge(x = 0.1)) + 
  geom_pointrange(aes(x=years, y=est3, ymin=upper3, ymax=lower3, shape="Selected Bldg Codes"), 
                  color="black") + 
  geom_hline(yintercept=0, linetype="dashed", color="grey70") +
  geom_vline(xintercept ="09-10", linetype="dashed", color="grey70") +
  scale_shape_manual(name = "Legend", values = shapes) + 
  labs(#title = "Estimated Coefficients on the Interactions between BIP and 2-year Period Dummies", 
    x="BIP x 2-Year Period Indicators", 
    y = "Estimate, %")+
  theme_classic()
ggsave("BIP_linear_reg/plot_bldg_codes.png",height=4.5,width=6)
