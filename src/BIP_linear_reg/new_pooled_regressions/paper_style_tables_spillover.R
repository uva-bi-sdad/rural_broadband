# Regression results tables and figures: Spillover Effects

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
lmfixed_fit15 <- readRDS("BIP_linear_reg/lm_fixedresults_15mi.RDS")
lmfixed_fit20 <- readRDS("BIP_linear_reg/lm_fixedresults_20mi.RDS")

lmfixed_match10 <- readRDS("BIP_linear_reg/lm_fe_matched_robust10.RDS")
lmfixed_match15 <- readRDS("BIP_linear_reg/lm_fe_matched_robust15.RDS")
lmfixed_match20 <- readRDS("BIP_linear_reg/lm_fe_matched_robust20.RDS")

match_fit <- fread("BIP_linear_reg/model_match_df.csv")

varnames <- c("BIP", "Year 2007-08", "Year 2009-10", "Year 2011-12", "Year 2013-14", "Year 2015+",
              "BIP x Year 2007-08", "BIP Year 2009-10", "BIP x Year 2011-12", 
              "BIP x Year 2013-14", "BIP x Year 2015+", "Observations")

varnames_m <- c("BIP_{2005-06}", "BIP_{2007-08}", "BIP_{2009-10}", "BIP_{2011-12}", 
                "BIP_{2012-13}", "BIP_{2015+}")

###############################
# TRACT EFFECTS
###############################

lmfixed_fit15 <- readRDS("BIP_linear_reg/lm_fixedresults_15mi.RDS")
lmfixed_fit15[["Estimate"]] <- as.numeric(levels(lmfixed_fit15[["Estimate"]]))[lmfixed_fit15[["Estimate"]]]
lmfixed_fit15[["Pr(>|t|)"]] <- as.numeric(levels(lmfixed_fit15[["Pr(>|t|)"]]))[lmfixed_fit15[["Pr(>|t|)"]]]
lmfixed_fit15[["Std. Error"]] <- as.numeric(levels(lmfixed_fit15[["Std. Error"]]))[lmfixed_fit15[["Std. Error"]]]

lmfe_15 <- as.data.frame(lmfixed_fit15[c(2, 11:15, 19:23), c(2,3,5)])
lmfe_15$`Std. Error` <- round(lmfe_15$`Std. Error`, 3)
lmfe_15$Estimate <- round(lmfe_15$Estimate, 3)
lmfe_15 <- lmfe_15 %>% mutate(est_sig = case_when(`Pr(>|t|)` <= 0.01 ~ paste0(round(as.numeric(lmfe_15$Estimate), 3), "***"),
                                                  `Pr(>|t|)`   <= 0.05 &  `Pr(>|t|)` > 0.01 ~  paste0(round(as.numeric(lmfe_15$Estimate), 3), "**"),
                                                  `Pr(>|t|)`   <= 0.1 &  `Pr(>|t|)` > 0.05 ~  paste0(round(as.numeric(lmfe_15$Estimate), 3), "*"),
                                                  `Pr(>|t|)`   > 0.1 ~  paste0(round(as.numeric(lmfe_15$Estimate), 3))
))
lmfe_15 <- rbind(lmfe_15, lmfixed_fit15[["5 %"]][length(lmfixed_fit15[["5 %"]])])

lmfixed_fit20 <- readRDS("BIP_linear_reg/lm_fixedresults_20mi.RDS")
lmfixed_fit20[["Estimate"]] <- as.numeric(levels(lmfixed_fit20[["Estimate"]]))[lmfixed_fit20[["Estimate"]]]
lmfixed_fit20[["Pr(>|t|)"]] <- as.numeric(levels(lmfixed_fit20[["Pr(>|t|)"]]))[lmfixed_fit20[["Pr(>|t|)"]]]
lmfixed_fit20[["Std. Error"]] <- as.numeric(levels(lmfixed_fit20[["Std. Error"]]))[lmfixed_fit20[["Std. Error"]]]

lmfe_20 <- as.data.frame(lmfixed_fit20[c(2, 11:15, 19:23), c(2,3,5)])
lmfe_20$`Std. Error` <- round(lmfe_20$`Std. Error`, 3)
lmfe_20$Estimate <- round(lmfe_20$Estimate, 3)
lmfe_20 <- lmfe_20 %>% mutate(est_sig = case_when(`Pr(>|t|)` <= 0.01 ~ paste0(round(as.numeric(lmfe_20$Estimate), 3), "***"),
                                                  `Pr(>|t|)`   <= 0.05 &  `Pr(>|t|)` > 0.01 ~  paste0(round(as.numeric(lmfe_20$Estimate), 3), "**"),
                                                  `Pr(>|t|)`   <= 0.1 &  `Pr(>|t|)` > 0.05 ~  paste0(round(as.numeric(lmfe_20$Estimate), 3), "*"),
                                                  `Pr(>|t|)`   > 0.1 ~  paste0(round(as.numeric(lmfe_20$Estimate), 3))
))
lmfe_20 <- rbind(lmfe_20, lmfixed_fit20[["5 %"]][length(lmfixed_fit20[["5 %"]])])

###################
# TFE ON MATCHED
###################

lmfema_15 <- as.data.frame(lmfixed_match15[c(2, 5:9, 13:17), c(1,2,4)])
lmfema_15$`Std. Error` <- round(as.numeric(lmfema_15$`Std. Error`), 3)
lmfema_15$Estimate <- round(as.numeric(lmfema_15$Estimate), 3)
lmfema_15 <- lmfema_15 %>% mutate(est_sig = case_when(`Pr(>|t|)` <= 0.01 ~ paste0(round(as.numeric(lmfema_15$Estimate), 3), "***"),
                                                      `Pr(>|t|)`   <= 0.05 &  `Pr(>|t|)` > 0.01 ~  paste0(round(as.numeric(lmfema_15$Estimate), 3), "**"),
                                                      `Pr(>|t|)`   <= 0.1 &  `Pr(>|t|)` > 0.05 ~  paste0(round(as.numeric(lmfema_15$Estimate), 3), "*"),
                                                      `Pr(>|t|)`   > 0.1 ~  paste0(round(as.numeric(lmfema_15$Estimate), 3))
))
lmfema_15 <- rbind(lmfema_15, lmfixed_match15["Std. Error"][nrow(lmfixed_match15["Std. Error"]),])

lmfema_20 <- as.data.frame(lmfixed_match20[c(2, 5:9, 13:17), c(1,2,4)])


lmfema_20[["Std. Error"]] <- as.numeric(levels(lmfema_20[["Std. Error"]]))[lmfema_20[["Std. Error"]]]
lmfema_20$`Std. Error` <- round(as.numeric(lmfema_20$`Std. Error`), 3)
lmfema_20[["Estimate"]] <- as.numeric(levels(lmfema_20[["Estimate"]]))[lmfema_20[["Estimate"]]]
lmfema_20$Estimate <- round(as.numeric(lmfema_20$Estimate), 3)
lmfema_20[["Pr(>|t|)"]] <- as.numeric(levels(lmfema_20[["Pr(>|t|)"]]))[lmfema_20[["Pr(>|t|)"]]]
lmfema_20 <- lmfema_20 %>% mutate(est_sig = case_when(`Pr(>|t|)` <= 0.01 ~ paste0(round(as.numeric(lmfema_20$Estimate), 3), "***"),
                                                      `Pr(>|t|)`   <= 0.05 &  `Pr(>|t|)` > 0.01 ~  paste0(round(as.numeric(lmfema_20$Estimate), 3), "**"),
                                                      `Pr(>|t|)`   <= 0.1 &  `Pr(>|t|)` > 0.05 ~  paste0(round(as.numeric(lmfema_20$Estimate), 3), "*"),
                                                      `Pr(>|t|)`   > 0.1 ~  paste0(round(as.numeric(lmfema_20$Estimate), 3))
))
lmfema_20 <- rbind(lmfema_20, lmfixed_match20[length(lmfixed_match20[,2]),2])

spil_main <- as.data.frame(lmfe_15[,c(1,2)])
spil_main <- cbind(spil_main, as.data.frame(lmfe_20[,c(1,2)]))
spil_main <- cbind(spil_main, as.data.frame(lmfema_15[,c(1,2)]))
spil_main <- cbind(spil_main, as.data.frame(lmfema_20[,c(1,2)]))

rownames(spil_main) <- varnames
names(spil_main) <- rep(c("Estimate", "SE"),4)

#######################
# MATCHING TABLE
#######################

match_fit <- as.data.frame(match_fit) 
match_fit["tstat"] <- match_fit$est/match_fit$se
match_fit["pval"] <- round(pt(match_fit$tstat, match_fit$wnobs), 3)
match_fit["se"] <- round(match_fit$se, 3)
match_fit["est_round"] <- round(match_fit$est, 3)
match_fit <- match_fit %>% mutate(est_sig = case_when(pval <= 0.01 ~ paste0(round(match_fit$est, 3), "***"),
                                                      pval <= 0.05 &  pval > 0.01 ~  paste0(round(match_fit$est, 3), "**"),
                                                      pval <= 0.1 &  pval > 0.05 ~  paste0(round(match_fit$est, 3), "*"),
                                                      pval > 0.1 ~  paste0(round(match_fit$est, 3))))

match_spil <- as.data.frame(match_fit[7:12, c(9,4,5)])
match_spil <- cbind(match_spil, match_fit[13:18, c(9,4,5)])
match_spil$wnobs <- match_spil$wnobs*2

rownames(match_spil) <- varnames_m
names(match_spil) <- rep(c("Estimate", "SE", "Observations"),2)

#####################
# PLOTS
#####################

#####################
# DID
#####################

yearnames <- c("07-08","09-10","11-12","13-14","15+")

lm_10 <-  as.data.frame(lmfixed_fit[c(19:23), c(2,6,7)])
rownames(lm_10) <- yearnames
names(lm_10) <- c("est1","lower1","upper1")
lm_10[["est1"]] <- as.numeric(levels(lm_10[["est1"]]))[lm_10[["est1"]]]
#lm_10[["lower1"]] <- as.numeric(levels(lm_10[["lower1"]]))[lm_10[["lower1"]]]
#lm_10[["upper1"]] <- as.numeric(levels(lm_10[["upper1"]]))[lm_10[["upper1"]]]
lm_10$years <- as.character(yearnames)

lm_15 <- as.data.frame(lmfixed_fit15[19:23,c(2,6,7)])
rownames(lm_15) <- yearnames
names(lm_15) <- c("est2","lower2","upper2")

lm_20 <- as.data.frame(lmfixed_fit20[19:23,c(2,6,7)])
rownames(lm_20) <- yearnames
names(lm_20) <- c("est3","lower3","upper3")

plotmodels_lm <- cbind(lm_10,lm_15,lm_20)

plotmodels_lm$est3 <- as.numeric(plotmodels_lm$est3)*100
plotmodels_lm$est1 <- as.numeric(plotmodels_lm$est1)*100
plotmodels_lm$est2 <- as.numeric(plotmodels_lm$est2)*100

plotmodels_lm$lower3 <- as.numeric(plotmodels_lm$lower3)*100
plotmodels_lm$lower1 <- as.numeric(plotmodels_lm$lower1)*100
plotmodels_lm$lower2 <- as.numeric(plotmodels_lm$lower2)*100

plotmodels_lm$upper3 <- as.numeric(plotmodels_lm$upper3)*100
plotmodels_lm$upper1 <- as.numeric(plotmodels_lm$upper1)*100
plotmodels_lm$upper2 <- as.numeric(plotmodels_lm$upper2)*100

shapes <- c('0-10mi' = 16, '5-15mi' = 15, "10-20mi" = 17)

ggplot(data=plotmodels_lm) + 
  geom_pointrange(aes(x=years, y=est1, ymin=upper1, ymax=lower1, shape="0-10mi"), 
                  color="black",position = position_nudge(x = -0.1)) + 
  geom_pointrange(aes(x=years, y=est2, ymin=upper2, ymax=lower2, shape="5-15mi"), 
                  color="black", position = position_nudge(x = 0.1)) + 
  geom_pointrange(aes(x=years, y=est3, ymin=upper3, ymax=lower3, shape="10-20mi"), 
                  color="black") + 
  geom_hline(yintercept=0, linetype="dashed", color="grey70") +
  geom_vline(xintercept ="09-10", linetype="dashed", color="grey70") +
  scale_shape_manual(name = "Legend", values = shapes) + 
  labs(#title = "Estimated Coefficients on the Interactions between BIP and 2-year Period Dummies", 
    x="BIP x 2-Year Period Indicators", 
    y = "Estimate, %")+
  theme_classic()
ggsave("BIP_linear_reg/lm_spilplot.png",height=4.5,width=6)

###################
# DID ON MATCHED
###################

lmma_10 <- as.data.frame(lmfixed_match10[13:17,c(1,5,6)])
rownames(lmma_10) <- yearnames
names(lmma_10) <- c("est1","lower1","upper1")
lmma_10$years <- as.character(yearnames)

lmma_15 <- as.data.frame(lmfixed_match15[13:17,c(1,5,6)])
rownames(lmma_15) <- yearnames
names(lmma_15) <- c("est2","lower2","upper2")

lmma_20 <- as.data.frame(lmfixed_match20[13:17,c(1,5,6)])
rownames(lmma_20) <- yearnames
names(lmma_20) <- c("est3","lower3","upper3")

plotmodels_lmma <- cbind(lmma_10,lmma_15,lmma_20)

plotmodels_lmma$est3 <- as.numeric(as.character(plotmodels_lmma$est3))*100
plotmodels_lmma$est1 <- as.numeric(plotmodels_lmma$est1)*100
plotmodels_lmma$est2 <- as.numeric(plotmodels_lmma$est2)*100

plotmodels_lmma$lower3 <- as.numeric(as.character(plotmodels_lmma$lower3))*100
plotmodels_lmma$lower1 <- as.numeric(plotmodels_lmma$lower1)*100
plotmodels_lmma$lower2 <- as.numeric(plotmodels_lmma$lower2)*100

plotmodels_lmma$upper3 <- as.numeric(as.character(plotmodels_lmma$upper3))*100
plotmodels_lmma$upper1 <- as.numeric(plotmodels_lmma$upper1)*100
plotmodels_lmma$upper2 <- as.numeric(plotmodels_lmma$upper2)*100

shapes <- c('0-10mi' = 16, '5-15mi' = 15, "10-20mi" = 17)

ggplot(data=plotmodels_lmma) + 
  geom_pointrange(aes(x=years, y=est1, ymin=upper1, ymax=lower1, shape="0-10mi"), 
                  color="black",position = position_nudge(x = -0.1)) + 
  geom_pointrange(aes(x=years, y=est2, ymin=upper2, ymax=lower2, shape="5-15mi"), 
                  color="black", position = position_nudge(x = 0.1)) + 
  geom_pointrange(aes(x=years, y=est3, ymin=upper3, ymax=lower3, shape="10-20mi"), 
                  color="black") + 
  geom_hline(yintercept=0, linetype="dashed", color="grey70") +
  geom_vline(xintercept ="09-10", linetype="dashed", color="grey70") +
  scale_shape_manual(name = "Legend", values = shapes) + 
  labs(#title = "Estimated Coefficients on the Interactions between BIP and 2-year Period Dummies", 
    x="BIP x 2-Year Period Indicators", 
    y = "Estimate, %")+
  theme_classic()
ggsave("BIP_linear_reg/lmma_spilplot.png",height=4.5,width=6)

######################
# MATCHED
######################

ma_10 <- match_10 <- match_fit[2:6,]
ma_10$lower <- ma_10$est - 1.96*ma_10$se
ma_10$upper <- ma_10$est + 1.96*ma_10$se
ma_10 <- ma_10 %>% dplyr::select("est","lower","upper")
rownames(ma_10) <- yearnames

ma_15 <- match_fit[8:12,]
ma_15$lower <- ma_15$est - 1.96*ma_15$se
ma_15$upper <- ma_15$est + 1.96*ma_15$se
ma_15 <- ma_15 %>% dplyr::select("est","lower","upper")

ma_20 <- match_fit[14:18,]
ma_20$lower <- ma_20$est - 1.96*ma_20$se
ma_20$upper <- ma_20$est + 1.96*ma_20$se
ma_20 <- ma_20 %>% dplyr::select("est","lower","upper")
ma_20$years <- as.character(yearnames)

plotmodels_ma <- cbind(ma_10,ma_15,ma_20)
colnames(plotmodels_ma) <- c("est1", "lower1", "upper1", "est2", "lower2", "upper2", "est3", "lower3", "upper3", "years")

plotmodels_ma$est3 <- as.numeric(plotmodels_ma$est3)*100
plotmodels_ma$est1 <- as.numeric(plotmodels_ma$est1)*100
plotmodels_ma$est2 <- as.numeric(plotmodels_ma$est2)*100

plotmodels_ma$lower3 <- as.numeric(plotmodels_ma$lower3)*100
plotmodels_ma$lower1 <- as.numeric(plotmodels_ma$lower1)*100
plotmodels_ma$lower2 <- as.numeric(plotmodels_ma$lower2)*100

plotmodels_ma$upper3 <- as.numeric(plotmodels_ma$upper3)*100
plotmodels_ma$upper1 <- as.numeric(plotmodels_ma$upper1)*100
plotmodels_ma$upper2 <- as.numeric(plotmodels_ma$upper2)*100

shapes <- c('0-10mi' = 16, '5-15mi' = 15, "10-20mi" = 17)

ggplot(data=plotmodels_ma) + 
  geom_pointrange(aes(x=years, y=est1, ymin=upper1, ymax=lower1, shape="0-10mi"), 
                  color="black",position = position_nudge(x = -0.1)) + 
  geom_pointrange(aes(x=years, y=est2, ymin=upper2, ymax=lower2, shape="5-15mi"), 
                  color="black", position = position_nudge(x = 0.1)) + 
  geom_pointrange(aes(x=years, y=est3, ymin=upper3, ymax=lower3, shape="10-20mi"), 
                  color="black") + 
  geom_hline(yintercept=0, linetype="dashed", color="grey70") +
  geom_vline(xintercept ="09-10", linetype="dashed", color="grey70") +
  scale_shape_manual(name = "Legend", values = shapes) + 
  labs(#title = "Estimated Coefficients on the Interactions between BIP and 2-year Period Dummies", 
    x="BIP x 2-Year Period Indicators", 
    y = "Estimate, %")+
  theme_classic()
ggsave("BIP_linear_reg/ma_spilplot.png",height=4.5,width=6)
