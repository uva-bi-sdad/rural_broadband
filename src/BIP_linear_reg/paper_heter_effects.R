# Heterogenous Effects plots 

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

# load in estimation results
lm_ftth <- readRDS("BIP_linear_reg/lm_fe_ftth.RDS")
lm_wire <- readRDS("BIP_linear_reg/lm_fe_wireless.RDS")
lm_dsl <- readRDS("BIP_linear_reg/lm_fe_dsl.RDS")

varnames <- c("BIP x Year 2007-08", "BIP Year 2009-10", "BIP x Year 2011-12", 
              "BIP x Year 2013-14", "BIP x Year 2015+", "Observations")

lm_ftth[[1]][["Estimate"]] <- as.numeric(levels(lm_ftth[[1]][["Estimate"]]))[lm_ftth[[1]][["Estimate"]]]
lm_ftth[[1]][["Pr(>|t|)"]] <- as.numeric(levels(lm_ftth[[1]][["Pr(>|t|)"]]))[lm_ftth[[1]][["Pr(>|t|)"]]]
lm_ftth[[1]][["Std. Error"]] <- as.numeric(levels(lm_ftth[[1]][["Std. Error"]]))[lm_ftth[[1]][["Std. Error"]]]

ftth <- as.data.frame(lm_ftth[[1]][c(17:21), c(2,3,5)])
ftth$`Std. Error` <- round(ftth$`Std. Error`, 3)
ftth$Estimate <- round(ftth$Estimate, 3)
ftth <- ftth %>% mutate(est_sig = case_when(`Pr(>|t|)` <= 0.01 ~ paste0(round(as.numeric(ftth$Estimate), 3), "***"),
                                                  `Pr(>|t|)`   <= 0.05 &  `Pr(>|t|)` > 0.01 ~  paste0(round(as.numeric(ftth$Estimate), 3), "**"),
                                                  `Pr(>|t|)`   <= 0.1 &  `Pr(>|t|)` > 0.05 ~  paste0(round(as.numeric(ftth$Estimate), 3), "*"),
                                                  `Pr(>|t|)`   > 0.1 ~  paste0(round(as.numeric(ftth$Estimate), 3))
))
ftth <- rbind(ftth, lm_ftth[[1]][["5 %"]][length(lm_ftth[[1]][["5 %"]])])

lm_wire[[1]][["Estimate"]] <- as.numeric(levels(lm_wire[[1]][["Estimate"]]))[lm_wire[[1]][["Estimate"]]]
lm_wire[[1]][["Pr(>|t|)"]] <- as.numeric(levels(lm_wire[[1]][["Pr(>|t|)"]]))[lm_wire[[1]][["Pr(>|t|)"]]]
lm_wire[[1]][["Std. Error"]] <- as.numeric(levels(lm_wire[[1]][["Std. Error"]]))[lm_wire[[1]][["Std. Error"]]]

wire <- as.data.frame(lm_wire[[1]][c(17:21), c(2,3,5)])
wire$`Std. Error` <- round(wire$`Std. Error`, 3)
wire$Estimate <- round(wire$Estimate, 3)
wire <- wire %>% mutate(est_sig = case_when(`Pr(>|t|)` <= 0.01 ~ paste0(round(as.numeric(wire$Estimate), 3), "***"),
                                            `Pr(>|t|)`   <= 0.05 &  `Pr(>|t|)` > 0.01 ~  paste0(round(as.numeric(wire$Estimate), 3), "**"),
                                            `Pr(>|t|)`   <= 0.1 &  `Pr(>|t|)` > 0.05 ~  paste0(round(as.numeric(wire$Estimate), 3), "*"),
                                            `Pr(>|t|)`   > 0.1 ~  paste0(round(as.numeric(wire$Estimate), 3))
))
wire <- rbind(wire, lm_wire[[1]][["5 %"]][length(lm_wire[[1]][["5 %"]])])

lm_dsl[[1]][["Estimate"]] <- as.numeric(levels(lm_dsl[[1]][["Estimate"]]))[lm_dsl[[1]][["Estimate"]]]
lm_dsl[[1]][["Pr(>|t|)"]] <- as.numeric(levels(lm_dsl[[1]][["Pr(>|t|)"]]))[lm_dsl[[1]][["Pr(>|t|)"]]]
lm_dsl[[1]][["Std. Error"]] <- as.numeric(levels(lm_dsl[[1]][["Std. Error"]]))[lm_dsl[[1]][["Std. Error"]]]

dsl <- as.data.frame(lm_dsl[[1]][c(17:21), c(2,3,5)])
dsl$`Std. Error` <- round(dsl$`Std. Error`, 3)
dsl$Estimate <- round(dsl$Estimate, 3)
dsl <- dsl %>% mutate(est_sig = case_when(`Pr(>|t|)` <= 0.01 ~ paste0(round(as.numeric(dsl$Estimate), 3), "***"),
                                            `Pr(>|t|)`   <= 0.05 &  `Pr(>|t|)` > 0.01 ~  paste0(round(as.numeric(dsl$Estimate), 3), "**"),
                                            `Pr(>|t|)`   <= 0.1 &  `Pr(>|t|)` > 0.05 ~  paste0(round(as.numeric(dsl$Estimate), 3), "*"),
                                            `Pr(>|t|)`   > 0.1 ~  paste0(round(as.numeric(dsl$Estimate), 3))
))

dsl <- rbind(dsl, lm_dsl[[1]][["5 %"]][length(lm_dsl[[1]][["5 %"]])])

table_main <- as.data.frame(ftth[,c(1,2)])
table_main <- cbind(table_main, as.data.frame(wire[,c(1,2)]))
table_main <- cbind(table_main, as.data.frame(dsl[,c(1,2)]))
rownames(table_main) <- varnames
names(table_main) <- rep(c("Estimate", "SE"),3)

############## FEs on MATCHED #####################

lmfe_ftth <- readRDS("BIP_linear_reg/fe_matched_ftth.RDS")
lmfe_wire <- readRDS("BIP_linear_reg/fe_matched_wire.RDS")
lmfe_dsl <- readRDS("BIP_linear_reg/fe_matched_dsl.RDS")

lmfe_ftth[[1]][["Estimate"]] <- as.numeric(levels(lmfe_ftth[[1]][["Estimate"]]))[lmfe_ftth[[1]][["Estimate"]]]
lmfe_ftth[[1]][["Pr(>|t|)"]] <- as.numeric(levels(lmfe_ftth[[1]][["Pr(>|t|)"]]))[lmfe_ftth[[1]][["Pr(>|t|)"]]]
lmfe_ftth[[1]][["Std. Error"]] <- as.numeric(levels(lmfe_ftth[[1]][["Std. Error"]]))[lmfe_ftth[[1]][["Std. Error"]]]

lmfema_ftth <- as.data.frame(lmfe_ftth[[1]][c(11:15), c(2,3,5)])
lmfema_ftth$`Std. Error` <- round(lmfema_ftth$`Std. Error`, 3)
lmfema_ftth$Estimate <- round(lmfema_ftth$Estimate, 3)
lmfema_ftth <- lmfema_ftth %>% mutate(est_sig = case_when(`Pr(>|t|)` <= 0.01 ~ paste0(round(as.numeric(lmfema_ftth$Estimate), 3), "***"),
                                                      `Pr(>|t|)`   <= 0.05 &  `Pr(>|t|)` > 0.01 ~  paste0(round(as.numeric(lmfema_ftth$Estimate), 3), "**"),
                                                      `Pr(>|t|)`   <= 0.1 &  `Pr(>|t|)` > 0.05 ~  paste0(round(as.numeric(lmfema_ftth$Estimate), 3), "*"),
                                                      `Pr(>|t|)`   > 0.1 ~  paste0(round(as.numeric(lmfema_ftth$Estimate), 3))
))
lmfema_ftth <- rbind(lmfema_ftth, lmfe_ftth[[1]][["5 %"]][length(lmfe_ftth[[1]][["5 %"]])])

lmfe_wire[[1]][["Estimate"]] <- as.numeric(levels(lmfe_wire[[1]][["Estimate"]]))[lmfe_wire[[1]][["Estimate"]]]
lmfe_wire[[1]][["Pr(>|t|)"]] <- as.numeric(levels(lmfe_wire[[1]][["Pr(>|t|)"]]))[lmfe_wire[[1]][["Pr(>|t|)"]]]
lmfe_wire[[1]][["Std. Error"]] <- as.numeric(levels(lmfe_wire[[1]][["Std. Error"]]))[lmfe_wire[[1]][["Std. Error"]]]

lmfema_wire <- as.data.frame(lmfe_wire[[1]][c(11:15), c(2,3,5)])
lmfema_wire$`Std. Error` <- round(lmfema_wire$`Std. Error`, 3)
lmfema_wire$Estimate <- round(lmfema_wire$Estimate, 3)
lmfema_wire <- lmfema_wire %>% mutate(est_sig = case_when(`Pr(>|t|)` <= 0.01 ~ paste0(round(as.numeric(lmfema_wire$Estimate), 3), "***"),
                                                          `Pr(>|t|)`   <= 0.05 &  `Pr(>|t|)` > 0.01 ~  paste0(round(as.numeric(lmfema_wire$Estimate), 3), "**"),
                                                          `Pr(>|t|)`   <= 0.1 &  `Pr(>|t|)` > 0.05 ~  paste0(round(as.numeric(lmfema_wire$Estimate), 3), "*"),
                                                          `Pr(>|t|)`   > 0.1 ~  paste0(round(as.numeric(lmfema_wire$Estimate), 3))
))
lmfema_wire <- rbind(lmfema_wire, lmfe_wire[[1]][["5 %"]][length(lmfe_wire[[1]][["5 %"]])])

lmfe_dsl[[1]][["Estimate"]] <- as.numeric(levels(lmfe_dsl[[1]][["Estimate"]]))[lmfe_dsl[[1]][["Estimate"]]]
lmfe_dsl[[1]][["Pr(>|t|)"]] <- as.numeric(levels(lmfe_dsl[[1]][["Pr(>|t|)"]]))[lmfe_dsl[[1]][["Pr(>|t|)"]]]
lmfe_dsl[[1]][["Std. Error"]] <- as.numeric(levels(lmfe_dsl[[1]][["Std. Error"]]))[lmfe_dsl[[1]][["Std. Error"]]]

lmfema_dsl <- as.data.frame(lmfe_dsl[[1]][c(11:15), c(2,3,5)])
lmfema_dsl$`Std. Error` <- round(lmfema_dsl$`Std. Error`, 3)
lmfema_dsl$Estimate <- round(lmfema_dsl$Estimate, 3)
lmfema_dsl <- lmfema_dsl %>% mutate(est_sig = case_when(`Pr(>|t|)` <= 0.01 ~ paste0(round(as.numeric(lmfema_dsl$Estimate), 3), "***"),
                                                          `Pr(>|t|)`   <= 0.05 &  `Pr(>|t|)` > 0.01 ~  paste0(round(as.numeric(lmfema_dsl$Estimate), 3), "**"),
                                                          `Pr(>|t|)`   <= 0.1 &  `Pr(>|t|)` > 0.05 ~  paste0(round(as.numeric(lmfema_dsl$Estimate), 3), "*"),
                                                          `Pr(>|t|)`   > 0.1 ~  paste0(round(as.numeric(lmfema_dsl$Estimate), 3))
))
lmfema_dsl <- rbind(lmfema_dsl, lmfe_dsl[[1]][["5 %"]][length(lmfe_dsl[[1]][["5 %"]])])

table_main <- as.data.frame(lmfema_ftth[,c(1,2)])
table_main <- cbind(table_main, as.data.frame(lmfema_wire[,c(1,2)]))
table_main <- cbind(table_main, as.data.frame(lmfema_dsl[,c(1,2)]))
rownames(table_main) <- varnames
names(table_main) <- rep(c("Estimate", "SE"),3)

####################### MATCHING #########################

match_ftth <- fread("BIP_linear_reg/ftth_match.csv")
match_wire <- fread("BIP_linear_reg/wireless_match.csv")
match_dsl <- fread("BIP_linear_reg/dsl_match.csv")

match_ftth <- as.data.frame(match_ftth) 
match_ftth["tstat"] <- match_ftth$est/match_ftth$se
match_ftth["pval"] <- round(pt(match_ftth$tstat, match_ftth$wnobs), 3)
match_ftth["se"] <- round(match_ftth$se, 3)
match_ftth["est_round"] <- round(match_ftth$est, 3)
match_ftth <- match_ftth %>% mutate(est_sig = case_when(pval <= 0.01 ~ paste0(round(match_ftth$est, 3), "***"),
                                                      pval <= 0.05 &  pval > 0.01 ~  paste0(round(match_ftth$est, 3), "**"),
                                                      pval <= 0.1 &  pval > 0.05 ~  paste0(round(match_ftth$est, 3), "*"),
                                                      pval > 0.1 ~  paste0(round(match_ftth$est, 3))))
match_wire <- as.data.frame(match_wire) 
match_wire["tstat"] <- match_wire$est/match_wire$se
match_wire["pval"] <- round(pt(match_wire$tstat, match_wire$wnobs), 3)
match_wire["se"] <- round(match_wire$se, 3)
match_wire["est_round"] <- round(match_wire$est, 3)
match_wire <- match_wire %>% mutate(est_sig = case_when(pval <= 0.01 ~ paste0(round(match_wire$est, 3), "***"),
                                                        pval <= 0.05 &  pval > 0.01 ~  paste0(round(match_wire$est, 3), "**"),
                                                        pval <= 0.1 &  pval > 0.05 ~  paste0(round(match_wire$est, 3), "*"),
                                                        pval > 0.1 ~  paste0(round(match_wire$est, 3))))
match_dsl <- as.data.frame(match_dsl) 
match_dsl["tstat"] <- match_dsl$est/match_dsl$se
match_dsl["pval"] <- round(pt(match_dsl$tstat, match_dsl$wnobs), 3)
match_dsl["se"] <- round(match_dsl$se, 3)
match_dsl["est_round"] <- round(match_dsl$est, 3)
match_dsl <- match_dsl %>% mutate(est_sig = case_when(pval <= 0.01 ~ paste0(round(match_dsl$est, 3), "***"),
                                                        pval <= 0.05 &  pval > 0.01 ~  paste0(round(match_dsl$est, 3), "**"),
                                                        pval <= 0.1 &  pval > 0.05 ~  paste0(round(match_dsl$est, 3), "*"),
                                                        pval > 0.1 ~  paste0(round(match_dsl$est, 3))))
# table mathcing
match_main <- as.data.frame(match_ftth[1:6, c(9,4)])
match_main <- cbind(match_main, as.data.frame(match_wire[1:6, c(9,4)]))
match_main <- cbind(match_main, as.data.frame(match_dsl[1:6, c(9,4)]))
match_main <- rbind(match_main, c(sum(match_ftth[1:6, 5]),"", sum(match_wire[1:6, 5]), "" ,sum(match_dsl[1:6, 5]), ""))

varnames_m <- c("BIP_{2005-06}", "BIP_{2007-08}", "BIP_{2009-10}", "BIP_{2011-12}", 
                "BIP_{2012-13}", "BIP_{2015+}", "Observations")
rownames(match_main) <- varnames_m
names(match_main) <- rep(c("Estimate", "SE"), 3)

#####################
# PLOTS
#####################
yearnames <- c("07-08","09-10","11-12","13-14","15+")

ftth_10 <- as.data.frame(lm_ftth[[1]][17:21,c(2,6,7)])
rownames(ftth_10) <- yearnames
names(ftth_10) <- c("est1","lower1","upper1")
ftth_10$years <- as.character(yearnames)

wire_10 <- as.data.frame(lm_wire[[1]][17:21,c(2,6,7)])
rownames(wire_10) <- yearnames
names(wire_10) <- c("est2","lower2","upper2")

dsl_10 <- as.data.frame(lm_dsl[[1]][17:21,c(2,6,7)])
rownames(dsl_10) <- yearnames
names(dsl_10) <- c("est3","lower3","upper3")


plotmodels_tech <- cbind(ftth_10,wire_10,dsl_10)

plotmodels_tech$est3 <- as.numeric(plotmodels_tech$est3)*100
plotmodels_tech$est1 <- as.numeric(plotmodels_tech$est1)*100
plotmodels_tech$est2 <- as.numeric(plotmodels_tech$est2)*100

plotmodels_tech$lower3 <- as.numeric(plotmodels_tech$lower3)*100
plotmodels_tech$lower1 <- as.numeric(plotmodels_tech$lower1)*100
plotmodels_tech$lower2 <- as.numeric(plotmodels_tech$lower2)*100

plotmodels_tech$upper3 <- as.numeric(plotmodels_tech$upper3)*100
plotmodels_tech$upper1 <- as.numeric(plotmodels_tech$upper1)*100
plotmodels_tech$upper2 <- as.numeric(plotmodels_tech$upper2)*100

shapes <- c('FTTH' = 16, 'Wireless' = 15, "DSL" = 17)

ggplot(data=plotmodels_tech) + 
  geom_pointrange(aes(x=years, y=est1, ymin=upper1, ymax=lower1, shape="FTTH"), 
                  color="black",position = position_nudge(x = -0.1)) + 
  geom_pointrange(aes(x=years, y=est2, ymin=upper2, ymax=lower2, shape="Wireless"), 
                  color="black", position = position_nudge(x = 0.1)) + 
  geom_pointrange(aes(x=years, y=est3, ymin=upper3, ymax=lower3, shape="DSL"), 
                  color="black") + 
  geom_hline(yintercept=0, linetype="dashed", color="grey70") +
  geom_vline(xintercept ="09-10", linetype="dashed", color="grey70") +
  scale_shape_manual(name = "Legend", values = shapes) + 
  labs(#title = "Estimated Coefficients on the Interactions between BIP and 2-year Period Dummies", 
    x="BIP x 2-Year Period Indicators", 
    y = "Estimate, %")+
  theme_classic()
ggsave("BIP_linear_reg/lm_techplot.png",height=4.5,width=6)

lmma_ftth <- as.data.frame(lmfe_ftth[[1]][11:15,c(2,6,7)])
rownames(lmma_ftth) <- yearnames
names(lmma_ftth) <- c("est1","lower1","upper1")
lmma_ftth$years <- as.character(yearnames)

lmma_wire <- as.data.frame(lmfe_wire[[1]][11:15,c(2,6,7)])
rownames(lmma_wire) <- yearnames
names(lmma_wire) <- c("est2","lower2","upper2")

lmma_dsl <- as.data.frame(lmfe_dsl[[1]][11:15,c(2,6,7)])
rownames(lmma_dsl) <- yearnames
names(lmma_dsl) <- c("est3","lower3","upper3")

plotmodels_tech <- cbind(lmma_ftth,lmma_wire,lmma_dsl)

plotmodels_tech$est3 <- as.numeric(plotmodels_tech$est3)*100
plotmodels_tech$est1 <- as.numeric(plotmodels_tech$est1)*100
plotmodels_tech$est2 <- as.numeric(plotmodels_tech$est2)*100

plotmodels_tech$lower3 <- as.numeric(plotmodels_tech$lower3)*100
plotmodels_tech$lower1 <- as.numeric(plotmodels_tech$lower1)*100
plotmodels_tech$lower2 <- as.numeric(plotmodels_tech$lower2)*100

plotmodels_tech$upper3 <- as.numeric(plotmodels_tech$upper3)*100
plotmodels_tech$upper1 <- as.numeric(plotmodels_tech$upper1)*100
plotmodels_tech$upper2 <- as.numeric(plotmodels_tech$upper2)*100

shapes <- c('FTTH' = 16, 'Wireless' = 15, "DSL" = 17)

ggplot(data=plotmodels_tech) + 
  geom_pointrange(aes(x=years, y=est1, ymin=upper1, ymax=lower1, shape="FTTH"), 
                  color="black",position = position_nudge(x = -0.1)) + 
  geom_pointrange(aes(x=years, y=est2, ymin=upper2, ymax=lower2, shape="Wireless"), 
                  color="black", position = position_nudge(x = 0.1)) + 
  geom_pointrange(aes(x=years, y=est3, ymin=upper3, ymax=lower3, shape="DSL"), 
                  color="black") + 
  geom_hline(yintercept=0, linetype="dashed", color="grey70") +
  geom_vline(xintercept ="09-10", linetype="dashed", color="grey70") +
  scale_shape_manual(name = "Legend", values = shapes) + 
  labs(#title = "Estimated Coefficients on the Interactions between BIP and 2-year Period Dummies", 
    x="BIP x 2-Year Period Indicators", 
    y = "Estimate, %")+
  theme_classic()
ggsave("BIP_linear_reg/lmma_techplot.png",height=4.5,width=6)

ma_ftth <- match_ftth[2:6,]
ma_ftth$lower <- ma_ftth$est - 1.96*ma_ftth$se
ma_ftth$upper <- ma_ftth$est + 1.96*ma_ftth$se
ma_ftth <- ma_ftth %>% dplyr::select("est","lower","upper")
rownames(ma_ftth) <- yearnames

ma_wire <- match_wire[2:6,]
ma_wire$lower <- ma_wire$est - 1.96*ma_wire$se
ma_wire$upper <- ma_wire$est + 1.96*ma_wire$se
ma_wire <- ma_wire %>% dplyr::select("est","lower","upper")

ma_dsl <- match_dsl[2:6,]
ma_dsl$lower <- ma_dsl$est - 1.96*ma_dsl$se
ma_dsl$upper <- ma_dsl$est + 1.96*ma_dsl$se
ma_dsl <- ma_dsl %>% dplyr::select("est","lower","upper")
ma_dsl$years <- as.character(yearnames)

plotmodels_tech <- cbind(ma_ftth, ma_wire, ma_dsl)
colnames(plotmodels_tech) <- c("est1", "lower1", "upper1", "est2", "lower2", "upper2", "est3", "lower3", "upper3", "years")

plotmodels_tech$est3 <- as.numeric(plotmodels_tech$est3)*100
plotmodels_tech$est1 <- as.numeric(plotmodels_tech$est1)*100
plotmodels_tech$est2 <- as.numeric(plotmodels_tech$est2)*100

plotmodels_tech$lower3 <- as.numeric(plotmodels_tech$lower3)*100
plotmodels_tech$lower1 <- as.numeric(plotmodels_tech$lower1)*100
plotmodels_tech$lower2 <- as.numeric(plotmodels_tech$lower2)*100

plotmodels_tech$upper3 <- as.numeric(plotmodels_tech$upper3)*100
plotmodels_tech$upper1 <- as.numeric(plotmodels_tech$upper1)*100
plotmodels_tech$upper2 <- as.numeric(plotmodels_tech$upper2)*100

shapes <- c('FTTH' = 16, 'Wireless' = 15, "DSL" = 17)

ggplot(data=plotmodels_tech) + 
  geom_pointrange(aes(x=years, y=est1, ymin=upper1, ymax=lower1, shape="FTTH"), 
                  color="black",position = position_nudge(x = -0.1)) + 
  geom_pointrange(aes(x=years, y=est2, ymin=upper2, ymax=lower2, shape="Wireless"), 
                  color="black", position = position_nudge(x = 0.1)) + 
  geom_pointrange(aes(x=years, y=est3, ymin=upper3, ymax=lower3, shape="DSL"), 
                  color="black") + 
  geom_hline(yintercept=0, linetype="dashed", color="grey70") +
  geom_vline(xintercept ="09-10", linetype="dashed", color="grey70") +
  scale_shape_manual(name = "Legend", values = shapes) + 
  labs(#title = "Estimated Coefficients on the Interactions between BIP and 2-year Period Dummies", 
    x="BIP x 2-Year Period Indicators", 
    y = "Estimate, %")+
  theme_classic()
ggsave("BIP_linear_reg/ma_techplot.png",height=4.5,width=6)

