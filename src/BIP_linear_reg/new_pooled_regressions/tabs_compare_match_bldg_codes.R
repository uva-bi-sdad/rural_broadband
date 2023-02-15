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

# load in results
lmfixed_match <- readRDS("BIP_linear_reg/lm_fe_matched_robust10.RDS")
bldg_match <- readRDS("BIP_linear_reg/lm_fe_matched_robust10_blng_codes.RDS") 
bldg_sel_match <- readRDS("BIP_linear_reg/lm_fe_matched_robust10_bldg_codes_sel.RDS")
  
varnames <- c("BIP", "Year 2007-08", "Year 2009-10", "Year 2011-12", "Year 2013-14", "Year 2015+",
              "BIP x Year 2007-08", "BIP Year 2009-10", "BIP x Year 2011-12", 
              "BIP x Year 2013-14", "BIP x Year 2015+", "Observations")

###################
# TABLE
###################

lmfema_10 <- as.data.frame(lmfixed_match[c(2, 5:9, 13:17), c(1,2,4)])
lmfema_10$`Std. Error` <- round(as.numeric(lmfema_10$`Std. Error`), 3)
lmfema_10$Estimate <- round(as.numeric(lmfema_10$Estimate), 3)
lmfema_10 <- lmfema_10 %>% mutate(est_sig = case_when(`Pr(>|t|)` <= 0.01 ~ paste0(round(as.numeric(lmfema_10$Estimate), 3), "***"),
                                                      `Pr(>|t|)`   <= 0.05 &  `Pr(>|t|)` > 0.01 ~  paste0(round(as.numeric(lmfema_10$Estimate), 3), "**"),
                                                      `Pr(>|t|)`   <= 0.1 &  `Pr(>|t|)` > 0.05 ~  paste0(round(as.numeric(lmfema_10$Estimate), 3), "*"),
                                                      `Pr(>|t|)`   > 0.1 ~  paste0(round(as.numeric(lmfema_10$Estimate), 3))
))
lmfema_10 <- rbind(lmfema_10, lmfixed_match["Std. Error"][nrow(lmfixed_match["Std. Error"]),])

bldg_10 <- as.data.frame(bldg_match[c(2, 5:9, 43:47), c(1,2,4)])
bldg_10$`Std. Error` <- round(as.numeric(bldg_10$`Std. Error`), 3)
bldg_10$Estimate <- round(as.numeric(bldg_10$Estimate), 3)
bldg_10 <- bldg_10 %>% mutate(est_sig = case_when(`Pr(>|t|)` <= 0.01 ~ paste0(round(as.numeric(bldg_10$Estimate), 3), "***"),
                                                      `Pr(>|t|)`   <= 0.05 &  `Pr(>|t|)` > 0.01 ~  paste0(round(as.numeric(bldg_10$Estimate), 3), "**"),
                                                      `Pr(>|t|)`   <= 0.1 &  `Pr(>|t|)` > 0.05 ~  paste0(round(as.numeric(bldg_10$Estimate), 3), "*"),
                                                      `Pr(>|t|)`   > 0.1 ~  paste0(round(as.numeric(bldg_10$Estimate), 3))
))
bldg_10 <- rbind(bldg_10, bldg_match["Std. Error"][nrow(bldg_match["Std. Error"]),])

bldg_sel_10 <- as.data.frame(bldg_sel_match[c(2, 5:9, 25:29), c(1,2,4)])
bldg_sel_10$`Std. Error` <- round(as.numeric(bldg_sel_10$`Std. Error`), 3)
bldg_sel_10$Estimate <- round(as.numeric(bldg_sel_10$Estimate), 3)
bldg_sel_10 <- bldg_sel_10 %>% mutate(est_sig = case_when(`Pr(>|t|)` <= 0.01 ~ paste0(round(as.numeric(bldg_sel_10$Estimate), 3), "***"),
                                                  `Pr(>|t|)`   <= 0.05 &  `Pr(>|t|)` > 0.01 ~  paste0(round(as.numeric(bldg_sel_10$Estimate), 3), "**"),
                                                  `Pr(>|t|)`   <= 0.1 &  `Pr(>|t|)` > 0.05 ~  paste0(round(as.numeric(bldg_sel_10$Estimate), 3), "*"),
                                                  `Pr(>|t|)`   > 0.1 ~  paste0(round(as.numeric(bldg_sel_10$Estimate), 3))
))
bldg_sel_10 <- rbind(bldg_sel_10, bldg_sel_match["Std. Error"][nrow(bldg_sel_match["Std. Error"]),])

table_main <- as.data.frame(lmfema_10[,c(1,2)])
table_main <- cbind(table_main, as.data.frame(bldg_10[,c(1,2)]))
table_main <- cbind(table_main, as.data.frame(bldg_sel_10[,c(1,2)]))

rownames(table_main) <- varnames
names(table_main) <- rep(c("Estimate", "SE"),3)

###################
# PLOT
###################

yearnames <- c("07-08","09-10","11-12","13-14","15+")

lmma_10 <- as.data.frame(lmfixed_match[13:17,c(1,5,6)])
rownames(lmma_10) <- yearnames
names(lmma_10) <- c("est1","lower1","upper1")
lmma_10$years <- as.character(yearnames)

lm_bldg_10 <- as.data.frame(bldg_match[43:47,c(1,5,6)])
rownames(lm_bldg_10) <- yearnames
names(lm_bldg_10) <- c("est2","lower2","upper2")

lm_bldgsel_10 <- as.data.frame(bldg_sel_match[25:29,c(1,5,6)])
rownames(lm_bldgsel_10) <- yearnames
names(lm_bldgsel_10) <- c("est3","lower3","upper3")

plotmodels_lm <- cbind(lmma_10,lm_bldg_10,lm_bldgsel_10)

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
ggsave("BIP_linear_reg/plot_bldg_codes_sel.png",height=4.5,width=6)
