# Pooled regression error bar plots

# packages
library(readr)
library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
library(haven)
library(tidycensus)
library(tigris)
library(maps)

# your working directory

setwd("~/git/rural_broadband/src")

# path to data in Rivanna project folder
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/BIP_linear_data/"

# data in
pooled <- read_xlsx(paste0(path, "pool_res_052322.xlsx"), sheet = "model")
pooled_tr <- read_xlsx(paste0(path, "pool_res_052322.xlsx"), sheet = "model_tract")
pooled_acs <- read_xlsx(paste0(path, "pool_res_ACS_052022.xlsx"), sheet= "model")


# initilize a df
est_df <- data.frame(matrix(nrow = 5))

est_df['var'] <- c("BIPxYEAR2007-8", "BIPxYEAR2009-10", "BIPxYEAR2011-12", "BIPxYEAR2013-14", "BIPxYEAR2015plus")

# pooled
est_df['est1'] <- pooled[19:23, "Estimate"] 
est_df$est1 <- as.numeric(est_df$est1) * 100 

est_df['low1'] <- pooled[19:23, "5 %"]
est_df$low1 <- as.numeric(est_df$low1) * 100

est_df['up1'] <- pooled[19:23, "95 %"]
est_df$up1 <- as.numeric(est_df$up1) * 100

# pooled with fes
est_df['est2'] <- pooled_tr[19:23, "Estimate"] 
est_df$est2 <- as.numeric(est_df$est2) * 100 

est_df['low2'] <- pooled_tr[19:23, "5 %"]
est_df$low2 <- as.numeric(est_df$low2) * 100

est_df['up2'] <- pooled_tr[19:23, "95 %"]
est_df$up2 <- as.numeric(est_df$up2) * 100

# pooled with ACS vars
est_df['est3'] <- pooled_acs[19:23, "Estimate"] 
est_df$est3 <- as.numeric(est_df$est3) * 100 

est_df['low3'] <- pooled_acs[19:23, "5 %"]
est_df$low3 <- as.numeric(est_df$low3) * 100

est_df['up3'] <- pooled_acs[19:23, "95 %"]
est_df$up3 <- as.numeric(est_df$up3) * 100

shapes <- c('Base' = 16, 'Tract FEs' = 15, "ACS controls" = 17)
ggplot(data=est_df) + 
  geom_pointrange(aes(x=var, y=est1, ymin=up1, ymax=low1, shape="Base"), 
                  color="black",position = position_nudge(x = -0.1)) + 
  geom_pointrange(aes(x=var, y=est2, ymin=up2, ymax=low2, shape="Tract FEs"), 
                  color="black", position = position_nudge(x = 0.1)) + 
  geom_pointrange(aes(x=var, y=est3, ymin=up3, ymax=low3, shape="ACS controls"), 
                  color="black") + 
  geom_hline(yintercept=0, linetype="dashed", color="grey70") +
  geom_vline(xintercept ="BIPxYEAR2009-10", linetype="dashed", color="grey70") +
  ylim(-20, 5) +
  scale_shape_manual(name = "Legend", values = shapes) + 
  labs(#title = "Estimated Coefficients on the Interactions between BIP and 2-year Period Dummies", 
    x="BIP x 2-Year Indicators", 
    y = "Estimate x 100")+
  theme_classic()


