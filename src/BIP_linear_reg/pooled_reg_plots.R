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
pooled <- read_xlsx(paste0(path, "pool_res_051122.xlsx"), sheet = "model")
pooled_tr <- read_xlsx(paste0(path, "pool_res_051122.xlsx"), sheet = "model_tract")

# data in
pooled10 <- read_xlsx(paste0(path, "pool_res_10mi_051122.xlsx"), sheet = "model")
pooled_tr10 <- read_xlsx(paste0(path, "pool_res_10mi_051122.xlsx"), sheet = "model_tract")

# initilize a df
est_df <- data.frame(matrix(nrow = 5))

est_df['var'] <- c("BIPxYEAR2007-8", "BIPxYEAR2009-10", "BIPxYEAR2011-12", "BIPxYEAR2013-14", "BIPxYEAR2015plus")

est_df['est1'] <- pooled[19:23, "Estimate"] 
est_df$est1 <- as.numeric(est_df$est1) * 100 

est_df['std1'] <- pooled[19:23, "Std. Error"]
est_df$std1 <- as.numeric(est_df$std1) * 100

est_df['est2'] <- pooled_tr[19:23, "Estimate"] 
est_df$est2 <- as.numeric(est_df$est2) * 100 

est_df['std2'] <- pooled_tr[19:23, "Std. Error"]
est_df$std2 <- as.numeric(est_df$std2) * 100

est_df['est3'] <- pooled10[19:23, "Estimate"] 
est_df$est3 <- as.numeric(est_df$est3) * 100 

est_df['std3'] <- pooled10[19:23, "Std. Error"]
est_df$std3 <- as.numeric(est_df$std3) * 100

est_df['est4'] <- pooled_tr10[19:23, "Estimate"] 
est_df$est4 <- as.numeric(est_df$est4) * 100 

est_df['std4'] <- pooled_tr10[19:23, "Std. Error"]
est_df$std4 <- as.numeric(est_df$std4) * 100

colors <- c("Pooled" = "#D55E00", "Pooled 10mi" = "#56B4E9")

ggplot(est_df, aes(x=var)) + 
  geom_line(aes(y = est1, group=1, color = "Pooled")) + 
  geom_point(aes(y = est1, group=1, color = "Pooled")) +
  geom_errorbar(aes(ymin=est1-std1, ymax=est1+std1, color = "Pooled"), width=.2,
                position=position_dodge(0.05)) + 
  geom_line(aes(y = est3, group=1, color = "Pooled 10mi"), linetype = "dashed") + 
  geom_point(aes(y = est3, group=1, color = "Pooled 10mi")) +
  geom_errorbar(aes(ymin=est3-std3, ymax=est3+std3, color = "Pooled 10mi"), width=.2,
                position=position_dodge(0.05)) + 
  geom_text(x="BIPxYEAR2013-14", y=-10, label="Nobs: 361,827; 10mi 352,628") +
  ylim(-10, 5) +
  labs(title = "Estimated Coefficients on the Interactions between BIP and 2-year Dummies", x="BIP x 2-Year Indicators", 
       y = "Estimate x 100", color = "Legend")+
  theme_classic() + 
  scale_color_manual(values = colors) + theme(legend.position="bottom")

ggplot(est_df, aes(x=var)) + 
  geom_line(aes(y = est2, group=1, color = "Pooled")) + 
  geom_point(aes(y = est2, group=1, color = "Pooled")) +
  geom_errorbar(aes(ymin=est2-std2, ymax=est2+std2, color = "Pooled"), width=.2,
                position=position_dodge(0.05)) + 
  geom_line(aes(y = est4, group=1, color = "Pooled 10mi"), linetype = "dashed") + 
  geom_point(aes(y = est4, group=1, color = "Pooled 10mi")) +
  geom_errorbar(aes(ymin=est4-std4, ymax=est4+std4, color = "Pooled 10mi"), width=.2,
                position=position_dodge(0.05)) + 
  geom_text(x="BIPxYEAR2013-14", y=5, label="Nobs: 293,979; 10mi 285,692") +
  ylim(-15, 5) +
  labs(title = "Estimated Coefficients on the Interactions between BIP and 2-year Dummies", x="BIP x 2-Year Indicators", 
       y = "Estimate x 100", color = "Legend")+
  theme_classic() + 
  scale_color_manual(values = colors) + theme(legend.position="bottom")
