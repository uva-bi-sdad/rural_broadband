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

# data in
pooled10 <- read_xlsx(paste0(path, "pool_res_10mi_052022.xlsx"), sheet = "model")
pooled_tr10 <- read_xlsx(paste0(path, "pool_res_10mi_052022.xlsx"), sheet = "model_tract")

# data in
pooled5 <- read_xlsx(paste0(path, "pool_res_10mi_052022.xlsx"), sheet = "model")
pooled_tr5 <- read_xlsx(paste0(path, "pool_res_10mi_052022.xlsx"), sheet = "model_tract")


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

# pooled 10mi
est_df['est10'] <- pooled10[19:23, "Estimate"] 
est_df$est10 <- as.numeric(est_df$est10) * 100 

est_df['low10'] <- pooled10[19:23, "5 %"]
est_df$low10 <- as.numeric(est_df$low10) * 100

est_df['up10'] <- pooled10[19:23, "95 %"]
est_df$up10 <- as.numeric(est_df$up10) * 100

# pooled 10mi with fes
est_df['est20'] <- pooled_tr10[19:23, "Estimate"] 
est_df$est20 <- as.numeric(est_df$est20) * 100 

est_df['low20'] <- pooled_tr10[19:23, "5 %"]
est_df$low20 <- as.numeric(est_df$low20) * 100

est_df['up20'] <- pooled_tr10[19:23, "95 %"]
est_df$up20 <- as.numeric(est_df$up20) * 100

# pooled 5mi
est_df['est15'] <- pooled5[19:23, "Estimate"] 
est_df$est15 <- as.numeric(est_df$est15) * 100 

est_df['low15'] <- pooled5[19:23, "5 %"]
est_df$low15 <- as.numeric(est_df$low15) * 100

est_df['up15'] <- pooled5[19:23, "95 %"]
est_df$up15 <- as.numeric(est_df$up15) * 100

# pooled 5mi with fes
est_df['est25'] <- pooled_tr5[19:23, "Estimate"] 
est_df$est25 <- as.numeric(est_df$est25) * 100 

est_df['low25'] <- pooled_tr5[19:23, "5 %"]
est_df$low25 <- as.numeric(est_df$low25) * 100

est_df['up25'] <- pooled_tr5[19:23, "95 %"]
est_df$up25 <- as.numeric(est_df$up25) * 100

colors <- c("base" = "#D55E00", "10mi" = "#56B4E9")
shapes <- c('Base w/ tract FEs' = 16, '10mi' = 15, "5mi" = 17)

ggplot(data=est_df) + 
  geom_pointrange(aes(x=var, y=est2, ymin=up2, ymax=low2, shape="Base w/ tract FEs"), 
                  color="black",position = position_nudge(x = -0.1)) + 
  geom_pointrange(aes(x=var, y=est20, ymin=up20, ymax=low20, shape="10mi"), 
                  color="black", position = position_nudge(x = 0.1)) + 
  geom_pointrange(aes(x=var, y=est25, ymin=up25, ymax=low25, shape="5mi"), 
                  color="black") + 
  geom_hline(yintercept=0, linetype="dashed", color="grey70") +
  geom_vline(xintercept ="BIPxYEAR2009-10", linetype="dashed", color="grey70") +
  ylim(-20, 5) +
  scale_shape_manual(name = "Legend", values = shapes) + 
  labs(#title = "Estimated Coefficients on the Interactions between BIP and 2-year Dummies", 
       x="BIP x 2-Year Indicators", 
       y = "Estimate x 100")+
  theme_classic()
  

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
