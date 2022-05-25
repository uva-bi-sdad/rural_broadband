# Error bar plots

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
model <- read_xlsx(paste0(path, "pool_res_402222.xlsx"), sheet = "model")
model_tr <- read_xlsx(paste0(path, "pool_res_402222.xlsx"), sheet = "model_tract")

############################
# ERROR BAR PLOT
############################

#############################
# year effects
#############################
est_df <- data.frame(matrix(nrow = 5))

#est_df['var'] <- c("BIPxYEAR2007-8", "BIPxYEAR2009-10", "BIPxYEAR2011-12", "BIPxYEAR2013-14", "BIPxYEAR2015plus")
est_df['var'] <- c("YEAR2007-8", "YEAR2009-10", "YEAR2011-12", "YEAR2013-14", "YEAR2015plus")

est_df['est_model'] <- model[18:22, "Estimate"]
est_df$est_model <- as.numeric(est_df$est_model)

est_df['std_model'] <- model[18:22, "Std. Error"]
est_df$std_model <- as.numeric(est_df$std_model)

est_df['est_model_tr'] <- model_tr[18:22, "Estimate"]
est_df$est_model_tr <- as.numeric(est_df$est_model_tr)

est_df['std_model_tr'] <- model_tr[18:22, "Std. Error"]
est_df$std_model_tr <- as.numeric(est_df$std_model_tr)

colors <- c("Model Base" = "#D55E00", "With Tract FEs" = "#56B4E9")

ggplot(est_df, aes(x=var)) + 
  geom_line(aes(y = est_model, group=1, color = "Model Base")) + 
  geom_point(aes(y = est_model, group=1, color = "Model Base")) +
  geom_errorbar(aes(ymin=est_model-std_model, ymax=est_model+std_model, color = "Model Base"), width=.2,
                position=position_dodge(0.05)) +
  geom_line(aes(y = est_model_tr, group=1, color="With Tract FEs")) +
  geom_point(aes(y = est_model_tr, group=1, color="With Tract FEs")) +
  geom_errorbar(aes(ymin=est_model_tr-std_model_tr, ymax=est_model_tr+std_model_tr, color="With Tract FEs"), width=.2,
                position=position_dodge(0.05)) +
  labs(title = "Estimated Coefficients on the 2-Year Dummy Variables", x="2-Year Indicators", 
       y = "Estimate", color = "Legend")+
  theme_classic() + 
  scale_color_manual(values = colors) + theme(legend.position="bottom")

##################
# year x BIP
##################

est_df <- data.frame(matrix(nrow = 5))

est_df['var'] <- c("BIPxYEAR2007-8", "BIPxYEAR2009-10", "BIPxYEAR2011-12", "BIPxYEAR2013-14", "BIPxYEAR2015plus")

est_df['est_model'] <- model[26:30, "Estimate"]
est_df$est_model <- as.numeric(est_df$est_model)

est_df['std_model'] <- model[26:30, "Std. Error"]
est_df$std_model <- as.numeric(est_df$std_model)

est_df['est_model_tr'] <- model_tr[26:30, "Estimate"]
est_df$est_model_tr <- as.numeric(est_df$est_model_tr)

est_df['std_model_tr'] <- model_tr[26:30, "Std. Error"]
est_df$std_model_tr <- as.numeric(est_df$std_model_tr)

ggplot(est_df, aes(x=var)) + 
  geom_line(aes(y = est_model, group=1, color = "Model Base")) + 
  geom_point(aes(y = est_model, group=1, color = "Model Base")) +
  geom_errorbar(aes(ymin=est_model-std_model, ymax=est_model+std_model, color = "Model Base"), width=.2,
                position=position_dodge(0.05)) +
  geom_line(aes(y = est_model_tr, group=1, color="With Tract FEs")) +
  geom_point(aes(y = est_model_tr, group=1, color="With Tract FEs")) +
  geom_errorbar(aes(ymin=est_model_tr-std_model_tr, ymax=est_model_tr+std_model_tr, color="With Tract FEs"), width=.2,
                position=position_dodge(0.05)) +
  labs(title = "Estimated Coefficients on the Interactions b/w BIP and 2-year Dummies", x="BIP Indicator x 2-year Dummy", 
       y = "Estimate", color = "Legend")+
  theme_classic() + 
  scale_color_manual(values = colors) + theme(legend.position="bottom")

###################
# tech dummies
###################

est_df <- data.frame(matrix(nrow = 9))

est_df['var'] <- c("ftth_grop", "ftth_ptp", "ftth", "fixed_wireless", "adsl", "vdsl", "mobile_wireless",
                   "hfc_cable", "power_line")

est_df['est_model'] <- model[9:17, "Estimate"]
est_df$est_model <- as.numeric(est_df$est_model)

est_df['std_model'] <- model[9:17, "Std. Error"]
est_df$std_model <- as.numeric(est_df$std_model)

est_df['est_model_tr'] <- model_tr[9:17, "Estimate"]
est_df$est_model_tr <- as.numeric(est_df$est_model_tr)

est_df['std_model_tr'] <- model_tr[9:17, "Std. Error"]
est_df$std_model_tr <- as.numeric(est_df$std_model_tr)

ggplot(est_df, aes(x=var)) + 
  geom_line(aes(y = est_model, group=1, color = "Model Base")) + 
  geom_point(aes(y = est_model, group=1, color = "Model Base")) +
  geom_errorbar(aes(ymin=est_model-std_model, ymax=est_model+std_model, color = "Model Base"), width=.2,
                position=position_dodge(0.05)) +
  geom_line(aes(y = est_model_tr, group=1, color="With Tract FEs")) +
  geom_point(aes(y = est_model_tr, group=1, color="With Tract FEs")) +
  geom_errorbar(aes(ymin=est_model_tr-std_model_tr, ymax=est_model_tr+std_model_tr, color="With Tract FEs"), width=.2,
                position=position_dodge(0.05)) +
  labs(title = "Estimated Coefficients on Technology Dummies", x="Technolody Indicators", 
       y = "Estimate", color = "Legend")+
  theme_classic() + 
  scale_color_manual(values = colors) + theme(legend.position="bottom")

####################
# tech x bip
####################
# data in
model_tech <- read_xlsx(paste0(path, "pool_res_402222.xlsx"), sheet = "model")
model_tr <- read_xlsx(paste0(path, "pool_res_402222.xlsx"), sheet = "model_tract")