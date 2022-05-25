# Hetero regressions plots

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


####################
# TECH REGS
####################

# data in
ftth <- read_xlsx(paste0(path, "tech_res_050322.xlsx"), sheet = "ftth_tract")
wireless <- read_xlsx(paste0(path, "tech_res_050322.xlsx"), sheet = "wireless_tract")
dsl <- read_xlsx(paste0(path, "tech_res_050322.xlsx"), sheet = "dsl_tract")

# data in
ftth10 <- read_xlsx(paste0(path, "tech_res_10mi_051022.xlsx"), sheet = "ftth_tract")
wireless10 <- read_xlsx(paste0(path, "tech_res_10mi_051022.xlsx"), sheet = "wireless_tract")
dsl10 <- read_xlsx(paste0(path, "tech_res_10mi_051022.xlsx"), sheet = "dsl_tract")

# initilize a df
est_df <- data.frame(matrix(nrow = 5))

est_df['var'] <- c("BIPxYEAR2007-8", "BIPxYEAR2009-10", "BIPxYEAR2011-12", "BIPxYEAR2013-14", "BIPxYEAR2015plus")

est_df['est_ftth'] <- ftth[17:21, "Estimate"] 
est_df$est_ftth <- as.numeric(est_df$est_ftth) * 100 

est_df['std_ftth'] <- ftth[17:21, "Std. Error"]
est_df$std_ftth <- as.numeric(est_df$std_ftth) * 100

est_df['est_wireless'] <- wireless[16:20, "Estimate"]
est_df$est_wireless <- as.numeric(est_df$est_wireless)* 100

est_df['std_wireless'] <- wireless[16:20, "Std. Error"]
est_df$std_wireless <- as.numeric(est_df$std_wireless) * 100

est_df['est_dsl'] <- dsl[17:21, "Estimate"] 
est_df$est_dsl <- as.numeric(est_df$est_dsl) * 100 

est_df['std_dsl'] <- dsl[17:21, "Std. Error"]
est_df$std_dsl <- as.numeric(est_df$std_dsl) * 100

est_df['est_ftth10'] <- ftth10[17:21, "Estimate"] 
est_df$est_ftth10 <- as.numeric(est_df$est_ftth10) * 100 

est_df['std_ftth10'] <- ftth10[17:21, "Std. Error"]
est_df$std_ftth10 <- as.numeric(est_df$std_ftth10) * 100

est_df['est_wireless10'] <- wireless10[16:20, "Estimate"]
est_df$est_wireless10 <- as.numeric(est_df$est_wireless10)* 100

est_df['std_wireless10'] <- wireless10[16:20, "Std. Error"]
est_df$std_wireless10 <- as.numeric(est_df$std_wireless10) * 100

est_df['est_dsl10'] <- dsl10[17:21, "Estimate"] 
est_df$est_dsl10 <- as.numeric(est_df$est_dsl10) * 100 

est_df['std_dsl10'] <- dsl10[17:21, "Std. Error"]
est_df$std_dsl10 <- as.numeric(est_df$std_dsl10) * 100

colors <- c("FTTH" = "#D55E00", "Wireless" = "#56B4E9", "DSL" = "#009E73")

ggplot(est_df, aes(x=var)) + 
  geom_line(aes(y = est_ftth, group=1, color = "FTTH")) + 
  geom_point(aes(y = est_ftth, group=1, color = "FTTH")) +
  geom_errorbar(aes(ymin=est_ftth-std_ftth, ymax=est_ftth+std_ftth, color = "FTTH"), width=.2,
                position=position_dodge(0.05)) + 
  geom_line(aes(y = est_ftth10, group=1, color = "FTTH"), linetype = "dashed") + 
  geom_point(aes(y = est_ftth10, group=1, color = "FTTH")) +
  geom_errorbar(aes(ymin=est_ftth10-std_ftth10, ymax=est_ftth10+std_ftth10, color = "FTTH"), width=.2,
                position=position_dodge(0.05)) + 
  geom_text(x="BIPxYEAR2013-14", y=-17, label="FTTH Nobs: 126,969; 10mi 124,743") +
  
  geom_line(aes(y = est_wireless, group=1, color="Wireless")) +
  geom_point(aes(y = est_wireless, group=1, color="Wireless")) +
  geom_errorbar(aes(ymin=est_wireless-std_wireless, ymax=est_wireless+std_wireless, color="Wireless"), width=.2,
                position=position_dodge(0.05)) +
  geom_line(aes(y = est_wireless10, group=1, color="Wireless"),linetype = "dashed") +
  geom_point(aes(y = est_wireless10, group=1, color="Wireless")) +
  geom_errorbar(aes(ymin=est_wireless10-std_wireless10, ymax=est_wireless10+std_wireless10, color="Wireless"), width=.2,
                position=position_dodge(0.05)) +
  geom_text(x="BIPxYEAR2013-14", y=-19, label="Wireless Nobs: 128,513; 10mi 125,969") +
  
  geom_line(aes(y = est_dsl, group=1, color = "DSL"), linetype = "dashed") + 
  geom_point(aes(y = est_dsl, group=1, color = "DSL")) +
  geom_errorbar(aes(ymin=est_dsl-std_dsl, ymax=est_dsl+std_dsl, color = "DSL"), width=.2,
                position=position_dodge(0.05)) + 
  geom_line(aes(y = est_dsl10, group=1, color = "DSL")) + 
  geom_point(aes(y = est_dsl10, group=1, color = "DSL")) +
  geom_errorbar(aes(ymin=est_dsl10-std_dsl10, ymax=est_dsl10+std_dsl10, color = "DSL"), width=.2,
                position=position_dodge(0.05)) + 
  geom_text(x="BIPxYEAR2013-14", y=-21, label="DSL Nobs: 35,789, 10mi 35,558") +
  ylim(-25, 20) +
  labs(title = "Estimated Coefficients on the Interactions between BIP and 2-year Dummies", x="BIP x 2-Year Indicators", 
       y = "Estimate x 100", color = "Legend")+
  theme_classic() + 
  scale_color_manual(values = colors) + theme(legend.position="bottom")

####################
# AWARD QUANTILES
####################

# data in
reg1 <- read_xlsx(paste0(path, "quant_res_050322.xlsx"), sheet = "quantile1_tract")
reg2 <- read_xlsx(paste0(path, "quant_res_050322.xlsx"), sheet = "quantile2_tract")
reg3 <- read_xlsx(paste0(path, "quant_res_050322.xlsx"), sheet = "quantile3_tract")

reg10 <- read_xlsx(paste0(path, "quant_res_10mi_051022.xlsx"), sheet = "quantile1_tract")
reg20 <- read_xlsx(paste0(path, "quant_res_10mi_051022.xlsx"), sheet = "quantile2_tract")
reg30 <- read_xlsx(paste0(path, "quant_res_10mi_051022.xlsx"), sheet = "quantile3_tract")

# initilize a df
est_df <- data.frame(matrix(nrow = 5))

est_df['var'] <- c("BIPxYEAR2007-8", "BIPxYEAR2009-10", "BIPxYEAR2011-12", "BIPxYEAR2013-14", "BIPxYEAR2015plus")

est_df['est1'] <- reg1[17:21, "Estimate"] 
est_df$est1 <- as.numeric(est_df$est1) * 100 

est_df['std1'] <- reg1[17:21, "Std. Error"]
est_df$std1 <- as.numeric(est_df$std1) * 100

est_df['est2'] <- reg2[17:21, "Estimate"] 
est_df$est2 <- as.numeric(est_df$est2) * 100 

est_df['std2'] <- reg2[17:21, "Std. Error"]
est_df$std2 <- as.numeric(est_df$std2) * 100

est_df['est3'] <- reg3[16:20, "Estimate"] 
est_df$est3 <- as.numeric(est_df$est3) * 100 

est_df['std3'] <- reg3[16:20, "Std. Error"]
est_df$std3 <- as.numeric(est_df$std3) * 100

est_df['est10'] <- reg10[17:21, "Estimate"] 
est_df$est10 <- as.numeric(est_df$est10) * 100 

est_df['std10'] <- reg10[17:21, "Std. Error"]
est_df$std10 <- as.numeric(est_df$std10) * 100

est_df['est20'] <- reg20[17:21, "Estimate"] 
est_df$est20 <- as.numeric(est_df$est20) * 100 

est_df['std20'] <- reg20[17:21, "Std. Error"]
est_df$std20 <- as.numeric(est_df$std20) * 100

est_df['est30'] <- reg30[16:20, "Estimate"] 
est_df$est30 <- as.numeric(est_df$est30) * 100 

est_df['std30'] <- reg30[16:20, "Std. Error"]
est_df$std30 <- as.numeric(est_df$std30) * 100

colors <- c("Percentile 0-33%" = "#D55E00", "Percentile 33-66%" = "#56B4E9", "Percentile 66-100%" = "#009E73")

ggplot(est_df, aes(x=var)) + 
  geom_line(aes(y = est1, group=1, color = "Percentile 0-33%")) + 
  geom_point(aes(y = est1, group=1, color = "Percentile 0-33%")) +
  geom_errorbar(aes(ymin=est1-std1, ymax=est1+std1, color = "Percentile 0-33%"), width=.2,
                position=position_dodge(0.05)) + 
  geom_line(aes(y = est10, group=1, color = "Percentile 0-33%"), linetype="dashed") + 
  geom_point(aes(y = est10, group=1, color = "Percentile 0-33%")) +
  geom_errorbar(aes(ymin=est10-std10, ymax=est10+std10, color = "Percentile 0-33%"), width=.2,
                position=position_dodge(0.05)) + 
  geom_text(x="BIPxYEAR2013-14", y=17, label="Percentile 0-33% Nobs: 164,192; 10mi 158,126") +
  
  geom_line(aes(y = est2, group=1, color="Percentile 33-66%")) +
  geom_point(aes(y = est2, group=1, color="Percentile 33-66%")) +
  geom_errorbar(aes(ymin=est2-std2, ymax=est2+std2, color="Percentile 33-66%"), width=.2,
                position=position_dodge(0.05)) +
  geom_line(aes(y = est20, group=1, color="Percentile 33-66%"), linetype="dashed") +
  geom_point(aes(y = est20, group=1, color="Percentile 33-66%")) +
  geom_errorbar(aes(ymin=est20-std20, ymax=est20+std20, color="Percentile 33-66%"), width=.2,
                position=position_dodge(0.05)) +
  geom_text(x="BIPxYEAR2013-14", y=19, label="Percentile 33-66% Nobs: 94,979: 10mi 93,022") +
  
  geom_line(aes(y = est3, group=1, color = "Percentile 66-100%")) + 
  geom_point(aes(y = est3, group=1, color = "Percentile 66-100%")) +
  geom_errorbar(aes(ymin=est3-std3, ymax=est3+std3, color = "Percentile 66-100%"), width=.2,
                position=position_dodge(0.05)) + 
  geom_line(aes(y = est30, group=1, color = "Percentile 66-100%"), linetype="dashed") + 
  geom_point(aes(y = est30, group=1, color = "Percentile 66-100%")) +
  geom_errorbar(aes(ymin=est30-std30, ymax=est30+std30, color = "Percentile 66-100%"), width=.2,
                position=position_dodge(0.05)) + 
  geom_text(x="BIPxYEAR2013-14", y=21, label="Percentile 66-100% Nobs: 34,767; 10mi 34,544") +
  ylim(-25, 20) +
  labs(title = "Estimated Coefficients on the Interactions between BIP and 2-year Dummies", x="BIP x 2-Year Indicators", 
       y = "Estimate x 100", color = "Legend")+
  theme_classic() + 
  scale_color_manual(values = colors) + theme(legend.position="bottom")

##############
# RUCA AREAS
##############

# data in
reg1 <- read_xlsx(paste0(path, "ruca_res_050322.xlsx"), sheet = "metro_tract")
reg2 <- read_xlsx(paste0(path, "ruca_res_050322.xlsx"), sheet = "micro_tract")
reg3 <- read_xlsx(paste0(path, "ruca_res_050322.xlsx"), sheet = "rural_tract")

reg10 <- read_xlsx(paste0(path, "ruca_res_10mi_051022.xlsx"), sheet = "metro_tract")
reg20 <- read_xlsx(paste0(path, "ruca_res_10mi_051022.xlsx"), sheet = "micro_tract")
reg30 <- read_xlsx(paste0(path, "ruca_res_10mi_051022.xlsx"), sheet = "rural_tract")


# initilize a df
est_df <- data.frame(matrix(nrow = 5))

est_df['var'] <- c("BIPxYEAR2007-8", "BIPxYEAR2009-10", "BIPxYEAR2011-12", "BIPxYEAR2013-14", "BIPxYEAR2015plus")

est_df['est1'] <- reg1[19:23, "Estimate"] 
est_df$est1 <- as.numeric(est_df$est1) * 100 

est_df['std1'] <- reg1[19:23, "Std. Error"]
est_df$std1 <- as.numeric(est_df$std1) * 100

est_df['est2'] <- reg2[19:23, "Estimate"] 
est_df$est2 <- as.numeric(est_df$est2) * 100 

est_df['std2'] <- reg2[19:23, "Std. Error"]
est_df$std2 <- as.numeric(est_df$std2) * 100

est_df['est3'] <- reg3[19:23, "Estimate"] 
est_df$est3 <- as.numeric(est_df$est3) * 100 

est_df['std3'] <- reg3[19:23, "Std. Error"]
est_df$std3 <- as.numeric(est_df$std3) * 100

est_df['est10'] <- reg10[19:23, "Estimate"] 
est_df$est10 <- as.numeric(est_df$est10) * 100 

est_df['std10'] <- reg10[19:23, "Std. Error"]
est_df$std10 <- as.numeric(est_df$std10) * 100

est_df['est20'] <- reg20[19:23, "Estimate"] 
est_df$est20 <- as.numeric(est_df$est20) * 100 

est_df['std20'] <- reg20[19:23, "Std. Error"]
est_df$std20 <- as.numeric(est_df$std20) * 100

est_df['est30'] <- reg30[19:23, "Estimate"] 
est_df$est30 <- as.numeric(est_df$est30) * 100 

est_df['std30'] <- reg30[19:23, "Std. Error"]
est_df$std30 <- as.numeric(est_df$std30) * 100

colors <- c("Metro" = "#D55E00", "Micro" = "#56B4E9", "Rural" = "#009E73")

ggplot(est_df, aes(x=var)) + 
  geom_line(aes(y = est1, group=1, color = "Metro")) + 
  geom_point(aes(y = est1, group=1, color = "Metro")) +
  geom_errorbar(aes(ymin=est1-std1, ymax=est1+std1, color = "Metro"), width=.2,
                position=position_dodge(0.05)) + 
  geom_line(aes(y = est10, group=1, color = "Metro"), linetype = "dashed") + 
  geom_point(aes(y = est10, group=1, color = "Metro")) +
  geom_errorbar(aes(ymin=est10-std10, ymax=est10+std10, color = "Metro"), width=.2,
                position=position_dodge(0.05)) + 
  geom_text(x="BIPxYEAR2013-14", y=17, label="Metro Nobs: 177,885; 10mi 176,237") +
  
  geom_line(aes(y = est2, group=1, color="Micro")) +
  geom_point(aes(y = est2, group=1, color="Micro")) +
  geom_errorbar(aes(ymin=est2-std2, ymax=est2+std2, color="Micro"), width=.2,
                position=position_dodge(0.05)) +
  geom_line(aes(y = est20, group=1, color="Micro"), linetype="dashed") +
  geom_point(aes(y = est20, group=1, color="Micro")) +
  geom_errorbar(aes(ymin=est20-std20, ymax=est20+std20, color="Micro"), width=.2,
                position=position_dodge(0.05)) +
  geom_text(x="BIPxYEAR2013-14", y=19, label="Micro Nobs: 55,236; 10mi 52,068") +
  
  geom_line(aes(y = est3, group=1, color = "Rural")) + 
  geom_point(aes(y = est3, group=1, color = "Rural")) +
  geom_errorbar(aes(ymin=est3-std3, ymax=est3+std3, color = "Rural"), width=.2,
                position=position_dodge(0.05)) + 
  geom_line(aes(y = est30, group=1, color = "Rural"), linetype="dashed") + 
  geom_point(aes(y = est30, group=1, color = "Rural")) +
  geom_errorbar(aes(ymin=est30-std30, ymax=est30+std30, color = "Rural"), width=.2,
                position=position_dodge(0.05)) + 
  geom_text(x="BIPxYEAR2013-14", y=21, label="Rural Nobs: 46,696; 10mi 43,314") +
  ylim(-25, 20) +
  labs(title = "Estimated Coefficients on the Interactions between BIP and 2-year Dummies", x="BIP x 2-Year Indicators", 
       y = "Estimate x 100", color = "Legend")+
  theme_classic() + 
  scale_color_manual(values = colors) + theme(legend.position="bottom")

