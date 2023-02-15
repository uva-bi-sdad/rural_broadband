# Heteregenous effects error bar plots
# Mahalnobis matching results
# Compare using windows of 0-10mi, 5-15mi, 10-20mi
# Plot all effects on the same scale

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
path <- "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/"

# load in files: linear model, linear model w/ fixed tract effects, mahalanobis matching
ftth_fit <- read_csv("BIP_linear_reg/heter_ftth_match.csv")
wireless_fit <- read_csv("BIP_linear_reg/heter_wireless_match.csv")
dsl_fit <- read_csv("BIP_linear_reg/heter_dsl_match.csv")

yearnames <- c("07-08","09-10","11-12","13-14","15+")

match_10_ftth <- ftth_fit[2:6,]
match_10_ftth['lower'] <- match_10_ftth$est - 1.96*match_10_ftth$se
match_10_ftth['upper'] <- match_10_ftth$est + 1.96*match_10_ftth$se
match_10_ftth <- match_10_ftth %>% dplyr::select("est","lower","upper")
rownames(match_10_ftth) <- yearnames
match_10_ftth$years <- yearnames; match_10_ftth$source <- "FTTH Matching 0-10mi"

match_10_wire <- wireless_fit[2:6,]
match_10_wire['lower'] <- match_10_wire$est - 1.96*match_10_wire$se
match_10_wire['upper'] <- match_10_wire$est + 1.96*match_10_wire$se
match_10_wire <- match_10_wire %>% dplyr::select("est","lower","upper")
rownames(match_10_wire) <- yearnames
match_10_wire$years <- yearnames; match_10_wire$source <- "Wireless Matching 0-10mi"

match_10_dsl <- dsl_fit[2:6,]
match_10_dsl['lower'] <- match_10_dsl$est - 1.96*match_10_dsl$se
match_10_dsl['upper'] <- match_10_dsl$est + 1.96*match_10_dsl$se
match_10_dsl <- match_10_dsl %>% dplyr::select("est","lower","upper")
rownames(match_10_dsl) <- yearnames
match_10_dsl$years <- yearnames; match_10_dsl$source <- "DSL Matching 0-10mi"

match_15_ftth <- ftth_fit[8:12,]
match_15_ftth$lower <- match_15_ftth$est - 1.96*match_15_ftth$se
match_15_ftth$upper <- match_15_ftth$est + 1.96*match_15_ftth$se
match_15_ftth <- match_15_ftth %>% dplyr::select("est","lower","upper")
rownames(match_15_ftth) <- yearnames
match_15_ftth$years <- yearnames; match_15_ftth$source <- "FTTH Matching 5-15mi"

match_20_ftth <- ftth_fit[14:18,]
match_20_ftth$lower <- match_20_ftth$est - 1.96*match_20_ftth$se
match_20_ftth$upper <- match_20_ftth$est + 1.96*match_20_ftth$se
match_20_ftth <- match_20_ftth %>% dplyr::select("est","lower","upper")
rownames(match_20_ftth) <- yearnames
match_20_ftth$years <- yearnames; match_20_ftth$source <- "FTTH Matching 10-20mi"

plotmodels10 <- rbind(match_10_ftth, match_10_wire, match_10_dsl)
plotmodels10$est <- exp( as.numeric(plotmodels10$est) )
plotmodels10$lower <- exp( as.numeric(plotmodels10$lower) )
plotmodels10$upper <- exp( as.numeric(plotmodels10$upper) )


# create plots
ggplot(plotmodels10, aes(x=years)) + 
  geom_point(aes(y = est, color = source),
             position=position_dodge(0.2)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, color = source), width=.2,
                position=position_dodge(0.2)) +
  ylim(0.8, 1.20) +
  labs(title = "Interactions between BIP and 2-year Indicators",
       subtitle = "Comparing sales 0-10 miles outside the BIP",
       x="BIP x 2-Year Indicators", 
       y = "Estimate", color = "Model")+
  theme_bw() + theme(legend.position="bottom")


# save plots as pdfs
pdf("BIP_linear_reg/10miplot.pdf",height=4.5,width=6)
p1
dev.off()

pdf("BIP_linear_reg/15miplot.pdf",height=4.5,width=6)
p2
dev.off()

pdf("BIP_linear_reg/20miplot.pdf",height=4.5,width=6)
p3
dev.off()
