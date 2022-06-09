# Pooled regression error bar plots
# Compare to regression with tract effects, and mahalnobis matching results
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
lm_fit <- readRDS("BIP_linear_reg/lm_results.RDS")
lmfixed_fit <- readRDS("BIP_linear_reg/lm_fixedresults.RDS")
match_fit <- fread("BIP_linear_reg/model_match.csv")

yearnames <- c("07-08","09-10","11-12","13-14","15+")

lm_10 <- as.data.frame( lm_fit[[1]][19:23,c(2,6,7)] )
rownames(lm_10) <- yearnames
names(lm_10) <- c("est","lower","upper")
lm_10$years <- yearnames; lm_10$source <- "Linear Model"
lm_15 <- as.data.frame( lm_fit[[2]][19:23,c(2,6,7)] )
rownames(lm_15) <- yearnames
names(lm_15) <- c("est","lower","upper")
lm_15$years <- yearnames; lm_15$source <- "Linear Model"
lm_20 <- as.data.frame( lm_fit[[3]][19:23,c(2,6,7)] )
rownames(lm_20) <- yearnames
names(lm_20) <- c("est","lower","upper")
lm_20$years <- yearnames; lm_20$source <- "Linear Model"


lmfixed_10 <- as.data.frame( lmfixed_fit[[1]][19:23,c(2,6,7)] )
rownames(lmfixed_10) <- yearnames
names(lmfixed_10) <- c("est","lower","upper")
lmfixed_10$years <- yearnames; lmfixed_10$source <- "LM fixed tracts"
lmfixed_15 <- as.data.frame( lmfixed_fit[[2]][19:23,c(2,6,7)] )
rownames(lmfixed_15) <- yearnames
names(lmfixed_15) <- c("est","lower","upper")
lmfixed_15$years <- yearnames; lmfixed_15$source <- "LM fixed tracts"
lmfixed_20 <- as.data.frame( lmfixed_fit[[3]][19:23,c(2,6,7)] )
rownames(lmfixed_20) <- yearnames
names(lmfixed_20) <- c("est","lower","upper")
lmfixed_20$years <- yearnames; lmfixed_20$source <- "LM fixed tracts"

match_10 <- match_fit[2:6,]
match_10$lower <- match_10$est - 1.96*match_10$se
match_10$upper <- match_10$est + 1.96*match_10$se
match_10 <- match_10[-1,] %>% dplyr::select("est","lower","upper")
rownames(match_10) <- yearnames
match_10$years <- yearnames; match_10$source <- "Matching"
match_15 <- match_fit[8:12,]
match_15$lower <- match_15$est - 1.96*match_15$se
match_15$upper <- match_15$est + 1.96*match_15$se
match_15 <- match_15[-1,] %>% dplyr::select("est","lower","upper")
rownames(match_15) <- yearnames
match_15$years <- yearnames; match_15$source <- "Matching"
match_20 <- match_fit[14:18,]
match_20$lower <- match_20$est - 1.96*match_20$se
match_20$upper <- match_20$est + 1.96*match_20$se
match_20 <- match_20[-1,] %>% dplyr::select("est","lower","upper")
rownames(match_20) <- yearnames
match_20$years <- yearnames; match_20$source <- "Matching"

plotmodels10 <- rbind(lm_10,lmfixed_10,match_10)
plotmodels10$est <- exp( as.numeric(plotmodels10$est) )
plotmodels10$lower <- exp( as.numeric(plotmodels10$lower) )
plotmodels10$upper <- exp( as.numeric(plotmodels10$upper) )

plotmodels15 <- rbind(lm_15,lmfixed_15,match_15)
plotmodels15$est <- exp( as.numeric(plotmodels15$est) )
plotmodels15$lower <- exp( as.numeric(plotmodels15$lower) )
plotmodels15$upper <- exp( as.numeric(plotmodels15$upper) )

plotmodels20 <- rbind(lm_20,lmfixed_20,match_20)
plotmodels20$est <- exp( as.numeric(plotmodels20$est) )
plotmodels20$lower <- exp( as.numeric(plotmodels20$lower) )
plotmodels20$upper <- exp( as.numeric(plotmodels20$upper) )

# create plots
p1 <- ggplot(plotmodels10, aes(x=years)) + 
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

p2 <- ggplot(plotmodels15, aes(x=years)) + 
  geom_point(aes(y = est, color = source),
             position=position_dodge(0.2)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, color = source), width=.2,
                position=position_dodge(0.2)) +
  ylim(0.80, 1.20) +
  labs(title = "Interactions between BIP and 2-year Indicators",
       subtitle = "Comparing sales 5-15 miles outside the BIP",
       x="BIP x 2-Year Indicators", 
       y = "Estimate", color = "Model")+
  theme_bw() + theme(legend.position="bottom")

p3 <- ggplot(plotmodels20, aes(x=years)) + 
  geom_point(aes(y = est, color = source),
             position=position_dodge(0.2)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, color = source), width=.2,
                position=position_dodge(0.2)) +
  ylim(0.80, 1.20) +
  labs(title = "Interactions between BIP and 2-year Indicators",
       subtitle = "Comparing sales 10-20 miles outside the BIP",
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
