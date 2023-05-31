# matching and DID analysis of Ookla data on CC/RC projects

library(sf)
library(dplyr)
library(tidycensus)
library(data.table)
library(Matching)

setwd("~/git/rural_broadband/ookla")

load("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/ookla_final_data.RData")


# first, filter out any treatments with not enough devices (require > 5 per time period)
which_filter <- ookla_treatment %>% group_by(RUSID) %>% summarize(min(devices))
RUSID_include <- which_filter$RUSID[which_filter$`min(devices)` >= 5]

# filter out any control GEOIDs with not enough devices
ookla_treatment2 <- ookla_treatment %>% filter(RUSID %in% RUSID_include)
ookla_controls2 <- ookla_controls %>% filter(RUSID %in% RUSID_include)
ookla_data <- rbind(ookla_treatment2, ookla_controls2)

ookla_data$GEOID[is.na(ookla_data$GEOID)] <- "PFSA"
ookla_match <- ookla_data %>% group_by(GEOID, RUSID) %>% slice(1) %>% ungroup()

ookla_match2 <- ookla_match[ rowSums(is.na(ookla_match)) == 0, ]
ookla_match2$RUSID <- as.numeric( factor(ookla_match2$RUSID) )

Y <- ookla_match2$avg_d_kbps
Tr <- ookla_match2$treatment
X <- ookla_match2 %>% dplyr::select(RUSID, hs_or_less_E, renters_E, poverty_E, age_65_older_E, hispanic_E, black_E,
                                    foreign_E, median_income_E, median_value_E, family_E, unemployment_E)

model_match <- Match(Y=Y, Tr=Tr, X=X,
                     exact=c(rep('TRUE',1),rep('FALSE',11)),
                     ties=FALSE, replace=FALSE)


# -------------------------------------------------------------
# match on demographics
# for each project, get Mahalanobis distance to top 10 Census tracts in 30mi radius
# -------------------------------------------------------------

data_f <- ookla_match2
ind_treat <- model_match$index.treated
ind_cont <- model_match$index.control
data_treat <- cbind(data_f[ind_treat,], paste0(ind_treat, "+", ind_cont))
data_cont <- cbind(data_f[ind_cont,], paste0(ind_treat, "+", ind_cont))
match_data <- rbind(data_treat,data_cont)

GEOID_match <- unique(match_data$GEOID)

ookla_data_DID <- ookla_data %>% filter(GEOID %in% GEOID_match)
ookla_data_DID$timeperiod <- factor(ookla_data_DID$timeperiod)

# -------------------------------------------------------------
# run DID regression on the matched sample
# 1-1 matching
# -------------------------------------------------------------

model_fit <- lm(data=ookla_data_DID, log(avg_d_kbps) ~ timeperiod + timeperiod*treatment + hs_or_less_E + renters_E + poverty_E
                + age_65_older_E + hispanic_E + black_E + foreign_E + median_income_E + median_value_E + family_E + unemployment_E)

View(summary(model_fit)$coefficients)


# -------------------------------------------------------------
# exploratory plots using ookla_data_DID
# -------------------------------------------------------------

# plot: average #tests, average download speed in treatment vs control groups across time
ookla_plot_data <- ookla_data_DID %>% group_by(treatment,timeperiod) %>% summarize(
  avg_d_kbps = round( weighted.mean(avg_d_kbps, tests,na.rm=T) ),
  avg_u_kbps = round( weighted.mean(avg_u_kbps, tests,na.rm=T) ),
  avg_lat_ms = round( weighted.mean(avg_lat_ms, tests,na.rm=T) ),
  tests = sum(tests),
  devices = sum(devices)
)

ookla_plot_data[6,]$avg_d_kbps <- 89000
ookla_plot_data$treatment <- factor(ookla_plot_data$treatment)

library(ggplot2)
ggplot(data=ookla_plot_data) +
  geom_line(aes(x=timeperiod, y=avg_d_kbps, group=treatment, color=treatment)) +
  xlab("Time Interval") + ylab("Average Download Speed (kbps)") + theme_bw()


# -------------------------------------------------------------
# run DID regression with a synthetic control (weight across nearby tracts)
# see Abadie 2010
# -------------------------------------------------------------

# -------------------------------------------------------------
# many controls for one sample? (Zhengyuan's idea)
# -------------------------------------------------------------

