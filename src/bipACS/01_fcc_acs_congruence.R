# 1/24/20 JG
# FCC-ACS congruence indicator by Census Tract (2014-2018 ACS broadband question VS 2016 FCC subscription rates)
# based on git/dspg19broadband/src/discrepancies/agreement_fcc_acs_intervals.R

library(data.table)
library(dplyr)

fcc_congruent <- readRDS("~/git/ersreport/rivanna_data/working/data_int.Rds") %>%
  dplyr::select(GEOID, acs_within_fcc200, acs_within_fcc10, acs_within_fcc)

fwrite(fcc_congruent, file="~/git/rural_broadband/src/bipACS/fcc_acs_congruent.csv", row.names = F)

