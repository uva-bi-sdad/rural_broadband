# fix eligibility files 5/5/21
# include the CC project service areas for projects approved in 2013 and 2014 in the 2018 CC project data

library(dplyr)
library(data.table)

# --------------------------------------
# answer John's question about IN_CC
eligible <- fread("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/USA_2014_18_final_CCRC_eligibility_fundedareas_df_06MAY2021.csv",
                      colClasses=c(GEOID10="character"))

eligible_old <- fread("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/USA_2014_18_final_CCRC_eligibility_fundedareas_df_17MAR2021.csv",
                  colClasses=c(GEOID10="character"))

# IN_CC for each tract is created by taking a population weighted mean of !is.na(CC_18_RUSID_intersect) at the Census block level
# IN_CC is based on the 8,203 funded areas in CC_18_analyze
table(eligible$CC_18_analyze)
# > table(eligible$CC_18_analyze)
# eligible, funded   eligible, unfunded   ineligible, funded ineligible, unfunded         NA, unfunded 
# 3416              2273057                 4787              8802607                  445 
eligible[eligible==""] <- NA
table(!is.na(eligible$CC_18_RUSID_intersect))
table(eligible_old$CC_18_elig_fund_ANY)
#eligible, funded   eligible, no funds   ineligible, funded ineligible, no funds 
#2500              2274418                 2416              8804978
eligible_old[eligible_old==""] <- NA
table(!is.na(eligible_old$CC_18_RUSID_intersect))

# this variable *should* have changed from the previous version to the current one

# --------------------------------------


#eligible <- fread("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/USA_2014_18_final_CCRC_eligibility_fundedareas_df_17MAR2021.csv",
#                  colClasses=c(GEOID10="character"))
eligible <- readRDS("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/USA_2014_18_final_CCRC_eligibility_fundedareas_df_14MAR2021.RDS")

table(eligible$CC_14_analyze)
table(is.na(eligible$CC_14_RUSID_intersect), eligible$CC_ELIG_2014)

table(eligible$CC_18_analyze)
table(is.na(eligible$CC_18_RUSID_intersect), eligible$CC_ELIG_2018)

table(eligible$RC_18_analyze)
table(is.na(eligible$RC_18_RUSID_intersect), eligible$RC_ELIG_2018)



# remake the "analyze" columns
eligible2 <- eligible %>% mutate(
  CC_14_analyze = case_when( !is.na(CC_14_RUSID_intersect) & CC_ELIG_2014 == "eligible" ~ "eligible, funded",
                                   is.na(CC_14_RUSID_intersect) & CC_ELIG_2014 == "eligible" ~ "eligible, unfunded",
                                   !is.na(CC_14_RUSID_intersect) & CC_ELIG_2014 == "ineligible" ~ "ineligible, funded",
                                   is.na(CC_14_RUSID_intersect) & CC_ELIG_2014 == "ineligible" ~ "ineligible, unfunded"),
  CC_18_analyze = case_when( !is.na(CC_18_RUSID_intersect) & CC_ELIG_2018 == "eligible" ~ "eligible, funded",
                             is.na(CC_18_RUSID_intersect) & CC_ELIG_2018 == "eligible" ~ "eligible, unfunded",
                             !is.na(CC_18_RUSID_intersect) & CC_ELIG_2018 == "ineligible" ~ "ineligible, funded",
                             is.na(CC_18_RUSID_intersect) & CC_ELIG_2018 == "ineligible" ~ "ineligible, unfunded"),
  RC_18_analyze = case_when( !is.na(RC_18_RUSID_intersect) & RC_ELIG_2018 == "eligible" ~ "eligible, funded",
                             is.na(RC_18_RUSID_intersect) & RC_ELIG_2018 == "eligible" ~ "eligible, unfunded",
                             !is.na(RC_18_RUSID_intersect) & RC_ELIG_2018 == "ineligible" ~ "ineligible, funded",
                             is.na(RC_18_RUSID_intersect) & RC_ELIG_2018 == "ineligible" ~ "ineligible, unfunded")
)



table(eligible2$CC_14_analyze)
table(is.na(eligible2$CC_14_RUSID_intersect), eligible2$CC_ELIG_2014)

table(eligible2$CC_18_analyze)
table(is.na(eligible2$CC_18_RUSID_intersect), eligible2$CC_ELIG_2018)

table(eligible2$RC_18_analyze)
table(is.na(eligible2$RC_18_RUSID_intersect), eligible2$RC_ELIG_2018)

fwrite(eligible2, "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/USA_2014_18_final_CCRC_eligibility_fundedareas_df_06MAY2021.csv")



# drop FULL_GEOID and move GEOID10 to the first column
#eligible <- eligible %>% select(-FULL_GEOID) %>% relocate(GEOID10)
#eligible[eligible==""] <- NA

# add the 2013 and 2014 eligible areas to 2018 CC eligibility
#eligible2 <- eligible %>% mutate(
#  CC_18_RUSID_intersect = case_when( !is.na(CC_14_RUSID_intersect) ~ CC_14_RUSID_intersect,
#                                     is.na(CC_14_RUSID_intersect) ~ CC_18_RUSID_intersect),
#  CC_18_RUSID_inside = case_when( !is.na(CC_14_RUSID_inside) ~ CC_14_RUSID_inside,
#                                     is.na(CC_14_RUSID_inside) ~ CC_18_RUSID_inside),
#  CC_18_RUSID_boundary = case_when( !is.na(CC_14_RUSID_boundary) ~ CC_14_RUSID_boundary,
#                                     is.na(CC_14_RUSID_boundary) ~ CC_18_RUSID_boundary)
#)
#table(is.na(eligible$CC_18_RUSID_intersect))
#table(is.na(eligible2$CC_18_RUSID_intersect))
#table(eligible2$CC_ELIG_2018)

# quality check: make sure new results make sense; more funded areas for 2018
table(eligible$CC_18_analyze)
#eligible, funded   eligible, unfunded   ineligible, funded ineligible, unfunded 
#3909              2546692                 4294              8527288 
#NA, unfunded 
#2129 
table(eligible3$CC_18_elig_fund_ANY)
#eligible, funded   eligible, no funds   ineligible, funded ineligible, no funds 
#3416              2273057                 4787              8802607 

#eligible, funded   eligible, no funds   ineligible, funded ineligible, no funds 
#3416              2273502                 4787              8802607
table(eligible$CC_18_elig_fund_INSIDE)
#eligible, funded   eligible, no funds   ineligible, funded ineligible, no funds 
#1534              2275384                 1181              8806213
table(eligible3$CC_18_elig_fund_INSIDE)
#eligible, funded   eligible, no funds   ineligible, funded ineligible, no funds 
#2157              2274761                 2730              8804664
table(eligible$CC_18_elig_fund_BOUND)
#eligible, funded   eligible, no funds   ineligible, funded ineligible, no funds 
#968              2275950                 1236              8806158 
table(eligible3$CC_18_elig_fund_BOUND)
#eligible, funded   eligible, no funds   ineligible, funded ineligible, no funds 
#1261              2275657                 2067              8805327

# write the fixed block file
fwrite(eligible3, "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/USA_2014_18_final_CCRC_eligibility_fundedareas_df_06MAY2021.csv")


