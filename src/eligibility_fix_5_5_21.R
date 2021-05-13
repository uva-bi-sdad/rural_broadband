# fix eligibility files 5/5/21
# include the CC project service areas for projects approved in 2013 and 2014 in the 2018 CC project data

library(dplyr)
library(data.table)

eligible <- fread("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/USA_2014_18_final_CCRC_eligibility_fundedareas_df_17MAR2021.csv")

eligible[eligible==""] <- NA

# add the 2013 and 2014 eligible areas to 2018 CC eligibility
eligible2 <- eligible %>% mutate(
  CC_18_RUSID_intersect = case_when( !is.na(CC_14_RUSID_intersect) ~ CC_14_RUSID_intersect,
                                     is.na(CC_14_RUSID_intersect) ~ CC_18_RUSID_intersect),
  CC_18_RUSID_inside = case_when( !is.na(CC_14_RUSID_inside) ~ CC_14_RUSID_inside,
                                     is.na(CC_14_RUSID_inside) ~ CC_18_RUSID_inside),
  CC_18_RUSID_boundary = case_when( !is.na(CC_14_RUSID_boundary) ~ CC_14_RUSID_boundary,
                                     is.na(CC_14_RUSID_boundary) ~ CC_18_RUSID_boundary)
)

#table(is.na(eligible2$CC_18_RUSID_intersect))
#table(eligible2$CC_ELIG_18)

eligible3 <- eligible2 %>% mutate(
  CC_18_elig_fund_ANY = case_when( !is.na(CC_18_RUSID_intersect) & CC_ELIG_18 == "eligible" ~ "eligible, funded",
                                   is.na(CC_18_RUSID_intersect) & CC_ELIG_18 == "eligible" ~ "eligible, no funds",
                                   !is.na(CC_18_RUSID_intersect) & CC_ELIG_18 == "ineligible" ~ "ineligible, funded",
                                   is.na(CC_18_RUSID_intersect) & CC_ELIG_18 == "ineligible" ~ "ineligible, no funds"),
  CC_18_elig_fund_INSIDE = case_when( !is.na(CC_18_RUSID_inside) & CC_ELIG_18 == "eligible" ~ "eligible, funded",
                                      is.na(CC_18_RUSID_inside) & CC_ELIG_18 == "eligible" ~ "eligible, no funds",
                                      !is.na(CC_18_RUSID_inside) & CC_ELIG_18 == "ineligible" ~ "ineligible, funded",
                                      is.na(CC_18_RUSID_inside) & CC_ELIG_18 == "ineligible" ~ "ineligible, no funds"),
  CC_18_elig_fund_BOUND = case_when( !is.na(CC_18_RUSID_boundary) & CC_ELIG_18 == "eligible" ~ "eligible, funded",
                                     is.na(CC_18_RUSID_boundary) & CC_ELIG_18 == "eligible" ~ "eligible, no funds",
                                     !is.na(CC_18_RUSID_boundary) & CC_ELIG_18 == "ineligible" ~ "ineligible, funded",
                                     is.na(CC_18_RUSID_boundary) & CC_ELIG_18 == "ineligible" ~ "ineligible, no funds")  
)

# quality check: make sure new results make sense; more funded areas for 2018
table(eligible$CC_18_elig_fund_ANY)
#eligible, funded   eligible, no funds   ineligible, funded ineligible, no funds 
#2500              2274418                 2416              8804978
table(eligible3$CC_18_elig_fund_ANY)
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


