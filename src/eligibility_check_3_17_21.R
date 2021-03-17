# check eligibility files from Devika
library(dplyr)
library(data.table)

# fixing RC_18_elig_fund_INSIDE, RC_18_elig_fund_BOUND
# adding CC_18_elig_fund_ANY, CC_18_elig_fund_INSIDE, CC_18_elig_fund_BOUND

eligible <- fread("~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/eligibility_summary_percentages/USA_2014_18_final_CCRC_eligibility_fundedareas_df_15DEC2020.csv")

eligible2 <- eligible %>% transmute(
  FULL_GEOID,
  pop_2010,
  CC_ELIG_14,
  CC_ELIG_18,
  CC_14_elig_fund_ANY = case_when( !is.na(CC_14_RUSID_intersect) & CC_ELIG_14 == "eligible" ~ "eligible, funded",
                                   is.na(CC_14_RUSID_intersect) & CC_ELIG_14 == "eligible" ~ "eligible, no funds",
                                   !is.na(CC_14_RUSID_intersect) & CC_ELIG_14 == "ineligible" ~ "ineligible, funded",
                                   is.na(CC_14_RUSID_intersect) & CC_ELIG_14 == "ineligible" ~ "ineligible, no funds"),
  CC_14_elig_fund_INSIDE = case_when( !is.na(CC_14_RUSID_inside) & CC_ELIG_14 == "eligible" ~ "eligible, funded",
                                   is.na(CC_14_RUSID_inside) & CC_ELIG_14 == "eligible" ~ "eligible, no funds",
                                   !is.na(CC_14_RUSID_inside) & CC_ELIG_14 == "ineligible" ~ "ineligible, funded",
                                   is.na(CC_14_RUSID_inside) & CC_ELIG_14 == "ineligible" ~ "ineligible, no funds"),
  CC_14_elig_fund_BOUND = case_when( !is.na(CC_14_RUSID_boundary) & CC_ELIG_14 == "eligible" ~ "eligible, funded",
                                   is.na(CC_14_RUSID_boundary) & CC_ELIG_14 == "eligible" ~ "eligible, no funds",
                                   !is.na(CC_14_RUSID_boundary) & CC_ELIG_14 == "ineligible" ~ "ineligible, funded",
                                   is.na(CC_14_RUSID_boundary) & CC_ELIG_14 == "ineligible" ~ "ineligible, no funds"),
  CC_14_RUSID_intersect,
  CC_14_RUSID_inside,
  CC_14_RUSID_boundary,
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
                                     is.na(CC_18_RUSID_boundary) & CC_ELIG_18 == "ineligible" ~ "ineligible, no funds"),
  CC_18_RUSID_intersect,
  CC_18_RUSID_inside,
  CC_18_RUSID_boundary,
  rc_elig_18,
  RC_18_elig_fund_ANY = case_when( !is.na(RC_18_RUSID_intersect) & rc_elig_18 == "eligible" ~ "eligible, funded",
                                   is.na(RC_18_RUSID_intersect) & rc_elig_18 == "eligible" ~ "eligible, no funds",
                                   !is.na(RC_18_RUSID_intersect) & rc_elig_18 == "ineligible" ~ "ineligible, funded",
                                   is.na(RC_18_RUSID_intersect) & rc_elig_18 == "ineligible" ~ "ineligible, no funds"),
  RC_18_elig_fund_INSIDE = case_when( !is.na(RC_18_RUSID_inside) & rc_elig_18 == "eligible" ~ "eligible, funded",
                                      is.na(RC_18_RUSID_inside) & rc_elig_18 == "eligible" ~ "eligible, no funds",
                                      !is.na(RC_18_RUSID_inside) & rc_elig_18 == "ineligible" ~ "ineligible, funded",
                                      is.na(RC_18_RUSID_inside) & rc_elig_18 == "ineligible" ~ "ineligible, no funds"),
  RC_18_elig_fund_BOUND = case_when( !is.na(RC_18_RUSID_boundary) & rc_elig_18 == "eligible" ~ "eligible, funded",
                                     is.na(RC_18_RUSID_boundary) & rc_elig_18 == "eligible" ~ "eligible, no funds",
                                     !is.na(RC_18_RUSID_boundary) & rc_elig_18 == "ineligible" ~ "ineligible, funded",
                                     is.na(RC_18_RUSID_boundary) & rc_elig_18 == "ineligible" ~ "ineligible, no funds"),
  RC_18_RUSID_intersect,
  RC_18_RUSID_inside,
  RC_18_RUSID_boundary
)

fwrite(eligible2, "~/../../../../project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/working/USA_2014_18_final_CCRC_eligibility_fundedareas_df_17MAR2021.csv")


# compare with previous estimates; all agree except for incorrect RC_18_elig_fund_INSIDE, RC_18_elig_fund_BOUND
table(eligible2$CC_18_elig_fund_ANY, useNA="always")
table(eligible2$CC_18_elig_fund_INSIDE, useNA="always")
table(eligible2$CC_18_elig_fund_BOUND, useNA="always")

table(eligible$CC_14_elig_fund_ANY, useNA="always")
table(eligible2$CC_14_elig_fund_ANY, useNA="always")

table(eligible$CC_14_elig_fund_INSIDE, useNA="always")
table(eligible2$CC_14_elig_fund_INSIDE, useNA="always")

table(eligible$CC_14_elig_fund_BOUND, useNA="always")
table(eligible2$CC_14_elig_fund_BOUND, useNA="always")

table(eligible$RC_14_elig_fund_ANY, useNA="always")
table(eligible2$RC_18_elig_fund_ANY, useNA="always")

table(eligible$RC_14_elig_fund_INSIDE, useNA="always")
table(eligible2$RC_18_elig_fund_INSIDE, useNA="always")

table(eligible$RC_14_elig_fund_BOUND, useNA="always")
table(eligible2$RC_18_elig_fund_BOUND, useNA="always")


# find a block with both INSIDE and BOUND
duplicates <- eligible2[eligible2$CC_14_elig_fund_INSIDE=="eligible, funded" & eligible2$CC_14_elig_fund_BOUND=="eligible, funded",]
# the duplicates occur because the block has an interior intersection with one funding area, and a boundary intersection with another funding area
# FULL_ GEOID 380039680002293
# CC_14_RUSID_inside: ND1401-A23
# CC_14_RUSID_boundary: ND1401-B23


# check that we are using the correct speed criteria for eligibility; 5 Mbps download + upload


