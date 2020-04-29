# 1/24/20 JG
# add FCC subscription in 2011, 2016 by Census Tract
# see https://www.fcc.gov/general/form-477-census-tract-data-internet-access-services

# ADD: 2011 PROVIDER COUNTS BY TRACT (200kbps, 3Mbps)

# 2010 subscription rates use 2000 Census Tracts; use 2011 instead to align across geography
# datasets:
#   hs_tractdata_v2_dec_2011.csv
#   tract_map_dec_2016.csv

# --------------------------------------------------------------------------------------------
# 2011 subscriptions
# --------------------------------------------------------------------------------------------

# Read in
fcc2011 <- fread("~/git/rural_broadband/dat/fcc_subscription/hs_tractdata_v2_dec_2011.csv",
                 colClasses = c(tract_fips="character"))

# Recode
# use rfc_per_1000_hhs instead of pcat_all (200 kbps)
# we can also use total_residential_prov; # residential provders by tract (availability)

fcc2011sub <- fcc2011 %>% transmute(GEOID=tract_fips,
                                    fcc2011_200min = case_when(rfc_per_1000_hhs == 0 ~ 0,
                                                               rfc_per_1000_hhs == 1 ~ 0,
                                                               rfc_per_1000_hhs == 2 ~ 200/1000,
                                                               rfc_per_1000_hhs == 3 ~ 400/1000,
                                                               rfc_per_1000_hhs == 4 ~ 600/1000,
                                                               rfc_per_1000_hhs == 5 ~ 800/1000),
                                    fcc2011_200max = case_when(rfc_per_1000_hhs == 0 ~ 0,
                                                               rfc_per_1000_hhs == 1 ~ 200/1000,
                                                               rfc_per_1000_hhs == 2 ~ 400/1000,
                                                               rfc_per_1000_hhs == 3 ~ 600/1000,
                                                               rfc_per_1000_hhs == 4 ~ 800/1000,
                                                               rfc_per_1000_hhs == 5 ~ 1)
)

fwrite(fcc2011sub,file="~/git/rural_broadband/src/bipACS/fcc2011_subscription.csv")


fcc2011_providers <- fcc2011 %>% transmute(GEOID=tract_fips,
                                           fcc2011_providers_200=total_residential_prov,
                                           fcc2011_providers_3=total_residential_prov_nbp)

fwrite(fcc2011_providers,file="~/git/rural_broadband/src/bipACS/fcc2011_providers.csv")

# also summarize #providers per tract
#total_residential_prov
#Providers of Residential Fixed High-Speed Connections over 200 kbps in at least one direction

#total_residential_prov_nbp
#Providers of Residential Fixed Connections at least 3 Mbps downstream and
#at least 768 kbps upstream

#> table(fcc2011_providers$fcc2011_providers_200)
#0     1     4     5     6     7     8     9    10    11    12    13 
#425 36727 18064 10669  4962  1916   678   214    79    23     8     2 
#> table(fcc2011_providers$fcc2011_providers_3)
#0     1     4     5     6     7    10 
#1470 67309  4167   702    97    21     1 

# --------------------------------------------------------------------------------------------
# 2016 subscriptions
# --------------------------------------------------------------------------------------------

# pcat_all: Residential Fixed High-Speed Connections over 200 kbps in at least one direction per per 1,000 Households
# pcat_10x1: Residential Fixed High-Speed Connections at least 10 Mbps downstream and at least 1 Mbps upstream per 1,000 Households 

# Code  Connections per 1,000 HHs
# 0     Zero
# 1     Zero < x <= 200
# 2     200 < x <=400
# 3     400 < x <=600
# 4     600 < x <=800
# 5     800 < x

# Read in
fcc2016 <- fread("~/git/rural_broadband/dat/fcc_subscription/tract_map_dec_2016.csv",
                 colClasses = c(tractcode="character"))

# Recode only 200kbps
fcc2016 <- fcc2016 %>% transmute(GEOID=tractcode,
                                 fcc2016_200min = case_when(pcat_all == 0 ~ 0,
                                                            pcat_all == 1 ~ 0,
                                                            pcat_all == 2 ~ 200/1000,
                                                            pcat_all == 3 ~ 400/1000,
                                                            pcat_all == 4 ~ 600/1000,
                                                            pcat_all == 5 ~ 800/1000),
                                 fcc2016_200max = case_when(pcat_all == 0 ~ 0,
                                                            pcat_all == 1 ~ 200/1000,
                                                            pcat_all == 2 ~ 400/1000,
                                                            pcat_all == 3 ~ 600/1000,
                                                            pcat_all == 4 ~ 800/1000,
                                                            pcat_all == 5 ~ 1)
)

fwrite(fcc2016,file="~/git/rural_broadband/src/bipACS/fcc2016_subscription.csv")
