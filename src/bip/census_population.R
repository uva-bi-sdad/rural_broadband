library(tidycensus)
library(purrr)

# Un-comment below and set your API key
# census_api_key("YOUR KEY GOES HERE")
census_api_key("ed8973afe8a22958b77ffdb8547c93c3cae1d2fc") # Teja's key

us <- unique(fips_codes$state)[1:51]

totpop_2010_tract <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B01003_001", 
          state = x, year = 2010)
})

str(totpop_2010_tract)

saveRDS(totpop_2010_tract, "~/git/rural_broadband/data/working/Census Shapefiles/census_totalpop_usa_tracts_2010.RDS")

# never ran this
# totpop_2010_block <- map_df(us, function(x) {
#   get_acs(geography = "block", variables = "B01003_001", 
#           state = x, year = 2010)
# })
# 
# str(totpop_2010_block)
# 
# saveRDS(totpop_2010_tract, "~/git/rural_broadband/data/working/Census Shapefiles/census_totalpop_usa_blocks_2010.RDS")
