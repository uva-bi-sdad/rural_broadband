

## DOWNLOAD 2014 DATA

# url <- "http://transition.fcc.gov/form477/BroadbandData/Fixed/Dec14/Version%203/US-Fixed-without-Satellite-Dec2014.zip"
# temp <- tempfile()
# download.file(url,temp)
# unlink(temp)

# grab temp pathname and go intoto terminal 
# ran this to see name of file in zip 
# unzip -l /tmp/RtmpwK6I3G/file33274b60567b
# ran this to unzip from terminal:
# unzip /tmp/RtmpwK6I3G/file33274b60567b -d /project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/fcc_availability/

fcc_2014 <- read.csv("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/fcc_availability/fbd_us_without_satellite_dec2014_v3.csv")

#threshold of 10/1
# fcc_2014_block_max <- fcc_2014 %>%
#   group_by(StateAbbr, BlockCode) %>%
#   summarise(MaxAdDown_ = max(MaxAdDown),
#             MaxAdUp_ = max(MaxAdUp)) %>%
#   mutate(speed_elig_down = ifelse(MaxAdDown_ >= 10, 0, 1),
#          speed_elig_up = ifelse(MaxAdUp_ >= 1, 0, 1),
#          speed_inelig = ifelse(MaxAdDown_ >= 10 & MaxAdUp_ >= 1, 1, 0),
#          speed_elig = ifelse(MaxAdDown_ >= 10 & MaxAdUp_ >= 1, 0, 1))

# #threshold of 4/1
# fcc_2014_block_max <- fcc_2014 %>%
#   group_by(StateAbbr, BlockCode) %>%
#   summarise(MaxAdDown_ = max(MaxAdDown),
#             MaxAdUp_ = max(MaxAdUp)) %>%
#   mutate(speed_elig_down = ifelse(MaxAdDown_ >= 4, 0, 1),
#          speed_elig_up = ifelse(MaxAdUp_ >= 1, 0, 1),
#          speed_inelig = ifelse(MaxAdDown_ >= 4 & MaxAdUp_ >= 1, 1, 0),
#          speed_elig = ifelse(MaxAdDown_ >= 4 & MaxAdUp_ >= 1, 0, 1))

# #threshold of 4/768
# fcc_2014_block_max_ <- fcc_2014 %>%
#   group_by(StateAbbr, BlockCode) %>%
#   summarise(MaxAdDown_ = max(MaxAdDown),
#             MaxAdUp_ = max(MaxAdUp)) %>%
#   mutate(speed_elig_down = ifelse(MaxAdDown_ >= 4, 0, 1),
#          speed_elig_up = ifelse(MaxAdUp_ >= 0.768, 0, 1),
#          speed_inelig = ifelse(MaxAdDown_ >= 4 & MaxAdUp_ >= 0.768, 1, 0),
#          speed_elig = ifelse(MaxAdDown_ >= 4 & MaxAdUp_ >= 0.768, 0, 1))

# #threshold of 3 mbps (download + upload)
fcc_2014_block_max_ <- fcc_2014 %>%
  group_by(StateAbbr, BlockCode) %>%
  summarise(MaxAdDown_ = max(MaxAdDown),
            MaxAdUp_ = max(MaxAdUp)) %>%
  mutate(total_speed = MaxAdDown_ + MaxAdUp_,
         speed_elig_down = NA, #ifelse(MaxAdDown_ >= 4, 0, 1),
         speed_elig_up = NA, #ifelse(MaxAdUp_ >= 0.768, 0, 1),
         speed_inelig = ifelse(total_speed < 3, 1, 0),
         speed_elig = ifelse(total_speed >= 3, 0, 1))


#threshold of 768/200 kbps ??


# saveRDS(fcc_2014_block_max, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/fcc_availability/fcc_2014_block_maxspdelig.RDS")
# saveRDS(fcc_2014_block_max, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/fcc_availability/fcc_2014_block_maxspdelig_threshold_4_1.RDS")
# saveRDS(fcc_2014_block_max_, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/fcc_availability/fcc_2014_block_maxspdelig_threshold_4_768.RDS")
saveRDS(fcc_2014_block_max_, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/fcc_availability/fcc_2014_block_maxspdelig_threshold_total3.RDS")


## DOWNLOAD 2018 DATA

# url <- "https://www.fcc.gov/form477/BroadbandData/Fixed/Dec18/Version%202/US-Fixed-without-Satellite-Dec2018.zip"
# temp <- tempfile()
# download.file(url,temp)
# unlink(temp)

# grab temp pathname and go intoto terminal 
# ran this to see name of file in zip 
# unzip -l /tmp/RtmpSdiLsM/file360be78486be5
# ran this to unzip from terminal:
# unzip /tmp/RtmpSdiLsM/file360be78486be5 -d /project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/fcc_availability/

## MANIPULATE 2018 DATA

fcc_2018 <- read.csv("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/fcc_availability/fbd_us_without_satellite_dec2018_v2.csv")

fcc_2018_block_max <- fcc_2018 %>%
  group_by(StateAbbr, BlockCode) %>%
  summarise(MaxAdDown_ = max(MaxAdDown),
            MaxAdUp_ = max(MaxAdUp)) %>%
  mutate(speed_elig_down = ifelse(MaxAdDown_ >= 10, 0, 1),
         speed_elig_up = ifelse(MaxAdUp_ >= 1, 0, 1),
         speed_inelig = ifelse(MaxAdDown_ >= 10 & MaxAdUp_ >= 1, 1, 0),
         speed_elig = ifelse(MaxAdDown_ >= 10 & MaxAdUp_ >= 1, 0, 1))

# saveRDS(fcc_2018_block_max, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/fcc_availability/fcc_2018_block_maxspdelig.RDS")