---
title: "Untitled"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
```


```{r cars}
# usa_blocks_speed_rural_projects_final_2014 <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2014_final_eligibility_sf_24NOV2020v3.RDS")
# 
# map_14_df <- usa_blocks_speed_rural_projects_final_2014 %>% data.frame()
# 
# map_14_df$geometry <- NULL
# 
# saveRDS(map_14_df, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2014_final_eligibility_dfnogeo_01DEC2020v2.RDS")

map_14_df <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2014_final_eligibility_dfnogeo_01DEC2020v2.RDS")
# 
# rm(usa_blocks_speed_rural_projects_final_2014)

# usa_blocks_speed_rural_projects_final_2018 <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2018_final_eligibility_sf_24NOV2020v3.RDS")
#
#
# map_18_df <- usa_blocks_speed_rural_projects_final_2018 %>% data.frame()
#
map_18_df$geometry <- NULL

# saveRDS(map_18_df, "/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2018_final_eligibility_dfnogeo_01DEC2020v2.RDS")

map_18_df <- readRDS("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/USA_2018_final_eligibility_dfnogeo_01DEC2020v2.RDS")


rm(usa_blocks_speed_rural_projects_final_2018)

```


```{r}
usa_blocks_speed_rural_projects_final_2014 %>% head()
map_14_df %>% head() #%>% filter(eligibility_final == "eligible")
class(map_14_df)


summary_natl <- data.frame("Year" = c(2014, 2018), 
                           "Slice" = c("USA Blocks", "USA Blocks"), 
                           "ELIG_CT" = c(map_14_df %>% filter(eligibility_final == "eligible") %>% nrow(),
                                         map_18_df %>% filter(eligibility_final == "eligible") %>% nrow()
                                         )) %>%
  mutate("PERC_ELIG_NATL" = ELIG_CT/(nrow(map_14_df) - ELIG_CT))

summary_2014_state <- left_join(map_14_df %>% filter(eligibility_final == "eligible") %>% count(state),
                                map_14_df %>% count(state), 
                                by = "state") %>% 
  transmute("Year" = 2014, "Slice" = "USA States", State = state, ELIG_CT = `n.x`, PERC_ELIG_STATE = ELIG_CT/`n.y`)

summary_2018_state <- left_join(map_18_df %>% filter(eligibility_final == "eligible") %>% count(state),
                                map_18_df %>% count(state), 
                                by = "state") %>% 
  transmute("Year" = 2018, "Slice" = "USA States", State = state, ELIG_CT = `n.x`, PERC_ELIG_STATE = ELIG_CT/`n.y`)

summary_natl_states <- rbind(summary_2014_state, summary_2018_state)

elig_states <- summary_natl_states %>% 
  reshape2::dcast(Slice + State ~ Year, value.var = "ELIG_CT")

colnames(elig_states) <- c("Slice", "State", "ELIGCT_14", "ELIGCT_18")

perc_elig_states <- summary_natl_states %>% 
  reshape2::dcast(Slice + State ~ Year, value.var = "PERC_ELIG_STATE")

colnames(perc_elig_states) <- c("Slice", "State", "PERCELIG_14", "PERCELIG_18")

state_diff <-left_join(elig_states, perc_elig_states, by = c("Slice", "State")) %>% 
  mutate(ELIGCT_DIFF = ELIGCT_18-ELIGCT_14, PERC_DIFF = PERCELIG_18 - PERCELIG_14)


```

# 2014 map

```{r}
states <- unique(map_14_df$StateAbbr)
states <- unique(map_14_df$STATEFP10)
county_map <- tigris::counties(state = states)

block_pop_2010 <- read.csv("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/population_by_census_block_2010 (2).csv")
block_pop_2010$GEOID <- ifelse(nchar(block_pop_2010$GEOID) < 15, paste0(0, block_pop_2010$GEOID), block_pop_2010$GEOID)

map_14_df_pop <- map_14_df %>% left_join(block_pop_2010 %>% transmute(GEOID  = as.factor(GEOID), pop = value), by = c("GEOID10" = "GEOID"))

rc_percelig_pop <- map_14_df_pop %>% 
  group_by(CTY = paste0(STATEFP10, COUNTYFP10), eligibility_final) %>% 
  summarise(sum_pop = sum(abs(pop)))

rc_percelig_pop2 <- rc_percelig_pop %>% 
  reshape2::dcast(CTY~eligibility_final, value.var = "sum_pop") %>% 
  mutate(eligible = ifelse(is.na(eligible), 0, eligible),
                            ineligible = ifelse(is.na(ineligible), 0, ineligible),
                            PERC_ELIG_14 = eligible / (eligible + ineligible))

county_map__14elig <- county_map %>% left_join(rc_percelig_pop2, by = c("GEOID" = "CTY"))


```

ReConnect Eligibility 2014

```{r}
library(leaflet)

qpal <- colorQuantile("Blues", county_map__14elig$PERC_ELIG_14, n = 5)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = county_map__14elig, 
              color = ~qpal(PERC_ELIG_14), stroke = FALSE, fillOpacity = 1) %>%
  addLegend("bottomright", pal = qpal, values = county_map__14elig$PERC_ELIG_14,
    title = "% Eligibility",
    labFormat = labelFormat(suffix = "%"),
    opacity = 1
  )


```





```{r}
# map_14_df_pop <- map_14_df %>% left_join(block_pop_2010 %>% transmute(GEOID  = as.factor(GEOID), pop = value), by = c("GEOID10" = "GEOID"))

cc_percelig_pop <- map_14_df_pop %>% 
  mutate(cc_elig = ifelse(disqual1 == "eligible" & disqual2 == "eligible", "eligible", "ineligible")) %>%
  group_by(CTY = paste0(STATEFP10, COUNTYFP10), cc_elig) %>% 
  summarise(sum_pop = sum(abs(pop))) 

cc_percelig_pop2 <- cc_percelig_pop %>% 
  reshape2::dcast(CTY~cc_elig, value.var = "sum_pop") %>% 
  mutate(eligible = ifelse(is.na(eligible), 0, eligible),
                            ineligible = ifelse(is.na(ineligible), 0, ineligible),
                            PERC_ELIG_14 = eligible / (eligible + ineligible))

county_map__14elig <- county_map %>% left_join(cc_percelig_pop2, by = c("GEOID" = "CTY"))


```

## CC 14 ELig 

```{r}
qpal <- colorQuantile("Blues", county_map__14elig$PERC_ELIG_14, n = 5)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = county_map__14elig, 
              color = ~qpal(PERC_ELIG_14), stroke = FALSE, fillOpacity = 1) %>%
  addLegend("bottomright", pal = qpal, values = county_map__14elig$PERC_ELIG_14,
    title = "% Eligibility",
    labFormat = labelFormat(suffix = "%"),
    opacity = 1
  )
```


# 2018 map

```{r}
# states <- unique(map_18_df$StateAbbr)
states <- unique(map_18_df$STATEFP10)
county_map <- tigris::counties(state = states)

block_pop_2010 <- read.csv("/project/biocomplexity/sdad/projects_data/project_data/usda/rural_broadband/population_by_census_block_2010 (2).csv")
block_pop_2010$GEOID <- ifelse(nchar(block_pop_2010$GEOID) < 15, paste0(0, block_pop_2010$GEOID), block_pop_2010$GEOID)

map_18_df_pop <- map_18_df %>% left_join(block_pop_2010 %>% transmute(GEOID  = as.factor(GEOID), pop = value), by = c("GEOID10" = "GEOID"))

rc_percelig_pop <- map_18_df_pop %>% 
  group_by(CTY = paste0(STATEFP10, COUNTYFP10), eligibility_final) %>% 
  summarise(sum_pop = sum(abs(pop)))

rc_percelig_pop2 <- rc_percelig_pop %>% 
  reshape2::dcast(CTY~eligibility_final, value.var = "sum_pop") %>% 
  mutate(eligible = ifelse(is.na(eligible), 0, eligible),
                            ineligible = ifelse(is.na(ineligible), 0, ineligible),
                            PERC_ELIG_14 = eligible / (eligible + ineligible))

county_map__18elig <- county_map %>% left_join(rc_percelig_pop2, by = c("GEOID" = "CTY"))


```


ReConnect Eligibility 2018

```{r}
# library(leaflet)

qpal <- colorQuantile("Blues", county_map__18elig$PERC_ELIG_14, n = 5)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = county_map__18elig, 
              color = ~qpal(PERC_ELIG_14), stroke = FALSE, fillOpacity = 1) %>%
  addLegend("bottomright", pal = qpal, values = county_map__18elig$PERC_ELIG_14,
    title = "% Eligibility",
    # labFormat = labelFormat(suffix = "%"),
    opacity = 1
  )


```




```{r}
# map_14_df_pop <- map_14_df %>% left_join(block_pop_2010 %>% transmute(GEOID  = as.factor(GEOID), pop = value), by = c("GEOID10" = "GEOID"))

cc_percelig_pop <- map_18_df_pop %>% 
  mutate(cc_elig = ifelse(disqual1 == "eligible" & disqual2 == "eligible", "eligible", "ineligible")) %>%
  group_by(CTY = paste0(STATEFP10, COUNTYFP10), cc_elig) %>% 
  summarise(sum_pop = sum(abs(pop))) 

cc_percelig_pop2 <- cc_percelig_pop %>% 
  reshape2::dcast(CTY~cc_elig, value.var = "sum_pop") %>% 
  mutate(eligible = ifelse(is.na(eligible), 0, eligible),
                            ineligible = ifelse(is.na(ineligible), 0, ineligible),
                            PERC_ELIG_14 = eligible / (eligible + ineligible))

county_map__18elig <- county_map %>% left_join(cc_percelig_pop2, by = c("GEOID" = "CTY"))


```

## CC 18 ELig 

```{r}
qpal <- colorQuantile("Blues", county_map__18elig$PERC_ELIG_14, n = 5)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = county_map__18elig, 
              color = ~qpal(PERC_ELIG_14), stroke = FALSE, fillOpacity = 1) %>%
  addLegend("bottomright", pal = qpal, values = county_map__18elig$PERC_ELIG_14,
    title = "% Eligibility",
    # labFormat = labelFormat(suffix = "%"),
    opacity = 1
  )
```



```{r}

# map_14_df_pop %>% filter(is.na(nonruralTF)) %>% .$state %>% unique

nonrural2010_pop <- map_14_df_pop %>% 
  # mutate(cc_elig = ifelse(disqual1 == "eligible" & disqual2 == "eligible", "eligible", "ineligible")) %>%
  filter(!is.na(nonruralTF)) %>% 
  group_by(CTY = paste0(STATEFP10, COUNTYFP10), nonruralTF) %>% 
  summarise(sum_pop = sum(abs(pop))) 

nonrural2010_pop2 <- nonrural2010_pop %>% 
  reshape2::dcast(CTY~nonruralTF, value.var = "sum_pop") %>% 
  mutate(rural = ifelse(is.na(`0`), 0, `0`),
                            nonrural = ifelse(is.na(`1`), 0, `1`),
                            PERC_RURAL_10 = rural / (rural + nonrural)) %>%
  select(CTY, rural, nonrural, PERC_RURAL_10)

county_map__10rural <- county_map %>% left_join(nonrural2010_pop2, by = c("GEOID" = "CTY"))

qpal <- colorQuantile("Reds", county_map__10rural$PERC_RURAL_10, n = 5)
qpal <- colorBin("Reds", county_map__10rural$PERC_RURAL_10, bins = 5)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = county_map__10rural, 
              color = ~qpal(PERC_RURAL_10), stroke = FALSE, fillOpacity = 1) %>%
  addLegend("bottomright", pal = qpal, values = county_map__10rural$PERC_RURAL_10,
    title = "% Rurality",
    # labFormat = labelFormat(suffix = "%"),
    opacity = 1
  )





```


