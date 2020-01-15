library(leaflet)
library(sf)
library(dplyr)

deedtax_IN_penderbipvariable <- readRDS("/project/biocomplexity/sdad/projects-active/usda/bb/working/deedtaxmergeall_in_bipvariables.RDS")
#readRDS("~/git/rural_broadband/data/Merged_Data_Smaller_States-John_Pender_18-09-2019/deedtaxmergeall_in_bipvariables.RDS") 

deedtax_IN_penderbipvariable <- deedtax_IN_penderbipvariable %>% st_transform(st_crs("+init=epsg:4326"))
testset <- deedtax_IN_penderbipvariable[deedtax_IN_penderbipvariable$bip_distance != "Non-BIP region",]

test_set <- testset %>% select(bip_distance) %>% mutate(color = recode(bip_distance,
                                                                       "50 miles" = "#e8cfe4", 
                                                                       "25 miles" = "#d2a1d4", 
                                                                       "10 miles" = "#df65b0", 
                                                                       "5 miles" = "#dd1c77", 
                                                                       "BIP region" = "#980043"))

indiana_plot_distances4 <- leaflet(test_set) %>% addTiles() %>%
  addCircleMarkers(radius = 3, color = test_set$color, stroke = F, fillOpacity = 0.5)

indiana_plot_distances3 <- leaflet(test_set) %>% addTiles() %>%
  addCircleMarkers(radius = 3, color = test_set$color, stroke = FALSE, fillOpacity = 0.5)


indiana_plot_distances <- leaflet(testset) %>% addTiles() %>%
  addCircleMarkers(
    #radius = ~ifelse(type == "ship", 6, 10),
    radius = 1,
    color = ~pal(bip_distance),
    stroke = FALSE, fillOpacity = 0.5
  )

indiana_plot_distances2 <- leaflet(testset) %>% addTiles() %>%
  addCircleMarkers(
    #radius = ~ifelse(type == "ship", 6, 10),
    radius = 3,
    color = ~pal(bip_distance),
    stroke = FALSE, fillOpacity = 0.5
  )

indiana_plot <- leaflet() %>% addTiles() %>%
  addCircles(data = deedtax_IN_penderbipvariable)

leaflet() %>% addTiles() %>% 
  addCircles(data = testset)

dplyr::tibble(pal(unique(testset$bip_distance)), unique(testset$bip_distance))

