library(rvest)
library(dplyr)
library(stringr)
library(readr)


# Daniel helped me with the "assign" part. Thank you Daniel :)
# File will write out to /src/us_cities.csv (assuming Rproj).


#
#------------------------------------ Get list of state names
#

# Get list of all states
states_url <- read_html("https://simple.wikipedia.org/wiki/List_of_U.S._states")
states <- states_url %>% 
  html_table(fill = TRUE)
states <- states[[1]] 

# Get state names only
state_names <- states$`State Name`

# Fix blank space to - for URLs
state_names <- str_replace_all(state_names, " ", "-")

# Make state URLs
state_list <- str_c("https://broadbandnow.com/", state_names)


#
#------------------------------------ Get broadband information at city level by state
#

# For each state: read the URL, get the tables, select only the third table (city table), add state variable and remove empty column, and assign to a dataframe.

for (val_i in 1:length(state_list)) {
  cities <- read_html(state_list[val_i])
  data <- cities %>% 
    html_table(fill = TRUE) %>%
    .[[3]] %>%
    mutate(State = state_names[val_i]) %>%
    select(-`Average speed over time`)
  
  assign(paste("frame", str_to_lower(state_names[val_i]), sep = "_"), data)
}


#
#------------------------------------ Merge and write out 


# Make a list of dataframes
dflist <- lapply(ls(pattern = "frame_"), function(x) if (class(get(x)) == "data.frame") get(x))

# Bind data frames
us_cities <- do.call("rbind", dflist)

# Write out
write_csv(us_cities, path = "/src/us_cities.csv")


#
#------------------------------------ Clean up workspace
#

remove(list = ls())
