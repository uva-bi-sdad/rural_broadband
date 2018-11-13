library(rvest)
library(dplyr)
library(stringr)


# Daniel helped me with the "assign" part. Thank you Daniel :)


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
state_names

# Fix blank space to - for URLs
state_names <- str_replace_all(state_names, " ", "-")
state_names 

# Make state URLs
state_list <- str_c("https://broadbandnow.com/", state_names)
state_list


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
  
  assign(x = str_to_lower(state_names[val_i]), data)
}


#
#------------------------------------ Merge and write out
#

# Working on this Wednesday


#
#------------------------------------ Clean up workspace
#

remove(list = c(ls(pattern = "cities"), ls(pattern = "data"), ls(pattern = "state"), ls(pattern = "val_i")))
