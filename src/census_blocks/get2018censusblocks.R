library(dplyr)
library(xml2)
library(rvest)
library(tigris)

tiger2018url<-  "https://www2.census.gov/geo/tiger/TIGER2018/TABBLOCK/"

### GET LINKS
url <- tiger2018url
url_href <- url %>% 
  xml2::read_html() %>%
  rvest::html_nodes("a") %>%
  rvest::html_attr("href")
links <- url_href[stringr::str_detect(url_href, ".zip$") == TRUE & !is.na(url_href)] %>% 
  dplyr::tibble("NAME" = links, "LINKS" =  paste0(url, links))

### DOWNLOAD DATA
## 1- Decide where it goes
## 2- Download
## 3- COMMENT OUT!! 
# setwd("~/censusblocks/blocks_TIGER2018_zips_shapes/")
# download.file(links$LINKS[], destfile = links$NAME[])

#xmlfiles <- unzip(temp, exdir = tdir)


### GET THE CENSUS FILE SIZES
url_td <- url %>% 
  xml2::read_html() %>%
  rvest::html_nodes("td") %>%
  rvest::html_text()

namesize_census <- stringr::str_trim(url_td[nchar(url_td) > 1])
size <- namesize_census[stringr::str_detect(namesize_census, "M$") == TRUE]
name <- namesize_census[stringr::str_detect(namesize_census, "^tl_2018") == TRUE]
namesize_census <- dplyr::tibble("CENSUS_NAME" = name, 
                                 "CENSUS_SIZE" = as.numeric(stringr::str_remove(size, "M")))
  
### GET THE SERVER FILE SIZES
namesize_server <- paste0("~/censusblocks/blocks_TIGER2018_zips_shapes/", list.files("~/censusblocks/blocks_TIGER2018_zips_shapes/")) %>% 
  file.info()  %>% 
  select(size) %>% 
  mutate("NAME" = rownames(.)) %>%
  filter(stringr::str_detect(NAME, ".zip$")==TRUE)  %>% 
  transmute("SERVER_NAME" = stringr::str_remove_all(NAME, "~/censusblocks/blocks_TIGER2018_zips_shapes/"), 
            "SERVER_SIZE" = size / 1048576)

### COMPARE
comparedownloads <- namesize_census %>% mutate(NAME = paste0("~/censusblocks/", name)) %>% left_join(namesize_server, by = "NAME")
colnames(comparedownloads) <- c("namefromtiger", "tigersize", "servername", "serversize")

usastatefipscodes <- read.csv("usastatefipscodes.csv") %>% select(State.Abbreviation, FIPS.Code, State.Name)
comparedownloads <- comparedownloads %>% 
  mutate(serverMB = serversize / 1048576, 
         check = as.numeric(stringr::str_remove(tigersize, "M")) - serverMB,
         decision = ifelse(check < 1, "success!", "revisit"), 
         fips = as.numeric(stringr::str_remove_all(namefromtiger, "tl_2018_|_tabblock10.zip"))) %>% 
  left_join(usastatefipscodes, by = c("fips" = "FIPS.Code"))

View(comparedownloads)


success <- comparedownloads %>% filter(decision == "success!") #18 worked!
tryagain <- comparedownloads %>% filter(decision == "revisit") #37 try again

#write.csv(comparedownloads, "comparedownloads.csv")
comparedownloads <- read.csv("~/comparedownloads.csv")
tryagain <- comparedownloads %>% filter(decision != "success!")
tryagain

tryagain <- all_links %>% inner_join(tryagain, by = c("links" = "namefromtiger"))

#temp <- tempfile(tmpdir = tdir <- "~/censusblocks2/")
setwd("censusblocks2/")
download.file(tryagain$fulllinks, destfile = tryagain$links) #link #9


namehere <- paste0("~/censusblocks2/", list.files("~/censusblocks2/"))
namesize_server <- file.info(namehere)

size_server <- namesize_server %>% select(size)
name_server <- rownames(namesize_server)
namesize_server <- dplyr::tibble("NAME" = name_server, "SIZE" = size_server$size) %>% mutate(NAME = stringr::str_remove(NAME, "~/censusblocks2/"))
comparedownloads %>% inner_join(namesize_server, by = c("namefromtiger" = "NAME")) %>%
  mutate(serverMB2 = SIZE / 1048576, 
         check2 = as.numeric(stringr::str_remove(tigersize, "M")) - serverMB2,
         decision2 = ifelse(check2 < 1, "success!", "revisit") )
