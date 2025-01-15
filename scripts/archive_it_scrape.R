
# libraries! --------------------------------------------------------------

library(tidyverse)
library(rvest)
library(xml2)


# scrape archive-it page --------------------------------------------------

collections <- as.data.frame(matrix(ncol = 12, nrow = 326))

colnames(collections) <- c("collection",
                           "collected.by",
                           "archived.since",
                           "description",
                           "subject",
                           "creator",
                           "publisher",
                           "type",
                           "date",
                           "collector",
                           "language",
                           "extra")

list_collections <- function(page, start_index, n_results){
  for(i in start_index:sum(start_index,n_results)){
    clump_path <- paste0('.result-item:nth-child(', i ,')') 
    clump <- page %>% html_element(css = clump_path) 
    # scrape items for table 
    collections$collection[i] <- clump %>% html_element(css = 'h3') %>% html_text()
    collections$collected.by[i] <- clump %>% html_element(css = 'p.collectedBy > a') %>% html_text()
    collections$description[i] <- clump %>% rvest::html_element(css = 'p.seamTextPara') %>% html_text()
    collections$archived.since[i] <- clump %>% html_element(css = '.collectedBy+ p') %>% html_text() %>% str_remove_all("Archived since: ")
    # scrape subject tags
    subjects <- clump %>% rvest::html_element(css = '.seamTextPara~ p') 
    if(is.na(subjects) == F){
      collections$subject[i] <- subjects %>% 
        rvest::html_elements(css = 'a') %>% 
        html_text() %>% 
        str_remove_all("\t") %>% 
        str_remove_all("\n") %>% 
        trimws("both") %>% 
        str_split(pattern = '" "') %>% 
        list()
    }
  }
  collections
}

# actual scraping

site_1 <- read_html("https://archive-it.org/explore?q=COVID&show=Collections")
collections_1 <- list_collections(site_1, 1, 100)

site_2 <- read_html("https://archive-it.org/explore?q=COVID&show=Collections&page=2&totalResultCount=326")
collections_2 <- list_collections(site_2, 1, 100)

site_3 <- read_html("https://archive-it.org/explore?q=COVID&page=3&show=Collections&totalResultCount=326")
collections_3 <- list_collections(site_3, 1, 100)

site_4 <- read_html("https://archive-it.org/explore?q=COVID&page=4&show=Collections&totalResultCount=326")
collections_4 <- list_collections(site_4, 1, 100)

# bind all DFs

collections <- rbind(collections_1, collections_2, collections_3, collections_4)

# assorted data cleaning tasks

collections_clean <- collections %>% 
  filter(is.na(collection) == F) %>% 
  mutate(archived.since = as.Date(parsedate::parse_date(archived.since)))