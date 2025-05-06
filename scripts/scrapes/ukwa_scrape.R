#### This script is no longer active -- UKWA provided their back-end metadata, which is in the `csv` file. 

# libraries! --------------------------------------------------------------

library(tidyverse)
library(rvest)
library(xml2)

# scrape archive-it page --------------------------------------------------

ukwa <- as.data.frame(matrix(ncol = 13, nrow = 5000))

colnames(ukwa) <- c("collection",
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
                           "URL",
                           "extra")

list_collections <- function(page, start_index, n_results){
  for(i in start_index:sum(start_index,n_results)){
    # scrape items for table 
    ukwa$collection[i] <- page %>% 
                          html_element(css = paste0('.col-sm-12:nth-child(', i, ') .margin-0')) %>% 
                          html_text() %>% 
                          trimws()
    descriptions <- page %>% 
                    html_elements(css = ".topics-themes-description") %>% 
                    html_attr("data-descript")
    ukwa$description <- descriptions[i+1]
    archive_dates <- page %>% 
                     html_elements(css = ".margin-top-10") %>% 
                     str_remove("Archived date: ") 
    if(i < 36){
      ukwa$archived.since[i] <- archive_dates
    }
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