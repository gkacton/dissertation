
# libraries! --------------------------------------------------------------

library(tidyverse)
library(rvest)
library(xml2)


# scrape page --------------------------------------------------

list_items <- function(url){
  ids <- as.data.frame(matrix(ncol = 2, nrow = 2000))
  colnames(ids) <- c("ID", "name")
  page <- read_html(url)
  for(j in 1:2000){
    row_path <- paste0(".post-", j)
    row <- page %>% html_element(css = row_path)
    ids$name[j] <- row %>% html_element(css = "td a") %>% html_text()
    # ids$name[j] <- page %>% html_element(css = path) %>% html_text()
    ids$ID[j] <- j
  }
  projects <- ids %>% filter(is.na(name) == F)

  # establish empty df
  items <- as.data.frame(matrix(ncol = 5, nrow = 100))
  colnames(items) <- c("title",
                       "URL",
                       "summary",
                       "categories",
                       "date")
  # populate df
  for(i in 1:nrow(projects)){
    ID <- projects$ID[i]

    # title
    items$title[i] <- projects$name[i]

    # # description
    items$summary[i] <- site %>%
                        html_element(css = paste0(".post-", ID, " p")) %>%
                        html_text() %>%
                        trimws()

    # # subjects
    row_path <- paste0("#post-row-", ID)
    row <- html_element(site, css = row_path)
    subjects <- row %>%
                html_elements(css = "td a span") %>%
                xml_attr("data-slug") %>%
                list()
    if(length(subjects) > 0){
      items$categories[i] <- subjects
    }

    # # date
    items$date[i] <- row %>% html_element(xpath = "td[6]") %>% html_text()

    # get rid of extra rows
    all_items <- items %>% filter(is.na(title) == F)
  }
  # return items df
  all_items
}

# scrape!

items <- list_items("https://coviddisabilityarchive.com/archive/")

# FIXING BUGGGGGS

# ids <- as.data.frame(matrix(ncol = 2, nrow = 2000))
# colnames(ids) <- c("ID", "name")
# 
# url <-  "https://coviddisabilityarchive.com/archive/"
# page <- read_html(url)
# for(j in 1:2000){
#   row_path <- paste0(".post-", j)
#   row <- page %>% html_element(css = row_path)
#   ids$name[j] <- row %>% html_element(css = "td a") %>% html_text()
#   # ids$name[j] <- page %>% html_element(css = path) %>% html_text()
#   ids$ID[j] <- j
#   projects <- ids %>% filter(is.na(name) == F)
# }

# write_json(items, "disability_JSON.json") 
# file disability_JSON.json (or dis_archive_JSON.json) is importable to OpenRefine!!

