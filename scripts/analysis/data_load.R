
# Load packages -----------------------------------------------------------

library(tidyjson)
library(jsonlite)



# Load data ---------------------------------------------------------------


# Load Archive-It
archive_it <- read.csv("data/csv/archive-it-C19WA.csv") %>% 
  select(-X) %>% 
  filter(if_any(everything(), ~ !is.na(.)))

# Load UKWA and add an NA column
ukwa <- read.csv("data/csv/UKWA_Covid19Collection.csv")
ukwa <- ukwa %>% 
  mutate(x.col = NA) # adds an NA column to use for any missing elements

# Load Disability Archive, add NA column, separate IDs from titles
disability <- fromJSON("data/json/dis_archive_JSON.json") %>% 
  mutate(x.col = NA) %>% # adds an NA column to use for missing elements
  filter(title != "The Covid Disability Archive") %>% 
  # separate item IDs from titles 
  separate_wider_position(cols = title,
                          widths = c(id = 3, 1, title = 200),
                          too_few = "align_start") 

# Load Mass Observation and add an NA column
massobvs <- read.csv("data/csv/MassObservation.csv")
massobvs <- massobvs %>% 
  mutate(x.col = NA)

# Load JOTPY from JSON file 
jotpy <- fromJSON("data/json/jotpy_backup.json")


### If something happens to JOTPY data, RUN THIS 
# source("scripts/scrapes/JOTPY_api.R") # takes a long time to run!

