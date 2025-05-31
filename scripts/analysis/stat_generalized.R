
# Generalized script for evaluating primary metadata categories --------
# target fields:
#   - description = description
#   - subject = subject
#   - geographic coverage = geographic
#   - temporal coverage = temporal
# Derived from Oksana Zavalina 'Contextual Metadata in Digital Aggregations' (2011)

### Converted to function

library(tidyverse)
library(tidyjson)

zavalina_stats <- function(data, z_colnames){
  # rename columns
  # data_s <- data %>% 
  #   mutate(description = description_col,
  #          subject = subject_col,
  #          geographic = geographic_col,
  #          temporal = temporal_col)
  
  # compute prevalence of each and form df of stats -------------------------
  # initialize table
  stats <- as.data.frame(matrix(nrow = 4, ncol = 5))
  
  # identify Zavalina's metadata elements
  z_elements <- c("description", "subject", "geographic", "temporal")
  
  # name columns
  colnames(stats) <- c("field", "occurences", "prevalence", "unique", "pct_unique")
  
  if(length(z_colnames) == 4){
    for(i in 1:4){
      stats$z.field[i] <- z_elements[i]
      stats$archive.field[i] <- z_colnames[i]
      
      # populate stats
      stats$occurences[i] <- nrow(data %>% filter(.data[[z_colnames[[i]]]] != "" & .data[[z_colnames[[i]]]] != "NULL"))
      stats$prevalence[i] <- stats$occurences[i]/nrow(data)
      stats$unique[i] <- nrow(data %>% filter(is.na(.data[[z_colnames[[i]]]]) == F) %>% distinct(.data[[z_colnames[[i]]]]))
      stats$pct_unique[i] <- stats$unique[i]/stats$occurences[i]
    }
  }
    return(stats)
}
#   # set up each row by field
#   stats$field[1] <- "description"
#   stats$field[2] <- "subject"
#   stats$field[3] <- "geographic"
#   stats$field[4] <- "temporal"
#   
#   # populate occurences
#   # number of occurences = number of rows for the field that are not blank 
#   stats$occurences[1] <- nrow(data_s %>% filter(is.na(description) == F & description != "" & description != "NULL"))
#   stats$occurences[2] <- nrow(data_s %>% filter(is.na(subject) == F & subject != "" & subject != "NULL"))
#   stats$occurences[3] <- nrow(data_s %>% filter(is.na(geographic) == F & geographic != "" & geographic != "NULL"))
#   stats$occurences[4] <- nrow(data_s %>% filter(is.na(temporal) == F & temporal != "" & temporal != "NULL"))
#   
#   # populate prevalence
#   total <- nrow(data_s)
#   
#   stats$prevalence[1] <- stats$occurences[1]/total
#   stats$prevalence[2] <- stats$occurences[2]/total
#   stats$prevalence[3] <- stats$occurences[3]/total
#   stats$prevalence[4] <- stats$occurences[4]/total
#   
#   # populate count of unique values
#   stats$unique[1] <- nrow(data_s %>% filter(is.na(description) == F) %>% distinct(description))
#   stats$unique[2] <- nrow(data_s %>% filter(is.na(subject) == F) %>% distinct(subject))
#   stats$unique[3] <- nrow(data_s %>% filter(is.na(geographic) == F) %>% distinct(geographic))
#   stats$unique[4] <- nrow(data_s %>% filter(is.na(temporal) == F) %>% distinct(temporal))
#   
#   # populate unique percentages
#   stats$pct_unique[1] <- stats$unique[1]/stats$occurences[1]
#   stats$pct_unique[2] <- stats$unique[2]/stats$occurences[2]
#   stats$pct_unique[3] <- stats$unique[3]/stats$occurences[3]
#   stats$pct_unique[4] <- stats$unique[4]/stats$occurences[4]
#   
#   return(stats)
# }


# DC Compliance -----------------------------------------------------------

# Script to evaluate compliance with the Dublin Core mandatory element set
# INPUTS:
  # data: df of metadata for archive
  # colnames: list of length 15 with column titles crosswalked to each DC element 
    ### MUST BE IN ORDER OF DC ELEMENTS BELOW
#--------------------------
 ### THE DC ELEMENTS:
#   | Title           |
#   | **Subject**     |
#   | **Description** |
#   | Type            |
#   | Source          |
#   | Relation        |
#   | **Coverage**    |
#   | Creator         |
#   | Publisher       |
#   | Contributor     |
#   | Rights          |
#   | **Date**        |
#   | Format          |
#   | Identifier      |
#   | Language        |

dc_stats <- function(data, dc_colnames) {
  
  # create list of DC elements
  dc_elements <- c("title", 
                   "subject", 
                   "description",
                   "type",
                   "source",
                   "relation",
                   "coverage",
                   "creator",
                   "publisher",
                   "contributor",
                   "rights",
                   "date",
                   "format", 
                   "identifier",
                   "language")
  
  # generate empty DF to populate
  stats <- as.data.frame(matrix(nrow = 15, ncol = 6))
  
  # name columns
  colnames(stats) <- c("dc.field", "archive.field", "occurences", "prevalence", "unique", "pct_unique")
  
  if(length(dc_colnames) == 15){
    for(i in 1:15){
      stats$dc.field[i] <- dc_elements[i]
      stats$archive.field[i] <- dc_colnames[i]
      
      # populate stats
      stats$occurences[i] <- nrow(data %>% filter(.data[[dc_colnames[[i]]]] != "" & .data[[dc_colnames[[i]]]] != "NULL"))
      stats$prevalence[i] <- stats$occurences[i]/nrow(data)
      stats$unique[i] <- nrow(data %>% filter(is.na(.data[[dc_colnames[[i]]]]) == F & data[[dc_colnames[[i]]]] != "NULL") %>% distinct(.data[[dc_colnames[[i]]]]))
      stats$pct_unique[i] <- stats$unique[i]/stats$occurences[i]
    }
  }
  return(stats)
}

dc_stats(archive_it, c("collection.name", 
                       "Subject",
                       "Description",
                       "Type",
                       "Source",
                       "Relation",
                       "Coverage",
                       "Creator",
                       "Publisher",
                       "Contributor",
                       "Rights",
                       "Archived.since",
                       "Format",
                       "Identifier",
                       "Language"))

### DEBUGGING
dc_colnames <- c("collection.name", 
                 "Subject",
                 "Description",
                 "Type",
                 "Source",
                 "Relation",
                 "Coverage",
                 "Creator",
                 "Publisher",
                 "Contributor",
                 "Rights",
                 "Archived.since",
                 "Format",
                 "Identifier",
                 "Language")

