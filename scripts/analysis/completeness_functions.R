# COMPLETENESS STATS FUNCTIONS SCRIPT
# Functions to compare metadata to a set standard -------------------------

# This script creates two functions which calculate completeness and uniqueness 
# metrics by comparing metadata against a target standard.
# 
# The first of these standards is a minimal metadata standard for community archives.
# 
# |Field                |Description                                                                                                                     |Type            |
# | ------------------- | ------------------------------------------------------------------------------------------------------------------------------ | -------------- |
# | Title               | A name given to the information object or resource.                                                                            | Administrative |
# | Object ID           | A unique identifier, such as an accession number, assigned to each information object.                                         | Administrative |
# | Description         | A free-text description of the resource and/or its contents.                                                                   | Discovery      |
# | Subject             | Topic(s) associated with the resource, typically sourced from controlled vocabularies or contributed by users in a folksonomy. | Discovery      |
# | Geographic Coverage | The spatial/geographic context or topic of the resource.                                                                       | Discovery      |
# | Temporal Coverage   | The temporal context or topic of the source.                                                                                   | Discovery      |
# 
# The second is the Dublin Core fifteen-element set. 


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(tidyjson)

# Minimal Metadata --------------------------------------------------------

## INPUTS
# Data = data.frame or tibble of metadata for the target archive
# Columns = list of columns which represent the data targeted by each field in the standard
# **Column names must be listed in the order:**
    # 1. Title
    # 2. Object ID
    # 3. Description
    # 4. Subject
    # 5. Geographic Coverage
    # 6. Temporal Coverage

# Defining function 
minimal_stats <- function(data, columns){
  
  # compute prevalence statistics -------------------------
  
  # initialize table
  stats <- as.data.frame(matrix(nrow = 6, ncol = 6))
  
  # identify target metadata elements
  elements <- c("title", "object_id", "description", "subject", "geographic", "temporal")
  
  # name columns 
  colnames(stats) <- c("target_field", "archive_field", "occurences", "prevalence", "unique", "pct_unique")
  
  if(length(columns) == 6){ # makes sure all fields are identified
    for(i in 1:6){
      stats$target_field[i] <- elements[i]
      stats$archive_field[i] <- columns[i]
      
      # populate stats
      stats$occurences[i] <- nrow(data %>% filter(.data[[columns[[i]]]] != "" & .data[[columns[[i]]]] != "NULL" & is.na(.data[[columns[[i]]]]) == F))
      stats$prevalence[i] <- stats$occurences[i]/nrow(data)
      stats$unique[i] <- nrow(data %>% filter(is.na(.data[[columns[[i]]]]) == F & .data[[columns[[i]]]] != "NULL") %>% distinct(.data[[columns[[i]]]]))
      stats$pct_unique[i] <- stats$unique[i]/stats$occurences[i]
    }
  }
    return(stats)
}


# Dublin Core -----------------------------------------------------------

# Script to evaluate compliance with the Dublin Core mandatory element set
# INPUTS:
  # data: data.frame or tibble of metadata for target archive
  # colnames: list of length 15 with column titles crosswalked to each DC element 
    ### List of column names **must** match the order below:
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

dc_stats <- function(data, columns) {
  
  # create list of DC elements
  elements <- c("title", 
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
  colnames(stats) <- c("target_field", "archive_field", "occurences", "prevalence", "unique", "pct_unique")
   
  if(length(columns) == 15){ # checks that all elements are listed
    for(i in 1:15){
      stats$target_field[i] <- elements[i]
      stats$archive_field[i] <- elements[i]
      
      # populate stats
      stats$occurences[i] <- nrow(data %>% filter(.data[[columns[[i]]]] != "" & .data[[columns[[i]]]] != "NULL" & is.na(.data[[columns[[i]]]]) == F))
      stats$prevalence[i] <- stats$occurences[i]/nrow(data)
      stats$unique[i] <- nrow(data %>% filter(is.na(.data[[columns[[i]]]]) == F & .data[[columns[[i]]]] != "NULL") %>% distinct(.data[[columns[[i]]]]))
      stats$pct_unique[i] <- stats$unique[i]/stats$occurences[i]
    }
  }
  return(stats)
}


# Tests -------------------------------------------------------------------


# dc_stats(archive_it, c("collection.name", 
#                        "Subject",
#                        "Description",
#                        "Type",
#                        "Source",
#                        "Relation",
#                        "Coverage",
#                        "Creator",
#                        "Publisher",
#                        "Contributor",
#                        "Rights",
#                        "Archived.since",
#                        "Format",
#                        "Identifier",
#                        "Language"))
# 
# minimal_stats(archive_it, c("collection.name",
#                             "Identifier",
#                             "Description",
#                             "Subject",
#                             "Coverage",
#                             "Archived.since"))
