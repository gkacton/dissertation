
# Generalized script for evaluating primary metadata categories --------
# target fields:
#   - description = description
#   - subject = subject
#   - geographic coverage = geographic
#   - temporal coverage = temporal

### Converted to function

library(tidyverse)
library(tidyjson)

generate_stats <- function(data, description_col, subject_col, geographic_col, temporal_col){
  # rename columns
  data_s <- data %>% 
    mutate(description = description_col,
           subject = subject_col,
           geographic = geographic_col,
           temporal = temporal_col)
  
  # compute prevalence of each and form df of stats -------------------------
  # initialize table
  stats <- as.data.frame(matrix(nrow = 4, ncol = 5))
  
  # name columns
  colnames(stats) <- c("field", "occurences", "prevalence", "unique", "pct_unique")
  
  # set up each row by field
  stats$field[1] <- "description"
  stats$field[2] <- "subject"
  stats$field[3] <- "geographic"
  stats$field[4] <- "temporal"
  
  # populate occurences
  # number of occurences = number of rows for the field that are not blank 
  stats$occurences[1] <- nrow(data_s %>% filter(is.na(description) == F & description != ""))
  stats$occurences[2] <- nrow(data_s %>% filter(is.na(subject) == F & subject != ""))
  stats$occurences[3] <- nrow(data_s %>% filter(is.na(geographic) == F & geographic != ""))
  stats$occurences[4] <- nrow(data_s %>% filter(is.na(temporal) == F & temporal != ""))
  
  # populate prevalence
  total <- nrow(data_s)
  
  stats$prevalence[1] <- stats$occurences[1]/total
  stats$prevalence[2] <- stats$occurences[2]/total
  stats$prevalence[3] <- stats$occurences[3]/total
  stats$prevalence[4] <- stats$occurences[4]/total
  
  # populate count of unique values
  stats$unique[1] <- length(unique(data_s$description))
  stats$unique[2] <- length(unique(data_s$subject))
  stats$unique[3] <- length(unique(data_s$geographic))
  stats$unique[4] <- length(unique(data_s$temporal))
  
  # populate unique percentages
  stats$pct_unique[1] <- stats$unique[1]/stats$occurences[1]
  stats$pct_unique[2] <- stats$unique[2]/stats$occurences[2]
  stats$pct_unique[3] <- stats$unique[3]/stats$occurences[3]
  stats$pct_unique[4] <- stats$unique[4]/stats$occurences[4]
  
  return(stats)
}

