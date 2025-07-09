# A script to compare sites in the Coronavirus Collection to all sites identified using the Annotation and Curation Tool


# libraries ---------------------------------------------------------------

library(tidyverse)

# Load ACT sheet ----------------------------------------------------------

act <- read.csv("data/csv/Annotation_Curation_Tool_Metadata-Collection_Seed_List_CSV.csv")


# load UKWA ---------------------------------------------------------------

ukwa <- read.csv("data/csv/UKWA_Covid19Collection.csv")



# how many UKWA sites are in the ACT data? --------------------------------

ukwa_act <- ukwa %>% 
  filter(Primary.Seed %in% act$Primary.Seed)

act_ukwa <- act %>% 
  filter(Primary.Seed %in% ukwa$Primary.Seed)
