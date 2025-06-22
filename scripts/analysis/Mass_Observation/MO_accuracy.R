
# load data ---------------------------------------------------------------

# county data from https://jharding.co.uk/list-of-uk-counties/
uk_counties <- read.csv("data/csv/UK_counties.csv")

# evaluate participant-contributed data -----------------------------------

MO_free_texts <- massobvs %>% 
  select(contains("Free.Text"))

counties <- MO_free_texts %>% 
  group_by(County.Free.Text) %>% 
  count() %>% 
  filter(County.Free.Text %in% uk_counties$County |
           tolower(County.Free.Text) %in% tolower(uk_counties$County))

counties_all <- MO_free_texts %>% 
  group_by(County.Free.Text) %>% 
  count()

ethnicity <- MO_free_texts %>% 
  group_by(Ethnicity.Free.Text) %>% 
  count()

MO_disability <- MO_free_texts %>% 
  group_by(Disability.Free.Text) %>% 
  count()

MO_religion <- MO_free_texts %>% 
  group_by(Religion.Free.Text) %>% 
  count()

MO_sexuality <- MO_free_texts %>% 
  group_by(Sexuality.Free.Text) %>% 
  count()
