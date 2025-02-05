
# libraries ---------------------------------------------------------------

library(tidyverse)


# load data ---------------------------------------------------------------

ukwa <- read.csv("~/Desktop/dissertation/csv/UKWA_Covid19Collection.csv")


# basic stats -------------------------------------------------------------

# number of rows:
nrow(ukwa)

# included variables:
colnames(ukwa)


# date formatting ---------------------------------------------------------

ukwa_clean <- ukwa %>% 
  mutate(Date.Created = parse_date(Date.Created,
                                   format = "%d %B %Y")) %>% 
  mutate(Updated = parse_date(Updated,
                              format = "%d %B %Y")) %>% 
  mutate(Crawl.Start.Date = parse_date(Crawl.Start.Date,
                                   format = "%d %B %Y",
                                   na = c("NULL", ""))) %>% 
  mutate(Crawl.End.Date = parse_date(Crawl.End.Date,
                                    format = "%d %B %Y",
                                     na = c("NULL", ""))) 


# plot dates/date ranges --------------------------------------------------

ggplot(data = ukwa_clean) +
  geom_bar(aes(x = Date.Created))
  ## presence of pre-2020 date indicates that this variable is creation date of sites

ukwa_noNA <- ukwa_clean %>% 
  filter(is.na(Crawl.Start.Date) == F & is.na(Crawl.End.Date) == F)

date_ranges <- ggplot(data = ukwa_noNA) +
  aes(y = Record.ID) +
  geom_linerange(aes(xmin = Crawl.Start.Date, xmax = Crawl.End.Date),
                 color = "pink",
                 linewidth = 2) +
  geom_point(aes(x = Updated)) +
  xlim(as_date("2020-01-01", format = "%Y-%m-%d"), as_date("2025-01-01", format = "%Y-%m-%d"))


# added pre/post covid? ---------------------------------------------------

pre_2020 <- ukwa_clean %>% 
  filter(Date.Created < as_date("2020-01-01"))

post_2020 <- ukwa_clean %>% 
  filter(Date.Created >= as_date("2020-01-01"))


# contains "covid"? -------------------------------------------------------

ukwa_enriched <- ukwa_clean %>% 
  mutate(Covid.Name = ifelse(grepl("covid", Primary.Seed) == T |
                               grepl("corona", Primary.Seed) == T |
                               grepl("Corona", Primary.Seed) == T |
                               grepl("COVID", Primary.Seed) == T |
                               grepl("Covid", Primary.Seed) == T |
                               grepl("CORONA", Primary.Seed) == T |
                               grepl("cov19", Primary.Seed) == T |
                               grepl("Cov19", Primary.Seed) == T |
                               grepl("COV19", Primary.Seed) == T, 
                             T, # True value
                             F) # False value
  ) 

for(i in 1:nrow(ukwa_enriched)){
  if(grepl("/twitter", ukwa_enriched$Primary.Seed[i]) == T){
    ukwa_enriched$Social.Platform[i] <- "Twitter"
  } else if(grepl(".facebook", ukwa_enriched$Primary.Seed[i]) == T){
    ukwa_enriched$Social.Platform[i] <- "Facebook"
  } else if(grepl("instagram", ukwa_enriched$Primary.Seed[i]) == T){
    ukwa_enriched$Social.Platform[i] <- "Instagram"
  } else{
    ukwa_enriched$Social.Platform[i] <- NA
  }
}
  

covid_name_counts <- ukwa_enriched %>% 
  group_by(Covid.Name) %>% 
  count()

social_counts <- ukwa_enriched %>% 
  group_by(Social.Platform) %>% 
  count()


# date ranges of covid-specific pages -------------------------------------

covid_pages <- ukwa_enriched %>% 
  filter(Covid.Name == T)

dates <- ggplot(data = covid_pages) +
  geom_bar(aes(x = Date.Created))
## presence of pre-2020 date indicates that this variable is creation date of sites


date_ranges <- ggplot(data = covid_pages) +
  aes(y = Record.ID) +
  geom_linerange(aes(xmin = Crawl.Start.Date, xmax = Crawl.End.Date),
                 color = "pink",
                 linewidth = 2) +
  geom_point(aes(x = Updated)) +
  xlim(as_date("2020-01-01", format = "%Y-%m-%d"), as_date("2025-01-01", format = "%Y-%m-%d"))

# range demonstrates that most crawls were cut off at the end of 2023/beginning of 2024
# very few have been updated since 2022
# many weren't updated since their crawl began


# crawl frequencies -------------------------------------------------------

crawl_frequencies <- ukwa_clean %>% 
  group_by(Crawl.Frequency) %>% 
  count() %>% 
  mutate(proportion = n/nrow(ukwa_clean))


# Total date spans ---------------------------------------------------------

dates <- ukwa_clean %>% 
  summarize(start = min(Crawl.Start.Date,
                        na.rm = T),
            end = max(Crawl.End.Date,
                      na.rm = T))

ukwa_enriched <- ukwa_enriched %>% 
  mutate(Days.Crawled = abs(int_length(interval(start = Crawl.Start.Date,
                                 end = Crawl.End.Date))/(60*60*24)))


ggplot(ukwa_enriched) +
  geom_bar(aes(x = Days.Crawled))

## mode of the dataset is 10 --> also the median and 1st Q 
## is 10 days considered a standard length of time to crawl a website?
## was this decided during COVID, or already established precedent?

ten_days <- ukwa_enriched %>% 
  filter(Days.Crawled == 10)

ten_days %>% 
  group_by(Crawl.Frequency) %>% 
  count()

ggplot(ten_days) +
  geom_bar(aes(x = Date.Created))

# all sites created squarely during COVID
# 462 were crawled daily, 39 crawled weekly (? what does that mean for a 10-day crawl)
# largely government pages 

