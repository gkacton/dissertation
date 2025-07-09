
# libraries ---------------------------------------------------------------

library(tidyverse)


# load data ---------------------------------------------------------------

ukwa <- read.csv("data/csv/UKWA_Covid19Collection.csv")


# basic stats -------------------------------------------------------------

# number of rows:
nrow(ukwa)

# included variables:
colnames(ukwa)


# date formatting ---------------------------------------------------------

ukwa_dates <- ukwa %>% 
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

ggplot(data = ukwa_dates) +
  geom_bar(aes(x = Updated))

ggplot(data = ukwa_dates) +
  geom_bar(aes(x = Date.Created))
  ## presence of pre-2020 date indicates that this variable is creation date of sites

ukwa_noNA <- ukwa_dates %>% 
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
  mutate(Covid.Name = ifelse(grepl(regex("covid", ignore_case = T), Primary.Seed) == T |
                               grepl(regex("CORONA", ignore_case = T), Primary.Seed) == T |
                               grepl(regex("cov19", ignore_case = T), Primary.Seed) == T, 
                             yes = T, # True value
                             no = F) # False value
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
                 color = "#ddf1faff",
                 linewidth = 2) +
  geom_point(aes(x = Updated),
             color = "#9b3800ff" ) +
  labs(x = "Date",
       y = "Record ID") +
  xlim(as_date("2020-01-01", format = "%Y-%m-%d"), as_date("2025-01-01", format = "%Y-%m-%d")) +
  theme_minimal() +
  theme(
    text = element_text(family = "mono"),
    axis.title = element_text(color = "#0f4c82ff",
                              size = 16),
    axis.text = element_text(size = 12)
  ) 

# range demonstrates that most crawls were cut off at the end of 2023/beginning of 2024
# very few have been updated since 2022
# many weren't updated since their crawl began


# crawl frequencies -------------------------------------------------------

crawl_frequencies <- ukwa_clean %>% 
  group_by(Crawl.Frequency) %>% 
  count() %>% 
  mutate(proportion = n/nrow(ukwa_dates))


# Total date spans ---------------------------------------------------------

date_spans <- ukwa_dates %>% 
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


# How long ago were sites last updated? -----------------------------------

update_lag <- ukwa_dates %>% 
  mutate(lag_to_present = dmy("22/06/2025") - Updated,
         lag_from_start = Updated - Crawl.Start.Date,
         lag_to_end = Crawl.End.Date - Updated) %>% 
  filter(Crawl.Frequency != "DOMAINCRAWL")

mean(update_lag$lag_to_present, na.rm=T)
mean(update_lag$lag_from_start, na.rm=T)
mean(update_lag$lag_to_end, na.rm=T)
median(update_lag$lag_to_present, na.rm=T)
median(update_lag$lag_from_start, na.rm=T)
median(update_lag$lag_to_end, na.rm=T)

lags <- update_lag %>% 
  filter(Crawl.Frequency != "DOMAINCRAWL") %>% 
  group_by(lag_from_start) %>% 
  count() %>% 
  arrange(desc(n))

before_crawl <- update_lag %>% 
  filter(lag_from_start < 0)

lag_by_frequency <- update_lag %>% 
  group_by(Crawl.Frequency) %>% 
  summarize(avg_lag = mean(lag_from_start, na.rm = T),
            med_lag = median(lag_from_start, na.rm = T), 
            min_lag = min(lag_from_start, na.rm = T),
            max_lag = max(lag_from_start, na.rm = T))

end_lag_by_frequency <- update_lag %>% 
  group_by(Crawl.Frequency) %>% 
  summarize(avg_lag = mean(lag_to_end, na.rm = T),
            med_lag = median(lag_to_end, na.rm = T), 
            min_lag = min(lag_to_end, na.rm = T),
            max_lag = max(lag_to_end, na.rm = T))

ggplot(update_lag) +
  geom_boxplot(aes(lag_from_start, Crawl.Frequency)) +
  theme_minimal() +
  xlab("Time from Crawl Start to Last Record Update") +
  ylab("Crawl Frequency") +
  theme(text = element_text(family = "Courier New")) 

ggplot(update_lag) +
  geom_boxplot(aes(lag_to_end, Crawl.Frequency)) +
  theme_minimal() +
  xlab("Time from Last Record Update to Crawl End") +
  ylab("Crawl Frequency") +
  theme(text = element_text(family = "Courier New")) +
  geom_vline(xintercept = 0, color = "#5e94d6")

ggplot(update_lag) +
  geom_bar(aes(x = lag_to_present))

ggplot(update_lag) +
  geom_bar(aes(x = lag_from_start)) +
  xlim(-15, 1000)

update_lag %>% 
  group_by(lag_from_start) %>% 
  count() %>% 
  arrange(desc(n))


# Updated since crawl start by domain -------------------------------------

ukwa_domains <- ukwa_dates %>% 
  mutate(domain = domain(Primary.Seed)) %>% 
  mutate(suffix = suffix_extract(domain)) %>% 
  mutate(updated_since_crawl_start = if_else(Updated > Crawl.Start.Date, T, F),
         updated_since_crawl_end = if_else(Updated > Crawl.End.Date, T, F)) %>% 
  group_by(suffix) %>% 
  mutate(Crawl.Frequency = factor(Crawl.Frequency, levels = c("DOMAINCRAWL", "ANNUAL", "SIXMONTHLY", "QUARTERLY", "MONTHLY", "WEEKLY", "DAILY"))) %>% 
  mutate(crawl_duration = Crawl.End.Date - Crawl.Start.Date)

ggplot(ukwa_domains) +
  geom_bar(aes(x = suffix$suffix, fill = updated_since_crawl_start))

ggplot(ukwa_domains) +
  geom_bar(aes(x = Crawl.Frequency, fill = updated_since_crawl_start)) +
  scale_fill_manual(values = c("FALSE" = "#cde1fa",
                               "TRUE" = "#0979b3"),
                    na.value = "grey70") +
  theme_minimal() +
  theme(text = element_text(family = "Courier New")) +
  labs(fill = "Updated since crawl start?",
       x = "Crawl Frequency")
  
ggplot(ukwa_domains) +
  geom_histogram(aes(x = crawl_duration, fill = updated_since_crawl_start), origin = 0, binwidth = 365) +
  scale_fill_manual(values = c("FALSE" = "#cde1fa",
                               "TRUE" = "#0979b3"),
                    na.value = "grey70") +
  theme_minimal() +
  theme(text = element_text(family = "Courier New"))

ukwa_domains %>% 
  group_by(updated_since_crawl_start, Crawl.Frequency) %>% 
  summarize(mean_duration = mean(crawl_duration, na.rm = T),
            count = n()) 
