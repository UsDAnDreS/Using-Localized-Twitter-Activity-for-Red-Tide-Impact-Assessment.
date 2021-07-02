source("Cleaning_functions.R")

library(readr)
library(tidyverse)
library(lubridate)
library(sp)

library(tm)
library(textstem)
library(tidytext)


## Include politics-tweets (red tide rick, red tide party)?
politics <- F





################
################
## Correlation of TOTAL AVG CELL COUNTS  with the # OF WEEKLY TWEETS
################
################

all.areas <- c("Pasco", "Tampa", "Clearwater", "StPete", "Bradenton", "Sarasota",
               "TampaBay", "Pinellas", "IMPRECISE_LINKS")
line.size <- 1.2

#if (!exists("full.df")){
full.df <- NULL
for (area in all.areas){
  full.df <- rbind(full.df,
                   read_csv(paste0(
                     "https://raw.githubusercontent.com/UsDAnDreS/Florida-Red-Tide-Event/master/Twitter_Scraping/Full%20Archive%20Premium/Stage10_SentimentAnalysis/", 
                     ifelse(politics, "POLITICS_", ""), "CLEANED_FOR_SENTIMENT_ANALYSIS_Full_",area, ".csv", sep=""),
                     col_types = cols()))
  
}
#}

dim(full.df)

full.df$created_at <- as.Date(full.df$created_at)

our.df <- full.df %>% filter(places_match | geoprofile_match)


our.df$tweet_full_contents <- lemmatize_strings(our.df$tweet_full_contents)




tidy.our.df <- as.tibble(our.df$tweet_full_contents) %>% unnest_tokens(word, value)
dim(tidy.our.df)

tidy.our.df <- tidy.our.df %>%
  anti_join(stop_words)

dim(tidy.our.df)



## All word counts (bar stopwords)
tidy.our.df.counts <- tidy.our.df %>%
  count(word, sort = TRUE)
View(tidy.our.df.counts)




## Topical terms
topic.df <- read.csv(file="Concern_Type_Vocabularies.csv" )



### Summary by topic
tidy.our.df.counts %>% 
  inner_join(topic.df) %>%
  group_by(Topic) %>%
  summarise(Mentions=sum(n)) %>%
  arrange(desc(Mentions))




