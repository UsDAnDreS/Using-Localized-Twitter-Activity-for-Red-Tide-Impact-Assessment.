###############
## Code calculating correlations between local Twitter activity 
## and dead fish levels observed on local beaches.
###############


source("Cleaning_functions.R")

library(readr)
library(tidyverse)
library(lubridate)
library(sp)


###############################
###############################
### USER NEEDS TO SPECIFY PARAMETERS for CORRELATION CALCULATION
###############################
###############################


# Do we want weekly correlations ("1 week")? Daily ("1 day")? Every 3 days ("3 days")?
time.window <- "1 day"
# Do we want entire TB surrounding area ("Total")? County-level ("County")? No city- or ZCTA-level available for K. brevis data.
locality.lvl <- "County"
# If it's county/city/ZCTA, then do we want per-capita (TRUE)? Or sheer totals (FALSE)?
per.capita <- F
# Do we want tweet counts ("Count") or total sentiments ("Sentiment")?
metric <- "Count"
# Do we want to run it only for all account types (NULL)? Citizens only ("Citizen")? Media only ("Media")? 
source.type <-  NULL    # NULL means all tweets are included 
## What kind of tweets are we interested in: 
# all of them? Only negative (if so - what's the threshold)? Only positive?
tweet.sentiment <- c("all", "negative", "positive")[1]
neg.threshold <- -0.5


# Location of the twitter data file
twitter.data.file <- "Florida-Red-Tide-Event/Twitter_Scraping/Full Archive Premium/Secure_Tweets_Data.csv"
# Location of the county population file
county.population.file <- "County_Population_Data.csv"
# Location to the file containing K. brevis sample data, starting in Jan 2017, and with each sample assigned to a FL county of interest.
kbrevis.sample.file <- "KBrevis_Full_Coord_Margin=3_All_Metros_From_2017-01-01.csv"




###############################
###############################
## MAIN CODE (WARNING: LOOK AT YOUR OWN PERIL)
###############################
###############################



all.metros <- c("Pasco", "Tampa", "Clearwater", "StPete", "Bradenton", "Sarasota")
all.counties <- c("Pasco", "Hillsborough", "Pinellas", "Manatee", "Sarasota")
all.counties.to.metros <- c("Pasco", "Hillsborough", "Pinellas", "Pinellas", "Manatee", "Sarasota")
all.areas <- c("Pasco", "Tampa", "Clearwater", "StPete", "Bradenton", "Sarasota",
               "TampaBay", "Pinellas")
line.size <- 1.2


## N for the "Top-N" K-Brevis measurements to be taken.
if (locality.lvl == "County") N <- 5
if (locality.lvl == "Total") N <- 10




## Get the weighing scheme to distribute credit for Tampa Bay & Pinellas tweets
TB.Pinellas.credit.df <- TB.Pinellas.credit.function(county.population.file = county.population.file)


## This one, for each metro area, includes KBrevis counts for areas
## a bit to the north & to the south of that respective area
## (trying to account for "carry-over" effects)

our_metro_df <- read_csv(file=kbrevis.sample.file,
                         col_types=cols())
head(our_metro_df)

our_metro_df$sample_date <- as.Date(our_metro_df$sample_date)


if (locality.lvl == "County"){
  
  ## Switching the "metro" names for county names.
  for (j in 1:length(all.metros)){
    our_metro_df$location[our_metro_df$location == all.metros[j]] <- all.counties.to.metros[j]
  }
  head(our_metro_df)
  
  
  
  date.from <- as.Date("2018-05-01")
  date.to <- as.Date("2019-05-01")
  
  
  #### Taking the mean of TOP-N MEASUREMENTS THAT WEEK
  #### (this will ANNULL the effect of DIFFERING SAMPLE SIZES)
  
  timeline_topN <- our_metro_df %>%
    filter(sample_date >= date.from & sample_date <= date.to) %>%
    mutate(sample_date = floor_date(sample_date,  unit="1 day"))
  
  ## Making sure we have a common grid of time periods (with 1-day, 3-day, 7-day jumps)
  date.seq <- seq(date.from, date.to, by=time.window)
  new.dates <- NULL
  for (j in 1:length(timeline_topN$sample_date)){
    new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - timeline_topN$sample_date[j]))]))
  }
  new.dates <- as.Date(new.dates)
  
  timeline_topN$Date <- new.dates
  ###
  
  timeline_topN <- timeline_topN %>%
    mutate(location = factor(location, levels=all.counties)) %>%
    complete(location=all.counties, Date = date.seq, fill=list(cellcount=0)) %>%
    group_by(location, Date) %>% 
    summarise(mean.cellcount = mean(sort(cellcount, decr=T)[1:ifelse(length(cellcount)>=N, N, length(cellcount))])) %>%
    mutate(type = "1. KBrevis",
           location = factor(location, levels=all.counties)) %>%
    ungroup()
  
  
  
  ################
  ## Loading the Twitter data
  ################
  
  all.areas <- c("Pasco", "Tampa", "Clearwater", "StPete", "Bradenton", "Sarasota",
                 "TampaBay", "Pinellas")
  line.size <- 1.2
  
  full.df <- read_csv(twitter.data.file,
                      col_types = cols())
  
  full.df$created_at <- as.Date(full.df$created_at)
  
  
  ## Switching the "metro" names for county names.
  for (j in 1:length(all.metros)){
    full.df$location[full.df$location == all.metros[j]] <- all.counties.to.metros[j]
  }
  head(full.df)
  
  
  #################
  ### Creating the MODELING DATA FRAME, piece-by-piece
  ##  First, initializing it with cell count data, 
  ##  then adding the tweet counts/sentiments
  #################
  
  model.df <- timeline_topN %>%
    select(location, Date, mean.cellcount)
  
  tweet.types <- c("places", 
                   "geo")
  
  if (per.capita){
    ###########################
    ### PER-CAPITA TWEETS #####
    ###########################
    
    if (metric == "Count"){
      ###################################
      ### FIRST, using TWEET COUNTS.
      ####################################
      
      
      for (tweet.type in tweet.types){
        print(tweet.type)
        if (tweet.type == "places") our.df <- full.df %>% filter(places_match) 
        if (tweet.type == "geoprofile") our.df <- full.df %>% filter(geoprofile_match) 
        if (tweet.type %in% c("geo","weighted.geo")) our.df <- full.df %>% filter(places_match | geoprofile_match) 
        
        ## If ONLY NEGATIVE ones needed
        if (tweet.sentiment == "negative") our.df <- our.df %>% filter(total_sentiment <= neg.threshold)
        if (tweet.sentiment == "positive") our.df <- our.df %>% filter(total_sentiment > 0)
        dim(our.df)
        
        ## ONLY CITIZENS??
        if (exists("source.type")) {if (!is.null(source.type)) our.df <- our.df %>% filter(source %in% source.type)}
        
        
        timeline_interm <- our.df %>% filter(!is.na(created_at), created_at >= date.from, created_at <= date.to) %>%
          mutate(created_at = floor_date(created_at, unit="1 day"))
        
        new.dates <- NULL
        for (j in 1:length(timeline_interm$created_at)){
          new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - as.Date(timeline_interm$created_at)[j]))]))
        }
        new.dates <- as.Date(new.dates)
        timeline_interm$Date <- new.dates
        
        
        
        timeline_by_metro <- timeline_interm %>% 
          complete(location=all.counties, Date = date.seq) %>%
          group_by(location, Date) %>% 
          summarise(tweet.count = ifelse(tweet.type=="weighted.geo", 
                                         sum(geo_weight, na.rm=T),
                                         n_distinct(id_str, na.rm=T))) %>%
          filter(!(location %in% c("TampaBay"))) %>%
          ungroup()
        
        
        timeline_by_area_TB_Pinellas <- timeline_interm %>% 
          complete(location=all.counties, Date = date.seq) %>%
          group_by(location, Date) %>% 
          summarise(tweet.count = ifelse(tweet.type=="weighted.geo", 
                                         sum(geo_weight, na.rm=T),
                                         n_distinct(id_str, na.rm=T))) %>%
          filter(location %in% c("TampaBay")) %>%
          ungroup()
        
        
        ## Creating extra rows for Tampa, Clearwater, StPete based on TampaBay & Pinellas tweets
        timeline_by_metro_TB_Pinellas <- NULL
        
        if (nrow(timeline_by_area_TB_Pinellas)>0){
          for (j in 1:nrow(timeline_by_area_TB_Pinellas)){
            cur.row <- timeline_by_area_TB_Pinellas[j,]
            if (cur.row$location == "TampaBay"){
              timeline_by_metro_TB_Pinellas <- rbind(timeline_by_metro_TB_Pinellas, 
                                                     tibble(location=c("Hillsborough", "Pinellas"), 
                                                            Date=c(cur.row$Date, cur.row$Date), 
                                                            tweet.count=TB.Pinellas.credit.df$TB.credit[all.counties %in% c("Hillsborough","Pinellas")]*cur.row$tweet.count))
            }
          }
        }
        
        
        ## Combining the extra credit due to TB/Pinellas, with the initial timeline by metro
        timeline_by_metro_ALL_INCLUDED <- rbind(timeline_by_metro,
                                                timeline_by_metro_TB_Pinellas) %>%
          group_by(location, Date) %>%
          summarise(tweet.count = sum(tweet.count)) %>%
          ungroup()
        
        
        
        # NORMALIZING BY THE "ELIGIBLE" POPULATION OF METRO AREA
        timeline_by_metro_ALL_INCLUDED_PerCapita <- 
          left_join(timeline_by_metro_ALL_INCLUDED,
                    TB.Pinellas.credit.df %>% select(location, Eligible.population),
                    by=c("location")) %>%
          mutate(tweet.count = 100000*(tweet.count/Eligible.population)) %>%
          select(-Eligible.population)
        
        colnames(timeline_by_metro_ALL_INCLUDED)[colnames(timeline_by_metro_ALL_INCLUDED) == "tweet.count"] <- paste0("tweet.count.", tweet.type)
        colnames(timeline_by_metro_ALL_INCLUDED_PerCapita)[colnames(timeline_by_metro_ALL_INCLUDED_PerCapita) == "tweet.count"] <- paste0("tweet.count.", tweet.type)
        
        
        model.df <- model.df %>%
          left_join(timeline_by_metro_ALL_INCLUDED_PerCapita)
        
        
      }
      
    }
    
    if (metric == "Sentiment"){
      
      
      for (tweet.type in tweet.types){
        if (tweet.type == "places") our.df <- full.df %>% filter(places_match) 
        if (tweet.type == "geoprofile") our.df <- full.df %>% filter(geoprofile_match) 
        if (tweet.type %in% c("geo","weighted.geo")) our.df <- full.df %>% filter(places_match | geoprofile_match) 
        
        ## If ONLY NEGATIVE ones needed
        if (tweet.sentiment == "negative") our.df <- our.df %>% filter(total_sentiment <= neg.threshold)
        if (tweet.sentiment == "positive") our.df <- our.df %>% filter(total_sentiment > 0)
        dim(our.df)
        
        ## ONLY CITIZENS??
        if (exists("source.type")) {if (!is.null(source.type)) our.df <- our.df %>% filter(source %in% source.type)}
        
        timeline_interm <- our.df %>% filter(!is.na(created_at), created_at >= date.from, created_at <= date.to) %>%
          mutate(created_at = floor_date(created_at, unit="1 day"))
        
        new.dates <- NULL
        for (j in 1:length(timeline_interm$created_at)){
          new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - as.Date(timeline_interm$created_at)[j]))]))
        }
        new.dates <- as.Date(new.dates)
        timeline_interm$Date <- new.dates
        
        
        
        timeline_by_metro <- timeline_interm %>% 
          complete(location=all.counties, Date=date.seq) %>%
          group_by(location, Date) %>% 
          summarise(tweet.count = ifelse(sum(total_sentiment, na.rm=T) == "NaN", 0, sum(total_sentiment, na.rm=T))) %>%
          filter(!(location %in% c("TampaBay"))) %>%
          ungroup()
        
        
        timeline_by_area_TB_Pinellas <- timeline_interm %>% 
          complete(location=all.counties, Date=date.seq) %>%
          group_by(location, Date) %>% 
          summarise(tweet.count = ifelse(sum(total_sentiment, na.rm=T) == "NaN", 0, sum(total_sentiment, na.rm=T))) %>%
          filter(location %in% c("TampaBay")) %>%
          ungroup()
        
        
        
        
        
        ## Creating extra rows for Tampa, Clearwater, StPete based on TampaBay & Pinellas tweets
        timeline_by_metro_TB_Pinellas <- NULL
        if (nrow(timeline_by_area_TB_Pinellas)>0){
          for (j in 1:nrow(timeline_by_area_TB_Pinellas)){
            cur.row <- timeline_by_area_TB_Pinellas[j,]
            if (cur.row$location == "TampaBay"){
              timeline_by_metro_TB_Pinellas <- rbind(timeline_by_metro_TB_Pinellas, 
                                                     tibble(location=c("Hillsborough", "Pinellas"), 
                                                            Date=c(cur.row$Date,cur.row$Date), 
                                                            tweet.count=TB.Pinellas.credit.df$TB.credit[all.counties %in% c("Hillsborough", "Pinellas")]*cur.row$tweet.count))
              # tweet.count=round(TB.Pinellas.credit.df$TB.credit[all.counties %in% c("Hillsborough", "Pinellas")]*cur.row$tweet.count)))
            }                                               
          }
        }
        
        
        ## Combining the extra credit due to TB/Pinellas, with the initial timeline by metro
        timeline_by_metro_ALL_INCLUDED <- rbind(timeline_by_metro,
                                                timeline_by_metro_TB_Pinellas) %>%
          group_by(location, Date) %>%
          summarise(tweet.count = sum(tweet.count))
        
        
        # NORMALIZING BY THE "ELIGIBLE" POPULATION OF METRO AREA
        timeline_by_metro_ALL_INCLUDED_PerCapita <- 
          left_join(timeline_by_metro_ALL_INCLUDED,
                    TB.Pinellas.credit.df %>% select(location, Eligible.population),
                    by=c("location")) %>%
          mutate(tweet.count = 100000*(tweet.count/Eligible.population)) %>%
          select(-Eligible.population)
        
        colnames(timeline_by_metro_ALL_INCLUDED)[colnames(timeline_by_metro_ALL_INCLUDED) == "tweet.count"] <- paste0("tweet.count.", tweet.type)
        colnames(timeline_by_metro_ALL_INCLUDED_PerCapita)[colnames(timeline_by_metro_ALL_INCLUDED_PerCapita) == "tweet.count"] <- paste0("tweet.count.", tweet.type)
        
        model.df <- model.df %>%
          left_join(timeline_by_metro_ALL_INCLUDED_PerCapita)
        
      }
      
    }
    
    
  }
  
  
  if (!per.capita){
    #############################################
    ### SHEER TWEET COUNTS (NOT PER-CAPITA) #####
    #############################################
    if (metric == "Count"){
      for (tweet.type in tweet.types){
        if (tweet.type == "places") our.df <- full.df %>% filter(places_match) 
        if (tweet.type == "geoprofile") our.df <- full.df %>% filter(geoprofile_match) 
        if (tweet.type %in% c("geo","weighted.geo")) our.df <- full.df %>% filter(places_match | geoprofile_match) 
        
        ## If ONLY NEGATIVE ones needed
        if (tweet.sentiment == "negative") our.df <- our.df %>% filter(total_sentiment <= neg.threshold)
        if (tweet.sentiment == "positive") our.df <- our.df %>% filter(total_sentiment > 0)
        dim(our.df)
        
        ## ONLY CITIZENS??
        if (exists("source.type")) {if (!is.null(source.type)) our.df <- our.df %>% filter(source %in% source.type)}
        
        timeline_interm <- our.df %>% filter(!is.na(created_at), created_at >= date.from, created_at <= date.to) %>%
          mutate(created_at = floor_date(created_at, unit="1 day"))
        
        new.dates <- NULL
        for (j in 1:length(timeline_interm$created_at)){
          new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - as.Date(timeline_interm$created_at)[j]))]))
        }
        new.dates <- as.Date(new.dates)
        timeline_interm$Date <- new.dates
        
        timeline_by_metro <- timeline_interm %>% 
          complete(location=all.counties, Date = date.seq) %>%
          group_by(location, Date) %>% 
          summarise(tweet.count = ifelse(tweet.type=="weighted.geo", 
                                         sum(geo_weight, na.rm=T),
                                         n_distinct(id_str, na.rm=T))) %>%
          filter(!(location %in% c("TampaBay"))) %>%
          ungroup()
        
        
        timeline_by_area_TB_Pinellas <- timeline_interm %>% 
          complete(location=all.counties, Date = date.seq) %>%
          group_by(location, Date) %>% 
          summarise(tweet.count = ifelse(tweet.type=="weighted.geo", 
                                         sum(geo_weight, na.rm=T),
                                         n_distinct(id_str, na.rm=T))) %>%
          filter(location %in% c("TampaBay")) %>%
          ungroup()
        
        
        ## Creating extra rows for Tampa, Clearwater, StPete based on TampaBay & Pinellas tweets
        timeline_by_metro_TB_Pinellas <- NULL
        
        if (nrow(timeline_by_area_TB_Pinellas)>0){
          for (j in 1:nrow(timeline_by_area_TB_Pinellas)){
            cur.row <- timeline_by_area_TB_Pinellas[j,]
            if (cur.row$location == "TampaBay"){
              timeline_by_metro_TB_Pinellas <- rbind(timeline_by_metro_TB_Pinellas, 
                                                     tibble(location=c("Hillsborough", "Pinellas"), 
                                                            Date=c(cur.row$Date, cur.row$Date), 
                                                            tweet.count=TB.Pinellas.credit.df$TB.credit[all.counties %in% c("Hillsborough","Pinellas")]*cur.row$tweet.count))
            }
          }
        }
        
        
        ## Combining the extra credit due to TB/Pinellas, with the initial timeline by metro
        timeline_by_metro_ALL_INCLUDED <- rbind(timeline_by_metro,
                                                timeline_by_metro_TB_Pinellas) %>%
          group_by(location, Date) %>%
          summarise(tweet.count = sum(tweet.count)) %>%
          ungroup()
        
        
        colnames(timeline_by_metro_ALL_INCLUDED)[colnames(timeline_by_metro_ALL_INCLUDED) == "tweet.count"] <- paste0("tweet.count.", tweet.type)
        
        
        model.df <- model.df %>%
          left_join(timeline_by_metro_ALL_INCLUDED)
        
        
      }
      
    }
    if (metric == "Sentiment"){
      
      for (tweet.type in tweet.types){
        if (tweet.type == "places") our.df <- full.df %>% filter(places_match) 
        if (tweet.type == "geoprofile") our.df <- full.df %>% filter(geoprofile_match) 
        if (tweet.type %in% c("geo","weighted.geo")) our.df <- full.df %>% filter(places_match | geoprofile_match) 
        
        ## If ONLY NEGATIVE ones needed
        if (tweet.sentiment == "negative") our.df <- our.df %>% filter(total_sentiment <= neg.threshold)
        if (tweet.sentiment == "positive") our.df <- our.df %>% filter(total_sentiment > 0)
        dim(our.df)
        
        ## ONLY CITIZENS??
        if (exists("source.type")) {if (!is.null(source.type)) our.df <- our.df %>% filter(source %in% source.type)}
        
        
        timeline_interm <- our.df %>% filter(!is.na(created_at), created_at >= date.from, created_at <= date.to) %>%
          mutate(created_at = floor_date(created_at, unit="1 day"))
        
        new.dates <- NULL
        for (j in 1:length(timeline_interm$created_at)){
          new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - as.Date(timeline_interm$created_at)[j]))]))
        }
        new.dates <- as.Date(new.dates)
        timeline_interm$Date <- new.dates
        
        
        
        timeline_by_metro <- timeline_interm %>% 
          complete(location=all.counties, Date=date.seq) %>%
          group_by(location, Date) %>% 
          summarise(tweet.count = ifelse(sum(total_sentiment, na.rm=T) == "NaN", 0, sum(total_sentiment, na.rm=T))) %>%
          filter(!(location %in% c("TampaBay"))) %>%
          ungroup()
        
        
        timeline_by_area_TB_Pinellas <- timeline_interm %>% 
          complete(location=all.counties, Date=date.seq) %>%
          group_by(location, Date) %>% 
          summarise(tweet.count = ifelse(sum(total_sentiment, na.rm=T) == "NaN", 0, sum(total_sentiment, na.rm=T))) %>%
          filter(location %in% c("TampaBay")) %>%
          ungroup()
        
        
        
        ## Creating extra rows for Tampa, Clearwater, StPete based on TampaBay & Pinellas tweets
        timeline_by_metro_TB_Pinellas <- NULL
        if (nrow(timeline_by_area_TB_Pinellas)>0){
          for (j in 1:nrow(timeline_by_area_TB_Pinellas)){
            cur.row <- timeline_by_area_TB_Pinellas[j,]
            if (cur.row$location == "TampaBay"){
              timeline_by_metro_TB_Pinellas <- rbind(timeline_by_metro_TB_Pinellas, 
                                                     tibble(location=c("Hillsborough", "Pinellas"), 
                                                            Date=c(cur.row$Date,cur.row$Date), 
                                                            tweet.count=TB.Pinellas.credit.df$TB.credit[all.counties %in% c("Hillsborough", "Pinellas")]*cur.row$tweet.count))
            }                                               
          }
        }
        
        
        ## Combining the extra credit due to TB/Pinellas, with the initial timeline by metro
        timeline_by_metro_ALL_INCLUDED <- rbind(timeline_by_metro,
                                                timeline_by_metro_TB_Pinellas) %>%
          group_by(location, Date) %>%
          summarise(tweet.count = sum(tweet.count))
        
        colnames(timeline_by_metro_ALL_INCLUDED)[colnames(timeline_by_metro_ALL_INCLUDED) == "tweet.count"] <- paste0("tweet.count.", tweet.type)
        
        model.df <- model.df %>%
          left_join(timeline_by_metro_ALL_INCLUDED)
        
      }
      
    }
    
  }
  
  ## Making sure "NA" are treated as 0 (days with no tweet matches)
  model.df <- model.df %>%
    mutate_if(str_detect(colnames(model.df), "tweet.count"), function(x) ifelse(is.na(x),0,x))
  
  
  head(model.df)
  dim(model.df)
  
  
  
  ######################
  ### CONCURRENT correlations:
  ####################
  concur.corr <- round(model.df %>% select(-location, -Date) %>% cor(use="complete.obs") %>% .[1,],2)  %>% .[c(2,3)]
  
  
  ######################
  ### POST-FACTUM correlations (correlation of tweets with PREVIOUS TIME PERIOD's Kbrevis data)
  ######################
  postfactum.corr <- model.df %>%
    group_by(location) %>%
    mutate(mean.cellcount.lag = lag(mean.cellcount, n=1)) %>%
    ungroup() %>%
    select(-location, -Date) %>%
    cor(use="complete.obs") %>% .[,"mean.cellcount.lag"] %>% round(2) %>% .[c(2,3)]
  
  
  ######################
  ### ANTICIPATORY correlations (correlation of tweets with NEXT TIME PERIOD's Kbrevis data)
  ######################
  anticip.corr <- model.df %>%
    group_by(location) %>%
    mutate(mean.cellcount.lead= lead(mean.cellcount, n=1)) %>%
    ungroup() %>%
    select(-location, -Date) %>%
    cor(use="complete.obs") %>% .[,"mean.cellcount.lead"] %>% round(2) %>% .[c(2,3)]
  
}

if (locality.lvl == "Total"){
  
  date.from <- as.Date("2018-05-01")
  date.to <- as.Date("2019-05-01")
  
  
  
  #### Taking the mean of TOP-N MEASUREMENTS THAT WEEK
  #### (this will ANNULL the effect of DIFFERING SAMPLE SIZES)
  
  timeline_topN <- our_metro_df %>%
    filter(sample_date >= date.from & sample_date <= date.to) %>%
    mutate(sample_date = floor_date(sample_date,  unit="1 day"))
  
  ## Making sure we have a common grid of time periods (with 1-day, 3-day, 7-day jumps)
  date.seq <- seq(date.from, date.to, by=time.window)
  new.dates <- NULL
  for (j in 1:length(timeline_topN$sample_date)){
    new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - timeline_topN$sample_date[j]))]))
  }
  new.dates <- as.Date(new.dates)
  
  timeline_topN$Date <- new.dates
  
  ###
  
  timeline_topN <- timeline_topN %>%
    complete(Date = date.seq, fill=list(cellcount=0)) %>%
    group_by(Date) %>% 
    summarise(mean.cellcount = mean(sort(cellcount, decr=T)[1:ifelse(length(cellcount)>=N, N, length(cellcount))])) %>%
    mutate(type = "1. KBrevis") %>%
    ungroup()
  
  
  ################
  ## Loading the Twitter data
  ################
  
  all.areas <- c("Pasco", "Tampa", "Clearwater", "StPete", "Bradenton", "Sarasota",
                 "TampaBay", "Pinellas")
  line.size <- 1.2
  
  full.df <- read_csv(twitter.data.file,
                      col_types = cols())
  
  full.df$created_at <- as.Date(full.df$created_at)
  
  
  #################
  ### Creating the MODELING DATA FRAME, piece-by-piece
  ##  First, initializing it with cell count data, 
  ##  then adding the tweet counts/sentiments
  #################
  
  model.df <- timeline_topN %>%
    select(Date, mean.cellcount)
  
  tweet.types <- c("places", "geo")
  
  
  
  if (metric == "Count"){
    
    for (tweet.type in tweet.types){
      if (tweet.type == "places") our.df <- full.df %>% filter(places_match) 
      if (tweet.type == "geoprofile") our.df <- full.df %>% filter(geoprofile_match) 
      if (tweet.type %in% c("geo","weighted.geo")) our.df <- full.df %>% filter(places_match | geoprofile_match) 
      
      ## If ONLY NEGATIVE ones needed
      if (tweet.sentiment == "negative") our.df <- our.df %>% filter(total_sentiment <= neg.threshold)
      if (tweet.sentiment == "positive") our.df <- our.df %>% filter(total_sentiment > 0)
      dim(our.df)
      
      ## ONLY CITIZENS??
      if (exists("source.type")) {if (!is.null(source.type)) our.df <- our.df %>% filter(source %in% source.type)}
      
      timeline_interm <- our.df %>% filter(!is.na(created_at), created_at >= date.from, created_at <= date.to) %>%
        mutate(created_at = floor_date(created_at, unit="1 day"))
      
      new.dates <- NULL
      for (j in 1:length(timeline_interm$created_at)){
        new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - as.Date(timeline_interm$created_at)[j]))]))
      }
      new.dates <- as.Date(new.dates)
      timeline_interm$Date <- new.dates
      
      timeline <- timeline_interm %>% 
        complete(Date = date.seq) %>%
        group_by(Date) %>% 
        summarise(tweet.count = ifelse(tweet.type=="weighted.geo", 
                                       sum(geo_weight, na.rm=T),
                                       n_distinct(id_str, na.rm=T))) %>%
        ungroup()
      
      
      ## Combining the extra credit due to TB/Pinellas, with the initial timeline by metro
      timeline_ALL_INCLUDED <- timeline %>%
        group_by(Date) %>%
        summarise(tweet.count = sum(tweet.count)) %>%
        ungroup()
      
      
      colnames(timeline)[colnames(timeline) == "tweet.count"] <- paste0("tweet.count.", tweet.type)
      
      
      
      model.df <- model.df %>%
        left_join(timeline)
      
    }
  }
  if (metric == "Sentiment"){
    for (tweet.type in tweet.types){
      if (tweet.type == "places") our.df <- full.df %>% filter(places_match) 
      if (tweet.type == "geoprofile") our.df <- full.df %>% filter(geoprofile_match) 
      if (tweet.type %in% c("geo","weighted.geo")) our.df <- full.df %>% filter(places_match | geoprofile_match) 
      
      ## If ONLY NEGATIVE ones needed
      if (tweet.sentiment == "negative") our.df <- our.df %>% filter(total_sentiment <= neg.threshold)
      if (tweet.sentiment == "positive") our.df <- our.df %>% filter(total_sentiment > 0)
      dim(our.df)
      
      ## ONLY CITIZENS??
      if (exists("source.type")) {if (!is.null(source.type)) our.df <- our.df %>% filter(source %in% source.type)}
      
      #####
      ## Sentiment sum
      #####
      
      timeline <- our.df %>% filter(!is.na(created_at)) %>% 
        mutate(Date = floor_date(created_at, unit=time.window)) %>%
        complete(Date) %>%
        group_by( Date) %>% 
        summarise(tweet.count = ifelse(sum(total_sentiment, na.rm=T) == "NaN", 0, sum(total_sentiment, na.rm=T))) %>%
        ungroup()
      
      
      
      
      
      timeline_interm <- our.df %>% filter(!is.na(created_at), created_at >= date.from, created_at <= date.to) %>%
        mutate(created_at = floor_date(created_at, unit="1 day"))
      
      new.dates <- NULL
      for (j in 1:length(timeline_interm$created_at)){
        new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - as.Date(timeline_interm$created_at)[j]))]))
      }
      new.dates <- as.Date(new.dates)
      timeline_interm$Date <- new.dates
      
      timeline <- timeline_interm %>% 
        complete(Date = date.seq) %>%
        group_by(Date) %>% 
        summarise(tweet.count = ifelse(sum(total_sentiment, na.rm=T) == "NaN", 0, sum(total_sentiment, na.rm=T))) %>%
        ungroup()
      
      colnames(timeline)[colnames(timeline) == "tweet.count"] <- paste0("tweet.count.", tweet.type)
      
      model.df <- model.df %>%
        left_join(timeline)
      
    }
    
    
  }
  
  ### CONCURRENT correlations:
  concur.corr <- round(cor(model.df %>% select(-Date))[1,],2) %>% .[c(2,3)]
  
  
  ## POST-FACTUM correlations (correlation of tweets with PREVIOUS WEEK's Kbrevis data)
  postfactum.corr <- model.df %>%
    mutate(mean.cellcount.lag = lag(mean.cellcount, n=1)) %>%
    select(-Date) %>%
    cor(use="complete.obs") %>% .[,"mean.cellcount.lag"] %>% round(2) %>% .[c(2,3)]
  
  
  ## ANTICIPATORY correlations (correlation of tweets with NEXT WEEK's Kbrevis data)
  anticip.corr <- model.df %>%
    mutate(mean.cellcount.lead= lead(mean.cellcount, n=1)) %>%
    select(-Date) %>%
    cor(use="complete.obs") %>% .[,"mean.cellcount.lead"] %>% round(2) %>% .[c(2,3)] 
}




###############################
###############################
### THE RESULTING CORRELATIONS:
###############################
###############################

### CONCURRENT correlations:
concur.corr 

### POST-FACTUM correlations (correlation of tweets with PREVIOUS TIME PERIOD's Resp Irrit Levels data)
postfactum.corr

### ANTICIPATORY correlations (correlation of tweets with NEXT TIME PERIOD's Resp Irrit Levels data)
anticip.corr

