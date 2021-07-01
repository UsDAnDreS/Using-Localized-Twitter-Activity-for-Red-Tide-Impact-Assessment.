###############
## Code calculating correlations between local Twitter activity 
## and respiratory irritation levels observed on local beaches.
###############

source("Cleaning_functions.R")
library(readr)
library(tidyverse)
library(lubridate)
library(sp)
library(sf)



###############################
###############################
### USER NEEDS TO SPECIFY PARAMETERS for CORRELATION CALCULATION
###############################
###############################

# Do we want weekly correlations ("1 week")? Daily ("1 day")? Every 3 days ("3 days")?
time.window <- "1 week"
# Do we want entire TB surrounding area ("Total")? County-level ("County")? City-level ("City")? ZCTA-level ("ZCTA")?
locality.lvl <- "City"
# If it's county/city/ZCTA, then do we want per-capita (TRUE)? Or sheer totals (FALSE)?
per.capita <- F
# Do we want tweet counts ("Count") or total sentiments ("Sentiment")?
metric <- "Count"
# Do we want to run it for all account types (NULL)? Citizens only ("Citizen")? Media only ("Media")? 
source.type <-  NULL    # NULL means all tweets are included 
## What kind of tweets are we interested in: 
# all of them? Only negative (if so - what's the threshold)? Only positive?
tweet.sentiment <- c("all", "negative", "positive")[1]
neg.threshold <- -0.5

# Location of the twitter data file
twitter.data.file <- "Florida-Red-Tide-Event/Twitter_Scraping/Full Archive Premium/Secure_Tweets_Data.csv"
# Location of the county population file
county.population.file <- "County_Population_Data.csv"
# Location of the "smoothed" city metro population file (to avoid overreacting to tiny cities when doing per-capita adjustment, see "Method & Materials" description in the paper)
city.metro.population.file <- "city.metro.2018.populations.csv"
# Location of the beach condition data (respiratory irritation and dead fish levels)
beach.cond.data.file <- "clean_beach_conditions_max_per_day.csv"
# Location of "our_zcta.Robj" file, containing all ZCTAs in our Florida Gulf coast of interest.
our_zcta.file <- "our_zcta.Robj"
# Location of "FL.ZCTA.county.Robj" file, containing data on ZCTAs, their coordinates, primary cities associated with it, etc.
list.FL.ZCTA.county.Robj.file <- "FL.ZCTA.county.Robj"
# Location of "list.beach.ZCTA.Robj" file, containing which ZCTAs match to which beach.
list.beach.ZCTA.Robj.file <- "list.beach.ZCTA.Robj"





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




## Get the weighing scheme to distribute credit for Tampa Bay & Pinellas tweets
TB.Pinellas.credit.df <- TB.Pinellas.credit.function(county.population.file = county.population.file)


## Obtaining beach conditions time series data
our_metro_df <- read_csv(file=beach.cond.data.file,
                         col_types=cols())
head(our_metro_df)

our_metro_df$created_at <- as.Date(our_metro_df$created_at)


if (locality.lvl == "ZCTA"){
  
  ## Get the surrounding city populations
  city.metro.population.df <-
    read.csv(city.metro.population.file)
  
  
  ########
  ## Breaking down beach conditions time series data by ZCTA
  ########
  
  our_metro_df <- our_metro_df %>% select(created_at, beach_id, beach_name, location, respiratory_irritation_num, respiratory_irritation_num)
  
  beach.names <- unique(our_metro_df %>% select(beach_id, beach_name) %>% arrange(beach_id) %>% .[[2]])
  
  
  ## Loading all the ZCTA-related information.
  load(list.FL.ZCTA.county.Robj.file)
  load(list.beach.ZCTA.Robj.file)
  
  
  affected_ZCTA_df <- NULL
  
  ## Filling it out for ZCTAs associated with one of 12 beaches we got.
  for (i in 1:length(list.beach.ZCTA)){
    for (ZCTA.code in list.beach.ZCTA[[i]]){
      interm.df <- our_metro_df %>% filter(beach_name == beach.names[i]) 
      interm.df$ZCTA <- ZCTA.code
      affected_ZCTA_df <- rbind(affected_ZCTA_df, interm.df)
    }
  }
  
  
  ## Adding ZCTAs that were NOT associated with any of the beaches.
  # zips <- read_csv("zip_code_database.csv")
  # florida_zcta <- st_read("florida_ZCTA.shp")
  # counties <- c("Sarasota County", "Manatee County", "Pinellas County", "Pasco County", "Hillsborough County")
  # our_counties <- zips %>% filter(county %in% counties)
  # our_zcta <- florida_zcta %>% filter(ZCTA5CE10 %in% our_counties$zip)
  load(our_zcta.file)
  
  
  ## ALL ZCTAs
  all.zctas <- sort(our_zcta$ZCTA5CE10)
  
  left.out.TB.area.zctas <- all.zctas[!all.zctas %in% unlist(list.beach.ZCTA)]
  
  unaffected_ZCTA_df <- NULL
  
  for (ZCTA.code in left.out.TB.area.zctas){
    interm.df <- our_metro_df %>% filter(beach_name == beach.names[1]) 
    interm.df %>% complete(created_at = seq.Date(min(affected_ZCTA_df$created_at), max(affected_ZCTA_df$created_at), by="day"))
    interm.df[,c("beach_id", "beach_name", "location")] <- "None"
    interm.df[,c("respiratory_irritation_num")] <- 0
    interm.df$ZCTA <- ZCTA.code
    unaffected_ZCTA_df <- rbind(unaffected_ZCTA_df, interm.df)
  }
  
  
  our_ZCTA_df <- rbind(affected_ZCTA_df,
                       unaffected_ZCTA_df)
  
  dim(our_ZCTA_df)
  
  
  
  
  ########
  ## Getting Red Tide data
  ########
  
  date.from <- as.Date("2018-05-01")
  date.to <- as.Date("2019-05-01")
  
  
  timeline_overall <- our_ZCTA_df %>%
    filter(created_at >= date.from & created_at <= date.to) %>%
    mutate(created_at = floor_date(created_at,  unit="1 day"))
  
  ## Making sure we have a common grid of time periods (with 1-day, 3-day, 7-day jumps)
  date.seq <- seq(date.from, date.to, by=time.window)
  new.dates <- NULL
  for (j in 1:length(timeline_overall$created_at)){
    new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - timeline_overall$created_at[j]))]))
  }
  new.dates <- as.Date(new.dates)
  
  timeline_overall$Date <- new.dates
  ###
  
  timeline_overall <- timeline_overall %>%
    mutate(ZCTA = factor(ZCTA, levels=all.zctas)) %>%
    complete(ZCTA, Date = date.seq, fill=list(respiratory_irritation_num=0)) %>%
    group_by(ZCTA, Date) %>% 
    summarise(median.respiratory_irritation_num = median(respiratory_irritation_num),
              max.respiratory_irritation_num = max(respiratory_irritation_num),
              mean.respiratory_irritation_num = mean(respiratory_irritation_num),
              sum.respiratory_irritation_num = sum(respiratory_irritation_num),
              n.of.points = n()) %>%
    mutate(type = "1. Resp Irrit Levels",
           ZCTA = factor(ZCTA, levels=all.zctas)) %>%
    ungroup()
  
  
  
  ##########
  ## Getting Tweet data
  ##########
  
  all.areas <- c("Pasco", "Tampa", "Clearwater", "StPete", "Bradenton", "Sarasota",
                 "TampaBay", "Pinellas")
  line.size <- 1.2
  
  full.df <- read_csv(twitter.data.file,
                      col_types = cols())
  
  full.df$created_at <- as.Date(full.df$created_at)
  
  full.df <- full.df %>% filter(geoprofile_match | places_match)
  
  
  
  #################
  ### Creating the MODELING DATA FRAME, piece-by-piece
  ##  First, initializing it with beach conditions data, 
  ##  then adding the tweet counts/sentiments
  #################
  
  
  model.df <- timeline_overall %>%
    select(ZCTA, Date, mean.respiratory_irritation_num)
  
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
      
      
      model.df <- timeline_overall %>%
        select(ZCTA, Date, mean.respiratory_irritation_num)
      
      tweet.types <- c("places", "geo")
      
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
          mutate(created_at = floor_date(created_at, unit="1 day"),
                 ZCTA = factor(ZCTA, levels=all.zctas))
        
        new.dates <- NULL
        for (j in 1:length(timeline_interm$created_at)){
          new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - as.Date(timeline_interm$created_at)[j]))]))
        }
        new.dates <- as.Date(new.dates)
        timeline_interm$Date <- new.dates
        
        timeline_by_metro <- timeline_interm %>% 
          complete(ZCTA, Date = date.seq) %>%
          group_by(ZCTA, Date) %>% 
          summarise(tweet.count = ifelse(tweet.type=="weighted.geo", 
                                         sum(geo_weight, na.rm=T),
                                         n_distinct(id_str, na.rm=T))) %>%
          ungroup()
        
        
        ## Combining the extra credit due to TB/Pinellas, with the initial timeline by metro
        timeline_by_metro_ALL_INCLUDED <- timeline_by_metro %>%
          group_by(ZCTA, Date) %>%
          summarise(tweet.count = sum(tweet.count)) %>%
          ungroup()
        
        
        # NORMALIZING BY THE "ELIGIBLE" POPULATION OF METRO AREA
        timeline_by_metro_ALL_INCLUDED_PerCapita <- 
          left_join(timeline_by_metro_ALL_INCLUDED %>% mutate(ZCTA = as.character(ZCTA)),
                    FL.ZCTA.county %>%   # Left-joining with a City-Metro match data set.
                      left_join(city.metro.population.df,
                                by =c("primary_city" = "city")) %>%
                      select(ZCTA5CE10, metro.population) %>%
                      filter(ZCTA5CE10 %in% all.zctas),
                    by=c("ZCTA" = "ZCTA5CE10")) %>%
          mutate(tweet.count = 100000*(tweet.count/metro.population)) %>%
          select(-metro.population)
        
        
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
          mutate(created_at = floor_date(created_at, unit="1 day"),
                 ZCTA = factor(ZCTA, levels=all.zctas))
        
        new.dates <- NULL
        for (j in 1:length(timeline_interm$created_at)){
          new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - as.Date(timeline_interm$created_at)[j]))]))
        }
        new.dates <- as.Date(new.dates)
        timeline_interm$Date <- new.dates
        
        timeline_by_metro <- timeline_interm %>% 
          complete(ZCTA, Date = date.seq) %>%
          group_by(ZCTA, Date) %>% 
          summarise(tweet.count = ifelse(sum(total_sentiment, na.rm=T) == "NaN", 0, sum(total_sentiment, na.rm=T))) %>%
          ungroup()
        
        
        ## Combining the extra credit due to TB/Pinellas, with the initial timeline by metro
        timeline_by_metro_ALL_INCLUDED <- timeline_by_metro %>%
          group_by(ZCTA, Date) %>%
          summarise(tweet.count = sum(tweet.count)) %>%
          ungroup()
        
        
        # NORMALIZING BY THE "ELIGIBLE" POPULATION OF METRO AREA
        timeline_by_metro_ALL_INCLUDED_PerCapita <- 
          left_join(timeline_by_metro_ALL_INCLUDED %>% mutate(ZCTA = as.character(ZCTA)),
                    FL.ZCTA.county %>%   # Left-joining with a City-Metro match data set.
                      left_join(city.metro.population.df,
                                by =c("primary_city" = "city")) %>%
                      select(ZCTA5CE10, metro.population) %>%
                      filter(ZCTA5CE10 %in% all.zctas),
                    by=c("ZCTA" = "ZCTA5CE10")) %>%
          mutate(tweet.count = 100000*(tweet.count/metro.population)) %>%
          select(-metro.population)
        
        
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
          mutate(created_at = floor_date(created_at, unit="1 day"),
                 ZCTA = factor(ZCTA, levels=all.zctas))
        
        new.dates <- NULL
        for (j in 1:length(timeline_interm$created_at)){
          new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - as.Date(timeline_interm$created_at)[j]))]))
        }
        new.dates <- as.Date(new.dates)
        timeline_interm$Date <- new.dates
        
        timeline_by_metro <- timeline_interm %>% 
          complete(ZCTA, Date = date.seq) %>%
          group_by(ZCTA, Date) %>% 
          summarise(tweet.count = ifelse(tweet.type=="weighted.geo", 
                                         sum(geo_weight, na.rm=T),
                                         n_distinct(id_str, na.rm=T))) %>%
          ungroup()
        
        ## Combining the extra credit due to TB/Pinellas, with the initial timeline by metro
        timeline_by_metro_ALL_INCLUDED <- timeline_by_metro %>%
          group_by(ZCTA, Date) %>%
          summarise(tweet.count = sum(tweet.count)) %>%
          ungroup()
        
        
        colnames(timeline_by_metro_ALL_INCLUDED)[colnames(timeline_by_metro_ALL_INCLUDED) == "tweet.count"] <- paste0("tweet.count.", tweet.type)
        
        
        model.df <- model.df %>%
          left_join(timeline_by_metro_ALL_INCLUDED %>% mutate(ZCTA = as.character(ZCTA)))
        
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
          mutate(created_at = floor_date(created_at, unit="1 day"),
                 ZCTA = factor(ZCTA, levels=all.zctas))
        
        new.dates <- NULL
        for (j in 1:length(timeline_interm$created_at)){
          new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - as.Date(timeline_interm$created_at)[j]))]))
        }
        new.dates <- as.Date(new.dates)
        timeline_interm$Date <- new.dates
        
        timeline_by_metro <- timeline_interm %>% 
          complete(ZCTA, Date = date.seq) %>%
          group_by(ZCTA, Date) %>% 
          summarise(tweet.count = ifelse(sum(total_sentiment, na.rm=T) == "NaN", 0, sum(total_sentiment, na.rm=T))) %>%
          ungroup()
        
        ## Combining the extra credit due to TB/Pinellas, with the initial timeline by metro
        timeline_by_metro_ALL_INCLUDED <- timeline_by_metro %>% 
          mutate(ZCTA = as.character(ZCTA)) %>%
          group_by(ZCTA, Date) %>%
          summarise(tweet.count = sum(tweet.count))
        
        
        colnames(timeline_by_metro_ALL_INCLUDED)[colnames(timeline_by_metro_ALL_INCLUDED) == "tweet.count"] <- paste0("tweet.count.", tweet.type)
        
        
        model.df <- model.df %>%
          left_join(timeline_by_metro_ALL_INCLUDED)
        
        
      }
      
    }
  }
  
  ### CONCURRENT correlations:
  concur.corr <- round(model.df %>% select(-ZCTA, -Date) %>% cor(use="complete.obs") %>% .[1,],2) %>% .[c(2,3)]
  
  ## POST-FACTUM correlations (correlation of tweets with PREVIOUS TIME PERIOD's dead fish levels)
  postfactum.corr <- model.df %>%
    group_by(ZCTA) %>%
    mutate(mean.respiratory_irritation_num.lag = lag(mean.respiratory_irritation_num, n=1)) %>%
    ungroup() %>%
    select(-ZCTA, -Date) %>%
    cor(use="complete.obs") %>% .[,"mean.respiratory_irritation_num.lag"] %>% round(2) %>% .[c(2,3)]
  
  
  ## ANTICIPATORY BY-METRO correlations (correlation of tweets with NEXT TIME PERIOD's dead fish levels)
  anticip.corr <- model.df %>%
    group_by(ZCTA) %>%
    mutate(mean.respiratory_irritation_num.lead= lead(mean.respiratory_irritation_num, n=1)) %>%
    ungroup() %>%
    select(-ZCTA, -Date) %>%
    cor(use="complete.obs") %>% .[,"mean.respiratory_irritation_num.lead"] %>% round(2) %>% .[c(2,3)]
}


if (locality.lvl == "City"){
  
  ## Get the surrounding city populations
  city.metro.population.df <-
    read.csv(city.metro.population.file)
  
  ########
  ## Obtaining beach conditions time series data,
  ## along with BREAKING IT DOWN by ZCTA
  ########
  our_metro_df <- our_metro_df %>% select(created_at, beach_id, beach_name, location, respiratory_irritation_num, respiratory_irritation_num)
  beach.names <- unique(our_metro_df %>% select(beach_id, beach_name) %>% arrange(beach_id) %>% .[[2]])
  
  
  ## Loading all the ZCTA-related information.
  load(list.FL.ZCTA.county.Robj.file)
  load(list.beach.ZCTA.Robj.file)
  
  
  affected_ZCTA_df <- NULL
  
  ## Filling it out for ZCTAs associated with one of 12 beaches we got.
  for (i in 1:length(list.beach.ZCTA)){
    for (ZCTA.code in list.beach.ZCTA[[i]]){
      interm.df <- our_metro_df %>% filter(beach_name == beach.names[i]) 
      interm.df$ZCTA <- ZCTA.code
      affected_ZCTA_df <- rbind(affected_ZCTA_df, interm.df)
    }
  }
  
  
  ## Adding ZCTAs that were NOT associated with any of the beaches.
  # zips <- read_csv("zip_code_database.csv")
  # florida_zcta <- st_read("florida_ZCTA.shp")
  # counties <- c("Sarasota County", "Manatee County", "Pinellas County", "Pasco County", "Hillsborough County")
  # our_counties <- zips %>% filter(county %in% counties)
  # our_zcta <- florida_zcta %>% filter(ZCTA5CE10 %in% our_counties$zip)
  load(our_zcta.file)
  
  
  ## ALL ZCTAs
  all.zctas <- sort(our_zcta$ZCTA5CE10)
  
  left.out.TB.area.zctas <- all.zctas[!all.zctas %in% unlist(list.beach.ZCTA)]
  
  unaffected_ZCTA_df <- NULL
  
  for (ZCTA.code in left.out.TB.area.zctas){
    interm.df <- our_metro_df %>% filter(beach_name == beach.names[1]) 
    interm.df %>% complete(created_at = seq.Date(min(affected_ZCTA_df$created_at), max(affected_ZCTA_df$created_at), by="day"))
    interm.df[,c("beach_id", "beach_name", "location")] <- "None"
    interm.df[,c("respiratory_irritation_num", "respiratory_irritation_num")] <- 0
    interm.df$ZCTA <- ZCTA.code
    unaffected_ZCTA_df <- rbind(unaffected_ZCTA_df, interm.df)
  }
  
  
  our_ZCTA_df <- rbind(affected_ZCTA_df,
                       unaffected_ZCTA_df)
  
  
  ## Matching the CITIES to ZIP CODES,
  our_CITY_df <- our_ZCTA_df %>% mutate(ZCTA = as.character(ZCTA)) %>%
    left_join(FL.ZCTA.county %>% select(ZCTA5CE10, primary_city), by=c("ZCTA" = "ZCTA5CE10"))
  
  
  dim(our_CITY_df)
  
  ## ALL CITIES
  all.cities <- sort(unique(our_CITY_df$primary_city))
  t(t(all.cities))
  
  
  
  
  ########
  ## Getting Red Tide data
  ########
  
  date.from <- as.Date("2018-05-01")
  date.to <- as.Date("2019-05-01")
  
  
  timeline_overall <- our_CITY_df %>%
    filter(created_at >= date.from & created_at <= date.to) %>%
    mutate(created_at = floor_date(created_at,  unit="1 day"))
  
  ## Making sure we have a common grid of time periods (with 1-day, 3-day, 7-day jumps)
  date.seq <- seq(date.from, date.to, by=time.window)
  new.dates <- NULL
  for (j in 1:length(timeline_overall$created_at)){
    new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - timeline_overall$created_at[j]))]))
  }
  new.dates <- as.Date(new.dates)
  
  timeline_overall$Date <- new.dates
  ###
  
  timeline_overall <- timeline_overall %>%
    mutate(primary_city = factor(primary_city, levels=all.cities)) %>%
    complete(primary_city, Date = date.seq, fill=list(respiratory_irritation_num=0)) %>%
    group_by(primary_city, Date) %>% 
    summarise(median.respiratory_irritation_num = median(respiratory_irritation_num),
              max.respiratory_irritation_num = max(respiratory_irritation_num),
              mean.respiratory_irritation_num = mean(respiratory_irritation_num),
              sum.respiratory_irritation_num = sum(respiratory_irritation_num),
              n.of.points = n()) %>%
    mutate(type = "1. Resp Irrit Levels",
           primary_city = factor(primary_city, levels=all.cities)) %>%
    ungroup()
  
  
  
  
  ##########
  ## Getting Tweet data
  ##########
  
  all.areas <- c("Pasco", "Tampa", "Clearwater", "StPete", "Bradenton", "Sarasota",
                 "TampaBay", "Pinellas")
  line.size <- 1.2
  
  full.df <- read_csv(twitter.data.file,
                      col_types = cols())
  
  full.df$created_at <- as.Date(full.df$created_at)
  
  
  full.df <- full.df %>% filter(geoprofile_match | places_match)
  
  
  
  #################
  ### Creating the MODELING DATA FRAME, piece-by-piece
  ##  First, initializing it with beach conditions data, 
  ##  then adding the tweet counts/sentiments
  #################
  
  
  model.df <- timeline_overall %>%
    select(primary_city, Date, mean.respiratory_irritation_num)
  
  tweet.types <- c("places", "geo")
  
  
  if (per.capita){
    ###########################
    ### PER-CAPITA TWEETS #####
    ###########################
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
        
        ## Adding the CITIES CLOSEST TO THIS ZCTA 
        ## (will double/triple-count some tweets when sharing a credit for a single ZCTA)
        our.df <- our.df %>% mutate(ZCTA = as.character(ZCTA)) %>%
          left_join(FL.ZCTA.county %>% select(ZCTA5CE10, primary_city), 
                    by=c("ZCTA" = "ZCTA5CE10"))
        
        
        timeline_interm <- our.df %>% filter(!is.na(created_at), created_at >= date.from, created_at <= date.to) %>%
          mutate(created_at = floor_date(created_at, unit="1 day"))
        
        new.dates <- NULL
        for (j in 1:length(timeline_interm$created_at)){
          new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - as.Date(timeline_interm$created_at)[j]))]))
        }
        new.dates <- as.Date(new.dates)
        timeline_interm$Date <- new.dates
        
        timeline_by_primary_city <- timeline_interm %>% 
          complete(primary_city, Date = date.seq) %>%
          group_by(primary_city, Date) %>% 
          summarise(tweet.count = ifelse(tweet.type=="weighted.geo", 
                                         sum(geo_weight, na.rm=T),
                                         n_distinct(id_str, na.rm=T))) %>%
          ungroup()
        
        
        ## Combining the extra credit due to TB/Pinellas, with the initial timeline by metro
        timeline_by_primary_city_ALL_INCLUDED <- timeline_by_primary_city %>%
          group_by(primary_city, Date) %>%
          summarise(tweet.count = sum(tweet.count)) %>%
          ungroup()
        
        
        # NORMALIZING BY THE "ELIGIBLE" POPULATION OF METRO AREA
        timeline_by_primary_city_ALL_INCLUDED_PerCapita <- 
          left_join(timeline_by_primary_city_ALL_INCLUDED,
                    city.metro.population.df %>%   # Left-joining with a City-Metro match data set.
                      full_join(FL.ZCTA.county %>% group_by(primary_city) %>% count(metro) %>% summarise(metro=names(which.max(table(metro)))),
                                by =c("city" = "primary_city")) %>%
                      select(city, metro.population) %>%
                      filter(city %in% all.cities),
                    by=c("primary_city" = "city")) %>%
          mutate(tweet.count = 100000*(tweet.count/metro.population)) %>%
          select(-metro.population)
        
        colnames(timeline_by_primary_city_ALL_INCLUDED)[colnames(timeline_by_primary_city_ALL_INCLUDED) == "tweet.count"] <- paste0("tweet.count.", tweet.type)
        colnames(timeline_by_primary_city_ALL_INCLUDED_PerCapita)[colnames(timeline_by_primary_city_ALL_INCLUDED_PerCapita) == "tweet.count"] <- paste0("tweet.count.", tweet.type)
        
        
        model.df <- model.df %>%
          left_join(timeline_by_primary_city_ALL_INCLUDED_PerCapita)
        
        
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
        
        
        ## Adding the CITIES CLOSEST TO THIS ZCTA 
        ## (will double/triple-count some tweets when sharing a credit for a single ZCTA)
        our.df <- our.df %>% mutate(ZCTA = as.character(ZCTA)) %>%
          left_join(FL.ZCTA.county %>% select(ZCTA5CE10, primary_city), 
                    by=c("ZCTA" = "ZCTA5CE10"))
        
        timeline_interm <- our.df %>% filter(!is.na(created_at), created_at >= date.from, created_at <= date.to) %>%
          mutate(created_at = floor_date(created_at, unit="1 day"))
        
        new.dates <- NULL
        for (j in 1:length(timeline_interm$created_at)){
          new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - as.Date(timeline_interm$created_at)[j]))]))
        }
        new.dates <- as.Date(new.dates)
        timeline_interm$Date <- new.dates
        
        timeline_by_primary_city <- timeline_interm %>% 
          complete(primary_city, Date = date.seq) %>%
          group_by(primary_city, Date) %>% 
          summarise(tweet.count = ifelse(sum(total_sentiment, na.rm=T) == "NaN", 0, sum(total_sentiment, na.rm=T))) %>%
          ungroup()
        
        
        ## Combining the extra credit due to TB/Pinellas, with the initial timeline by primary_city
        timeline_by_primary_city_ALL_INCLUDED <- timeline_by_primary_city %>%
          group_by(primary_city, Date) %>%
          summarise(tweet.count = sum(tweet.count)) %>%
          ungroup()
        
        
        # NORMALIZING BY THE "ELIGIBLE" POPULATION OF METRO AREA
        timeline_by_primary_city_ALL_INCLUDED_PerCapita <- 
          left_join(timeline_by_primary_city_ALL_INCLUDED,
                    city.metro.population.df %>%   # Left-joining with a City-Metro match data set.
                      full_join(FL.ZCTA.county %>% group_by(primary_city) %>% count(metro) %>% summarise(metro=names(which.max(table(metro)))),
                                by =c("city" = "primary_city")) %>%
                      select(city, metro.population) %>%
                      filter(city %in% all.cities),
                    by=c("primary_city" = "city")) %>%
          mutate(tweet.count = 100000*(tweet.count/metro.population)) %>%
          select(-metro.population)
        
        colnames(timeline_by_primary_city_ALL_INCLUDED)[colnames(timeline_by_primary_city_ALL_INCLUDED) == "tweet.count"] <- paste0("tweet.count.", tweet.type)
        colnames(timeline_by_primary_city_ALL_INCLUDED_PerCapita)[colnames(timeline_by_primary_city_ALL_INCLUDED_PerCapita) == "tweet.count"] <- paste0("tweet.count.", tweet.type)
        
        model.df <- model.df %>%
          left_join(timeline_by_primary_city_ALL_INCLUDED_PerCapita)
        
        
      }
      
      
    }
  }
  
  
  if (!per.capita){
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
        
        ## Adding the CITIES CLOSEST TO THIS ZCTA 
        ## (will double/triple-count some tweets when sharing a credit for a single ZCTA)
        our.df <- our.df %>% mutate(ZCTA = as.character(ZCTA)) %>%
          left_join(FL.ZCTA.county %>% select(ZCTA5CE10, primary_city), 
                    by=c("ZCTA" = "ZCTA5CE10"))
        
        
        timeline_interm <- our.df %>% filter(!is.na(created_at), created_at >= date.from, created_at <= date.to) %>%
          mutate(created_at = floor_date(created_at, unit="1 day"))
        
        new.dates <- NULL
        for (j in 1:length(timeline_interm$created_at)){
          new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - as.Date(timeline_interm$created_at)[j]))]))
        }
        new.dates <- as.Date(new.dates)
        timeline_interm$Date <- new.dates
        
        timeline_by_primary_city <- timeline_interm %>% 
          complete(primary_city, Date = date.seq) %>%
          group_by(primary_city, Date) %>% 
          summarise(tweet.count = ifelse(tweet.type=="weighted.geo", 
                                         sum(geo_weight, na.rm=T),
                                         n_distinct(id_str, na.rm=T))) %>%
          ungroup()
        
        
        ## Combining the extra credit due to TB/Pinellas, with the initial timeline by metro
        timeline_by_primary_city_ALL_INCLUDED <- timeline_by_primary_city %>%
          group_by(primary_city, Date) %>%
          summarise(tweet.count = sum(tweet.count)) %>%
          ungroup()
        
        colnames(timeline_by_primary_city_ALL_INCLUDED)[colnames(timeline_by_primary_city_ALL_INCLUDED) == "tweet.count"] <- paste0("tweet.count.", tweet.type)
        
        model.df <- model.df %>%
          left_join(timeline_by_primary_city_ALL_INCLUDED)
        
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
        
        
        ## Adding the CITIES CLOSEST TO THIS ZCTA 
        ## (will double/triple-count some tweets when sharing a credit for a single ZCTA)
        our.df <- our.df %>% mutate(ZCTA = as.character(ZCTA)) %>%
          left_join(FL.ZCTA.county %>% select(ZCTA5CE10, primary_city), 
                    by=c("ZCTA" = "ZCTA5CE10"))
        
        timeline_interm <- our.df %>% filter(!is.na(created_at), created_at >= date.from, created_at <= date.to) %>%
          mutate(created_at = floor_date(created_at, unit="1 day"))
        
        new.dates <- NULL
        for (j in 1:length(timeline_interm$created_at)){
          new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - as.Date(timeline_interm$created_at)[j]))]))
        }
        new.dates <- as.Date(new.dates)
        timeline_interm$Date <- new.dates
        
        timeline_by_primary_city <- timeline_interm %>% 
          complete(primary_city, Date = date.seq) %>%
          group_by(primary_city, Date) %>% 
          summarise(tweet.count = ifelse(sum(total_sentiment, na.rm=T) == "NaN", 0, sum(total_sentiment, na.rm=T))) %>%
          ungroup()
        
        
        ## Combining the extra credit due to TB/Pinellas, with the initial timeline by primary_city
        timeline_by_primary_city_ALL_INCLUDED <- timeline_by_primary_city %>%
          group_by(primary_city, Date) %>%
          summarise(tweet.count = sum(tweet.count)) %>%
          ungroup()
        
        colnames(timeline_by_primary_city_ALL_INCLUDED)[colnames(timeline_by_primary_city_ALL_INCLUDED) == "tweet.count"] <- paste0("tweet.count.", tweet.type)
        
        model.df <- model.df %>%
          left_join(timeline_by_primary_city_ALL_INCLUDED)
        
        
      }
    }
  }
  
  ## Making sure "NA" are treated as 0 (days with no tweet matches)
  model.df <- model.df %>%
    mutate_if(str_detect(colnames(model.df), "tweet.count"), function(x) ifelse(is.na(x),0,x))
  
  
  
  head(model.df)
  dim(model.df)
  
  
  ## Calculating total tweet counts for each primary_city
  tweet.count.cities <- model.df %>% 
    group_by(primary_city) %>%
    summarise(place.counts=sum(tweet.count.places),
              geo.counts=sum(tweet.count.geo)) 
  
  
  
  ### CONCURRENT BY-METRO correlations:
  concur.corr <- round(model.df %>% select(-primary_city, -Date) %>% cor(use="complete.obs") %>% .[1,],2) %>% .[c(2,3)]
  
  
  ## POST-FACTUM BY-METRO correlations (correlation of tweets with PREVIOUS TIME PERIOD's Resp Irrit Levels data)
  postfactum.corr <- model.df %>%
    group_by(primary_city) %>%
    mutate(mean.respiratory_irritation_num.lag = lag(mean.respiratory_irritation_num, n=1)) %>%
    ungroup() %>%
    select(-primary_city, -Date) %>%
    cor(use="complete.obs") %>% .[,"mean.respiratory_irritation_num.lag"] %>% round(2) %>% .[c(2,3)]
  
  
  ## ANTICIPATORY BY-METRO correlations (correlation of tweets with NEXT TIME PERIOD's Resp Irrit Levels data)
  anticip.corr <- model.df %>%
    group_by(primary_city) %>%
    mutate(mean.respiratory_irritation_num.lead= lead(mean.respiratory_irritation_num, n=1)) %>%
    ungroup() %>%
    select(-primary_city, -Date) %>%
    cor(use="complete.obs") %>% .[,"mean.respiratory_irritation_num.lead"] %>% round(2) %>% .[c(2,3)]
  
  
}


if (locality.lvl == "County"){
  
  ## Switching the "metro" names for county names.
  for (j in 1:length(all.metros)){
    our_metro_df$location[our_metro_df$location == all.metros[j]] <- all.counties.to.metros[j]
  }
  head(our_metro_df)
  
  
  
  date.from <- as.Date("2018-05-01")
  date.to <- as.Date("2019-05-01")
  
  ## Getting the summaries of Resp Irrit Levels over a year-long period.
  ## Breaking them up by metro areas.
  
  timeline_overall <- our_metro_df %>%
    filter(created_at >= date.from & created_at <= date.to) %>%
    mutate(created_at = floor_date(created_at,  unit="1 day"))
  
  ## Making sure we have a common grid of time periods (with 1-day, 3-day, 7-day jumps)
  date.seq <- seq(date.from, date.to, by=time.window)
  new.dates <- NULL
  for (j in 1:length(timeline_overall$created_at)){
    new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - timeline_overall$created_at[j]))]))
  }
  new.dates <- as.Date(new.dates)
  
  timeline_overall$Date <- new.dates
  ###
  
  timeline_overall <- timeline_overall %>%
    mutate(location = factor(location, levels=all.counties)) %>%
    #complete(location, Date, fill=list(respiratory_irritation_num=0)) %>%
    complete(location=all.counties, Date = date.seq, fill=list(respiratory_irritation_num=0)) %>%
    group_by(location, Date) %>% 
    summarise(median.respiratory_irritation_num = median(respiratory_irritation_num),
              max.respiratory_irritation_num = max(respiratory_irritation_num),
              mean.respiratory_irritation_num = mean(respiratory_irritation_num),
              sum.respiratory_irritation_num = sum(respiratory_irritation_num),
              n.of.points = n()) %>%
    mutate(type = "1. Resp Irrit Levels",
           location = factor(location, levels=all.counties)) %>%
    ungroup()
  
  
  
  ##########
  ## Getting Tweet data
  ##########
  
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
  ##  First, initializing it with beach conditions data, 
  ##  then adding the tweet counts/sentiments
  #################
  
  model.df <- timeline_overall %>%
    select(location, Date, mean.respiratory_irritation_num)
  
  tweet.types <- c("places",  "geo")
  
  
  if (per.capita){
    ###########################
    ### PER-CAPITA TWEETS #####
    ###########################
    
    if (metric == "Count"){
      ###################################
      ### FIRST, using TWEET COUNTS.
      ####################################
      
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
  
  
  
  ### CONCURRENT BY-METRO correlations:
  concur.corr <- round(model.df %>% select(-location, -Date) %>% cor(use="complete.obs") %>% .[1,],2) %>% .[c(2,3)]
  
  
  ## POST-FACTUM BY-METRO correlations (correlation of tweets with PREVIOUS TIME PERIOD's Resp Irrit Levels data)
  postfactum.corr <- model.df %>%
    group_by(location) %>%
    mutate(mean.respiratory_irritation_num.lag = lag(mean.respiratory_irritation_num, n=1)) %>%
    ungroup() %>%
    select(-location, -Date) %>%
    cor(use="complete.obs") %>% .[,"mean.respiratory_irritation_num.lag"] %>% round(2) %>% .[c(2,3)]
  
  
  ## ANTICIPATORY BY-METRO correlations (correlation of tweets with NEXT TIME PERIOD's Resp Irrit Levels data)
  anticip.corr <- model.df %>%
    group_by(location) %>%
    mutate(mean.respiratory_irritation_num.lead= lead(mean.respiratory_irritation_num, n=1)) %>%
    ungroup() %>%
    select(-location, -Date) %>%
    cor(use="complete.obs") %>% .[,"mean.respiratory_irritation_num.lead"] %>% round(2) %>% .[c(2,3)]
  
}


if (locality.lvl == "Total"){
  date.from <- as.Date("2018-05-01")
  date.to <- as.Date("2019-05-01")
  
  timeline_overall <- our_metro_df %>%
    filter(created_at >= date.from & created_at <= date.to) %>%
    mutate(created_at = floor_date(created_at,  unit="1 day"))
  
  ## Making sure we have a common grid of time periods (with 1-day, 3-day, 7-day jumps)
  date.seq <- seq(date.from, date.to, by=time.window)
  new.dates <- NULL
  for (j in 1:length(timeline_overall$created_at)){
    new.dates <- c(new.dates, as.character(date.seq[which.min(abs(date.seq - timeline_overall$created_at[j]))]))
  }
  new.dates <- as.Date(new.dates)
  
  timeline_overall$Date <- new.dates
  ###
  
  timeline_overall <- timeline_overall%>%
    complete(Date = date.seq, fill=list(respiratory_irritation_num=0)) %>%
    group_by(Date) %>% 
    summarise(median.respiratory_irritation_num = median(respiratory_irritation_num),
              max.respiratory_irritation_num = max(respiratory_irritation_num),
              mean.respiratory_irritation_num = mean(respiratory_irritation_num),
              sum.respiratory_irritation_num = sum(respiratory_irritation_num),
              n.of.points = n()) %>%
    mutate(type = "1. Resp Irrit Levels") %>%
    ungroup()
  
  
  
  ##########
  ## Getting Tweet data
  ##########
  
  all.areas <- c("Pasco", "Tampa", "Clearwater", "StPete", "Bradenton", "Sarasota",
                 "TampaBay", "Pinellas", "IMPRECISE_LINKS")
  line.size <- 1.2
  
  full.df <- read_csv(twitter.data.file,
                      col_types = cols())
  
  full.df$created_at <- as.Date(full.df$created_at)
  
  
  #################
  ### Creating the MODELING DATA FRAME, piece-by-piece
  ##  First, initializing it with beach conditions data, 
  ##  then adding the tweet counts/sentiments
  #################
  
  model.df <- timeline_overall %>%
    select(Date, mean.respiratory_irritation_num) 
  
  tweet.types <- c("places","geo")
  
  
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
  
  ## Making sure "NA" are treated as 0 (days with no tweet matches)
  model.df <- model.df %>%
    mutate_if(str_detect(colnames(model.df), "tweet.count"), function(x) ifelse(is.na(x),0,x))
  
  
  ### CONCURRENT correlations:
  concur.corr <- round(cor(model.df %>% select(-Date))[1,],2) %>% .[c(2,3)]
  
  ## POST-FACTUM correlations (correlation of tweets with PREVIOUS TIME PERIOD's Resp Irrit Levels data)
  postfactum.corr <- model.df %>%
    mutate(mean.respiratory_irritation_num.lag = lag(mean.respiratory_irritation_num, n=1)) %>%
    select(-Date) %>%
    cor(use="complete.obs") %>% .[,"mean.respiratory_irritation_num.lag"] %>% round(2) %>% .[c(2,3)]
  
  ## ANTICIPATORY correlations (correlation of tweets with NEXT TIME PERIOD's Resp Irrit Levels data)
  anticip.corr <- model.df %>%
    mutate(mean.respiratory_irritation_num.lead= lead(mean.respiratory_irritation_num, n=1)) %>%
    select(-Date) %>%
    cor(use="complete.obs") %>% .[,"mean.respiratory_irritation_num.lead"] %>% round(2) %>% .[c(2,3)]
  
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
