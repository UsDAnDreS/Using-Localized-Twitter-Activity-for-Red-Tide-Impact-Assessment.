# https://github.com/trinker/sentimentr

##########
## Cleaning up the tweets, predominantly taking care of hashtags
## (breaking them up into separate words, 
##  making sure that trailing hashtags at the end of the tweet are treated as
##  separate messages)
## Also disposing of links, ampersands & things of that nature.
## Making all tweets lowercase.
###########

library(sentimentr)
library(readr)
library(tidyverse)

############
## Reading the tweet data
## (from wherever you put the "Secure_Tweets_Data.csv" file)
full.df <- read.csv(paste0(getwd(),"/Florida-Red-Tide-Event/Twitter_Scraping/Full Archive Premium/Secure_Tweets_Data.csv"))
dim(full.df)

##############
## All Matches (full.df)
#############

# full.df <- metro.df %>% filter(places_match | geoprofile_match)
# TB.Pinellas.geo.df <- TB.Pinellas.df %>% filter(places_match | geoprofile_match)


## Pre-Processing:

## 0. Disposing of links ("\\S" means ANY NON-SPACE CHARACTER), 
##    amps etc
##    @-mentions
##    Making all "multi-spaces" into just one space (e.g. "#ht1    #ht2" into "#ht1 #ht2")
##    Disposing of spaces at the end of the tweet.
full.df$tweet_full_contents <- gsub("http\\S*","",  full.df$tweet_full_contents)
full.df$tweet_full_contents <- gsub("https\\S*","", full.df$tweet_full_contents)
full.df$tweet_full_contents <- gsub("@\\S*","", full.df$tweet_full_contents)
full.df$tweet_full_contents <- gsub("&amp;","&", full.df$tweet_full_contents)
full.df$tweet_full_contents <- gsub("&gt;",">", full.df$tweet_full_contents)
full.df$tweet_full_contents <- gsub("&lt;","<", full.df$tweet_full_contents)
full.df$tweet_full_contents <- gsub("’", "'", full.df$tweet_full_contents)
full.df$tweet_full_contents <- str_replace_all(full.df$tweet_full_contents,"\\s+", " ")
full.df$tweet_full_contents <- str_replace_all(full.df$tweet_full_contents," $", "")
full.df$tweet_full_contents <- str_replace_all(full.df$tweet_full_contents,"&", " and ")

## 0. Adding the urls into it, as they have some useful info in them 
## (e.g. "https://www.baynews9/com/fl/tampa/news/2018/10/20/red-tide-keeps-older-gulfport-residents-indoors")

full.df$tweet_urls_edited <- full.df$tweet_urls
full.df$tweet_urls_edited <- gsub("https","",  full.df$tweet_urls_edited)
full.df$tweet_urls_edited <- gsub("http","",  full.df$tweet_urls_edited)
full.df$tweet_urls_edited <- gsub("://",". ",  full.df$tweet_urls_edited)
full.df$tweet_urls_edited <- gsub("\\.","",  full.df$tweet_urls_edited)
full.df$tweet_urls_edited <- gsub("/",". ",  full.df$tweet_urls_edited)
full.df$tweet_urls_edited <- gsub("\\?\\S*",". ",  full.df$tweet_urls_edited)
full.df$tweet_urls_edited <- gsub("-"," ",  full.df$tweet_urls_edited)
full.df$tweet_urls_edited <- gsub("_"," ",  full.df$tweet_urls_edited)
full.df$tweet_urls_edited <- ifelse(is.na(full.df$tweet_urls_edited ), "", full.df$tweet_urls_edited)
full.df$tweet_full_contents <- paste(full.df$tweet_full_contents, full.df$tweet_urls_edited)
full.df$tweet_urls_edited <- NULL
#View(full.df$tweet_urls_edited)


## 1. Replacing emojis with corresponding terms (e.g. heart emoji with the word "heart", etc)
full.df$tweet_full_contents <- replace_emoji(full.df$tweet_full_contents)

## 2. Make the TRAILING HASHTAGS at the END OF A TWEET into SENTENCES
##   Hence,
##    a. Recognize the sequence of trailing hashtags at the end of the tweet.
##    b. Replace "#"'s with "."'s for THESE HASHTAGS ONLY.

ind.logic <- sapply(str_extract_all(full.df$tweet_full_contents, "(\\#([A-Za-z0-9]+ ))*\\#([A-Za-z0-9]+)$"), length)>0
str.to.replace <- sapply(str_extract_all(full.df$tweet_full_contents, "(\\#([A-Za-z0-9]+ ))*\\#([A-Za-z0-9]+)$")[ind.logic], function(x) paste(x, collapse=" "))
str.to.replace.regex <- str_replace_all(str.to.replace, "\\#", "\\\\#")
replacement.strings <- str_replace_all(str.to.replace, "\\#", "\\. \\#")

full.df$tweet_full_contents[ind.logic] <- sapply(1:sum(ind.logic), 
                                                 function(x) str_replace(full.df$tweet_full_contents[which(ind.logic)[x]],
                                                                         str.to.replace.regex[x],
                                                                         replacement.strings[x]))

full.df$tweet_full_contents <- str_replace_all(full.df$tweet_full_contents,"\\. \\.", "\\.")


## 3. Breaking hashtags apart into individual words.

## MORE ON REG EXPRESSIONS when DOING THE REPLACEMENT OF A MATCHED SUBSTRING!!!
## https://www.regular-expressions.info/rlanguage.html#:~:text=Use%20gsub%20instead%20of%20sub,its%20own%20replacement%20string%20syntax.&text=There%20is%20no%20option%20to%20use%20the%20PCRE2%20replacement%20string%20syntax.

#all.hashtags <- unique(unlist(str_extract_all(full.df$tweet_full_contents, "#[A-Za-z0-9]+")))
all.hashtags <- unique(unlist(str_extract_all(full.df$tweet_full_contents, "#[\\w]+")))
all.hashtags <- str_replace(all.hashtags, "#", "")

## a) First, we find hashtags where each word starts with a capital letter ("DeadFish, "AlgaeBloom"), 
##    followed by at least one lower-case letter,
##    and break them by capital letter (that's easy).
all.hashtags <- as.list(all.hashtags)
ind.logic <- sapply(str_extract_all(all.hashtags, "^[A-Z][a-z]+(_)*([A-Z][a-z]+(_)*)+"), length)>0
all.hashtags.replacers <- sapply(str_extract_all(all.hashtags, "[A-Z][a-z]+")[ind.logic], function(x) paste(x, collapse=" "))
View(data.frame(unlist(all.hashtags)[ind.logic],
                all.hashtags.replacers))

######
## Replacing those into the actual tweets
######
ind <- str_detect(full.df$tweet_full_contents, 
                  str_c(paste0("#", unlist(all.hashtags[ind.logic])), "\\b", collapse="|"))
#str_c("\\b", unlist(all.hashtags)[-ind.drop], "\\b", collapse="|"))
View(full.df$tweet_full_contents[ind])

full.df$tweet_full_contents <- 
  str_replace_all(full.df$tweet_full_contents, 
                  str_c("#", unlist(all.hashtags[ind.logic]), "\\b", collapse="|"),
                  function(x){
                    return(all.hashtags.replacers[which(paste0("#",unlist(all.hashtags[ind.logic])) == x)])
                  })




## b) Second, from remaining hashtags AND the FULL TWEETS, we convert EVERYTHING into lowercase.
full.df$tweet_full_contents <- tolower(full.df$tweet_full_contents)
all.hashtags.lower <- unique(sapply(all.hashtags[!ind.logic],
                                    tolower))
all.hashtags.replacers.lower <- all.hashtags.lower

## c) Third, we try our best to break apart the lower-case glued hashtags into individual words.
##    That process includes:
##      - Creating a word list (of all possible English words)
##      - Running it through a parser that detects the longest opening English word in the hashtag string,
##        records it, strips it off the hashtag, rinses and repeats the process.

####
## Making a word list
####

wordlist <- read_csv("http://www-personal.umich.edu/~jlawler/wordlist", 
                     col_names = FALSE)
wordlist <- unique(c(wordlist[[1]]))
wordlist <- unique(c(wordlist, 
                     #unlist(lapply(all.terms, function(x) strsplit(tolower(x),split=" "))), 
                     "pinellas", "rotonda", "insta", "repost", "gators", "turtles", "algae", "algea", 
                     "gulf", "snorkling", "dont", "fracking", "kbrevis", "karenia", "brevis", "fest", 
                     "ruining", "tweet", "twitter", "tweeting", "exam", "exams", "chasing", "celebrities"))


# write.csv(wordlist,
#           file=paste0(getwd(),"/Florida-Red-Tide-Event/Twitter_Scraping/Full Archive Premium/Hashtag_Break_Apart_Words.csv"),
#           row.names = F)


## Finding the longest initial sequence that matches a word in our wordlist
find_word <- function(term, wordlist){
  i <- nchar(term) + 1
  while (i > 1){
    i <- i-1
    if (substr(term,1,i) %in% wordlist) return(substr(term,1,i))
  }
  return("None")
}

## Breaking down glued hashtags into separate words
parse_tag <- function(term, wordlist){
  term <- str_replace_all(str_replace_all(term, "\\d", ""), "_","")
  words <- NULL
  word <- find_word(term, wordlist) 
  #print(word)
  while ((word != "None") & (length(term) > 0)){
    words <- c(words, word)            
    if (nchar(term) == nchar(word)) # Special case for when eating rest of word
      break;
    term = substr(term,nchar(word)+1,nchar(term))
    #print(term)
    word = find_word(term, wordlist)
  }
  
  return(paste(words, collapse=" "))
}


all.hashtags.replacers.lower <- sapply(all.hashtags.lower,      
                                       function(x) parse_tag(x, wordlist))

## Checking the hashtags and their "broken down" version
View(data.frame(all.hashtags.lower,
                all.hashtags.replacers.lower))





######
## Replacing those into the actual tweets
######
ind <- str_detect(full.df$tweet_full_contents, 
                  str_c(paste0("#",all.hashtags.lower), "\\b", collapse="|"))
#str_c("\\b", unlist(all.hashtags)[-ind.drop], "\\b", collapse="|"))
View(full.df$tweet_full_contents[ind])

#full.df$tweet_full_contents <- 
interm.tweet_full_contents <-
  str_replace_all(full.df$tweet_full_contents, 
                  str_c("#", all.hashtags.lower, "\\b", collapse="|"),
                  function(x){
                    return(all.hashtags.replacers.lower[which(paste0("#",all.hashtags.lower) == x)])
                  })
View(data.frame(full.df$tweet_full_contents[ind],
                interm.tweet_full_contents[ind]))


full.df$tweet_full_contents <- 
  str_replace_all(full.df$tweet_full_contents, 
                  str_c("#", all.hashtags.lower, "\\b", collapse="|"),
                  function(x){
                    return(all.hashtags.replacers.lower[which(paste0("#",all.hashtags.lower) == x)])
                  })


#########
## Saving the results
#########

all.locations <- unique(full.df$location)

## Provide the directory/file locations according to where you want them saves
for (loc in all.locations){
  write.csv(full.df %>% 
              mutate(created_at = as.character(created_at),
                     retweeted_status.created_at = as.character(retweeted_status.created_at)) %>%
              filter(location == loc), 
            file=paste("CLEANED_FOR_SENTIMENT_ANALYSIS_Full_", loc, ".csv",sep=""),
            row.names=F)
}


#  View(full.df %>% filter(!is.na(tweet_urls)) %>% select(tweet_full_contents))
