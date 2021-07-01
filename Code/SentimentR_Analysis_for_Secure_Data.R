# https://github.com/trinker/sentimentr

## 1. Working on the sentiment vocabulary for red tide situation
## (e.g. disposing of "bloom" as a positive word, things of that nature).
## 2. Applying sentiment scores to tweets, double-checking those.
###########

library(sentimentr)
library(readr)
library(tidyverse)

############
## Reading the tweet data

all.areas <- c("Pasco", "Tampa", "Clearwater", "StPete", "Bradenton", "Sarasota",
               "TampaBay", "Pinellas", "IMPRECISE_LINKS")


full.df <- NULL
for (area in all.areas){
  full.df <- rbind(full.df,
                   read.csv(paste("CLEANED_FOR_SENTIMENT_ANALYSIS_Full_", area, ".csv",sep="")))
  
  
}

dim(full.df)



## 2. Disposing of "bloom" as a "positive" word, and TONS OF OTHER CHANGES..
## "quality" had "+1", but rarely a positive thing... etc.

#####
## Updating the vocabulary of polarized sentiment words
#####

drop_terms_list <- unique(c("bloom", "bloomed", "yes", "yeah", "primary", "ahead", 
                            "quality", "worst", "pollution", "thanks", "thanksgiving",
                            "extend", "extends", "global", "new", "paradise", "accountable", 
                            "treatment", "feeling", "breaking", "honeymoon", "stronger",
                            "outreach", "found", "young", "coast", "purposeful", "including",
                            "combat", "combats", "major", "visit", "persistent", "confirmed",
                            "continue", "continuing", "partner", "substantial", "expert", 
                            "addresses", "director", "wish", "shark", "sharks",
                            "clean", "cleaning", "devastated",
                            "fucking", "fuckin", 'fucked', 'fucks',
                            'freaking',
                            "smell", "smelled", "smelling", "smells",
                            "bleed", "bleeding", "bleeds",
                            "beware", "empty",
                            "warned", "warning", "warnings", "warns",
                            "atrocious", "gorgeous", "inundate", "inundated",
                            "blast", "blasted", "blasts",
                            "endanger", "endangered", "joke",
                            "increase", "increased", "increases", "heighten", "treasure",
                            "please", "information", "well", "yes", "holy", "spent", "bait", 
                            "learn", "learning", "talk", "results", "sunset", "sunsets",
                            "received", "visit", "visitor", "management",
                            "share", "shared", "shares", "harvest", "art", "big",
                            "apt", "art", "ax", "big", "boy", "bs", "cad", "con", 
                            "cur", "din", "eat", "err", "fib", "fit", "hot", "hum", "ilu", 
                            "jam", "led", "mar", "maw", "nab", "nap", "new", "oaf", "pan", 
                            "pus", "ram", "sag", "sap", "sex", "shy", "sir", "spa", "sty", 
                            "sue", "top", "vex"))


updated_sentiment_df <- data.frame(matrix(c(
  'fishkill', -0.8, 'fishkills',-0.8, 'booed', -0.8,
  'increase', -0.4, 'increased', -0.4, 'increases', -0.4, 'increasing', -0.4,
  'heighten', -0.4, 'heightened', -0.4, 'heightens', -0.4, 'heightening', -0.4,
  'no joke', -0.3, 'what a joke', -0.5, 'holy cow', -0.3,
  'endanger',  -0.75, 'endangered',  -0.75, 'endangering',  -0.75, 'endangers',  -0.75,
  'blast', -0.75, 'blasted', -0.75, 'blasts', -0.75, 'blasting', -0.75,
  'had a blast', 1, 'having a blast', 1, 'full blast', 0,
  'warned', -0.75, 'warning', -0.75, 'warnings', -0.75, 'warns', -0.75,
  'oh no', -1, 'devastated', -1, 'boo', -1, 'atrocious', -1, 'atrocity', -1,
  'bleed', -1, 'bleeding', -1, 'bled', -1, 'gorgeous', 1,
  'empty', -0.7, 'darkening', -0.75, 'inundate', -0.7, 'inundated', -0.7,
  'beware', -0.8, 'no swimming', -0.8, 'no swim', -0.8, 'no beach', -0.8,
  'fucking', -1, 'fuckin', -1, 'fucked', -1, 'fucks', -1,
  'raising hands', 0.5, 'red heart', 0.4, 'blue heart', 0.4,
  'mitigate', 0.5, 'mitigating', 0.5, 'mitigated', 0.5,
  'freakin', -0.9, 'freaking', -0.9,
  'smell', -0.5, 'smelled', -0.5, 'smelling', -0.5, 'smells', -0.5,
  'clean', 0.3, 'clean up', 0,
  'deaths', -0.75,
  'high level', -1, 'high levels', -1, 'higher level', -0.85, 'higher levels', -0.85,
  'reduced level', 0.3, 'reduced levels', 0.3, 'reduced concentration', 0.3, 'reduced concentrations', 0.3,
  'high lvl', -1, 'high lvls', -1, 'higher lvl', -0.85, 'higher lvls', -0.85,
  'medium level', -0.75, 'medium levels', -0.75, 'medium lvl', -0.75, 'medium lvls', -0.75,
  'high concentration', -1, 'high concentrations', -1, 
  'higher concentration', -0.85, 'higher concentrations', -0.85,
  'medium concentration', -0.75, 'medium concentrations', -0.75,
  'algae',-0.2, 'algea', -0.2, 'algal', -0.2,
  'low level', -0.35, 'low levels', -0.35, 'lower level', -0.25, 'lower levels', -0.25,
  'low lvl', -0.35, 'low lvls', -0.35, 'lower lvl', -0.25, 'lower lvls', -0.25,
  'low concentration', -0.35, 'low concentrations', -0.35, 
  'lower concentration', -0.25, 'lower concentrations', -0.25,
  'affect', -0.75, 'affects', -0.75, 'affected', -0.75, 'affecting', -0.75,
  'impact', -0.75, 'impacts', -0.75, 'impacted', -0.75, 'impacting', -0.75,
  'effect', -0.75, 'effects', -0.75, 'brevis', -0.2, 'kbrevis', -0.2,
  'toxicity', -0.5, 'toxins', -0.5, 'shutdown', -0.5, 'worst', -1,
  'pollution', -0.3, 'pollutants', -0.3, 'polluter', -0.3, 'help', 0.3,
  'red tide rick', -0.3, 'pouting', -0.5, 'super bloom', -0.3, 
  'persist', -0.75, 'persists', -0.75, 'persisting', -0.75, 
  'persisted', -0.75, 'persistent', -0.75,
  'extend', -0.5, 'extends', -0.5, 'extended', -0.5, 'extending', -0.5,
  'global warming', -0.1, 'climate change', -0.1, 'medical mask', -0.5,
  'more red tide', -1,
  'no red tide', 1, 'red tide free', 1, 'no red tide smell', 1, 'red tide-free', 1,
  'free of red tide',1, 'free of the red tide', 1, 'clear of red tide', 1, 'clear of the red tide', 1,
  'red tide is gone', 1, 'red tide gone', 1,
  'new', 0.2, 'yikes', -0.7, 'paradise', 1,
  'research', 0.3, 'researcher', 0.3, 'researchers', 0.3,
  'open', 0.5, 'opens', 0.5, 'opened', 0.5, 'opening', 0.5,
  'accountable', -0.5, 'closed', -0.5, 'panic-inducing', -0.75, 'exposure', -0.5,
  'vote them', -0.4, 'stronger', -0.25, 'outreach', 0.15,
  'right now', 0, 'right here', 0, 'right off', 0, 'right away', 0, 'right there', 0,
  'right down', 0, 'right up', 0, 'right on', 0, 'right over', 0, 'right then', 0,
  'combat', 0.15, 'combating', 0.15,
  'volunteers needed', -0.25, 'need volunteers', -0.25,  'needs volunteers', -0.25,
  'calls on volunteers', -0.25, 'calling on volunteers', -0.25,  'volunteers', 0.2,
  'red tide detected', -0.75, 'red tide present', -0.75, 'red tide hits', -1, 'red tide hit', -1, 'red tide has hit', -1,
  'red tide reaches', -1, 'red tide reached', -1, 'red tide has reached', -1,
  'red tide returns', -0.75, 'red tides back', -0.75, 'red tide is back', -0.75,
  'there is red tide', -1, 'theres red tide', -1,
  'no sign of red tide', 1, 'no signs of red tide', 1,
  'no sign of the red tide', 1, 'no signs of the red tide', 1,
  'no obvious signs', 0.8, 'no apparent signs', 0.8,
  'dodge red tide', 1, 'dodging red tide', 1, 'dodged red tide', 1,
  'dodge the red tide', 1, 'dodging the red tide', 1, 'dodged the red tide', 1,
  'confirmed', -0.25, 'stay away', -0.5, 
  'smh', -0.5, 'forces', -0.5, 'forcing', -0.5, 'hell no', -0.6, 'brown', -0.2,
  'unbelievable experience', 0.75, 'yuck', -0.4, 'address', 0.4, 'addresses', 0.4, 
  'emergencies', 0.75, 'need help', -0.5, 'needing help', -0.5, 'go away', -0.4, 'walk away', -0.3,
  'wish', -0.25, 'we need', -1, 'need to', -0.5, 'need to know', 0,
  'strikes', -0.75, 'smells good', 1, 'smells great', 1,
  'clear condition', 1, 'clear conditions', 1, 
  'cant breathe', -1, 'cant breath', -1, 'hard to breath', -1, 'hard to breathe', -1, 
  'impossible to breathe', -1, 'impossible to breath', -1, 'cant even breathe', -1, 'couldnt even breathe', -1,
  'tough to breath', -1, 'tough to breathe', -1,
  'normal condition', 0.9, 'normal conditions', 0.9,
  'normal', 0.4, 'new normal',0, 'old normal', 0,
  'lingered', -0.4, 'hindered', -0.8, 'resurgence', -1,
  'asthma', -0.5, 'asthmatic', -0.5, 'asthmatics', -0.5,
  'stressor', -0.75, 'stressors', -0.75, 'chemicals', -0.3,
  'relax', 0.5, 'relaxing', 0.5, 'canceled', -0.8, 'stfu', -1,
  'red tide is still', -0.75, 'red tide still', -0.75,
  'finally hit', -1, 'no better place', 1, 'no better feeling', 1, 'tested positive', -1, 'tests positive', -1,
  'mad beach', 0, 'robinson preserve', 0, 'restrictions', -0.75), 
  byrow=T, ncol=2),
  stringsAsFactors = FALSE
)

colnames(updated_sentiment_df) <- c("words", "polarity")
updated_sentiment_df$polarity <- as.numeric(updated_sentiment_df$polarity)


updated_hash_sentiment <- sentimentr:::update_polarity_table(lexicon::hash_sentiment_jockers_rinker,
                                                             drop = drop_terms_list,
                                                             x = updated_sentiment_df)



View(lexicon::hash_sentiment_jockers_rinker[sapply(lexicon::hash_sentiment_jockers_rinker$x, str_length) <= 3])

#######
## Updating vocabulary of "valence shifters": negators (no, wasn't, etc) and
## words like "more", "less", "hardly", which affect strength of polarizers
#######
updated_valence_shifters_df <- data.frame(matrix(c(
  "minimal", 3, "minimum", 3, "minor", 3, 
  "maximal", 2, "maximum", 2, "major", 2, "significant", 2,
  "ain't no", 1, "no signs of", 1, "no sign of", 1, "free of", 1, "clear of", 1,
  "thousands", 2, "hundreds", 2, "tons", 2, "millions", 2, "billions", 2
), 
byrow=T, ncol=2),
stringsAsFactors = FALSE
)
updated_valence_shifters <- sentimentr:::update_valence_shifter_table(lexicon::hash_valence_shifters,
                                                                      x = updated_valence_shifters_df,
                                                                      comparison = updated_hash_sentiment)


updated_hash_sentiment[updated_hash_sentiment$x == "hi"]

updated_valence_shifters[updated_valence_shifters$y == 1]
full.df$tweet_full_contents[grepl("washed out", full.df$tweet_full_contents, ignore.case=T)]


###########
### Trying on the SAME SAMPLE OF 500 TWEETS:
###########

## Downweighing the questions, putting their sentiment at 0.25 instead of 1 (question.weight=0.25)
## E.g. tweets like "what is red tide? why is it killing fish? and can it make humans sick?"
##      originally colored as strongly negative, but given that the person is asking a question/wondering,
##      (instead of getting pissed off or actually stating something) => gotta soften the blow)


set.seed(1)
sample.tweets <- sample(full.df %>% filter(!is.na(tweet_urls)) %>% select(tweet_full_contents) %>% .[[1]], 50)

sentiment.df <- sentiment(sample.tweets, polarity_dt = updated_hash_sentiment, 
                          valence_shifters_dt = updated_valence_shifters,
                          question.weight = 0.25) %>%
  replace_na(list(word_count=0, sentiment=0))
#averaging.function = function(x) return(sum(x))) %>%


## Scores across multiple sentences will be calculated as a sheer sum of 
## each sentence's sentiment (NOT their average, as in default).
## Yet, we will have two ways of calculating each sentence's score:
##    1. Dividing total sentiment of a sentence by square root of words in that sentence
##       ("ave_sentiment", that is the DEFAULT).
##    2. Not doing that (hence, simply summing up the total sentiment of a sentence
##       which results of summing polarities of all polarized words, adjusted for valence shifters etc;
##       called it "total_sentiment)

sentiment.obj <- sentiment.df %>%
  group_by(element_id) %>%
  summarise(no_of_sentences=sum(!is.na(word_count)),
            ave_sentiment = round(sum(sentiment),2),
            total_sentiment = round(sum(sentiment*sqrt(word_count)),2))
sentiment.obj$tweet_full_contents <- sample.tweets




## Checking the tweets & their assigned sentiments
View(sentiment.obj %>% select(tweet_full_contents, ave_sentiment, total_sentiment))


## Looking at each sentence's estimates, if need to break it down/
## figure out which words might be the driving force
sentiment(sample.tweets, polarity_dt = updated_hash_sentiment) %>%
  filter(element_id == 8)
sample.tweets[45]

# Checking if a word is contained in polarized vocabulary, what score is assigned to it
updated_hash_sentiment[str_detect(updated_hash_sentiment$x,"shark")]
# Checking all tweets that contain a word/phrase, to get a feel of how its used in our tweets
full.df$tweet_full_contents[grepl("no better feeling", full.df$tweet_full_contents, ignore.case=T)]


## Playing with sentences, figuring out which words impact its score the most
sentiment("no better place",
          polarity_dt = updated_hash_sentiment,
          valence_shifters_dt = updated_valence_shifters)


####
## Trying to find "emotion/feeling" expression ONLY type of lexicon (no factual words, like "dead/death/kill")
## Can't really find such a thing..
####
lexicon::hash_sentiment_huliu[lexicon::hash_sentiment_huliu$x == "dead"]
lexicon::hash_sentiment_nrc[lexicon::hash_sentiment_nrc$x == "dead"]
lexicon::hash_sentiment_senticnet[lexicon::hash_sentiment_senticnet$x == "death"]
lexicon::hash_sentiment_sentiword[lexicon::hash_sentiment_sentiword$x == "happy"]


####
## Dealing with QUESTIONS
####
ind <- str_detect(sample.tweets, "\\?")
View(tibble(Tweets=sample.tweets[ind],
            Sum=sent.1[ind]))

sentiment(sample.tweets[ind], polarity_dt = updated_hash_sentiment,
          valence_shifters_dt = updated_valence_shifters,
          question.weight = 0.25) %>%
  filter(element_id == 34)




## Accounting for exaspiration via "..", "..."
##  -0.3 for those, make it "per-sentence" for the "ave_sentiment" (divide by square root of sentences)
##                  

View(sentiment.obj %>%
       mutate(no_of_periods = str_count(tweet_full_contents, "\\.\\.(\\.)*")) %>%
       mutate(ave_sentiment = round(ave_sentiment - 0.15*no_of_periods/sqrt(no_of_sentences),2),
              total_sentiment = round(total_sentiment - 0.15*no_of_periods,2)) %>%
       select(tweet_full_contents, ave_sentiment, total_sentiment, no_of_periods))


##########################################################################
##########################################################################
## Getting the sentiment values for the full data set   ##################
##########################################################################
##########################################################################

full.sentiment.df <- sentiment(full.df$tweet_full_contents, polarity_dt = updated_hash_sentiment, 
                               valence_shifters_dt = updated_valence_shifters,
                               question.weight = 0.25) %>%
  replace_na(list(word_count=0, sentiment=0))


full.sentiment.obj <- full.sentiment.df %>%
  group_by(element_id) %>%
  summarise(no_of_sentences=sum(!is.na(word_count)),
            ave_sentiment = round(sum(sentiment),2),
            total_sentiment = round(sum(sentiment*sqrt(word_count)),2))
full.sentiment.obj$tweet_full_contents <- full.df$tweet_full_contents

full.sentiment.obj.w.periods <- full.sentiment.obj %>%
  mutate(no_of_periods = str_count(tweet_full_contents, "\\.\\.(\\.)*")) %>%
  mutate(ave_sentiment = round(ave_sentiment - 0.15*no_of_periods/sqrt(no_of_sentences),2),
         total_sentiment = round(total_sentiment - 0.15*no_of_periods,2)) %>%
  select(tweet_full_contents, ave_sentiment, total_sentiment)

full.df$ave_sentiment <- full.sentiment.obj.w.periods$ave_sentiment
full.df$total_sentiment <- full.sentiment.obj.w.periods$total_sentiment

View(full.df %>% filter(str_detect(tweet_full_contents, 
                                   "will be updated readings today, but here")) %>%
       select(tweet_full_contents, total_sentiment))


sentiment("
red tide lingers. 
there will be updated readings today, 
but here is the most recent information from pinellas county environmental management 
(describing concentrations) and visit st. pete/clearwater (describing...

",
          polarity_dt = updated_hash_sentiment, 
          valence_shifters_dt = updated_valence_shifters,
          question.weight = 0.25)



#########
## Saving the results
#########

all.locations <- unique(full.df$location)

for (loc in all.locations){
  write.csv(full.df %>% 
              mutate(created_at = as.character(created_at),
                     retweeted_status.created_at = as.character(retweeted_status.created_at)) %>%
              filter(location == loc),
            file=paste("WITH_SENTIMENT_SCORES_Full_", loc, ".csv",sep=""),
            row.names=F)
}
