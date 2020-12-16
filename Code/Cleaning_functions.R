## This function add all variations of terms that can embody a phrase.
## E.g. if one of "terms" is "Red Tide Rick", this function will also add
##  "RedTideRick", "Red-Tide-Rick"(if dash=T), allowing to match hashtags (#RedTideRick)
## and links (https://tampa.com/red-tide-rick)
hashtag.fun <- function(terms, dash=F){
  hashtag.terms <- NULL
  for (i in terms) if (grepl(" ", i) | grepl("-",i)) hashtag.terms <- c(hashtag.terms,gsub("-","",gsub(" ", "", i)))
  all.terms <- unique(c(terms, hashtag.terms))
  if (dash) all.terms <- unique(c(all.terms, gsub(" ", "-", all.terms)))
  return(all.terms)
}


##########
## This function checks for tweets that contain certain "bad.terms"
## (terms that usually indicate concepts not related either to the phenomena of red tide, 
##  or the metro area of interest)
## and further processes them to see if there are any other parts of the tweet
## that are pertinent to our search.
##
## For example:
##   A lot of tweets that contain "Red Tide Rick" are matched, but they mostly talk about 
##  politics rather than the actual red tide (e.g. "Don't vote for Red Tide Rick! Go Blue!")
##  But, at the same time, if tweet has "Red Tide Rick" reference, it can still have
## a part of it that references the actual red tide, e.g.
##  "Red Tide Rick couldn't do a thing about red tide, in fact he contributed to it by not doing X-Y-X blah blah blah"
##
##  This function allows us to see that second type of tweets as pertinent, and not simply 
##  dispose of every single tweet mentioning "Red Tide Rick".
#########

postprocess.badterm.cleanup <- function(bad.terms, grep.terms=NULL, agrep.terms=NULL){
  all.terms <- c(grep.terms, agrep.terms)
  ind <- sapply(result.df$tweet_full_contents, function(x) any(sapply(bad.terms, function(y) grepl(y,x,ignore.case = T)))) |
    sapply(result.df$tweet_urls, function(x) any(sapply(bad.terms, function(y) grepl(y,x,ignore.case = T))))
  actual.inds <- which(ind)
  
  if (length(actual.inds) == 0) return(NULL)
  
  cleaned.up.tweet_full_contents <- result.df$tweet_full_contents
  cleaned.up.tweet_urls <- result.df$tweet_urls
  for (bad.term in bad.terms){
    cleaned.up.tweet_full_contents <- gsub(bad.term, "", cleaned.up.tweet_full_contents, ignore.case=T)
    cleaned.up.tweet_urls <- gsub(bad.term, "", cleaned.up.tweet_urls, ignore.case=T)
  }
  
  if (is.null(agrep.terms)) ind.agrep <- rep(FALSE, length(actual.inds))
  if (!is.null(agrep.terms)){
    ind.agrep <- sapply(cleaned.up.tweet_full_contents[actual.inds], 
                        function(x) any(sapply(agrep.terms, function(y) agrepl(y,x,ignore.case = T, max.distance=0.05))))
    ind.agrep <- ind.agrep | sapply(cleaned.up.tweet_urls[actual.inds], 
                                    function(x) any(sapply(agrep.terms, function(y) agrepl(y,x,ignore.case = T, max.distance=0.05))))
  }
  
  if (is.null(grep.terms)) ind.grep <- rep(FALSE, length(actual.inds))
  if (!is.null(grep.terms)){
    ind.grep <- sapply(cleaned.up.tweet_full_contents[actual.inds], 
                       function(x) any(sapply(grep.terms, function(y) grepl(y,x,ignore.case = T))))
    ind.grep <- ind.grep | sapply(cleaned.up.tweet_urls[actual.inds], 
                                  function(x) any(sapply(grep.terms, function(y) grepl(y,x,ignore.case = T))))
  }
  
  ind.drop <- !(ind.agrep | ind.grep)
  actual.inds[ind.drop]
  
  return(list(actual.inds=actual.inds,
              ind.drop=ind.drop))
}


postprocess.badterm.cleanup.arbitrary.input <- function(input, bad.terms, grep.terms=NULL, agrep.terms=NULL){
  all.terms <- c(grep.terms, agrep.terms)
  ind <- sapply(input, function(x) any(sapply(bad.terms, function(y) grepl(y,x,ignore.case = T)))) 
  actual.inds <- which(ind)
  
  if (length(actual.inds) == 0) return(NULL)
  
  cleaned.up.input <- input
  for (bad.term in bad.terms)  cleaned.up.input <- gsub(bad.term, "", cleaned.up.input, ignore.case=T)

  
  if (is.null(agrep.terms)) ind.agrep <- rep(FALSE, length(actual.inds))
  if (!is.null(agrep.terms)){
    ind.agrep <- sapply(cleaned.up.input[actual.inds], 
                        function(x) any(sapply(agrep.terms, function(y) agrepl(y,x,ignore.case = T, max.distance=0.05))))
    ind.agrep <- ind.agrep | sapply(cleaned.up.input[actual.inds], 
                                    function(x) any(sapply(agrep.terms, function(y) agrepl(y,x,ignore.case = T, max.distance=0.05))))
  }
  
  if (is.null(grep.terms)) ind.grep <- rep(FALSE, length(actual.inds))
  if (!is.null(grep.terms)){
    ind.grep <- sapply(cleaned.up.input[actual.inds], 
                       function(x) any(sapply(grep.terms, function(y) grepl(y,x,ignore.case = T))))
    ind.grep <- ind.grep | sapply(cleaned.up.input[actual.inds], 
                                  function(x) any(sapply(grep.terms, function(y) grepl(y,x,ignore.case = T))))
  }
  
  ind.drop <- !(ind.agrep | ind.grep)
  actual.inds[ind.drop]
  
  return(list(actual.inds=actual.inds,
              ind.drop=ind.drop))
}



###################
## Distributing credit for "Tampa Bay" and "Pinellas" tweets
###################

TB.Pinellas.credit.function <- function(county.population.file = "/home/andrey/County_Population_Data.csv"){
                                        #                                         stpete.population.file,
                                        #                                         county.population.file,
                                        #                                         all.metros = c("Pasco", "Tampa", "Clearwater", "StPete", "Bradenton", "Sarasota"),
                                        #                                         counties = c("Pasco", "Hillsborough", "Pinellas", "Pinellas", "Manatee", "Sarasota")){
                                        
county.population <- read.csv(county.population.file)
all.counties <- county.population$county
eligible.population <- county.population$population

TampaBay.credit <- numeric(length(all.counties))
ind <- all.counties %in% c("Hillsborough", "Pinellas")
TampaBay.credit[ind] <- prop.table(eligible.population[ind])
TampaBay.credit[!ind] <- 0


Pinellas.credit <- numeric(length(all.counties))
ind <- all.counties %in% c("Pinellas")
Pinellas.credit[ind] <- prop.table(eligible.population[ind])


TB.Pinellas.credit.df <- data.frame(location =all.counties, 
                                    TB.credit = TampaBay.credit,
                                    Pinellas.credit = Pinellas.credit,
                                    Eligible.population = eligible.population)

return(TB.Pinellas.credit.df)
}


# TB.Pinellas.credit.function <- function(clearwater.population.file,
#                                         stpete.population.file,
#                                         county.population.file,
#                                         all.metros = c("Pasco", "Tampa", "Clearwater", "StPete", "Bradenton", "Sarasota"),
#                                         counties = c("Pasco", "Hillsborough", "Pinellas", "Pinellas", "Manatee", "Sarasota")){
#   eligible.population <- numeric(length(all.metros))
#   for (i in 1:length(all.metros)){
#     metro <- all.metros[i]
#     if (metro %in% c("Clearwater", "StPete")){
#       pop.dat <- read_csv(ifelse(metro == "Clearwater",
#                                  clearwater.population.file,
#                                  stpete.population.file),
#                           na=c("","NA","X"), col_types = cols())
#       head(pop.dat)
#       pop.df <- data.frame(t(pop.dat[,-1]))
#       colnames(pop.df) <- pop.dat[,1][[1]]
#       pop.df$`Popul. 2019` <- ifelse(is.na(pop.df$`Popul. 2019`), 
#                                      mean(pop.df$`Popul. 2019`/pop.df$`Popul. 2010`, na.rm=T)*pop.df$`Popul. 2010`,
#                                      pop.df$`Popul. 2019`)
#       eligible.population[i] <- sum(pop.df %>% mutate(Eligible.Population = round((100 - (`Under 18yo, %` + `Under 5yo, %`)/2 - `Over 65yo, %`)*`Popul. 2019`/100)) %>% 
#                                       select(Eligible.Population) %>% 
#                                       .[[1]])
#     }
#     
#     if (!(metro %in% c("Clearwater", "StPete"))){
#       pop.dat <- read_csv(county.population.file,
#                           na=c("","NA","X"), col_types = cols())
#       pop.df <- data.frame(t(pop.dat[,-1]))
#       colnames(pop.df) <- pop.dat[,1][[1]]
#       eligible.population[i] <- pop.df %>% filter(grepl(counties[i],rownames(pop.df))) %>% 
#         mutate(Eligible.Population = round((100 - (`Under 18yo, %` + `Under 5yo, %`)/2 - `Over 65yo, %`)*`Popul. 2019`/100 )) %>% 
#         select(Eligible.Population) %>% 
#         .[[1]]
#     }
#   }
#   
#   ## Give respective proportions of the remaining Pinellas population to each of Clearwater & StPete
#   ## (E.g. 100%x(eligible Clearwater/eligible Pinellas) x eligible pinellas  => clearwater;)
#   pop.dat <- read_csv(county.population.file,
#                       na=c("","NA","X"), col_types = cols())
#   pop.df <- data.frame(t(pop.dat[,-1]))
#   colnames(pop.df) <- pop.dat[,1][[1]]
#   Pinellas.eligible.pop <- pop.df %>% filter(grepl("Pinellas",rownames(pop.df))) %>% 
#     mutate(Eligible.Population = round((100 - (`Under 18yo, %` + `Under 5yo, %`)/2 - `Over 65yo, %`)*`Popul. 2019`/100 )) %>% 
#     select(Eligible.Population) %>% 
#     .[[1]]
#   
#   ind <- all.metros %in% c("Clearwater", "StPete")
#   remaining.Pinellas.eligible.pop <- Pinellas.eligible.pop - sum(eligible.population[ind])
#   eligible.population[ind] <- round(eligible.population[ind] + prop.table(eligible.population[ind])*remaining.Pinellas.eligible.pop)
#   
#   TampaBay.credit <- numeric(length(all.metros))
#   ind <- all.metros %in% c("Tampa", "Clearwater", "StPete")
#   TampaBay.credit[ind] <- prop.table(eligible.population[ind])
#   TampaBay.credit[!ind] <- 0
#   TampaBay.credit
#   
#   
#   Pinellas.credit <- numeric(length(all.metros))
#   ind <- all.metros %in% c("Clearwater", "StPete")
#   Pinellas.credit[ind] <- prop.table(eligible.population[ind])
#   Pinellas.credit[!ind]
#   Pinellas.credit
#   
#   
#   TB.Pinellas.credit.df <- data.frame(location =all.metros, 
#                                       TB.credit = TampaBay.credit,
#                                       Pinellas.credit = Pinellas.credit,
#                                       Eligible.population = eligible.population)
#   return(TB.Pinellas.credit.df)
# }








