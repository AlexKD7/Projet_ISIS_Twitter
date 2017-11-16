library(ggplot2)
install.packages("httr")
install.packages("magrittr")
#install.packages("ggplot2")
library(plyr)
library(purrr)
#install.packages("plyr")
#install.packages("purrr")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("SnowballC")u
#install.packages("wordcloud")
#install.packages("jsonlite")
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("twitteR")
library(twitteR)
library(jsonlite)
library(dplyr)
library(stringr)
library(tm)
library(data.table)
library(dplyr)
library(stringi)
#install.packages("XML")
#install.packages("rlist")
library(rlist)
#install.packages("tm") 
setwd("/home/amstelsus/Documents/Projet Twitter Isis/tweets_djihad/")
getwd()
list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                      full.names=FALSE, ignore.case=FALSE) {
  # use full.names=TRUE to pass to file.info
  all <- list.files(path, pattern, all.dirs,
                    full.names=TRUE, recursive=FALSE, ignore.case)
  dirs <- all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
  if(isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}
dirs=list.dirs("/home/amstelsus/Documents/Projet Twitter Isis/tweets_djihad/")
length(dirs)
for(i in 1:length(dirs))
{
directory=paste("/home/amstelsus/Documents/Projet Twitter Isis/tweets_djihad/",dirs[i],sep="")
print(directory)
temp <- list.files(directory,
                   pattern="ID-centai-tablette-*", full.names=TRUE)
if(i==1)
{
myJSON <-jsonlite::fromJ
}
else
{
    temporaryList <- lapply(temp, function(x) 
    {
      is.na(x) = is.null(x)
      jsonlite::fromJSON(x)
    }) 
  myJSON=c(myJSON,temporaryList)
}
}
head(myJSON)
library(dplyr)
library(tidyr)

library(magrittr)
df <- data.frame(a=colonnesUtiles, b=names(colonnesUtiles)) %>% mutate(key=cumsum(b=="experience.duration")) %>% 
  split(.$key) %>% lapply(function(x) x %>% select(-key) %>% spread(b, a)) %>% 
  do.call(rbind, .) %>% t %>% data.frame

df$key <- rownames(df)
unlistedData=unlist(sapply(myJSON,simplify = TRUE,USE.NAMES = TRUE, function(x) unlist(use.names = T,lapply(x, function(x) ifelse(is.null(x),NA,x)))))
unlistedData=unlist(myJSON)
colonnesUtiles=unlistedData[names(unlistedData)==
               c("text",
                 "user.time_zone",
                 "user.name",
                 "possibly_sensitive",
                 "user.location",
                 "user.description",
                 "user.created_at",
                 "user.lang",
                 "user.time_zone",
                 "user.listed_count",
                 "user.friends_count",
                 "user.statuses_count",
                 "user.followers_count")]
do.call(rbind, lapply(myJSON, function(x)as.data.frame(x)))

dfessai=data.frame
colonnesUtilesF=jsonlite::flatten(colonnesUtiles)
lapply(myJSON,function(x)
  {
  dfessai=cbind(dfessai,x)
  })
dfessai
myJSON
install.packages("gtools")
library(gtools)
do.call("smartbind",myJSON)
listColonnesUtiles=as.list(colonnesUtiles)
listColonnesUtiles$possibly_sensitive
unique(names(unlistedData[is.na(unlistedData)]))
noms=unique(names(unlistedData))
class(colonnesUtiles)
noms
summary(myJSON)
dfessai=as.data.frame(colonnesUtiles)
head(dfessai)
unique(names(myJSON))
unique(names(unlistedData))
columns=unique(names(unlistedData))
essai
columns
place
dfTweets=data.frame(unlistedData[names(unlistedData)=="text"],check.names = T)
dfTweets=cbind(dfTweets,unlistedData,unlistedData[names(unlistedData)=="place.country"])
names(dfTweets)
dfTweets

unlis
tweets[,"user"]
head(dfTweets)
names(dfTweets)
texts$text = gsub("[[:punct:]]", "", texts$text)
texts$text = gsub("[[:digit:]]", "", texts$text)
texts$text = gsub("http\\w+", "", texts$text)
texts$text = gsub("[ \t]{2,}", "", texts$text)
texts$text = gsub("^\\s+|\\s+$", "", texts$text)
texts$text = gsub("\n", " ", texts$text)
texts
dfTweets
tweets$entities
class(tweets)
str(tweets$entities)
hashtags
hashtag.regex <- regex("(?<=^|\\s)#\\S+")
hashtags <- str_extract_all(tweets$text, hashtag.regex)
tweets_tags=cbind(texts$text,hashtags)
tweets_tags
summary(fusion)
