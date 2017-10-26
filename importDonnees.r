library(ggplot2)
#install.packages("ggplot2")
library(plyr)
#install.packages("plyr")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("SnowballC")
#install.packages("wordcloud")
#install.packages("jsonlite")
#install.packages("stringr")
#install.packages("dplyr")
library(jsonlite)
library(dplyr)
library(stringr)
library(tm)
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
for(i in 1:2)
{
directory=paste("/home/amstelsus/Documents/Projet Twitter Isis/tweets_djihad/",dirs[i],sep="")
print(directory)
temp <- list.files(directory,
                   pattern="ID-centai-tablette-*", full.names=TRUE)
if(i==1)
{
tweets <- purrr::map_df(temp, function(x) { 
  purrr::map(jsonlite::stream_in(file(x)), function(y) ifelse(is.null(y), NA, y)) 
})
}
else
{
  tweets=dplyr::bind_rows(tweets,purrr::map_df(temp, function(x) { 
    purrr::map(jsonlite::stream_in(file(x)), function(y) ifelse(is.null(y), NA, y)) 
  }))
}
}
texts=c(tweets["text"])
texts
texts = gsub("[[:punct:]]", "", texts)
texts = gsub("[[:digit:]]", "", texts)
texts = gsub("http\\w+", "", texts)
texts = gsub("[ \t]{2,}", "", texts)
texts = gsub("^\\s+|\\s+$", "", texts)
tweets$entities
str(tweets$entities)
hashtag.regex <- regex("(?<=^|\\s)#\\S+")
tweets=as.data.frame(tweets)
hashtags <- str_extract_all(tweets$text, hashtag.regex)
tweets_tags=cbind(tweets,hashtags)
summary(fusion)