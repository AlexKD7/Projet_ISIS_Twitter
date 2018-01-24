install.packages("ggplo2")
install.packages("plyr")
install.packages("purrr")
install.packages("twitteR")
install.packages("jsonlite")
install.packages("stringr")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("arabicStemR")
install.packages("quanteda")
install.packages("data.table")
install.packages("dplyr")
install.packages("rlist")
library(ggplot2)
library(plyr)
library(purrr)
library(twitteR)
library(jsonlite)
library(stringr)
library(tm)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(arabicStemR)
library(quanteda)
library(data.table)
library(dplyr)
library(rlist)
source("arborescence.r")
arborescence="/Users/alexisung/Cours/BI A3/Projet de fin de parcours - Twitter/Projet Twitter ISIS/tweets_attentats/"
setwd(arborescence)
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
dirs=list.dirs(arborescence)
length(dirs)

for(i in 1:length(dirs))
{
  directory=paste(arborescence,dirs[i],sep="")
  print(directory)
  temp <- list.files(directory,
                     pattern="ID-centai-tablette-*", full.names=TRUE)
  if(i==1)
  {
    myJSON <- lapply(temp, function(x) fromJSON(x))
  }
  else
  {
    myJSON=append(myJSON,lapply(temp, function(x) fromJSON(x)))
  }
}
user.name=lapply(myJSON,function(x) x$user$name)
user.name
text=lapply(myJSON,function(x) x$text)
created_at=lapply(myJSON,function(x) x$created_at)
user.description=lapply(myJSON,function(x) x$user$description)
user.created_at=lapply(myJSON,function(x) x$user$created_at)
user.lang=lapply(myJSON,function(x) x$user$lang)
user.listed_count=lapply(myJSON,function(x) x$user$listed_count)
user.friends_count=lapply(myJSON,function(x) x$user$friends_count)
user.statuses_count=lapply(myJSON,function(x) x$user$statuses_count)
user.followers_count=lapply(myJSON,function(x) x$user$followers_count)
head(myJSON)
user.name
lang=lapply(myJSON,function(x) x$lang)
lang
length(unlist(user.followers_count))
dfTweets=do.call(rbind,lapply(1:length(text),
                              function(i)
                                data_frame(text=unlist(text[i]),
                                           user.name=unlist(user.name[i]),
                                           created_at=unlist(created_at[i]),
                                           user.created_at=unlist(user.created_at[i]),
                                           user.lang=unlist(user.lang[i]),
                                           lang=unlist(lang[i]),
                                           user.friends_count=unlist(user.friends_count[i]),
                                           user.statuses_count=unlist(user.statuses_count[i])
                                )))
dfTweets$lang
dfTweets=dfTweets[!duplicated(dfTweets[,c('text')]),]
dfTweets
dfTweets3$text= gsub("[[:punct:]]", "", dfTweets3$text)
dfTweets3$text = gsub("[[:digit:]]", "", dfTweets3$text)
dfTweets$text = gsub("http\\w+", " ", dfTweets$text)
dfTweets$text = gsub("https\\w+", " ", dfTweets$text)
dfTweets3$text = gsub("[ \t]{2,}", "", dfTweets3$text)
dfTweets3$text = gsub("^\\s+|\\s+$", "", dfTweets3$text)
dfTweets$text = gsub("\n", " ", dfTweets$text)
dfTweets$text
summary(dfTweets)
library(tidytext)
output<- 'word'
input<- 'text'
dfTweets
help(unnest_tokens)
tidy_tweets <- bind_rows(dfTweets[,dfTweets$lang="ar"]%>% 
                           mutate(langue = "Julia"),
                         tweets_dave %>% 
                           mutate(person = "David")) %>%
  mutate(timestamp = ymd_hms(timestamp))
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
format.str <- "%a %b %d %H:%M:%S %z %Y"
Sys.setlocale("LC_TIME","en_US.UTF-8")
as.POSIXct(strptime(dfTweets[,"created_at"], format.str, tz = "GMT"), tz = "GMT")
tweets <- bind_rows(dfTweets %>% mutate(timestamp=as.POSIXct(strptime(created_at, format.str, tz = "GMT"), tz = "GMT")))
tweets$timestamp
ggplot(tweets, aes(x = timestamp, fill = lang)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~lang, ncol = 1)
arabicTweets=tweets[tweets$lang=="ar",]
frenchTweets=tweets[tweets$lang=="fr",]
frenchTweets$timestamp=as.Date(frenchTweets$timestamp)
frenchTweets13=frenchTweets[frenchTweets$timestamp=="2015-11-13",]
frenchTweets13[,"radical"]=NA
frenchTweets13$radical
nrow(frenchTweets13)
frenchTweets13[8,1]
frenchTweets13[8,]$radical=1
write.table(frenchTweets13, "/Users/alexisung/Desktop/Data/frenchTweets13.txt", sep="\t")
frenchTweets14=frenchTweets[frenchTweets$timestamp=="2015-11-14",]
frenchTweets14[,"radical"]=NA 
nrow(frenchTweets14)
frenchTweets14[200,1]
frenchTweets14[200,]$radical=-1
frenchTweets14$radical
write.table(frenchTweets14, "/Users/alexisung/Desktop/Data/frenchTweets14.txt", sep="\t")
frenchTweets15=frenchTweets[frenchTweets$timestamp=="2015-11-15",]
frenchTweets15[,"radical"]=NA 
nrow(frenchTweets15)
frenchTweets15[200,1]
frenchTweets15[200,]$radical=0
frenchTweets15$radical
write.table(frenchTweets15, "/Users/alexisung/Desktop/Data/frenchTweets15.txt", sep="\t")
englishTweets=tweets[tweets$lang=="en",]
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^ء-يA-Za-za_\\d#@']|'(?![ء-يA-Za-z_\\d#@]))"
arabicStopWords=scan(file = 'arabicStopWords.txt',what = "character"())
arabicStopWords
library(stringr)
library(dplyr)
library(stringr)
library(tidytext)
# tidy_tweets <- tweets %>% 
#   filter(!str_detect(text, "^RT")) %>%
#   mutate(text = str_replace_all(text, replace_reg, "")) %>%
#   unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
#   filter(!word %in% ifelse(lang=="ar",arabicStopWords,ifelse(lang=="fr",stopwords("french"),stopwords("english"))),
#          str_detect(word, "[a-zء-ي]"))
summary(dfTweets)
nrow(arabicTweets)
arabicTweets[16,1]
arabicTweets[15,]$radical=0
arabicTweets$timestamp
nvArabicTweets=arabicTweets
nrow(nvArabicTweets[nvArabicTweets$timestamp=="2015-11-13",])
nvArabicTweets13=nvArabicTweets[nvArabicTweets$timestamp=="2015-11-13",]
nvArabicTweets13[17,1]
nvArabicTweets13[17,]$radical=1
nvArabicTweets13$radical
#17
nvArabicTweets13$radical
nvArabicTweets14=nvArabicTweets[nvArabicTweets$timestamp=="2015-11-14",]
nvArabicTweets14[19,1]
nvArabicTweets14[18,]$radical=1
nvArabicTweets12=nvArabicTweets[nvArabicTweets$timestamp=="2015-11-12",]
nvArabicTweets12[1,1]
nvArabicTweets12[18,]$radical=1
filter(arabicTweets, timestamp == "2015-11-13")
nvArabicTweets[nvArabicTweets$timestamp=="2015-11-13","timestamp"]
summary(nvArabicTweets)
head(arabicTweets[,"radical"],13)
class(arabicTweets$timestamp)
summary(arabicTweets)
tidy_arabicTweets <- arabicTweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, ""),linenumber = row_number()) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% arabicStopWords,
         str_detect(word, "[a-zء-ي]"))
tidy_englishTweets <- englishTweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stopwords("english"),
         str_detect(word, "[a-zء-ي]"))
tidy_frenchTweets <- frenchTweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stopwords("french"),
         str_detect(word, "[a-zء-ي]"))
tidy_arabicTweets %>%
  count(word) %>%
  with(wordcloud(word, n, max.words=200, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Dark2")))
dfTweets %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 2) %>%
  separate(trigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% arabicStopWords,
         !word2 %in% arabicStopWords) %>%
  count(word1, word2, sort = TRUE)
library(wordcloud)
frequency <- tidy_arabicTweets %>% 
  group_by(lang) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_arabicTweets%>% 
              group_by(lang) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)
library(ggplot2)
tidy_arabicTweets %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
library(tidyr)
arabicSentiment <- tidy_arabicTweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(user.name, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
library(tidyr)
library(xlsx)
library(dplyr)
library(stringr)
arabic = read.csv(file="Arabic_Emoticon_Lexicon.txt",sep="\t",header=T)
arabic[arabic$word=="#دولة_الخلافة",]
dfTweets[grepl("هيت",dfTweets$text),]
summary(arabic)
colnames(arabic)[1]="word"
arabic
arabic$sentiment=ifelse(arabic$X.Sentiment.Score.<0,"negative","positif")
nrcjoy <- arabic %>% 
  filter(sentiment == "positif")
tidy_arabicTweets %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)
plot(1:nrow(arabic),arabic$X.Sentiment.Score.,ylim=c(-5,5),xlim=c(0,600))
arabic$X.Sentiment.Score.
janeaustensentiment <- tidy_arabicTweets %>%
  inner_join(arabic) %>%
  count(lang, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
library(ggplot2)
library(tidytext)
essais <- tidy_arabicTweets %>%
  inner_join(arabic) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
essais
essais %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
docsFrench = tm::Corpus(tm::VectorSource(dfTweets$text[which(dfTweets$lang=='fr')]))
docsEnglish = tm::Corpus(tm::VectorSource(dfTweets$text[which(dfTweets$lang=='en')]))
docsArabic = tm::Corpus(tm::VectorSource(dfTweets$text[which(dfTweets$lang=='ar')]))
arabicStopWords=tm_map(docsArabic,arabicStemR::removeStopWords)[[2]]$content
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docsFrench <- tm_map(docsFrench, toSpace, "/")
docsFrench <- tm_map(docsFrench, toSpace, "@")
docsFrench <- tm_map(docsFrench, toSpace, "\\|")
docsArabic <- tm_map(docsArabic, toSpace, "/")
docsArabic <- tm_map(docsArabic, toSpace, "@")
docsArabic <- tm_map(docsArabic, toSpace, "\\|")
docsEnglish <- tm_map(docsEnglish, toSpace, "/")
docsEnglish <- tm_map(docsEnglish, toSpace, "@")
docsEnglish <- tm_map(docsEnglish, toSpace, "\\|")
docsFrench <- tm_map(docsFrench, content_transformer(tolower))
docsFrench <- tm_map(docsFrench, removeNumbers)
docsFrench <- tm_map(docsFrench, removeWords, stopwords("french"))
docsArabic <- tm_map(docsArabic, arabicStemR::removeNumbers)
docsArabic <- tm_map(docsArabic, arabicStemR::removePunctuation)
docsArabic <- tm_map(docsArabic,removeWords,arabicStopWords)
docsEnglish <- tm_map(docsEnglish, content_transformer(tolower))
docsEnglish <- tm_map(docsEnglish, removeNumbers)
docsEnglish <- tm_map(docsEnglish, removeWords, stopwords("english"))

inspect(docsEnglish)
tdmFrench=TermDocumentMatrix(docsFrench)
tdmArabic=TermDocumentMatrix(docsArabic)
tdmEnglish=TermDocumentMatrix(docsEnglish)
wordsListFrench <- data.frame(word = names(sort(rowSums(as.matrix(tdmFrench)),decreasing = T)),freq=sort(rowSums(as.matrix(tdmFrench)),decreasing = T))
wordsListArabic <- data.frame(word = names(sort(rowSums(as.matrix(tdmArabic)),decreasing = T)),freq=sort(rowSums(as.matrix(tdmArabic)),decreasing = T))
wordsListEnglish <- data.frame(word = names(sort(rowSums(as.matrix(tdmEnglish)),decreasing = T)),freq=sort(rowSums(as.matrix(tdmEnglish)),decreasing = T)).
set.seed(1234)
head(wordsListArabic,10)
wordcloud(words = wordsListArabic$word, freq = wordsListArabic$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
findFreqTerms(tdmArabic, lowfreq = 200)
findAssocs(tdmArabic, terms = "", corlimit = 0.3)
barplot(wordsListArabic[1:10,]$freq, las = 2, names.arg = wordsListArabic[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
head(wordsListEnglish,10)
head(wordsListArabic,10)
head(wordsListFrench)
inspect(docsEnglish)
hashtag.regex <- regex("(?<=^|\\s)#\\S+")
hashtags <- str_extract_all(dfTweets3$text, hashtag.regex)
hashtags