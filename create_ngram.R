#lib
library(tidyverse)
library(stringi)
library(tm)

# set Working Directory
setwd("../w7")
setwd("./en_US")

SouceFileList <- list.files()

## scan souce file (.txt)
Souce_twitter <- scan(SouceFileList[3],encoding = "UTF-8", what = character(), sep = "\n", blank.lines.skip = F)
Souce_blogs <- scan(SouceFileList[1],encoding = "UTF-8", what = character(), sep = "\n", blank.lines.skip = F)
Souce_news <- scan(SouceFileList[2],encoding = "UTF-8", what = character(), sep = "\n", blank.lines.skip = F)

# create Raw Data list
RawData <- list(twitter=Souce_twitter,
                blogs=Souce_blogs,
                news=Souce_news)
# Remove scan data
rm(Souce_twitter,Souce_blogs,Souce_news)


## Sampling from data sets
set.seed(123456)

# 1% Sampling
samp_twitter <- data_frame(source="twitter", 
                        line=sample(1:length(RawData$twitter),
                        length(RawData$twitter)*0.01)) %>% 
                        arrange(line) %>% 
                        mutate(sentence=RawData$twitter[line])
# 1% Sampling
samp_blogs <- data_frame(source="blogs",
                        line=sample(1:length(RawData$blogs),
                        length(RawData$blogs)*0.01)) %>% 
                        arrange(line) %>% 
                        mutate(sentence=RawData$blogs[line])
# 10% Sampling
samp_news <- data_frame(source="news",
                        line=sample(1:length(RawData$news),
                        length(RawData$news)*0.1)) %>% 
                        arrange(line) %>% 
                        mutate(sentence=RawData$news[line])

SampleData <- list(twitter=samp_twitter,
                blogs=samp_blogs,
                news=samp_news)
# Remove
rm(samp_twitter,samp_blogs,samp_news)
rm(RawData)

## Processing data

procesFunction <- function(sentence){
        sentence <- sentence %>%
                str_to_lower %>%        #convert to lower letters
                removeWords(stopwords("english")) %>% #remove STOPWORDS
                removePunctuation %>%   #remove punctuation
                removeNumbers %>%       #remove numbers
                stripWhitespace %>%     #remove extra whitespace
                str_replace("^ ","")}

ProcesData <- SampleData %>% lapply(mutate,sentence=procesFunction(sentence))


## Create N-gram data

tryNgramFunction <- function(x,n) {
        ans <- NULL
        x <-  strsplit(x," +") %>% unlist
                for(i in 1:(length(x) - n + 1)){
                        ans[i] <- paste(x[i:(i+n-1)], collapse=" ")}
        ans
        }

# 2-gram
Ngram2_tw <- ProcesData$twitter %>% select(sentence) %>% sapply(tryNgramFunction,2) 
Ngram2_bl <- ProcesData$blogs %>% select(sentence) %>% sapply(tryNgramFunction,2) 
Ngram2_ne <- ProcesData$news %>% select(sentence) %>% sapply(tryNgramFunction,2) 

Ngram2Data <-list(twitter=Ngram2_tw,
                   blogs=Ngram2_bl,
                   news=Ngram2_ne)

rm(Ngram2_tw,Ngram2_bl,Ngram2_ne)

# 3-gram
Ngram3_tw <- ProcesData$twitter %>% select(sentence) %>% sapply(tryNgramFunction,3) 
Ngram3_bl <- ProcesData$blogs %>% select(sentence) %>% sapply(tryNgramFunction,3) 
Ngram3_ne <- ProcesData$news %>% select(sentence) %>% sapply(tryNgramFunction,3) 

Ngram3Data <-list(twitter=Ngram3_tw,
                  blogs=Ngram3_bl,
                  news=Ngram3_ne)

rm(Ngram3_tw,Ngram3_bl,Ngram3_ne)

# 4-gram
Ngram4_tw <- ProcesData$twitter %>% select(sentence) %>% sapply(tryNgramFunction,4) 
Ngram4_bl <- ProcesData$blogs %>% select(sentence) %>% sapply(tryNgramFunction,4) 
Ngram4_ne <- ProcesData$news %>% select(sentence) %>% sapply(tryNgramFunction,4) 

Ngram4Data <-list(twitter=Ngram4_tw,
                  blogs=Ngram4_bl,
                  news=Ngram4_ne)

rm(Ngram4_tw,Ngram4_bl,Ngram4_ne)

## Save .RData
setwd("../")
save (Ngram2Data, file="Ngram2Data.RData")
save (Ngram3Data, file="Ngram3Data.RData")
save (Ngram4Data, file="Ngram4Data.RData")
save (ProcesData, file="ProcesData.RData")
save (SampleData, file="SampleData.RData")
