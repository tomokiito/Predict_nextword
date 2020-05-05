library(shiny)
library(tidyverse)
library(stringi)
library(tm)

#load Ngram data
load("./Data/Ngram2Data.RData")
load("./Data/Ngram3Data.RData")


shinyServer(function(input, output) {

        procesFunction <- function(sentence){
                sentence <- sentence %>%
                        str_to_lower %>%        #convert to lower letters
                        removeWords(stopwords("english")) %>% #remove STOPWORDS
                        removePunctuation %>%   #remove punctuation
                        removeNumbers %>%       #remove numbers
                        stripWhitespace %>%     #remove extra whitespace
                        str_replace("^ ","")}
        
        # explore Ngram file
        predictFunction <- function(Data,Word){
                top5 <- subset(Data,grepl(paste0("^",Word," "),Data)) %>% 
                        table %>% sort(decreasing = TRUE) %>% 
                        rownames %>% head(n=5)}
        
        output$textoutput <- renderText({
                input$textinput
        })
        
        DataSouce <- reactive({
                switch(input$DataSouce,
                       twitter = list(souce  = "twitter",
                                      Ngram2 = Ngram2Data$twitter,
                                      Ngram3 = Ngram3Data$twitter),
                       blog    = list(souce  = "blog",
                                      Ngram2 = Ngram2Data$blog,
                                      Ngram3 = Ngram3Data$blog),
                       news    = list(souce  = "news",
                                      Ngram2 = Ngram2Data$news,
                                      Ngram3 = Ngram3Data$news)
                )
        })
        
        # clan input text
        myText <- reactive({
                list(
                        text = input$textinput %>% procesFunction,
                        vec  = input$textinput %>% procesFunction %>%
                                strsplit(" +") %>% unlist
                )
        })
        
        # predict next word
        myWord <- reactive({
                n <- length(myText()$vec)
                
                if(n == 0) { myWord <- list(word="",top="",ngram="") } 
                else if(n == 1){
                        LastWord <- myText()$vec[n]
                        Ngram2top <- predictFunction(DataSouce()$Ngram2,LastWord)
                        if(length(Ngram2top)){
                                myWord <- list(word=gsub(paste0(LastWord," "),"",Ngram2top[1]),
                                               top=Ngram2top,ngram="bigram model")}
                        else myWord <- list(word="",top="None.",ngram="")
                        }
                else if(n >= 2){
                        LastWord <- paste(myText()$vec[(n-1):n],collapse=" ")
                        Ngram3top <- predictFunction(DataSouce()$Ngram3,LastWord)
                        if(length(Ngram3top)){
                                myWord <- list(word=gsub(paste0(LastWord," "),"",Ngram3top[1]),
                                       top=Ngram3top,ngram="trigram model")}
                        else {
                                LastWord <- myText()$vec[n]
                                Ngram2top <- predictFunction(DataSouce()$Ngram2,LastWord)
                                if(length(Ngram2top)){
                                        myWord <- list(word=gsub(paste0(LastWord," "),"",Ngram2top[1]),
                                                       top=Ngram2top,ngram="bigram model")}
                                else myWord <- list(word="",top="None.",ngram="")
                        }
                }
        })
        
        output$predictTextoutput <- renderText({
                myWord()$word
        })        
        
        output$dataSouceTextoutput<- renderText({
                paste("Data souce:",DataSouce()$souce,myWord()$ngram)
        })        
        
        output$dataSouceTop1output<- renderText({myWord()$top[1]})        
        output$dataSouceTop2output<- renderText({
                if(!is.na(myWord()$top[2])){myWord()$top[2]}})        
        output$dataSouceTop3output<- renderText({
                if(!is.na(myWord()$top[3])){myWord()$top[3]}})        
        output$dataSouceTop4output<- renderText({
                if(!is.na(myWord()$top[4])){myWord()$top[4]}})        
        
        output$cleanTextoutput <- renderText({
                myText()$text
        })

})