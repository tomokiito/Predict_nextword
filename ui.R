library(shiny)
library(tidyverse)
library(stringi)
library(tm)

shinyUI(
        pageWithSidebar(
                
                # Application title
                headerPanel("Data Science Capstone - PA: Final Project Submission"),

                # Sidebar with a slider input
                sidebarPanel(
                        selectInput("DataSouce", "Choose a data souce:",
                                    choices = c("twitter", "blog", "news")),
                        textInput("textinput", "text:", ""),
                        br(),
                        helpText(a("Download Coursera-SwiftKey Dataset",
                                   target="_blank",
                                   href="https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")
                        )
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        h2("Input text:"),
                        verbatimTextOutput("textoutput"),
                        br(),
                        h2("Predicted next word:"),
                        verbatimTextOutput("predictTextoutput"),
                        br(),br(),
                        h4(textOutput("dataSouceTextoutput")),
                        h4("Pick list:"),
                        textOutput("dataSouceTop1output"),
                        textOutput("dataSouceTop2output"),
                        textOutput("dataSouceTop3output"),
                        textOutput("dataSouceTop4output"),
                        br(),
                        h4("Cleaned text:"),
                        verbatimTextOutput("cleanTextoutput"),
                        helpText(HTML("<strong>Note:</strong><br/>
                                       <strong>Cleaned text</strong>"),
                                       "is the result of the following processing for prediction",
                                 HTML("<ul>
                                        <li>convert to lower letters</li>
                                        <li>remove punctuation</li>
                                        <li>remove numbers</li>
                                        <li>remove extra whitespace</li>
                                        <li>and remove english
                                         <a href='https://www.rdocumentation.org/packages/tm/versions/0.7-7/topics/stopwords' target='_blank'>
                                          STOPWORDS</a></li>
                                       </ul>")
                        )
                )
        )
)