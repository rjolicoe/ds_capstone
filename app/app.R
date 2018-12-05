#Data Science Capstone Project

library(readr)
library(shiny)
library(tidyverse)
library(tokenizers)
library(stringr)
library(tidytext)
library(png)


swear_words <- readLines("swear_words.txt", warn = FALSE, skipNul = TRUE)
swear_words <- as.data.frame(swear_words)

bigram_filtered <-  read_rds("bigram.rds")
trigram_filtered <- read_rds("trigram.rds")
quadgram_filtered <- read_rds("quadgram.rds")
  
  
colnames(swear_words) <- c("word")
swear_words$word      <- gsub("[,]","", swear_words$word)


# Define UI for application that draws a histogram
ui <- fluidPage(
    img(src = 'coursera_image.png', height = '100px', width = '100px'),
    img(src = 'johns_hopkins.png', height = '150px', width = '150px'),
    img(src = 'swiftkey.png', height = '50px', width = '250px'),
   
   # Application title
   titlePanel("Data Science Capstone Project"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
            textInput("text",
                  "Enter the word or words in the following box",
                  "Test"),
        submitButton("Suggestion")
      ),      
      mainPanel(
         tabsetPanel(type = "tabs",
                     tabPanel("Word Prediction",
                              fluidRow(
                                br(),
                                br(),
                                verbatimTextOutput("text"))
                              ),
                     tabPanel("Table_Output",
                              tableOutput("table"),
                              br(),
                              br(),
                              fluidRow(
                                br(),
                                tags$b("The frequency that the next word would be the predicted word")
                              )),
                     tabPanel("About Page",
                              br(),
                              fluidRow(
                              tags$b("Objective"),
                              tags$div(
                                tags$ul(
                                  tags$li("The goal of this application is to take a word 
                                          and be able to predict the next word"),
                                  tags$li("We will use a few R packages for the text processing"),
                                  tags$li("We will model bi-grams, tri-grams and quad-grams")
                              ))),
                              br(),
                              fluidRow(
                                tags$b("Methodology"),
                                tags$div(
                                  tags$p("We create a sample from the text data in which we"),
                                  tags$ul(
                                    tags$li("Cleanse the data"),
                                    tags$li("Remove special characters"),
                                    tags$li("Move the data to lowercase"),
                                    tags$li("Remove numbers from the data"),
                                    tags$li("Prepare the data for the algorithm")
                                  ),
                                  br(),
                                  tags$b("Prediction Algorithm"),
                                  tags$ul(
                                    tags$li("The entered words are then passed to the algorithm"),
                                    tags$li("Prediction function searches the n gram dictionary,
                                            for the top predicted word"),
                                    tags$li("The predicted word is then displayed in the main
                                            panel"),
                                    tags$li("The prediction algorithm can accept words between 1 and
                                            three words."),
                                    tags$li("Phrases over three words will request that only the
                                            first three words are entered"),
                                    tags$li("If a match isn't found it will display word not found")
                                  )
                                )
                              ))
      )
   )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dataInput <- reactive({
    words <- tolower(input$text)
    words <- tokenize_words(words)
    if(length(words[[1]])> 3){
      "Phrase too long"
    } else if(length(words[[1]]) == 2){
      if(words[[1]][1] %in% swear_words$word){
        "Profanity Not Approved"
      } else if(words[[1]][2] %in% swear_words$word){
        "Profanity Not Approved"
      } else 
      {
        words[[1]]
      }
    } 
    else if(length(words[[1]])==3){
      if(words[[1]][1] %in% swear_words$word){
        "Profanity Not Approved"
      } else if(words[[1]][2] %in% swear_words$word){
        "Profanity Not Approved"
      } else if(words[[1]][3] %in% swear_words$word){
        "Profanity Not Approved"
      } else{
        words[[1]]
      }
    }
    else {
      if(words[[1]] %in% swear_words$word){
        "Profanity Not Approved"
      }
      else 
      {
        words[[1]]
      }
    }
  })
  
  join_data <- reactive({
    if(length(dataInput())==1){
      if(dataInput() %in% c("Profanity Not Approved")){
        "Profanity"
      } else if(dataInput() %in% c("Phrase too long")){
        "Length"
      }
      else {
        dataset <- bigram_filtered 
      }
    }
    else if(length(dataInput())==2){
      dataset <- trigram_filtered 
    } else {
      dataset <- quadgram_filtered
    }
  })
  
  output$table <- renderTable({
    input_data <- tolower(input$text)
    data_frame <- join_data()
    data_frame <- data_frame %>% 
                   filter(str_detect(Matching_Phrase, tolower(input_data))) %>%
                    select(n, Prediction_Phrase)
    head(data_frame)
  })
  
  predicted_word <- reactive({
    if(class(join_data()) == "character"){
      suppressWarnings(paste("Please try again due to:", join_data()))
    }
    else{
      input_data <- tolower(input$text)
      predicted_word <- join_data() %>% 
        select(Matching_Phrase, Prediction_Phrase, n) %>%
        filter(str_detect(Matching_Phrase,input_data)) %>%
        top_n(1, n) %>%
        select(Prediction_Phrase)
      
      if(length(predicted_word$Prediction_Phrase) > 1){
        suppressWarnings(predicted_word$Prediction_Phrase[[1]])
      } 
      else {
        suppressWarnings(predicted_word$Prediction_Phrase)
      }
    }
    
  })
  
  word_check <- reactive({
    word_check <- tokenize_words(predicted_word())
    if(length(word_check) > 1){
      suppressWarnings(predicted_word())
    } else if(length(word_check)==1){
      suppressWarnings(predicted_word())
    } else{
      input_check <- tolower(input$text[[1]])
      input_check <- paste(input_check, collapse = " ")
      predicted_word <- join_data() %>%
        select(Matching_Phrase, Prediction_Phrase, n) %>%
        filter(str_detect(Matching_Phrase, input_check)) %>%
        top_n(1,n) %>%
        select(Prediction_Phrase)
      
      if(length(predicted_word$Prediction_Phrase) > 1){
        suppressWarnings(predicted_word$Prediction_Phrase[[1]])
      } else if(length(predicted_word$Prediction_Phrase) == 1){
        suppressWarnings(predicted_word$Prediction_Phrase)
      } else{
        "We werent able to find a match"
      }
    }
  })
  
  output$text <- renderText({
    paste("Predicted next word is:", word_check())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

