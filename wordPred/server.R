# Next Word Prediction
# Data Science Capstone Project
# Johns Hopkins University on Coursera
# Author: Natasa Sarafijanovic-Djukic
# January 2016

library(shiny)

options(shiny.maxRequestSize=100540864)
options(width=30)
source("prediction.R",local=TRUE)
# words<-function(t){t}
# pred<-function(x){
#         if (length(x)>0){
#                 words <- unlist(strsplit(x, "\\s+"))
#                 words[length(words)]
#         }
# }
#                 


shinyServer(function(input, output, session) {

        #output$pred <- renderText({ input$text })        
        observeEvent(input$reset_input, {
                updateTextInput(session, "text", value = "")
        }) 
        
        x<-reactive({t<-input$text
                l<-nchar(t);
                last_char<-substr(t, l, l)
#                 if (last_char==" "|t=="") {
#                         pred(t)
#                 } else if (last_char==".") {
#                         updateTextInput(session, "text", value = "")
#                         pred("")
#                 }
        if (last_char==".") {
                updateTextInput(session, "text", value = "")
                pred("")
                         
        } else {
                pred(t) 
        }
        })
        
        output$pred1 <- renderText({ x()[1] })
        output$pred2 <- renderText({ x()[2] })
        output$pred3 <- renderText({ x()[3] })
})


