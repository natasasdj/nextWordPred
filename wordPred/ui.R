library(shiny)


# shinyUI(fluidPage(
#         titlePanel("Next Word Prediction"),
#         textInput("text","Type text"),
#         actionButton("reset_input", "reset"),
#         h3("\n\n Next word:"),
#         verbatimTextOutput("pred")
#         #textOutput("pred",inline=F)
# ))

shinyUI(
        navbarPage("",
        tabPanel("Application",
                 fluidPage(
                         titlePanel("Next Word Prediction"),
                         textInput("text","Input text"),
                         actionButton("reset_input", "reset"),
                         h3("\n\n Next word:"),
                         verbatimTextOutput("pred1"),
                      #   textOutput("pred1"),
                         textOutput("pred2"),
                         textOutput("pred3")
                 )
        ),
        tabPanel('Description',
                helpText(
                p("This application allows a user to type a partial sentence and it predict the next word following the user's input."),
                p("The application predicts the three most probable words."),
                p("As a user is typing in the box 'Input text' , the next predicted word is shown in the box 'Next word'."),
                p("Bellow this are shown two other possible words."),
              #  p("The next word is shown only when a user types the blank symbol."),
                p("The input text is reset:"),
                p("    - with the button 'reset'"),
                p("    - or if a user types '.'.")
                )
        )
        )
)