

shinyUI(fluidPage(
      
      titlePanel("Data Science Capstone N-Gram Prediction Model"),
      
      sidebarLayout(
            sidebarPanel(
                  helpText("Enter a sentence in the text field and the model will predict the following word and give you a list of other choices."),
                  helpText("Please hit the Predict button and allow a few seconds for the datasets to load. It makes the predicting process faster."),
                  helpText("Once the output boxes populate for the first time, everything is all set to go."),
                  helpText("Can you guess what the first prediction will be?... so corny.")
            ),
            mainPanel(
                  textInput("input.string", label = h3("Input"), value = "Give it a"),
                  h4("input"),
                  verbatimTextOutput("clean"),
                  h4("Main choice"),
                  verbatimTextOutput("prediction"),
                  h4("Other choices"),
                  verbatimTextOutput("other.choices")
            )
      )
)
)