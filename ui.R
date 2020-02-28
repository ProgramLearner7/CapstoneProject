library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Capstone Project - Predictive text"),
  p("The purpose of this tool is to predict the third word based on the two words user entered."),
  p("The prediction methodology is based on N-gram and back off Model."),
  br(),
  p("The reference of the methodology is below."), 
  a(href="https://rstudio-pubs-static.s3.amazonaws.com/271652_1525c0598da74774bfa4047803cee0d5.html", "https://rstudio-pubs-static.s3.amazonaws.com/271652_1525c0598da74774bfa4047803cee0d5.html"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput(inputId = "pre", label = "Please enter two words (no space in the end)", value = "Enter Word"),
       actionButton("Go", "Apply Changes")
       ),
    
    # Show a plot of the generated distribution
    mainPanel(
        h2("The word cloud of the third word. It shows the top 150 words based on its probability"),
        imageOutput("wordcloud"),
        h2("The probability of top 6 words"),
        tableOutput("table")
    )
  )
))
