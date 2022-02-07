library(shiny)

# Function use to create the user interface and display elements
ui <- fluidPage(
  
  # Input functions
  
  sliderInput(
    inputId = 'normal_mean',
    label = 'Choose the mean of a normal distribution',
    min = -10, max = 10, value = 0
  ),
  
  sliderInput(
    inputId = 'normal_sd',
    label = 'Choose the standard deviation of a normal distribution',
    value = 1, min = 0.01, max = 10
  ),
  
  # Output functions
  
  plotOutput(
    outputId = 'normal_hist'
  ),
  
  verbatimTextOutput(
    outputId = 'normal_summary'
  )
)

# Function used to assemple inputs into outputs
server <- function(input, output){
  
  x <- reactive(rnorm(n = 5, mean = input$normal_mean, sd = input$normal_sd))
  # Now x is a function!
  
  output$normal_hist <- renderPlot(
    hist(x())
  )
  
  output$normal_summary <- renderPrint(
    summary(x())
  )
  
}

# Function that builds the app with an interface and a server given
shinyApp(ui = ui, server = server)