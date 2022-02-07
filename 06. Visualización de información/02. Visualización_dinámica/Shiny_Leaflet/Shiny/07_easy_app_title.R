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
  
  textInput(
    inputId = 'histogram_title',
    label = 'Choose a title for the histogram',
    value = 'Histogram of a random normal variable'
  ),
  
  # Output functions
  
  plotOutput(
    outputId = 'normal_hist'
  )
)

# Function used to assemple inputs into outputs
server <- function(input, output){
  
  output$normal_hist <- renderPlot(
    hist(rnorm(n = 1000, mean = input$normal_mean, sd = input$normal_sd),
         main = isolate(input$histogram_title))
  )
}

# Function that builds the app with an interface and a server given
shinyApp(ui = ui, server = server)