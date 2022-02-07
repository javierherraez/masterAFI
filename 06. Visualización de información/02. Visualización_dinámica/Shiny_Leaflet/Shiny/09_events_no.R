library(shiny)

# Function use to create the user interface and display elements
ui <- fluidPage(
  
  # Input functions
  
  sliderInput(inputId = 'hist_mean',
              label = 'Choose the mean of a normal distribution',
              min = -10, max = 10, value = 0),
  
  actionButton(
    inputId = 'update',
    label = 'Update'
  ),
  
  # Output functions
  
  plotOutput(outputId = 'histogram')
)

# Function used to assemple inputs into outputs
server <- function(input, output){
  
  output$histogram <- renderPlot(
    hist(rnorm(1000, mean = input$hist_mean, sd = 1))
  )
  
}

# Function that builds the app with an interface and a server given
shinyApp(ui = ui, server = server)
