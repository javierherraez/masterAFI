library(shiny)

# Function use to create the user interface and display elements
ui <- fluidPage(
  
  # Input functions
  
  actionButton(
    inputId = 'unif',
    label = 'Uniform'
  ),
  
  actionButton(
    inputId = 'norm',
    label = 'Normal'
  ),
  
  # Output functions
  
  plotOutput(outputId = 'histogram')
)

# Function used to assemple inputs into outputs
server <- function(input, output){
  
  rv <- reactiveValues(x = rnorm(1000))
  
  observeEvent(input$unif, {rv$x <- runif(1000)})
  observeEvent(input$norm, {rv$x <- rnorm(1000)})
  
  output$histogram <- renderPlot(
    hist(rv$x)
  )
  
}

# Function that builds the app with an interface and a server given
shinyApp(ui = ui, server = server)