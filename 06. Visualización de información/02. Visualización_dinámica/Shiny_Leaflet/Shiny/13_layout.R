library(shiny)

# Function use to create the user interface and display elements
ui <- fluidPage(

  titlePanel("Hello Shiny!"),
  shinythemes::themeSelector(),
  
  # Input functions
  fluidRow(
    column(width = 12, 
           sliderInput(
              inputId = 'normal_mean',
              label = 'Choose the mean of a normal distribution',
              min = -10, max = 10, value = 0
           )
  ),
  fluidRow(
    column(6,div(style = "height:200px;background-color: yellow;", "Topleft")),
    column(6,div(style = "height:100px;background-color: blue;", "Topright"))
  ),
  fluidRow(
    column(6,div(style = "height:100px;background-color: green;", "Bottomleft")),
    column(6,div( style = "height:150px;background-color: red;", "Bottomright", 
                  sliderInput(
                      inputId = 'normal_sd',
                      label = 'Choose the standard deviation of a normal distribution',
                      value = 1, min = 0.01, max = 10
                  )
                )
          )
  ),
  fluidRow(
    column(width = 8, 
           plotOutput(
             outputId = 'normal_hist'
           ),
           offset = 2)
  
  )
)
)

# Function used to assemple inputs into outputs
server <- function(input, output){
  
  output$normal_hist <- renderPlot(
    hist(rnorm(n = 1000, mean = input$normal_mean, sd = input$normal_sd))
  )
}

# Function that builds the app with an interface and a server given
shinyApp(ui = ui, server = server)