library(shiny)

# Function use to create the user interface and display elements
ui <- fluidPage(
  
  actionButton(
    inputId = 'num_clicks_in',
    label = 'Click me!'
    )

  )

# Function used to assemple inputs into outputs
server <- function(input, output){
  
  observeEvent(input$num_clicks_in, {
                  print(as.numeric(input$num_clicks_in))
                }
               )

}

# Function that builds the app with an interface and a server given
shinyApp(ui = ui, server = server)
