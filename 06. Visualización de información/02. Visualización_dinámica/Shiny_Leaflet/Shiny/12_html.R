library(shiny)

# Function use to create the user interface and display elements
ui <- fluidPage(
  HTML('<p>This is a paragraph</p>
        <p>This is another paragraph. There will be an iframe below</p>
        <iframe width="560" height="315" src="https://www.youtube.com/embed/Gyrfsrd4zK0" frameborder="0" allowfullscreen></iframe>')
)

# Function used to assemple inputs into outputs
server <- function(input, output){}

# Function that builds the app with an interface and a server given
shinyApp(ui = ui, server = server)
