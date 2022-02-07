library(shiny)
library(shinydashboard)

ui <-  dashboardPage(
    skin = 'purple',
    dashboardHeader(title = 'Probability distributions',
                    titleWidth = 250),
    dashboardSidebar(width = 250),
    dashboardBody()
)


server <- function(input, output){
  
}

shinyApp(ui = ui, server = server)