
library(shiny)
library(shinydashboard)

ui <-  dashboardPage(
  skin = 'purple',
  dashboardHeader(title = 'Probability distributions',
                  titleWidth = 250),
  dashboardSidebar(width = 250,
                   sidebarMenu(
                     menuItem('Normal', tabName = 'Normal'),
                     menuItem('Uniform', tabName = 'Uniform')
                   )),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'Normal',
              h2('Normal distribution here')),
      tabItem(tabName = 'Uniform',
              h2('Uniform distribution here'))
    )
  )
)


server <- function(input, output){
  
}

shinyApp(ui = ui, server = server)