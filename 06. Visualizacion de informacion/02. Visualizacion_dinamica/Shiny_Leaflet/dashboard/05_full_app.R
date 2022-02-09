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
              fluidRow(h2('Normal distribution here')),
              fluidRow(
                column(width = 4, sliderInput(inputId = 'normal_mean',
                                              label = 'Choose the mean',
                                              min = -5, max = 5, value = 0,
                                              width = '100%')),
                column(width = 4, offset = 4, sliderInput(inputId = 'normal_sd',
                                                          label = 'Choose the standard deviation',
                                                          min = 0, max = 10, value = 1,
                                                          width = '100%'))
              ),
              fluidRow(
                column(width = 8, offset = 2, plotOutput(outputId = 'normal_hist'))
              )
      ),
      tabItem(tabName = 'Uniform',
              fluidRow(h2('Uniform distribution here')),
              fluidRow(
                column(width = 4, sliderInput(inputId = 'unif_b',
                                              label = 'Choose the maximum value of a uniform distribution',
                                              min = 1, max = 10, value = 5,
                                              width = '100%'))
              ),
              fluidRow(
                column(width = 8, offset = 2, plotOutput(outputId = 'unif_hist'))
              )
      )
    )
  )
)


server <- function(input, output){
  output$normal_hist <- renderPlot(hist(rnorm(1000, mean = input$normal_mean, sd = input$normal_sd)))
  output$unif_hist <- renderPlot(hist(runif(1000, min = 0, max = input$unif_b)))
}

shinyApp(ui = ui, server = server)