library(shiny)

ees <- read.csv("Shiny_Leaflet/datos_ejercicios/ees2014.tsv", sep = "\t")

head(ees)

ui <- fluidPage(
    shinythemes::themeSelector(),
    fluidRow(
        column(6,
            fluidRow(sliderInput(
                      inputId = 'normal_sd',
                      label = 'Choose the standard deviation of a normal distribution',
                      value = 1, min = 0.01, max = 10
                  )),
            fluidRow(sliderInput(
                      inputId = 'normal_sd',
                      label = 'Choose the standard deviation of a normal distribution',
                      value = 1, min = 0.01, max = 10
                  )),
            fluidRow(sliderInput(
                      inputId = 'normal_sd',
                      label = 'Choose the standard deviation of a normal distribution',
                      value = 1, min = 0.01, max = 10
                  ))
        ),
        column(6,
                sliderInput(
                      inputId = 'normal_sd',
                      label = 'Choose the standard deviation of a normal distribution',
                      value = 1, min = 0.01, max = 10
                  ))
        )


)

server <- function(input, output, session){



}



shinyApp(ui = ui, server = server)