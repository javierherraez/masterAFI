library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(leaflet)
library(jsonlite)
library(dygraphs)
# setwd('ASIGNATURAS MASTER/06. Visualización de información/02. Visualización_dinámica/Practica/visualitation/')
setwd('C:/Users/jherraez/Documents/masterAFI/06. Visualización de información/02. Visualización_dinámica/Practica/visualitation')
source('functions.R')

nba_df = read.csv('../data/nba2020.csv')
nba_df$date <- as.Date(nba_df$date)
geojson <- readLines('../data/arenas.geojson', warn = FALSE, encoding = 'utf-8') %>%
  paste(collapse = '\n') %>%
  fromJSON(simplifyVector = FALSE)

geojson$style = list(
  weight = 1,
  opacity = 1,
  fillOpacity = 0.3,
  color = 'red'
)

# Interface

ui <- dashboardPage(
  skin = 'blue',
  dashboardHeader(
    title = 'NBA 2020 - 2021',
    titleWidth = 300
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Estadísticas Promedio Semanales', tabName = 'Temporal'),
      menuItem('Salario por equipos', tabName = 'Map')
    ),
    width = 300),
  dashboardBody(
    includeCSS('custom.css'),
    tabItems(
      tabItem(tabName = 'Temporal',
              fluidRow(
                column(12, h2('Estadísticas promedio semanales', style='color:#3C8DBC'))
              ),
              fluidRow(
                column(width = 3,
                       wellPanel(selectInput(inputId = 'team_weekly',
                                             label = 'Equipo',
                                             choices = c('TODOS', unique(nba_df$team_name)),
                                             multiple = FALSE))
                ),
                column(width = 9,
                       box(dygraphOutput('dygraph'), width=12),
                       box(textOutput('legenddygraph'), width=12)
                )
              )
      ),
      tabItem(tabName = 'Map',
              fluidRow(
                column(width = 4,
                       wellPanel(checkboxGroupInput(inputId = 'position_map',
                                                    label = 'Posición',
                                                    choices = c('Center' = 'C', 
                                                                'Foward' = 'F',
                                                                'Guard' = 'G'),
                                                    selected = 'C'))
                ),
                column(width = 6,
                       leafletOutput(outputId = 'mapa'))
              )
      )
    )
  )
)

# Server

server <- function(input, output, session){
  output$dygraph <- renderDygraph({
    draw_dygraph(input$team_weekly)
  })
  
  output$mapa <- renderLeaflet({
    draw_map(input$position_map)
  })
  
  observe({
    if(length(input$position_map) < 1){
      updateCheckboxGroupInput(session, 'position_map', selected= 'C')
    }
  })
}

# Deployment

shinyApp(ui = ui, server = server)
