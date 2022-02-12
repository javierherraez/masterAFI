library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(leaflet)
library(jsonlite)
library(dygraphs)
library(plotly)
require(scales)
source('functions.R')
# setwd('C:/Users/jherraez/Documents/masterAFI/06. Visualizacion de informacion/02. Visualizacion_dinamica/Practica/visualitation')
# setwd('C:/Users/Javier/Documents/masterAFI/06. Visualizacion de informacion/02. Visualizacion_dinamica/Practica/visualitation')

nba_df = read.csv('nba2020.csv')
nba_df$date <- as.Date(nba_df$date)
geojson <- readLines('arenas.geojson', warn = FALSE, encoding = 'utf-8') %>%
  paste(collapse = '\n') %>%
  fromJSON(simplifyVector = FALSE)

conferences_nba <- unique(sapply(geojson$features, function(feat){
  return (feat$properties$conference)
}))

divisions_nba <- unique(sapply(geojson$features, function(feat){
  return (feat$properties$division)
}))

pair_conference_division <- unique(as.data.frame(t(sapply(geojson$features, function(feat){
  return (c(feat$properties$conference, feat$properties$division))
}))))

stats <- c('Puntos' =  'points',
           'Asistencias' =  'assists',
           'Rebotes' = 'rebounds',
           'Robos' = 'steals',
           'Bloqueos' = 'blocks',
           'Faltas Personales' = 'personal_foul',
           'Pérdidas' = 'turnovers',
           'Tiros de campo anotados' = 'field_goals_made',
           'Tiros de campo intentados' = 'field_goals_attempted',
           'Salario' = 'salary')

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
      menuItem('Salario por equipos', tabName = 'Map'),
      menuItem('Comparación Estadísticas', tabName = 'Comparison')
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
                column(12, h2('Salarios por Equipos', style='color:#3C8DBC'))
              ),
              fluidRow(
                column(width = 3,
                       wellPanel(checkboxGroupInput(inputId = 'position_map',
                                                    label = 'Posición',
                                                    choices = c('Center' = 'C', 
                                                                'Foward' = 'F',
                                                                'Guard' = 'G'),
                                                    selected = 'C, F, G')),
                       wellPanel(checkboxGroupInput(inputId = 'conference_map',
                                                    label = 'Conferencia',
                                                    choices = conferences_nba,
                                                    selected = conferences_nba)),
                       wellPanel(checkboxGroupInput(inputId = 'division_map',
                                                    label = 'Division',
                                                    choices = divisions_nba,
                                                    selected = divisions_nba))
                ),
                column(width = 9,
                       leafletOutput(outputId = 'mapa', height = 500))
              )
      ),
      tabItem(tabName = 'Comparison',
              fluidRow(
                column(12, h2('Comparación de Estadísticas', style='color:#3C8DBC'))
              ),
              fluidRow(
                column(width = 3,
                       wellPanel(radioButtons(inputId = 'var_comparison',
                                              label = 'Variable a Comparar',
                                              choices = c('Equipo' = 'team_name', 
                                                          'Posición' = 'player_position',
                                                          'Top 30 Jugadores' = 'player'),
                                              selected = 'team_name')),
                       wellPanel(radioButtons(inputId = 'stat_comparison',
                                              label = 'Estadística',
                                              choices = stats,
                                              selected = stats[1]))
                ),
                column(width = 9,
                       box(plotlyOutput('comparison_plot'), width=12)
                )
              )
      )
    )
  )
)

# Server

server <- function(input, output, session){
  output$dygraph <- renderDygraph({
    draw_dygraph(nba_df, input$team_weekly)
  })
  
  output$mapa <- renderLeaflet({
    draw_map(nba_df, geojson, input$position_map, input$conference_map, input$division_map)
  })
  
  output$comparison_plot <- renderPlotly({
    draw_barplot(nba_df, input$stat_comparison, input$var_comparison)
  })
  
  observe({
    if(length(input$position_map) < 1){
      updateCheckboxGroupInput(session, 'position_map', selected = 'C')
    }
    if(length(input$conference_map) < 1){
      updateCheckboxGroupInput(session, 'conference_map', selected = conferences_nba)
    }
    if(length(input$division_map) < 1){
      updateCheckboxGroupInput(session, 'division_map', selected = divisions_nba)
    }
    conf_div <- expand.grid(input$conference_map, input$division_map)
    if(!any(do.call(paste0, conf_div) %in% do.call(paste0, pair_conference_division))){
      updateCheckboxGroupInput(session, 'conference_map', selected = conferences_nba)
    }
  })
}

# Deployment

shinyApp(ui = ui, server = server)
