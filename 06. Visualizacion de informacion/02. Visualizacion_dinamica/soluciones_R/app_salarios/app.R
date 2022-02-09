library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(leaflet)
library(jsonlite)

# Interface

ui <- dashboardPage(
  skin = 'blue',
  dashboardHeader(
    title = 'Salarios 2014'
  ),
  dashboardSidebar(
                   sidebarMenu(
                     menuItem('Salarios a nivel nacional', tabName = 'Nacional'),
                     menuItem('Salario por provincias', tabName = 'Provincias')
                   )),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'Nacional',
              fluidRow(
                column(width = 4,
                      wellPanel(selectInput(inputId = 'variable',
                                  label = 'Variable en el eje X',
                                  choices = c('CNAE' = 'var_grupo_cnae', 'Tipo de contrato' = 'var_tipocontrato', 'Antigüedad' = 'var_antiguedad'),
                                  selected = 'var_grupo_cnae',
                                  multiple = FALSE)),
                      wellPanel(radioButtons(inputId = 'boton_sexo',
                                   label = 'Separar por sexo',
                                   choices = c('Sí', 'No'),
                                   selected = 'No')),
                      wellPanel(checkboxGroupInput(inputId = 'tipo_jornada',
                                    label = 'Tipo de jornada',
                                    choices = c('A tiempo completo' = 'Tiempo completo', 'A tiempo parcial' = 'Tiempo parcial'),
                                    selected = c('Tiempo completo', 'Tiempo parcial')))
                ),
                column(width = 8,
                  plotOutput(outputId = 'grafico_barras')
                )
              )
      ),
      tabItem(tabName = 'Provincias',
              fluidRow(
                column(width = 4,
                       wellPanel(checkboxGroupInput(inputId = 'sexos',
                                                    label = 'Sexo',
                                                    choices = c('Hombres' = 'hombre', 'Mujeres' = 'mujer'),
                                                    selected = c('hombre', 'mujer'))),
                       wellPanel(checkboxGroupInput(inputId = 'contratos',
                                                    label = 'Tipo de contrato',
                                                    choices = c('Indefinido', 'Temporal'),
                                                    selected = c('Indefinido', 'Temporal')))
                       ),
                column(width = 8,
                       leafletOutput(outputId = 'mapa'))
              )
              )
    )
  )
)

# Server

server <- function(input, output){
  
  # Functionality for first tab
  
  ees <- read.table('soluciones_R/ees2014.tsv', 
                    sep = '\t', header = T, stringsAsFactors = F)
  
  output$grafico_barras <- renderPlot({
    if(input$boton_sexo == 'No'){
      eval(
        substitute(
          ees %>% filter(var_tipojornada %in% input$tipo_jornada) %>%
              group_by(column) %>%
              summarise(salario = sum(salario_bruto_anual_medio*factor_elev)/sum(factor_elev)) %>%
            ggplot(aes(x = column, y = salario)) +
            geom_bar(fill = 'salmon', stat = 'identity'),
          list(column = as.symbol(input$variable))))
    }else{
      eval(
        substitute(
          ees %>% filter(var_tipojornada %in% input$tipo_jornada) %>%
            group_by(column, var_sexo) %>%
            summarise(salario = sum(salario_bruto_anual_medio*factor_elev)/sum(factor_elev)) %>%
            ggplot(aes(x = column, y = salario, fill = var_sexo)) +
            geom_bar(stat = 'identity', position = 'dodge'),
          list(column = as.symbol(input$variable))))
    }
  })
  
  # Functionality for second tab
    
  geojson <- readLines('soluciones_R/provinciasSimplified.json', warn = FALSE, encoding = 'utf-8') %>%
    paste(collapse = '\n') %>%
    fromJSON(simplifyVector = FALSE)
  
  geojson$style = list(
    weight = 1,
    opacity = 1,
    fillOpacity = 0.3,
    color = 'gray'
  )
  
  output$mapa <- renderLeaflet({
    
    ees %>% filter(var_sexo %in% input$sexos) %>%
            filter(var_tipocontrato %in% input$contratos) %>%
            group_by(var_provincia) %>%
            summarise(salario = sum(salario_bruto_anual_medio*factor_elev)/sum(factor_elev)) -> map_df
    
    pal <- colorNumeric('Greens', map_df$salario)
    geojson$features <- lapply(geojson$features, function(feat){
       provincia <- as.numeric(feat$properties$id)
       feat$properties$style <- list(fillColor = pal(map_df$salario[map_df$var_provincia == provincia]))
       feat$properties$popup <- paste0('<b>Provincia:</b> ', feat$properties$description,
                                       '<br><b>Salario medio:</b> ',
                                       round(map_df$salario[map_df$var_provincia == provincia]))
       return(feat)
    })
    
    leaflet() %>% setView(lng = -3.68, lat = 40.43, zoom = 5) %>%
                  addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
                  addGeoJSON(geojson) %>% 
                  addLegend("bottomright", pal = pal, values = map_df$salario,
                                               title = "Salario",
                                               opacity = 1)
    })
}

# Deployment

shinyApp(ui = ui, server = server)