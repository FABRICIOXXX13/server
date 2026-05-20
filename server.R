library(shiny)
library(shinydashboard)
library(gapminder)
library(dplyr)
library(plotly)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)

# Configuración de la UI
ui <- dashboardPage(
  dashboardHeader(title = "Gapminder Insights"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      
      hr(),
      tags$p("FILTROS DE DATOS", style = "padding: 10px; color: #b8c7ce; font-size: 12px;"),
      
      # Filtro 1: Continente
      selectInput("continent", "Continente", 
                  choices = c("Todos" = "All", levels(gapminder$continent))),
      
      # Filtro 2: País (Reactivo al continente)
      selectInput("country", "País", choices = NULL),
      
      # Filtro 3: Indicador
      selectInput("indicator", "Indicador", 
                  choices = c("Esperanza de Vida" = "lifeExp", 
                              "Población" = "pop", 
                              "PIB per Cápita" = "gdpPercap"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f8f9fa; }
        .box { border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }
      "))
    ),
    
    fluidRow(
      # KPIs
      valueBoxOutput("kpi_life", width = 4),
      valueBoxOutput("kpi_pop", width = 4),
      valueBoxOutput("kpi_gdp", width = 4)
    ),
    
    fluidRow(
      box(title = "Distribución Global", width = 12, solidHeader = TRUE,
          leafletOutput("map", height = 400))
    ),
    
    fluidRow(
      box(title = "Tendencia Histórica", width = 7,
          plotlyOutput("lineChart")),
      box(title = "Comparación Regional", width = 5,
          plotlyOutput("barChart"))
    )
  )
)

# Lógica del Servidor
server <- function(input, output, session) {
  
  # Lógica de filtros jerárquicos
  observe({
    countries <- if (input$continent == "All") {
      levels(gapminder$country)
    } else {
      gapminder %>% 
        filter(continent == input$continent) %>% 
        pull(country) %>% 
        unique() %>% 
        as.character()
    }
    updateSelectInput(session, "country", choices = c("Seleccionar País..." = "", countries))
  })
  
  # Datos filtrados
  filtered_data <- reactive({
    data <- gapminder
    if (input$continent != "All") data <- data %>% filter(continent == input$continent)
    if (input$country != "") data <- data %>% filter(country == input$country)
    data
  })
  
  # Renderizado de KPIs
  output$kpi_life <- renderValueBox({
    val <- round(mean(filtered_data()$lifeExp, na.rm = TRUE), 1)
    valueBox(paste(val, "años"), "Esperanza de Vida Media", icon = icon("heart"), color = "blue")
  })
  
  output$kpi_pop <- renderValueBox({
    val <- round(sum(as.numeric(filtered_data()$pop), na.rm = TRUE) / 1e9, 2)
    valueBox(paste(val, "B"), "Población Total", icon = icon("users"), color = "aqua")
  })
  
  output$kpi_gdp <- renderValueBox({
    val <- format(round(mean(filtered_data()$gdpPercap, na.rm = TRUE), 0), big.mark=",")
    valueBox(paste("$", val), "PIB per Cápita Promedio", icon = icon("money-bill"), color = "light-blue")
  })
  
  # Mapa Mundial
  output$map <- renderLeaflet({
    world <- ne_countries(scale = "medium", returnclass = "sf")
    data_latest <- gapminder %>% filter(year == 2007)
    
    map_data <- world %>% 
      left_join(data_latest, by = c("name" = "country"))
    
    pal <- colorNumeric("YlGnBu", domain = map_data[[input$indicator]])
    
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(get(input$indicator)), 
                  fillOpacity = 0.7, color = "white", weight = 1,
                  label = ~paste(name, ":", get(input$indicator)))
  })
  
  # Gráfico de Líneas (Interactivo)
  output$lineChart <- renderPlotly({
    req(input$country)
    p <- filtered_data() %>%
      plot_ly(x = ~year, y = ~get(input$indicator), type = 'scatter', mode = 'lines+markers',
              line = list(color = '#2563eb')) %>%
      layout(xaxis = list(title = "Año"), yaxis = list(title = input$indicator))
    p
  })
  
  # Gráfico de Barras
  output$barChart <- renderPlotly({
    data_2007 <- gapminder %>% 
      filter(year == 2007) %>%
      group_by(continent) %>%
      summarise(value = mean(get(input$indicator)))
    
    plot_ly(data_2007, x = ~continent, y = ~value, type = 'bar', marker = list(color = '#60a5fa')) %>%
      layout(xaxis = list(title = "Continente"), yaxis = list(title = "Promedio"))
  })
}

shinyApp(ui, server)

