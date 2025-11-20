library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)

ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  
  # TÍTULO
  titlePanel("Dashboard de Reportes"),
  
  # LOGO SUPERIOR
  div(
    id = "logo_container",
    img(src = "logo_blanco_ues.png", id = "logo")
  ),
  
  sidebarLayout(
    
    # ---------- SIDEBAR ----------
    sidebarPanel(
      id = "sidebar",
      
      h4("Filtros", style = "color:white;"),
      
      # Reporte seleccionado
      selectInput(
        "reporte",
        "Seleccionar reporte:",
        choices = c("Agentes", "Coberturas", "Influenza"),
        selected = "Agentes"
      ),
      
      # Filtros múltiples
      uiOutput("filtro_x"),
      uiOutput("filtro_y"),
      uiOutput("filtro_extra"),
      
      width = 3
    ),
    
    # ---------- MAIN PANEL ----------
    mainPanel(
      
      # Fecha dinámica
      div(id = "fecha_mostrada", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),
      
      tabsetPanel(
        id = "tabs",
        
        tabPanel("Tabla",
                 tableOutput("tabla")
        ),
        
        tabPanel("Gráfico",
                 plotlyOutput("grafico", height = "450px")
        )
      ),
      width = 9
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  
  # ---- Ejemplo de datos temporales ----
  agentes <- data.frame(
    x = c("A","B","C"),
    y = c(10,20,30),
    region = c("Norte","Centro","Sur")
  )
  
  coberturas <- data.frame(
    x = c("A","A","B","B"),
    y = c(80,90,70,85),
    categoria = c("Infantil","Adulto","Infantil","Adulto")
  )
  
  influenza <- data.frame(
    fecha = as.Date(c("2024-01-01","2024-02-01","2024-03-01")),
    valor = c(200,300,250),
    grupo = c("Niños","Adultos","Adultos")
  )
  
  # ----------- Fecha dinámica -----------
  observe({
    texto <- paste("Fecha del reporte:", Sys.Date())
    output$fecha_mostrada <- renderUI({
      div(style = "font-weight:bold; font-size:16px;", texto)
    })
  })
  
  # ----------- Filtros según reporte -----------
  
  output$filtro_x <- renderUI({
    req(input$reporte)
    datos <- switch(input$reporte,
                    "Agentes" = agentes,
                    "Coberturas" = coberturas,
                    "Influenza" = influenza)
    
    if ("x" %in% names(datos)) {
      selectInput("x", "X:", choices = c("Todos", unique(datos$x)),
                  selected = "Todos", multiple = TRUE)
    }
  })
  
  output$filtro_y <- renderUI({
    req(input$reporte)
    datos <- switch(input$reporte,
                    "Agentes" = agentes,
                    "Coberturas" = coberturas,
                    "Influenza" = influenza)
    
    if ("y" %in% names(datos)) {
      selectInput("y", "Y:", choices = c("Todos", unique(datos$y)),
                  selected = "Todos", multiple = TRUE)
    }
  })
  
  output$filtro_extra <- renderUI({
    req(input$reporte)
    
    if (input$reporte == "Agentes")
      return(selectInput("extra", "Región:",
                         choices = c("Todos", unique(agentes$region)),
                         multiple = TRUE, selected = "Todos"))
    
    if (input$reporte == "Coberturas")
      return(selectInput("extra", "Categoria:",
                         choices = c("Todos", unique(coberturas$categoria)),
                         multiple = TRUE, selected = "Todos"))
    
    if (input$reporte == "Influenza")
      return(selectInput("extra", "Grupo:",
                         choices = c("Todos", unique(influenza$grupo)),
                         multiple = TRUE, selected = "Todos"))
  })
  
  # ----------- Datos filtrados -----------
  datos_filtrados <- reactive({
    req(input$reporte)
    
    df <- switch(input$reporte,
                 "Agentes" = agentes,
                 "Coberturas" = coberturas,
                 "Influenza" = influenza)
    
    if (!is.null(input$x) && !("Todos" %in% input$x) && "x" %in% names(df))
      df <- df[df$x %in% input$x, ]
    
    if (!is.null(input$y) && !("Todos" %in% input$y) && "y" %in% names(df))
      df <- df[df$y %in% input$y, ]
    
    if (!is.null(input$extra) && !("Todos" %in% input$extra)) {
      col <- switch(input$reporte,
                    "Agentes" = "region",
                    "Coberturas" = "categoria",
                    "Influenza" = "grupo")
      df <- df[df[[col]] %in% input$extra, ]
    }
    
    return(df)
  })
  
  # ----------- Tabla -----------
  output$tabla <- renderTable({
    datos_filtrados()
  })
  
  # ----------- Gráfico -----------
  output$grafico <- renderPlotly({
    df <- datos_filtrados()
    
    plot_ly(df, x = ~names(df)[1], y = ~names(df)[2], type = "bar")
  })
}

shinyApp(ui, server)
