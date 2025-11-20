library(shiny)
library(dplyr)
library(readr)
library(plotly)
library(ggplot2)

# ================================
# CONFIGURACIÓN DE COLORES
# ================================
color_titulo   <- "#191970"   # barra superior
color_sidebar  <- "#191970"   # sidebar
color_tabs     <- "#191970"   # fondo pestañas
color_tab_act  <- "#EEE9E9"   # pestaña activa


# ================================
# UI
# ================================
ui <- fluidPage(
  
  # ================================
  # CSS GLOBAL
  # ================================
  tags$head(
    tags$style(HTML(sprintf("

      /* -------------------------------
         BARRA SUPERIOR
      --------------------------------*/
      .titulo-banner {
        background-color: %s;
        padding: 18px;
        color: white;
        font-size: 28px;
        font-weight: bold;
        text-align: center;
        margin-bottom: 8px;
      }

      /* -------------------------------
         SIDEBAR
      --------------------------------*/
      .sidebar-custom {
        background-color: %s;
        height: 100vh;
        color: white;
        padding: 15px;
        border-right: 2px solid #001b33;
        position: relative;
      }

      .sidebar-logo {
        width: 120px;
        margin: 0 auto;
        display: block;
      }

      /* Logo abajo centrado */
      .logo-bottom {
        position: absolute;
        bottom: 15px;
        width: 100%%;
        text-align: center;
      }

      /* -------------------------------
         PESTAÑAS
      --------------------------------*/
      .nav-tabs {
        background-color: %s;
        border-bottom: 2px solid %s;
      }

      .nav-tabs > li > a {
        color: white !important;
        font-weight: bold;
      }

      .nav-tabs > li > a:hover {
        background-color: %s !important;
        color: #fff !important;
      }

      .nav-tabs > li.active > a {
        background-color: %s !important;
        color: white !important;
        border: none !important;
      }

      /* -------------------------------
         FILTROS HORIZONTALES
      --------------------------------*/
      .filtros-inline .form-group {
        display: inline-block;
        margin-right: 20px;
      }

    ", color_titulo, color_sidebar, color_tabs, color_tab_act,
                            color_tab_act, color_tab_act
    )))
  ),
  
  # ================================
  # BARRA SUPERIOR
  # ================================
  div(class = "titulo-banner", "UES O'Higgins – Reportes"),
  
  fluidRow(
    
    # ================================
    # SIDEBAR IZQUIERDO
    # ================================
    column(
      width = 2,
      
      div(class = "sidebar-custom",
          
          # ---- Selector de reporte ----
          selectInput(
            "reporte",
            "Seleccione un reporte:",
            choices = c(
              "Reporte A – Coberturas",
              "Reporte B – Influenza",
              "Reporte C – Agentes Etiológicos"
            )
          ),
          
          # ---- Fecha ----
          div(style = "margin-top: 15px; font-size: 13px;",
              paste("Fecha del reporte:", format(Sys.Date(), "%d-%m-%Y"))
          ),
          
          # ---- LOGO ABAJO ----
          div(class = "logo-bottom",
              img(src = "logo_blanco_ues.png", class = "sidebar-logo")
          )
      )
    ),
    
    # ================================
    # CONTENIDO PRINCIPAL
    # ================================
    column(
      width = 10,
      
      # ---- FILTROS SUPERIORES ----
      div(class = "filtros-inline",
          uiOutput("filtros")
      ),
      
      # ---- PESTAÑAS ----
      tabsetPanel(
        tabPanel("Tabla", br(), tableOutput("tabla")),
        tabPanel("Gráfico", br(), plotlyOutput("grafico"))
      )
    )
  )
)


# ================================
# SERVER
# ================================
server <- function(input, output, session) {
  
  # CARGA LOS DATOS SEGÚN REPORTE
  datos_reporte <- reactive({
    
    if (input$reporte == "Reporte A – Coberturas") {
      read_csv("data/coberturas.csv")
      
    } else if (input$reporte == "Reporte B – Influenza") {
      read_csv("data/influenza.csv")
      
    } else {
      read_csv("data/agentes.csv")
    }
  })
  
  # ================================
  # FILTROS DINÁMICOS
  # ================================
  output$filtros <- renderUI({
    
    datos <- datos_reporte()
    
    if (input$reporte == "Reporte A – Coberturas") {
      tagList(
        selectInput(
          "filtro_categoria",
          "Categoría:",
          choices = c("Todos", unique(datos$categoria)),
          selected = "Todos",
          multiple = TRUE
        )
      )
      
    } else if (input$reporte == "Reporte B – Influenza") {
      tagList(
        selectInput(
          "filtro_grupo",
          "Grupo:",
          choices = c("Todos", unique(datos$grupo)),
          selected = "Todos",
          multiple = TRUE
        )
      )
      
    } else {
      tagList(
        selectInput(
          "filtro_region",
          "Región:",
          choices = c("Todos", unique(datos$region)),
          selected = "Todos",
          multiple = TRUE
        )
      )
    }
  })
  
  # ================================
  # DATOS FILTRADOS
  # ================================
  datos_filtrados <- reactive({
    
    datos <- datos_reporte()
    
    if (input$reporte == "Reporte A – Coberturas") {
      if (!"Todos" %in% input$filtro_categoria)
        datos <- datos %>% filter(categoria %in% input$filtro_categoria)
      
    } else if (input$reporte == "Reporte B – Influenza") {
      if (!"Todos" %in% input$filtro_grupo)
        datos <- datos %>% filter(grupo %in% input$filtro_grupo)
      
    } else {
      if (!"Todos" %in% input$filtro_region)
        datos <- datos %>% filter(region %in% input$filtro_region)
    }
    
    datos
  })
  
  # ================================
  # TABLA
  # ================================
  output$tabla <- renderTable({
    datos_filtrados()
  })
  
  # ================================
  # GRÁFICO
  # ================================
  output$grafico <- renderPlotly({
    
    datos <- datos_filtrados()
    
    if (input$reporte == "Reporte A – Coberturas") {
      
      p <- ggplot(datos, aes(x = categoria, y = y)) +
        geom_col() +
        theme_minimal()
      
    } else if (input$reporte == "Reporte B – Influenza") {
      
      p <- ggplot(datos, aes(x = fecha, y = valor)) +
        geom_line() +
        theme_minimal()
      
    } else {
      
      p <- ggplot(datos, aes(x = x, y = y, fill = region)) +
        geom_col() +
        theme_minimal()
    }
    
    ggplotly(p)
  })
}

shinyApp(ui, server)

