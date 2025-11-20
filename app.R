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

      /* --------------------------------
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

      /* --------------------------------
         SIDEBAR
      --------------------------------*/
      .sidebar-custom {
        background-color: %s;
        height: 100vh;
        color: white;
        padding: 15px;
        border-right: 2px solid #001b33;
      }

      .sidebar-logo {
        width: 120px;
        margin: 0 auto 20px auto;
        display: block;
      }

      /* --------------------------------
         PESTAÑAS (tabsetPanel)
      --------------------------------*/
      .nav-tabs {
        background-color: %s;
        border-bottom: 2px solid #001b33;
      }

      .nav-tabs > li > a {
        color: white !important;
        font-weight: bold;
      }

      /* Hover */
      .nav-tabs > li > a:hover {
        background-color: #001b33 !important;
        color: white !important;
      }

      /* Activa -> texto negro para contraste */
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        background-color: %s !important;
        color: black !important;
        border: none !important;
      }

      /* --------------------------------
         FILTROS HORIZONTALES
      --------------------------------*/
      .filtros-inline .form-group {
        display: inline-block;
        margin-right: 20px;
      }

    ", color_titulo, color_sidebar, color_tabs, color_tab_act)))
  ),

  # ================================
  # BARRA SUPERIOR
  # ================================
  div(class = "titulo-banner", "UES O'Higgins – Reportes"),

  fluidRow(

    # ================================
    # SIDEBAR
    # ================================
    column(
      width = 2,

      div(class = "sidebar-custom",

          # Logo arriba como antes
          img(src = "logo_blanco_ues.png", class = "sidebar-logo"),

          # Selector de reporte
          selectInput(
            "reporte",
            "Seleccione un reporte:",
            choices = c(
              "Reporte A – Coberturas",
              "Reporte B – Influenza",
              "Reporte C – Agentes Etiológicos"
            )
          ),

          # Fecha dinámica por reporte
          div(id = "fecha_dynamic", style = "margin-top: 15px; font-size: 13px;")
      )
    ),

    # ================================
    # CONTENIDO
    # ================================
    column(
      width = 10,

      # ---- Filtros ----
      div(class = "filtros-inline",
          uiOutput("filtros")
      ),

      # ---- Pestañas ----
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

  # --------------------------------------
  # FECHA DINÁMICA SEGÚN REPORTE
  # --------------------------------------
  observeEvent(input$reporte, {
    fecha_rep <- format(Sys.time(), "%d-%m-%Y %H:%M")  # más elegante
    texto <- paste("Fecha del reporte seleccionado:", fecha_rep)

    insertUI(
      selector = "#fecha_dynamic",
      where = "afterBegin",
      immediate = TRUE,
      ui = div(style="color:white;", texto)
    )
  })


  # **************************************
  # CARGA DE DATOS
  # **************************************
  datos_reporte <- reactive({

    if (input$reporte == "Reporte A – Coberturas") {
      read_csv("data/coberturas.csv")

    } else if (input$reporte == "Reporte B – Influenza") {
      read_csv("data/influenza.csv")

    } else {
      read_csv("data/agentes.csv")
    }
  })


  # **************************************
  # FILTROS
  # **************************************
  output$filtros <- renderUI({

    datos <- datos_reporte()

    if (input$reporte == "Reporte A – Coberturas") {
      selectInput(
        "filtro_categoria", "Categoría:",
        choices = c("Todos", unique(datos$categoria)),
        selected = "Todos", multiple = TRUE
      )

    } else if (input$reporte == "Reporte B – Influenza") {
      selectInput(
        "filtro_grupo", "Grupo:",
        choices = c("Todos", unique(datos$grupo)),
        selected = "Todos", multiple = TRUE
      )

    } else {
      selectInput(
        "filtro_region", "Región:",
        choices = c("Todos", unique(datos$region)),
        selected = "Todos", multiple = TRUE
      )
    }
  })


  # **************************************
  # FILTRADO DE DATOS
  # **************************************
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


  # **************************************
  # TABLA
  # **************************************
  output$tabla <- renderTable({
    datos_filtrados()
  })


  # **************************************
  # GRÁFICO
  # **************************************
  output$grafico <- renderPlotly({

    datos <- datos_filtrados()

    if (input$reporte == "Reporte A – Coberturas") {

      p <- ggplot(datos, aes(x = categoria, y = y)) +
        geom_col(fill = "#1f77b4") +
        theme_minimal()

    } else if (input$reporte == "Reporte B – Influenza") {

      p <- ggplot(datos, aes(x = fecha, y = valor)) +
        geom_line(color = "#d62728") +
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

