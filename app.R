library(shiny)
library(dplyr)
library(readr)
library(plotly)
library(ggplot2)

# ================================
# CONFIGURACIÓN DE COLORES
# ================================
color_titulo <- "#005CA9"
color_sidebar <- "#F0F4F7"
color_borde <- "#D0D7DD"

# ================================
# UI
# ================================
ui <- fluidPage(

  # ---- ESTILOS CSS ----
  tags$head(
    tags$style(HTML(sprintf("
      .titulo-banner {
        background-color: %s;
        padding: 18px;
        color: white;
        font-size: 26px;
        font-weight: bold;
      }

      .sidebar-custom {
        background-color: %s;
        padding: 20px;
        border-right: 2px solid %s;
        height: 100vh;
      }

      .sidebar-logo {
        width: 140px;   /* <- TAMAÑO FIJO */
        margin-bottom: 20px;
      }

      .fecha-reporte {
        margin-top: 15px;
        font-size: 13px;
        color: #555;
      }

      .filtro-box {
        display: inline-block;
        margin-right: 20px;
        vertical-align: top;
      }

    ", color_titulo, color_sidebar, color_borde)))
  ),

  # ---- BANNER SUPERIOR ----
  div(class = "titulo-banner", "UES O'Higgins – Reportes"),

  # ---- LAYOUT PRINCIPAL ----
  fluidRow(

    # -------- SIDEBAR --------
    column(
      width = 2,
      div(class = "sidebar-custom",

          img(src = "logo_blanco_ues.png", class = "sidebar-logo"),

          selectInput(
            "reporte",
            "Seleccione un reporte:",
            choices = c(
              "Reporte A – Coberturas",
              "Reporte B – Influenza",
              "Reporte C – Agentes Etiológicos"
            )
          ),

          div(class = "fecha-reporte",
              paste("Fecha del reporte:", format(Sys.Date(), "%d-%m-%Y"))
          )
      )
    ),

    # -------- CONTENIDO --------
    column(
      width = 10,

      uiOutput("filtros"),

      tabsetPanel(
        tabPanel("Tabla", tableOutput("tabla")),
        tabPanel("Gráfico", plotlyOutput("grafico"))
      )
    )
  )
)

# ================================
# SERVER
# ================================
server <- function(input, output, session) {

  # ---- CARGA REACTIVA DE DATOS ----
  datos_reporte <- reactive({

    if (input$reporte == "Reporte A – Coberturas") {
      read_csv("data/coberturas.csv")

    } else if (input$reporte == "Reporte B – Influenza") {
      read_csv("data/influenza.csv")

    } else {
      read_csv("data/agentes.csv")
    }
  })

  # ---- FILTROS ----
  output$filtros <- renderUI({

    datos <- datos_reporte()

    fluidRow(

      # ---- FILTRO PRINCIPAL ----
      div(class = "filtro-box",
          if (input$reporte == "Reporte A – Coberturas") {
            selectInput(
              "filtro_cat",
              "Categoría:",
              choices = c("Todos", unique(datos$categoria)),
              multiple = TRUE
            )

          } else if (input$reporte == "Reporte B – Influenza") {
            selectInput(
              "filtro_grupo",
              "Grupo:",
              choices = c("Todos", unique(datos$grupo)),
              multiple = TRUE
            )

          } else {
            selectInput(
              "filtro_region",
              "Región:",
              choices = c("Todos", unique(datos$region)),
              multiple = TRUE
            )
          }
      ),

      # ---- SEXO ----
      div(class = "filtro-box",
          selectInput(
            "filtro_sexo",
            "Sexo:",
            choices = c("Todos", "Hombre", "Mujer"),
            multiple = TRUE,
            selected = "Todos"
          )
      ),

      # ---- EDAD ----
      div(class = "filtro-box",
          selectInput(
            "filtro_edad",
            "Edad:",
            choices = c("Todos", "0-4", "5-14", "15-64", "65+"),
            multiple = TRUE,
            selected = "Todos"
          )
      )
    )
  })

  # ================================
  # APLICACIÓN DE FILTROS
  # ================================
  datos_filtrados <- reactive({

    datos <- datos_reporte()

    # ---- Filtro principal ----
    if (input$reporte == "Reporte A – Coberturas") {
      if (!("Todos" %in% input$filtro_cat))
        datos <- datos %>% filter(categoria %in% input$filtro_cat)

    } else if (input$reporte == "Reporte B – Influenza") {
      if (!("Todos" %in% input$filtro_grupo))
        datos <- datos %>% filter(grupo %in% input$filtro_grupo)

    } else {
      if (!("Todos" %in% input$filtro_region))
        datos <- datos %>% filter(region %in% input$filtro_region)
    }

    # ---- Filtro sexo ----
    if (!("Todos" %in% input$filtro_sexo)) {
      datos <- datos %>% filter(sexo %in% input$filtro_sexo)
    }

    # ---- Filtro edad ----
    if (!("Todos" %in% input$filtro_edad)) {
      datos <- datos %>% filter(edad %in% input$filtro_edad)
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
