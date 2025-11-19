library(shiny)
library(dplyr)
library(readr)
library(plotly)
library(ggplot2)

# ================================
# CONFIGURACIÓN DE COLORES
# ================================
color_titulo <- "#005CA9"    # azul institucional
color_sidebar <- "#F0F4F7"   # gris claro
color_borde <- "#D0D7DD"

# ================================
# UI
# ================================
ui <- fluidPage(

  # ---- ESTILOS CSS ----
  tags$head(
    tags$style(HTML(sprintf("
      /* Banda superior */
      .titulo-banner {
        background-color: %s;
        padding: 18px;
        color: white;
        font-size: 26px;
        font-weight: bold;
      }

      /* Sidebar */
      .sidebar-custom {
        background-color: %s;
        padding: 20px;
        border-right: 2px solid %s;
        height: 100vh;
      }

      .sidebar-logo {
        width: 120px;
        margin-bottom: 20px;
      }

      .fecha-reporte {
        margin-top: 15px;
        font-size: 13px;
        color: #555;
      }
    ", color_titulo, color_sidebar, color_borde)))
  ),

  # ---- BANNER SUPERIOR ----
  div(class = "titulo-banner", "UES O'Higgins – Reportes"),

  # ---- LAYOUT PRINCIPAL ----
  fluidRow(

    # --------- SIDEBAR IZQUIERDO ---------
    column(
      width = 2,
      div(class = "sidebar-custom",

          # Logo (opcional)
          img(src = "https://upload.wikimedia.org/wikipedia/commons/6/6b/Logo_Ministerio_de_Salud_de_Chile.png",
              class = "sidebar-logo"),

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

          # Fecha del reporte
          div(class = "fecha-reporte",
              paste("Fecha del reporte:", format(Sys.Date(), "%d-%m-%Y"))
          )
      )
    ),

    # --------- CONTENIDO PRINCIPAL ---------
    column(
      width = 10,

      # ---- FILTRO SUPERIOR ----
      uiOutput("filtros"),

      # ---- PESTAÑAS ----
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
      read_csv("data/coberturas.csv") %>%
        rename(
          categoria = categoria,
          x = x,
          y = y
        )

    } else if (input$reporte == "Reporte B – Influenza") {
      read_csv("data/influenza.csv") %>%
        rename(
          fecha = fecha,
          valor = valor,
          grupo = grupo
        )

    } else {
      read_csv("data/agentes.csv") %>%
        rename(
          x = x,
          y = y,
          region = region
        )
    }
  })

  # ---- FILTROS DINÁMICOS ----
  output$filtros <- renderUI({

    datos <- datos_reporte()

    if (input$reporte == "Reporte A – Coberturas") {
      selectInput("filtro_cat", "Categoría:", choices = unique(datos$categoria))

    } else if (input$reporte == "Reporte B – Influenza") {
      selectInput("filtro_grupo", "Grupo:", choices = unique(datos$grupo))

    } else {
      selectInput("filtro_region", "Región:", choices = unique(datos$region))
    }
  })

  # ---- DATOS FILTRADOS ----
  datos_filtrados <- reactive({

    datos <- datos_reporte()

    if (input$reporte == "Reporte A – Coberturas") {
      req(input$filtro_cat)
      datos %>% filter(categoria == input$filtro_cat)

    } else if (input$reporte == "Reporte B – Influenza") {
      req(input$filtro_grupo)
      datos %>% filter(grupo == input$filtro_grupo)

    } else {
      req(input$filtro_region)
      datos %>% filter(region == input$filtro_region)
    }
  })

  # ---- TABLA ----
  output$tabla <- renderTable({
    datos_filtrados()
  })

  # ---- GRÁFICO ----
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
