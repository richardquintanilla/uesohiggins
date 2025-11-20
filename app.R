library(shiny)
library(dplyr)
library(readr)
library(plotly)
library(ggplot2)

# ================================
# COLORES
# ================================
COLOR_BARRA <- "#191970"
COLOR_TAB_ACTIVA <- "#EEE9E9"
COLOR_TAB_INACTIVA <- "#f5f5f5"
COLOR_BORDE_TAB <- "white"


# ================================
# UI
# ================================
ui <- fluidPage(

  # ---- CSS GENERAL ----
  tags$head(
    tags$style(HTML(sprintf("
      .titulo-banner {
        background-color: %s;
        padding: 18px;
        color: white;
        font-size: 26px;
        font-weight: bold;
        text-align: center;
      }

      .sidebar-custom {
        background-color: %s;
        padding: 20px;
        height: 100vh;
        color: white;
      }

      .sidebar-logo {
        width: 120px;
        display: block;
        margin-left: auto;
        margin-right: auto;
        margin-bottom: 20px;
      }

      .filtros-inline > * {
        display: inline-block;
        margin-right: 20px;
      }

      .nav-tabs > li > a {
        background-color: %s !important;
        border-color: %s !important;
        color: black !important;
      }

      .nav-tabs > li.active > a {
        background-color: %s !important;
        color: black !important;
        border-color: %s !important;
        font-weight: bold;
      }
    ",
    COLOR_BARRA,
    COLOR_BARRA,
    COLOR_TAB_INACTIVA,
    COLOR_BORDE_TAB,
    COLOR_TAB_ACTIVA,
    COLOR_BORDE_TAB
    )))
  ),

  # ---- BARRA SUPERIOR ----
  div(class = "titulo-banner", "UES O'Higgins – Reportes"),

  fluidRow(

    # ======================
    # SIDEBAR IZQUIERDA
    # ======================
    column(
      width = 2,
      div(class = "sidebar-custom",

          img(src = "logo_ues_blanco.png", class = "sidebar-logo"),

          selectInput(
            "reporte",
            "Seleccione un reporte:",
            choices = c(
              "Reporte A – Coberturas",
              "Reporte B – Influenza",
              "Reporte C – Agentes Etiológicos"
            )
          ),

          div(id = "fecha_texto",
              style = "margin-top:20px; font-size:14px;",
              paste("Fecha del reporte:", format(Sys.Date(), "%d-%m-%Y"))
          )
      )
    ),

    # ======================
    # CONTENIDO PRINCIPAL
    # ======================
    column(
      width = 10,

      uiOutput("filtros_ui"),
      br(),

      tabsetPanel(
        id = "tabs",
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

  # ----------------------------
  # CARGA DE DATOS SEGÚN REPORTE
  # ----------------------------
  get_dataset <- reactive({

    if (input$reporte == "Reporte A – Coberturas") {
      df <- read_csv("data/coberturas.csv")
      list(df = df, var_main = "categoria", var_num = "y")
    }

    else if (input$reporte == "Reporte B – Influenza") {
      df <- read_csv("data/influenza.csv")
      list(df = df, var_main = "grupo", var_num = "valor")
    }

    else {
      df <- read_csv("data/agentes.csv")
      list(df = df, var_main = "region", var_num = "y")
    }
  })

  # ----------------------------
  # FILTROS DINÁMICOS (siempre 2)
  # ----------------------------
  output$filtros_ui <- renderUI({

    ds_info <- get_dataset()
    df <- ds_info$df
    var_main <- ds_info$var_main
    var_num <- ds_info$var_num

    tagList(
      div(class = "filtros-inline",

          # Filtro principal (siempre existe)
          selectInput(
            "filtro_main",
            "Filtro principal:",
            choices = c("Todos", sort(unique(df[[var_main]]))),
            selected = "Todos",
            multiple = TRUE,
            width = "250px"
          ),

          # Filtro numérico adicional (siempre existe)
          sliderInput(
            "filtro_rango",
            label = paste("Filtrar", var_num, "(mín–máx):"),
            min = min(df[[var_num]], na.rm = TRUE),
            max = max(df[[var_num]], na.rm = TRUE),
            value = c(min(df[[var_num]], na.rm = TRUE),
                      max(df[[var_num]], na.rm = TRUE)),
            width = "350px"
          )
      )
    )
  })

  # ----------------------------
  # APLICAR FILTROS
  # ----------------------------
  datos_filtrados <- reactive({

    ds_info <- get_dataset()
    df <- ds_info$df
    var_main <- ds_info$var_main
    var_num <- ds_info$var_num

    # Filtro 1
    if (!"Todos" %in% input$filtro_main)
      df <- df %>% filter(.data[[var_main]] %in% input$filtro_main)

    # Filtro 2 (rango)
    df <- df %>% filter(.data[[var_num]] >= input$filtro_rango[1],
                        .data[[var_num]] <= input$filtro_rango[2])

    df
  })

  # ----------------------------
  # TABLA
  # ----------------------------
  output$tabla <- renderTable({
    datos_filtrados()
  })

  # ----------------------------
  # GRÁFICO
  # ----------------------------
  output$grafico <- renderPlotly({

    df <- datos_filtrados()
    ds_info <- get_dataset()

    if (input$reporte == "Reporte B – Influenza") {

      df$fecha <- as.Date(df$fecha)

      p <- ggplot(df, aes(x = fecha, y = valor, color = grupo)) +
        geom_line() + geom_point() +
        theme_minimal()

    } else {

      p <- ggplot(df, aes_string(x = ds_info$var_main, y = ds_info$var_num)) +
        geom_col(fill = "#1f77b4") +
        theme_minimal()
    }

    ggplotly(p)
  })
}

shinyApp(ui, server)
