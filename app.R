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
      /* Barra superior */
      .titulo-banner {
        background-color: %s !important;
        padding: 18px;
        color: white !important;
        font-size: 26px;
        font-weight: bold;
        text-align: center;
      }

      /* Sidebar */
      .sidebar-custom {
        background-color: %s !important;
        padding: 20px;
        height: 100vh;
        color: white !important;
      }

      /* Logo */
      .sidebar-logo {
        width: 120px;
        display: block;
        margin-left: auto;
        margin-right: auto;
        margin-bottom: 20px;
      }

      /* Filtros horizontales */
      .filtros-inline > * {
        display: inline-block;
        margin-right: 20px;
      }

      /* Tabs */
      .nav-tabs > li > a {
        background-color: %s !important;
        border: 1px solid %s !important;
        color: black !important;
      }

      .nav-tabs > li.active > a {
        background-color: %s !important;
        color: black !important;
        border: 1px solid %s !important;
        font-weight: bold !important;
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

    # ==== SIDEBAR IZQUIERDA ====
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
              style = "margin-top:20px; font-size:14px; color:white;",
              paste("Fecha del reporte:", format(Sys.Date(), "%d-%m-%Y"))
          )
      )
    ),

    # ==== CONTENIDO PRINCIPAL ====
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

  # ACTUALIZAR FECHA DINÁMICAMENTE
  observeEvent(input$reporte, {
    updateText <- paste("Fecha del reporte:", format(Sys.Date(), "%d-%m-%Y"))
    insertUI("#fecha_texto", where = "afterEnd", ui = NULL)
    removeUI(selector = "#fecha_texto", multiple = TRUE)
    insertUI(
      selector = ".sidebar-custom",
      where = "beforeEnd",
      ui = div(id = "fecha_texto",
               style = "margin-top:20px; font-size:14px; color:white;",
               updateText)
    )
  })

  # ----------------------------
  # CARGA DE DATOS REACTIVA
  # ----------------------------
  get_dataset <- reactive({

    if (input$reporte == "Reporte A – Coberturas") {
      df <- read_csv("data/coberturas.csv")
      list(df = df, var_main = "categoria")
    }

    else if (input$reporte == "Reporte B – Influenza") {
      df <- read_csv("data/influenza.csv")
      list(df = df, var_main = "grupo")
    }

    else {
      df <- read_csv("data/agentes.csv")
      list(df = df, var_main = "region")
    }
  })

  # ----------------------------
  # FILTROS DINÁMICOS
  # ----------------------------
  output$filtros_ui <- renderUI({
    ds <- get_dataset()$df
    var_main <- get_dataset()$var_main

    if (is.null(ds)) return(NULL)

    filtros <- tagList(
      selectInput(
        "filtro_main",
        "Filtro principal:",
        choices = c("Todos", sort(unique(ds[[var_main]]))),
        selected = "Todos",
        multiple = TRUE,
        width = "250px"
      )
    )

    if ("sexo" %in% names(ds)) {
      filtros <- tagList(
        filtros,
        selectInput(
          "filtro_sexo",
          "Sexo:",
          choices = c("Todos", sort(unique(ds$sexo))),
          selected = "Todos",
          multiple = TRUE
        )
      )
    }

    if ("edad" %in% names(ds)) {
      filtros <- tagList(
        filtros,
        selectInput(
          "filtro_edad",
          "Edad:",
          choices = c("Todos", sort(unique(ds$edad))),
          selected = "Todos",
          multiple = TRUE
        )
      )
    }

    div(class = "filtros-inline", filtros)
  })

  # ----------------------------
  # APLICAR FILTROS
  # ----------------------------
  datos_filtrados <- reactive({
    ds <- get_dataset()$df
    var_main <- get_dataset()$var_main

    df <- ds

    if (!"Todos" %in% input$filtro_main)
      df <- df %>% filter(.data[[var_main]] %in% input$filtro_main)

    if (!is.null(input$filtro_sexo) && !"Todos" %in% input$filtro_sexo)
      df <- df %>% filter(sexo %in% input$filtro_sexo)

    if (!is.null(input$filtro_edad) && !"Todos" %in% input$filtro_edad)
      df <- df %>% filter(edad %in% input$filtro_edad)

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

    if (input$reporte == "Reporte B – Influenza") {

      if (!inherits(df$fecha, "Date"))
        df$fecha <- as.Date(df$fecha)

      p <- ggplot(df, aes(x = fecha, y = valor, color = grupo)) +
        geom_line() + geom_point() + theme_minimal()

    } else if (input$reporte == "Reporte C – Agentes Etiológicos") {

      p <- ggplot(df, aes(x = x, y = y, fill = region)) +
        geom_col() + theme_minimal()

    } else {

      p <- ggplot(df, aes(x = categoria, y = y)) +
        geom_col(fill = "#1f77b4") + theme_minimal()
    }

    ggplotly(p)
  })
}

shinyApp(ui, server)
