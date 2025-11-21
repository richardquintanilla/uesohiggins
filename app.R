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

  tags$head(
    tags$style(HTML(sprintf("
      .titulo-banner {
        background-color: %s !important;
        padding: 18px;
        color: white !important;
        font-size: 26px;
        font-weight: bold;
        text-align: center;
      }
      .sidebar-custom {
        background-color: %s !important;
        padding: 20px;
        height: 100vh;
        color: white !important;
        text-align: center;
      }
      .sidebar-logo {
        width: 120px;
        display: block;
        margin-left: auto;
        margin-right: auto;
        margin-bottom: 20px;
      }
      .filtros-inline > div {
        display: inline-block;
        margin-right: 20px;
        vertical-align: top;
      }
      .nav-tabs > li > a {
        background-color: %s !important;
        border: 1px solid %s !important;
        color: black !important;
      }
      .nav-tabs > li.active > a {
        background-color: %s !important;
        border: 1px solid %s !important;
        color: black !important;
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

  # Actualizar fecha dinámicamente
  observeEvent(input$reporte, {
    updateText <- paste("Fecha del reporte:", format(Sys.Date(), "%d-%m-%Y"))

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

    if (input$reporte == "Reporte B – Influenza") {
      df <- read_csv("data/influenza.csv")
      list(df = df, var_main = "grupo")
    }

    df <- read_csv("data/agentes.csv")
    list(df = df, var_main = "region")
  })

  # ----------------------------
  # FILTROS DINÁMICOS (HORIZONTALES)
  # ----------------------------
  output$filtros_ui <- renderUI({
    ds <- get_dataset()$df
    var_main <- get_dataset()$var_main

    if (is.null(ds)) return(NULL)

    # función para crear selectize con checkbox + select all dentro
    make_filter <- function(id, label, choices) {
      div(
        width = "250px",
        selectizeInput(
          inputId = id,
          label = label,
          choices = choices,
          selected = choices,
          multiple = TRUE,
          options = list(
            plugins = c("remove_button"),
            placeholder = "Seleccione...",
            onInitialize = I(
              'function() {
                var selectize = this;
                var selectAll = $("<div style=\'padding:6px; cursor:pointer; font-weight:bold;\'>✓ Seleccionar todo</div>");
                var deselectAll = $("<div style=\'padding:6px; cursor:pointer; font-weight:bold;\'>✗ Deseleccionar todo</div>");

                selectAll.on("click", function() {
                  selectize.setValue(selectize.options.map(x => x.value));
                });

                deselectAll.on("click", function() {
                  selectize.clear();
                });

                this.$dropdown.prepend(deselectAll);
                this.$dropdown.prepend(selectAll);
              }')
          )
        )
      )
    }

    filtros <- tagList(
      make_filter("filtro_main", "Filtro principal:", sort(unique(ds[[var_main]])))
    )

    if ("sexo" %in% names(ds)) {
      filtros <- tagList(filtros,
                         make_filter("filtro_sexo", "Sexo:", sort(unique(ds$sexo))))
    }

    if ("edad" %in% names(ds)) {
      filtros <- tagList(filtros,
                         make_filter("filtro_edad", "Edad:", sort(unique(ds$edad))))
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

    if (!is.null(input$filtro_main)) {
      df <- df %>% filter(.data[[var_main]] %in% input$filtro_main)
    }

    if (!is.null(input$filtro_sexo) && "sexo" %in% names(df)) {
      df <- df %>% filter(sexo %in% input$filtro_sexo)
    }

    if (!is.null(input$filtro_edad) && "edad" %in% names(df)) {
      df <- df %>% filter(edad %in% input$filtro_edad)
    }

    df
  })

  # ----------------------------
  # TABLA
  # ----------------------------
  output$tabla <- renderTable({
    datos_filtrados()
  }, rownames = TRUE)

  # ----------------------------
  # GRÁFICO
  # ----------------------------
  output$grafico <- renderPlotly({
    df <- datos_filtrados()
    info <- get_dataset()

    if (nrow(df) == 0) {
      p <- ggplot() + theme_void() + ggtitle("No hay datos para mostrar")
      return(ggplotly(p))
    }

    # Influenza
    if (input$reporte == "Reporte B – Influenza") {

      if ("fecha" %in% names(df) && !inherits(df$fecha, "Date")) {
        suppressWarnings(df$fecha <- as.Date(df$fecha))
      }

      p <- ggplot(df, aes(x = fecha, y = valor, color = grupo)) +
        geom_line() + geom_point() + theme_minimal()
    }

    # Agentes
    else if (input$reporte == "Reporte C – Agentes Etiológicos") {

      p <- ggplot(df, aes(x = x, y = y, fill = region)) +
        geom_col() + theme_minimal()
    }

    # Coberturas
    else {
      p <- ggplot(df, aes(x = categoria, y = y)) +
        geom_col(fill = "#1f77b4") + theme_minimal()
    }

    ggplotly(p)
  })
}

shinyApp(ui, server)
