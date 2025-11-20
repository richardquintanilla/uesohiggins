# app.R
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)

# ---------------------------
# CARGA DE DATOS (UNA VEZ)
# ---------------------------
# Lee los CSV una sola vez al iniciar la app (evita lecturas repetidas)
coberturas <- tryCatch(read_csv("data/coberturas.csv"), error = function(e) NULL)
influenza  <- tryCatch(read_csv("data/influenza.csv"), error = function(e) NULL)
agentes    <- tryCatch(read_csv("data/agentes.csv"), error = function(e) NULL)

# ---------------------------
# CSS / COLORES
# ---------------------------
sidebar_bg <- "#191970"
tab_active_bg <- "#EEE9E9"

custom_css <- HTML(sprintf("
/* Header */
.skin-blue .main-header .navbar { background-color: %s !important; }
.skin-blue .main-header .logo { background-color: %s !important; }

/* Sidebar */
.main-sidebar { background-color: %s !important; color: white; }
.sidebar .user-panel > .info { color: white !important; }

/* Sidebar links */
.sidebar a { color: white !important; }

/* Logo in sidebar top */
.sidebar-logo { display:block; margin: 10px auto 12px auto; max-width:140px; }

/* Tab styling (tabsetPanel) */
.nav-tabs { background-color: %s; border-bottom: 1px solid #ffffff; }
.nav-tabs > li > a { color: white !important; font-weight: 600; }
.nav-tabs > li > a:hover { background-color: #101830 !important; color: white !important; }

/* Active tab: light background and black text + white border */
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:focus,
.nav-tabs > li.active > a:hover {
  background-color: %s !important;
  color: black !important;
  border: 1px solid #ffffff !important;
}

/* Filtros inline (compact) */
.filtros-inline .form-group { display:inline-block; margin-right: 18px; vertical-align: middle; }
", sidebar_bg, sidebar_bg, sidebar_bg, sidebar_bg, tab_active_bg))


# ---------------------------
# UI
# ---------------------------
header <- dashboardHeader(title = "UES O'Higgins – Reportes", titleWidth = 300)

sidebar <- dashboardSidebar(
  width = 300,
  # Logo arriba (archivo en /www/logo_blanco_ues.png)
  tags$img(src = "logo_blanco_ues.png", class = "sidebar-logo", alt = "logo"),
  # Selector principal de reporte
  selectInput(
    "reporte",
    "Seleccione un reporte:",
    choices = c("Reporte A – Coberturas", "Reporte B – Influenza", "Reporte C – Agentes Etiológicos"),
    selected = "Reporte A – Coberturas"
  ),
  # Fecha dinámica (se actualiza según el reporte)
  tags$div(textOutput("fecha_reporte"), style = "color: white; margin-top: 8px; font-size: 13px;")
)

body <- dashboardBody(
  tags$head(tags$style(custom_css)),
  fluidRow(
    box(width = 12, solidHeader = FALSE,
        # FILTROS HORIZONTALES (se generan dinámicamente)
        div(class = "filtros-inline", uiOutput("filtros_ui"))
    )
  ),
  fluidRow(
    box(width = 12, solidHeader = FALSE,
        tabsetPanel(
          tabPanel("Tabla", br(), DTOutput("tabla_dt")),
          tabPanel("Gráfico", br(), plotlyOutput("grafico", height = "600px"))
        )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "blue")


# ---------------------------
# SERVER
# ---------------------------
server <- function(input, output, session) {

  # Helper: get dataset and filepath by report
  get_dataset <- reactive({
    switch(input$reporte,
           "Reporte A – Coberturas" = list(df = coberturas, path = "data/coberturas.csv"),
           "Reporte B – Influenza"  = list(df = influenza,  path = "data/influenza.csv"),
           "Reporte C – Agentes Etiológicos" = list(df = agentes, path = "data/agentes.csv")
    )
  })

  # Fecha dinámiva: fecha de modificación del archivo si existe, si no, Sys.time()
  output$fecha_reporte <- renderText({
    ds <- get_dataset()
    p <- ds$path
    if (!is.null(p) && file.exists(p)) {
      fm <- file.info(p)$mtime
      paste0("Fecha del reporte: ", format(fm, "%d-%m-%Y %H:%M"))
    } else {
      paste0("Fecha del reporte: ", format(Sys.time(), "%d-%m-%Y %H:%M"))
    }
  })

  # Genera los filtros (main, sexo, edad) dinámicamente y horizontalmente
  output$filtros_ui <- renderUI({
    ds <- get_dataset()$df
    if (is.null(ds)) return(NULL)

    main_choices <- switch(input$reporte,
                           "Reporte A – Coberturas" = if ("categoria" %in% names(ds)) c("Todos", sort(unique(ds$categoria))) else "Todos",
                           "Reporte B – Influenza"  = if ("grupo" %in% names(ds)) c("Todos", sort(unique(ds$grupo))) else "Todos",
                           "Reporte C – Agentes Etiológicos" = if ("region" %in% names(ds)) c("Todos", sort(unique(ds$region))) else "Todos"
    )

    # main filter id: filtro_main
    main_input <- selectInput("filtro_main", label = "Filtro principal:",
                              choices = main_choices,
                              selected = "Todos",
                              multiple = TRUE)

    # sexo (only if present)
    sexo_input <- NULL
    if ("sexo" %in% names(ds)) {
      sexo_choices <- c("Todos", sort(unique(ds$sexo)))
      sexo_input <- selectInput("filtro_sexo", label = "Sexo:", choices = sexo_choices,
                                selected = "Todos", multiple = TRUE)
    } else {
      # create a hidden empty input so server code can rely on existence
      sexo_input <- tags$span(style="display:none;", selectInput("filtro_sexo", label = NULL, choices = "Todos", selected = "Todos", multiple=TRUE))
    }

    # edad (only if present)
    edad_input <- NULL
    if ("edad" %in% names(ds)) {
      edad_choices <- c("Todos", sort(unique(ds$edad)))
      edad_input <- selectInput("filtro_edad", label = "Edad:", choices = edad_choices,
                                selected = "Todos", multiple = TRUE)
    } else {
      edad_input <- tags$span(style="display:none;", selectInput("filtro_edad", label = NULL, choices = "Todos", selected = "Todos", multiple=TRUE))
    }

    tagList(
      main_input, sexo_input, edad_input
    )
  })

  # Reactive: datos filtrados según filtros con comportamiento "Todos" = sin filtro
  datos_filtrados <- reactive({
    ds <- get_dataset()$df
    req(ds)  # dataset must exist

    # Main filter - depending on dataset
    main_vals <- input$filtro_main
    if (!is.null(main_vals) && !"Todos" %in% main_vals) {
      ds <- switch(input$reporte,
                   "Reporte A – Coberturas" = { if ("categoria" %in% names(ds)) ds %>% filter(categoria %in% main_vals) else ds },
                   "Reporte B – Influenza"  = { if ("grupo" %in% names(ds)) ds %>% filter(grupo %in% main_vals) else ds },
                   "Reporte C – Agentes Etiológicos" = { if ("region" %in% names(ds)) ds %>% filter(region %in% main_vals) else ds }
      )
    }

    # Sexo filter
    sx <- input$filtro_sexo
    if (!is.null(sx) && !"Todos" %in% sx && "sexo" %in% names(ds)) {
      ds <- ds %>% filter(sexo %in% sx)
    }

    # Edad filter
    ed <- input$filtro_edad
    if (!is.null(ed) && !"Todos" %in% ed && "edad" %in% names(ds)) {
      ds <- ds %>% filter(edad %in% ed)
    }

    ds
  })

  # ---------------------------
  # Tabla: DT (server = TRUE para datasets grandes)
  # ---------------------------
  output$tabla_dt <- renderDT({
    dat <- datos_filtrados()
    if (is.null(dat)) return(NULL)
    datatable(dat, options = list(pageLength = 25), rownames = FALSE)
  }, server = TRUE)

  # ---------------------------
  # Grafico: plotly según reporte
  # ---------------------------
  output$grafico <- renderPlotly({
    dat <- datos_filtrados()
    req(dat)

    p <- switch(input$reporte,
                "Reporte A – Coberturas" = {
                  # expects: categoria, y
                  if (!all(c("categoria","y") %in% names(dat))) return(NULL)
                  ggplot(dat, aes(x = categoria, y = y)) +
                    geom_col(fill = "#1f77b4") + theme_minimal() + labs(x = "Categoría", y = "Valor")
                },
                "Reporte B – Influenza" = {
                  # expects: fecha, valor, grupa(optional)
                  if (!all(c("fecha","valor") %in% names(dat))) return(NULL)
                  # ensure fecha is date-like
                  if (!inherits(dat$fecha, "Date") && !inherits(dat$fecha, "POSIXct")) {
                    suppressWarnings({
                      dat$fecha <- as.Date(dat$fecha)
                    })
                  }
                  ggplot(dat, aes(x = fecha, y = valor, color = if("grupo" %in% names(dat)) grupo else NULL, group = if("grupo" %in% names(dat)) grupo else 1)) +
                    geom_line() + geom_point() + theme_minimal() + labs(x = "Fecha", y = "Valor")
                },
                "Reporte C – Agentes Etiológicos" = {
                  # expects: x,y,region
                  if (!all(c("x","y") %in% names(dat))) return(NULL)
                  ggplot(dat, aes(x = x, y = y, fill = if("region" %in% names(dat)) region else NULL)) +
                    geom_col() + theme_minimal() + labs(x = "Agente", y = "Valor")
                }
    )

    if (is.null(p)) return(NULL)
    ggplotly(p, tooltip = "all")
  })

  # When the report changes, ensure main filters default to "Todos" (helps UX)
  observeEvent(input$reporte, {
    # set default values in UI (will re-render filters_ui automatically)
    # small delay to wait UI update:
    invalidateLater(100, session)
    # the select inputs are re-created by UI, so not necessary to call updateSelectInput here
  })

}

shinyApp(ui, server)
