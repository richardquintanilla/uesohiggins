# app.R
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)

# ---------------------------
# CARGA DE DATOS (UNA VEZ)
# ---------------------------
coberturas <- tryCatch(read_csv("data/coberturas.csv"), error = function(e) NULL)
influenza  <- tryCatch(read_csv("data/influenza.csv"), error = function(e) NULL)
agentes    <- tryCatch(read_csv("data/agentes.csv"), error = function(e) NULL)

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  tags$head(
    # CSS embebido - garantiza que se aplique sin depender de archivos externos
    tags$style(HTML("
      /* Title banner */
      .title-banner { background:#191970; color:white; padding:12px 10px; font-size:22px; font-weight:700; text-align:center; margin-bottom:8px; }

      /* Layout: sidebar */
      .my-sidebar { background:#191970; color:white; padding:15px; height:100vh; }
      .my-sidebar .shiny-input-container label { color: white !important; }
      .logo-top { display:block; margin: 0 auto 12px auto; max-width:160px; }

      /* Tabs: background, active/inactive */
      .nav-tabs { background-color:#191970; border-bottom:1px solid #ffffff; }
      .nav-tabs > li > a { color: white !important; font-weight:600; }
      .nav-tabs > li > a:hover { background-color:#101830 !important; color: white !important; }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        background-color:#EEE9E9 !important;
        color: black !important;
        border: 1px solid #ffffff !important;
      }

      /* inline filters */
      .filtros-inline .form-group { display:inline-block; margin-right:16px; vertical-align: middle; }
      /* small screens adjustments */
      @media (max-width: 768px) {
        .filtros-inline .form-group { display:block; margin-bottom:8px; }
        .my-sidebar { height:auto; }
      }
    "))
  ),

  div(class = "title-banner", "UES O'Higgins – Reportes"),

  # Layout principal
  fluidRow(
    column(
      width = 3,
      div(class = "my-sidebar",
          # logo (archivo debe estar en /www/logo_blanco_ues.png)
          img(src = "logo_blanco_ues.png", class = "logo-top", alt = "logo"),
          br(),
          selectInput("reporte", "Seleccione reporte:",
                      choices = c("Reporte A – Coberturas", "Reporte B – Influenza", "Reporte C – Agentes Etiológicos"),
                      selected = "Reporte A – Coberturas"),
          br(),
          # fecha (dinámica)
          uiOutput("fecha_reporte_ui")
      )
    ),

    column(
      width = 9,
      # filtros horizontales
      div(class = "filtros-inline", uiOutput("filtros_ui")),
      br(),
      tabsetPanel(
        tabPanel("Tabla", br(), DTOutput("tabla_dt")),
        tabPanel("Gráfico", br(), plotlyOutput("grafico", height = "600px"))
      )
    )
  )
)

# ---------------------------
# SERVER
# ---------------------------
server <- function(input, output, session) {

  # Encuentra dataset según reporte seleccionado
  get_dataset <- reactive({
    switch(input$reporte,
           "Reporte A – Coberturas" = list(df = coberturas, path = "data/coberturas.csv"),
           "Reporte B – Influenza"  = list(df = influenza,  path = "data/influenza.csv"),
           "Reporte C – Agentes Etiológicos" = list(df = agentes, path = "data/agentes.csv")
    )
  })

  # Fecha dinámica: muestra mtime del archivo si existe, si no usa Sys.time()
  output$fecha_reporte_ui <- renderUI({
    ds <- get_dataset()
    p <- ds$path
    if (!is.null(p) && file.exists(p)) {
      fm <- file.info(p)$mtime
      div(style = "color:white; font-size:13px;", paste0("Fecha del reporte: ", format(fm, "%d-%m-%Y %H:%M")))
    } else {
      div(style = "color:white; font-size:13px;", paste0("Fecha del reporte: ", format(Sys.time(), "%d-%m-%Y %H:%M")))
    }
  })

  # Generar filtros dinámicos (todos con "Todos" arriba, multiple = TRUE)
  output$filtros_ui <- renderUI({
    ds <- get_dataset()$df
    if (is.null(ds)) return(NULL)

    # main choices según reporte
    main_choices <- switch(input$reporte,
                           "Reporte A – Coberturas" = if ("categoria" %in% names(ds)) c("Todos", sort(unique(ds$categoria))) else c("Todos"),
                           "Reporte B – Influenza"  = if ("grupo" %in% names(ds)) c("Todos", sort(unique(ds$grupo))) else c("Todos"),
                           "Reporte C – Agentes Etiológicos" = if ("region" %in% names(ds)) c("Todos", sort(unique(ds$region))) else c("Todos")
    )

    main_input <- selectInput("filtro_main", "Filtro principal:",
                              choices = main_choices, selected = "Todos", multiple = TRUE)

    # sexo
    if ("sexo" %in% names(ds)) {
      sexo_choices <- c("Todos", sort(unique(ds$sexo)))
      sexo_input <- selectInput("filtro_sexo", "Sexo:", choices = sexo_choices, selected = "Todos", multiple = TRUE)
    } else {
      sexo_input <- NULL
    }

    # edad
    if ("edad" %in% names(ds)) {
      edad_choices <- c("Todos", sort(unique(ds$edad)))
      edad_input <- selectInput("filtro_edad", "Edad:", choices = edad_choices, selected = "Todos", multiple = TRUE)
    } else {
      edad_input <- NULL
    }

    tagList(main_input, sexo_input, edad_input)
  })

  # Filtrado con comportamiento "Todos" = sin filtro
  datos_filtrados <- reactive({
    ds <- get_dataset()$df
    req(ds)

    # main filter
    mv <- input$filtro_main
    if (!is.null(mv) && !"Todos" %in% mv) {
      ds <- switch(input$reporte,
                   "Reporte A – Coberturas" = { if ("categoria" %in% names(ds)) ds %>% filter(categoria %in% mv) else ds },
                   "Reporte B – Influenza"  = { if ("grupo" %in% names(ds)) ds %>% filter(grupo %in% mv) else ds },
                   "Reporte C – Agentes Etiológicos" = { if ("region" %in% names(ds)) ds %>% filter(region %in% mv) else ds }
      )
    }

    # sexo
    sx <- input$filtro_sexo
    if (!is.null(sx) && !"Todos" %in% sx && "sexo" %in% names(ds)) {
      ds <- ds %>% filter(sexo %in% sx)
    }

    # edad
    ed <- input$filtro_edad
    if (!is.null(ed) && !"Todos" %in% ed && "edad" %in% names(ds)) {
      ds <- ds %>% filter(edad %in% ed)
    }

    ds
  })

  # Tabla: DT server=TRUE
  output$tabla_dt <- renderDT({
    df <- datos_filtrados()
    req(df)
    datatable(df, options = list(pageLength = 25), rownames = FALSE)
  }, server = TRUE)

  # Grafico: ggplot + ggplotly
  output$grafico <- renderPlotly({
    df <- datos_filtrados()
    req(df)

    p <- switch(input$reporte,
                "Reporte A – Coberturas" = {
                  if (!all(c("categoria","y") %in% names(df))) return(NULL)
                  ggplot(df, aes(x = categoria, y = y)) + geom_col(fill="#1f77b4") + theme_minimal()
                },
                "Reporte B – Influenza" = {
                  if (!all(c("fecha","valor") %in% names(df))) return(NULL)
                  if (!inherits(df$fecha, "Date")) df$fecha <- as.Date(df$fecha)
                  ggplot(df, aes(x = fecha, y = valor, color = if("grupo" %in% names(df)) grupo else NULL, group = if("grupo" %in% names(df)) grupo else 1)) +
                    geom_line() + geom_point() + theme_minimal()
                },
                "Reporte C – Agentes Etiológicos" = {
                  if (!all(c("x","y") %in% names(df))) return(NULL)
                  ggplot(df, aes(x = x, y = y, fill = if("region" %in% names(df)) region else NULL)) + geom_col() + theme_minimal()
                }
    )

    if (is.null(p)) return(NULL)
    ggplotly(p, tooltip = "all")
  })

}

shinyApp(ui, server)
