library(shiny)
library(dplyr)
library(readr)
library(plotly)
library(ggplot2)

# ============================================================
# FUNCIONES
# ============================================================

# ---- Extrae fecha desde nombre con formato AAMMDD_XXXX.csv ----
extraer_fecha <- function(nombre_archivo) {
  fecha_str <- substr(nombre_archivo, 1, 6)  # AAMMDD
  
  # Convertir AAMMDD → "20AA-MM-DD"
  suppressWarnings({
    aa <- substr(fecha_str, 1, 2)
    mm <- substr(fecha_str, 3, 4)
    dd <- substr(fecha_str, 5, 6)
    
    # Año asumido 20AA
    fecha_convertida <- paste0("20", aa, "-", mm, "-", dd)
    as.Date(fecha_convertida)
  })
}

# ---- Buscar archivo más reciente según patrón ----
archivo_mas_reciente <- function(patron) {

  archivos <- list.files("data", pattern = patron, full.names = TRUE)
  if (length(archivos) == 0) return(NA)

  fechas <- sapply(basename(archivos), extraer_fecha)
  idx <- which.max(fechas)

  list(
    archivo = archivos[idx],
    fecha = fechas[idx]
  )
}

# ============================================================
# CARGA AUTOMÁTICA DE ARCHIVOS (según tu carpeta GitHub)
# ============================================================

info_cob <- archivo_mas_reciente("_coberturas.csv")
info_inf <- archivo_mas_reciente("_influenza.csv")
info_age <- archivo_mas_reciente("_agentes.csv")

# ============================================================
# COLORES
# ============================================================
COLOR_BARRA <- "#191970"
COLOR_TAB_ACTIVA <- "#EEE9E9"
COLOR_TAB_INACTIVA <- "#f5f5f5"
COLOR_BORDE_TAB <- "white"

# ============================================================
# UI (NO SE MODIFICA NADA)
# ============================================================
ui <- fluidPage(

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
    COLOR_BARRA, COLOR_BARRA,
    COLOR_TAB_INACTIVA, COLOR_BORDE_TAB,
    COLOR_TAB_ACTIVA, COLOR_BORDE_TAB
    )))
  ),

  div(class = "titulo-banner", "UES O'Higgins – Reportes"),

  fluidRow(

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
              span("Fecha del reporte: "),
              textOutput("fecha_actualizacion", inline = TRUE)
          )
      )
    ),

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

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # ---- Carga dataset según reporte ----
  get_dataset <- reactive({

    if (input$reporte == "Reporte A – Coberturas") {
      df <- read_csv(info_cob$archivo)
      list(df = df, var_main = "categoria", var_num = "y")
    }

    else if (input$reporte == "Reporte B – Influenza") {
      df <- read_csv(info_inf$archivo)
      list(df = df, var_main = "grupo", var_num = "valor")
    }

    else {
      df <- read_csv(info_age$archivo)
      list(df = df, var_main = "region", var_num = "y")
    }
  })

  # ---- Fecha dinámica ----
  output$fecha_actualizacion <- renderText({

    fecha <- switch(input$reporte,
                    "Reporte A – Coberturas" = info_cob$fecha,
                    "Reporte B – Influenza"  = info_inf$fecha,
                    "Reporte C – Agentes Etiológicos" = info_age$fecha,
                    NA)

    if (is.na(fecha)) return("N/A")

    format(fecha, "%d-%m-%Y")
  })

  # ---- Filtros dinámicos ----
  output$filtros_ui <- renderUI({

    ds_info <- get_dataset()
    df <- ds_info$df
    var_main <- ds_info$var_main
    var_num <- ds_info$var_num

    tagList(
      div(class = "filtros-inline",

          selectInput(
            "filtro_main",
            "Filtro principal:",
            choices = c("Todos", sort(unique(df[[var_main]]))),
            selected = "Todos",
            multiple = TRUE,
            width = "250px"
          ),

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

  # ---- Aplicar filtros ----
  datos_filtrados <- reactive({

    ds_info <- get_dataset()
    df <- ds_info$df
    var_main <- ds_info$var_main
    var_num <- ds_info$var_num

    if (!"Todos" %in% input$filtro_main)
      df <- df %>% filter(.data[[var_main]] %in% input$filtro_main)

    df <- df %>% filter(.data[[var_num]] >= input$filtro_rango[1],
                        .data[[var_num]] <= input$filtro_rango[2])

    df
  })

  # ---- Tabla ----
  output$tabla <- renderTable({
    datos_filtrados()
  })

  # ---- Gráfico ----
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
