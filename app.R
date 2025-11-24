library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(stringr)
library(tibble)
library(reactable)

# Cargar funciones
load("www/rt_tabla.rda")
load("www/grafico_barras.rda")
load("www/mapa_estadistico.rda")

# ================================
# COLORES Y CONSTANTES
# ================================
COLOR_BARRA      <- "#191970"
COLOR_TAB_ACTIVA <- "#191970"
COLOR_TAB_INACTIVA <- "#EEE9E9"
COLOR_BORDE_TAB  <- "white"

# ================================
# Función: detectar archivo más reciente
# ================================
archivo_mas_reciente_info <- function(sufijo) {
  pattern <- paste0("^[0-9]{6}.*", sufijo, "$")
  archivos <- list.files("data", pattern = pattern, full.names = TRUE)

  if (length(archivos) == 0) {
    return(list(archivo = NA_character_, fecha = NA))
  }

  nombres_base <- basename(archivos)
  prefijos <- str_extract(nombres_base, "^[0-9]{6}")
  fechas <- suppressWarnings(as.Date(prefijos, format = "%y%m%d"))
  idx <- which.max(fechas)

  list(archivo = archivos[idx], fecha = fechas[idx])
}

# ================================
# Detectar archivo más reciente SOLO para Olas de Calor
# ================================
info_olas_calor <- archivo_mas_reciente_info("_coberturas.csv")

# Si no existe archivo -> tibble vacío
if (!is.na(info_olas_calor$archivo)) {
  olas_calor_df <- read_csv(info_olas_calor$archivo, show_col_types = FALSE)
} else {
  olas_calor_df <- tibble(x = character(), y = numeric(), categoria = character())
}

# Formato fecha para sidebar
fecha_olas_calor_str <- if (!is.na(info_olas_calor$fecha)) {
  format(info_olas_calor$fecha, "%d-%m-%Y")
} else {
  "N/A"
}

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

      .selectize-dropdown .selectize-dropdown-content div {
        text-align: left !important;
      }
      .selectize-control .item {
        text-align: left !important;
      }
      .selectize-input {
        text-align: left !important;
      }

      .sidebar-logo {
        width: 120px;
        display: block;
        margin-left: auto;
        margin-right: auto;
        margin-bottom: 10px;
      }

      .filtros-inline > div {
        display: inline-block;
        margin-right: 20px;
        margin-top: 15px;
        vertical-align: top;
      }

      .nav-tabs > li > a {
        background-color: %s !important;
        border: 1px solid %s !important;
        color: black !important;
      }
      .nav-tabs > li.active > a {
        background-color: %s !important;
        color: white !important;
        border: 1px solid %s !important;
        font-weight: bold !important;
      }
    ",
COLOR_BARRA, COLOR_BARRA,
COLOR_TAB_INACTIVA, COLOR_BORDE_TAB,
COLOR_TAB_ACTIVA, COLOR_BORDE_TAB
    )))
  ),

  fluidRow(
    # Sidebar
    column(
      width = 2,
      div(class = "sidebar-custom",
          selectInput(
            "reporte",
            "Seleccione un reporte:",
            choices = c("Vigilancia Olas de Calor")
          ),

          div(id = "fecha_texto",
              style = "margin-top:20px; font-size:14px; font-weight: bold; color:white;",
              span("Fecha del reporte: "),
              textOutput("fecha_actualizacion", inline = TRUE)
          ),

          div(style = "height:20px;"),

          img(src = "logo_epi.png", class = "sidebar-logo"),
          img(src = "logo_ues_blanco.png", class = "sidebar-logo")
      )
    ),

    # Contenido
    column(
      width = 10,
      uiOutput("filtros_ui"),
      br(),

      tabsetPanel(
        id = "tabs",
        tabPanel("Tabla", reactableOutput("tabla"))
        tabPanel("Gráfico", plotlyOutput("grafico"))
      )
    )
  )
)

# ================================
# SERVER
# ================================
server <- function(input, output, session) {

  # Fecha dinámica
  output$fecha_actualizacion <- renderText({
    fecha_olas_calor_str
  })

  # Dataset estático
  get_dataset <- reactive({
    list(df = olas_calor_df, var_main = "categoria", var_num = "y")
  })

  # ----------------------------
  # UI de filtros
  # ----------------------------
  output$filtros_ui <- renderUI({
    ds <- get_dataset()
    df <- ds$df

    main_vals <- if ("categoria" %in% names(df)) sort(unique(df$categoria)) else character(0)

    selectizeInput(
      "filtro_main",
      "Categoría:",
      choices = main_vals,
      selected = main_vals,
      multiple = TRUE,
      options = list(
        plugins = list("remove_button"),
        onInitialize = I(
          'function() {
             var s = this;
             var all = $("<div style=\\"padding:6px;cursor:pointer;font-weight:bold;border-bottom:1px solid #eee;\\">✓ Seleccionar todo</div>");
             var none = $("<div style=\\"padding:6px;cursor:pointer;font-weight:bold;\\">✗ Deseleccionar todo</div>");
             all.on("click",function(){ s.setValue(Object.keys(s.options)); });
             none.on("click",function(){ s.clear(); });
             this.$dropdown.prepend(none);
             this.$dropdown.prepend(all);
           }'
        )
      )
    )
  })

  # ----------------------------
  # Filtrado
  # ----------------------------
  datos_filtrados <- reactive({
    df <- get_dataset()$df

    if (!is.null(input$filtro_main) && length(input$filtro_main) > 0) {
      df <- df %>% filter(categoria %in% input$filtro_main)
    }

    df
  })

  # ----------------------------
  # Tabla
  # ----------------------------
  output$tabla <- renderReactable({
  df <- datos_filtrados()
  rt_tabla(df = df, fijas = names(df)[1], destacar = names(df)[2])
})

  # ----------------------------
  # Gráfico
  # ----------------------------
  output$grafico <- renderPlotly({
    df <- datos_filtrados()

    if (nrow(df) == 0) {
      p <- ggplot() + theme_void() + ggtitle("No hay datos para mostrar")
      return(ggplotly(p))
    }

    p <- ggplot(df, aes(x = categoria, y = y, 
                        text = paste0("Categoría: ", categoria, 
                                      "<br>",
                                      "N° de algo: ", y))) +
      geom_col() +
      theme_minimal()

    ggplotly(p, tooltip = "text")
  })
}

# ================================
# Run App
# ================================
shinyApp(ui, server)


