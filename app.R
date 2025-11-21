library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(stringr)

# ================================
# COLORES
# ================================
COLOR_BARRA <- "#191970"
COLOR_TAB_ACTIVA <- "#EEE9E9"
COLOR_TAB_INACTIVA <- "#f5f5f5"
COLOR_BORDE_TAB <- "white"

# ================================
# FUNCIONES: detectar archivo más reciente por sufijo (_influenza.csv, ...)
# ================================
archivo_mas_reciente_info <- function(sufijo) {
  archivos <- list.files("data", pattern = paste0("^[0-9]{6}.*", sufijo, "$"), full.names = TRUE)
  if (length(archivos) == 0) return(list(archivo = NA_character_, fecha = NA))

  # extraer prefijo AAMMDD de cada nombre
  prefijos <- str_extract(basename(archivos), "^[0-9]{6}")
  fechas <- suppressWarnings(as.Date(prefijos, format = "%y%m%d"))  # AAMMDD -> %y%m%d

  idx <- which.max(fechas)
  list(archivo = archivos[idx], fecha = fechas[idx])
}

# Detectar archivos (se ejecuta 1 sola vez al iniciar)
info_cob <- archivo_mas_reciente_info("_coberturas.csv")
info_inf <- archivo_mas_reciente_info("_influenza.csv")
info_age <- archivo_mas_reciente_info("_agentes.csv")

# ================================
# CARGAR CSV UNA VEZ (mejora rendimiento)
# Si no existe el archivo, crear data.frames vacíos con columnas esperadas
# ================================
# Coberturas: x, y, categoria
if (!is.na(info_cob$archivo)) {
  cob_df <- read_csv(info_cob$archivo, show_col_types = FALSE)
} else {
  cob_df <- tibble::tibble(x = character(), y = numeric(), categoria = character())
}

# Influenza: fecha, valor, grupo
if (!is.na(info_inf$archivo)) {
  inf_df <- read_csv(info_inf$archivo, show_col_types = FALSE)
} else {
  inf_df <- tibble::tibble(fecha = character(), valor = numeric(), grupo = character())
}

# Agentes: x, y, region
if (!is.na(info_age$archivo)) {
  age_df <- read_csv(info_age$archivo, show_col_types = FALSE)
} else {
  age_df <- tibble::tibble(x = character(), y = numeric(), region = character())
}

# Formatear fechas para mostrar en sidebar (DD-MM-YYYY) o "N/A"
fecha_cob_str <- if (!is.na(info_cob$fecha)) format(info_cob$fecha, "%d-%m-%Y") else "N/A"
fecha_inf_str <- if (!is.na(info_inf$fecha)) format(info_inf$fecha, "%d-%m-%Y") else "N/A"
fecha_age_str <- if (!is.na(info_age$fecha)) format(info_age$fecha, "%d-%m-%Y") else "N/A"

# ================================
# UI
# ================================
ui <- fluidPage(

  tags$head(
    tags$style(HTML(sprintf("
      /* Banda superior */
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
        text-align: center;              /* centrar todo dentro del sidebar */
      }

      .sidebar-custom .control-label {
        display: block;
        width: 100%;
        text-align: center;              /* centrar labels de inputs */
      }

      /* Logo */
      .sidebar-logo {
        width: 120px;
        display: block;
        margin-left: auto;
        margin-right: auto;
        margin-bottom: 10px;
      }

      /* Filtros horizontales */
      .filtros-inline > * {
        display: inline-block;
        margin-right: 20px;
        vertical-align: middle;
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

      /* Make table responsive */
      .table-responsive { overflow-x: auto; }
    ",
      COLOR_BARRA, COLOR_BARRA,
      COLOR_TAB_INACTIVA, COLOR_BORDE_TAB,
      COLOR_TAB_ACTIVA, COLOR_BORDE_TAB
    )))
  ),

  # ---- BARRA SUPERIOR ----
  div(class = "titulo-banner", "UES O'Higgins – Reportes"),

  fluidRow(

    # ---- SIDEBAR IZQUIERDA ----
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

    # ---- CONTENIDO PRINCIPAL ----
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

  # fecha dinámica según reporte
  output$fecha_actualizacion <- renderText({
    fecha <- switch(input$reporte,
                    "Reporte A – Coberturas" = fecha_cob_str,
                    "Reporte B – Influenza"  = fecha_inf_str,
                    "Reporte C – Agentes Etiológicos" = fecha_age_str,
                    "N/A")
    fecha
  })

  # Devuelve dataset ya cargado y metadatos (no leer archivos repetidamente)
  get_dataset <- reactive({
    if (input$reporte == "Reporte A – Coberturas") {
      list(df = cob_df, var_main = "categoria", var_num = "y")
    } else if (input$reporte == "Reporte B – Influenza") {
      # asegurar fecha como Date si existe
      if ("fecha" %in% names(inf_df)) {
        # intentar convertir si no lo está
        if (!inherits(inf_df$fecha, "Date")) {
          suppressWarnings(inf_df$fecha <- as.Date(inf_df$fecha))
        }
      }
      list(df = inf_df, var_main = "grupo", var_num = "valor")
    } else {
      list(df = age_df, var_main = "region", var_num = "y")
    }
  })

  # FILTROS dinámicos con checkboxes y botones Select All / Deselect All
  output$filtros_ui <- renderUI({
    ds_info <- get_dataset()
    df <- ds_info$df
    var_main <- ds_info$var_main
    var_num <- ds_info$var_num

    # choices seguros: if column missing or empty -> empty vector
    main_vals <- if (nrow(df) == 0 || !(var_main %in% names(df))) character(0) else sort(unique(df[[var_main]]))
    sexo_vals <- if (nrow(df) == 0 || !("sexo" %in% names(df))) character(0) else sort(unique(df$sexo))
    edad_vals <- if (nrow(df) == 0 || !("edad" %in% names(df))) character(0) else sort(unique(df$edad))

    filtros <- tagList(
      div(class = "filtros-inline",
          # Main filter: buttons + checkbox group (default all selected)
          tagList(
            actionButton("select_all_main", "Seleccionar todo"),
            actionButton("deselect_all_main", "Deseleccionar todo"),
            br(),
            checkboxGroupInput(
              inputId = "filtro_main",
              label = "Filtro principal:",
              choices = main_vals,
              selected = main_vals
            )
          )
      )
    )

    # sexo filter (if exists)
    if (length(sexo_vals) > 0) {
      filtros <- tagList(
        filtros,
        div(class = "filtros-inline",
            actionButton("select_all_sexo", "Seleccionar todo (sexo)"),
            actionButton("deselect_all_sexo", "Deseleccionar todo (sexo)"),
            br(),
            checkboxGroupInput(
              inputId = "filtro_sexo",
              label = "Sexo:",
              choices = sexo_vals,
              selected = sexo_vals
            )
        )
      )
    }

    # edad filter (if exists)
    if (length(edad_vals) > 0) {
      filtros <- tagList(
        filtros,
        div(class = "filtros-inline",
            actionButton("select_all_edad", "Seleccionar todo (edad)"),
            actionButton("deselect_all_edad", "Deseleccionar todo (edad)"),
            br(),
            checkboxGroupInput(
              inputId = "filtro_edad",
              label = "Edad:",
              choices = edad_vals,
              selected = edad_vals
            )
        )
      )
    }

    # slider numeric (if applicable)
    if (var_num %in% names(df) && is.numeric(df[[var_num]]) && nrow(df) > 0) {
      rng_min <- min(df[[var_num]], na.rm = TRUE)
      rng_max <- max(df[[var_num]], na.rm = TRUE)
      if (!is.finite(rng_min)) rng_min <- 0
      if (!is.finite(rng_max)) rng_max <- 1

      filtros <- tagList(
        filtros,
        div(class = "filtros-inline",
            sliderInput(
              "filtro_rango",
              label = paste("Filtrar", var_num, "(mín–máx):"),
              min = rng_min,
              max = rng_max,
              value = c(rng_min, rng_max),
              width = "300px"
            )
        )
      )
    } else {
      # hidden slider to keep logic consistent
      filtros <- tagList(
        filtros,
        tags$div(style = "display:none;",
                 sliderInput("filtro_rango", label = NULL, min = 0, max = 1, value = c(0,1)))
      )
    }

    div(class = "filtros-inline", filtros)
  })

  # Observers for select/deselect buttons -> they update the corresponding checkboxGroup
  observeEvent(input$select_all_main, {
    ds_info <- get_dataset()
    df <- ds_info$df
    var_main <- ds_info$var_main
    vals <- if (nrow(df) == 0 || !(var_main %in% names(df))) character(0) else sort(unique(df[[var_main]]))
    updateCheckboxGroupInput(session, "filtro_main", selected = vals)
  })
  observeEvent(input$deselect_all_main, {
    updateCheckboxGroupInput(session, "filtro_main", selected = character(0))
  })

  observeEvent(input$select_all_sexo, {
    ds_info <- get_dataset()
    df <- ds_info$df
    vals <- if (nrow(df) == 0 || !("sexo" %in% names(df))) character(0) else sort(unique(df$sexo))
    updateCheckboxGroupInput(session, "filtro_sexo", selected = vals)
  })
  observeEvent(input$deselect_all_sexo, {
    updateCheckboxGroupInput(session, "filtro_sexo", selected = character(0))
  })

  observeEvent(input$select_all_edad, {
    ds_info <- get_dataset()
    df <- ds_info$df
    vals <- if (nrow(df) == 0 || !("edad" %in% names(df))) character(0) else sort(unique(df$edad))
    updateCheckboxGroupInput(session, "filtro_edad", selected = vals)
  })
  observeEvent(input$deselect_all_edad, {
    updateCheckboxGroupInput(session, "filtro_edad", selected = character(0))
  })

  # APLICAR FILTROS (con manejo de empty selection => treat as all)
  datos_filtrados <- reactive({
    ds_info <- get_dataset()
    df <- ds_info$df
    var_main <- ds_info$var_main
    var_num <- ds_info$var_num

    # si df vacío, retornar df tal cual para que la tabla muestre vacío
    if (nrow(df) == 0) return(df)

    # filtro principal: if input$filtro_main is NULL or length 0 -> no filtering (all)
    if (!is.null(input$filtro_main) && length(input$filtro_main) > 0) {
      df <- df %>% filter(.data[[var_main]] %in% input$filtro_main)
    }

    # filtro sexo
    if (!is.null(input$filtro_sexo) && length(input$filtro_sexo) > 0 && "sexo" %in% names(df)) {
      df <- df %>% filter(sexo %in% input$filtro_sexo)
    }

    # filtro edad
    if (!is.null(input$filtro_edad) && length(input$filtro_edad) > 0 && "edad" %in% names(df)) {
      df <- df %>% filter(edad %in% input$filtro_edad)
    }

    # filtro rango (si existe y es numérico)
    if (!is.null(input$filtro_rango) && var_num %in% names(df) && is.numeric(df[[var_num]])) {
      df <- df %>% filter(.data[[var_num]] >= input$filtro_rango[1],
                          .data[[var_num]] <= input$filtro_rango[2])
    }

    df
  })

  # TABLA
  output$tabla <- renderTable({
    datos_filtrados()
  }, rownames = TRUE)

  # GRAFICO
  output$grafico <- renderPlotly({
    df <- datos_filtrados()
    ds_info <- get_dataset()

    # si df vacío, devolver gráfico vacío
    if (nrow(df) == 0) {
      p <- ggplot() + theme_void() + ggtitle("No hay datos para mostrar")
      return(ggplotly(p))
    }

    if (input$reporte == "Reporte B – Influenza") {
      # asegurar fecha tipo Date
      if ("fecha" %in% names(df) && !inherits(df$fecha, "Date")) {
        suppressWarnings(df$fecha <- as.Date(df$fecha))
      }

      # si hay columna grupo:
      if ("grupo" %in% names(df)) {
        p <- ggplot(df, aes(x = fecha, y = valor, color = grupo)) +
          geom_line() + geom_point() + theme_minimal()
      } else {
        p <- ggplot(df, aes(x = fecha, y = valor)) +
          geom_line(color = "#cc0000") + geom_point(color = "#cc0000") + theme_minimal()
      }

    } else if (input$reporte == "Reporte C – Agentes Etiológicos") {

      if ("region" %in% names(df)) {
        p <- ggplot(df, aes(x = x, y = y, fill = region)) + geom_col() + theme_minimal()
      } else {
        p <- ggplot(df, aes(x = x, y = y)) + geom_col(fill = "#1f77b4") + theme_minimal()
      }

    } else {
      # Coberturas
      if ("categoria" %in% names(df)) {
        p <- ggplot(df, aes(x = categoria, y = y)) + geom_col(fill = "#1f77b4") + theme_minimal()
      } else {
        p <- ggplot(df, aes(x = x, y = y)) + geom_col(fill = "#1f77b4") + theme_minimal()
      }
    }

    ggplotly(p)
  })
}

shinyApp(ui, server)
