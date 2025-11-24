# app.R - Versión detallada y verbosa (larga) con todas las funciones que pediste
# - Detección automática del archivo más reciente por patrón (AAMMDD_..._tipo.csv)
# - Fecha dinámica mostrada en sidebar
# - Logo centrado en sidebar
# - Filtros horizontales sobre el contenido con selectize (checkbox-style) y botones
#   "Seleccionar todo / Deseleccionar todo" insertados dentro del dropdown
# - Carga de CSV una sola vez (mejora rendimiento)
# - CSS con % escapados (%%) para evitar errores en sprintf()

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(stringr)
library(tibble)

# ================================
# COLORES Y CONSTANTES
# ================================
COLOR_BARRA      <- "#191970"
COLOR_TAB_ACTIVA <- "#EEE9E9"
COLOR_TAB_INACTIVA <- "#f5f5f5"
COLOR_BORDE_TAB  <- "white"

# ================================
# Función: detectar archivo más reciente por sufijo/patrón
# - Busca archivos en la carpeta "data"
# - Espera nombres que comiencen con AAMMDD, por ejemplo: 241120_coberturas.csv
# - Devuelve lista con ruta y fecha (Date)
# ================================
archivo_mas_reciente_info <- function(sufijo) {
  # Construir patrón: prefijo fecha (6 dígitos) seguido de cualquier cosa y el sufijo exacto
  pattern <- paste0("^[0-9]{6}.*", sufijo, "$")
  archivos <- list.files("data", pattern = pattern, full.names = TRUE)
  
  if (length(archivos) == 0) {
    # No se encontraron archivos que coincidan
    return(list(archivo = NA_character_, fecha = NA))
  }
  
  # Extraer el prefijo AAMMDD desde el nombre base
  nombres_base <- basename(archivos)
  prefijos <- str_extract(nombres_base, "^[0-9]{6}")
  
  # Convertir AAMMDD -> Date mediante format %y%m%d
  fechas <- suppressWarnings(as.Date(prefijos, format = "%y%m%d"))
  
  # Elegir el índice con la fecha máxima (más reciente)
  idx <- which.max(fechas)
  
  # Retornar ruta y fecha
  list(archivo = archivos[idx], fecha = fechas[idx])
}

# ================================
# Detectar los archivos más recientes para cada reporte (se ejecuta 1 vez al iniciar)
# ================================
info_cob <- archivo_mas_reciente_info("_coberturas.csv")
info_inf <- archivo_mas_reciente_info("_influenza.csv")
info_age <- archivo_mas_reciente_info("_agentes.csv")

# ================================
# Cargar CSV UNA SOLA VEZ (mejora considerablemente el rendimiento)
# Si no existe archivo, se genera tibble vacío con columnas esperadas
# ================================
# Coberturas: columnas esperadas x, y, categoria (si varían, el app maneja columnas via existence checks)
if (!is.na(info_cob$archivo)) {
  cob_df <- read_csv(info_cob$archivo, show_col_types = FALSE)
} else {
  cob_df <- tibble(x = character(), y = numeric(), categoria = character())
}

# Influenza: fecha, valor, grupo
if (!is.na(info_inf$archivo)) {
  inf_df <- read_csv(info_inf$archivo, show_col_types = FALSE)
} else {
  inf_df <- tibble(fecha = character(), valor = numeric(), grupo = character())
}

# Agentes: x, y, region
if (!is.na(info_age$archivo)) {
  age_df <- read_csv(info_age$archivo, show_col_types = FALSE)
} else {
  age_df <- tibble(x = character(), y = numeric(), region = character())
}

# ================================
# Formatear fechas para mostrar en el sidebar (DD-MM-YYYY) o "N/A"
# ================================
fecha_cob_str <- if (!is.na(info_cob$fecha)) format(info_cob$fecha, "%d-%m-%Y") else "N/A"
fecha_inf_str <- if (!is.na(info_inf$fecha)) format(info_inf$fecha, "%d-%m-%Y") else "N/A"
fecha_age_str <- if (!is.na(info_age$fecha)) format(info_age$fecha, "%d-%m-%Y") else "N/A"

# ================================
# UI - Long form, con comentarios y espaciado
# ================================
ui <- fluidPage(
  tags$head(
    # IMPORTANTE: en sprintf cualquier % debe ir como %% -> ya escapado en el CSS abajo
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
        text-align: center;              /* centrar contenido en sidebar */
      }

      /* Centrar labels de inputs en sidebar */
      .sidebar-custom .control-label {
        display: block;
        width: 100%%;
        text-align: center;
      }
      
      /* Quitar estos 3 para centrar lo de la seleccion multiple */
      /* Alinear texto de las opciones del selectize a la izquierda */
        .selectize-dropdown .selectize-dropdown-content div {
        text-align: left !important;
      }
        .selectize-control .item {
        text-align: left !important;
      }
        .selectize-input {
        text-align: left !important;
      }



      /* Logo */
      .sidebar-logo {
        width: 120px;
        display: block;
        margin-left: auto;
        margin-right: auto;
        margin-bottom: 10px;
      }

      /* Filtros horizontales: cada filtro dentro de un div inline */
      .filtros-inline > div {
        display: inline-block;
        margin-right: 20px;
        margin-top: 15px;
        vertical-align: top;
      }

      /* Pestañas */
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

      /* Tabla responsiva */
      .table-responsive { overflow-x: auto; }
    ",
COLOR_BARRA, COLOR_BARRA,
COLOR_TAB_INACTIVA, COLOR_BORDE_TAB,
COLOR_TAB_ACTIVA, COLOR_BORDE_TAB
    )))
  ),

# -----------------------
# Barra superior (título)
# -----------------------
# div(class = "titulo-banner", "UES O'Higgins – Reportes"),

# -----------------------
# Layout principal: sidebar (col 2) + contenido (col 10)
# -----------------------
fluidRow(
  # Sidebar
  column(
    width = 2,
    div(class = "sidebar-custom",
        # Logo (archivo en /www/logo_ues_blanco.png)
        img(src = "logo_ues_blanco.png", class = "sidebar-logo"),
        
        # Selector de reporte (NO cambiar)
        selectInput(
          inputId = "reporte",
          label = "Seleccione un reporte:",
          choices = c(
            "Reporte A – Coberturas",
            "Reporte B – Influenza",
            "Reporte C – Agentes Etiológicos"
          )
        ),
        
        # Fecha dinámica del reporte (renderText en server)
        div(id = "fecha_texto",
            style = "margin-top:20px; font-size:14px; color:white;",
            span("Fecha del reporte: "),
            textOutput("fecha_actualizacion", inline = TRUE)
        )
    )
  ),
  
  # Contenido principal
  column(
    width = 10,
    # Aquí van los filtros dinámicos horizontales
    uiOutput("filtros_ui"),
    br(),
    
    # Pestañas principal: Tabla y Gráfico
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
  # Fecha dinámica que se muestra en sidebar
  # ----------------------------
  output$fecha_actualizacion <- renderText({
    fecha <- switch(input$reporte,
                    "Reporte A – Coberturas" = fecha_cob_str,
                    "Reporte B – Influenza"  = fecha_inf_str,
                    "Reporte C – Agentes Etiológicos" = fecha_age_str,
                    "N/A")
    fecha
  })
  
  # ----------------------------
  # Función reactiva que devuelve dataset ya cargado y metadatos
  # (NO lee archivos repetidamente)
  # ----------------------------
  get_dataset <- reactive({
    if (input$reporte == "Reporte A – Coberturas") {
      list(df = cob_df, var_main = "categoria", var_num = "y")
    } else if (input$reporte == "Reporte B – Influenza") {
      # convertir columna fecha a Date si existe y no lo está
      if ("fecha" %in% names(inf_df)) {
        if (!inherits(inf_df$fecha, "Date")) {
          # Intentamos convertir; si falla, quedará NA y se manejará luego
          suppressWarnings(inf_df$fecha <- as.Date(inf_df$fecha))
        }
      }
      list(df = inf_df, var_main = "grupo", var_num = "valor")
    } else {
      list(df = age_df, var_main = "region", var_num = "y")
    }
  })
  
  # ----------------------------
  # Filtros horizontales - UI dinámico
  # - Usamos selectizeInput con opciones JS para insertar botones dentro del dropdown
  # ----------------------------
  output$filtros_ui <- renderUI({
    ds_info <- get_dataset()
    df <- ds_info$df
    var_main <- ds_info$var_main
    var_num <- ds_info$var_num
    
    # Si df está vacío o la columna principal no existe -> mostrar filtro vacío (evitar errores)
    main_vals <- if (nrow(df) == 0 || !(var_main %in% names(df))) character(0) else sort(unique(df[[var_main]]))
    sexo_vals <- if (nrow(df) == 0 || !("sexo" %in% names(df))) character(0) else sort(unique(df$sexo))
    edad_vals <- if (nrow(df) == 0 || !("edad" %in% names(df))) character(0) else sort(unique(df$edad))
    
    # Función local para crear un selectize con botones dentro
    make_selectize <- function(inputId, label, choices) {
      # Si no hay choices, crear un selectize vacío (maneja el caso de datasets vacíos)
      if (length(choices) == 0) {
        return(
          div(style = "display:inline-block; margin-right:20px;",
              tags$label(label), br(),
              tags$small("(sin valores)")
          )
        )
      }
      
      # Construimos el selectize con un bloque JS en onInitialize que
      # inserta botones arriba del dropdown que permiten seleccionar/limpiar todo.
      selectizeInput(
        inputId = inputId,
        label = label,
        choices = choices,
        selected = choices,       # por defecto selecciona todos
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          placeholder = "Seleccione...",
          onInitialize = I(
            'function() {
               var selectize = this;
               // Crear botones
               var btnAll = $("<div style=\\"padding:6px; cursor:pointer; font-weight:bold; border-bottom:1px solid #eee;\\">✓ Seleccionar todo</div>");
               var btnNone = $("<div style=\\"padding:6px; cursor:pointer; font-weight:bold;\\">✗ Deseleccionar todo</div>");
               // Eventos
               btnAll.on("click", function() {
                 var vals = Object.keys(selectize.options);
                 selectize.setValue(vals);
               });
               btnNone.on("click", function() {
                 selectize.clear();
               });
               // Prepend botones al dropdown (arriba)
               this.$dropdown.prepend(btnNone);
               this.$dropdown.prepend(btnAll);
             }'
          )
        )
      )
    }
    
    # Construir filtros: siempre incluir filtro principal
    filtros <- tagList(
      div(class = "filtros-inline",
          make_selectize("filtro_main", "Filtro principal:", main_vals)
      )
    )
    
    # Añadir filtro sexo si existe
    if (length(sexo_vals) > 0) {
      filtros <- tagList(
        filtros,
        div(class = "filtros-inline",
            make_selectize("filtro_sexo", "Sexo:", sexo_vals)
        )
      )
    }
    
    # Añadir filtro edad si existe
    if (length(edad_vals) > 0) {
      filtros <- tagList(
        filtros,
        div(class = "filtros-inline",
            make_selectize("filtro_edad", "Edad:", edad_vals)
        )
      )
    }
    
    # Añadir slider numérico si aplica (var_num existe y es numérica)
    if (var_num %in% names(df) && is.numeric(df[[var_num]]) && nrow(df) > 0) {
      rng_min <- min(df[[var_num]], na.rm = TRUE)
      rng_max <- max(df[[var_num]], na.rm = TRUE)
      if (!is.finite(rng_min)) rng_min <- 0
      if (!is.finite(rng_max)) rng_max <- 1
      
      filtros <- tagList(
        filtros,
        div(class = "filtros-inline",
            sliderInput("filtro_rango",
                        label = paste("Filtrar", var_num, "(mín–máx):"),
                        min = rng_min,
                        max = rng_max,
                        value = c(rng_min, rng_max),
                        width = "300px")
        )
      )
    } else {
      # Mantener control oculto para evitar errores en la lógica de filtrado
      filtros <- tagList(
        filtros,
        tags$div(style = "display:none;",
                 sliderInput("filtro_rango", label = NULL, min = 0, max = 1, value = c(0,1)))
      )
    }
    
    # Retornar container con filtros en línea
    div(class = "filtros-inline", filtros)
  })
  
  # ----------------------------
  # Aplicar filtros a los datos (reactivo)
  # - Si selección está vacía o NULL -> se interpreta como "todos" (no filtrar)
  # ----------------------------
  datos_filtrados <- reactive({
    ds_info <- get_dataset()
    df <- ds_info$df
    var_main <- ds_info$var_main
    var_num <- ds_info$var_num
    
    # Si df está vacío, retornar tal cual
    if (nrow(df) == 0) return(df)
    
    # Filtro principal
    if (!is.null(input$filtro_main) && length(input$filtro_main) > 0) {
      df <- df %>% filter(.data[[var_main]] %in% input$filtro_main)
    }
    
    # Filtro sexo
    if (!is.null(input$filtro_sexo) && length(input$filtro_sexo) > 0 && "sexo" %in% names(df)) {
      df <- df %>% filter(sexo %in% input$filtro_sexo)
    }
    
    # Filtro edad
    if (!is.null(input$filtro_edad) && length(input$filtro_edad) > 0 && "edad" %in% names(df)) {
      df <- df %>% filter(edad %in% input$filtro_edad)
    }
    
    # Filtro rango numérico
    if (!is.null(input$filtro_rango) && var_num %in% names(df) && is.numeric(df[[var_num]])) {
      df <- df %>% filter(.data[[var_num]] >= input$filtro_rango[1],
                          .data[[var_num]] <= input$filtro_rango[2])
    }
    
    df
  })
  
  # ----------------------------
  # Render: Tabla
  # ----------------------------
  output$tabla <- renderTable({
    datos_filtrados()
  }, rownames = TRUE)
  
  # ----------------------------
  # Render: Gráfico (ggplot + ggplotly)
  # ----------------------------
  output$grafico <- renderPlotly({
    df <- datos_filtrados()
    ds_info <- get_dataset()
    
    # Si no hay datos -> gráfico vacío con mensaje
    if (nrow(df) == 0) {
      p <- ggplot() + theme_void() + ggtitle("No hay datos para mostrar")
      return(ggplotly(p))
    }
    
    # Influenza: serie temporal
    if (input$reporte == "Reporte B – Influenza") {
      if ("fecha" %in% names(df) && !inherits(df$fecha, "Date")) {
        suppressWarnings(df$fecha <- as.Date(df$fecha))
      }
      
      if ("grupo" %in% names(df)) {
        p <- ggplot(df, aes(x = fecha, y = valor, color = grupo)) +
          geom_line() + geom_point() + theme_minimal()
      } else {
        p <- ggplot(df, aes(x = fecha, y = valor)) +
          geom_line(color = "#cc0000") + geom_point(color = "#cc0000") + theme_minimal()
      }
      
      # Agentes: barras por categoría/region
    } else if (input$reporte == "Reporte C – Agentes Etiológicos") {
      if ("region" %in% names(df)) {
        p <- ggplot(df, aes(x = x, y = y, fill = region)) + geom_col() + theme_minimal()
      } else {
        p <- ggplot(df, aes(x = x, y = y)) + geom_col(fill = "#1f77b4") + theme_minimal()
      }
      
      # Coberturas: barras por categoria
    } else {
      if ("categoria" %in% names(df)) {
        p <- ggplot(df, aes(x = categoria, y = y)) + geom_col(fill = "#1f77b4") + theme_minimal()
      } else {
        # fallback: usar x,y si no existe categoria
        p <- ggplot(df, aes(x = x, y = y)) + geom_col(fill = "#1f77b4") + theme_minimal()
      }
    }
    
    ggplotly(p)
  })
}

# ================================
# Ejecutar app
# ================================
shinyApp(ui, server)

