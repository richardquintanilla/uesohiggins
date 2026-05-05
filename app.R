# app.R - Vigilancia GES

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(lubridate)
library(reactable)
library(htmltools)

# ============================================
# FUNCIONES COMUNES
# ============================================

clasificacion_problemas <- list(
  "^Catarátas" = "Cataratas",
  "^Endoprótesis" = "Endoprótesis de Cadera",
  "^Vicios de Refracción" = "Vicios de Refracción",
  "^Cáncer de Mama" = "Cáncer de Mama",
  "^Cáncer de Testículo" = "Cáncer de Testículo",
  "^Cáncer Cervicouterino" = "Cáncer Cervicouterino",
  "^Cáncer Colorectal" = "Cáncer Colorrectal",
  "^Cáncer de Próstata" = "Cáncer de Próstata",
  "^Cáncer de Pulmón" = "Cáncer de Pulmón",
  "^Cáncer de Ovario" = "Cáncer de Ovario",
  "^Cáncer de Tiroides" = "Cáncer de Tiroides",
  "^Cáncer Gástrico" = "Cáncer Gástrico",
  "^Cáncer Renal" = "Cáncer Renal",
  "^Cáncer Vesical" = "Cáncer Vesical",
  "^Artrosis de Cadera y/o Rodilla" = "Artrosis de Cadera/Rodilla",
  "^Leucemia Adulto" = "Leucemia Adulto",
  "Diabetes Mellitus Tipo 1 Diabetes Mellitus I" = "Diabetes Mellitus Tipo 1",
  "^Disrrafias Espinales" = "Disrafias Espinales",
  "^Artritis Reumatoide" = "Artritis Reumatoide",
  "Enfermedad Pulmonar Obstructiva" = "Enfermedad Pulmonar Obstructiva",
  "^Hipertensión Arterial" = "Hipertensión Arterial",
  "Infarto Agudo del Miocardio" = "Infarto Agudo al Miocardio",
  "^Ataque Cerebrovascular" = "Ataque Cerebrovascular",
  "ENFERMEDAD RENAL CRÓNICA ETAPA 4 Y 5" = "Enfermedad Renal Crónica Etapa 4 y 5",
  "Tratamiento Erradicación HELICOBACTER PYLORI" = "Tratamiento Erradicación Helicobacter Pylori"
)

oncologicos_problemas <- list(
  "^Cáncer",
  "Alivio del dolor",
  "^Leucemia",
  "^Linfoma",
  "Osteosarcoma",
  "Mieloma múltiple"
)

clasificar_problema <- function(texto) {
  if(is.na(texto) || texto == "No especificado" || texto == "") return(texto)
  for(pattern in names(clasificacion_problemas)) {
    if(grepl(pattern, texto, ignore.case = TRUE)) {
      return(clasificacion_problemas[[pattern]])
    }
  }
  if(grepl("Cáncer", texto, ignore.case = TRUE)) return("Cáncer (otros)")
  return(texto)
}

es_oncologico <- function(texto) {
  if(is.na(texto)) return(FALSE)
  for(pattern in oncologicos_problemas) {
    if(grepl(pattern, texto, ignore.case = TRUE)) return(TRUE)
  }
  return(FALSE)
}

# Función para encontrar archivo en múltiples rutas
encontrar_archivo <- function(nombres_posibles) {
  for(ruta in nombres_posibles) {
    if(file.exists(ruta)) {
      return(ruta)
    }
  }
  return(NULL)
}

# Función para formatear números con separador de miles (.)
formatear_numero <- function(x) {
  if(is.na(x)) return("")
  format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

# Función para crear tabla con reactable
crear_tabla_detalle <- function(df, col_fijas = NULL, col_destacar = NULL) {
  
  if(is.null(df) || nrow(df) == 0) {
    df_mensaje <- data.frame(Mensaje = "No hay datos con los filtros seleccionados")
    return(reactable(
      df_mensaje,
      columns = list(Mensaje = reactable::colDef(name = "Mensaje", align = "center")),
      searchable = FALSE,
      striped = FALSE,
      bordered = TRUE,
      pagination = FALSE
    ))
  }
  
  df <- df %>%
    mutate(across(where(is.numeric), ~ as.numeric(.)))
  
  columnas <- lapply(names(df), function(col) {
    class_col <- paste0("col-", gsub("\\s+", "_", gsub("[^a-zA-Z0-9]", "_", col)))
    
    if(col %in% col_fijas) {
      return(reactable::colDef(
        name = col,
        align = "left",
        class = paste(class_col, "col-fija"),
        style = list(background = "#191970", color = "white", fontWeight = "bold"),
        headerStyle = list(background = "#191970", color = "white", fontWeight = "bold", position = "sticky", top = 0, zIndex = 1)
      ))
    }
    
    if(col %in% col_destacar) {
      return(reactable::colDef(
        name = col,
        align = "center",
        class = class_col,
        style = list(background = "#e3e3e3", fontWeight = "bold"),
        cell = function(value) {
          if(is.numeric(value)) return(formatear_numero(value))
          return(as.character(value))
        },
        headerStyle = list(background = "#191970", color = "white", fontWeight = "bold", position = "sticky", top = 0, zIndex = 1)
      ))
    }
    
    if(is.numeric(df[[col]])) {
      return(reactable::colDef(
        name = col,
        align = "center",
        class = class_col,
        cell = function(value) formatear_numero(value),
        headerStyle = list(background = "#191970", color = "white", fontWeight = "bold", position = "sticky", top = 0, zIndex = 1)
      ))
    }
    
    reactable::colDef(
      name = col,
      align = "center",
      class = class_col,
      headerStyle = list(background = "#191970", color = "white", fontWeight = "bold", position = "sticky", top = 0, zIndex = 1)
    )
  })
  
  names(columnas) <- names(df)
  
  reactable::reactable(
    df,
    columns = columnas,
    searchable = FALSE,
    striped = TRUE,
    bordered = TRUE,
    highlight = FALSE,
    pagination = FALSE,
    height = 500,
    defaultColDef = reactable::colDef(
      headerStyle = list(
        background = "#191970",
        color = "white",
        fontWeight = "bold",
        position = "sticky",
        top = 0,
        zIndex = 1
      )
    )
  )
}

# ============================================
# UI
# ============================================

ui <- dashboardPage(
  dashboardHeader(
    title = "Vigilancia GES", 
    titleWidth = 300,
    tags$li(class = "dropdown",
            div(style = "margin-right: 20px; margin-top: 15px; color: white; font-weight: normal;",
                textOutput("fecha_corte_header"))
    )
  ),
  
  dashboardSidebar(
    width = 300,
    tags$style(HTML("
      .skin-blue .main-header { position: fixed; width: 100%; z-index: 1030; top: 0; }
      .main-sidebar { position: fixed; top: 50px; bottom: 0; left: 0; z-index: 1020; overflow-y: auto; }
      .content-wrapper, .right-side { margin-left: 300px; padding-top: 50px; overflow-x: hidden; }
      @media (max-width: 767px) { .content-wrapper, .right-side { margin-left: 0; } }
      .main-sidebar { background-color: #191970 !important; }
      .sidebar-menu > li > a { color: #ecf0f1 !important; background-color: #191970 !important; }
      .sidebar-menu > li > a:hover { background-color: #2c2c8a !important; }
      .skin-blue .main-header .navbar { background-color: #191970 !important; }
      .skin-blue .main-header .logo { background-color: #0f0f4f !important; color: #ecf0f1 !important; }
      .content-wrapper, .right-side { background-color: #f4f4f4; }
      .box, .portlet { border: none !important; box-shadow: none !important; }
      .box.box-primary, .box.box-info { border: none !important; }
      .box-body { border: none !important; }
      .box.box-solid.box-primary > .box-header { border-bottom: 1px solid #e0e0e0 !important; }
      .box.box-solid.box-info > .box-header { border-bottom: 1px solid #e0e0e0 !important; }
      .control-label { font-weight: normal !important; }
      .sidebar .selectize-control, .sidebar .shiny-input-container:not(.shiny-input-container-inline) { width: 100% !important; }
      .sidebar .checkbox, .sidebar .action-button { width: 100%; margin-left: 0; margin-right: 0; }
      .sidebar .action-button { margin-top: 5px; }
      .custom-box { border-radius: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24); transition: all 0.3s cubic-bezier(.25,.8,.25,1); margin-bottom: 20px; position: relative; color: white; }
      .custom-box:hover { box-shadow: 0 14px 28px rgba(0,0,0,0.25), 0 10px 10px rgba(0,0,0,0.22); }
      .custom-box .inner { padding: 15px; text-align: center; }
      .custom-box .inner h3 { font-size: 38px; font-weight: bold; margin: 0 0 10px 0; white-space: nowrap; padding: 0; }
      .custom-box .inner p { font-size: 14px; margin: 0; font-weight: bold; }
      .custom-box .icon { position: absolute; right: 10px; top: 10px; font-size: 50px; opacity: 0.3; }
      .bg-purple-custom { background-color: #8e44ad; }
      .bg-green-custom { background-color: #4CAF50; }
      .bg-orange-custom { background-color: #FFC107; }
      .bg-red-custom { background-color: #E53935; }
      .box.box-primary > .box-header { background-color: #191970 !important; color: white !important; }
      .box.box-info > .box-header { background-color: #2c2c8a !important; color: white !important; }
      .selectize-input, .selectize-dropdown { background-color: #ecf0f1 !important; color: #191970 !important; }
      .reactable { height: 500px !important; overflow-y: auto !important; }
    ")),
    
    div(style = "display: flex; justify-content: center; align-items: center; gap: 15px; padding: 0 0 15px 0;",
        tags$img(src = "https://raw.githubusercontent.com/richardquintanilla/uesohiggins/main/www/logo_seremi.png", 
                 height = "90px", style = "display: block;"),
        tags$img(src = "https://raw.githubusercontent.com/richardquintanilla/uesohiggins/main/www/logo_ues_blanco.png", 
                 height = "100px", style = "display: block;")
    ),
    
    sidebarMenu(
      menuItem("✅ GES Vigentes", tabName = "vigentes"),
      menuItem("⚠️ GES Retrasadas", tabName = "retrasadas"),
      menuItem("⏸️ GES Exceptuadas Transitorias", tabName = "exceptuadas")
    ),
    
    br(),
    hr(),
    h4("Filtros", style = "padding-left: 15px; color: #ecf0f1; font-weight: normal; margin-bottom: 10px;"),
    
    selectInput("responsable_filter", "Responsable de Garantía:",
                choices = c(""), 
                selected = "",
                multiple = FALSE,
                selectize = TRUE),
    
    selectInput("problema_filter", "Problema de Salud:",
                choices = c(""), 
                selected = "",
                multiple = FALSE,
                selectize = TRUE),
    
    div(style = "width: 100%; margin-left: 0; margin-right: 0;",
        checkboxInput("oncologicos_check", "🔬 Casos Oncológicos", value = FALSE)),
    
    actionButton("clear_filters", "Limpiar Filtros", icon = icon("eraser"),
                 style = "width: 100%; background-color: #95a5a6; color: white; border: none; margin-top: 5px; margin-left: 0; margin-right: 0;")
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "vigentes",
              fluidRow(
                uiOutput("tarjeta_total_vigentes"),
                uiOutput("tarjeta_tempranas"),
                uiOutput("tarjeta_intermedias"),
                uiOutput("tarjeta_avanzadas")
              ),
              fluidRow(
                box(title = "Top 10 Responsables de Garantía con más Vigentes", status = "info", width = 6,
                    plotlyOutput("top_responsables_vigentes", height = "450px")),
                box(title = "Top 10 Problemas de Salud con más Vigentes", status = "info", width = 6,
                    plotlyOutput("top_problemas_vigentes", height = "450px"))
              ),
              fluidRow(
                box(title = "Detalle de GES Vigentes por Responsable de Garantía y Problema de Salud", status = "primary", solidHeader = TRUE, width = 12,
                    reactableOutput("tabla_detalle_vigentes"))
              ),
              fluidRow(
                box(title = "Evolución Histórica de GES Vigentes", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("grafico_evolucion_vigentes", height = "500px"))
              )
      ),
      
      tabItem(tabName = "retrasadas",
              fluidRow(
                uiOutput("tarjeta_total_retrasadas"),
                uiOutput("tarjeta_nueva_vencida"),
                uiOutput("tarjeta_vencida"),
                uiOutput("tarjeta_vencida_prolongada")
              ),
              fluidRow(
                box(title = "Top 10 Responsables de Garantía con más Retrasadas", status = "info", width = 6,
                    plotlyOutput("top_responsables_retrasadas", height = "450px")),
                box(title = "Top 10 Problemas de Salud con más Retrasadas", status = "info", width = 6,
                    plotlyOutput("top_problemas_retrasadas", height = "450px"))
              ),
              fluidRow(
                box(title = "Detalle de GES Retrasadas por Responsable de Garantía y Problema de Salud", status = "primary", solidHeader = TRUE, width = 12,
                    reactableOutput("tabla_detalle_retrasadas"))
              ),
              fluidRow(
                box(title = "Evolución Histórica de GES Retrasadas", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("grafico_evolucion_retrasadas", height = "500px"))
              )
      ),
      
      tabItem(tabName = "exceptuadas",
              fluidRow(
                uiOutput("tarjeta_total_exceptuadas"),
                uiOutput("tarjeta_exceptuadas_hasta2024"),
                uiOutput("tarjeta_exceptuadas_desde2025")
              ),
              fluidRow(
                box(title = "Top 10 Responsables de Garantía con más Exceptuadas Transitorias", status = "info", width = 6,
                    plotlyOutput("top_responsables_exceptuadas", height = "450px")),
                box(title = "Top 10 Problemas de Salud con más Exceptuadas Transitorias", status = "info", width = 6,
                    plotlyOutput("top_problemas_exceptuadas", height = "450px"))
              ),
              fluidRow(
                box(title = "Detalle de GES Exceptuadas Transitorias por Responsable de Garantía y Problema de Salud", status = "primary", solidHeader = TRUE, width = 12,
                    reactableOutput("tabla_detalle_exceptuadas"))
              ),
              fluidRow(
                box(title = "Evolución Histórica de GES Exceptuadas Transitorias", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("grafico_evolucion_exceptuadas", height = "500px"))
              )
      )
    )
  )
)

# ============================================
# SERVER
# ============================================

server <- function(input, output, session) {
  
  # ------------------------------------------------------------
  # 1. CARGA DE DATOS CON DETECCIÓN DE RUTAS MÚLTIPLES
  # ------------------------------------------------------------
  
  # Rutas posibles para VIGENTES
  rutas_vigentes <- c(
    "data/ges_vigentes.csv",
    "ges/listados/data/ges_vigentes.csv"
  )
  
  # Rutas posibles para RETRASADAS
  rutas_retrasadas <- c(
    "data/ges_retrasadas.csv",
    "ges/listados/data/ges_retrasadas.csv"
  )
  
  # Rutas posibles para EXCEPTUADAS TRANSITORIAS
  rutas_exceptuadas <- c(
    "data/ges_exceptuadas_transitorias.csv",
    "ges/listados/data/ges_exceptuadas_transitorias.csv"
  )
  
  # Encontrar archivos
  ruta_vigentes <- encontrar_archivo(rutas_vigentes)
  ruta_retrasadas <- encontrar_archivo(rutas_retrasadas)
  ruta_exceptuadas <- encontrar_archivo(rutas_exceptuadas)
  
  # Verificar si se encontraron los archivos
  if(is.null(ruta_vigentes)) {
    stop(paste("No se encontró el archivo de VIGENTES en ninguna de estas rutas:", 
               paste(rutas_vigentes, collapse = ", ")))
  }
  
  if(is.null(ruta_retrasadas)) {
    stop(paste("No se encontró el archivo de RETRASADAS en ninguna de estas rutas:", 
               paste(rutas_retrasadas, collapse = ", ")))
  }
  
  if(is.null(ruta_exceptuadas)) {
    stop(paste("No se encontró el archivo de EXCEPTUADAS TRANSITORIAS en ninguna de estas rutas:", 
               paste(rutas_exceptuadas, collapse = ", ")))
  }
  
  # Mensaje de depuración
  cat("✅ Archivo VIGENTES encontrado en:", ruta_vigentes, "\n")
  cat("✅ Archivo RETRASADAS encontrado en:", ruta_retrasadas, "\n")
  cat("✅ Archivo EXCEPTUADAS encontrado en:", ruta_exceptuadas, "\n")
  
  # Cargar VIGENTES (CON NUEVOS RANGOS: 33%, 34-66%, 67%+)
  df_vigentes_raw <- read.csv(ruta_vigentes, stringsAsFactors = FALSE) %>%
    mutate(
      fecha_corte = as.Date(fecha_corte),
      fecha_inicio = as.Date(fecha_inicio),
      fecha_limite = as.Date(fecha_limite),
      responsable_de_garantia = if_else(is.na(responsable_de_garantia), "No especificado", responsable_de_garantia),
      problema_de_salud = if_else(is.na(problema_de_salud), "No especificado", problema_de_salud),
      problema_clasificado = sapply(problema_de_salud, clasificar_problema),
      es_oncologico = sapply(problema_de_salud, es_oncologico),
      clasificacion_avance = case_when(
        porcentaje_avance <= 33 ~ "Tempranas (Avance ≤ 33% del plazo total)",
        porcentaje_avance <= 66 ~ "Intermedias (Avance entre 34 y 66% del plazo total)",
        porcentaje_avance > 66 ~ "Avanzadas (Avance ≥ 67% del plazo total)"
      )
    )
  
  # Cargar RETRASADAS
  df_retrasadas <- read.csv(ruta_retrasadas, stringsAsFactors = FALSE) %>%
    mutate(
      fecha_corte = as.Date(fecha_corte),
      fecha_limite = as.Date(fecha_limite),
      responsable_de_garantia = if_else(is.na(responsable_de_garantia), "No especificado", responsable_de_garantia),
      problema_de_salud = if_else(is.na(problema_de_salud), "No especificado", problema_de_salud),
      tipo_retraso = case_when(
        dias_atraso <= 7 ~ "Nuevas Vencidas (Retraso ≤ 7 días)",
        dias_atraso <= 365 ~ "Vencidas (Retraso entre 8 y 365 días)",
        TRUE ~ "Vencidas Prolongadas (Retraso > 365 días)"
      ),
      problema_clasificado = sapply(problema_de_salud, clasificar_problema),
      es_oncologico = sapply(problema_de_salud, es_oncologico)
    )
  
  # Cargar EXCEPTUADAS TRANSITORIAS
  df_exceptuadas_raw <- read.csv(ruta_exceptuadas, stringsAsFactors = FALSE) %>%
    mutate(
      fecha_corte = as.Date(fecha_corte),
      fecha_excepcion = as.Date(fecha_excepcion),
      problema_de_salud = if_else(is.na(problema_de_salud), "No especificado", problema_de_salud),
      responsable_de_garantia = if_else(is.na(responsable_de_garantia), "No especificado", responsable_de_garantia),
      causal_excepcion = if_else(is.na(causal_excepcion), "No especificado", causal_excepcion),
      problema_clasificado = sapply(problema_de_salud, clasificar_problema),
      es_oncologico = sapply(problema_de_salud, es_oncologico),
      periodo_excepcion = if_else(anio_excepcion <= 2024, "Exceptuadas hasta 2024", "Exceptuadas desde 2025")
    )
  
  # ------------------------------------------------------------
  # 2. PREPARACIÓN DATOS POR CORTE
  # ------------------------------------------------------------
  
  fecha_max_vigentes <- max(df_vigentes_raw$fecha_corte, na.rm = TRUE)
  datos_historicos_vigentes <- df_vigentes_raw
  datos_recientes_vigentes <- df_vigentes_raw %>% filter(fecha_corte == fecha_max_vigentes)
  
  fecha_max_retrasadas <- max(df_retrasadas$fecha_corte, na.rm = TRUE)
  datos_historicos_retrasadas <- df_retrasadas
  datos_recientes_retrasadas <- df_retrasadas %>% filter(fecha_corte == fecha_max_retrasadas)
  
  # Para EXCEPTUADAS - usar el corte más reciente para tarjetas, top y tabla detalle
  fecha_max_exceptuadas <- max(df_exceptuadas_raw$fecha_corte, na.rm = TRUE)
  datos_recientes_exceptuadas <- df_exceptuadas_raw %>% filter(fecha_corte == fecha_max_exceptuadas)
  # Datos históricos completos para el gráfico de evolución
  datos_historicos_exceptuadas <- df_exceptuadas_raw
  
  # ------------------------------------------------------------
  # 3. FILTROS (selección única)
  # ------------------------------------------------------------
  
  observe({
    responsables_ret <- sort(unique(df_retrasadas$responsable_de_garantia))
    problemas_ret <- sort(unique(df_retrasadas$problema_clasificado))
    
    updateSelectInput(session, "responsable_filter", 
                      choices = c("", responsables_ret),
                      selected = "")
    updateSelectInput(session, "problema_filter", 
                      choices = c("", problemas_ret),
                      selected = "")
  })
  
  # Reactivos para VIGENTES
  datos_recientes_filt_vig <- reactive({
    df <- datos_recientes_vigentes
    
    if(input$oncologicos_check) {
      df <- df %>% filter(es_oncologico == TRUE)
    }
    if(input$responsable_filter != "") {
      df <- df %>% filter(responsable_de_garantia == input$responsable_filter)
    }
    if(input$problema_filter != "") {
      df <- df %>% filter(problema_clasificado == input$problema_filter)
    }
    df
  })
  
  datos_historicos_filt_vig <- reactive({
    df <- datos_historicos_vigentes
    
    if(input$oncologicos_check) {
      df <- df %>% filter(es_oncologico == TRUE)
    }
    if(input$responsable_filter != "") {
      df <- df %>% filter(responsable_de_garantia == input$responsable_filter)
    }
    if(input$problema_filter != "") {
      df <- df %>% filter(problema_clasificado == input$problema_filter)
    }
    df
  })
  
  # Reactivos para RETRASADAS
  datos_recientes_filt_ret <- reactive({
    df <- datos_recientes_retrasadas
    
    if(input$oncologicos_check) {
      df <- df %>% filter(es_oncologico == TRUE)
    }
    if(input$responsable_filter != "") {
      df <- df %>% filter(responsable_de_garantia == input$responsable_filter)
    }
    if(input$problema_filter != "") {
      df <- df %>% filter(problema_clasificado == input$problema_filter)
    }
    df
  })
  
  datos_historicos_filt_ret <- reactive({
    df <- datos_historicos_retrasadas
    
    if(input$oncologicos_check) {
      df <- df %>% filter(es_oncologico == TRUE)
    }
    if(input$responsable_filter != "") {
      df <- df %>% filter(responsable_de_garantia == input$responsable_filter)
    }
    if(input$problema_filter != "") {
      df <- df %>% filter(problema_clasificado == input$problema_filter)
    }
    df
  })
  
  # Reactivos para EXCEPTUADAS
  datos_recientes_filt_exc <- reactive({
    df <- datos_recientes_exceptuadas
    
    if(input$oncologicos_check) {
      df <- df %>% filter(es_oncologico == TRUE)
    }
    if(input$responsable_filter != "") {
      df <- df %>% filter(responsable_de_garantia == input$responsable_filter)
    }
    if(input$problema_filter != "") {
      df <- df %>% filter(problema_clasificado == input$problema_filter)
    }
    df
  })
  
  datos_historicos_filt_exc <- reactive({
    df <- datos_historicos_exceptuadas
    
    if(input$oncologicos_check) {
      df <- df %>% filter(es_oncologico == TRUE)
    }
    if(input$responsable_filter != "") {
      df <- df %>% filter(responsable_de_garantia == input$responsable_filter)
    }
    if(input$problema_filter != "") {
      df <- df %>% filter(problema_clasificado == input$problema_filter)
    }
    df
  })
  
  observeEvent(input$clear_filters, {
    updateSelectInput(session, "responsable_filter", selected = "")
    updateSelectInput(session, "problema_filter", selected = "")
    updateCheckboxInput(session, "oncologicos_check", value = FALSE)
  })
  
  # ------------------------------------------------------------
  # 4. TABLAS DE DETALLE
  # ------------------------------------------------------------
  
  # Tabla detalle VIGENTES (CON NUEVOS RANGOS)
  output$tabla_detalle_vigentes <- renderReactable({
    datos <- datos_recientes_filt_vig()
    req(datos)
    
    if(nrow(datos) == 0) {
      return(crear_tabla_detalle(NULL))
    }
    
    tabla_resumen <- datos %>%
      group_by(responsable_de_garantia, problema_clasificado) %>%
      summarise(
        Tempranas = sum(clasificacion_avance == "Tempranas (Avance ≤ 33% del plazo total)"),
        Intermedias = sum(clasificacion_avance == "Intermedias (Avance entre 34 y 66% del plazo total)"),
        Avanzadas = sum(clasificacion_avance == "Avanzadas (Avance ≥ 67% del plazo total)"),
        Total = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(Total))
    
    if(nrow(tabla_resumen) == 0) {
      return(crear_tabla_detalle(NULL))
    }
    
    names(tabla_resumen) <- c("Responsable de Garantía", "Problema de Salud", "Tempranas", "Intermedias", "Avanzadas", "Total")
    
    tabla_resumen <- tabla_resumen %>%
      select(`Responsable de Garantía`, `Problema de Salud`, Total, Tempranas, Intermedias, Avanzadas)
    
    crear_tabla_detalle(
      tabla_resumen,
      col_fijas = c("Responsable de Garantía", "Problema de Salud"),
      col_destacar = "Total"
    )
  })
  
  # Tabla detalle RETRASADAS
  output$tabla_detalle_retrasadas <- renderReactable({
    datos <- datos_recientes_filt_ret()
    req(datos)
    
    if(nrow(datos) == 0) {
      return(crear_tabla_detalle(NULL))
    }
    
    tabla_resumen <- datos %>%
      group_by(responsable_de_garantia, problema_clasificado) %>%
      summarise(
        `Nuevas Vencidas` = sum(tipo_retraso == "Nuevas Vencidas (Retraso ≤ 7 días)"),
        `Vencidas` = sum(tipo_retraso == "Vencidas (Retraso entre 8 y 365 días)"),
        `Vencidas Prolongadas` = sum(tipo_retraso == "Vencidas Prolongadas (Retraso > 365 días)"),
        Total = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(Total))
    
    if(nrow(tabla_resumen) == 0) {
      return(crear_tabla_detalle(NULL))
    }
    
    names(tabla_resumen) <- c("Responsable de Garantía", "Problema de Salud", "Nuevas Vencidas", "Vencidas", "Vencidas Prolongadas", "Total")
    
    tabla_resumen <- tabla_resumen %>%
      select(`Responsable de Garantía`, `Problema de Salud`, Total, `Nuevas Vencidas`, `Vencidas`, `Vencidas Prolongadas`)
    
    crear_tabla_detalle(
      tabla_resumen,
      col_fijas = c("Responsable de Garantía", "Problema de Salud"),
      col_destacar = "Total"
    )
  })
  
  # Tabla detalle EXCEPTUADAS (con Inasistencia y Postergacion - CORREGIDO)
  output$tabla_detalle_exceptuadas <- renderReactable({
    datos <- datos_recientes_filt_exc()
    req(datos)
    
    if(nrow(datos) == 0) {
      return(crear_tabla_detalle(NULL))
    }
    
    tabla_resumen <- datos %>%
      group_by(responsable_de_garantia, problema_clasificado) %>%
      summarise(
        Inasistencia = sum(causal_excepcion == "Inasistencia", na.rm = TRUE),
        Postergacion = sum(grepl("Posterg", causal_excepcion, ignore.case = TRUE), na.rm = TRUE),
        Total = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(Total))
    
    if(nrow(tabla_resumen) == 0) {
      return(crear_tabla_detalle(NULL))
    }
    
    names(tabla_resumen) <- c("Responsable de Garantía", "Problema de Salud", "Inasistencia", "Postergación de la Prestación", "Total")
    
    tabla_resumen <- tabla_resumen %>%
      select(`Responsable de Garantía`, `Problema de Salud`, Total, Inasistencia, `Postergación de la Prestación`)
    
    crear_tabla_detalle(
      tabla_resumen,
      col_fijas = c("Responsable de Garantía", "Problema de Salud"),
      col_destacar = "Total"
    )
  })
  
  # ------------------------------------------------------------
  # 5. OUTPUTS - VIGENTES (CON NUEVOS RANGOS)
  # ------------------------------------------------------------
  
  output$tarjeta_total_vigentes <- renderUI({
    datos <- datos_recientes_filt_vig()
    req(datos)
    div(class = "col-sm-3",
        div(class = "custom-box bg-purple-custom",
            div(class = "icon", icon("check-circle")),
            div(class = "inner", 
                h3(format(nrow(datos), big.mark = ".", decimal.mark = ",")), 
                p("Total GES Vigentes", style = "font-weight: bold;"))))
  })
  
  output$tarjeta_tempranas <- renderUI({
    datos <- datos_recientes_filt_vig()
    req(datos)
    n <- datos %>% filter(clasificacion_avance == "Tempranas (Avance ≤ 33% del plazo total)") %>% nrow()
    div(class = "col-sm-3",
        div(class = "custom-box bg-green-custom",
            div(class = "icon", icon("seedling")),
            div(class = "inner", 
                h3(format(n, big.mark = ".", decimal.mark = ",")), 
                p("Tempranas (Avance ≤ 33% del plazo total)", style = "font-weight: bold;"))))
  })
  
  output$tarjeta_intermedias <- renderUI({
    datos <- datos_recientes_filt_vig()
    req(datos)
    n <- datos %>% filter(clasificacion_avance == "Intermedias (Avance entre 34 y 66% del plazo total)") %>% nrow()
    div(class = "col-sm-3",
        div(class = "custom-box bg-orange-custom",
            div(class = "icon", icon("chart-line")),
            div(class = "inner", 
                h3(format(n, big.mark = ".", decimal.mark = ",")), 
                p("Intermedias (Avance entre 34 y 66% del plazo total)", style = "font-weight: bold;"))))
  })
  
  output$tarjeta_avanzadas <- renderUI({
    datos <- datos_recientes_filt_vig()
    req(datos)
    n <- datos %>% filter(clasificacion_avance == "Avanzadas (Avance ≥ 67% del plazo total)") %>% nrow()
    div(class = "col-sm-3",
        div(class = "custom-box bg-red-custom",
            div(class = "icon", icon("flag-checkered")),
            div(class = "inner", 
                h3(format(n, big.mark = ".", decimal.mark = ",")), 
                p("Avanzadas (Avance ≥ 67% del plazo total)", style = "font-weight: bold;"))))
  })
  
  output$top_responsables_vigentes <- renderPlotly({
    datos <- datos_recientes_filt_vig()
    req(datos)
    if(nrow(datos) == 0) return(plotly::plot_ly() %>% layout(title = "No hay datos"))
    top <- datos %>% count(responsable_de_garantia) %>% arrange(desc(n)) %>% head(10)
    max_n <- max(top$n)
    
    plot_ly(top, x = ~n, y = ~reorder(responsable_de_garantia, n), type = "bar", orientation = "h",
            text = ~format(n, big.mark = ".", decimal.mark = ","), textposition = "outside",
            textfont = list(size = 11),
            marker = list(color = "#191970"),
            hovertemplate = paste("Total: %{x}<extra></extra>")) %>%
      layout(autosize = TRUE,
             xaxis = list(title = "", range = c(0, max_n * 1.25), showticklabels = FALSE, showgrid = FALSE), 
             yaxis = list(title = ""), 
             margin = list(l = 150, r = 120, t = 10, b = 10))
  })
  
  output$top_problemas_vigentes <- renderPlotly({
    datos <- datos_recientes_filt_vig()
    req(datos)
    if(nrow(datos) == 0) return(plotly::plot_ly() %>% layout(title = "No hay datos"))
    top <- datos %>% count(problema_clasificado) %>% arrange(desc(n)) %>% head(10)
    max_n <- max(top$n)
    
    plot_ly(top, x = ~n, y = ~reorder(problema_clasificado, n), type = "bar", orientation = "h",
            text = ~format(n, big.mark = ".", decimal.mark = ","), textposition = "outside",
            textfont = list(size = 11),
            marker = list(color = "#191970"),
            hovertemplate = paste("Total: %{x}<extra></extra>")) %>%
      layout(autosize = TRUE,
             xaxis = list(title = "", range = c(0, max_n * 1.25), showticklabels = FALSE, showgrid = FALSE), 
             yaxis = list(title = ""), 
             margin = list(l = 200, r = 120, t = 10, b = 10))
  })
  
  output$grafico_evolucion_vigentes <- renderPlotly({
    df_hist <- datos_historicos_filt_vig()
    req(df_hist)
    if(nrow(df_hist) == 0) return(plotly::plot_ly() %>% layout(title = "No hay datos históricos"))
    
    evolucion <- df_hist %>%
      group_by(fecha_corte, clasificacion_avance) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(fecha_corte) %>%
      group_by(clasificacion_avance) %>%
      mutate(
        n_prev = lag(n),
        variacion = n - n_prev,
        variacion_pct = (n / n_prev - 1) * 100
      ) %>%
      ungroup() %>%
      mutate(clasificacion_avance = factor(clasificacion_avance, levels = c("Avanzadas (Avance ≥ 67% del plazo total)", "Intermedias (Avance entre 34 y 66% del plazo total)", "Tempranas (Avance ≤ 33% del plazo total)")))
    
    totales_por_fecha <- evolucion %>%
      group_by(fecha_corte) %>%
      summarise(total = sum(n), .groups = "drop") %>%
      arrange(fecha_corte) %>%
      mutate(
        total_prev = lag(total),
        variacion_total = total - total_prev,
        variacion_total_pct = (total / total_prev - 1) * 100
      )
    
    p <- plot_ly()
    colores_avance <- c("Tempranas (Avance ≤ 33% del plazo total)" = "#4CAF50", 
                        "Intermedias (Avance entre 34 y 66% del plazo total)" = "#FFC107", 
                        "Avanzadas (Avance ≥ 67% del plazo total)" = "#E53935")
    
    for(nivel in c("Avanzadas (Avance ≥ 67% del plazo total)", "Intermedias (Avance entre 34 y 66% del plazo total)", "Tempranas (Avance ≤ 33% del plazo total)")) {
      datos_nivel <- evolucion %>% filter(clasificacion_avance == nivel)
      if(nrow(datos_nivel) > 0) {
        hover_text <- paste0(
          "<b>", nivel, "</b><br>",
          "Cantidad: ", format(datos_nivel$n, big.mark = ".", decimal.mark = ","), "<br>",
          "Fecha: ", format(datos_nivel$fecha_corte, "%d/%m/%y"), "<br>",
          ifelse(!is.na(datos_nivel$variacion),
                 paste0("Variación: ", format(datos_nivel$variacion, big.mark = ".", decimal.mark = ","),
                        " (", round(datos_nivel$variacion_pct, 1), "%)"),
                 "Variación: -")
        )
        
        p <- p %>% add_trace(data = datos_nivel, x = ~fecha_corte, y = ~n, name = nivel, type = "bar",
                             marker = list(color = colores_avance[nivel]),
                             hovertemplate = paste(hover_text, "<extra></extra>"))
      }
    }
    
    if(nrow(totales_por_fecha) > 0) {
      total_hover <- paste0(
        "<b>Total GES Vigentes</b><br>",
        "Total: ", format(totales_por_fecha$total, big.mark = ".", decimal.mark = ","), "<br>",
        "Fecha: ", format(totales_por_fecha$fecha_corte, "%d/%m/%y"), "<br>",
        ifelse(!is.na(totales_por_fecha$variacion_total),
               paste0("Variación: ", format(totales_por_fecha$variacion_total, big.mark = ".", decimal.mark = ","),
                      " (", round(totales_por_fecha$variacion_total_pct, 1), "%)"),
               "Variación: -")
      )
      
      p <- p %>% add_trace(data = totales_por_fecha, x = ~fecha_corte, y = ~total, type = "scatter", mode = "text",
                           text = ~format(total, big.mark = ".", decimal.mark = ","), textposition = "top center",
                           hoverinfo = "text", hovertext = total_hover, showlegend = FALSE, inherit = FALSE)
    }
    
    fechas_unicas <- sort(unique(evolucion$fecha_corte))
    
    p %>% layout(barmode = "stack", title = NULL,
                 xaxis = list(title = "Fecha de Corte", tickangle = -45, tickmode = "array", 
                              tickvals = fechas_unicas, ticktext = format(fechas_unicas, "%d/%m/%y")),
                 yaxis = list(title = "Cantidad de Garantías Vigentes", tickformat = ".0s"),
                 legend = list(orientation = "h", yanchor = "bottom", y = -0.3, xanchor = "center", x = 0.5))
  })
  
  # ------------------------------------------------------------
  # 6. OUTPUTS - RETRASADAS
  # ------------------------------------------------------------
  
  output$tarjeta_total_retrasadas <- renderUI({
    datos <- datos_recientes_filt_ret()
    req(datos)
    div(class = "col-sm-3",
        div(class = "custom-box bg-purple-custom",
            div(class = "icon", icon("exclamation-triangle")),
            div(class = "inner", 
                h3(format(nrow(datos), big.mark = ".", decimal.mark = ",")), 
                p("Total GES Retrasadas", style = "font-weight: bold;"))))
  })
  
  output$tarjeta_nueva_vencida <- renderUI({
    datos <- datos_recientes_filt_ret()
    req(datos)
    n <- datos %>% filter(tipo_retraso == "Nuevas Vencidas (Retraso ≤ 7 días)") %>% nrow()
    div(class = "col-sm-3",
        div(class = "custom-box bg-green-custom",
            div(class = "icon", icon("clock")),
            div(class = "inner", 
                h3(format(n, big.mark = ".", decimal.mark = ",")), 
                p("Nuevas Vencidas (Retraso ≤ 7 días)", style = "font-weight: bold;"))))
  })
  
  output$tarjeta_vencida <- renderUI({
    datos <- datos_recientes_filt_ret()
    req(datos)
    n <- datos %>% filter(tipo_retraso == "Vencidas (Retraso entre 8 y 365 días)") %>% nrow()
    div(class = "col-sm-3",
        div(class = "custom-box bg-orange-custom",
            div(class = "icon", icon("hourglass-half")),
            div(class = "inner", 
                h3(format(n, big.mark = ".", decimal.mark = ",")), 
                p("Vencidas (Retraso entre 8 y 365 días)", style = "font-weight: bold;"))))
  })
  
  output$tarjeta_vencida_prolongada <- renderUI({
    datos <- datos_recientes_filt_ret()
    req(datos)
    n <- datos %>% filter(tipo_retraso == "Vencidas Prolongadas (Retraso > 365 días)") %>% nrow()
    div(class = "col-sm-3",
        div(class = "custom-box bg-red-custom",
            div(class = "icon", icon("calendar-times")),
            div(class = "inner", 
                h3(format(n, big.mark = ".", decimal.mark = ",")), 
                p("Vencidas Prolongadas (Retraso > 365 días)", style = "font-weight: bold;"))))
  })
  
  output$top_responsables_retrasadas <- renderPlotly({
    datos <- datos_recientes_filt_ret()
    req(datos)
    if(nrow(datos) == 0) return(plotly::plot_ly() %>% layout(title = "No hay datos"))
    top <- datos %>% count(responsable_de_garantia) %>% arrange(desc(n)) %>% head(10)
    max_n <- max(top$n)
    
    plot_ly(top, x = ~n, y = ~reorder(responsable_de_garantia, n), type = "bar", orientation = "h", 
            text = ~format(n, big.mark = ".", decimal.mark = ","), textposition = "outside", 
            textfont = list(size = 11),
            marker = list(color = "#191970"),
            hovertemplate = paste("Total: %{x}<extra></extra>")) %>%
      layout(autosize = TRUE,
             xaxis = list(title = "", range = c(0, max_n * 1.25), showticklabels = FALSE, showgrid = FALSE), 
             yaxis = list(title = ""), 
             margin = list(l = 150, r = 120, t = 10, b = 10))
  })
  
  output$top_problemas_retrasadas <- renderPlotly({
    datos <- datos_recientes_filt_ret()
    req(datos)
    if(nrow(datos) == 0) return(plotly::plot_ly() %>% layout(title = "No hay datos"))
    top <- datos %>% count(problema_clasificado) %>% arrange(desc(n)) %>% head(10)
    max_n <- max(top$n)
    
    plot_ly(top, x = ~n, y = ~reorder(problema_clasificado, n), type = "bar", orientation = "h", 
            text = ~format(n, big.mark = ".", decimal.mark = ","), textposition = "outside", 
            textfont = list(size = 11),
            marker = list(color = "#191970"),
            hovertemplate = paste("Total: %{x}<extra></extra>")) %>%
      layout(autosize = TRUE,
             xaxis = list(title = "", range = c(0, max_n * 1.25), showticklabels = FALSE, showgrid = FALSE), 
             yaxis = list(title = ""), 
             margin = list(l = 200, r = 120, t = 10, b = 10))
  })
  
  output$grafico_evolucion_retrasadas <- renderPlotly({
    df_hist <- datos_historicos_filt_ret()
    req(df_hist)
    if(nrow(df_hist) == 0) return(plotly::plot_ly() %>% layout(title = "No hay datos"))
    
    resultado <- df_hist %>%
      group_by(fecha_corte, tipo_retraso) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(fecha_corte) %>%
      mutate(tipo_retraso = factor(tipo_retraso, levels = c("Vencidas Prolongadas (Retraso > 365 días)", "Vencidas (Retraso entre 8 y 365 días)", "Nuevas Vencidas (Retraso ≤ 7 días)"))) %>%
      group_by(tipo_retraso) %>%
      mutate(variacion = n - lag(n), variacion_pct = (n / lag(n) - 1) * 100) %>%
      ungroup()
    
    totales_por_fecha <- resultado %>%
      group_by(fecha_corte) %>%
      summarise(total = sum(n), .groups = "drop") %>%
      mutate(variacion_total = total - lag(total), variacion_total_pct = (total / lag(total) - 1) * 100)
    
    p <- plot_ly()
    colores <- c("Nuevas Vencidas (Retraso ≤ 7 días)" = "#4CAF50", 
                 "Vencidas (Retraso entre 8 y 365 días)" = "#FFC107", 
                 "Vencidas Prolongadas (Retraso > 365 días)" = "#E53935")
    
    for(tipo in c("Vencidas Prolongadas (Retraso > 365 días)", "Vencidas (Retraso entre 8 y 365 días)", "Nuevas Vencidas (Retraso ≤ 7 días)")) {
      datos_tipo <- resultado %>% filter(tipo_retraso == tipo)
      if(nrow(datos_tipo) > 0) {
        hover_text <- paste0(
          "<b>", tipo, "</b><br>",
          "Cantidad: ", format(datos_tipo$n, big.mark = ".", decimal.mark = ","), "<br>",
          "Fecha: ", format(datos_tipo$fecha_corte, "%d/%m/%y"), "<br>",
          ifelse(!is.na(datos_tipo$variacion),
                 paste0("Variación: ", format(datos_tipo$variacion, big.mark = ".", decimal.mark = ","),
                        " (", round(datos_tipo$variacion_pct, 1), "%)"),
                 "Variación: -")
        )
        
        p <- p %>% add_trace(data = datos_tipo, x = ~fecha_corte, y = ~n, name = tipo, type = "bar", 
                             marker = list(color = colores[tipo]), 
                             hovertemplate = paste(hover_text, "<extra></extra>"))
      }
    }
    
    if(nrow(totales_por_fecha) > 0) {
      total_hover <- paste0(
        "<b>Total GES Retrasadas</b><br>",
        "Total: ", format(totales_por_fecha$total, big.mark = ".", decimal.mark = ","), "<br>",
        "Fecha: ", format(totales_por_fecha$fecha_corte, "%d/%m/%y"), "<br>",
        ifelse(!is.na(totales_por_fecha$variacion_total),
               paste0("Variación: ", format(totales_por_fecha$variacion_total, big.mark = ".", decimal.mark = ","),
                      " (", round(totales_por_fecha$variacion_total_pct, 1), "%)"),
               "Variación: -")
      )
      
      p <- p %>% add_trace(data = totales_por_fecha, x = ~fecha_corte, y = ~total, type = "scatter", mode = "text", 
                           text = ~format(total, big.mark = ".", decimal.mark = ","), textposition = "top center", 
                           hoverinfo = "text", hovertext = total_hover, showlegend = FALSE, inherit = FALSE)
    }
    
    fechas_unicas <- sort(unique(resultado$fecha_corte))
    
    p %>% layout(barmode = "stack", title = NULL, 
                 xaxis = list(title = "Fecha de corte", tickangle = -45, tickmode = "array", 
                              tickvals = fechas_unicas, ticktext = format(fechas_unicas, "%d/%m/%y")), 
                 yaxis = list(title = "N° GES Retrasadas", tickformat = ".0s"), 
                 legend = list(orientation = "h", yanchor = "bottom", y = -0.3, xanchor = "center", x = 0.5))
  })
  
  # ------------------------------------------------------------
  # 7. OUTPUTS - EXCEPTUADAS TRANSITORIAS
  # ------------------------------------------------------------
  
  output$tarjeta_total_exceptuadas <- renderUI({
    datos <- datos_recientes_filt_exc()
    req(datos)
    div(class = "col-sm-4",
        div(class = "custom-box bg-purple-custom",
            div(class = "icon", icon("pause-circle")),
            div(class = "inner", 
                h3(format(nrow(datos), big.mark = ".", decimal.mark = ",")), 
                p("Total GES Exceptuadas Transitorias", style = "font-weight: bold;"))))
  })
  
  output$tarjeta_exceptuadas_hasta2024 <- renderUI({
    datos <- datos_recientes_filt_exc()
    req(datos)
    n <- datos %>% filter(periodo_excepcion == "Exceptuadas hasta 2024") %>% nrow()
    div(class = "col-sm-4",
        div(class = "custom-box bg-green-custom",
            div(class = "icon", icon("calendar-alt")),
            div(class = "inner", 
                h3(format(n, big.mark = ".", decimal.mark = ",")), 
                p("Exceptuadas hasta 2024", style = "font-weight: bold;"))))
  })
  
  output$tarjeta_exceptuadas_desde2025 <- renderUI({
    datos <- datos_recientes_filt_exc()
    req(datos)
    n <- datos %>% filter(periodo_excepcion == "Exceptuadas desde 2025") %>% nrow()
    div(class = "col-sm-4",
        div(class = "custom-box bg-orange-custom",
            div(class = "icon", icon("calendar-plus")),
            div(class = "inner", 
                h3(format(n, big.mark = ".", decimal.mark = ",")), 
                p("Exceptuadas desde 2025", style = "font-weight: bold;"))))
  })
  
  output$top_responsables_exceptuadas <- renderPlotly({
    datos <- datos_recientes_filt_exc()
    req(datos)
    if(nrow(datos) == 0) return(plotly::plot_ly() %>% layout(title = "No hay datos"))
    top <- datos %>% count(responsable_de_garantia) %>% arrange(desc(n)) %>% head(10)
    max_n <- max(top$n)
    
    plot_ly(top, x = ~n, y = ~reorder(responsable_de_garantia, n), type = "bar", orientation = "h",
            text = ~format(n, big.mark = ".", decimal.mark = ","), textposition = "outside",
            textfont = list(size = 11),
            marker = list(color = "#191970"),
            hovertemplate = paste("Total: %{x}<extra></extra>")) %>%
      layout(autosize = TRUE,
             xaxis = list(title = "", range = c(0, max_n * 1.25), showticklabels = FALSE, showgrid = FALSE), 
             yaxis = list(title = ""), 
             margin = list(l = 150, r = 120, t = 10, b = 10))
  })
  
  output$top_problemas_exceptuadas <- renderPlotly({
    datos <- datos_recientes_filt_exc()
    req(datos)
    if(nrow(datos) == 0) return(plotly::plot_ly() %>% layout(title = "No hay datos"))
    top <- datos %>% count(problema_clasificado) %>% arrange(desc(n)) %>% head(10)
    max_n <- max(top$n)
    
    plot_ly(top, x = ~n, y = ~reorder(problema_clasificado, n), type = "bar", orientation = "h",
            text = ~format(n, big.mark = ".", decimal.mark = ","), textposition = "outside",
            textfont = list(size = 11),
            marker = list(color = "#191970"),
            hovertemplate = paste("Total: %{x}<extra></extra>")) %>%
      layout(autosize = TRUE,
             xaxis = list(title = "", range = c(0, max_n * 1.25), showticklabels = FALSE, showgrid = FALSE), 
             yaxis = list(title = ""), 
             margin = list(l = 200, r = 120, t = 10, b = 10))
  })
  
  output$grafico_evolucion_exceptuadas <- renderPlotly({
    df_hist <- datos_historicos_filt_exc()
    req(df_hist)
    if(nrow(df_hist) == 0) return(plotly::plot_ly() %>% layout(title = "No hay datos históricos"))
    
    # Evolución por período (desde 2025 primero, luego hasta 2024 - orden invertido)
    evolucion <- df_hist %>%
      group_by(fecha_corte, periodo_excepcion) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(fecha_corte) %>%
      group_by(periodo_excepcion) %>%
      mutate(
        variacion = n - lag(n),
        variacion_pct = (n / lag(n) - 1) * 100
      ) %>%
      ungroup()
    
    totales_por_fecha <- evolucion %>%
      group_by(fecha_corte) %>%
      summarise(total = sum(n), .groups = "drop") %>%
      arrange(fecha_corte) %>%
      mutate(
        variacion_total = total - lag(total),
        variacion_total_pct = (total / lag(total) - 1) * 100
      )
    
    p <- plot_ly()
    colores_periodo <- c("Exceptuadas desde 2025" = "#FFC107", "Exceptuadas hasta 2024" = "#4CAF50")
    
    # Orden invertido: mostrar primero "desde 2025" y luego "hasta 2024"
    for(periodo in c("Exceptuadas desde 2025", "Exceptuadas hasta 2024")) {
      datos_periodo <- evolucion %>% filter(periodo_excepcion == periodo)
      if(nrow(datos_periodo) > 0) {
        hover_text <- paste0(
          "<b>", periodo, "</b><br>",
          "Cantidad: ", format(datos_periodo$n, big.mark = ".", decimal.mark = ","), "<br>",
          "Fecha: ", format(datos_periodo$fecha_corte, "%d/%m/%y"), "<br>",
          ifelse(!is.na(datos_periodo$variacion),
                 paste0("Variación: ", format(datos_periodo$variacion, big.mark = ".", decimal.mark = ","),
                        " (", round(datos_periodo$variacion_pct, 1), "%)"),
                 "Variación: -")
        )
        
        p <- p %>% add_trace(data = datos_periodo, x = ~fecha_corte, y = ~n, name = periodo, type = "bar",
                             marker = list(color = colores_periodo[periodo]),
                             hovertemplate = paste(hover_text, "<extra></extra>"))
      }
    }
    
    if(nrow(totales_por_fecha) > 0) {
      total_hover <- paste0(
        "<b>Total GES Exceptuadas Transitorias</b><br>",
        "Total: ", format(totales_por_fecha$total, big.mark = ".", decimal.mark = ","), "<br>",
        "Fecha: ", format(totales_por_fecha$fecha_corte, "%d/%m/%y"), "<br>",
        ifelse(!is.na(totales_por_fecha$variacion_total),
               paste0("Variación: ", format(totales_por_fecha$variacion_total, big.mark = ".", decimal.mark = ","),
                      " (", round(totales_por_fecha$variacion_total_pct, 1), "%)"),
               "Variación: -")
      )
      
      p <- p %>% add_trace(data = totales_por_fecha, x = ~fecha_corte, y = ~total, type = "scatter", mode = "text",
                           text = ~format(total, big.mark = ".", decimal.mark = ","), textposition = "top center",
                           hoverinfo = "text", hovertext = total_hover, showlegend = FALSE, inherit = FALSE)
    }
    
    fechas_unicas <- sort(unique(evolucion$fecha_corte))
    
    p %>% layout(barmode = "stack", title = NULL,
                 xaxis = list(title = "Fecha de Corte", tickangle = -45, tickmode = "array", 
                              tickvals = fechas_unicas, ticktext = format(fechas_unicas, "%d/%m/%y")),
                 yaxis = list(title = "Cantidad de Garantías Exceptuadas", tickformat = ".0s"),
                 legend = list(orientation = "h", yanchor = "bottom", y = -0.3, xanchor = "center", x = 0.5))
  })
  
  output$fecha_corte_header <- renderText({
    paste("📅 Fecha de corte:", format(fecha_max_retrasadas, "%d-%m-%Y"))
  })
}

shinyApp(ui = ui, server = server)
