# app.R - Vigilancia GES (VERSIÓN LIMPIA - SIN CLASIFICACIÓN REDUNDANTE)

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(lubridate)
library(reactable)
library(htmltools)
library(fst)

# ============================================
# FUNCIONES COMUNES (solo las necesarias)
# ============================================

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
      .skin-blue .main-header .logo { background-color: #191970 !important; }
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
      
      .sidebar-menu {margin-top: 0 !important; padding-top: 10px !important;}
      .main-sidebar, .sidebar {padding-top: 0 !important; margin-top: 0 !important; background-color: #191970 !important;}
      .wrapper {background-color: #191970 !important;}
      
      #clear_filters {background-color: #EEE9E9 !important;color: #191970 !important;}
      #clear_filters:hover {background-color: #d3d3d3 !important; color: #191970 !important;}
      .sidebar-menu > li.active > a {border-left-color: #ff0000 !important;}
      .sidebar-menu > li > a:hover {border-left-color: transparent !important; background-color: #EEE9E9 !important; color: #191970 !important;}
      .skin-blue .main-header .sidebar-toggle:hover {background-color: #EEE9E9 !important;}
      
    ")),
          
          div(style = "display: flex; justify-content: center; align-items: center; gap: 15px; padding: 0 0 0 0; margin: 0; margin-top: 10px;",
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
          
          actionButton("clear_filters", "Limpiar filtros", icon = icon("eraser"),
                       style = "width: 100%; background-color: #95a5a6; color: #191970; border: none; margin-top: 5px; margin-left: 0; margin-right: 0;")
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
     # 1. CARGA DE DATOS
     # ------------------------------------------------------------
     
     # Rutas para archivos RECIENTES (todas las columnas)
     rutas_vigentes_reciente <- c("data/ges_vigentes_reciente.fst", "ges/listados/data/ges_vigentes_reciente.fst")
     rutas_retrasadas_reciente <- c("data/ges_retrasadas_reciente.fst", "ges/listados/data/ges_retrasadas_reciente.fst")
     rutas_exceptuadas_reciente <- c("data/ges_exceptuadas_reciente.fst", "ges/listados/data/ges_exceptuadas_reciente.fst")
     
     # Rutas para archivos HISTÓRICOS LIGEROS (solo para gráficos de evolución)
     rutas_vigentes_historico <- c("data/ges_vigentes_historico_ligero.fst", "ges/listados/data/ges_vigentes_historico_ligero.fst")
     rutas_retrasadas_historico <- c("data/ges_retrasadas_historico_ligero.fst", "ges/listados/data/ges_retrasadas_historico_ligero.fst")
     rutas_exceptuadas_historico <- c("data/ges_exceptuadas_historico_ligero.fst", "ges/listados/data/ges_exceptuadas_historico_ligero.fst")
     
     # Encontrar archivos RECIENTES
     ruta_vigentes_reciente <- encontrar_archivo(rutas_vigentes_reciente)
     ruta_retrasadas_reciente <- encontrar_archivo(rutas_retrasadas_reciente)
     ruta_exceptuadas_reciente <- encontrar_archivo(rutas_exceptuadas_reciente)
     
     # Encontrar archivos HISTÓRICOS
     ruta_vigentes_historico <- encontrar_archivo(rutas_vigentes_historico)
     ruta_retrasadas_historico <- encontrar_archivo(rutas_retrasadas_historico)
     ruta_exceptuadas_historico <- encontrar_archivo(rutas_exceptuadas_historico)
     
     # Verificar archivos RECIENTES
     if(is.null(ruta_vigentes_reciente)) stop("No se encontró archivo RECIENTE de VIGENTES")
     if(is.null(ruta_retrasadas_reciente)) stop("No se encontró archivo RECIENTE de RETRASADAS")
     if(is.null(ruta_exceptuadas_reciente)) stop("No se encontró archivo RECIENTE de EXCEPTUADAS")
     
     cat("✅ Archivos RECIENTES encontrados\n")
     
     # ------------------------------------------------------------
     # 2. CARGAR DATOS RECIENTES (ya vienen con clasificación incluida)
     # ------------------------------------------------------------
     
     datos_recientes_vigentes <- read_fst(ruta_vigentes_reciente, as.data.table = FALSE)
     datos_recientes_retrasadas <- read_fst(ruta_retrasadas_reciente, as.data.table = FALSE)
     datos_recientes_exceptuadas <- read_fst(ruta_exceptuadas_reciente, as.data.table = FALSE)
     
     # Para RETRASADAS, agregar tipo_retraso (no estaba en el archivo reciente)
     datos_recientes_retrasadas <- datos_recientes_retrasadas %>%
          mutate(
               tipo_retraso = case_when(
                    dias_atraso <= 7 ~ "Nuevas Vencidas (Retraso ≤ 7 días)",
                    dias_atraso <= 365 ~ "Vencidas (Retraso entre 8 y 365 días)",
                    TRUE ~ "Vencidas Prolongadas (Retraso > 365 días)"
               )
          )
     
     cat("📊 Datos RECIENTES cargados:\n")
     cat("  VIGENTES:", nrow(datos_recientes_vigentes), "registros\n")
     cat("  RETRASADAS:", nrow(datos_recientes_retrasadas), "registros\n")
     cat("  EXCEPTUADAS:", nrow(datos_recientes_exceptuadas), "registros\n")
     
     # ------------------------------------------------------------
     # 3. CARGAR DATOS HISTÓRICOS LIGEROS
     # ------------------------------------------------------------
     
     if(!is.null(ruta_vigentes_historico)) {
          datos_historicos_vigentes <- read_fst(ruta_vigentes_historico, as.data.table = FALSE)
          cat("✅ Histórico VIGENTES LIGERO cargado:", nrow(datos_historicos_vigentes), "registros\n")
     } else {
          datos_historicos_vigentes <- datos_recientes_vigentes %>%
               select(fecha_corte, clasificacion_avance, problema_clasificado, responsable_de_garantia, es_oncologico)
          cat("⚠️ Usando datos RECIENTES como respaldo para histórico VIGENTES\n")
     }
     
     if(!is.null(ruta_retrasadas_historico)) {
          datos_historicos_retrasadas <- read_fst(ruta_retrasadas_historico, as.data.table = FALSE)
          cat("✅ Histórico RETRASADAS LIGERO cargado:", nrow(datos_historicos_retrasadas), "registros\n")
     } else {
          datos_historicos_retrasadas <- datos_recientes_retrasadas %>%
               select(fecha_corte, tipo_retraso, problema_clasificado, responsable_de_garantia, es_oncologico)
          cat("⚠️ Usando datos RECIENTES como respaldo para histórico RETRASADAS\n")
     }
     
     if(!is.null(ruta_exceptuadas_historico)) {
          datos_historicos_exceptuadas <- read_fst(ruta_exceptuadas_historico, as.data.table = FALSE)
          cat("✅ Histórico EXCEPTUADAS LIGERO cargado:", nrow(datos_historicos_exceptuadas), "registros\n")
     } else {
          datos_historicos_exceptuadas <- datos_recientes_exceptuadas %>%
               select(fecha_corte, periodo_excepcion, problema_clasificado, responsable_de_garantia, es_oncologico)
          cat("⚠️ Usando datos RECIENTES como respaldo para histórico EXCEPTUADAS\n")
     }
     
     # Fechas máximas
     fecha_max_ret <- max(datos_recientes_retrasadas$fecha_corte, na.rm = TRUE)
     
     # ------------------------------------------------------------
     # 4. FILTROS
     # ------------------------------------------------------------
     
     observe({
          responsables_ret <- sort(unique(datos_recientes_retrasadas$responsable_de_garantia))
          problemas_ret <- sort(unique(datos_recientes_retrasadas$problema_clasificado))
          
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
          if(input$oncologicos_check) df <- df %>% filter(es_oncologico == TRUE)
          if(input$responsable_filter != "") df <- df %>% filter(responsable_de_garantia == input$responsable_filter)
          if(input$problema_filter != "") df <- df %>% filter(problema_clasificado == input$problema_filter)
          df
     })
     
     datos_historicos_filt_vig <- reactive({
          df <- datos_historicos_vigentes
          if(input$oncologicos_check) df <- df %>% filter(es_oncologico == TRUE)
          if(input$responsable_filter != "") df <- df %>% filter(responsable_de_garantia == input$responsable_filter)
          if(input$problema_filter != "") df <- df %>% filter(problema_clasificado == input$problema_filter)
          df
     })
     
     # Reactivos para RETRASADAS
     datos_recientes_filt_ret <- reactive({
          df <- datos_recientes_retrasadas
          if(input$oncologicos_check) df <- df %>% filter(es_oncologico == TRUE)
          if(input$responsable_filter != "") df <- df %>% filter(responsable_de_garantia == input$responsable_filter)
          if(input$problema_filter != "") df <- df %>% filter(problema_clasificado == input$problema_filter)
          df
     })
     
     datos_historicos_filt_ret <- reactive({
          df <- datos_historicos_retrasadas
          if(input$oncologicos_check) df <- df %>% filter(es_oncologico == TRUE)
          if(input$responsable_filter != "") df <- df %>% filter(responsable_de_garantia == input$responsable_filter)
          if(input$problema_filter != "") df <- df %>% filter(problema_clasificado == input$problema_filter)
          df
     })
     
     # Reactivos para EXCEPTUADAS
     datos_recientes_filt_exc <- reactive({
          df <- datos_recientes_exceptuadas
          if(input$oncologicos_check) df <- df %>% filter(es_oncologico == TRUE)
          if(input$responsable_filter != "") df <- df %>% filter(responsable_de_garantia == input$responsable_filter)
          if(input$problema_filter != "") df <- df %>% filter(problema_clasificado == input$problema_filter)
          df
     })
     
     datos_historicos_filt_exc <- reactive({
          df <- datos_historicos_exceptuadas
          if(input$oncologicos_check) df <- df %>% filter(es_oncologico == TRUE)
          if(input$responsable_filter != "") df <- df %>% filter(responsable_de_garantia == input$responsable_filter)
          if(input$problema_filter != "") df <- df %>% filter(problema_clasificado == input$problema_filter)
          df
     })
     
     observeEvent(input$clear_filters, {
          updateSelectInput(session, "responsable_filter", selected = "")
          updateSelectInput(session, "problema_filter", selected = "")
          updateCheckboxInput(session, "oncologicos_check", value = FALSE)
     })
     
     # ------------------------------------------------------------
     # 5. TABLAS DE DETALLE
     # ------------------------------------------------------------
     
     output$tabla_detalle_vigentes <- renderReactable({
          datos <- datos_recientes_filt_vig()
          req(datos)
          
          if(nrow(datos) == 0) return(crear_tabla_detalle(NULL))
          
          tabla_resumen <- datos %>%
               group_by(responsable_de_garantia, problema_clasificado) %>%
               summarise(
                    Tempranas = sum(clasificacion_avance == "Tempranas (Avance ≤ 33% del plazo total)"),
                    Intermedias = sum(clasificacion_avance == "Intermedias (Avance entre 34 y 66% del plazo total)"),
                    Avanzadas = sum(clasificacion_avance == "Avanzadas (Avance > 66% del plazo total)"),
                    Total = n(),
                    .groups = "drop"
               ) %>%
               arrange(desc(Total))
          
          if(nrow(tabla_resumen) == 0) return(crear_tabla_detalle(NULL))
          
          names(tabla_resumen) <- c("Responsable de Garantía", "Problema de Salud", "Tempranas", "Intermedias", "Avanzadas", "Total")
          
          tabla_resumen <- tabla_resumen %>%
               select(`Responsable de Garantía`, `Problema de Salud`, Total, Tempranas, Intermedias, Avanzadas)
          
          crear_tabla_detalle(tabla_resumen, col_fijas = c("Responsable de Garantía", "Problema de Salud"), col_destacar = "Total")
     })
     
     output$tabla_detalle_retrasadas <- renderReactable({
          datos <- datos_recientes_filt_ret()
          req(datos)
          
          if(nrow(datos) == 0) return(crear_tabla_detalle(NULL))
          
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
          
          if(nrow(tabla_resumen) == 0) return(crear_tabla_detalle(NULL))
          
          names(tabla_resumen) <- c("Responsable de Garantía", "Problema de Salud", "Nuevas Vencidas", "Vencidas", "Vencidas Prolongadas", "Total")
          
          tabla_resumen <- tabla_resumen %>%
               select(`Responsable de Garantía`, `Problema de Salud`, Total, `Nuevas Vencidas`, `Vencidas`, `Vencidas Prolongadas`)
          
          crear_tabla_detalle(tabla_resumen, col_fijas = c("Responsable de Garantía", "Problema de Salud"), col_destacar = "Total")
     })
     
     output$tabla_detalle_exceptuadas <- renderReactable({
          datos <- datos_recientes_filt_exc()
          req(datos)
          
          if(nrow(datos) == 0) return(crear_tabla_detalle(NULL))
          
          tabla_resumen <- datos %>%
               group_by(responsable_de_garantia, problema_clasificado) %>%
               summarise(
                    Inasistencia = sum(causal_excepcion == "Inasistencia", na.rm = TRUE),
                    Postergacion = sum(grepl("Posterg", causal_excepcion, ignore.case = TRUE), na.rm = TRUE),
                    Total = n(),
                    .groups = "drop"
               ) %>%
               arrange(desc(Total))
          
          if(nrow(tabla_resumen) == 0) return(crear_tabla_detalle(NULL))
          
          names(tabla_resumen) <- c("Responsable de Garantía", "Problema de Salud", "Inasistencia", "Postergación de la Prestación", "Total")
          
          tabla_resumen <- tabla_resumen %>%
               select(`Responsable de Garantía`, `Problema de Salud`, Total, Inasistencia, `Postergación de la Prestación`)
          
          crear_tabla_detalle(tabla_resumen, col_fijas = c("Responsable de Garantía", "Problema de Salud"), col_destacar = "Total")
     })
     
     # ------------------------------------------------------------
     # 6. OUTPUTS - VIGENTES
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
          n <- datos %>% filter(clasificacion_avance == "Avanzadas (Avance > 66% del plazo total)") %>% nrow()
          div(class = "col-sm-3",
              div(class = "custom-box bg-red-custom",
                  div(class = "icon", icon("flag-checkered")),
                  div(class = "inner", 
                      h3(format(n, big.mark = ".", decimal.mark = ",")), 
                      p("Avanzadas (Avance > 66% del plazo total)", style = "font-weight: bold;"))))
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
               mutate(clasificacion_avance = factor(clasificacion_avance, levels = c("Avanzadas (Avance > 66% del plazo total)", "Intermedias (Avance entre 34 y 66% del plazo total)", "Tempranas (Avance ≤ 33% del plazo total)")))
          
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
                              "Avanzadas (Avance > 66% del plazo total)" = "#E53935")
          
          for(nivel in c("Avanzadas (Avance > 66% del plazo total)", "Intermedias (Avance entre 34 y 66% del plazo total)", "Tempranas (Avance ≤ 33% del plazo total)")) {
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
     # 7. OUTPUTS - RETRASADAS
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
          if(nrow(df_hist) == 0) return(plotly::plot_ly() %>% layout(title = "No hay datos históricos"))
          
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
               arrange(fecha_corte) %>%
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
     # 8. OUTPUTS - EXCEPTUADAS TRANSITORIAS
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
          paste("📅 Fecha de corte:", format(fecha_max_ret, "%d-%m-%Y"))
     })
}

shinyApp(ui = ui, server = server)
