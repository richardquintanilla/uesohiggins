# app.R - Versión FINAL (tooltip mejorado + variación)

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

# Diccionario de clasificación
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
     
     if(grepl("Cáncer", texto, ignore.case = TRUE)) {
          return("Cáncer (otros)")
     }
     
     return(texto)
}

es_oncologico <- function(texto) {
     if(is.na(texto)) return(FALSE)
     for(pattern in oncologicos_problemas) {
          if(grepl(pattern, texto, ignore.case = TRUE)) {
               return(TRUE)
          }
     }
     return(FALSE)
}

# UI
ui <- dashboardPage(
     dashboardHeader(title = "Monitoreo GES Retrasadas", titleWidth = 300),
     
     dashboardSidebar(
          width = 300,
          tags$style(HTML("
      .main-sidebar { background-color: #191970 !important; }
      .sidebar-menu > li > a { color: #ecf0f1 !important; background-color: #191970 !important; }
      .sidebar-menu > li > a:hover { background-color: #2c2c8a !important; }
      .skin-blue .main-header .navbar { background-color: #191970 !important; }
      .skin-blue .main-header .logo { background-color: #0f0f4f !important; color: #ecf0f1 !important; }
      .content-wrapper, .right-side { background-color: #f4f4f4; }
      
      .custom-box {
        border-radius: 10px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        transition: all 0.3s cubic-bezier(.25,.8,.25,1);
        margin-bottom: 20px;
        position: relative;
        color: white;
      }
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
    ")),
          
          div(style = "text-align: center; padding: 20px 0;",
              tags$img(src = "logo.png", height = "80px", 
                       style = "border-radius: 10px; background-color: white; padding: 10px;")),
          
          sidebarMenu(
               menuItem("Resumen General", tabName = "resumen", icon = icon("chart-line"))
          ),
          
          br(), hr(),
          h4("Filtros", style = "padding-left: 15px; color: #ecf0f1;"),
          
          selectInput("responsable_filter", "Responsable de Garantía:",
                      choices = c("Todos"), selected = "Todos", multiple = TRUE),
          
          selectInput("problema_filter", "Problema de Salud:",
                      choices = c("Todos"), selected = "Todos", multiple = TRUE),
          
          div(style = "padding-left: 15px; padding-right: 15px; margin-bottom: 10px;",
              checkboxInput("oncologicos_check", "🔬 Solo casos oncológicos", value = FALSE)),
          
          div(style = "padding-left: 15px; padding-right: 15px;",
              actionButton("clear_filters", "Limpiar Filtros", icon = icon("eraser"),
                           style = "width: 100%; background-color: #95a5a6; color: white;")),
          
          br(), hr(),
          h5("Fecha de última actualización:", style = "padding-left: 15px; color: #ecf0f1;"),
          div(style = "padding-left: 15px; color: #ecf0f1; font-weight: bold;",
              textOutput("fecha_actualizacion"))
     ),
     
     dashboardBody(
          tabItems(
               tabItem(tabName = "resumen",
                       fluidRow(
                            uiOutput("tarjeta_total"),
                            uiOutput("tarjeta_nueva_vencida"),
                            uiOutput("tarjeta_vencida"),
                            uiOutput("tarjeta_vencida_prolongada")
                       ),
                       fluidRow(
                            box(title = "GES Retrasadas acumuladas por semana y tipo",
                                status = "primary", solidHeader = TRUE, width = 12,
                                plotlyOutput("grafico_evolucion", height = "500px"))
                       ),
                       fluidRow(
                            box(title = "Top 10 Responsables con más Retrasos",
                                status = "info", width = 6,
                                plotlyOutput("top_responsables", height = "450px")),
                            box(title = "Top 10 Problemas de Salud con más Retrasos",
                                status = "info", width = 6,
                                plotlyOutput("top_problemas", height = "450px"))
                       )
               )
          )
     )
)

# Server
server <- function(input, output, session) {
     
     # Leer CSV
     ruta_csv <- "data/ges_retrasadas.csv"
     
     if(!file.exists(ruta_csv)) {
          stop(paste("No se encuentra el archivo:", ruta_csv))
     }
     
     df <- read.csv(ruta_csv, stringsAsFactors = FALSE) %>%
          mutate(
               fecha_corte = as.Date(fecha_corte),
               fecha_limite = as.Date(fecha_limite),
               responsable_de_garantia = if_else(is.na(responsable_de_garantia), "No especificado", responsable_de_garantia),
               problema_de_salud = if_else(is.na(problema_de_salud), "No especificado", problema_de_salud),
               tipo_retraso = case_when(
                    dias_atraso <= 7 ~ "Nueva vencida",
                    dias_atraso <= 365 ~ "Vencida",
                    TRUE ~ "Vencida prolongada"
               ),
               problema_clasificado = sapply(problema_de_salud, clasificar_problema),
               es_oncologico = sapply(problema_de_salud, es_oncologico)
          )
     
     fecha_max <- max(df$fecha_corte)
     datos_reciente <- df %>% filter(fecha_corte == fecha_max)
     datos_historico <- df
     
     # Actualizar filtros
     observe({
          responsables <- c("Todos", sort(unique(datos_reciente$responsable_de_garantia)))
          problemas <- c("Todos", sort(unique(datos_reciente$problema_clasificado)))
          
          updateSelectInput(session, "responsable_filter", choices = responsables, selected = "Todos")
          updateSelectInput(session, "problema_filter", choices = problemas, selected = "Todos")
     })
     
     # Datos filtrados
     datos_filtrados <- reactive({
          df_filt <- datos_reciente
          
          if(input$oncologicos_check) df_filt <- df_filt %>% filter(es_oncologico == TRUE)
          if(!("Todos" %in% input$responsable_filter) && length(input$responsable_filter) > 0) {
               df_filt <- df_filt %>% filter(responsable_de_garantia %in% input$responsable_filter)
          }
          if(!("Todos" %in% input$problema_filter) && length(input$problema_filter) > 0) {
               df_filt <- df_filt %>% filter(problema_clasificado %in% input$problema_filter)
          }
          df_filt
     })
     
     datos_historicos_filtrados <- reactive({
          df_filt <- datos_historico
          
          if(input$oncologicos_check) df_filt <- df_filt %>% filter(es_oncologico == TRUE)
          if(!("Todos" %in% input$responsable_filter) && length(input$responsable_filter) > 0) {
               df_filt <- df_filt %>% filter(responsable_de_garantia %in% input$responsable_filter)
          }
          if(!("Todos" %in% input$problema_filter) && length(input$problema_filter) > 0) {
               df_filt <- df_filt %>% filter(problema_clasificado %in% input$problema_filter)
          }
          df_filt
     })
     
     # Limpiar filtros
     observeEvent(input$clear_filters, {
          updateSelectInput(session, "responsable_filter", selected = "Todos")
          updateSelectInput(session, "problema_filter", selected = "Todos")
          updateCheckboxInput(session, "oncologicos_check", value = FALSE)
     })
     
     # Tarjetas
     output$tarjeta_total <- renderUI({
          datos <- datos_filtrados()
          req(datos)
          div(class = "col-sm-3",
              div(class = "custom-box bg-purple-custom",
                  div(class = "icon", icon("exclamation-triangle")),
                  div(class = "inner", 
                      h3(format(nrow(datos), big.mark = ".", decimal.mark = ",")), 
                      p("Total GES Retrasadas", style = "font-weight: bold;"))))
     })
     
     output$tarjeta_nueva_vencida <- renderUI({
          datos <- datos_filtrados()
          req(datos)
          n <- datos %>% filter(tipo_retraso == "Nueva vencida") %>% nrow()
          div(class = "col-sm-3",
              div(class = "custom-box bg-green-custom",
                  div(class = "icon", icon("clock")),
                  div(class = "inner", 
                      h3(format(n, big.mark = ".", decimal.mark = ",")), 
                      p("Nuevas Vencidas (Retraso ≤ 7 días)", style = "font-weight: bold;"))))
     })
     
     output$tarjeta_vencida <- renderUI({
          datos <- datos_filtrados()
          req(datos)
          n <- datos %>% filter(tipo_retraso == "Vencida") %>% nrow()
          div(class = "col-sm-3",
              div(class = "custom-box bg-orange-custom",
                  div(class = "icon", icon("hourglass-half")),
                  div(class = "inner", 
                      h3(format(n, big.mark = ".", decimal.mark = ",")), 
                      p("Vencidas (Retraso entre 8 y 365 días)", style = "font-weight: bold;"))))
     })
     
     output$tarjeta_vencida_prolongada <- renderUI({
          datos <- datos_filtrados()
          req(datos)
          n <- datos %>% filter(tipo_retraso == "Vencida prolongada") %>% nrow()
          div(class = "col-sm-3",
              div(class = "custom-box bg-red-custom",
                  div(class = "icon", icon("calendar-times")),
                  div(class = "inner", 
                      h3(format(n, big.mark = ".", decimal.mark = ",")), 
                      p("Vencidas Prolongadas (Retraso > 365 días)", style = "font-weight: bold;"))))
     })
     
     # Gráfico de evolución con tooltip mejorado Y VARIACIÓN
     output$grafico_evolucion <- renderPlotly({
          df_hist <- datos_historicos_filtrados()
          req(df_hist)
          
          if(nrow(df_hist) == 0) {
               return(plotly::plot_ly() %>% layout(title = "No hay datos para mostrar"))
          }
          
          # Calcular agregados por fecha
          resultado <- df_hist %>%
               group_by(fecha_corte, tipo_retraso) %>%
               summarise(n = n(), .groups = "drop") %>%
               arrange(fecha_corte) %>%
               mutate(tipo_retraso = factor(tipo_retraso, levels = c("Vencida prolongada", "Vencida", "Nueva vencida")))
          
          # Calcular variación por tipo
          resultado <- resultado %>%
               group_by(tipo_retraso) %>%
               mutate(
                    n_prev = lag(n),
                    variacion = n - n_prev,
                    variacion_pct = (n / n_prev - 1) * 100
               ) %>%
               ungroup()
          
          # Calcular totales y su variación
          totales_por_fecha <- resultado %>%
               group_by(fecha_corte) %>%
               summarise(total = sum(n), .groups = "drop") %>%
               arrange(fecha_corte) %>%
               mutate(
                    total_prev = lag(total),
                    variacion_total = total - total_prev,
                    variacion_total_pct = (total / total_prev - 1) * 100
               )
          
          p <- plot_ly()
          
          # Colores
          colores <- c("Nueva vencida" = "#4CAF50", "Vencida" = "#FFC107", "Vencida prolongada" = "#E53935")
          
          # Nombres para la leyenda
          nombres_leyenda <- c(
               "Nueva vencida" = "Nuevas Vencidas (≤7 días)",
               "Vencida" = "Vencidas (8-365 días)",
               "Vencida prolongada" = "Vencidas Prolongadas (>365 días)"
          )
          
          # Tooltips con variación
          tooltip_text <- c(
               "Nueva vencida" = "Nuevas Vencidas<br>(Retraso ≤ 7 días)",
               "Vencida" = "Vencidas<br>(Retraso entre 8 y 365 días)",
               "Vencida prolongada" = "Vencidas Prolongadas<br>(Retraso > 365 días)"
          )
          
          for(tipo in c("Vencida prolongada", "Vencida", "Nueva vencida")) {
               datos_tipo <- resultado %>% filter(tipo_retraso == tipo)
               if(nrow(datos_tipo) > 0) {
                    
                    # Crear tooltip con variación
                    hover_text <- paste0(
                         "<b>", tooltip_text[tipo], "</b><br>",
                         "Cantidad: ", format(datos_tipo$n, big.mark = ".", decimal.mark = ","), "<br>",
                         "Fecha: ", format(datos_tipo$fecha_corte, "%d/%m/%y"), "<br>",
                         ifelse(!is.na(datos_tipo$variacion),
                                paste0("Variación: ", format(datos_tipo$variacion, big.mark = ".", decimal.mark = ","),
                                       " (", format(round(datos_tipo$variacion_pct, 1), decimal.mark = ","), "%)"),
                                "Variación: -")
                    )
                    
                    p <- p %>% add_trace(
                         data = datos_tipo, 
                         x = ~fecha_corte, 
                         y = ~n, 
                         name = nombres_leyenda[tipo],
                         type = "bar",
                         marker = list(color = colores[tipo]),
                         hovertemplate = paste(hover_text, "<extra></extra>")
                    )
               }
          }
          
          # Totales con variación
          if(nrow(totales_por_fecha) > 0) {
               total_hover <- paste0(
                    "<b>Total GES Retrasadas</b><br>",
                    "Total: ", format(totales_por_fecha$total, big.mark = ".", decimal.mark = ","), "<br>",
                    "Fecha: ", format(totales_por_fecha$fecha_corte, "%d/%m/%y"), "<br>",
                    ifelse(!is.na(totales_por_fecha$variacion_total),
                           paste0("Variación: ", format(totales_por_fecha$variacion_total, big.mark = ".", decimal.mark = ","),
                                  " (", format(round(totales_por_fecha$variacion_total_pct, 1), decimal.mark = ","), "%)"),
                           "Variación: -")
               )
               
               p <- p %>% add_trace(
                    data = totales_por_fecha, 
                    x = ~fecha_corte, 
                    y = ~total,
                    type = "scatter", 
                    mode = "text",
                    text = ~format(total, big.mark = ".", decimal.mark = ","),
                    textposition = "top center",
                    textfont = list(size = 11, color = "#191970", family = "Arial Black"),
                    hoverinfo = "text",
                    hovertext = total_hover,
                    showlegend = FALSE, 
                    inherit = FALSE
               )
          }
          
          fechas_unicas <- sort(unique(resultado$fecha_corte))
          
          p %>% layout(
               barmode = "stack", 
               title = NULL,
               xaxis = list(
                    title = "Fecha de corte", 
                    tickangle = -45, 
                    tickmode = "array",
                    tickvals = fechas_unicas,
                    ticktext = format(fechas_unicas, "%d/%m/%y"),
                    gridcolor = "#e6e6e6", 
                    titlefont = list(color = "#191970")
               ),
               yaxis = list(
                    title = "N° GES Retrasadas", 
                    gridcolor = "#e6e6e6", 
                    titlefont = list(color = "#191970")
               ),
               hovermode = "closest", 
               plot_bgcolor = "#f8f9fa", 
               paper_bgcolor = "#f8f9fa",
               legend = list(
                    orientation = "h", 
                    yanchor = "bottom", 
                    y = 1.02, 
                    xanchor = "center", 
                    x = 0.5,
                    font = list(size = 11)
               )
          )
     })
     
     # Top 10 responsables
     output$top_responsables <- renderPlotly({
          datos <- datos_filtrados()
          req(datos)
          
          if(nrow(datos) == 0) {
               return(plotly::plot_ly() %>% layout(title = "No hay datos para mostrar"))
          }
          
          top <- datos %>%
               count(responsable_de_garantia) %>%
               arrange(desc(n)) %>%
               head(10)
          
          max_n <- max(top$n)
          
          plot_ly(top, x = ~n, y = ~reorder(responsable_de_garantia, n), 
                  type = "bar", orientation = "h",
                  text = ~format(n, big.mark = ".", decimal.mark = ","), 
                  textposition = "outside",
                  marker = list(color = "#191970"),
                  hovertemplate = paste("Responsable: %{y}<br>Total: %{x}<extra></extra>")) %>%
               layout(xaxis = list(title = "Cantidad de Retrasos", range = c(0, max_n * 1.15)),
                      yaxis = list(title = ""),
                      margin = list(l = 150, r = 80))
     })
     
     # Top 10 problemas
     output$top_problemas <- renderPlotly({
          datos <- datos_filtrados()
          req(datos)
          
          if(nrow(datos) == 0) {
               return(plotly::plot_ly() %>% layout(title = "No hay datos para mostrar"))
          }
          
          top <- datos %>%
               count(problema_clasificado) %>%
               arrange(desc(n)) %>%
               head(10)
          
          max_n <- max(top$n)
          
          plot_ly(top, x = ~n, y = ~reorder(problema_clasificado, n), 
                  type = "bar", orientation = "h",
                  text = ~format(n, big.mark = ".", decimal.mark = ","), 
                  textposition = "outside",
                  marker = list(color = "#191970"),
                  hovertemplate = paste("Problema: %{y}<br>Total: %{x}<extra></extra>")) %>%
               layout(xaxis = list(title = "Cantidad de Retrasos", range = c(0, max_n * 1.15)),
                      yaxis = list(title = ""),
                      margin = list(l = 200, r = 80))
     })
     
     output$fecha_actualizacion <- renderText({
          format(fecha_max, "%d-%m-%Y")
     })
}

shinyApp(ui = ui, server = server)
