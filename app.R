# app.R
library(shiny)
library(bslib)
library(thematic)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
library(writexl)
library(rmarkdown)

# ---------------- THEME ----------------
tema_moderno <- bs_theme(
  version = 5,
  primary = "#191970",    # midnightblue
  secondary = "#CD0000",  # red3
  bg = "#ffffff",
  fg = "#1a1a1a",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

thematic_shiny()

# ---------------- UI ----------------
ui <- navbarPage(
  title = div(img(src = "logo.png", height = "36px", style = "margin-right:8px;"), "UES O'Higgins"),
  theme = tema_moderno,
  collapsible = TRUE,
  
  tabPanel("Inicio",
           fluidPage(
             h2("Bienvenido al Dashboard institucional"),
             p("Seleccione un reporte en el menú superior o use las pestañas.")
           )),
  
  tabPanel("Coberturas",
           sidebarLayout(
             sidebarPanel(
               h4("Filtros Coberturas"),
               uiOutput("filtros_a"),
               downloadButton("download_a_excel", "Descargar Excel")
             ),
             mainPanel(
               fluidRow(
                 column(4, wellPanel(h5("Total registros"), textOutput("kpi_a_total"))),
                 column(4, wellPanel(h5("Promedio Y"), textOutput("kpi_a_prom"))),
                 column(4, wellPanel(h5("N categorías"), textOutput("kpi_a_cat")))
               ),
               br(),
               DTOutput("table_a"),
               br(),
               plotlyOutput("plot_a"),
               br(),
               downloadButton("download_a_png", "Descargar gráfico (PNG)"),
               downloadButton("report_a", "Generar reporte (HTML/PDF)")
             )
           )),
  
  tabPanel("Influenza",
           sidebarLayout(
             sidebarPanel(
               h4("Filtros Influenza"),
               uiOutput("filtros_b"),
               downloadButton("download_b_excel", "Descargar Excel")
             ),
             mainPanel(
               fluidRow(
                 column(4, wellPanel(h5("Casos totales"), textOutput("kpi_b_total"))),
                 column(4, wellPanel(h5("Último valor"), textOutput("kpi_b_last"))),
                 column(4, wellPanel(h5("N grupos"), textOutput("kpi_b_groups")))
               ),
               br(),
               DTOutput("table_b"),
               br(),
               plotlyOutput("plot_b"),
               br(),
               downloadButton("download_b_png", "Descargar gráfico (PNG)"),
               downloadButton("report_b", "Generar reporte (HTML/PDF)")
             )
           )),
  
  tabPanel("Agentes",
           sidebarLayout(
             sidebarPanel(
               h4("Filtros Agentes"),
               uiOutput("filtros_c"),
               downloadButton("download_c_excel", "Descargar Excel")
             ),
             mainPanel(
               fluidRow(
                 column(4, wellPanel(h5("Total agentes"), textOutput("kpi_c_total"))),
                 column(4, wellPanel(h5("Regiones"), textOutput("kpi_c_regs"))),
                 column(4, wellPanel(h5("Máx Y"), textOutput("kpi_c_max")))
               ),
               br(),
               DTOutput("table_c"),
               br(),
               plotlyOutput("plot_c"),
               br(),
               downloadButton("download_c_png", "Descargar gráfico (PNG)"),
               downloadButton("report_c", "Generar reporte (HTML/PDF)")
             )
           )),
  
  tabPanel("Descargas",
           fluidPage(
             h3("Descargas masivas"),
             p("Descargar datasets completos o generar reportes globales."),
             downloadButton("download_all_excel", "Descargar todo en Excel (zip)"),
             br(), br(),
             downloadButton("report_all", "Generar reporte global (HTML/PDF)")
           )),
  
  tabPanel("Acerca",
           fluidPage(
             h3("Acerca de"),
             p("Dashboard construido para UES O'Higgins. Coloca tu logo en www/logo.png")
           ))
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  # -- cargar datos
  datos_a <- reactive({ read_csv("data/coberturas.csv", show_col_types = FALSE) })
  datos_b <- reactive({ read_csv("data/influenza.csv", show_col_types = FALSE) })
  datos_c <- reactive({ read_csv("data/agentes.csv", show_col_types = FALSE) })
  
  # ----- FILTROS UI dinámicos -----
  output$filtros_a <- renderUI({
    df <- datos_a()
    tagList(
      selectInput("f_cat", "Categoría", choices = c("Todos", sort(unique(df$categoria))), selected = "Todos")
    )
  })
  output$filtros_b <- renderUI({
    df <- datos_b()
    tagList(
      dateRangeInput("f_fecha", "Rango fecha", start = min(as.Date(df$fecha)), end = max(as.Date(df$fecha))),
      selectInput("f_grp", "Grupo", choices = c("Todos", sort(unique(df$grupo))), selected = "Todos")
    )
  })
  output$filtros_c <- renderUI({
    df <- datos_c()
    tagList(
      selectInput("f_reg", "Región", choices = c("Todos", sort(unique(df$region))), selected = "Todos")
    )
  })
  
  # ----- DATOS FILTRADOS -----
  a_filtrado <- reactive({
    df <- datos_a()
    if (!is.null(input$f_cat) && input$f_cat != "Todos") df <- df %>% filter(categoria == input$f_cat)
    df
  })
  b_filtrado <- reactive({
    df <- datos_b()
    if (!is.null(input$f_grp) && input$f_grp != "Todos") df <- df %>% filter(grupo == input$f_grp)
    if (!is.null(input$f_fecha)) {
      df <- df %>% mutate(fecha = as.Date(fecha)) %>% filter(fecha >= as.Date(input$f_fecha[1]) & fecha <= as.Date(input$f_fecha[2]))
    }
    df
  })
  c_filtrado <- reactive({
    df <- datos_c()
    if (!is.null(input$f_reg) && input$f_reg != "Todos") df <- df %>% filter(region == input$f_reg)
    df
  })
  
  # ----- KPIs -----
  output$kpi_a_total <- renderText({ nrow(a_filtrado()) })
  output$kpi_a_prom  <- renderText({ round(mean(a_filtrado()$y, na.rm = TRUE), 1) })
  output$kpi_a_cat   <- renderText({ length(unique(a_filtrado()$categoria)) })
  
  output$kpi_b_total <- renderText({ sum(b_filtrado()$valor, na.rm = TRUE) })
  output$kpi_b_last  <- renderText({ tail(b_filtrado()$valor, 1) })
  output$kpi_b_groups<- renderText({ length(unique(b_filtrado()$grupo)) })
  
  output$kpi_c_total <- renderText({ nrow(c_filtrado()) })
  output$kpi_c_regs  <- renderText({ length(unique(c_filtrado()$region)) })
  output$kpi_c_max   <- renderText({ max(c_filtrado()$y, na.rm = TRUE) })
  
  # ----- TABLAS DT -----
  output$table_a <- renderDT({
    dat <- a_filtrado()
    datatable(dat, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy','csv','excel','pdf','print')))
  })
  output$table_b <- renderDT({
    dat <- b_filtrado()
    datatable(dat, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy','csv','excel','pdf','print')))
  })
  output$table_c <- renderDT({
    dat <- c_filtrado()
    datatable(dat, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy','csv','excel','pdf','print')))
  })
  
  # ----- GRÁFICOS (ggplot + plotly) -----
  plot_a_gg <- reactive({
    ggplot(a_filtrado(), aes(x = x, y = y, fill = categoria)) +
      geom_col() +
      labs(x = NULL, y = "Y") +
      theme_minimal(base_size = 14)
  })
  output$plot_a <- renderPlotly({ ggplotly(plot_a_gg()) %>% layout(legend = list(orientation = "h")) })
  
  plot_b_gg <- reactive({
    ggplot(b_filtrado(), aes(x = as.Date(fecha), y = valor, color = grupo)) +
      geom_line(size = 1.2) +
      labs(x = "Fecha", y = "Casos") +
      theme_minimal(base_size = 14)
  })
  output$plot_b <- renderPlotly({ ggplotly(plot_b_gg()) %>% layout(legend = list(orientation = "h")) })
  
  plot_c_gg <- reactive({
    ggplot(c_filtrado(), aes(x = x, y = y, fill = region)) +
      geom_col() +
      labs(x = NULL, y = "Y") +
      theme_minimal(base_size = 14)
  })
  output$plot_c <- renderPlotly({ ggplotly(plot_c_gg()) %>% layout(legend = list(orientation = "h")) })
  
  # ----- DESCARGAS EXCEL -----
  output$download_a_excel <- downloadHandler(
    filename = function() paste0("coberturas_", Sys.Date(), ".xlsx"),
    content = function(file) writexl::write_xlsx(a_filtrado(), path = file)
  )
  output$download_b_excel <- downloadHandler(
    filename = function() paste0("influenza_", Sys.Date(), ".xlsx"),
    content = function(file) writexl::write_xlsx(b_filtrado(), path = file)
  )
  output$download_c_excel <- downloadHandler(
    filename = function() paste0("agentes_", Sys.Date(), ".xlsx"),
    content = function(file) writexl::write_xlsx(c_filtrado(), path = file)
  )
  
  # ----- DESCARGAR GRÁFICO PNG -----
  output$download_a_png <- downloadHandler(
    filename = function() paste0("plot_coberturas_", Sys.Date(), ".png"),
    content = function(file){
      ggsave(file, plot = plot_a_gg(), device = "png", width = 10, height = 6)
    }
  )
  output$download_b_png <- downloadHandler(
    filename = function() paste0("plot_influenza_", Sys.Date(), ".png"),
    content = function(file){
      ggsave(file, plot = plot_b_gg(), device = "png", width = 10, height = 6)
    }
  )
  output$download_c_png <- downloadHandler(
    filename = function() paste0("plot_agentes_", Sys.Date(), ".png"),
    content = function(file){
      ggsave(file, plot = plot_c_gg(), device = "png", width = 10, height = 6)
    }
  )
  
  # ----- REPORTES AUTOMÁTICOS (RMarkdown) -----
  # Se asume que existe "reports/report_template.Rmd" en tu repo (te doy la plantilla abajo).
  # Por defecto generará HTML; para PDF se requiere una instalación de LaTeX en el contenedor.
  
  generar_reporte <- function(data, tipo, out_file){
    params <- list(data = data, tipo = tipo)
    rmarkdown::render(input = "reports/report_template.Rmd",
                      output_file = out_file,
                      params = params,
                      envir = new.env(parent = globalenv()),
                      quiet = TRUE)
  }
  
  output$report_a <- downloadHandler(
    filename = function() paste0("report_coberturas_", Sys.Date(), ".html"),
    content = function(file){
      tmp <- tempfile(fileext = ".html")
      generar_reporte(a_filtrado(), "Coberturas", tmp)
      file.copy(tmp, file)
    }
  )
  output$report_b <- downloadHandler(
    filename = function() paste0("report_influenza_", Sys.Date(), ".html"),
    content = function(file){
      tmp <- tempfile(fileext = ".html")
      generar_reporte(b_filtrado(), "Influenza", tmp)
      file.copy(tmp, file)
    }
  )
  output$report_c <- downloadHandler(
    filename = function() paste0("report_agentes_", Sys.Date(), ".html"),
    content = function(file){
      tmp <- tempfile(fileext = ".html")
      generar_reporte(c_filtrado(), "Agentes", tmp)
      file.copy(tmp, file)
    }
  )
  
  output$download_all_excel <- downloadHandler(
    filename = function() paste0("datasets_all_", Sys.Date(), ".zip"),
    content = function(file){
      # crea xlsx temporales y zipéalos
      tmpdir <- tempdir()
      f1 <- file.path(tmpdir, "coberturas.xlsx")
      f2 <- file.path(tmpdir, "influenza.xlsx")
      f3 <- file.path(tmpdir, "agentes.xlsx")
      writexl::write_xlsx(datos_a(), path = f1)
      writexl::write_xlsx(datos_b(), path = f2)
      writexl::write_xlsx(datos_c(), path = f3)
      zip::zip(zipfile = file, files = c(f1,f2,f3), mode = "cherry-pick")
    }
  )
  
  output$report_all <- downloadHandler(
    filename = function() paste0("report_all_", Sys.Date(), ".html"),
    content = function(file){
      tmp <- tempfile(fileext = ".html")
      generar_reporte(datos_a(), "Coberturas (global)", tmp) # ejemplo: generar con coberturas
      file.copy(tmp, file)
    }
  )
}

shinyApp(ui, server)
