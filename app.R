library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$img(src = "logo.png", height = "40px", style = "margin-right:10px;"),
      "Dashboard de Reportes"
    )
  ),
  
  dashboardSidebar(
    style = "background-color:#191970;",
    selectInput("reporte", "Seleccionar reporte:",
                choices = c("Reporte A", "Reporte B")),
    selectInput("anio", "Año:", choices = 2015:2024, selected = 2024),
    selectInput("sexo", "Sexo:", choices = c("Todos", "Hombre", "Mujer")),
    selectInput("categoria", "Categoría:",
                choices = c("Todos", "A", "B", "C")),
    width = 250
  ),
  
  dashboardBody(
    
    # ---- CSS ESTILO ----
    tags$style(HTML("
      /* Barra superior */
      .skin-blue .main-header .navbar {
        background-color: #191970 !important;
      }
      .skin-blue .main-header .logo {
        background-color: #191970 !important;
      }

      /* Sidebar texto y links */
      .skin-blue .sidebar a {
        color: white !important;
      }

      /* Pestañas */
      .nav-tabs-custom > .nav-tabs > li.active > a {
        background-color: #EEE9E9 !important;
        color: black !important;
        border: 1px solid white !important;
      }
      .nav-tabs-custom > .nav-tabs > li > a {
        border: 1px solid white !important;
        color: #191970 !important;
      }
    ")),
    
    # ---- CONTENIDO ----
    uiOutput("fechaUI"),
    
    tabsetPanel(id = "tabs",
      tabPanel("Tabla", DT::dataTableOutput("tabla")),
      tabPanel("Gráfico", plotOutput("grafico"))
    )
  )
)

server <- function(input, output, session) {
  
  # --- Fecha dinámica sin acumulación ---
  output$fechaUI <- renderUI({
    req(input$reporte)
    fecha_actual <- format(Sys.Date(), "%d-%m-%Y")
    HTML(paste0(
      "<h4 style='text-align:right; margin-right:20px; color:#191970;'>",
      "Fecha reporte ", input$reporte, ": ", fecha_actual,
      "</h4>"
    ))
  })
  
  # --- Tabla dummy ---
  output$tabla <- DT::renderDataTable({
    data.frame(
      categoria = c("A", "B", "C"),
      valor = sample(1:100, 3)
    )
  })
  
  # --- Gráfico dummy ---
  output$grafico <- renderPlot({
    plot(1:10, main = paste("Gráfico:", input$reporte))
  })
}

shinyApp(ui, server)
