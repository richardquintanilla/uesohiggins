# app.R restaurado con formato previo + corrección de fecha

library(shiny)
library(shinyWidgets)
library(dplyr)
library(lubridate)
library(ggplot2)

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("body { font-family: 'Segoe UI'; }"))),

  # Título centrado -------------------------------------------------------
  fluidRow(
    column(12, align = "center",
           h2("Dashboard de Reportes"),
           htmlOutput("fecha_reporte")
    )
  ),
  hr(),

  # Selector de reporte ---------------------------------------------------
  fluidRow(
    column(4,
           selectInput("reporte", "Seleccione un reporte:",
                       choices = c("Reporte A", "Reporte B"),
                       selected = "Reporte A")
    )
  ),
  br(),

  # Filtros comunes -------------------------------------------------------
  fluidRow(
    column(3,
           pickerInput(
             inputId = "filtro_ano",
             label = "Año",
             choices = 2018:2025,
             selected = 2018:2025,
             multiple = TRUE,
             options = list(`actions-box` = TRUE)
           )
    ),
    column(3,
           pickerInput(
             inputId = "filtro_sexo",
             label = "Sexo",
             choices = c("Hombre", "Mujer"),
             selected = c("Hombre", "Mujer"),
             multiple = TRUE,
             options = list(`actions-box` = TRUE)
           )
    ),
    column(3,
           pickerInput(
             inputId = "filtro_edad",
             label = "Grupo de Edad",
             choices = c("0-4", "5-14", "15-64", "65+"),
             selected = c("0-4", "5-14", "15-64", "65+"),
             multiple = TRUE,
             options = list(`actions-box` = TRUE)
           )
    )
  ),
  br(),
  hr(),

  # Contenido del reporte seleccionado ------------------------------------
  uiOutput("contenido_reporte")
)


# SERVER ------------------------------------------------------------------
server <- function(input, output, session) {

  # Fecha del reporte ------------------------------------------------------
  output$fecha_reporte <- renderUI({
    fecha <- format(Sys.Date(), "%d/%m/%Y")
    HTML(paste0("<h4>Fecha del reporte: ", fecha, "</h4>"))
  })

  # Contenido dinámico ----------------------------------------------------
  output$contenido_reporte <- renderUI({
    if (input$reporte == "Reporte A") {
      tabsetPanel(
        tabPanel("Tabla", h4("Tabla del Reporte A"), tableOutput("tablaA")),
        tabPanel("Gráfico", h4("Gráfico del Reporte A"), plotOutput("graficoA")),
        tabPanel("Mapa", h4("Mapa del Reporte A"), "(Mapa de ejemplo)")
      )
    } else {
      tabsetPanel(
        tabPanel("Tabla", h4("Tabla del Reporte B"), tableOutput("tablaB")),
        tabPanel("Gráfico", h4("Gráfico del Reporte B"), plotOutput("graficoB")),
        tabPanel("Mapa", h4("Mapa del Reporte B"), "(Mapa de ejemplo)")
      )
    }
  })

  # Datos de ejemplo ------------------------------------------------------
  datos <- reactive({
    tibble(
      ano = sample(2018:2025, 300, replace = TRUE),
      sexo = sample(c("Hombre", "Mujer"), 300, replace = TRUE),
      edad = sample(c("0-4", "5-14", "15-64", "65+"), 300, replace = TRUE),
      valor = runif(300, 0, 100)
    ) %>%
      dplyr::filter(
        ano %in% input$filtro_ano,
        sexo %in% input$filtro_sexo,
        edad %in% input$filtro_edad
      )
  })

  # TABLAS ----------------------------------------------------------------
  output$tablaA <- renderTable({ datos() })
  output$tablaB <- renderTable({ datos() })

  # GRÁFICOS ---------------------------------------------------------------
  output$graficoA <- renderPlot({ ggplot(datos(), aes(x = ano, y = valor)) + geom_line() })
  output$graficoB <- renderPlot({ ggplot(datos(), aes(x = ano, y = valor)) + geom_point() })

}

# Run App -----------------------------------------------------------------
shinyApp(ui, server)
