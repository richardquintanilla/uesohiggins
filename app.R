library(shiny)
library(bslib)
library(thematic)
library(readr)
library(dplyr)
library(ggplot2)

thematic::thematic_on()

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#191970",
  secondary = "#F8F9FF"
)

ui <- page_fluid(
  theme = theme,
  div(style="display:flex; align-items:center; gap:15px;",
      img(src='logo.png', height='60px'),
      h2("UES O'Higgins – Reportes")
  ),
  layout_sidebar(
    sidebar = sidebar(
      selectInput(
        "reporte",
        "Seleccione un reporte:",
        choices = c(
          "Reporte A – Coberturas",
          "Reporte B – Influenza",
          "Reporte C – Agentes Etiológicos"
        )
      )
    ),
    uiOutput("contenido_reporte")
  )
)

server <- function(input, output, session){

  cargar_reporte_a <- reactive({ read_csv("data/coberturas.csv") })
  cargar_reporte_b <- reactive({ read_csv("data/influenza.csv") })
  cargar_reporte_c <- reactive({ read_csv("data/agentes.csv") })

  output$contenido_reporte <- renderUI({
    switch(input$reporte,
      "Reporte A – Coberturas" = tagList(
        h3("Reporte A – Coberturas"), tableOutput("tabla_a"), plotOutput("plot_a")
      ),
      "Reporte B – Influenza" = tagList(
        h3("Reporte B – Influenza"), tableOutput("tabla_b"), plotOutput("plot_b")
      ),
      tagList(
        h3("Reporte C – Agentes Etiológicos"), tableOutput("tabla_c"), plotOutput("plot_c")
      )
    )
  })

  output$tabla_a <- renderTable(cargar_reporte_a())
  output$plot_a <- renderPlot({
    ggplot(cargar_reporte_a(), aes(x=x, y=y, fill=categoria)) +
      geom_col() + theme_minimal()
  })

  output$tabla_b <- renderTable(cargar_reporte_b())
  output$plot_b <- renderPlot({
    ggplot(cargar_reporte_b(), aes(x=fecha, y=valor, color=grupo)) +
      geom_line() + theme_minimal()
  })

  output$tabla_c <- renderTable(cargar_reporte_c())
  output$plot_c <- renderPlot({
    ggplot(cargar_reporte_c(), aes(x=x, y=y, fill=region)) +
      geom_col() + theme_minimal()
  })
}

shinyApp(ui, server)
