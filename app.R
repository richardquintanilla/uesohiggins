library(shiny)
library(dplyr)
library(readr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("UES O'Higgins – Reportes"),

  sidebarLayout(
    sidebarPanel(
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

    mainPanel(
      uiOutput("contenido_reporte")
    )
  )
)

server <- function(input, output, session) {

  # ---- CARGAS QUE SE HACEN SOLO CUANDO SE NECESITAN ----
  cargar_reporte_a <- reactive({
    read_csv("data/coberturas.csv")
  })

  cargar_reporte_b <- reactive({
    read_csv("data/influenza.csv")
  })

  cargar_reporte_c <- reactive({
    read_csv("data/agentes.csv")
  })


  # ---- CONTENIDO DE CADA REPORTE ----
  output$contenido_reporte <- renderUI({

    if (input$reporte == "Reporte A – Coberturas") {

      tagList(
        h3("Reporte A – Coberturas"),
        tableOutput("tabla_a"),
        plotOutput("plot_a")
      )

    } else if (input$reporte == "Reporte B – Influenza") {

      tagList(
        h3("Reporte B – Influenza"),
        tableOutput("tabla_b"),
        plotOutput("plot_b")
      )

    } else {

      tagList(
        h3("Reporte C – Agentes Etiológicos"),
        tableOutput("tabla_c"),
        plotOutput("plot_c")
      )
    }
  })

  # ---- TABLAS Y GRÁFICOS INDIVIDUALES ----
  output$tabla_a <- renderTable({
    cargar_reporte_a()
  })
  output$plot_a <- renderPlot({
    ggplot(cargar_reporte_a(), aes(comuna, cobertura)) +
      geom_col() + theme_minimal()
  })

  output$tabla_b <- renderTable({
    cargar_reporte_b()
  })
  output$plot_b <- renderPlot({
    ggplot(cargar_reporte_b(), aes(semana, casos)) +
      geom_line() + theme_minimal()
  })

  output$tabla_c <- renderTable({
    cargar_reporte_c()
  })
  output$plot_c <- renderPlot({
    ggplot(cargar_reporte_c(), aes(agente, n)) +
      geom_col() + theme_minimal()
  })
}

shinyApp(ui, server)
