options(shiny.port = 8080, shiny.host = "0.0.0.0")

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

  # ---- CARGAS REACTIVAS ----
  cargar_reporte_a <- reactive({
    read_csv("data/coberturas.csv")
  })

  cargar_reporte_b <- reactive({
    read_csv("data/influenza.csv")
  })

  cargar_reporte_c <- reactive({
    read_csv("data/agentes.csv")
  })


  # ---- UI dinámico según reporte ----
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

  # ---- TABLAS Y GRÁFICOS ----

  # REPORTE A – COBERTURAS (x, y, categoria)
  output$tabla_a <- renderTable({
    cargar_reporte_a()
  })
  output$plot_a <- renderPlot({
    ggplot(cargar_reporte_a(), aes(x = x, y = y, fill = categoria)) +
      geom_col() +
      theme_minimal()
  })

  # REPORTE B – INFLUENZA (fecha, valor, grupo)
  output$tabla_b <- renderTable({
    cargar_reporte_b()
  })
  output$plot_b <- renderPlot({
    ggplot(cargar_reporte_b(), aes(x = fecha, y = valor, color = grupo)) +
      geom_line() +
      theme_minimal()
  })

  # REPORTE C – AGENTES (x, y, region)
  output$tabla_c <- renderTable({
    cargar_reporte_c()
  })
  output$plot_c <- renderPlot({
    ggplot(cargar_reporte_c(), aes(x = x, y = y, fill = region)) +
      geom_col() +
      theme_minimal()
  })
}

shinyApp(ui, server)
