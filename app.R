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

  # ---- CARGA DE DATOS ----
  cargar_reporte_a <- reactive({
    read_csv("data/coberturas.csv") %>%
      rename(categoria = categoria, x = x, y = y)
  })

  cargar_reporte_b <- reactive({
    read_csv("data/influenza.csv") %>%
      rename(fecha = fecha, valor = valor, grupo = grupo)
  })

  cargar_reporte_c <- reactive({
    read_csv("data/agentes.csv") %>%
      rename(x = x, y = y, region = region)
  })

  # ---- CONTENIDO UI DINÁMICO ----
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

  # --- REPORTE A ---
  output$tabla_a <- renderTable({
    cargar_reporte_a()
  })

  output$plot_a <- renderPlot({
    datos <- cargar_reporte_a()

    ggplot(datos, aes(x = categoria, y = y)) +
      geom_col(fill = "steelblue") +
      labs(x = "Categoría", y = "Valor") +
      theme_minimal()
  })

  # --- REPORTE B ---
  output$tabla_b <- renderTable({
    cargar_reporte_b()
  })

  output$plot_b <- renderPlot({
    datos <- cargar_reporte_b()

    ggplot(datos, aes(x = fecha, y = valor, group = grupo, color = grupo)) +
      geom_line() +
      labs(x = "Fecha", y = "Valor", color = "Grupo") +
      theme_minimal()
  })

  # --- REPORTE C ---
  output$tabla_c <- renderTable({
    cargar_reporte_c()
  })

  output$plot_c <- renderPlot({
    datos <- cargar_reporte_c()

    ggplot(datos, aes(x = x, y = y, fill = region)) +
      geom_col() +
      labs(x = "Agente (x)", y = "Casos (y)", fill = "Región") +
      theme_minimal()
  })
}

shinyApp(ui, server)
