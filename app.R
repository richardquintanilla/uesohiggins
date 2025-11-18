library(shiny)

ui <- fluidPage(
  titlePanel("Selector de Reportes"),
  sidebarLayout(
    sidebarPanel(
      selectInput("reporte", "Seleccione un reporte:",
                  choices = c("Reporte A", "Reporte B"))
    ),
    mainPanel(
      uiOutput("contenido_reporte")
    )
  )
)

server <- function(input, output, session) {

  output$contenido_reporte <- renderUI({
    if (input$reporte == "Reporte A") {
      includeMarkdown("reportes/reporte_a.md")
    } else {
      includeMarkdown("reportes/reporte_b.md")
    }
  })

}

shinyApp(ui, server)
