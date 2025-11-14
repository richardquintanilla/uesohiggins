
library(shiny)
library(dplyr)
library(DT)
library(plotly)

df <- read.csv("data/ejemplo.csv")

ui <- fluidPage(
  titlePanel("Dashboard Interactivo - Ejemplo"),

  sidebarLayout(
    sidebarPanel(
      selectInput("cat", "Categoría:", choices = unique(df$categoria)),
      hr(),
      downloadButton("descargar", "Descargar datos filtrados")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Tabla", DTOutput("tabla")),
        tabPanel("Gráfico", plotlyOutput("grafico"))
      )
    )
  )
)

server <- function(input, output, session){

  datos_filtrados <- reactive({
    df %>% filter(categoria == input$cat)
  })

  output$tabla <- renderDT({
    datatable(datos_filtrados())
  })

  output$grafico <- renderPlotly({
    plot_ly(datos_filtrados(), x = ~x, y = ~y,
            type = "scatter", mode = "markers")
  })

  output$descargar <- downloadHandler(
    filename = function(){ "datos_filtrados.csv" },
    content = function(file){
      write.csv(datos_filtrados(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
