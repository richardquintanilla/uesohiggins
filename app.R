# app.R
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(leaflet)
library(dplyr)

# ---------------------------
# Colores (modifica acá)
# ---------------------------
colors <- list(
  primary = "#2C7FB8",
  secondary = "#7FCDBB",
  accent = "#FEB24C",
  background = "#F7FCFD",
  text = "#08306B"
)

# ---------------------------
# Datos de ejemplo
# Sustituye por tus datos reales.
# Debe contener al menos: year, sex, age_group, value, lat, lon, report (A/B)
# ---------------------------
set.seed(42)
n <- 1000
df <- data.frame(
  id = 1:n,
  year = sample(2018:2024, n, replace = TRUE),
  sex = sample(c("Male","Female","Other"), n, replace = TRUE, prob = c(0.48,0.5,0.02)),
  age_group = sample(c("0-17","18-35","36-55","56+"), n, replace = TRUE),
  value = round(rnorm(n, 50, 20),1),
  lat = jitter(rep(c(-33.45, -34.6, -32.9), length.out = n), 0.1),
  lon = jitter(rep(c(-70.66, -70.9, -71.2), length.out = n), 0.1),
  report = sample(c("A","B"), n, replace = TRUE)
)
# ---------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML(sprintf("
      body { background: %s; color: %s; }
      .navbar-default { background-color: %s; border-color: transparent; }
      .well { background: white; }
      .filter-label { font-weight: 600; color: %s; }
    ", colors$background, colors$text, colors$primary, colors$text)))
  ),

  titlePanel("Dashboard: Reportes A y B"),

  sidebarLayout(
    sidebarPanel(
      selectInput("report_sel", "Seleccione reporte:", choices = c("A","B"), selected = "A"),
      selectInput("year", "Año:", choices = sort(unique(df$year), decreasing = TRUE), multiple = FALSE),
      selectInput("sex", "Sexo:", choices = c("All", sort(unique(df$sex))), selected = "All"),
      selectInput("age", "Grupo etario:", choices = c("All", sort(unique(df$age_group))), selected = "All"),
      hr(),
      helpText("Los colores están definidos en el objeto `colors` en app.R. Modifícalos ahí.")
    ),

    mainPanel(
      # Pestañas que cambian según el reporte seleccionado (pero estructura fija)
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Tabla",
                 br(),
                 DTOutput("table")
        ),
        tabPanel("Gráfico",
                 br(),
                 plotlyOutput("plot")
        ),
        tabPanel("Mapa",
                 br(),
                 leafletOutput("map", height = 600)
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # reactive dataset filtrado según inputs globales y reporte
  df_filtered <- reactive({
    req(input$year, input$report_sel)
    d <- df %>%
      filter(year == as.integer(input$year), report == input$report_sel)

    if (input$sex != "All") d <- filter(d, sex == input$sex)
    if (input$age != "All") d <- filter(d, age_group == input$age)
    d
  })

  # TAB: Tabla
  output$table <- renderDT({
    dat <- df_filtered() %>%
      select(id, year, sex, age_group, value) %>%
      arrange(desc(value))
    datatable(dat, options = list(pageLength = 10))
  })

  # TAB: Gráfico (ejemplo: promedio por grupo etario)
  output$plot <- renderPlotly({
    dat <- df_filtered() %>%
      group_by(age_group) %>%
      summarize(mean_value = mean(value, na.rm = TRUE), n = n()) %>%
      arrange(age_group)

    p <- ggplot(dat, aes(x = age_group, y = mean_value, fill = age_group)) +
      geom_col() +
      labs(title = paste0("Promedio (Reporte ", input$report_sel, ") - Año ", input$year),
           x = "Grupo etario", y = "Valor promedio") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(color = colors$text))

    ggplotly(p)
  })

  # TAB: Mapa (puntos de muestra)
  output$map <- renderLeaflet({
    dat <- df_filtered()
    if (nrow(dat) == 0) {
      # mapa vacío centrado en Chile (ejemplo)
      leaflet() %>%
        addTiles() %>%
        setView(lng = -70.7, lat = -33.4, zoom = 5)
    } else {
      pal <- colorNumeric(palette = c(colors$primary, colors$accent), domain = dat$value)
      leaflet(dat) %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        addCircleMarkers(~lon, ~lat, radius = 6,
                         color = ~pal(value),
                         stroke = FALSE, fillOpacity = 0.8,
                         popup = ~paste0("ID: ", id, "<br>",
                                         "Valor: ", value, "<br>",
                                         "Edad: ", age_group, "<br>",
                                         "Sexo: ", sex)) %>%
        addLegend("bottomright", pal = pal, values = ~value, title = "Valor")
    }
  })

  # Si quisieras que cada reporte tenga pestañas distintas o lógica diferente,
  # puedes usar observeEvent sobre input$report_sel para cambiar UI/filtrado/visuales.
}

shinyApp(ui, server)
