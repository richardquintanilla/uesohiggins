library(shiny)
library(dplyr)
library(readr)
library(plotly)
library(ggplot2)

# ================================
# CONFIGURACIÓN DE COLORES
# ================================
color_titulo <- "#005CA9"    
color_sidebar <- "#F0F4F7"   
color_borde <- "#D0D7DD"

# ================================
# UI
# ================================
ui <- fluidPage(

  # ---- ESTILOS CSS ----
  tags$head(
    tags$style(HTML(sprintf("
      .titulo-banner {
        background-color: %s;
        padding: 18px;
        color: white;
        font-size: 26px;
        font-weight: bold;
      }

      .sidebar-custom {
        background-color: %s;
        padding: 20px;
        border-right: 2px solid %s;
        height: 100vh;
      }

      .sidebar-logo {
        width: 120px;
        margin-bottom: 20px;
      }

      .fecha-reporte {
        margin-top: 15px;
        font-size: 13px;
        color: #555;
      }
    ", color_titulo, color_sidebar, color_borde)))
  ),

  # ---- BANNER SUPERIOR ----
  div(class = "titulo-banner", "UES O'Higgins – Reportes"),

  # ---- LAYOUT PRINCIPAL ----
  fluidRow(

    # --------- SIDEBAR IZQUIERDO ---------
    column(
      width = 2,
      div(class = "sidebar-custom",

          # Logo cargado desde carpeta /www
          img(src = "logo_blanco_ues.png", class = "sidebar-logo"),

          # Selector de reporte
          selectInput(
            "reporte",
            "Seleccione un reporte:",
            choices = c(
              "Reporte A – Coberturas",
              "Reporte B – Influenza",
              "Reporte C – Agentes Etiológicos"
            )
          ),

          div(class = "fecha-reporte",
              paste("Fecha del reporte:", format(Sys.Date(), "%d-%m-%Y"))
          )
      )
    ),

    # --------- CONTENIDO PRINCIPAL ---------
    column(
      width = 10,

      # ---- FILTROS SUPERIORES DINÁMICOS ----
      uiOutput("filtros"),

      # ---- PESTAÑAS ----
      tabsetPanel(
        tabPanel("Tabla", tableOutput("tabla")),
        tabPanel("Gráfico", plotlyOutput("grafico"))
      )
    )
  )
)

# ================================
# SERVER
# ================================
server <- function(input, output, session) {

  # ---- CARGA REACTIVA DE DATOS ----
  datos_reporte <- reactive({

    if (input$reporte == "Reporte A – Coberturas") {
      read_csv("data/coberturas.csv")

    } else if (input$reporte == "Reporte B – Influenza") {
      read_csv("data/influenza.csv")

    } else {
      read_csv("data/agentes.csv")
    }
  })


  # ============================================================
  #  NUEVO: FILTROS DINÁMICOS + SEXO + EDAD + SELECCIÓN MÚLTIPLE
  # ============================================================
  output$filtros <- renderUI({

    datos <- datos_reporte()

    filtros_base <- list()

    # ---- Filtro dinámico según reporte ----
    if (input$reporte == "Reporte A – Coberturas") {

      filtros_base[[1]] <- selectInput(
        "filtro_cat",
        "Categoría:",
        choices = unique(datos$categoria),
        multiple = TRUE      # <<--- ahora es múltiple
      )

    } else if (input$reporte == "Reporte B – Influenza") {

      filtros_base[[1]] <- selectInput(
        "filtro_grupo",
        "Grupo:",
        choices = unique(datos$grupo),
        multiple = TRUE
      )

    } else {

      filtros_base[[1]] <- selectInput(
        "filtro_region",
        "Región:",
        choices = unique(datos$region),
        multiple = TRUE
      )
    }

    # ---- NUEVOS: Sexo y Edad ----
    filtros_base[[2]] <- selectInput(
      "filtro_sexo",
      "Sexo:",
      choices = c("Ambos", "Hombre", "Mujer"),
      selected = "Ambos"
    )

    filtros_base[[3]] <- selectInput(
      "filtro_edad",
      "Edad:",
      choices = c("0-4", "5-14", "15-64", "65+"),
      selected = "15-64"
    )

    return(filtros_base)
  })


  # ================================
  # DATOS FILTRADOS
  # ================================
  datos_filtrados <- reactive({

    datos <- datos_reporte()

    # Filtro dinámico por reporte
    if (input$reporte == "Reporte A – Coberturas") {
      req(input$filtro_cat)
      datos <- datos %>% filter(categoria %in% input$filtro_cat)

    } else if (input$reporte == "Reporte B – Influenza") {
      req(input$filtro_grupo)
      datos <- datos %>% filter(grupo %in% input$filtro_grupo)

    } else {
      req(input$filtro_region)
      datos <- datos %>% filter(region %in% input$filtro_region)
    }

    # Filtro SEXO
    if (input$filtro_sexo != "Ambos") {
      datos <- datos %>% filter(sexo == input$filtro_sexo)
    }

    # Filtro EDAD
    datos <- datos %>% filter(edad == input$filtro_edad)

    return(datos)
  })

  # ================================
  # TABLA
  # ================================
  output$tabla <- renderTable({
    datos_filtrados()
  })

  # ================================
  # GRÁFICO
  # ================================
  output$grafico <- renderPlotly({

    datos <- datos_filtrados()

    if (input$reporte == "Reporte A – Coberturas") {

      p <- ggplot(datos, aes(x = categoria, y = y)) +
        geom_col(fill = "#1f77b4") +
        theme_minimal()

    } else if (input$reporte == "Reporte B – Influenza") {

      p <- ggplot(datos, aes(x = fecha, y = valor)) +
        geom_line(color = "#d62728") +
        theme_minimal()

    } else {

      p <- ggplot(datos, aes(x = x, y = y, fill = region)) +
        geom_col() +
        theme_minimal()
    }

    ggplotly(p)
  })
}

shinyApp(ui, server)

