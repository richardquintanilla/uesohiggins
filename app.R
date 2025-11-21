library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(stringr)

# ============================================================
#             FUNCION PARA IDENTIFICAR ARCHIVO MÁS RECIENTE
# ============================================================

extraer_info_archivo <- function(sufijo) {

  archivos <- list.files("data", pattern = paste0(sufijo, "$"), full.names = TRUE)

  if (length(archivos) == 0) {
    stop(paste("No se encontraron archivos con sufijo:", sufijo))
  }

  # extraer AAMMDD
  fechas_str <- str_extract(basename(archivos), "^[0-9]{6}")

  # convertir AAMMDD → %y%m%d
  fechas <- suppressWarnings(as.Date(fechas_str, format = "%y%m%d"))

  idx <- which.max(fechas)

  list(
    archivo = archivos[idx],
    fecha   = fechas[idx]
  )
}

# ============================================================
#      IDENTIFICAR ARCHIVOS RECIENTES (SOLO 1 VEZ)
# ============================================================

info_cob <- extraer_info_archivo("_coberturas.csv")
info_inf <- extraer_info_archivo("_influenza.csv")
info_age <- extraer_info_archivo("_agentes.csv")

# ============================================================
#   CARGAR LOS CSV UNA SOLA VEZ → MUCHÍSIMO MÁS RÁPIDO
# ============================================================

cob_df <- read_csv(info_cob$archivo, show_col_types = FALSE)
inf_df <- read_csv(info_inf$archivo, show_col_types = FALSE)
age_df <- read_csv(info_age$archivo, show_col_types = FALSE)

# Fechas formateadas
fecha_cob <- if (!is.na(info_cob$fecha)) format(info_cob$fecha, "%d-%m-%Y") else "N/A"
fecha_inf <- if (!is.na(info_inf$fecha)) format(info_inf$fecha, "%d-%m-%Y") else "N/A"
fecha_age <- if (!is.na(info_age$fecha)) format(info_age$fecha, "%d-%m-%Y") else "N/A"


# ============================================================
#                           UI
# ============================================================

ui <- fluidPage(

  sidebarLayout(
    sidebarPanel(

      div(
        style = "text-align:center; margin-bottom:20px;",
        tags$img(src = "logo_ues_blanco.png", width = "70%")
      ),

      selectInput(
        "reporte",
        "Seleccionar reporte:",
        choices = c("Coberturas", "Influenza", "Agentes")
      ),

      uiOutput("filtros_dinamicos")
    ),

    mainPanel(
      h3(textOutput("titulo_reporte")),
      h5(textOutput("fecha_reporte"), style = "color:gray; margin-top:-10px;"),

      tabsetPanel(
        tabPanel("Tabla", tableOutput("tabla")),
        tabPanel("Gráfico", plotlyOutput("grafico"))
      )
    )
  )
)

# ============================================================
#                         SERVER
# ============================================================

server <- function(input, output, session) {

  # ------------------------------------------------------------
  #                 FILTROS DINÁMICOS
  # ------------------------------------------------------------
  output$filtros_dinamicos <- renderUI({

    if (input$reporte == "Coberturas") {
      return(
        selectInput(
          "categoria", "Categoría",
          choices = c("Todos", sort(unique(cob_df$categoria))),
          multiple = TRUE
        )
      )
    }

    if (input$reporte == "Influenza") {
      return(
        selectInput(
          "grupo", "Grupo",
          choices = c("Todos", sort(unique(inf_df$grupo))),
          multiple = TRUE
        )
      )
    }

    if (input$reporte == "Agentes") {
      return(
        selectInput(
          "region", "Región",
          choices = c("Todos", sort(unique(age_df$region))),
          multiple = TRUE
        )
      )
    }
  })

  # ------------------------------------------------------------
  #          TÍTULO Y FECHA DE ACTUALIZACIÓN
  # ------------------------------------------------------------
  output$titulo_reporte <- renderText({
    paste("Reporte", input$reporte)
  })

  output$fecha_reporte <- renderText({
    if (input$reporte == "Coberturas") return(paste("Actualizado a:", fecha_cob))
    if (input$reporte == "Influenza")  return(paste("Actualizado a:", fecha_inf))
    if (input$reporte == "Agentes")    return(paste("Actualizado a:", fecha_age))
  })

  # ------------------------------------------------------------
  #                       TABLA
  # ------------------------------------------------------------
  output$tabla <- renderTable({

    if (input$reporte == "Coberturas") {
      df <- cob_df
      if (!("Todos" %in% input$categoria)) {
        df <- df %>% filter(categoria %in% input$categoria)
      }
      return(df)
    }

    if (input$reporte == "Influenza") {
      df <- inf_df
      if (!("Todos" %in% input$grupo)) {
        df <- df %>% filter(grupo %in% input$grupo)
      }
      return(df)
    }

    if (input$reporte == "Agentes") {
      df <- age_df
      if (!("Todos" %in% input$region)) {
        df <- df %>% filter(region %in% input$region)
      }
      return(df)
    }
  })

  # ------------------------------------------------------------
  #                      GRÁFICO
  # ------------------------------------------------------------
  output$grafico <- renderPlotly({

    if (input$reporte == "Coberturas") {

      df <- cob_df
      if (!("Todos" %in% input$categoria)) {
        df <- df %>% filter(categoria %in% input$categoria)
      }

      g <- ggplot(df, aes(x = x, y = y)) +
        geom_col(fill = "#004a99") +
        theme_minimal()

      return(ggplotly(g))
    }

    if (input$reporte == "Influenza") {

      df <- inf_df
      if (!("Todos" %in% input$grupo)) {
        df <- df %>% filter(grupo %in% input$grupo)
      }

      df$fecha <- as.Date(df$fecha)

      g <- ggplot(df, aes(x = fecha, y = valor)) +
        geom_line(size = 1.1, color = "#cc0000") +
        theme_minimal()

      return(ggplotly(g))
    }

    if (input$reporte == "Agentes") {

      df <- age_df
      if (!("Todos" %in% input$region)) {
        df <- df %>% filter(region %in% input$region)
      }

      g <- ggplot(df, aes(x = x, y = y)) +
        geom_point(size = 3, color = "#009944") +
        theme_minimal()

      return(ggplotly(g))
    }
  })
}

shinyApp(ui, server)
