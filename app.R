library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
library(stringr)
library(lubridate)

# ============================================================
#             FUNCION PARA CARGAR EL ARCHIVO MÁS RECIENTE
# ============================================================

cargar_archivo_reciente <- function(sufijo) {

    archivos <- list.files("data", pattern = paste0(sufijo, "$"), full.names = TRUE)

    if (length(archivos) == 0) {
        stop(paste("No se encontraron archivos con sufijo:", sufijo))
    }

    # Extraer AAMMDD de cada archivo
    fechas <- str_extract(basename(archivos), "^[0-9]{6}")

    # Convertir a fecha usando formato AAMMDD → 20AA-MM-DD
    fechas_limpias <- as.Date(fechas, format = "%y%m%d")

    # Tomar el archivo con la fecha mayor
    archivo_max <- archivos[which.max(fechas_limpias)]

    list(
        archivo = archivo_max,
        fecha = fechas_limpias[which.max(fechas_limpias)]
    )
}

# ============================================================
#                   CARGA DE LOS 3 REPORTES
# ============================================================

infl_data <- cargar_archivo_reciente("_influenza.csv")
cov_data  <- cargar_archivo_reciente("_coberturas.csv")
age_data  <- cargar_archivo_reciente("_agentes.csv")

influenza   <- read_csv(infl_data$archivo, show_col_types = FALSE)
coberturas  <- read_csv(cov_data$archivo,  show_col_types = FALSE)
agentes     <- read_csv(age_data$archivo,  show_col_types = FALSE)

# Fechas formateadas para mostrar
fecha_influenza  <- format(infl_data$fecha, "%d-%m-%Y")
fecha_cobertura  <- format(cov_data$fecha,  "%d-%m-%Y")
fecha_agentes    <- format(age_data$fecha,  "%d-%m-%Y")


# ============================================================
#                          UI
# ============================================================

ui <- fluidPage(

    # ==== SIDEBAR CON LOGO ====
    sidebarLayout(
        sidebarPanel(
            div(
                style="text-align:center; margin-bottom:20px;",
                tags$img(src="logo_ues_blanco.png", width="70%")
            ),

            selectInput("reporte", "Seleccionar reporte:",
                        choices = c("Coberturas", "Influenza", "Agentes")),

            uiOutput("filtros_dinamicos")
        ),

        mainPanel(
            h3(textOutput("titulo_reporte")),
            h5(textOutput("fecha_reporte"), style="color:gray; margin-top:-10px;"),

            tabsetPanel(
                tabPanel("Tabla", DTOutput("tabla")),
                tabPanel("Gráfico", plotlyOutput("grafico"))
            )
        )
    )
)

# ============================================================
#                       SERVER
# ============================================================

server <- function(input, output, session) {


    # ------------------------------------------------------------
    #          GENERA FILTROS DEPENDIENDO DEL REPORTE
    # ------------------------------------------------------------
    output$filtros_dinamicos <- renderUI({

        if (input$reporte == "Coberturas") {
            tagList(
                selectInput("categoria", "Categoría",
                            choices = c("Todos", sort(unique(coberturas$categoria))),
                            multiple = TRUE)
            )
        }

        if (input$reporte == "Influenza") {
            tagList(
                selectInput("grupo", "Grupo",
                            choices = c("Todos", sort(unique(influenza$grupo))),
                            multiple = TRUE)
            )
        }

        if (input$reporte == "Agentes") {
            tagList(
                selectInput("region", "Región",
                            choices = c("Todos", sort(unique(agentes$region))),
                            multiple = TRUE)
            )
        }
    })


    # ------------------------------------------------------------
    #                TÍTULO Y FECHA DEPENDEN DEL REPORTE
    # ------------------------------------------------------------
    output$titulo_reporte <- renderText({
        paste("Reporte", input$reporte)
    })

    output$fecha_reporte <- renderText({
        if (input$reporte == "Coberturas") return(paste("Actualizado a:", fecha_cobertura))
        if (input$reporte == "Influenza")  return(paste("Actualizado a:", fecha_influenza))
        if (input$reporte == "Agentes")    return(paste("Actualizado a:", fecha_agentes))
    })


    # ------------------------------------------------------------
    #             TABLA DEPENDIENDO DEL REPORTE
    # ------------------------------------------------------------

    output$tabla <- renderDT({

        if (input$reporte == "Coberturas") {
            df <- coberturas
            if (!("Todos" %in% input$categoria)) {
                df <- df %>% filter(categoria %in% input$categoria)
            }
        }

        if (input$reporte == "Influenza") {
            df <- influenza
            if (!("Todos" %in% input$grupo)) {
                df <- df %>% filter(grupo %in% input$grupo)
            }
        }

        if (input$reporte == "Agentes") {
            df <- agentes
            if (!("Todos" %in% input$region)) {
                df <- df %>% filter(region %in% input$region)
            }
        }

        datatable(df)
    })


    # ------------------------------------------------------------
    #             GRÁFICO DEPENDIENDO DEL REPORTE
    # ------------------------------------------------------------

    output$grafico <- renderPlotly({

        if (input$reporte == "Coberturas") {
            df <- coberturas
            if (!("Todos" %in% input$categoria)) {
                df <- df %>% filter(categoria %in% input$categoria)
            }
            g <- ggplot(df, aes(x = x, y = y)) +
                geom_col(fill="#004a99") +
                theme_minimal()
        }

        if (input$reporte == "Influenza") {
            df <- influenza
            if (!("Todos" %in% input$grupo)) {
                df <- df %>% filter(grupo %in% input$grupo)
            }
            g <- ggplot(df, aes(x = fecha, y = valor)) +
                geom_line(size=1.1, color="#cc0000") +
                theme_minimal()
        }

        if (input$reporte == "Agentes") {
            df <- agentes
            if (!("Todos" %in% input$region)) {
                df <- df %>% filter(region %in% input$region)
            }
            g <- ggplot(df, aes(x = x, y = y)) +
                geom_point(size=3, color="#009944") +
                theme_minimal()
        }

        ggplotly(g)
    })
}

shinyApp(ui, server)
