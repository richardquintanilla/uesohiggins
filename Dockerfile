# Dockerfile para Render (shiny app)
FROM rocker/r-ver:4.4.0

# instalar dependencias del sistema si hacen falta
RUN apt-get update && apt-get install -y \
    libssl-dev libcurl4-openssl-dev libxml2-dev libfontconfig1 \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# instalar paquetes R necesarios (incluye shinydashboard y DT)
RUN R -e "install.packages(c('shiny','shinydashboard','tidyverse','plotly','DT'), repos='https://cloud.r-project.org/')"

WORKDIR /app

# copiar la app (app.R), datos y www (logo)
COPY app.R /app/
COPY data /app/data
COPY www /app/www

ENV PORT=3838
EXPOSE 3838

# Ejecutar la app directamente con shiny::runApp usando el puerto de Render
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')) )"]
