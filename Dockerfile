FROM rocker/r-ver:4.4.0

# Instalar sistema
RUN apt-get update && apt-get install -y \
    libssl-dev libcurl4-openssl-dev libxml2-dev \
    && apt-get clean

# Instalar Shiny
RUN R -e "install.packages(c('shiny', 'tidyverse'))"

# Crear carpeta de la app
WORKDIR /app
COPY app.R /app/

# Render setea PORT dinámicamente
ENV PORT=3838

EXPOSE 3838

# Ejecutar la app
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')))"]
