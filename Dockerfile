# Dockerfile para GES Monitoreo App
FROM rocker/r-ver:4.4.0

# Instalar dependencias del sistema
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Instalar TODOS los paquetes de R necesarios (en una sola línea)
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'tidyverse', 'dplyr', 'ggplot2', 'plotly', 'DT', 'readr', 'stringr', 'lubridate', 'janitor', 'readxl'), dependencies = TRUE)"

# Crear directorio de trabajo
WORKDIR /app

# Copiar archivos de la aplicación
COPY app.R /app/
COPY listados /app/listados/
COPY www /app/www/

# Exponer puerto
EXPOSE 3838

# Comando para ejecutar la app
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
