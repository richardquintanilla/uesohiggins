# Dockerfile alternativo (más ligero)
FROM rocker/shiny:4.4.0

# Instalar paquetes adicionales
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Instalar paquetes de R
RUN R -e "install.packages(c(
    'shinydashboard',
    'tidyverse',
    'plotly',
    'DT',
    'lubridate',
    'janitor',
    'readxl'
))"

# Copiar app
COPY app.R /srv/shiny-server/
COPY listados /srv/shiny-server/listados/
COPY www /srv/shiny-server/www/

# Exponer puerto
EXPOSE 3838

# Comando para ejecutar
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]




