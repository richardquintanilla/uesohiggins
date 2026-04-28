FROM rocker/shiny:4.4.0

# Instalar paquetes
RUN R -e "install.packages(c('shinydashboard', 'tidyverse', 'plotly', 'DT', 'lubridate', 'janitor', 'readxl'))"

# Copiar app.R a la ubicación correcta
COPY app.R /srv/shiny-server/

# Exponer puerto
EXPOSE 3838

# Ejecutar desde el directorio correcto
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
