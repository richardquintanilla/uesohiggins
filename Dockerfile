FROM rocker/shiny:4.4.0

# Instalar paquetes
RUN R -e "install.packages(c('shinydashboard', 'tidyverse', 'plotly', 'DT', 'lubridate', 'janitor', 'readxl'))"

# Copiar app
COPY app.R /srv/shiny-server/

# Copiar datos
COPY data /srv/shiny-server/data/

# Copiar www (logo)
COPY www /srv/shiny-server/www/

# Exponer puerto
EXPOSE 3838

# Ejecutar
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
