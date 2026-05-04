FROM rocker/shiny:4.4.0

# Instalar TODAS las librerías que usa app.R (shiny incluido)
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'tidyverse', 'plotly', 'lubridate', 'reactable', 'htmltools'))"

# Crear directorio principal
RUN mkdir -p /srv/shiny-server

# Copiar archivos
COPY app.R /srv/shiny-server/
COPY data /srv/shiny-server/data/
COPY www /srv/shiny-server/www/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
