FROM rocker/shiny:4.4.0

RUN R -e "install.packages(c('shinydashboard', 'tidyverse', 'plotly', 'DT', 'lubridate', 'janitor', 'readxl', 'reactable', 'reactablefmtr', 'data.table'))"

# Crear directorio principal
RUN mkdir -p /srv/shiny-server

# Copiar archivos (¡orden IMPORTANTE!)
COPY app.R /srv/shiny-server/
COPY data /srv/shiny-server/data/
COPY www /srv/shiny-server/www/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
