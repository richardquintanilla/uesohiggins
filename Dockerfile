FROM rocker/shiny:4.4.0

# Instalar SOLO lo que realmente usa app.R
RUN R -e "install.packages(c('shinydashboard', 'tidyverse', 'plotly', 'lubridate', 'reactable', 'htmltools', 'fst', 'openxlsx', 'data.table'))"

RUN mkdir -p /srv/shiny-server

COPY app.R /srv/shiny-server/
COPY data /srv/shiny-server/data/
COPY www /srv/shiny-server/www/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
