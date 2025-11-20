FROM rocker/shiny:latest

# Instala paquetes adicionales de R
RUN install2.r --error \
    shinythemes \
    plotly \
    DT \
    dplyr \
    tidyr \
    readr

# Copiar la app en /srv/shiny-server
COPY . /srv/shiny-server/

# Dar permisos
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
