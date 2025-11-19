FROM rocker/shiny:latest

# Dependencias del sistema
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    && apt-get clean

# Instalar paquetes R necesarios (ajusta la lista si ya tienes renv)
RUN R -e "install.packages(c('shiny','DT','plotly','dplyr','readr'), repos='https://cloud.r-project.org')"

# Copiar todo el repo al contenedor
COPY . /srv/shiny-server/
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server", "/srv/shiny-server/shiny-server.conf"]
