FROM rocker/shiny:4.4.0

# Dependencias del SO si necesarias
RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl-dev libcurl4-openssl-dev libxml2-dev libfontconfig1 \
  && apt-get clean && rm -rf /var/lib/apt/lists/*

# Instalar paquetes R necesarios
RUN R -e "install.packages(c('shiny','dplyr','readr','ggplot2','plotly','DT'), repos='https://cloud.r-project.org/')"

WORKDIR /srv/shiny-server

# Copiar la app, datos y www (logo, si existe)
COPY . /srv/shiny-server/

RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
