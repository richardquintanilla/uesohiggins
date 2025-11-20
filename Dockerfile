FROM rocker/shiny:4.4.0

# Dependencias del sistema
RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl-dev libcurl4-openssl-dev libxml2-dev libfontconfig1 \
  && apt-get clean && rm -rf /var/lib/apt/lists/*

# Instalar paquetes R necesarios
RUN R -e "install.packages(c('shiny','dplyr','readr','ggplot2','plotly'), repos='https://cloud.r-project.org/')"

# Trabajar dentro del directorio por defecto de Shiny Server
WORKDIR /srv/shiny-server

# Copiar app + data + www
COPY app.R /srv/shiny-server/app.R
COPY data /srv/shiny-server/data
COPY www /srv/shiny-server/www

# Permisos
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
