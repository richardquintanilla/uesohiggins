# Imagen base con R y Shiny Server
FROM rocker/shiny:latest

# Instalar paquetes de Linux necesarios
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    && rm -rf /var/lib/apt/lists/*

# Copiar tu aplicación al contenedor
COPY . /srv/shiny-server/

# Dar permisos
RUN chown -R shiny:shiny /srv/shiny-server

# Instalar los paquetes de R que necesite tu app
# Si tienes un archivo install.R mejor, si no agrega aquí a mano:
RUN R -e "install.packages(c('shiny','dplyr','readr','ggplot2','plotly','DT'), repos='https://cloud.r-project.org')"

# Exponer el puerto donde corre Shiny
EXPOSE 3838

# Comando para iniciar Shiny Server
CMD ["/usr/bin/shiny-server"]
