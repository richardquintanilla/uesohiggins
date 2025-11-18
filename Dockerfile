FROM rocker/shiny:latest

# Instalar paquetes del sistema necesarios
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    && apt-get clean

# Instalar paquetes de R que tu app necesite
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'DT', 'tidyverse', 'plotly'))"

# Copiar el contenido del repo al servidor
COPY . /srv/shiny-server/

# Cambiar permisos
RUN chown -R shiny:shiny /srv/shiny-server

# Puerto donde correrá Shiny
EXPOSE 3838

# Comando de inicio
CMD ["/usr/bin/shiny-server"]
