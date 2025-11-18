FROM rocker/shiny:latest

# Instalar paquetes necesarios
RUN R -e "install.packages(c( \
    'shiny','bslib','thematic','ggplot2','readr','dplyr' \
))"

# Copiar la app
COPY . /srv/shiny-server/

# Cambiar puerto de 3838 a 8080
RUN sed -i 's/3838/8080/g' /etc/shiny-server/shiny-server.conf

EXPOSE 8080

CMD ["/usr/bin/shiny-server"]
