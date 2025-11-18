FROM rocker/shiny:4.3.1

# Instalar paquetes necesarios
RUN install2.r --error \
    shiny \
    dplyr \
    readr \
    ggplot2

# Copiar la aplicación
COPY . /srv/shiny-server/

# Dar permisos correctos
RUN chown -R shiny:shiny /srv/shiny-server

# Render usa el puerto 8080
EXPOSE 8080
ENV SHINY_PORT=8080
ENV SHINY_HOST=0.0.0.0

CMD ["/usr/bin/shiny-server"]
