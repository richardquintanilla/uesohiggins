FROM rocker/shiny:latest

# Crear carpeta para la app
RUN mkdir -p /srv/shiny-server/app/

# Copiar archivos dentro de la carpeta correcta
COPY app.R /srv/shiny-server/app/
COPY data /srv/shiny-server/app/data/

# Cambiar puerto a 8080
RUN sed -i 's/3838/8080/g' /etc/shiny-server/shiny-server.conf

EXPOSE 8080

CMD ["/usr/bin/shiny-server"]
