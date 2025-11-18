FROM rocker/shiny:latest

# Copiar archivo de configuración personalizado
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Copiar la app a la carpeta correcta
RUN mkdir -p /srv/shiny-server/app/
COPY app.R /srv/shiny-server/app/
COPY data /srv/shiny-server/app/data/

EXPOSE 8080

CMD ["/usr/bin/shiny-server"]
