FROM rocker/shiny:latest

# Copiar la app completa
COPY . /srv/shiny-server/

# Render necesita 8080
RUN sed -i 's/3838/8080/g' /etc/shiny-server/shiny-server.conf

EXPOSE 8080

CMD ["/usr/bin/shiny-server"]

