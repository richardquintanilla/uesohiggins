FROM rocker/shiny:latest

COPY . /srv/shiny-server/

# Render requiere que la app escuche en 0.0.0.0:8080
RUN sed -i 's/3838/8080/g' /etc/shiny-server/shiny-server.conf

EXPOSE 8080

CMD ["/usr/bin/shiny-server"]
