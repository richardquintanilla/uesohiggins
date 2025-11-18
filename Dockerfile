FROM rocker/shiny:latest

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

RUN mkdir -p /srv/shiny-server/app/
COPY app.R /srv/shiny-server/app/
COPY data /srv/shiny-server/app/data/

EXPOSE 8080

CMD ["/usr/bin/shiny-server"]
