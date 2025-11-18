FROM rocker/shiny:latest
COPY . /srv/shiny-server/
RUN sed -i 's/3838/8080/g' /etc/shiny-server/shiny-server.conf
EXPOSE 8080
CMD ["/usr/bin/shiny-server"]
