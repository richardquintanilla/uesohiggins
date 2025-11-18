FROM rocker/shiny:latest

# Copiar la app a la carpeta default donde Shiny ya espera apps
COPY . /srv/shiny-server/

# Exponer puerto correcto para Render
EXPOSE 8080

# Forzar Shiny a escuchar en 0.0.0.0:8080
ENV SHINY_PORT=8080
ENV SHINY_HOST=0.0.0.0

CMD ["/usr/bin/shiny-server"]
