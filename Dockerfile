# Dockerfile - Versión ultra simple
FROM rocker/shiny:4.4.0

# Instalar paquetes de R necesarios
RUN R -e "install.packages('shinydashboard')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('plotly')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('lubridate')"
RUN R -e "install.packages('janitor')"
RUN R -e "install.packages('readxl')"

# Crear directorio
RUN mkdir -p /srv/shiny-server/ges

# Copiar archivos
COPY app.R /srv/shiny-server/ges/
COPY listados /srv/shiny-server/ges/listados/
COPY www /srv/shiny-server/ges/www/

# Exponer puerto
EXPOSE 3838

# Ejecutar
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/ges', host='0.0.0.0', port=3838)"]
