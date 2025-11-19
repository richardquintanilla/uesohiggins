FROM rocker/r-ver:4.4.0

# instalar dependencias del sistema (ajusta según lo que uses)
RUN apt-get update && apt-get install -y \
    libssl-dev libcurl4-openssl-dev libxml2-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# instalar los paquetes de R necesarios (ajusta los nombres)
RUN R -e "install.packages(c('shiny', 'tidyverse', 'plotly'))"

# copiar la app
WORKDIR /app
COPY app.R /app/

# definir el puerto para Render
ENV PORT=3838
EXPOSE 3838

# comando para lanzar la app
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')) )"]
