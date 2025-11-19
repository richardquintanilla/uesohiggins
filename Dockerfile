# Dockerfile (para Render.com / cualquier plataforma que entregue $PORT)
FROM rocker/shiny:latest

# (Opcional) versión fija:
# FROM rocker/shiny:4.2.3

# install system libraries required by some R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgeos-dev \
    libudunits2-dev \
    libgdal-dev \
    libproj-dev \
    locales \
    && rm -rf /var/lib/apt/lists/*

# set locale
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen

# copy app
COPY . /srv/shiny-server/

WORKDIR /srv/shiny-server/

# install R packages (add or remove según necesites)
RUN R -e "install.packages(c('shiny','DT','ggplot2','plotly','leaflet','dplyr'), repos='https://cloud.r-project.org')"

# Expose port (no importa para Render, pero documenta)
EXPOSE 3838

# Start the app using the PORT env var (Render sets PORT). 
# If PORT undefined, default to 3838.
CMD R -e "port <- as.integer(Sys.getenv('PORT', Sys.getenv('SHINY_PORT','3838'))); if(is.na(port)) port <- 3838; message('Starting Shiny on port: ', port); shiny::runApp('/srv/shiny-server', host='0.0.0.0', port = port)"
