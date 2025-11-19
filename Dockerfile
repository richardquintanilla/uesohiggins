FROM rocker/r-ver:4.4.0

RUN apt-get update && apt-get install -y \
    libssl-dev libcurl4-openssl-dev libxml2-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny', 'tidyverse', 'plotly', 'readr', 'dplyr'))"

WORKDIR /app

COPY app.R /app/
COPY data /app/data
# Si tienes www: COPY www /app/www

ENV PORT=3838
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')))"]
