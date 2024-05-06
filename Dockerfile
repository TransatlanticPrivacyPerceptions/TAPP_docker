# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

    ## update system libraries
RUN apt-get update && \
apt-get upgrade -y && \
apt-get clean

# copy necessary files
## app folder
COPY . /app

# install packages
RUN R -e "install.packages(pkgs=c('shiny','haven','dplyr','tidyr','hrbrthemes','echarts4r','shinydashboard','shinydashboardPlus','stringr','shinyjs','DT','htmlwidgets'), repos='https://cran.rstudio.com/')"

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]