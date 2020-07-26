FROM rocker/shiny-verse:latest
EXPOSE 3838:3838
RUN apt-get update 
RUN apt-get install -y  libjpeg-dev  && apt-get install -y  libtiff-dev && apt-get install -y  libpng-dev && apt-get install -y  pandoc && apt-get install -y  pandoc-citeproc  && apt-get install -y  libssl-dev && apt-get install -y  libcurl4-gnutls-dev  && apt-get install -y  libcairo2-dev
RUN apt-get install -y  libgdal-dev && apt-get install -y  libgeos-dev && apt-get install -y  libproj-dev && apt-get install -y  libxml2-dev && apt-get install -y  libxt-dev && apt-get install -y  libv8-dev && apt-get install -y  libmagick++-dev  && apt-get install -y  git 
RUN mkdir /srv/shiny-server/pmcv
RUN sudo R -e 'install.packages(c("shiny","shinyWidgets","httr","jsonlite","shinyjs","ggplot2"),dep=T)'
RUN exit
    