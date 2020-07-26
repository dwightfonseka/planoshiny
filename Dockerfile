FROM rocker/shiny-verse:latest
EXPOSE 3838:3838
RUN apt-get update 
RUN apt-get install -y  libjpeg-dev  
RUN apt-get install -y  libtiff-dev
RUN apt-get install -y  libpng-dev 
RUN apt-get install -y  pandoc
RUN apt-get install -y  pandoc-citeproc 
RUN apt-get install -y  libssl-dev
RUN apt-get install -y  libcurl4-gnutls-dev 
RUN apt-get install -y  libcairo2-dev
RUN apt-get install -y  libgdal-dev
RUN apt-get install -y  libgeos-dev
RUN apt-get install -y  libproj-dev
RUN apt-get install -y  libxml2-dev
RUN apt-get install -y  libxt-dev
RUN apt-get install -y  libv8-dev
RUN apt-get install -y  libmagick++-dev 
RUN apt-get install -y  git 
RUN cd /srv/shiny-server
RUN mkdir pmcv
RUN pwd
RUN sudo R -e 'install.packages(c("shiny","shinyWidgets","httr","jsonlite","shinyjs","ggplot2"),dep=T)'
    