FROM rocker/shiny-verse:latest
EXPOSE 3838:3838
RUN apt-get update 
RUN apt-get install -y \ libjpeg-dev \ libtiff-dev \ libpng-dev 
RUN apt-get install -y \ pandoc \ pandoc-citeproc 
RUN apt-get install -y \ libssl-dev \ libcurl4-gnutls-dev \ libcairo2-dev \ libgdal-dev \ libgeos-dev \ libproj-dev \ libxml2-dev \ libxt-dev \ libv8-dev \ libmagick++-dev\ git 
RUN cd /srv/shiny-server
RUN mkdir pmcv
COPY /home/ubuntu/pmcv/app.R ./srv/shiny-server/pmcv/app.R
RUN sudo R -e 'install.packages(c("shiny","shinyWidgets","httr","jsonlite","shinyjs","ggplot2"),dep=T)'
    