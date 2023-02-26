# Dockerfile for shiny server
FROM rocker/shiny-verse

MAINTAINER Ge lab "xijin.ge@sdstate.edu"
RUN apt-get update || apt-get update
RUN apt-get update -qq && apt-get install -y \
  git-core \
  wget \
  unzip \
  libcurl4-openssl-dev \
  libxml2-dev \
  libxml2  \
  libssl-dev \
  libudunits2-dev \
  libmariadbclient-dev \
  libpng-dev \
  libproj-dev \
  vim

COPY ./config /usr/local/src/myscripts
COPY ./shinyapps /srv/shiny-server

RUN mkdir -p /srv/data

WORKDIR /usr/local/src/myscripts

EXPOSE 3838

# Install required R libraries
RUN Rscript librarySetup.R
