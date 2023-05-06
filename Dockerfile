FROM rocker/geospatial:latest

RUN apt-get update && apt-get install -y \
    git \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN R -e 'install.packages(c("httr", "lubridate", "dplyr", "stringr", "janitor"))'
RUN R -e 'install.packages(c("readr"))'
RUN R -e 'install.packages(c("purrr"))'
RUN R -e 'install.packages(c("ggplot2"))'
RUN R -e 'install.packages(c("rtweet"))'
RUN R -e 'install.packages(c("glue"))'
RUN R -e 'install.packages(c("gt"))'
RUN R -e 'install.packages(c("webshot2"))'

#CMD ["/bin/bash"]
CMD ["/init"]

