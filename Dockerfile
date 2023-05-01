FROM rocker/r-base:latest

RUN apt-get update && apt-get install -y \
    git \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN R -e 'install.packages(c("httr", "lubridate"))'

CMD ["/bin/bash"]
