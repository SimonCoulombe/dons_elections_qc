FROM rocker/geospatial:latest

RUN apt-get update && apt-get install -y \
    fonts-liberation \
    libasound2 \
    libatk-bridge2.0-0 \
    libatk1.0-0 \
    libatspi2.0-0 \
    libcups2 \
    libdbus-1-3 \
    libdrm2 \
    libgbm1 \
    libgtk-3-0 \
#    libgtk-4-1 \
    libnspr4 \
    libnss3 \
    libwayland-client0 \
    libxcomposite1 \
    libxdamage1 \
    libxfixes3 \
    libxkbcommon0 \
    libxrandr2 \
    xdg-utils \
    libu2f-udev \
    libvulkan1
 # Chrome instalation 
RUN curl -LO  https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
RUN apt-get install -y ./google-chrome-stable_current_amd64.deb
RUN rm google-chrome-stable_current_amd64.deb
# Check chrome version
RUN echo "Chrome: " && google-chrome --version

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
RUN R -e 'install.packages(c("gtExtras"))'


#CMD ["/bin/bash"]
CMD ["/init"]

