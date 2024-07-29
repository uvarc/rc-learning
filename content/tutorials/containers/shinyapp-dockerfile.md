---
date: "2023-05-01"
title: "Writing the Dockerfile"
weight: 210
---

Below is the Dockerfile that we will use to build the container for our Chick Weight app. Each line is a command for building our environment and corresponds to a different layer of our image. We will cover each section below.


## 1. Choose a base image
```
# Install R version 4.1.2
FROM r-base:4.1.2
```

In this section we are specifying that we are starting with the r-base Docker image. The r-base container uses Ubuntu and already has R installed, so we don't need to worry about installing that ourselves. There are many other base containers out there that you can use depending on what kind of app you're developing.

## 2. Install Ubuntu packages and libraries

The following packages and libraries will allow us to install Shiny server and various R packages that we need for our app. This list will cover most of your bases for most Shiny apps. If you find you need additional libraries, you can just add them to this list.

How do you know if you're missing a library? You'll get an error message, and we will cover how to debug in a later section.
```
# Install Ubuntu packages
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev \
    libssl-dev \
    libxml2-dev \
    libnlopt-dev \
    libudunits2-dev \
    libgeos-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgdal-dev \
    git             
```

## 3. Install Shiny server

This just installs Shiny server in your image. If you're not developing a Shiny app, no need to include it. If you *are* developing a Shiny app, no need to change it!

```
# Install Shiny server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt) && \
    wget "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb
```

## 4. Install R Packages

Here we are installing all the packages that we need for our Shiny app. Again, if you're not developing a Shiny app, you can skip this part. 

```
##### Install R packages that are required ######
## CRAN packages
RUN R -e "install.packages(c('shiny','shinydashboard','dplyr','ggplot2','fresh'))"
```

## 5. Copy configuration files to the Docker image

These are just some files that make our Shiny app run. These will be in the directory that your app is in. These will be the same for all Shiny apps.
```
# Copy configuration files into the Docker image
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN rm -rf /srv/shiny-server/*
```

## 6. Copy your code to your app

Ideally your code will be maintained within a GitHub repository (we will cover how to do this in a later section). Here we are cloning the GitHub repo and copying the contents to the shiny-server folder.
```
# Get the app code
RUN git clone https://github.com/uvarc/chickweight.git
COPY chickweight/* /srv/shiny-server/
RUN rm -rf chickweight
```

## 7. Some R and Shiny Stuff

This is just some stuff for setting R paths, etc.
```
# Make the ShinyApp available at port 80
ENV R_HOME=/usr/lib/R
ENV PATH=/usr/lib/R:/usr/lib/R/bin:$PATH
EXPOSE 80
WORKDIR /srv/shiny-server
RUN chown shiny.shiny /usr/bin/shiny-server.sh && chmod 755 /usr/bin/shiny-server.sh
```

## 8. Run the Shiny app!

The CMD just tells the container what to run once it starts. This line starts up the Shiny server and app.
```
# Run the server setup script
CMD ["/usr/bin/shiny-server.sh"]
```

Once we're done writing the Dockerfile, we save it as "Dockerfile" (no file extension).
```
