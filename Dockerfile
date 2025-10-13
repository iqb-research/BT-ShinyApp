# Base image with R and Shiny Server
FROM rocker/shiny:4.4.0

# System dependencies
RUN apt-get update && apt-get install -y \
    libudunits2-dev libproj-dev libgdal-dev \
    libharfbuzz-dev libfribidi-dev cmake \
    texlive-xetex pandoc && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Install R dependencies
RUN R -e "install.packages('remotes', repos = 'https://cloud.r-project.org')"
RUN R -e "remotes::install_github('iqb-research/eatMap', dependencies = TRUE)"

# Copy and install the Shiny app package
COPY R /tmp/BTShinyApp/R
COPY data /tmp/BTShinyApp/data
COPY inst /tmp/BTShinyApp/inst
COPY man /tmp/BTShinyApp/man
COPY DESCRIPTION /tmp/BTShinyApp/DESCRIPTION
COPY NAMESPACE /tmp/BTShinyApp/NAMESPACE
COPY .Rbuildignore /tmp/BTShinyApp/.Rbuildignore

RUN R -e "remotes::install_local('/tmp/BTShinyApp', dependencies = TRUE)" \
    && rm -rf /tmp/BTShinyApp

# Create Shiny app directory for Shiny Server
WORKDIR /srv/shiny-server

# Use a small R script to launch the app from the package
RUN echo "BTShinyApp::run_app()" > app.R

# Shiny Server runs on port 3838
EXPOSE 3838

# Optionally: adjust permissions (important for Dockerized Shiny)
RUN chown -R shiny:shiny /srv/shiny-server

# Run Shiny Server (the default CMD in rocker/shiny is already `shiny-server`)
CMD ["/usr/bin/shiny-server"]
