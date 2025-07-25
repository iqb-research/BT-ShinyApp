FROM rocker/shiny-verse:4.4

# Remove default example apps
RUN rm -rf /srv/shiny-server/*

# Install system libraries needed by common R packages
RUN apt-get update && apt-get install -y \
    libudunits2-dev libproj-dev libgdal-dev \
    libharfbuzz-dev libfribidi-dev cmake \
    less wget vim && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install LaTeX 
RUN R -e "install.packages('tinytex'); tinytex::install_tinytex()"

# Copy your Shiny package into the container
COPY src /srv/shiny-server/app

# Install the Shiny app package (including dependencies from DESCRIPTION)
RUN R -e "devtools::install_local('/srv/shiny-server/app', dependencies = TRUE)"

# Make 'shiny' user the owner of the app directory
RUN chown -R shiny:shiny /srv/shiny-server/app

# Switch to non-root user for running the app
USER shiny

# Expose the port Shiny Server runs on
EXPOSE 3838

# Default CMD for shiny-server (loads any app in /srv/shiny-server)
CMD ["/usr/bin/shiny-server"]
