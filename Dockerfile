FROM rocker/shiny-verse:4.4

# Remove default example apps
RUN rm -rf /srv/shiny-server/*

# Install system libraries needed by common R packages
RUN apt-get update && apt-get install -y \
    libudunits2-dev libproj-dev libgdal-dev \
    libharfbuzz-dev libfribidi-dev cmake texlive-xetex \
    less wget vim && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Copy your R package source to /tmp for installation
COPY R /tmp/BTShinyApp/R
COPY data /tmp/BTShinyApp/data
COPY inst /tmp/BTShinyApp/inst
COPY man /tmp/BTShinyApp/man
COPY DESCRIPTION /tmp/BTShinyApp/DESCRIPTION
COPY NAMESPACE /tmp/BTShinyApp/NAMESPACE
COPY .Rbuildignore /tmp/BTShinyApp/.Rbuildignore

RUN Rscript -e "remotes::install_github('iqb-research/eatMap')"

# Install the Shiny app package (including dependencies from DESCRIPTION)
RUN R -e "devtools::install_local('/tmp/BTShinyApp', dependencies = TRUE)"

# Clean up package source folder after install
RUN rm -rf /tmp/BTShinyApp

# Deploy the Shiny app files (app.R, export.Rmd, and inst/extdata) to Shiny Server folder
COPY inst/BT_Shiny_App/app.R /srv/shiny-server/app/app.R
COPY inst/BT_Shiny_App/export.Rmd /srv/shiny-server/app/export.Rmd
COPY inst/extdata /srv/shiny-server/app/inst/extdata

# Make 'shiny' user the owner of the app directory
RUN chown -R shiny:shiny /srv/shiny-server/app

# Switch to non-root user for running the app
USER shiny

# Expose Shiny Server port
EXPOSE 3838

# Run Shiny Server
CMD ["/usr/bin/shiny-server"]




RUN rm -rf /tmp/BTShinyApp



# Expose the port Shiny Server runs on
EXPOSE 3838

# Default CMD for shiny-server (loads any app in /srv/shiny-server)
CMD ["/usr/bin/shiny-server"]
