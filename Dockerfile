FROM rocker/r-ver:4.4

# System dependencies
RUN apt-get update && apt-get install -y \
    libudunits2-dev libproj-dev libgdal-dev \
    libharfbuzz-dev libfribidi-dev cmake texlive-xetex \
    pandoc

# Install R dependencies
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('iqb-research/eatMap')"

# Install Shiny app
COPY R /tmp/BTShinyApp/R
COPY data /tmp/BTShinyApp/data
COPY inst /tmp/BTShinyApp/inst
COPY man /tmp/BTShinyApp/man
COPY DESCRIPTION /tmp/BTShinyApp/DESCRIPTION
COPY NAMESPACE /tmp/BTShinyApp/NAMESPACE
COPY .Rbuildignore /tmp/BTShinyApp/.Rbuildignore
RUN R -e "remotes::install_local('/tmp/BTShinyApp', dependencies = TRUE)"
RUN rm -rf /tmp/BTShinyApp

EXPOSE 3838

# Run the app from the installed R package which already includes server software
CMD ["R", "-e", "BTShinyApp::run_app()"]

