FROM rocker/shiny-verse:4.4

# Remove example apps (optional)
RUN rm -rf /srv/shiny-server/*

# System dependencies (adjust as needed)
RUN apt-get update && apt-get install -y \
    libudunits2-dev libproj-dev libgdal-dev \
    libharfbuzz-dev libfribidi-dev cmake texlive-xetex \
    less wget vim && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install remotes and other dependencies
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('iqb-research/eatMap')"

# Install your Shiny app package from GitHub (this includes everything: R/, data/, man/, inst/, etc.)
RUN R -e "remotes::install_github('iqb-research/BT-ShinyApp@v1.1.0')"

# Run app directly from package — no need to copy files
EXPOSE 3838
CMD ["R", "-e", "BTShinyApp::run_app()"]
