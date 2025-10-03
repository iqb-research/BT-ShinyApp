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
    
# Accept GITHUB_PAT as a build argument
ARG GITHUB_PAT

# Set environment variable for R to use it
ENV GITHUB_PAT=${GITHUB_PAT}   

# Install remotes and other dependencies
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('iqb-research/eatMap')"

# Install your Shiny app package from GitHub (this includes everything: R/, data/, man/, inst/, etc.)
# RUN R -e "remotes::install_github('iqb-research/BT-ShinyApp@v1.1.0')"
RUN R -e 'remotes::install_github("iqb-research/BT-ShinyApp", ref = "dockerTest3")'

# Run app directly from package — no need to copy files
EXPOSE 3838

# Add this line
ENV SHINY_SERVER_VERSION=1.5.0

# Set default host and port (can be overridden at runtime)
ENV SHINY_HOST=0.0.0.0
ENV SHINY_PORT=3838

# Run the app from the installed R package
CMD R -e "BTShinyApp::run_app()"

