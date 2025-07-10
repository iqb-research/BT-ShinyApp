FROM rocker/shiny-verse:4.4

# Remove example apps
RUN rm -rf /srv/shiny-server/*

# Copy application code into the container
COPY src /srv/shiny-server/app

# Install basic tooling and R dependencies
RUN apt update && apt install -y less wget vim libudunits2-dev libproj-dev libgdal-dev libharfbuzz-dev libfribidi-dev cmake
RUN Rscript /srv/shiny-server/app/requirements.R

# Make user 'shiny' (non-root) owner of all shiny files
RUN chown -R shiny:shiny /srv/shiny-server

# Set user which runs the application (non-root)
USER shiny

# Open network port
EXPOSE 3838

# Run server application
CMD ["/usr/bin/shiny-server"]
