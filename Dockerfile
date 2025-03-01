FROM rocker/shiny-verse:4.4

# Helper tools for dev
RUN apt update && apt install -y less wget vim

COPY src /srv/shiny-server/app

# Install R dependencies
RUN apt install -y libudunits2-dev libproj-dev libgdal-dev libharfbuzz-dev libfribidi-dev
RUN Rscript /srv/shiny-server/app/requirements.R

RUN chown -R shiny:shiny /srv/shiny-server

USER shiny

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]