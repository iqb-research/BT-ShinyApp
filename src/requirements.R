packages <- c(
  # Shiny:
  "shiny",
  "shinythemes",
  "shinyWidgets",
  # Infobuttons:
  "shinyBS", 
  # HTML:
  "widgetframe",
  # Plots (Deutschlandkarte und PDF Export):
  "ggplot2",
  "eatMap",
  # Datenselektion:
  "tidyverse",
  # Geodaten
  "sf",
  # Übersetzung
  "shiny.i18n",
  "readxl",
  "rjson",
  "jsonlite",
  "purrr"
)


# fehlende Pakete installieren
install_if_missing <- function(pkg) {
  print(pkg)
  if(!require(pkg, character.only = TRUE)) {
    if(pkg != "eatMap") {
      install.packages(pkg, repos = "https://cloud.r-project.org/")
    } else {
      remotes::install_github("franikowsp/eatMap", upgrade="never")
    }
  }
}

lapply(packages, install_if_missing)

# Pakete laden
lapply(packages, library, character.only = TRUE)

