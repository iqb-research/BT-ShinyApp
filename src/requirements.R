packages <- c("remotes", "shinythemes", "shinyWidgets", "widgetframe", "ggplot2", "sf", "shinyBS", "eatMap", "shiny.i18n")

install_if_missing <- function(pkg) {
  if(!require(pkg, character.only = TRUE)) {
    if(pkg != "eatMap") {
      install.packages(pkg, repos = "https://cloud.r-project.org/")
    } else {
      remotes::install_github("franikowsp/eatMap", upgrade="never")
    }
  }
}

lapply(packages, install_if_missing)

