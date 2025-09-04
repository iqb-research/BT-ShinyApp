#' Run the Shiny App
#' @export
run_app <- function() {
  appDir <- system.file("app", package = "BTShinyApp")
  shiny::runApp(appDir, display.mode = "normal")
}
