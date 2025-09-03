#' Run the Shiny App
#' @export
run_app <- function() {
  appDir <- system.file("BT_Shiny_App", package = "BTShinyApp")
  shiny::runApp(appDir, display.mode = "normal")
}
