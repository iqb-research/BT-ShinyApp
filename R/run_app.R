#' Run the Shiny App
#' @export
run_app <- function() {
  app_dir <- system.file("BT_Shiny_App", package = "BTShinyApp")
  if (app_dir == "") stop("Could not find app directory inside package.")
  shiny::runApp(appDir, host = "0.0.0.0", port = 3838, launch.browser = FALSE)
}