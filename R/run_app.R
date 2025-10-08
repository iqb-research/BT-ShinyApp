#' Run the Shiny App
#' @export
run_app <- function(host = Sys.getenv("SHINY_HOST", "0.0.0.0"),
                    port = as.numeric(Sys.getenv("SHINY_PORT", 3838))) {
  app_dir <- system.file("BT_Shiny_App", package = "BTShinyApp")
  if (app_dir == "") stop("Could not find app directory inside package.")
  shiny::runApp(app_dir, host = host, port = port, launch.browser = FALSE)
}
