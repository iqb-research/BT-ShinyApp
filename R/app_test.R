library(shiny)
library(bslib)
library(fontawesome)

# Beispiel-HTML-Infotext
#infotextfile <- file.path("extdata/text_elements/Infotexte.xlsx")
#infotexte <- readxl::read_excel(infotextfile)

language <- "de"  # oder "en"

infotexte_list <- setNames(
  infotexte[[language]],
  infotexte$chunk
)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "sandstone"),
  
  # Infobutton mit Hover
  actionButton(
    inputId = "infobutton_zyklus",
    label = "",
    icon = icon("info", lib = "font-awesome"),
    class = "custom-btn",
    `data-bs-toggle` = "popover",
    `data-bs-trigger` = "click",     # oder 'manual' + hover-Logik
    `data-bs-placement` = "right"
  ),
  
  # Popover initialisieren mit HTML
  tags$script(HTML(paste0("
    var button = document.querySelector('#infobutton_zyklus');
    new bootstrap.Popover(button, {
      html: true,
      content: `", infotexte_list[["Erhebungsreihe"]], "`,
      trigger: 'click',
      placement: 'right'
    });
  ")))
)

server <- function(input, output, session) {}

shinyApp(ui, server)
