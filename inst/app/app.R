# Sprache der App --------------------------------------------------------------
# "de" für Deutsch, "en" für Englisch
language <- "de"

# Pakete -----------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(shiny.i18n)
library(readxl)
library(jsonlite)
library(rjson)
library(dplyr)
library(ggplot2)
library(viridis)
library(sf)
library(gridExtra)
library(grid)
library(purrr)
library(widgetframe)
library(rmarkdown)
library(knitr)
library(eatMap)
library(tinytex)
library(stringr)

# Konfigurationsliste ----------------------------------------------------------
# ... wird von eatMap verarbeitet und beim PDF-Export für ggplot2 verwendet
# ... beinhaltet auch implizit die Reihenfolge der entsprechenden Einträge
source(system.file("config", "config.R", package = "BTShinyApp"))

# Datensätze -------------------------------------------------------------------
# Rohdaten: "data/allDat.RData"
# Die Datenaufbereitung erfolgt in einem separaten Skript: "data_preparation.R".
# "data_preparation.R" muss neu ausgeführt werden, wenn "data/allDat.RData" geupdated wird.
BTdata <- readRDS(system.file("data", "BTdata_processed.Rds", package = "BTShinyApp"))


# Kartendaten
# https://gadm.org/download_country.html
map_path <- system.file("extdata", "map_data", package = "BTShinyApp")
mapdata <- sf::st_read(dsn = map_path, layer = "gadm41_DEU_1")
mapdata <- mapdata[, c("NAME_1", "geometry")]
names(mapdata) <- c("Bundesland", "geometry")

# BT Daten und Kartendaten werden erst zusammengefügt, nachdem der BT Datensatz
# im Server selektiert wurde, da es sonst zu Fehlern mit den Geodaten kommt


# UI Choices -------------------------------------------------------------------

# Zyklen
available_cycles <- unique(BTdata$cycle)[order(unique(BTdata$cycle))] # alle erhobenen Zyklen, Hotfix für Ordnung
available_parameters <- unique(BTdata$parameter) # Parameter
default_newest_cycle <- BTdata %>%
  filter(year == max(BTdata$year)) %>% # Get rows for the most recent year
  distinct(cycle) %>%                  # Find the unique cycle(s) in that year
  pull(cycle) %>%                      # Extract the cycle name(s) as a vector
  .[1]

# alle im Datensatz enthaltenen Kombinationen der UI choices
combinations <-
  BTdata %>%
  distinct(cycle, fachKb, year, parameter, targetPop)

# Vorbereitung der grouped radio buttons ---------------------------------------

radioSubgroup <- function(inputId, parentId, label, choices, selected, inline = FALSE) {
  values <- paste0(parentId, "-", choices)
  choices <- setNames(values, choices)
  rb <- radioButtons(inputId, label, choices, selected, inline = inline)
  rb$children
}

radioGroupContainer <- function(inputId, ...) {
  class <- "form-group shiny-input-radiogroup shiny-input-container"
  div(id = inputId, class = class, ...)
}

make_radioSubgroup <- function(kb_current, subject, n_subject) {
  selected_choice = character(0)
  # Treat first Kb differently
  if (n_subject == 1) {
    subject1 <- names(kb_current)[1]
    kb1 <- kb_current[[subject1]][1]
    
    selected_choice <- str_glue("{subject1}-{kb1}")
  }
  
  column(
    12,
    radioSubgroup(
      inputId = "fachKb",
      parentId = subject,
      label = str_glue("{subject}:"),
      choices = kb_current[[subject]],
      selected = selected_choice
    )
  )
}

make_radioGroupContainer <- function(kb_current) {
  radioGroupContainer("fachKb",
                      fluidRow(
                        # Uses an indexed loop (arguments are current entry (like with any loop) and
                        # its index (to check for first subject to log on cycle change)
                        imap(names(kb_current), function(x, i) {
                          make_radioSubgroup(kb_current, x, i)}
                        )
                      ))
}

# cycle_current <- "9. Klasse: Mathe/Naturwissenschaften"
make_YearPopulationParameter <- function(cycle_current) {
  # 1. Kb separieren
  fachKb1 <- config$fachKb[[cycle_current]][1]
  fach1 <- names(fachKb1)
  fachKb_default <- str_glue("{fach1}-{fachKb1[[1]][1]}")
  
  selected_combinations <- combinations[combinations$cycle == cycle_current &
                                          combinations$fachKb == fachKb_default, ]
  
  targetPop_default <- ifelse(language == "en", "All", "alle")
  parameter_default <- "mean"
  
  years <- sort(unique(selected_combinations[selected_combinations$targetPop == targetPop_default &
                                               selected_combinations$parameter == parameter_default, ]$year))
  
  # Darüber sollte immer der aktuellste BT angesteuert werden
  year_default <- max(years)
  
  parameters <- order_parameters(unique(selected_combinations[selected_combinations$targetPop == targetPop_default &
                                                                selected_combinations$year == year_default, ]$parameter))
  
  targetPops <- order_targetpop(unique(selected_combinations[selected_combinations$year == year_default &
                                                               selected_combinations$parameter == parameter_default, ]$targetPop))
  
  div(
    sliderTextInput(inputId = "Jahr",
                    label = i18n$t("Jahr"),
                    grid = TRUE,
                    choices = years,
                    selected = year_default,
                    hide_min_max = TRUE,
                    width='75%'),
    
    selectInput(
      inputId = "Zielpopulation",
      label = i18n$t("Zielpopulation"),
      choices = targetPops,
      selected = targetPop_default,
      width = '95%'
    ),
    
    selectInput(
      inputId = "Kennwert",
      label = i18n$t("Kennwert"),
      choices = parameters,
      selected = parameter_default,
      width = '95%'
    )
  )
}

# Übersetzung ------------------------------------------------------------------

json_path <- system.file("extdata", "text_elements", "translation.json", package = "BTShinyApp")
i18n <- shiny.i18n::Translator$new(translation_json_path = json_path)
infotextfile <- system.file("extdata/text_elements/Infotexte.xlsx", package = "BTShinyApp")
infotexte <- readxl::read_excel(infotextfile)

# JSON einlesen
json_path <- system.file("extdata", "text_elements", "translation.json", package = "BTShinyApp")
woerterbuch <- jsonlite::fromJSON(paste(readLines(json_path), collapse = ""), flatten = TRUE)
woerterbuch <- setNames(woerterbuch$translation$en, woerterbuch$translation$de)

# Translator setzen
i18n$set_translation_language(language)


recode_nested_list <- function(my_list, recode_rules) {
  names(my_list) <- recode(names(my_list), !!!recode_rules, .default = names(my_list))
  map(my_list, function(x) { # map() = Apply a function to each element of a vector
    if (is.list(x)) {
      recode_nested_list(x, recode_rules)  # rekursiv alle Listen durchgehen
    } else if (is.character(x)) {
      recode(x, !!! recode_rules)  # einzelne Elemente rekodieren
    } else {
      x  # alles was kein character ist in Ruhe lassen
    }
  })
}

# config Liste übersetzen
if(language == "en"){
  config <- recode_nested_list(config, woerterbuch)
}


# Funktionen zum Ordnen der Auswahlmöglichkeiten
predefined_order_parameters <- names(config$parameter)
names(predefined_order_parameters) <- config$parameter %>% map("label")
predefined_order_targetpop <- config$targetPop

order_parameters <- function(params) {
  ordered_parameters <- predefined_order_parameters[which(predefined_order_parameters %in% params)]
}

order_targetpop <- function(targetpops) {
  predefined_order_targetpop[which(predefined_order_targetpop %in% targetpops)]
}



# Datensatz rekodieren
if(language == "en"){
  BTdata <- BTdata %>%
    # 1. alle Spalten übersetzen
    mutate(across(c(cycle, fach, klassenstufe, kb, targetPop), ~ recode(.x, !!! woerterbuch))) %>%
    # 2. 'fachKb' neu erstellen
    mutate(fachKb = paste(fach, kb, sep = "-"))
}

# UI Choices übersetzen
if(language == "en"){
  combinations <- combinations %>%
    mutate(
      # 1. alle "einfachen" Spalten übersetzen
      across(c(cycle, targetPop), ~ recode(.x, !!! woerterbuch)),
      
      # 2. 'fachKb' trennen, übersetzen und neu erstellen
      fachKb = map_chr(fachKb, function(x) {
        parts <- unlist(strsplit(x, "-"))
        translated_parts <- recode(parts, !!! woerterbuch)
        paste(translated_parts, collapse = "-")
      })
    )
  
  available_cycles <- recode(available_cycles, !!! woerterbuch)
  available_parameters <- recode(available_parameters, !!! woerterbuch)
}

if(language == "en"){
  default_newest_cycle <- recode(default_newest_cycle, !!! woerterbuch)
}


# UI ---------------------------------------------------------------------------

ui <- fluidPage(
  
  # Styling --------------------------------------------------------------------
  theme = shinytheme("sandstone"),
  
  # Aussehen des Sliders
  # .irs-grid-pol.small: entfernt die vertikalen Gitternetzlinien zwischen den Ticks
  # .irs-grid-text: Schriftgröße der Labels unterhalb des Sliders
  # .irs-bar.irs-bar--single: Setzt Hintergrund der Slider-Leiste transparent
  
  # Infobutton
  # .fa-info: Anpassungen am i Symbol auf dem Button
  # .custom-btn: Farbe usw. des Infobuttons
  # .popover: Box des Popovers (Breite der Box)
  # .no-padding: custom Klasse für column ohne padding am Rand
  
  tags$style(
    HTML(
      "
        .irs-grid-pol.small {
          height: 0 !important;
        }
        .irs-grid-text {
          font-size: 12px !important;
        }
        .irs-bar.irs-bar--single {
          background: #5342ca00 !important;
          border: none !important;
        }
        .fa-info {
          color: #808080 !important;
        }
        .custom-btn {
          background-color: #A9A9A9;
          color: white;
          border-radius: 5px;
          border: none;
          padding: 0px 0px;
          display: flex;
          align-items: center;
          justify-content: center;
        }
        .popover {
        max-width: 600px !important;
        width: 600px !important;
        }
        .no-padding {
        padding: 0 !important;
        }
      "
    )
  ),
  
  
  # Navigationsfeld links ------------------------------------------------------
  sidebarPanel(
    
    # Erhebungsreihe (Zyklus)
    fluidRow(
      column(
        width = 11, # Spalte nimmt 11/12 des Panels ein
        class = "no-padding", # ohne zusätzlichen Rand
        selectInput(
          inputId = "Zyklus",
          label = i18n$t("Erhebungsreihe"),
          choices = available_cycles,
          selected = default_newest_cycle,
          width = '95%'
        )
      ),
      column(
        width = 1, # Info-Button in einer separaten Spalte
        class = "no-padding",
        tags$div(style = "height: 30px;"), # vertikaler Abstand
        bsButton(
          inputId = "infobutton_zyklus",
          label = "",
          icon = icon("info", lib = "font-awesome"),
          style = "custom-btn",
          size = "extra-small"
        )
      )
    ),
    
    # Kompetenzbereiche
    fluidRow(
      column(
        width = 11, # Spalte für die dynamischen Inhalte
        class = "no-padding",
        uiOutput('dynamicPanel_kompetenzbereiche')
      ),
      column(
        width = 1, # Info-Button in einer separaten Spalte
        class = "no-padding",
        bsButton(
          inputId = "infobutton_kompetenzbereiche",
          label = "",
          icon = icon("info", lib = "font-awesome"),
          style = "custom-btn",
          size = "extra-small"
        )
      )
    ),
    
    # Jahr, Zielpopulation und Kennwert
    fluidRow(
      column(
        width = 11, # Spalte für die dynamischen Inhalte
        class = "no-padding",
        uiOutput('dynamicPanel_JahrZielpopulationKennwert')
      ),
      column(
        width = 1, # Info-Button in einer separaten Spalte
        tags$div(style = "height: 45px;"), # vertikaler Abstand
        class = "no-padding",
        bsButton(
          inputId = "infobutton_jahre",
          label = "",
          icon = icon("info", lib = "font-awesome"),
          style = "custom-btn",
          size = "extra-small"
        ),
        tags$div(style = "height: 60px;"), # vertikaler Abstand
        bsButton(
          inputId = "infobutton_zielpopulation",
          label = "",
          icon = icon("info", lib = "font-awesome"),
          style = "custom-btn",
          size = "extra-small"
        ),
        tags$div(style = "height: 50px;"), # vertikaler Abstand
        bsButton(
          inputId = "infobutton_kennwert",
          label = "",
          icon = icon("info", lib = "font-awesome"),
          style = "custom-btn",
          size = "extra-small"
        )
      )
    ),
    tags$div(style = "height: 20px;"),
    
    # Download-Button ------------------------------------------------------------
    
    fluidRow(
      column(
        width = 12, # Spalte für die dynamischen Inhalte
        class = "no-padding",
        downloadButton("report", " PDF Export",
                       style = "width:100%; margin-top:10px;
                   background-color:#f0f0f0; color: #000000;
                   border: 1px solid #A9A9A9;
                   padding: 3px 8px;
                   height: 30px;")
      )
    )
  ),
  
  # Main-Panel mit der Deutschlandkarte ----------------------------------------
  mainPanel(
    eatMapOutput("deutschlandkarte", width = "100%")
  ),
  
  
  # Texte in den Infobuttons ---------------------------------------------------
  # Info-Button Popover für Zyklus
  bsPopover(
    id = "infobutton_zyklus",
    title = i18n$t("Erhebungsreihe"),
    content = HTML(paste0(infotexte[infotexte$chunk == "Erhebungsreihe", language])),
    placement = "right",
    trigger = "klick",
    options = list(container = "body")
  ),
  
  # Info-Button Popover für Kompetenzbereiche
  bsPopover(
    id = "infobutton_kompetenzbereiche",
    title = i18n$t("Kompetenzbereich"),
    content = HTML(paste0(infotexte[infotexte$chunk == "Kompetenzbereich", language])),
    placement = "right",
    trigger = "klick",
    options = list(container = "body")
  ),
  
  # Info-Button Popover für Jahre
  bsPopover(
    id = "infobutton_jahre",
    title = i18n$t("Jahr"),
    content = HTML(paste0(infotexte[infotexte$chunk == "Jahr", language])),
    placement = "right",
    trigger = "klick",
    options = list(container = "body")
  ),
  
  # Info-Buttons Zielpopulation und Kennwert
  
  bsPopover(
    id = "infobutton_zielpopulation",
    title = i18n$t("Grundgesamtheit"),
    content = HTML(paste0(infotexte[infotexte$chunk == "Zielpopulation", language])),
    placement = "right",
    trigger = "klick",
    options = list(container = "body")
  ),
  
  bsPopover(
    id = "infobutton_kennwert",
    title = i18n$t("Kennwert"),
    content = HTML(paste0(infotexte[infotexte$chunk == "Kennwert", language])),
    placement = "right",
    trigger = "klick",
    options = list(container = "body")
  )
)

# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Eingabevariablen auslesen und zwischenspeichern ----------------------------
  
  # Zyklus (davon hängt ab, welche Eingabefelder dynamisch angezeigt werden)
  selectedZyklus <- reactive({
    input$Zyklus
  })
  
  # Kennwert
  selectedKennwert <- reactive({
    input$Kennwert
  })
  
  # Kompetenzbereich (aus dem dynamischen Panel)
  selectedKompetenzbereich <- reactive({
    input$fachKb
  })
  
  # Jahr (aus dynamischem Panel)
  selectedJahr <- reactive({
    input$Jahr
  })
  
  # Zielpopulation (aus dynamischem panel)
  selectedZielpopulation <- reactive({
    input$Zielpopulation
  })
  
  
  # Dynamisches Auswahlpanel für Kompetenzbereiche generieren ------------------
  output$dynamicPanel_kompetenzbereiche <- renderUI({
    kb_current <- config$fachKb[[selectedZyklus()]]
    make_radioGroupContainer(kb_current)
  })
  
  # Dynamisches Auswahlpanel für Jahr, Kennwert $ Zielpopulation ---------------
  output$dynamicPanel_JahrZielpopulationKennwert <- renderUI({
    make_YearPopulationParameter(selectedZyklus())
  })
  
  
  # Jahr, Zielpopulation & Kennwert jeweils voneinander abhängig ---------------
  observe({
    req(selectedKompetenzbereich())
    
    selected_combinations <- combinations[combinations$cycle == selectedZyklus() &
                                            combinations$fachKb == selectedKompetenzbereich() , ]
    
    zielpopulationen <- order_targetpop(unique(selected_combinations$targetPop))
    # ...abhängig von Zyklus und Fach-Kompetenzbereich
    
    kennwerte <- order_parameters(unique(selected_combinations[selected_combinations$targetPop == selectedZielpopulation() , ]$parameter))
    # ...abhängig von Zyklus, Fach-Kompetenzbereich, und Zielpopulation
    
    jahre <- unique(selected_combinations[selected_combinations$parameter == selectedKennwert() &
                                            selected_combinations$targetPop == selectedZielpopulation(), ]$year)
    # ...abhängig von Zyklus, Fach-Kompetenzbereich, Zielpopulation, und Kennwert
    
    updateSliderTextInput(session,
                          inputId = "Jahr",
                          label = i18n$t("Jahr"),
                          choices = jahre,
                          selected = selectedJahr())
    
    updateSelectInput(session,
                      inputId = "Zielpopulation",
                      label = i18n$t("Zielpopulation"),
                      choices = zielpopulationen,
                      selected = ifelse(selectedZielpopulation() %in% zielpopulationen,
                                        selectedZielpopulation(),
                                        zielpopulationen[1]))
    
    updateSelectInput(session,
                      inputId = "Kennwert",
                      label = i18n$t("Kennwert"),
                      choices = kennwerte,
                      selected = ifelse(selectedKennwert() %in% kennwerte,
                                        selectedKennwert(),
                                        kennwerte[1]))
  })
  
  # Datensatz selektieren ------------------------------------------------------
  # anhand der User-Eingaben
  
  data_selected <- reactive({
    req(selectedKennwert())
    data_selected <- BTdata[ BTdata$cycle == selectedZyklus() &
                               BTdata$parameter == selectedKennwert() &
                               BTdata$year == selectedJahr() &
                               BTdata$fachKb == selectedKompetenzbereich() &
                               BTdata$targetPop == selectedZielpopulation(), ]
    data_selected
  })
  
  # Wähle Minimum und Maximum für die Skala
  config_parameter <- eventReactive(selectedKennwert(), config$parameter[[selectedKennwert()]])
  
  
  
  # Deutschlandkarte -----------------------------------------------------------
  
  output$deutschlandkarte <- renderEatMap({
    req(data_selected(), selectedKompetenzbereich())
    
    data_selected() %>%
      eatMap(data = ., config = config)
  })
  
  # PDF Export -----------------------------------------------------------------
  
  output$report <- downloadHandler(
    
    filename = "IQB_Bildungstrendkarte.pdf",
    content = function(file) {
      
      # Lade-Anzeige (Feedback) während Download vorbereitet wird
      showModal(modalDialog(i18n$t("PDF-Download wird vorbereitet..."), footer=NULL))
      on.exit(removeModal())
      
      # PDF soll in temporäres directory kopiert werden, falls keine Schreibrechte
      # für das aktuelle directory vorliegen
      tempReport <- file.path(tempdir(), "export.Rmd")
      template_path <- system.file("app", "export.Rmd", package = "BTShinyApp")
      file.copy(template_path, tempReport, overwrite = TRUE)
      
      # Quellenangaben einlesen
      sources <- readxl::read_xlsx(system.file("extdata", "text_elements", "BT_Quellenangaben.xlsx", package = "BTShinyApp"))
      
      # Parameter für das .Rmd Dokument
      params <- list(data = right_join(x = mapdata,
                                      y = data_selected(),
                                      by = "Bundesland"),
                     min_est = config_parameter()$range$min,
                     max_est = config_parameter()$range$max,
                     reverse = config_parameter()$reverse,
                     legendentitel = config_parameter()$title,
                     kennwert = input$Kennwert,
                     na_label = config$na_label,
                     quelle = sources[sources$year == selectedJahr(), ]$source)
      
      # Knitten
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  ) 
}


# Build App --------------------------------------------------------------------

shinyApp(ui = ui, server = server)

