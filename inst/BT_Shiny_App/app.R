# Sprache der App --------------------------------------------------------------
# "de" für Deutsch, "en" für Englisch
language <- "de"

if(language != "de" & language != "en") stop("Language selection must be \"de\" or \"en\".")

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
library(tinytex)
library(stringr)

library(bslib)

if (!requireNamespace("eatMap", quietly = TRUE)) {
  remotes::install_github("franikowsp/eatMap")
}
library(eatMap)

if (!requireNamespace("BTShinyApp", quietly = TRUE)) {
  remotes::install_github("iqb-research/BT-ShinyApp")
}
library(BTShinyApp)

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
  
  div(class = "radio-group-container",
      tags$label(class = "radio-group-label", label),  # Überschrift fett
      radioButtons(inputId, NULL, choices, selected, inline = inline)
  )
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

# JSON Übersetzung

json_path <- system.file("extdata", "text_elements", "translation.json", package = "BTShinyApp")
i18n <- shiny.i18n::Translator$new(translation_json_path = json_path)

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

# Texte für die Infobuttons ----------------------------------------------------

infotextfile <- system.file("extdata/text_elements/Infotexte.xlsx", package = "BTShinyApp")
infotexte <- readxl::read_excel(infotextfile)

infotexte_list <- setNames(
  infotexte[[language]],
  infotexte$chunk
)


# UI ---------------------------------------------------------------------------

ui <- fluidPage(
  
  # Styling --------------------------------------------------------------------
  theme = bs_theme(version = 5, bootswatch = "sandstone"),
  
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
        .irs-grid-pol.small {height: 0 !important;}
        .irs-grid-text {font-size: 12px !important;}
        .irs-bar.irs-bar--single {background: #5342ca00 !important; border: none !important;}
        
        #deutschlandkarte svg {
          max-width: 100% !important;
          height: auto !important;
        }
        
        /* Haupt-Label der Inputs fett */
        .form-group > label {
          font-weight: bold;
        }
        
        /* Radiobuttons Überschrift fett */
        .radio-group-container > .radio-group-label {
          font-weight: bold;
          display: block;
          margin-bottom: 5px; /* Abstand zu den Optionsbuttons */
        }
        
        /* Radiobuttons Optionslabels normal */
        .radio-group-container .radio label,
        .radio-group-container .checkbox label {
          font-weight: normal;
        }
                
        /* --- Infobutton Styling --- */
        .custom-btn {
          width: 22px;              
          height: 22px;
          padding: 0;
          background-color: #bfbdbd; 
          color: white;
          border-radius: 4px;        /* leicht abgerundete Ecken, viereckig */
          border: none;
          display: flex;
          align-items: center;
          justify-content: center;
          box-shadow: 2px 2px 5px rgba(0,0,0,0.3);
          transition: all 0.2s ease;
        }
        
        .custom-btn:hover {
          background-color: #495057; /* dunkler beim Hover */
          cursor: pointer;
        }
        
        .custom-btn:active {
          background-color: #343a40; /* noch dunkler beim Klick */
        }
        
        /* Icon anpassen */
        .custom-btn .fa-info {
          font-size: 14px; 
          margin: 0;
        }
        
        .popover {
          max-width: 60vw !important;   /* nur 60% Breite */
          width: auto;                  /* wächst dynamisch */
        }


        @media (max-width: 992px) {
          .container-fluid > .row {
            flex-direction: column;  /* Sidebar oben, Karte unten */
          }
          @media (max-width: 992px) {
          .container-fluid > .row {
            flex-direction: column;  /* Sidebar oben, Karte unten */
          }
          .popover {
            max-width: 90vw !important; /* 90% der Viewport-Breite */
            width: auto;
            }
          }
        }
      "
    )
  ),
  
  # Navigationsfeld links ------------------------------------------------------
  div(class = "container-fluid",
      div(class = "row", 
          div(class = "col-lg-4",
              div(class = "well",
                  # Erhebungsreihe (Zyklus) --------------------------------------------------
                  div(
                    style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
                    
                    # Input links 
                    div(
                      style = "flex-grow:1; min-width:0; padding-right:6px;",
                      selectInput(
                        inputId = "Zyklus",
                        label = i18n$t("Erhebungsreihe"),
                        choices = available_cycles,
                        selected = default_newest_cycle,
                        width = '100%'
                      )
                    ),
                    
                    # Button rechts 
                    div(
                      style = "flex:0 0 auto;",
                      tags$div(style = "height: 15px;"), # vertikaler Abstand
                      actionButton(
                        inputId = "infobutton_zyklus",
                        label = "",
                        icon = icon("info", lib = "font-awesome"),
                        class = "custom-btn",
                        `data-bs-toggle` = "popover",
                        `data-bs-trigger` = "click",  
                        `data-bs-placement` = "right"
                      )
                    )
                  ),
                  
                  # Kompetenzbereiche --------------------------------------------------------
                  div(
                    style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
                    
                    # Input links 
                    div(
                      style = "flex-grow:1; min-width:0; padding-right:6px;",
                      uiOutput('dynamicPanel_kompetenzbereiche')
                    ),
                    
                    # Button rechts
                    div(
                      style = "flex:0 0 auto;",
                      actionButton(
                        inputId = "infobutton_kompetenzbereich",
                        label = "",
                        icon = icon("info", lib = "font-awesome"),
                        class = "custom-btn",
                        `data-bs-toggle` = "popover",
                        `data-bs-trigger` = "click",  
                        `data-bs-placement` = "right"
                      )
                    )
                  ),
                  
                  # Jahr, Zielpopulation und Kennwert ----------------------------------------
                  div(
                    style = "display:flex; align-items:top; justify-content:space-between; width:100%;",
              
                    # Input links 
                    div(
                      style = "flex-grow:1; min-width:0; padding-right:6px;",
                      uiOutput('dynamicPanel_JahrZielpopulationKennwert')
                    ),
                  
                    # Buttons rechts
                    div(
                      style = "flex:0 0 auto;",
                      tags$div(style = "height: 50px;"), # vertikaler Abstand
                      actionButton(
                        inputId = "infobutton_jahr",
                        label = "",
                        icon = icon("info", lib = "font-awesome"),
                        class = "custom-btn",
                        `data-bs-toggle` = "popover",
                        `data-bs-trigger` = "click",  
                        `data-bs-placement` = "right"
                      ),
                      tags$div(style = "height: 75px;"), # vertikaler Abstand
                      actionButton(
                        inputId = "infobutton_zielpopulation",
                        label = "",
                        icon = icon("info", lib = "font-awesome"),
                        class = "custom-btn",
                        `data-bs-toggle` = "popover",
                        `data-bs-trigger` = "click",  
                        `data-bs-placement` = "right"
                      ),
                      tags$div(style = "height: 65px;"), # vertikaler Abstand
                      actionButton(
                        inputId = "infobutton_kennwert",
                        label = "",
                        icon = icon("info", lib = "font-awesome"),
                        class = "custom-btn",
                        `data-bs-toggle` = "popover",
                        `data-bs-trigger` = "click",  
                        `data-bs-placement` = "right"
                      ),
                      tags$div(style = "height: 20px;")
                    )
                  ),
                  
                  # Download-Button ----------------------------------------------------------
                  
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
              )
          ),
          div(
            class = "col-12 col-lg-8",  # volle Breite auf kleinen Geräten
            style = "padding:0; display:flex; justify-content:center;",
            eatMapOutput("deutschlandkarte", width = "100%", height = "auto")
          )
      )
  ),
  
  # JavaScript für Infobutton-Popover
  tags$script(HTML("
  
    // Alle Infobutton-IDs und ihr Inhalt aus infotexte_list
  
    // Popover-Inhalte definieren 
    var popoverContents = {
      '#infobutton_zyklus': `", infotexte_list[["Erhebungsreihe"]], "`,
      '#infobutton_kompetenzbereich': `", infotexte_list[["Kompetenzbereich"]], "`,
      '#infobutton_jahr': `", infotexte_list[["Jahr"]], "`,
      '#infobutton_zielpopulation': `", infotexte_list[["Zielpopulation"]], "`,
      '#infobutton_kennwert': `", infotexte_list[["Kennwert"]], "`
    };
    
    // Popover-Titel definieren 
    var popoverTitles = {
      '#infobutton_zyklus': '", ifelse(language == "en", recode("Erhebungsreihe", !!!woerterbuch), "Erhebungsreihe"), "',
      '#infobutton_kompetenzbereich': '", ifelse(language == "en", recode("Kompetenzbereich", !!!woerterbuch), "Kompetenzbereich"), "',
      '#infobutton_jahr': '", ifelse(language == "en", recode("Jahr", !!!woerterbuch), "Jahr"), "',
      '#infobutton_zielpopulation': '", ifelse(language == "en", recode("Zielpopulation", !!!woerterbuch), "Zielpopulation"), "',
      '#infobutton_kennwert': '", ifelse(language == "en", recode("Kennwert", !!!woerterbuch), "Kennwert"), "'
    };

      
    // Array mit allen Infobutton-IDs
    var buttons = ['#infobutton_zyklus', '#infobutton_kompetenzbereich',
                   '#infobutton_jahr', '#infobutton_zielpopulation', '#infobutton_kennwert'];
  
    // Popovers initialisieren
    buttons.forEach(function(btnId) {
      var btn = document.querySelector(btnId);
      var placement = window.innerWidth <= 992 ? 'bottom' : 'right'; // für kleine Geräte soll der Text unter dem Button auftauchen
      new bootstrap.Popover(btn, {
        html: true,
        trigger: 'manual',
        container: 'body',
        placement: placement,
        fallbackPlacements: [], // keine Alternativen für das Placement zulassen
        boundary: 'viewport',
        title: popoverTitles[btnId],
        content: popoverContents[btnId]
      });
  
      // Click Event
      btn.addEventListener('click', function() {
        // alle anderen Popovers schließen
        buttons.forEach(function(otherId) {
          if(otherId !== btnId) {
            var otherBtn = document.querySelector(otherId);
            bootstrap.Popover.getInstance(otherBtn)?.hide();
          }
        });
  
        // aktuellen Popover toggeln
        var pop = bootstrap.Popover.getInstance(btn);
        pop.toggle();
      });
    });
  
    // Popover schließen, wenn außerhalb geklickt wird
    document.addEventListener('click', function(e) {
      if (!buttons.some(id => document.querySelector(id).contains(e.target))) {
        buttons.forEach(function(btnId) {
          bootstrap.Popover.getInstance(document.querySelector(btnId))?.hide();
        });
      }
    });
  "))
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
  
    #totaler Mist-Hotfix an der falschen Stelle  
    if(any(data_selected()$fach %in% "Französisch")) config$total_label <- "Gesamt"  
    if(any(data_selected()$fach %in% "French")) config$total_label <- "Total"  
    
    data_selected() %>%
      eatMap(data = ., config = config)
  })
  
  # PDF Export -----------------------------------------------------------------
  
  output$report <- downloadHandler(
    
    filename = ifelse(language == "de", "IQB_Bildungstrendkarte.pdf", "IQB_Trends_in_Student_Achievement_Map.pdf"),
    content = function(file) {
      
      # Lade-Anzeige (Feedback) während Download vorbereitet wird
      showModal(modalDialog(i18n$t("PDF-Download wird vorbereitet..."), footer=NULL))
      on.exit(removeModal())
      
      # PDF soll in temporäres directory kopiert werden, falls keine Schreibrechte
      # für das aktuelle directory vorliegen
      tempReport <- file.path(tempdir(), "export.Rmd")
      template_path <- system.file("BT_Shiny_app", "export.Rmd", package = "BTShinyApp")
      file.copy(template_path, tempReport, overwrite = TRUE)
      
      # Quellenangaben einlesen
      sources <- readxl::read_xlsx(system.file("extdata", "text_elements", "BT_Quellenangaben.xlsx", package = "BTShinyApp"))
      
      # Parameter für das .Rmd Dokument
      params <- list(mapdata = mapdata,
                     data_selected = data_selected(),
                     min_est = config_parameter()$range$min,
                     max_est = config_parameter()$range$max,
                     reverse = config_parameter()$reverse,
                     legendentitel = config_parameter()$title,
                     kennwert = input$Kennwert,
                     na_label = config$na_label,
                     quelle = sources[sources$year == selectedJahr(), ]$source,
                     language = language,
                     woerterbuch = woerterbuch)
      
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

