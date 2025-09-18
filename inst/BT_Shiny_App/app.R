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
library(tinytex)
library(stringr)
library(bslib)
library(eatMap)

# if (!requireNamespace("BTShinyApp", quietly = TRUE)) {
#   remotes::install_github("iqb-research/BT-ShinyApp@v0.1.9")
# }
# library(BTShinyApp)


# "data_preparation.R" muss neu ausgeführt werden, wenn Kartendaten neu, 
# BT-Daten neu oder config/Übersetzung neu, sprich eines der Folgenden:

# BT-Daten ---------------------------------------------------------------------
BTdata <- readRDS(system.file("data", "BTdata_processed.Rds", package = "BTShinyApp"))

# Configs for UI ---------------------------------------------------------------
load(system.file("data", "uichoices.RData", package = "BTShinyApp"))




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
                      # selectInput(
                      #   inputId = "Zyklus",
                      #   label = i18n$t("Erhebungsreihe"),
                      #   choices = available_cycles,
                      #   selected = default_newest_cycle,
                      #   width = '100%'
                      # )
                      uiOutput("zyklus_ui")
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
  uiOutput("js_popovers")
  
)

# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  # lang auslesen
  query <- reactive({
    parseQueryString(session$clientData$url_search)
  })
  
  lang <- reactive({
    lng <- query()$lang
    if (!is.null(lng) && lng %in% c("de", "en")) {
      lng
    } else {
      "de"  
    }
  })

  observe({
    i18n$set_translation_language(lang())
  })

  # all solches überarbeiten... #################################################
  output$zyklus_ui <- renderUI({
    selectInput(
      inputId = "Zyklus",
      label = i18n$t("Erhebungsreihe"),
      choices = available_cycles[[lang()]],
      selected = default_newest_cycle[[lang()]],
      width = '100%'
    )
  })
  
  output$js_popovers <- renderUI({
    tags$script(HTML(sprintf("
    var popoverContents = {
      '#infobutton_zyklus': `%s`,
      '#infobutton_kompetenzbereich': `%s`,
      '#infobutton_jahr': `%s`,
      '#infobutton_zielpopulation': `%s`,
      '#infobutton_kennwert': `%s`
    };
    
    // Popover-Titel definieren 
    var popoverTitles = {
      '#infobutton_zyklus': '%s',
      '#infobutton_kompetenzbereich': '%s',
      '#infobutton_jahr': '%s',
      '#infobutton_zielpopulation': '%s',
      '#infobutton_kennwert': '%s'
    };

    // Array mit allen Infobutton-IDs
    var buttons = ['#infobutton_zyklus', '#infobutton_kompetenzbereich',
                   '#infobutton_jahr', '#infobutton_zielpopulation', '#infobutton_kennwert'];
  
    // Popovers initialisieren
    buttons.forEach(function(btnId) {
      var btn = document.querySelector(btnId);
      var placement = window.innerWidth <= 992 ? 'bottom' : 'right';
      new bootstrap.Popover(btn, {
        html: true,
        trigger: 'manual',
        container: 'body',
        placement: placement,
        fallbackPlacements: [],
        boundary: 'viewport',
        title: popoverTitles[btnId],
        content: popoverContents[btnId]
      });
  
      // Click Event
      btn.addEventListener('click', function() {
        buttons.forEach(function(otherId) {
          if(otherId !== btnId) {
            var otherBtn = document.querySelector(otherId);
            bootstrap.Popover.getInstance(otherBtn)?.hide();
          }
        });
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
  ",
    infotexte_list[[lang()]][["Erhebungsreihe"]],
    infotexte_list[[lang()]][["Kompetenzbereich"]],
    infotexte_list[[lang()]][["Jahr"]],
    infotexte_list[[lang()]][["Zielpopulation"]],
    infotexte_list[[lang()]][["Kennwert"]],
    ifelse(lang() == "en", recode("Erhebungsreihe", !!!woerterbuch), "Erhebungsreihe"),
    ifelse(lang() == "en", recode("Kompetenzbereich", !!!woerterbuch), "Kompetenzbereich"),
    ifelse(lang() == "en", recode("Jahr", !!!woerterbuch), "Jahr"),
    ifelse(lang() == "en", recode("Zielpopulation", !!!woerterbuch), "Zielpopulation"),
    ifelse(lang() == "en", recode("Kennwert", !!!woerterbuch), "Kennwert")
    )))
  })
  

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
    req(selectedZyklus())
    kb_current <- config[[lang()]]$fachKb[[selectedZyklus()]]
    make_radioGroupContainer(kb_current)
  })
  
  # Dynamisches Auswahlpanel für Jahr, Kennwert $ Zielpopulation ---------------
  output$dynamicPanel_JahrZielpopulationKennwert <- renderUI({
    req(selectedZyklus())
    make_YearPopulationParameter(selectedZyklus(), config[[lang()]], combinations[[lang()]], 
            lang(), predefined_order_parameters[[lang()]], predefined_order_targetpop[[lang()]])
  })
  
  
  # Jahr, Zielpopulation & Kennwert jeweils voneinander abhängig ---------------
  observe({
    req(selectedKompetenzbereich())
  
    selected_combinations <- combinations[[lang()]][combinations[[lang()]]$cycle == selectedZyklus() &
                                            combinations[[lang()]]$fachKb == selectedKompetenzbereich() , ]
    
    zielpopulationen <- order_targetpop(unique(selected_combinations[[lang()]]$targetPop), predefined_order_targetpop[[lang()]])
    # ...abhängig von Zyklus und Fach-Kompetenzbereich
    
    kennwerte <- order_parameters(unique(selected_combinations[[lang()]][selected_combinations[[lang()]]$targetPop == selectedZielpopulation() , ]$parameter), predefined_order_parameters[[lang()]])
    # ...abhängig von Zyklus, Fach-Kompetenzbereich, und Zielpopulation
    
    jahre <- unique(selected_combinations[[lang()]][selected_combinations[[lang()]]$parameter == selectedKennwert() &
                                            selected_combinations[[lang()]]$targetPop == selectedZielpopulation(), ]$year)
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
    data_selected <- BTdata[[lang()]][ BTdata[[lang()]]$cycle == selectedZyklus() &
                               BTdata[[lang()]]$parameter == selectedKennwert() &
                               BTdata[[lang()]]$year == selectedJahr() &
                               BTdata[[lang()]]$fachKb == selectedKompetenzbereich() &
                               BTdata[[lang()]]$targetPop == selectedZielpopulation(), ]
    data_selected
  })
  
  # Wähle Minimum und Maximum für die Skala
  config_parameter <- eventReactive(selectedKennwert(), config[[lang()]]$parameter[[selectedKennwert()]])
  
  # Deutschlandkarte -----------------------------------------------------------
  
  output$deutschlandkarte <- renderEatMap({
    req(data_selected(), selectedKompetenzbereich(), lang())
  
    #totaler Mist-Hotfix an der falschen Stelle  
    if(any(data_selected()$fach %in% "Französisch")) config[[lang()]]$total_label <- "Gesamt"  
    if(any(data_selected()$fach %in% "French")) config[[lang()]]$total_label <- "Total"  
    
    data_selected() %>%
      eatMap(data = ., config = config[[lang()]])
  })
  
  # PDF Export -----------------------------------------------------------------
  
  output$report <- downloadHandler(

    filename = function() {
      if (lang() == "de") {
        "IQB_Bildungstrendkarte.pdf"
      } else {
        "IQB_Trends_in_Student_Achievement_Map.pdf"
      }     
      
    },
    content = function(file) {
      
      # Lade-Anzeige (Feedback) während Download vorbereitet wird
      showModal(modalDialog(i18n$t("PDF-Download wird vorbereitet..."), footer=NULL))
      on.exit(removeModal())
      
      # PDF soll in temporäres directory kopiert werden, falls keine Schreibrechte
      # für das aktuelle directory vorliegen
      tempReport <- file.path(tempdir(), "export.Rmd")
      template_path <- "export.Rmd"
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
                     na_label = config[[lang()]]$na_label,
                     quelle = sources[sources$year == selectedJahr(), ]$source,
                     language = lang(),
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

