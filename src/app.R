# TODO: Wenn es alles passt, könnt Ihr die Referenzen zu den alten Code-Stellen gern rauslöschen!
# Betroffene Kommentare habe ich kommentiert mit
# TODO: start-delete
# {CODE}
# TODO: end-delete

# Shiny
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)

# HTML
library(widgetframe)

# Plot
library(ggplot2)
# TODO: delete-start
# funktioniert mit der neuesten Version von eatMap
# devtools::install_github("franikowsp/eatMap")
# TODO: delete-end
library(eatMap)
# library(ggplot2) # tidyverse

# Datenselektion
library(tidyverse)

# Geodaten
library(sf)
# Infobuttons
library(shinyBS)


# Vorbereitung der Kartendaten -------------------------------------------------

# https://gadm.org/download_country.html

mapdata <- st_read("gadm41_DEU_shp", layer = "gadm41_DEU_1")
mapdata <- mapdata[, c("NAME_1", "geometry")]
names(mapdata) <- c("Bundesland", "geometry")

# Konfigurationsliste -----------------------------------------------------
# Diese beinhaltet auch implizit die Reihenfolge der entsprechenden Einträge
# Diese Liste wird auch von eatMap verarbeitet! Man muss sie nur an dieser Stelle pflegen und kann sie dann sowohl für
# eatMap als auch für ggplot2 nutzen!
config <-
  list(
    na_label = "keine Daten",
    fachKb = list(
      # 4. Klasse
      "4. Klasse" = list(
        "Deutsch" = c("Lesen",
                      "Zuhören",
                      "Orthografie"),
        "Mathematik" = c("Globalskala",
                         "Zahlen und Operationen",
                         "Größen und Messen",
                         "Raum und Form",
                         "Muster und Strukturen",
                         "Daten, Häufigkeit und Wahrscheinlichkeit")
      ),
      # 9. Klasse: Sprachen
      "9. Klasse: Sprachen" = list(
        "Deutsch" = c("Lesen",
                      "Zuhören",
                      "Orthografie"),
        "Englisch" = c("Leseverstehen",
                       "Hörverstehen"),
        "Französisch" = c("Leseverstehen",
                          "Hörverstehen")
      ),
      # 9. Klasse: Mathe/Nawi
      "9. Klasse: Mathe/Naturwissenschaften" = list(
        "Mathematik" = c("Globalskala",
                         "Zahl",
                         "Messen",
                         "Raum und Form",
                         "Funktionaler Zusammenhang",
                         "Daten und Zufall"),
        "Biologie" = c("Erkenntnisgewinnung",
                       "Fachwissen"),
        "Chemie" = c("Erkenntnisgewinnung",
                     "Fachwissen"),
        "Physik" = c("Erkenntnisgewinnung",
                     "Fachwissen")
      )
    ),
    parameter =
      list(
        "mean" = list(
          label = "Mittelwert",
          title = "Kompetenz-\nmittelwert",
          range = list(min = 405, max = 595),
          reverse = FALSE
        ),
        "sd" = list(
          label = "Streuung",
          title = "Streuung",
          range = list(min = 75, max = 125),
          reverse = TRUE
        ),
        "minVerfehlt" = list(
          label = "Mindeststandard verfehlt (%)",
          title = "Mindeststandard \nverfehlt (%)",
          range = list(min = 5, max = 30),
          reverse = TRUE
        ),
        "minVerfehltESA" = list(
          label = "Mindeststandard für ESA verfehlt (%)",
          title = "Mindeststandard für \nESA verfehlt (%)",
          range = list(min = 0, max = 20),
          reverse = TRUE
        ),
        "minVerfehltMSA" = list(
          label = "Mindeststandard für MSA verfehlt (%)",
          title = "Mindeststandard für \nMSA verfehlt (%)",
          range = list(min = 5, max = 30),
          reverse = TRUE
        ),
        "regErreicht" = list(
          label = "Regelstandard erreicht (%)",
          title = "Regelstandard \nerreicht (%)",
          range = list(min = 40, max = 80),
          reverse = FALSE
        ),
        "regErreichtMSA" = list(
          label = "Regelstandard für MSA erreicht (%)",
          title = "Regelstandard für \nMSA erreicht (%)",
          range = list(min = 30, max = 75),
          reverse = FALSE
        ),
        "optErreicht" = list(
          label = "Optimalstandard erreicht (%)",
          title = "Optimalstandard \nerreicht (%)",
          range = list(min = 0, max = 25),
          reverse = FALSE
        ),
        "optErreichtMSA" = list(
          label = "Optimalstandard für MSA erreicht (%)",
          title = "Optimalstandard für \nMSA erreicht (%)",
          range = list(min = 0, max = 20),
          reverse = FALSE
        )
      ),
    targetPop = c("alle",
                  "alle (zielgleich unterrichtet)",
                  "alle ohne Sonderpädagogischen Förderbedarf",
                  "Mittlerer Schulabschluss (MSA)",
                  "Gymnasium")
  )

# Das ist ein Auszug, der die Färbung deckelt (est_delimited)
# eatMap braucht den eigentlich bereits nicht mehr (läuft intern)
range_check <-
  config$parameter %>%
  map("range") %>%
  enframe(name = "parameter") %>%
  mutate(
    value = map(value, as_tibble)
  ) %>%
  unnest(value)

# Vorbereitung der BT Daten ----------------------------------------------------
#
# TODO: start-delete
# Achtung: Hier kommen jetzt ein paar Schritte von unten hoch - keiner der Schritte muss aus meiner Sicht in einem reactive passieren!
# Ich würde sogar noch einen Schritt weitergehen und nur das Objekt BTdata am Ende in die Shiny-App überreichen, d. h. die Datenaufbereitung
# komplett extern laufen zu lassen?
# TODO: end-delete
load("data/allDat.RData")

allDatRec <-
  allDat %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    # Rekodierung: full -> zielgleich bei allen Standards
    targetPop = ifelse(parameter %in% c("minVerfehlt",
                                        "regErreicht",
                                        "optErreicht",
                                        "minVerfehltESA",
                                        "minVerfehltMSA",
                                        "optErreichtMSA",
                                        "regErreichtMSA") &
                         targetPop == "full",
                       "zielgleich",
                       targetPop),
    # Zielpopulation
    targetPop = dplyr::recode(
      targetPop,
      "full" = "alle",
      "zielgleich" = "alle (zielgleich unterrichtet)",
      "nonSPF" = "alle ohne Sonderpädagogischen Förderbedarf",
      "MSA" = "Mittlerer Schulabschluss (MSA)",
      "Gymnasium" = "Gymnasium"
    ),
    # Ausschreiben Zyklus
    cycle = dplyr::recode(
      cycle,
      "9. Klasse: Mathe/Nawi" = "9. Klasse: Mathe/Naturwissenschaften"
    ),
    # Umlaute
    TR_BUNDESLAND = dplyr::recode(
      TR_BUNDESLAND,
      "Thueringen" = "Thüringen",
      "Baden-Wuerttemberg" = "Baden-Württemberg"
    )
  ) %>%
  dplyr::rename(Bundesland = TR_BUNDESLAND)

print_percent <- function(x) {
  # Anteil in Prozentwert umwandeln und "%" anhängen
  x_perc <- paste0(x, "%")
  
  # Korrektur, falls NAs zu Strings geworden sind
  ifelse(x_perc == "NA%", NA_character_, x_perc)
}

BTdata <-
  allDatRec %>%
  mutate(
    # die Spalte "est" enthält die tatsächlichen Kompetenzmittelwerte
    # ...diese sollen aber so nicht im Hover-Effekt eingetragen werden, sondern
    # zur leichteren Interpretierbarkeit gerundet werden -> neue Spalte
    est = case_when(
      parameter %in% c("mean", "sd") ~ est,
      # Anteil bereits in Prozentwert umgerechnet
      .default = est * 100
    ),
    se = case_when(
      parameter %in% c("mean", "sd") ~ se,
      # Anteil bereits in Prozentwert umgerechnet
      .default = se * 100
    ),
    est_print = case_when(
      parameter %in% c("mean", "sd") ~ paste0(round(est, 0)),
      .default = print_percent(est)
    ),
    # NAs sollen außerdem als "keine Daten" beschriftet werden
    est_print = ifelse(is.na(est_print), config$na_label, est_print)
  ) %>%
  left_join(range_check) %>%
  mutate(
    # Werte, die < min_est sind, sollen farblich als min_est und
    # Werte, die > max_est sind, sollen farblich als max_est eingetragen werden
    est_delimited = case_when(
      est < min ~ min,
      est > max ~ max,
      .default = est
    ),
    fachKb = str_glue("{fach}-{kb}")
  )

rm(allDat, allDatRec)

# BT Daten und Kartendaten werden erst zusammengefügt, nachdem der BT Datensatz
# im Server selektiert wurde

# UI Choices -------------------------------------------------------------------

# Zyklen
available_cycles <- unique(BTdata$cycle)[order(unique(BTdata$cycle))] # alle erhobenen Zyklen, Hotfix für Ordnung
available_parameters <- unique(BTdata$parameter) # Parameter

# alle im Datensatz enthaltenen Kombinationen der UI choices
combinations <-
  BTdata %>%
  distinct(cycle, fachKb, year, parameter, targetPop)

# Vorbereitung der grouped radio buttons ---------------------------------------
# TODO: start-delete
# Ich habe hier das zweite id in parentId umgeändert, damit das ggf. noch ein wenig klarer wird, was das ist?
# TODO: end-delete
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

# TODO: start-delete
# # Zum Gegenprüfen
# kb_current <- config$fachKb[["4. Klasse"]]
# subject = "Deutsch"
# n_subject = 1
# # Funktion ersetzt dann alle diese Knoten
# column(
#   12,
#   radioSubgroup(
#     "Kompetenzbereich",
#     "Deutsch",
#     label = "Deutsch:",
#     choices = kb_GS_deutsch,
#     selected = "Deutsch-Lesen"
#   )
# )
# TODO: end-delete
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
make_YearParameterPopulation <- function(cycle_current) {
  # 1. Kb separieren
  fachKb1 <- config$fachKb[[cycle_current]][1]
  fach1 <- names(fachKb1)
  fachKb_default <- str_glue("{fach1}-{fachKb1[[1]][1]}")
  
  selected_combinations <- combinations[combinations$cycle == cycle_current &
                                          combinations$fachKb == fachKb_default, ]
  
  targetPop_default <- "alle"
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
                    label = "Jahr",
                    grid = TRUE,
                    choices = years,
                    selected = year_default,
                    hide_min_max = TRUE,
                    width='75%'),
    
    selectInput(
      inputId = "Kennwert",
      label = "Kennwert",
      choices = parameters,
      selected = parameter_default,
      width = '95%'
    ),
    
    selectInput(
      inputId = "Zielpopulation",
      label = "Zielpopulation",
      choices = targetPops,
      selected = targetPop_default,
      width = '95%'
    )
  )
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
          label = "Erhebungsreihe",
          choices = available_cycles,
          selected = "9. Klasse: Sprachen",
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
    
    # Jahr, Kennwert und Zielpopulation
    fluidRow(
      column(
        width = 11, # Spalte für die dynamischen Inhalte
        class = "no-padding",
        uiOutput('dynamicPanel_JahrKennwertZielpopulation')
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
          inputId = "infobutton_kennwert",
          label = "",
          icon = icon("info", lib = "font-awesome"),
          style = "custom-btn",
          size = "extra-small"
        ),
        tags$div(style = "height: 50px;"), # vertikaler Abstand
        bsButton(
          inputId = "infobutton_zielpopulation",
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
    title = "Erhebungsreihe",
    content = HTML(paste0(
      "Im Rahmen des <a href=\"http://www.kmk.org/bildung-schule/qualitaetssicherung-in-schulen/bildungsmonitoring/ueberblick-gesamtstrategie-zum-bildungsmonitoring.html\" target=\"_blank\">Bildungsmonitorings</a> überprüft das Institut zur Qualitätsentwicklung im Bildungswesen (IQB) im Auftrag der Kultusministerkonferenz regelmäßig, inwieweit die Lernziele erreicht werden, die in den <a href=\"https://www.kmk.org/themen/qualitaetssicherung-in-schulen/bildungsstandards.html\" target=\"_blank\">Bildungsstandards</a> definiert wurden. <br><br>Diese Erhebungen führt das IQB in verschiedenen Reihen durch: Sie heißen \"IQB-Bildungstrends\". Alle Ergebnisberichte sind mit umfassenden Zusatzinformationen <a href=\"https://www.iqb.hu-berlin.de/bt/\" target=\"_blank\">hier</a> frei verfügbar. Im Primarbereich (Grundschule) wird das Erreichen der Bildungsstandards in Deutsch und Mathematik alle fünf Jahre überprüft. In der Sekundarstufe I wechseln sich alle drei Jahre Testungen in Deutsch, Englisch und Französisch mit solchen in Mathematik, Biologie, Chemie und Physik ab."
    )),
    placement = "right",
    trigger = "klick",
    options = list(container = "body")
  ),
  
  # Info-Button Popover für Kompetenzbereiche
  bsPopover(
    id = "infobutton_kompetenzbereiche",
    title = "Kompetenzbereich",
    content = HTML(paste0(
      "Bundesweit geltende Bildungsstandards, die von der Kultusministerkonferenz zu verschiedenen Schulfächern verabschiedet wurden, können <a href=\"https://www.iqb.hu-berlin.de/bista/subject/\" target=\"_blank\">hier</a> eingesehen werden. <strong>Kompetenzbereiche</strong> sind Teilbereiche dieser Schulfächer.<br><br> Die <a href=\"https://www.iqb.hu-berlin.de/bista/subject/\" target=\"_blank\">Bildungsstandards</a> beschreiben, welche Fertigkeiten Schüler:innen in verschiedenen Schulfächern und deren Teilbereichen bis zu einem bestimmten Zeitpunkt in ihrer Schullaufbahn entwickelt haben sollen."
    )),
    placement = "right",
    trigger = "klick",
    options = list(container = "body")
  ),
  
  # Info-Button Popover für Jahre
  bsPopover(
    id = "infobutton_jahre",
    title = "Jahr",
    content = HTML(paste0(
      "Die Erhebungsjahre richten sich nach der Systematik der oben beschriebenen Erhebungsreihen.<br><br>Besonderheiten in einzelnen Jahren:<br><ul><li><strong>2009</strong>: Es wurden keine Schüler:innen mit sonderpädagogischem Förderbedarf (SPF) getestet. Daher ist die Zielpopulation „alle (zielgleich)“ nicht direkt mit den Werten von 2015 und 2022 vergleichbar. Genauere Informationen finden sich in den Berichtsbänden, insbesondere <a href=\"https://www.iqb.hu-berlin.de/bt/BT2022/Bericht/\" target=\"_blank\">hier</a>.</li> <li><strong>2011</strong>: In der Grundschule wurde Orthografie nicht getestet. Weitere Einzelheiten sind im <a href=\"https://www.iqb.hu-berlin.de/bt/BT2021/Bericht/\" target=\"_blank\">Bericht</a> nachzulesen.</li> <li><strong>2021</strong>: Pandemiebedingte Schulschließungen in Mecklenburg-Vorpommern führten zu einem unzureichenden Datensatz. Daher können keine belastbaren Aussagen getroffen werden, und anstelle von Werten wird „keine Daten“ angezeigt.</li></ul>"
    )),
    placement = "right",
    trigger = "klick",
    options = list(container = "body")
  ),
  
  # Info-Buttons Kennwert und Zielpopulation
  bsPopover(
    id = "infobutton_kennwert",
    title = "Kennwert",
    content = HTML(paste0(
      "Ein <strong>Mittelwert</strong> gibt die durchschnittlich erreichten Kompetenzwerte einer bestimmten Gruppe oder Untergruppe an.<br>Die Skala (z.B. Kompetenzwert für Deutsch Lesen) wurde so festgelegt, dass die Grundgesamtheit (z.B. alle Viertklässler:innen im Jahr 2011) einen Mittelwert von 500 hat.<br><br>Eine <strong>Standardabweichung (Streuung)</strong> gibt an, wie stark die Kompetenzen innerhalb einer Gruppe variieren. Sie beschreibt, ob die Verteilung der Kompetenzwerte eher einheitlich (homogen, niedrige Werte) oder unterschiedlich (heterogen, hohe Werte) ist.<br>Die Skala (z.B. Kompetenzwert für Deutsch Lesen) wurde so festgelegt, dass die Grundgesamtheit (z.B. alle Viertklässler:innen im Jahr 2011) eine Standardabweichung von 100 hat.<br><br>Die Kompetenzstufenmodelle beinhalten Beschreibungen, welche Anforderungen Schüler:innen zu bestimmten Zeitpunkten ihrer Schullaufbahn mindestens, in der Regel und optimalerweise erreichen sollten.<br>Die Prozentangabe unter <strong>„Mindeststandard verfehlt“</strong> gibt an, welcher Anteil der Schüler:innen die grundlegenden Anforderungen nicht erfüllt.<br>Die Angabe <strong>„Regelstandard erreicht“</strong> beschreibt den Anteil der Schüler:innen, die die erwarteten Anforderungen erfüllen, während <strong>„Optimalstandard erreicht“</strong> den Anteil nennt, der die höchsten Anforderungen erfüllt.<br><br>Die Abkürzungen <strong>ESA</strong> und <strong>MSA</strong> stehen dabei für den Ersten Schulabschluss (früher Hauptschulabschluss) bzw. den Mittleren Schulabschluss.<br><br>Weitere Informationen: <a href=\"https://datatab.de/tutorial/mittelwert-median-modus\" target=\"_blank\">Mittelwert</a>, <a href=\"https://datatab.de/tutorial/standardabweichung\" target=\"_blank\">Standardabweichung</a> und <a href=\"https://www.iqb.hu-berlin.de/bista/ksm/\" target=\"_blank\">Standards der Kompetenzstufenmodelle</a>"
    )),
    placement = "right",
    trigger = "klick",
    options = list(container = "body")
  ),
  
  bsPopover(
    id = "infobutton_zielpopulation",
    title = "Grundgesamtheit",
    content = HTML(paste0(
      "In den IQB-Bildungstrend-Studien sollen alle Schüler:innen der 4. und 9. Jahrgangsstufe in Deutschland beschrieben werden. Dazu werden Stichproben aus der sogenannten <a href=\"https://datatab.de/tutorial/hypothesentest\" target=\"_blank\">Grundgesamtheit</a> gezogen, auch Population oder <strong>Zielpopulation</strong> genannt. Diese umfasst alle Schüler:innen, über die mit dem jeweiligen Studienergebnis Aussagen getroffen werden sollen. Welche Kennwerte und welche Subpopulationen aus welchen Gründen in den einzelnen Erhebungsreihen enthalten sind, kann den jeweiligen <a href=\"https://www.iqb.hu-berlin.de/bt/\" target=\"_blank\">Berichtsbänden</a> entnommen werden.<br><br>Die Angabe <strong>„alle“</strong> umfasst alle Schüler:innen an allen Schularten, einschließlich Schüler:innen mit Sonderpädagogischem Förderbedarf (SPF). Ausgenommen sind nur Förderschüler:innen im Bereich „geistige Entwicklung“ und Schüler:innen, die weniger als ein Jahr in deutscher Sprache unterrichtet wurden.<br><br>Bei der Angabe <strong>„alle (zielgleich unterrichtet)“</strong> wird unterschieden, ob Schüler:innen gemäß den Regelungen des jeweiligen Landes zielgleich und somit auf Grundlage der Bildungsstandards unterrichtet werden. Das Erreichen der Bildungsstandards wird nur für zielgleich unterrichtete Schüler:innen berichtet.<br><br>Zudem werden in den IQB-Bildungstrend-Studien auch Ergebnisse nur für Schüler:innen, die mindestens den <strong>Mittleren Schulabschlusses (MSA)</strong> anstreben, und Schüler:innen an <strong>Gymnasien</strong> separat ausgewiesen."
    )),
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
    # TODO: start-delete
    # # Ist nun immer zusammengeschrieben und auch im BTdata-Objekt verfügbar
    # req(input$Kompetenzbereich)
    # parts <- unlist(strsplit(input$Kompetenzbereich, "-"))
    # list(fach = parts[1], kb = parts[2])
    # TODO: end-delete
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
  output$dynamicPanel_JahrKennwertZielpopulation <- renderUI({
    make_YearParameterPopulation(selectedZyklus())
  })
  
  
  # Jahr, Kennwert & Zielpopulation jeweils voneinander abhängig ---------------
  observe({
    req(selectedKompetenzbereich())
    
    selected_combinations <- combinations[combinations$cycle == selectedZyklus() &
                                            combinations$fachKb == selectedKompetenzbereich() , ]
    
    kennwerte <- order_parameters(unique(selected_combinations$parameter))
    # ...abhängig von Zyklus und Fach-Kompetenzbereich
    
    zielpopulationen <- order_targetpop(unique(selected_combinations[selected_combinations$parameter == selectedKennwert() , ]$targetPop))
    # ...abhängig von Zyklus, Fach-Kompetenzbereich, und Kennwert
    
    jahre <- unique(selected_combinations[selected_combinations$parameter == selectedKennwert() &
                                            selected_combinations$targetPop == selectedZielpopulation(), ]$year)
    # ...abhängig von Zyklus, Fach-Kompetenzbereich, Kennwert, und Zielpopulation
    
    updateSliderTextInput(session,
                          inputId = "Jahr",
                          label = "Jahr",
                          choices = jahre,
                          selected = selectedJahr())
    
    updateSelectInput(session,
                      inputId = "Kennwert",
                      label = "Kennwert",
                      choices = kennwerte,
                      selected = selectedKennwert())
    
    updateSelectInput(session,
                      inputId = "Zielpopulation",
                      label = "Zielpopulation",
                      choices = zielpopulationen,
                      selected = ifelse(selectedZielpopulation() %in% zielpopulationen,
                                        selectedZielpopulation(),
                                        zielpopulationen[1]))
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
      showModal(modalDialog("PDF-Download wird vorbereitet...", footer=NULL))
      on.exit(removeModal())
      
      # PDF soll in temporäres directory kopiert werden, falls keine Schreibrechte
      # für das aktuelle directory vorliegen
      tempReport <- file.path(tempdir(), "export.Rmd")
      file.copy("export.Rmd", tempReport, overwrite = TRUE)
      
      # Parameter für das .Rmd Dokument
      params <- list(data = left_join(x = mapdata,
                                      y = data_selected(),
                                      by = "Bundesland"),
                     min_est = config_parameter()$range$min,
                     max_est = config_parameter()$range$max,
                     reverse = config_parameter()$reverse,
                     legendentitel = config_parameter()$title,
                     kennwert = input$Kennwert,
                     na_label = config$na_label)
      
      # Knitten
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  ) # output$report
}


# Build App --------------------------------------------------------------------

shinyApp(ui = ui, server = server)


#rsconnect::deployApp('C:\\Users\\alina\\Desktop\\IQB\\BT-Shiny-Minimalloesung')