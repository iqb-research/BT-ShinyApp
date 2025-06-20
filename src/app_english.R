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
library(ggiraph)
# TODO: delete-start
# funktioniert mit der neuesten Version von eatMap
# TODO: delete-end
library(eatMap)
# library(ggplot2) # tidyverse

# Datenselektion
library(tidyverse)

# Geodaten
library(sf)
# Infobuttons
library(shinyBS)

#setwd("/../BT-Shiny-Minimalloesung")

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
    na_label = "no data",
    fachKb = list(
      # 4. Klasse
      "4th Grade" = list(
        "German" = c("Reading",
                      "Listening",
                      "Orthography"),
        "Mathematics" = c("Global Scale",
                         "Numbers and Operations",
                         "Sizes and Measurements",
                         "Space and Shape",
                         "Patterns and Structures",
                         "Data, Frequency, and Probability")
      ),
      # 9. Klasse: Languages
      "9th Grade: Languages" = list(
        "German" = c("Reading",
                      "Listening",
                      "Orthography"),
        "English" = c("Reading Comprehension",
                      "Listening Comprehension"),
        "French" = c("Reading Comprehension",
                     "Listening Comprehension")
      ),
      # 9. Klasse: Mathe/Nawi
      "9th Grade: Math/Science" = list(
        "Mathematics" = c("Global Scale",
                         "Numbers",
                         "Measurements",
                         "Space and Shape",
                         "Functional Relations",
                         "Data and Chance"),
        "Biology" = c("Scientific Inquiry",
                      "Subject Knowledge"),
        "Chemistry" = c("Scientific Inquiry",
                        "Subject Knowledge"),
        "Physics" = c("Scientific Inquiry",
                      "Subject Knowledge")
      )
    ),
    parameter =
      list(
        "mean" = list(
          label = "Mean",
          title = "Mean Proficiency",
          range = list(min = 405, max = 595),
          reverse = FALSE
        ),
        "sd" = list(
          label = "Dispersion",
          title = "Dispersion",
          range = list(min = 75, max = 125),
          reverse = TRUE
        ),
        "minVerfehlt" = list(
          label = "Minmum Standard missed (%)",
          title = "Minmum Standard \nmissed (%)",
          range = list(min = 5, max = 30),
          reverse = TRUE
        ),
        "minVerfehltESA" = list(
          label = "Minmum Standard for ESA missed (%)",
          title = "Minmum Standard for ESA \nmissed (%)",
          range = list(min = 0, max = 20),
          reverse = TRUE
        ),
        "minVerfehltMSA" = list(
          label = "Minmum Standard for MSA missed (%)",
          title = "Minmum Standard for MSA \nmissed (%)",
          range = list(min = 5, max = 30),
          reverse = TRUE
        ),
        "regErreicht" = list(
          label = "Normative Standard met (%)",
          title = "Normative Standard \nmet (%)",
          range = list(min = 40, max = 80),
          reverse = FALSE
        ),
        "regErreichtMSA" = list(
          label = "Normative Standard for MSA met (%)",
          title = "Normative Standard for MSA \nmet (%)",
          range = list(min = 30, max = 75),
          reverse = FALSE
        ),
        "optErreicht" = list(
          label = "Optimal Standard met (%)",
          title = "Optimal Standard \nmet (%)",
          range = list(min = 0, max = 25),
          reverse = FALSE
        ),
        "optErreichtMSA" = list(
          label = "Optimal Standard for MSA met (%)",
          title = "Optimal Standard for MSA \nmet (%)",
          range = list(min = 0, max = 20),
          reverse = FALSE
        )
      ),
    targetPop = c("all",
                  "all (taught on the basis of educational standards)",
                  "all without special educational needs",
                  "secondary school leaving certificate (MSA)",
                  "college-preparatory secondary schools [Gymnasien]")
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
      "full" = "all",
      "zielgleich" = "all (taught on the basis of educational standards)",
      "nonSPF" = "all without special educational needs",
      "MSA" = "secondary school leaving certificate (MSA)",
      "Gymnasium" = "college-preparatory secondary schools [Gymnasien]"
    ),
    # Ausschreiben Zyklus
    cycle = dplyr::recode(
      cycle,
      "4. Klasse" = "4th Grade",
      "9. Klasse: Sprachen" = "9th Grade: Languages",
      "9. Klasse: Mathe/Nawi" = "9th Grade: Math/Science"
    ),
    # Fach
    fach = dplyr::recode(
      fach,
      "Biologie" = "Biology",
      "Chemie" = "Chemistry",
      "Deutsch" = "German",
      "Englisch" = "English",
      "Französisch" = "French",
      "Mathematik" = "Mathematics",
      "Physik" = "Physics"
    ),
    # Kompetenzbereiche
    kb = dplyr::recode(
      kb,
      "Daten und Zufall" = "Data and Chance",
      "Daten, Häufigkeit und Wahrscheinlichkeit" = "Data, Frequency, and Probability",
      "Erkenntnisgewinnung" = "Scientific Inquiry",
      "Fachwissen" = "Subject Knowledge",
      "Funktionaler Zusammenhang" = "Functional Relations",
      "Globalskala" = "Global Scale",
      "Größen und Messen" = "Sizes and Measurements",
      "Hörverstehen" = "Listening Comprehension",
      "Lesen" = "Reading",
      "Leseverstehen" = "Reading Comprehension",
      "Messen" = "Measurements",
      "Muster und Strukturen" = "Patterns and Structures",
      "Orthografie" = "Orthography",
      "Raum und Form" = "Space and Shape",
      "Zahl" = "Numbers",
      "Zahlen und Operationen" = "Numbers and Operations",
      "Zuhören" = "Listening"
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
    est_print = ifelse(is.na(est_print), "no data", est_print)
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
# kb_current <- config$fachKb[["4th Grade"]]
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
make_YearPopulationParameter <- function(cycle_current) {
  # 1. Kb separieren
  fachKb1 <- config$fachKb[[cycle_current]][1]
  fach1 <- names(fachKb1)
  fachKb_default <- str_glue("{fach1}-{fachKb1[[1]][1]}")

  selected_combinations <- combinations[combinations$cycle == cycle_current &
                                          combinations$fachKb == fachKb_default, ]

  targetPop_default <- "all"
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
                    label = "Year",
                    grid = TRUE,
                    choices = years,
                    selected = year_default,
                    hide_min_max = TRUE,
                    width='75%'),

    selectInput(
      inputId = "Zielpopulation",
      label = "Target Population",
      choices = targetPops,
      selected = targetPop_default,
      width = '95%'
    ),

    selectInput(
      inputId = "Kennwert",
      label = "Parameter",
      choices = parameters,
      selected = parameter_default,
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

  # Navigationsbalken Weibseiten-Kopf: -----------------------------------------
  navbarPage(title = "IQB Trends in Student Achievement - Map"),

  # Navigationsfeld links ------------------------------------------------------
  sidebarPanel(

    # Erhebungsreihe (Zyklus)
    fluidRow(
      column(
        width = 11, # Spalte nimmt 11/12 des Panels ein
        class = "no-padding", # ohne zusätzlichen Rand
        selectInput(
          inputId = "Zyklus",
          label = "Assessment Cycle",
          choices = available_cycles,
          selected = "9th Grade: Languages",
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
    )
  ),

  # Main-Panel mit der Deutschlandkarte ----------------------------------------
  mainPanel(
    eatMapOutput("deutschlandkarte", width = "100%")
  ),
  # TODO: delete-start
  # girafeOutput("deutschlandkarte", width = '100%')),
  # TODO: delete-end

  # Texte in den Infobuttons ---------------------------------------------------
  # Info-Button Popover für Zyklus
  bsPopover(
    id = "infobutton_zyklus",
    title = "Assessment Cycle",
    content = HTML(paste0(
      "As part of the <a href=\"http://www.kmk.org/bildung-schule/qualitaetssicherung-in-schulen/bildungsmonitoring/ueberblick-gesamtstrategie-zum-bildungsmonitoring.html\" target=\"_blank\">educational monitoring</a>, the Institute for Educational Quality Improvement (IQB) regularly reviews the extent to which the learning objectives defined in the <a href=\"https://www.kmk.org/themen/qualitaetssicherung-in-schulen/bildungsstandards.html\" target=\"_blank\">national educational standards</a> are achieved, on behalf of the Standing Conference of the Ministers of Education and Cultural Affairs of the States in the Federal Republic of Germany (KMK).<br><br>The IQB conducts these surveys in various periods: They are called \"IQB Trends in Student Achievement\". All results reports are freely available <a href=\"https://www.iqb.hu-berlin.de/bt/\" target=\"_blank\">here</a> with comprehensive additional information. At primary level (elementary school), achievement of the educational standards in German and mathematics is assessed every five years. At secondary level I, tests in German, English and French alternate with tests in mathematics, biology, chemistry and physics every three years."
    )),
    placement = "right",
    trigger = "klick",
    options = list(container = "body")
  ),

  # Info-Button Popover für Kompetenzbereiche
  bsPopover(
    id = "infobutton_kompetenzbereiche",
    title = "Proficiency Domains",
    content = HTML(paste0(
      "Nationwide educational standards adopted by the Standing Conference of the Ministers of Education and Cultural Affairs of the States in the Federal Republic of Germany (KMK) for various school subjects can be viewed <a href=\"https://www.iqb.hu-berlin.de/bista/subject/\" target=\"_blank\">here</a>. The <strong>proficiency domains</strong> are sub-areas of these school subjects.<br><br>The <a href=\"https://www.iqb.hu-berlin.de/bista/subject/\" target=\"_blank\">educational standards</a> describe which skills students should have developed in various school subjects and their sub-areas by a certain point in their school career."
    )),
    placement = "right",
    trigger = "klick",
    options = list(container = "body")
  ),

  # Info-Button Popover für Jahre
  bsPopover(
    id = "infobutton_jahre",
    title = "Year",
    content = HTML(paste0(
      "The survey years are based on the system of the survey periods described above.<br><br>Special features in individual years:<br><ul><li><strong>2009</strong>: No students with special educational needs were tested. Therefore, the target population “all (taught on the basis of educational standards)” is not directly comparable with the values from 2015 and 2022. More detailed information can be found in the report volumes, in particular <a href=\"https://www.iqb.hu-berlin.de/bt/BT2022/Bericht/\" target=\"_blank\">here</a>.</li><li><strong>2011</strong>: Orthography was not tested in elementary school. Further details can be found in the <a href=\"https://www.iqb.hu-berlin.de/bt/BT2021/Bericht/\" target=\"_blank\">report</a>.</li><li><strong>2021</strong>: Pandemic-related school closures in Mecklenburg-Vorpommern led to an insufficient data set. Therefore, no reliable statements can be made and “no data” is displayed instead of values.</li></ul>"
    )),
    placement = "right",
    trigger = "klick",
    options = list(container = "body")
  ),

  # Info-Buttons Zielpopulation und Kennwert
  bsPopover(
    id = "infobutton_zielpopulation",
    title = "Population",
    content = HTML(paste0(
      "The IQB Trends in Student Achievement studies aim to describe all students in the 4th and 9th grades in Germany. For this purpose, samples are drawn from the so-called <a href=\"https://datatab.de/tutorial/hypothesentest\" target=\"_blank\">target population</a>. This includes all students that we want to make statements about with the respective study result. Which parameters and which subpopulations are included in the individual assessment cycles and for what reasons can be found in the respective <a href=\"https://www.iqb.hu-berlin.de/bt/\" target=\"_blank\">report volumes</a>.<br><br>The specification <strong>“all”</strong> includes all students at all types of schools, including students with special educational needs. The only exceptions are special needs students in the area of “mental development” and students who have been taught in German for less than one year. The information <strong>“all (taught on the basis of educational standards)”</strong> distinguishes whether students are taught \"zielgleich\" meaning on the basis of the same standard curriculum. Achievement of the educational standards is only reported for students taught in that way.<br><br>In addition, the IQB Trends in Student Achievement studies also show results separately only for students who are aiming for at least the <strong>secondary school leaving certificate (MSA)</strong> and students at <strong>college-preparatory secondary schools (Gymnasien)</strong>."
    )),
    placement = "right",
    trigger = "klick",
    options = list(container = "body")
  ),

  bsPopover(
    id = "infobutton_kennwert",
    title = "Parameter",
    content = HTML(paste0(
      "The <strong>mean</strong> indicates the average proficiency score achieved by a specific group or subgroup.<br>The scale (e.g., proficiency score for German reading) was defined so that the population (e.g., all fourth graders in 2011) has a mean of 500.<br><br>The <strong>standard deviation (dispersion)</strong> indicates the extent to which proficiencies vary within a group. It describes whether the distribution of proficiencies scores is more uniform (homogeneous, low values) or different (heterogeneous, high values).<br>The scale (e.g., proficiencies score for German reading) was defined so that the population (e.g., all fourth graders in 2011) has a standard deviation of 100.<br><br>The proficiency level models contain descriptions of the requirements that students should achieve at least, generally and ideally at certain points in their school career.<br>The percentage under <strong>“minimum standard missed”</strong> indicates the proportion of students who failed to meet the basic requirements.<br><strong>“Regular standard met”</strong> describes the proportion of students who meet the expected requirements, while <strong>“optimal standard met”</strong> indicates the proportion who meet the highest requirements.<br><br>The abbreviations <strong>ESA</strong> and <strong>MSA</strong> stand for \"Erster Schulabschluss\" (engl. first school leaving certificate, formerly Hauptschulabschluss) and \"Mittlerer Schulabschluss\" (engl. secondary school leaving certificate) respectively.<br><br>Additional information: <a href=\"https://datatab.de/tutorial/mittelwert-median-modus\" target=\"_blank\">Mean</a>, <a href=\"https://datatab.de/tutorial/standardabweichung\" target=\"_blank\">standard deviation</a>, and <a href=\"https://www.iqb.hu-berlin.de/bista/ksm/\" target=\"_blank\">standards for the proficiency level models</a>."
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

  # Dynamisches Auswahlpanel für Jahr, Zielpopulation & Kennwert ---------------
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
                          label = "Year",
                          choices = jahre,
                          selected = selectedJahr())
    
    updateSelectInput(session,
                      inputId = "Zielpopulation",
                      label = "Population",
                      choices = zielpopulationen,
                      selected = ifelse(selectedZielpopulation() %in% zielpopulationen,
                                        selectedZielpopulation(),
                                        zielpopulationen[1]))
    
    updateSelectInput(session,
                      inputId = "Kennwert",
                      label = "Parameter",
                      choices = kennwerte,
                      selected = ifelse(selectedKennwert() %in% kennwerte,
                                        selectedKennwert(),
                                        kennwerte[1]))
  })

  # Datensatz selektieren ------------------------------------------------------


  # Datensatz anhand der User-Eingaben selektieren
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
  # TODO: delete-start
  # # kann nun raus, eatMap hat nun einen Fallback
  #
  # output$deutschlandkarte <- renderGirafe({
  #
  #   # mit Kartendaten zusammenfügen
  #   data <- left_join(x = mapdata,
  #                     y = data_selected(),
  #                     by = "Bundesland")
  #
  #   # Legende ------------------------------------------------------------------
  #
  #   # Range der Legende
  #   min_est <- config_parameter()$range$min
  #   max_est <- config_parameter()$range$max
  #
  #   # Legendentitel
  #   legendentitel <- config_parameter()$title
  #
  #   # Achseninversion
  #   reverse <- config_parameter()$reverse
  #
  #   # Plot ---------------------------------------------------------------------
  #
  #   gg <-
  #     ggplot(data) +
  #     geom_sf_interactive(aes(
  #       fill = est_delimited,
  #       tooltip = sprintf("%s<br/>%s", Bundesland, est_print),
  #       data_id = Bundesland
  #     ),
  #     color = "black") +
  #     theme_void() +
  #     scale_fill_viridis_c(
  #       limits = c(min_est, max_est),
  #       direction = ifelse(reverse, -1, 1), # hier sind große Zahlen schlecht
  #       name = legendentitel,
  #       guide = guide_colorbar(reverse = reverse)
  #     ) +
  #     labs(fill = input$Kennwert)
  #   xx <- girafe(ggobj = gg)
  #   print(xx)
  # })
  # TODO: delete-end

  # # Datensatz selektieren ------------------------------------------------------
  # # ... anhand der User-Eingaben
  output$deutschlandkarte <- renderEatMap({
    req(data_selected(), selectedKompetenzbereich())

    data_selected() %>%
      eatMap(data = ., config = config)
  })


}


# Build App --------------------------------------------------------------------

shinyApp(ui = ui, server = server)


#rsconnect::deployApp('C:\\Users\\alina\\Desktop\\IQB\\BT-Shiny-Minimalloesung')
