################################################################################
# Rohdaten: "data/allDat.RData"
#
# "data_preparation.R" muss neu ausgeführt werden, 
# wenn "data/allDat.RData" geupdated wird.
################################################################################

#' @importFrom magrittr "%>%"


# Vorbereitung -----------------------------------------------------------------
# Laden der erforderlichen Bibliotheken
# library(dplyr)
# library(tidyverse)

# Laden der Konfigurationsliste
source(system.file("config", "config.R", package = "BTShinyApp"))

# Rohdatensatz
data("allDat", package = "BTShinyApp")

# Rekodierung von targetPop, Zyklusbezeichnungen, Variablennamen ---------------
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


# Funktionen für den nächsten Schritt ------------------------------------------

# Umwandlung 
print_percent <- function(x) {
  # Anteil in Prozentwert umwandeln und "%" anhängen
  x_perc <- paste0(x, "%")
  
  # Korrektur, falls NAs zu Strings geworden sind
  ifelse(x_perc == "NA%", NA_character_, x_perc)
}

range_check <-
  config$parameter %>%
  purrr::map("range") %>%
  tibble::enframe(name = "parameter") %>%
  dplyr::mutate(
    value = purrr::map(value, tibble::as_tibble)
  ) %>%
  tidyr::unnest(value)


# Umformatierung der Kompetenzwerte --------------------------------------------
BTdata <-
  allDatRec %>%
  dplyr::mutate(
    # die Spalte "est" enthält die tatsächlichen Kompetenzmittelwerte
    # ...diese sollen aber so nicht im Hover-Effekt eingetragen werden, sondern
    # zur leichteren Interpretierbarkeit gerundet werden -> neue Spalte
    est = dplyr::case_when(
      parameter %in% c("mean", "sd") ~ est,
      # Anteil bereits in Prozentwert umgerechnet
      .default = est * 100
    ),
    se = dplyr::case_when(
      parameter %in% c("mean", "sd") ~ se,
      # Anteil bereits in Prozentwert umgerechnet
      .default = se * 100
    ),
    est_print = dplyr::case_when(
      parameter %in% c("mean", "sd") ~ paste0(round(est, 0)),
      .default = print_percent(est)
    ),
    # NAs sollen außerdem mit dem NA-Label aus der config-Liste beschriftet werden
    est_print = ifelse(is.na(est_print), config$na_label, est_print)
  ) %>%
  dplyr::left_join(range_check) %>%
  dplyr::mutate(
    # Werte, die < min_est sind, sollen farblich als min_est und
    # Werte, die > max_est sind, sollen farblich als max_est eingetragen werden
    est_delimited = dplyr::case_when(
      est < min ~ min,
      est > max ~ max,
      .default = est
    ),
    # neue Spalte fachKb mit der Kombination von Fachbereichen
    fachKb = stringr::str_glue("{fach}-{kb}")
  )

# Umformatieren der Prozentwerte -----------------------------------------------
# z.B. "50%" -> "50.0%"

BTdata$est_print <- ifelse(
  grepl("%$", BTdata$est_print), 
  sprintf("%.1f%%", as.numeric(sub("%", "", BTdata$est_print))),
  BTdata$est_print 
)


# auch übersetzt ablegen ("it’s usually better to just load the 250 KB with all translations upfront. It keeps your app simpler, faster at runtime, and 250 KB is trivial in size")

# Part II config übersetzen und abspeichern ------------------------------------
source(system.file("config", "config.R", package = "BTShinyApp"))

config_de <- config

# JSON Übersetzung

json_path <- system.file("extdata", "text_elements", "translation.json", package = "BTShinyApp")
i18n <- shiny.i18n::Translator$new(translation_json_path = json_path)

woerterbuch <- jsonlite::fromJSON(paste(readLines(json_path), collapse = ""), flatten = TRUE)
woerterbuch <- setNames(woerterbuch$translation$en, woerterbuch$translation$de)

# Translator setzen
i18n$set_translation_language("en")

# config Liste übersetzen
config_en <- recode_nested_list(config, woerterbuch)

config <- list("de" = config_de, "en" = config_en)

# Abspeichern ------------------------------------------------------------------
#saveRDS(config, "data/config.Rds")

BTdata_de <- BTdata

# Datensatz rekodieren
BTdata_en <- BTdata %>%
  # 1. alle Spalten übersetzen
  mutate(across(c(cycle, fach, klassenstufe, kb, targetPop), ~ recode(.x, !!! woerterbuch))) %>%
  # 2. 'fachKb' neu erstellen
  mutate(fachKb = paste(fach, kb, sep = "-"))

BTdata <- list("de" = BTdata_de, "en"=BTdata_en)

# Abspeichern ------------------------------------------------------------------
saveRDS(BTdata, "inst/extdata/BTdata_processed.Rds")










######################################################## hier neu:

# Part II config übersetzen und abspeichern ------------------------------------
source(system.file("config", "config.R", package = "BTShinyApp"))

config_de <- config

# JSON Übersetzung

json_path <- system.file("extdata", "text_elements", "translation.json", package = "BTShinyApp")
i18n <- shiny.i18n::Translator$new(translation_json_path = json_path)

woerterbuch <- jsonlite::fromJSON(paste(readLines(json_path), collapse = ""), flatten = TRUE)
woerterbuch <- setNames(woerterbuch$translation$en, woerterbuch$translation$de)

# Translator setzen
i18n$set_translation_language("en")

# config Liste übersetzen
config_en <- recode_nested_list(config, woerterbuch)

config <- list("de" = config_de, "en" = config_en)

# Abspeichern ------------------------------------------------------------------
#saveRDS(config, "data/config.Rds")
# mache auch mit rein bei den anderen Configs, s.u.


# UI Choices -------------------------------------------------------------------

# Zyklen
available_cycles_de <- unique(BTdata_de$cycle)[order(unique(BTdata_de$cycle))] # alle erhobenen Zyklen, Hotfix für Ordnung
available_cycles_en <- unique(BTdata_en$cycle)[order(unique(BTdata_en$cycle))] # alle erhobenen Zyklen, Hotfix für Ordnung
available_parameters_de <- unique(BTdata_de$parameter) # Parameter
available_parameters_en <- unique(BTdata_en$parameter) # Parameter

default_newest_cycle_de <- BTdata_de %>%
  filter(year == max(BTdata_de$year)) %>% # Get rows for the most recent year
  distinct(cycle) %>%                  # Find the unique cycle(s) in that year
  pull(cycle) %>%                      # Extract the cycle name(s) as a vector
  .[1]
default_newest_cycle_en <- BTdata_en %>%
  filter(year == max(BTdata_en$year)) %>% # Get rows for the most recent year
  distinct(cycle) %>%                  # Find the unique cycle(s) in that year
  pull(cycle) %>%                      # Extract the cycle name(s) as a vector
  .[1]

# alle im Datensatz enthaltenen Kombinationen der UI choices
combinations_de <-
  BTdata_de %>%
  distinct(cycle, fachKb, year, parameter, targetPop)
combinations_en <-
  BTdata_en %>%
  distinct(cycle, fachKb, year, parameter, targetPop)

predefined_order_parameters_de <- names(config_de$parameter)
predefined_order_parameters_en <- names(config_en$parameter)

names(predefined_order_parameters_de) <- config_de$parameter %>% map("label")
names(predefined_order_parameters_en) <- config_en$parameter %>% map("label")

predefined_order_targetpop_de <- config_de$targetPop
predefined_order_targetpop_en <- config_en$targetPop


available_cycles <- list("de" = available_cycles_de, "en" = available_cycles_en)
available_parameters <- list("de" = available_parameters_de, "en" = available_parameters_en)
default_newest_cycle <- list("de" = default_newest_cycle_de, "en" = default_newest_cycle_en)
combinations <- list("de" = combinations_de, "en" = combinations_en)
predefined_order_parameters <- list("de" = predefined_order_parameters_de, "en" = predefined_order_parameters_en)
predefined_order_targetpop <- list("de" = predefined_order_targetpop_de, "en" = predefined_order_targetpop_en)


# Texte für die Infobuttons ----------------------------------------------------

infotextfile <- system.file("extdata/text_elements/Infotexte.xlsx", package = "BTShinyApp")
infotexte <- readxl::read_excel(infotextfile)

infotexte_list_de <- setNames(
  infotexte[["de"]],
  infotexte$chunk
)
infotexte_list_en <- setNames(
  infotexte[["en"]],
  infotexte$chunk
)

infotexte_list <- list("de" = infotexte_list_de, "en" = infotexte_list_en)


save(config, i18n, infotexte_list, available_cycles, available_parameters, default_newest_cycle, combinations, predefined_order_parameters, predefined_order_targetpop, woerterbuch, file="data/ui_variables.RData")





######################## Kartendaten auch ablegen

map_path <- system.file("extdata", "map_data", package = "BTShinyApp")
mapdata <- sf::st_read(dsn = map_path, layer = "gadm41_DEU_1")
mapdata <- mapdata[, c("NAME_1", "geometry")]
names(mapdata) <- c("Bundesland", "geometry")

# Abspeichern ------------------------------------------------------------------
saveRDS(mapdata, "inst/extdata/mapdata.Rds")

rm(list = ls())
