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

# Abspeichern ------------------------------------------------------------------
#saveRDS(BTdata, "data/BTdata_processed.Rds")
rm(list = ls())
