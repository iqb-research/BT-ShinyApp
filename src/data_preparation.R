# Rohdaten: "data/allDat.RData"
# "data_preparation.R" muss neu ausgeführt werden, wenn "data/allDat.RData" geupdated wird.

library(dplyr)
library(tidyverse)

source("config.R")

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

range_check <-
  config$parameter %>%
  map("range") %>%
  enframe(name = "parameter") %>%
  mutate(
    value = map(value, as_tibble)
  ) %>%
  unnest(value)

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

saveRDS(BTdata, "data/BTdata_processed.Rds")
rm(list = ls())
