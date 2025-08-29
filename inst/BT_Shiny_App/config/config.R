config <-
  list(
    total_label = "Deutschland",
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