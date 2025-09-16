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


order_parameters <- function(params, predefined_order_parameters) {
  ordered_parameters <- predefined_order_parameters[which(predefined_order_parameters %in% params)]
}

order_targetpop <- function(targetpops, predefined_order_targetpop) {
  predefined_order_targetpop[which(predefined_order_targetpop %in% targetpops)]
}



