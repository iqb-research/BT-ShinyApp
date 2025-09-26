# translation helper:

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



.onLoad <- function(libname, pkgname) {
  # Load BTdata
  bt_path <- system.file("extdata", "BTdata_processed.Rds", package = pkgname)
  if (file.exists(bt_path)) {
    bt <- readRDS(bt_path)
    assign("BTdata", bt, envir = asNamespace(pkgname))
  }
  
  # Load mapdata
  map_path <- system.file("extdata", "mapdata.Rds", package = pkgname)
  if (file.exists(map_path)) {
    mp <- readRDS(map_path)
    assign("mapdata", mp, envir = asNamespace(pkgname))
  }
  
  # Load UI variables
  ui_path <- system.file("data", "ui_variables.RData", package = pkgname)
  if (file.exists(ui_path)) {
    load(ui_path, envir = asNamespace(pkgname))
  }
}
