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
  message("Loading BTShinyApp package...")
  
  # Load BTdata
  bt_path <- system.file("extdata", "BTdata_processed.Rds", package = pkgname)
  #message("BTdata path: ", bt_path)
  if (file.exists(bt_path)) {
    bt <- readRDS(bt_path)
    assign("BTdata", bt, envir = asNamespace(pkgname))
    #message("BTdata loaded")
  } else {
    warning("BTdata file not found!") 
  }
  
  # Load mapdata
  map_path <- system.file("extdata", "mapdata.Rds", package = pkgname)
  #message("mapdata path: ", map_path)
  if (file.exists(map_path)) {
    mp <- readRDS(map_path)
    assign("mapdata", mp, envir = asNamespace(pkgname))
    #message("mapdata loaded")
  } else {
    warning("mapdata file not found!")
  }

}
