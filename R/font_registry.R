registry <- new.env(parent = emptyenv())

registry_add <- function(x) {
  registry[[x$name]] <- x
}


registry_get <- function(name) {
  registry[[name]]
}


registry_list <- function() {
  ls(registry)
}


registry_clear <- function() {
  rm(list = ls(envir = registry), envir = registry)
}


registry_load <- function(path, verbose = FALSE) {
  name <- figlet_font_name(path)
  if (name %in% registry_list()) {
    message_verbose(sprintf("'%s' (%s) already loaded", name, path), verbose)
  } else {
    message_verbose(sprintf("'%s' (%s)", name, path), verbose)
    registry_add(figlet_font_read(path))
  }
  invisible(name)
}


registry_load_dir <- function(path, verbose = FALSE,
                              pattern = "\\.(tlf|flf)$") {
  fonts <- dir(path, pattern, full.names = TRUE, ignore.case = TRUE)
  message_verbose(sprintf("Found '%d' fonts in '%s'", length(fonts), path),
                  verbose)
  for (f in fonts) {
    registry_load(f, verbose)
  }
}
