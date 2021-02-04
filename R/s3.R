##' @export
print.figlet_text <- function(x, ...) {
  cat(paste0(x, "\n", collapse = ""))
  invisible(x)
}


##' @export
print.figlet_font <- function(x, preview = TRUE, ...) {
  cat(sprintf("<figlet_font object: %s>\n", x$name))
  if (preview) {
    print(figlet(x$name, x))
  }
  invisible(x)
}


##' @export
as.character.figlet_text <- function(x, ...) {
  paste0(x, collapse = "\n")
}
