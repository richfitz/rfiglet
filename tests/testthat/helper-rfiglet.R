if (!nzchar(Sys.getenv("RFIGLET_FONT_DIR", ""))) {
  Sys.setenv("RFIGLET_FONT_DIR" = tempfile())
}


skip_if_no_extra_fonts <- function(name = "emboss") {
  tryCatch(
    figlet_font(name),
    error = function(e) testthat::skip("Extra fonts not available"))
}
