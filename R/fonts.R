##' List or search figlet fonts
##' @title List or search figlet fonts
##' @param height Optional height - can be length 1 (exact height) or
##' length 2, to find fonts within a height range.
##' @export
##' @examples
##' length(figlet_fonts())
##' figlet_fonts(height=10)
##' figlet_fonts(height=c(15, Inf))
figlet_fonts <- function(height=NULL) {
  if (length(height) == 0L) {
    ok <- rep(TRUE, nrow(figlet_data))
  } else if (length(height) == 1L) {
    ok <- figlet_data[, "height"] == height
  } else if (length(height) == 2L) {
    h <- figlet_data[, "height"]
    ok <- h >= height[1] & h <= height[2]
  } else {
    stop("Invalid length for height")
  }
  rownames(figlet_data)[ok]
}

## Complete list of fonts:
figlet_fonts_in_dir <- function(path=NULL) {
  if (is.null(path)) {
    path <- system.file("fonts", package=.packageName)
  }
  re <- "\\.(flf|tlf)$"
  fonts <- dir(path, pattern=re)
  sub(re, "", fonts)
}

figlet_font_info <- function(font) {
  data <- figlet_font_load(figlet_font_read(font))
  ret <- list(name=font,
              comment=data$comment,
              options=data$options)
  class(ret) <- "figlet_font_info"
  ret
}
