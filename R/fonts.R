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
  d <- load_figlet_data()
  if (length(height) == 0L) {
    ok <- rep(TRUE, nrow(d))
  } else if (length(height) == 1L) {
    ok <- d[, "height"] == height
  } else if (length(height) == 2L) {
    h <- d[, "height"]
    ok <- h >= height[1] & h <= height[2]
  } else {
    stop("Invalid length for height")
  }
  d$name[ok]
}

figlet_base_fonts <- function() {
  figlet_base_data$name
}

load_figlet_data <- function() {
  if (exists(".info", font_cache)) {
    font_cache$.info
  } else {
    info <- font_info_path()
    if (file.exists(info)) {
      dat <- readRDS(info)
    } else {
      dat <- NULL
    }
    ret <- rbind(figlet_base_data, dat)
    assign(".info", ret, envir=font_cache)
    ret
  }
}
