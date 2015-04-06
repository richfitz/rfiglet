##' Download extra fonts
##' @title Download extra fonts
##' @param verbose Be verbose?
##' @export
install_fonts <- function(verbose=TRUE) {
  path <- font_path()
  if (!file.exists(path)) {
    download_fonts(verbose)
  }
  if (!file.exists(font_info_path())) {
    update_fonts_info(verbose)
  }
}

##' @importFrom rappdirs user_data_dir
font_path <- function() {
  rappdirs::user_data_dir("rfiglet")
}
font_info_path <- function() {
  file.path(font_path(), "info.rds")
}

uninstall_fonts <- function() {
  path <- font_path()
  unlink(path, recursive=TRUE)
}

update_fonts_info <- function(verbose=TRUE) {
  if (verbose) {
    message(figlet("caching fonts"))
  }
  ## Now, things to create the cached information about these fonts.
  path <- font_path()
  contents <- dir(path)
  collections <- contents[is_directory(file.path(path, contents))]
  ## Exclude the "ours" directory though, as it duplicates base, and
  ## exclude Obanner and ms-dos because it's difficult...
  exclude <- c("ours", "Obanner", "ms-dos", "Obanner-canon")
  collections <- setdiff(collections, exclude)

  info <- lapply(file.path(path, collections), build_font_info)
  names(info) <- collections

  info <- do.call("rbind", info, quote=TRUE)
  rownames(info) <- NULL

  saveRDS(info, font_info_path())
}

download_fonts <- function(verbose=TRUE) {
  if (verbose) {
    message(figlet("downloading"))
  }
  url <- "https://github.com/cmatsuoka/figlet-fonts/archive/master.zip"
  tmp1 <- tempfile()
  tmp2 <- tempfile()

  downloader::download(url, tmp1)
  unzip(tmp1, exdir=tmp2)
  tld <- dir(tmp2)
  if (length(tld) != 1L) {
    stop("Expected archive with single top-level directory")
  }

  path <- font_path()
  dir.create(path, FALSE, TRUE)

  contents <- list.files(file.path(tmp2, tld))
  file.rename(file.path(tmp2, tld, contents),
              file.path(path, contents))
  file.remove(tmp1)
  unlink(tmp2, recursive=TRUE)
}

build_font_info <- function(path) {
  get_data <- function(font) {
    try(figlet_font_load(figlet_font_read(font, path)),
        silent=TRUE)
  }
  f <- function(data) {
    unlist(data$options[c("height", "print_direction", "smush_mode")])
  }

  ## These won't load at present:
  blacklist <- c(
    # Won't load
    "dwhistled", "eftifont", "pyramid",
    # Won't render
    "dosrebel", "cns", "konto", "kontoslant")
  ## ascii12 and ascii9 plus derivatives could be fixed -- the endmark
  ## is not getting picked up.
  prefix <- c("ascii", "bigascii", "bigmono", "smascii", "mono", "smmono")
  err_end <- c(paste0(prefix, "12"), paste0(prefix, "9"))
  blacklist <- c(blacklist, err_end)

  fonts <- setdiff(figlet_fonts_in_dir(path), blacklist)
  data <- lapply(fonts, get_data)
  ok <- !sapply(data, inherits, "try-error")
  res <- as.data.frame(t(vapply(data[ok], f, integer(3))))
  res$collection <- basename(path)
  res$name <- fonts[ok]

  if (any(!ok)) {
    message("Ignoring: ", paste(fonts[!ok], collapse=", "))
  }

  res
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
