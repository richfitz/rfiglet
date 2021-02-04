##' Download a large set of fonts. These will be available to [figlet]
##' and [figlet_font] by name once downloaded.
##'
##' @title Download more fonts
##'
##' @param dest Directory to use. We use the environment variable
##'   `RFIGLET_FONT_DIR` if defined, then fall back on
##'   `tools::R_user_dir("rfiglet", "data")`
##'
##' @param verbose Be verbose?
##'
##' @export
##' @examples
##' if (interactive()) {
##'   path <- tempfile()
##'   rfiglet::figlet_download_fonts(path)
##'   rfiglet::figlet("hello", file.path(path, "emboss.tlf"))
##' }
figlet_download_fonts <- function(dest = NULL, verbose = TRUE) {
  download_fonts(dest %||% extra_font_dir(), verbose)
}


download_fonts <- function(dest, verbose = TRUE) {
  if (length(dir(dest)) > 0L) {
    message_verbose(sprintf("Fonts already exist at '%s'", dest), verbose)
    return(invisible(dest))
  }
  message_verbose(sprintf("Downloading fonts to '%s'", dest), verbose)

  url <- "https://github.com/cmatsuoka/figlet-fonts/archive/master.zip"
  path_zip <- tempfile()
  on.exit(unlink(path_zip))
  path_zip <- download_file(url, path_zip, verbose)

  path_extract <- tempfile()
  on.exit(unlink(path_extract, recursive = TRUE))
  utils::unzip(path_zip, exdir = path_extract)

  tld <- dir(path_extract) # figlet-fonts-master
  files <- with_dir(file.path(path_extract, tld),
                    dir(recursive = TRUE, pattern = "\\.(flf|tlf)$"))

  exclude <- c("ours", "Obanner", "ms-dos", "Obanner-canon")
  exclude <- grepl(sprintf("^(%s)[/\\]", paste(exclude, collapse = "|")),
                   files)
  copy <- files[!exclude]

  file_src <- file.path(path_extract, tld, copy)
  file_dest <- file.path(dest, sub("_+\\.", ".", basename(file_src)))

  ## NOTE: we do lose some duplicates here, somewhat randomly
  dir.create(dest, FALSE, TRUE)
  file.copy(file_src, file_dest)
  invisible(dest)
}


extra_font_dir <- function() {
  Sys.getenv("RFIGLET_FONT_DIR",
             tools::R_user_dir("rfiglet", "data"))
}
