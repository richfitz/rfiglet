##' Get a figlet font, returning a `figlet_font` object that can be
##' used with [figlet].
##'
##' This function tries to do the right thing with loading fonts:
##'
##' 1. if `font` is a `figlet_font` already, return it
##' 2. if `font` is a known `figlet_font` in the internal font registry,
##'    then return it (faster than reading it again)
##' 3. if `font` is a filename, then load the font and add it to the registry
##'
##' @title Get figlet font
##'
##' @param font Path or name of the font to load
##'
##' @param verbose Logical, indicating if we should be verbose during font
##'   load.
##'
##' @return A `figlet_font` object for use with [figlet]
##' @export
##' @examples
##' # Load a font with a full path
##' rfiglet::figlet_font(system.file("fonts/standard.flf", package = "rfiglet"))
##'
##' # Load a previously seen font with a name
##' rfiglet::figlet_font("standard")
##'
##' # List known fonts
##' rfiglet::figlet_font_list()
figlet_font <- function(font, verbose = FALSE) {
  if (inherits(font, "figlet_font")) {
    return(font)
  }
  if (file.exists(font)) {
    font <- registry_load(font, verbose)
  }
  ret <- registry_get(font)
  if (is.null(ret)) {
    path_extra <- extra_font_dir()
    pos <- file.path(path_extra, paste0(font, c("", ".flf", ".tlf")))
    i <- file.exists(pos)
    if (any(i)) {
      ret <- registry_get(registry_load(pos[i][[1]], verbose))
    } else {
      if (grepl("(/|\\.(flf|tlf)$)", font)) {
        stop(sprintf(
          "File '%s' not found (and no corresponding font in registry)", font))
      } else {
        stop(sprintf("Font '%s' not found in registry", font))
      }
    }
  }
  ret
}


##' @export
##' @rdname figlet_font
figlet_font_list <- function() {
  registry_list()
}


figlet_font_name <- function(path) {
  sub("_+$", "", tools::file_path_sans_ext(basename(path)))
}


figlet_font_read <- function(filename, extended = FALSE) {
  name <- figlet_font_name(filename)
  if (!file.exists(filename)) {
    stop(sprintf("'%s' (%s) does not exist", name, filename))
  }
  data <- read_lines(filename)
  options <- figlet_font_options(data, filename, name)

  is_comment <- seq_len(options$comment_lines) + 1L
  chars <- figlet_font_characters(data[-c(1L, is_comment)], options,
                                  filename, name, extended)
  ret <- list(name = name,
              comments = data[is_comment],
              chars = chars,
              options = options)
  class(ret) <- "figlet_font"
  ret
}


figlet_font_options <- function(data, filename, name) {
  if (length(data) == 0) {
    stop(sprintf("'%s' (%s) is empty", name, filename))
  }
  re_magic_number <- "^[tf]lf2."
  header <- data[[1]]
  if (!grepl(re_magic_number, header, perl = TRUE)) {
    stop(sprintf("'%s' (%s) is not a valid font", name, filename))
  }
  header <- strsplit(sub(re_magic_number, "", header), " ")[[1]]
  if (length(header) < 6) {
    stop(sprintf("'%s' (%s) has a malformed header", name, filename))
  }

  nms <- c("hard_blank", "height", "base_line", "max_length",
           "old_layout", "comment_lines", "print_direction", "full_layout")

  msg <- max(0L, length(nms) - length(header))
  if (msg > 0L) {
    header <- c(header, rep(list(NA_integer_), msg))
  }

  options <- as.list(header[seq_along(nms)])
  options[-1] <- lapply(options[-1], as.integer)
  names(options) <- nms

  ## if the new layout style isn't available,
  ## convert old layout style. backwards compatability
  if (is.na(options$full_layout)) {
    if (options$old_layout == 0L) {
      options$full_layout <- 64L
    } else if (options$old_layout < 0L) {
      options$full_layout <- 0L
    } else {
      options$full_layout <- as.integer((options$old_layout %&% 31L) %|% 128L)
    }
  }

  options$smush_mode <- options$full_layout
  options$right_to_left <- identical(options$print_direction, 1L)
  options
}


figlet_font_characters <- function(data, options, filename, name, extended) {
  ## http://www.jave.de/docs/figfont.txt
  code_standard <- 32:126
  code_extra <- c(196, 214, 220, 228, 246, 252, 223)
  code_req <- c(code_standard, code_extra)

  get_character <- function(i, d) {
    figlet_font_character(d[, i], options)
  }

  ## First, load ascii and required characters:
  i_req <- seq_len(length(code_req) * options$height)
  dat_req <- matrix(data[i_req], options$height)
  chars <- vector("list", max(code_req))
  chars[code_req] <- lapply(seq_along(code_req), get_character, dat_req)

  if (!extended) {
    return(chars)
  }

  ## Then, extra:
  tmp <- data[-i_req]
  tmp <- tmp[nzchar(tmp)]
  rem <- length(tmp) %% (options$height + 1L)
  if (rem == 0L) {
    dat_extra <- matrix(tmp, options$height + 1L)
    name_extra <- dat_extra[1, ]
    char_extra <- lapply(seq_len(ncol(dat_extra)), get_character,
                         dat_extra[-1, , drop = FALSE])
    code_extra <- as.integer(sub("\\s+.*$", "", name_extra))

    ## This avoids the null character present in 5x7 at least.
    keep <- code_extra > 0
    chars[code_extra[keep]] <- char_extra[keep]
  } else {
    ## bdffonts/5x8.flf (is actually totally broken)
    ## extra/cjkfonts/jiskan16.flf (height is a mess)
    ## extra/cjkfonts/cns.flf (just has trailing junk)
    message(sprintf(
      "Possibly corrupt font '%s' (%s); discarding extra characters",
      name, filename))
  }

  chars
}


figlet_font_character <- function(x, options) {
  ## If the font contains non-ascii symbols then things go quite
  ## poorly here, and that is really hard to detect.
  if (any(is.na(iconv(x)))) {
    return(NULL)
  }

  re_end_marker <- ".*?(.)\\s*$"
  char <- sub(re_end_marker, "\\1", x[[1]])
  ## Corner case triggered by toilet::emboss
  if (identical(char, "^")) {
    char <- "\\^"
  } else if (identical(char, "\\")) { # toilet::emboss
    char <- "\\\\"
  }
  re_end <- sprintf("[%s]{1,2}\\s*$", char)
  txt <- sub(re_end, "", x, perl = TRUE)
  width <- max(nchar(gsub(options$hard_blank, "", txt, fixed = TRUE)))

  m <- strsplit(txt, NULL)
  n <- lengths(m)
  if (any(i <- n < max(n))) {
    m[i] <- lapply(m[i], function(x) c(x, rep(" ", max(n) - length(x))))
  }
  list(width = as.integer(width),
       data = matrix(unlist(m), length(txt), byrow = TRUE))
}
