font_cache <- new.env(parent=emptyenv())

figlet_font_name <- function(font, collection=NULL) {
  if (inherits(font, "figlet_font_name")) {
    return(font)
  }
  if (grepl("::", font, fixed=TRUE)) {
    tmp <- strsplit(font, "::", fixed=TRUE)[[1]]
    collection <- tmp[[1]]
    if (collection == "") {
      collection <- "ours"
    }
    key <- font
    font <- tmp[[2]]
  } else {
    d <- load_figlet_data()
    if (is.null(collection)) {
      i <- match(font, d$name)
      collection <- d$collection[i]
    }
    if (is.na(i)) {
      stop(sprintf("Font %s not found in any collection", font))
    }
    key <- paste(collection, font, sep="::")
  }
  attr(key, "name") <- font
  attr(key, "collection") <- collection
  class(key) <- "figlet_font_name"
  key
}

## TODO: logic around path here is a bit broken.
figlet_font_filename <- function(font, path=NULL) {
  if (inherits(font, "figlet_font_name")) {
    collection <- attr(font, "collection")
    font <- attr(font, "name")
    if (collection == "ours") {
      path <- system.file("fonts", package=.packageName)
    } else {
      path <- file.path(font_path(), collection)
    }
  } else if (is.null(path)) {
    path <- system.file("fonts", package=.packageName)
  }

  extensions <- c("tlf", "flf")
  pos <- file.path(path, paste(font, extensions, sep="."))
  pos <- pos[file.exists(pos)]
  if (length(pos) == 0L) {
    stop("Font not found") # TODO: class error
  }
  pos[[1]]
}

## Duplicate font names are a problem here:
figlet_font <- function(font, collection=NULL) {
  key <- figlet_font_name(font, collection)
  if (exists(key, font_cache)) {
    return(font_cache[[key]])
  }

  data <- figlet_font_load(figlet_font_read(key))
  options <- data$options
  chars <- figlet_font_load_characters(data$data, options$height)
  w <- function(x) {
    if (is.null(x)) NA_integer_ else as.integer(max(nchar(x)))
  }
  width <- vapply(chars, w, integer(1))
  ret <- list(font=font,
              comment=data$comment,
              chars=chars,
              width=width,
              options=options)
  class(ret) <- "figlet_font"

  assign(key, ret, envir=font_cache)

  ret
}

as.figlet_font <- function(x, collection=NULL) {
  if (is.character(x)) {
    x <- figlet_font(x, collection)
  } else if (!inherits(x, "figlet_font")) {
    stop("Can't convert to a figlet_font")
  }
  x
}

figlet_font_load <- function(data) {
  re_magic_number <- '^[tf]lf2.'

  header <- data[[1]]
  if (!grepl(re_magic_number, header, perl=TRUE)) {
    stop("Not a valid font")
  }
  header <- strsplit(sub(re_magic_number, "", header), " ")[[1]]
  if (length(header) < 6) {
    stop("malformed header for") # FontError ## TODO: name missing
  }

  nms <- c("hard_blank", "height", "base_line", "max_length",
           "old_layout", "comment_lines",
           "print_direction", "full_layout")

  msg <- max(0L, length(nms) - length(header))
  if (msg > 0L) {
    header <- c(header, rep(list(NA_integer_), msg))
  }

  ret <- as.list(header[seq_along(nms)])
  ret[-1] <- lapply(ret[-1], as.integer)
  names(ret) <- nms

  ## if the new layout style isn't available,
  ## convert old layout style. backwards compatability
  if (is.na(ret$full_layout)) {
    if (ret$old_layout == 0L) {
      ret$full_layout <- 64L
    } else if (ret$old_layout < 0L) {
      ret$full_layout <- 0L
    } else {
      ret$full_layout <- as.integer((ret$old_layout %&% 31L) %|% 128L)
    }
  }

  ## Keep:
  ##   height
  ##   hard_blank
  ##   print_direction
  ##   smush_mode
  ret$smush_mode <- ret$full_layout

  is_comment <- seq_len(ret$comment_lines) + 1L
  list(options=ret,
       comment=data[is_comment],
       data=data[-c(1L, is_comment)])
}

figlet_font_load_char <- function(x) {
  re_end_marker <- '.*?(.)\\s*$'
  char <- sub(re_end_marker, "\\1", x[[1]])
  ## Corner case triggered by toilet::emboss
  if (identical(char, "^")) {
    char <- "\\^"
  } else if (identical(char, "\\")) { # toilet::emboss
    char <- "\\\\"
  }
  re_end <- sprintf("[%s]{1,2}$", char)
  sub(re_end, "", x, perl=TRUE)
}

figlet_font_load_characters <- function(data, height) {
  ## http://www.jave.de/docs/figfont.txt
  code_standard <- 32:126
  code_extra <- c(196, 214, 220, 228, 246, 252, 223)
  code_req <- c(code_standard, code_extra)

  ## Helper:
  f <- function(i, d) {
    figlet_font_load_char(d[, i])
  }

  ## First, load ascii and required characters:
  i_req <- seq_len(length(code_req) * height)
  dat_req <- matrix(data[i_req], height)
  chars <- vector("list", max(code_req))
  chars[code_req] <- lapply(seq_along(code_req), f, dat_req)

  ## Then, extra:
  tmp <- data[-i_req]
  rem <- length(tmp) %% (height + 1L)
  if (rem == 0L) {
    dat_extra <- matrix(data[-i_req], height + 1L)
    name_extra <- dat_extra[1, ]
    char_extra <- lapply(seq_len(ncol(dat_extra)), f,
                         dat_extra[-1, , drop=FALSE])
    code_extra <- as.integer(sub("\\s+.*$", "", name_extra))

    ## This avoids the null character present in 5x7 at least.
    keep <- code_extra > 0
    chars[code_extra[keep]] <- char_extra[keep]
  } else {
    ## TODO: get the filename here...
    ##
    ## NOTE: At the moment this is only jiskan16, so making this a
    ## message rather than a warning.
    message("Discarding extra characters")
  }

  chars
}

## TODO: print in figlet style...
##' @export
print.figlet_font <- function(x, ...) {
  print(sprintf("<figlet_font object: %s>", x$font))
}

figlet_font_read <- function(font, path=NULL) {
  filename <- figlet_font_filename(font, path)
  str <- readLines(filename, warn=FALSE)

  ## Delete *trailing* whitespace.
  last_non_empty <- max(which(str != ""))
  if (last_non_empty < length(str)) {
    str <- str[seq_len(last_non_empty)]
  }

  str
}

## TODO: get types right on load and clean up NA values...
font_is_r2l <- function(font) {
  identical(font$options$print_direction, 1L)
}
