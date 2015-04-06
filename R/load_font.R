font_cache <- new.env(parent=emptyenv())

figlet_font <- function(font) {
  if (exists(font, font_cache)) {
    return(font_cache[[font]])
  }

  data <- figlet_font_load(figlet_font_read(font))
  options <- data$options
  chars <- figlet_font_load_characters(data$data, options$height)
  w <- function(x) {
    if (is.null(x)) NA_integer_ else max(nchar(x))
  }
  width <- vapply(chars, w, integer(1))
  ret <- list(font=font,
              comment=data$comment,
              chars=chars,
              width=width,
              options=options)
  class(ret) <- "figlet_font"

  assign(font, ret, envir=font_cache)

  ret
}

as.figlet_font <- function(x) {
  if (is.character(x)) {
    x <- figlet_font(x)
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
    header <- c(header, rep(list(NA), msg))
  }

  ret <- as.list(header[seq_along(nms)])
  ret[-1] <- lapply(ret[-1], as.integer)
  names(ret) <- nms

  # if the new layout style isn't available,
  # convert old layout style. backwards compatability
  if (is.na(ret$full_layout)) {
    if (ret$old_layout == 0L) {
      ret$full_layout <- 64
    } else if (ret$old_layout < 0L) {
      ret$full_layout <- 0
    } else {
      ## TODO: drop this for simple modulo stuff?
      ret$full_layout <-
        bitops::bitOr(bitops::bitAnd(ret$old_layout, 31), 128)
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

figlet_font_load_char <- function(x, height) {
  re_end_marker <- '.*?(.)\\s*$'
  re_end <- sprintf("[%s]{1,2}$", sub(re_end_marker, "\\1", x[[1]]))
  sub(re_end, "", x)
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
  dat_extra <- matrix(data[-i_req], height + 1L)
  name_extra <- dat_extra[1, ]
  char_extra <- lapply(seq_len(ncol(dat_extra)), f,
                       dat_extra[-1, , drop=FALSE])
  code_extra <- as.integer(sub("\\s+.*$", "", name_extra))

  ## This avoids the null character present in 5x7 at least.
  keep <- code_extra > 0
  chars[code_extra[keep]] <- char_extra[keep]

  chars
}

## Not implemented:
##   isValidFont
##   getFonts
##   infoFont
##   classed errors

## TODO: print in figlet style...
##' @export
print.figlet_font <- function(x, ...) {
  print(sprintf("<figlet_font object: %s>", x$font))
}

## This should allow loading user-procided fonts, of course, with a
## PATH argument or similar...
## TODO: to implement some of the bits below, factor out the filename bit.
figlet_font_read <- function(font) {
  extensions <- c("tlf", "flf")
  pos <- system.file(sprintf("fonts/%s.%s", font, extensions),
                     package=.packageName)
  if (length(pos) == 0L) {
    stop("Font not found") # TODO: class error
  }
  str <- readLines(pos[[1]])

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
