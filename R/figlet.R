##' FIGlet
##' @title FIGlet
##' @param text Text to make bigger
##' @param font Name of the font
##' @export
##' @examples
##' message(figlet("FIGlet"))
figlet <- function(text, font="standard") {
  make_figlet(font)(text)
}

make_figlet <- function(font="standard") {
  info <- list(font=as.figlet_font(font),
               direction="left-to-right",
               justify="left")
  if (font_is_r2l(info$font)) {
    info$direction <- "right-to-left"
  }
  ret <- function(text) {
    figlet_render(text, info)
  }
  class(ret) <- "figlet"
  ret
}

## TODO: Flag here to allow immediate conversion to text rather than
## as the figlet string?
figlet_render <- function(text, info) {
  info$cur_char_width <- 0L
  info$prev_char_width <- 0L

  ## TODO: move from options
  height <- info$font$options$height

  ## buffer <- matrix('', height, 1L)
  buffer <- rep("", height)

  for (char in asc(text)) {
    cur_char <- info$font$chars[[char]]
    if (is.null(cur_char)) {
      next
    }

    info$cur_char_width <- info$font$width[[char]]
    max_smush <- smush_amount(buffer, cur_char, info)
    for (row in seq_len(height)) {
      add_left  <- buffer[row]
      add_right <- cur_char[row]
      n_left <- nchar(add_left)
      n_right <- nchar(add_right)

      if (info$direction == 'right-to-left') {
        al <- add_left
        add_left <- add_right
        add_right <- al
      }
      for (i in seq_len(max_smush)) {
        idx <- n_left - max_smush + i
        if (idx >= 1L && idx <= n_left) {
          left <- substr(add_left, idx, idx)
        } else {
          left <- ''
        }
        right <- substr(add_right, i, i)
        smushed <- smush_chars(left, right, info)

        idx <- n_left - max_smush + i
        if (idx >= 1L && idx <= n_left) {
          substr(add_left, idx, idx) <- smushed
          # l <- strsplit(add_left, NULL)[[1]]
          # l[idx] <- smushed
          # add_left <- paste0(l)
        }
      }
      buffer[row] <- paste0(add_left,
                            substr(add_right, max_smush + 1L, n_right))
    }
    info$prev_char_width <- info$cur_char_width
  }

  figlet_string(buffer, info$font)
}

figlet_string <- function(string, font, ...) {
  ret <- list(string=string, font=font)
  class(ret) <- "figlet_string"
  ret
}

##' @export
as.character.figlet_string <- function(x, ...) {
  ## TODO: Line breaks
  ## TODO: Optionally trim blank lines above/below

  # if (info$justify == "right") {
    ## TODO:
    ## for row in range(0, info.base.Font.height):
    ##     buffer[row] = (
    ##         ' ' * (info.base.width - len(buffer[row]) - 1)
    ##     ) + buffer[row]
  # } else if (info$justify == "center") {
    ## TODO:
    ## for row in range(0, info.base.Font.height):
    ##     buffer[row] = (
    ##         ' ' * int((info.base.width - len(buffer[row])) / 2)
    ##     ) + buffer[row]
  # }

  ret <- iconv(x$string, "UTF-8")
  ret <- gsub(x$font$options$hard_blank, " ", ret, fixed=TRUE)
  ret <- paste(ret, collapse="\n")
  ret
}

##' @export
print.figlet_string <- function(x, ...) {
  cat(as.character(x), sep="\n")
}

## The kerning/smushing engine.  Considerable room for cleaning.  This
## is a direct port of the unrefactored python code that is a direct
## port of the C.  It's not pretty!

## Kerning engine constants:
SM_EQUAL     <- 1    # smush equal chars (not hardblanks)
SM_LOWLINE   <- 2    # smush _ with any char in hierarchy
SM_HIERARCHY <- 4    # hierarchy: |, /\, [], {}, (), <>
SM_PAIR      <- 8    # hierarchy: [ + ] -> |, { + } -> |, ( + ) -> |
SM_BIGX      <- 16   # / + \ -> X, > + < -> X
SM_HARDBLANK <- 32   # hardblank + hardblank -> hardblank
SM_KERN      <- 64
SM_SMUSH     <- 128

## info is a list that must have elements:
##   font
##   prev_char_width
##   cur_char_width
##   direction
smush_chars <- function(left, right, info) {
  ## Given 2 characters which represent the edges rendered figlet
  ## fonts where they would touch, see if they can be smushed
  ## together.  Returns NULL if this cannot or should not be done.
  if (is_space(left)) {
    return(right)
  } else if (is_space(right)) {
    return(left)
  }

  ## Disallows overlapping if previous or current char has a width
  ## of 1 or zero
  if (info$prev_char_width < 2 || info$cur_char_width < 2) {
    return(NULL)
  }

  smush_mode <- info$font$options$smush_mode

  ## kerning only
  if ((smush_mode %&% SM_SMUSH) == 0) {
    return(NULL)
  }

  ## smushing by universal overlapping
  hard_blank <- info$font$options$hard_blank
  if ((smush_mode %&% 63) == 0) {
    ## Ensure preference to visiable characters.
    if (left == hard_blank) {
      return(right)
    }
    if (right == hard_blank) {
      return(left)
    }

    if (info$direction == 'right-to-left') {
      return(left)
    } else {
      return(right)
    }
  }

  if (smush_mode %&% SM_HARDBLANK) {
    if (left == hard_blank && right == hard_blank) {
      return(left)
    }
  }

  ## Missing from pyfiglet?
  if (left == hard_blank && right == hard_blank) {
    return(NULL)
  }

  if (smush_mode %&% SM_EQUAL) {
    if (left == right) {
      return(left)
    }
  }

  smushes = list()
  if (smush_mode %&% SM_LOWLINE) {
    smushes <- c(smushes, list(c("_", "|/\\[]{}()<>")))
  }

  if (smush_mode %&% SM_HIERARCHY) {
    smushes <- c(smushes, list(
      c("|",   "|/\\[]{}()<>"),
      c("\\/", "[]{}()<>"),
      c("[]",  "{}()<>"),
      c("{}",  "()<>"),
      c("()",  "<>")))
  }

  for (el in smushes) {
    a <- el[[1]]
    b <- el[[2]]
    ## This is where shit gets annoying; we don't have character-wise
    ## "in" like python.
    if (left %cin% a && right %cin% b) {
      return(right)
    }
    if (right %cin% a && left %cin% b) {
      return(left)
    }
  }

  if (smush_mode %&% SM_PAIR) {
    for (pair in paste0(c(right, left), c(left, right))) {
      if (pair %in% c("[]", "{}", "()")) {
        return("|")
      }
    }
  }

  if (smush_mode %&% SM_BIGX) {
    if (left == "/" && right == "\\") {
      return("|")
    }
    if (right == "/" && left == "\\") {
      return("Y")
    }
    if (left == ">" && right == "<") {
      return("X")
    }
  }
  NULL
}

smush_amount <- function(buffer, cur_char, info) {
  ## Calculate the amount of smushing we can do between this char and
  ## the last If this is the first char it will throw a series of
  ## exceptions which are caught and cause appropriate values to be
  ## set for later.

  ## This differs from C figlet which will just get bogus values from
  ## memory and then discard them after.

  if ((info$font$options$smush_mode %&% (SM_SMUSH %|% SM_KERN)) == 0) {
    return(0L)
  }

  max_smush <- info$cur_char_width

  for (row in seq_len(info$font$options$height)) {
    line_left  <- buffer[row]
    line_right <- cur_char[row]
    if (info$direction == 'right-to-left') {
      ll <- line_left
      line_left <- line_right
      line_right <- ll
    }

    ## BASE0:
    linebd <- nchar(rstrip(line_left)) - 1L
    if (linebd < 0) {
      linebd <- 0
    }
    if (linebd < nchar(line_left)) {
      ch1 <- substr(line_left, linebd + 1L, linebd + 1L)
    } else {
      linebd <- 0L
      ch1 <- ""
    }

    linebd <- nchar(rstrip(line_left))
    if (linebd < 1L) {
      linebd <- 1L
    }

    if (linebd <= nchar(line_left)) {
      ch1 <- substr(line_left, linebd, linebd)
    } else {
      linebd <- 1L
      ch1 <- ""
    }

    charbd <- nchar(line_right) - nchar(lstrip(line_right)) + 1L
    if (charbd <= nchar(line_right)) {
      ch2 <- substr(line_right, charbd, charbd)
    } else {
      charbd <- nchar(line_right) + 1L
      ch2 <- ''
    }

    amt <- (charbd - 1L) + nchar(line_left) - 1L - (linebd - 1L)

    if (ch1 == '' || ch1 == ' ') {
      amt <- amt + 1L
    } else if (ch2 != '' && !is.null(smush_chars(ch1, ch2, info))) {
      amt <- amt + 1L
    }

    if (amt < max_smush) {
      max_smush <- amt
    }
  }

  max_smush
}
