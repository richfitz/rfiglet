##' FIGlet
##' @title FIGlet
##'
##' @param text Text to make bigger
##'
##' @param font Name of font, path to font, or `figlet_font` object;
##'   see [figlet_font] for details
##'
##' @param width Width to use when justifying and breaking lines
##'
##' @param justify Text justification to use in rendering ("left",
##'   "centre", "right")
##'
##' @param absolute Logical, indicating if alignment is absolute
##'   (relative to `width`). Has an effect when justify is `right` or
##'   `centre` only.
##'
##' @param strip Logical, indicating if whitespace (trailing, plus
##'   leading/trailing empty lines) should be removed.
##'
##' @return An object of class `figlet_text` which is a character
##'   vector with a handy print method and attributes `font` and
##'   `text`
##'
##' @export
##' @examples
##' rfiglet::figlet("FIGlet")
figlet <- function(text, font = "standard", width = getOption("width", 80),
                   justify = "left", absolute = FALSE, strip = TRUE) {
  font <- figlet_font(font)
  str <- figlet_render(text, font, width, justify, absolute, strip)
  class(str) <- "figlet_text"
  attr(str, "font") <- font$name
  attr(str, "text") <- text
  str
}


figlet_render <- function(text, font, width = getOption("width", 80),
                          justify = "left", absolute = FALSE, strip = TRUE) {
  if (any(grepl("\n", text, fixed = TRUE))) {
    text <- unlist(strsplit(text, "\n", fixed = TRUE))
  }
  if (length(text) == 1L) {
    dat <- figlet_render_horizontal(text, font)
  } else {
    dat <- lapply(text, figlet_render_horizontal, font)
  }
  lines <- figlet_render_layout(dat, font$options$hard_blank, width, justify,
                                absolute)
  mat <- figlet_render_vertical(lines, font)
  matrix_to_text(mat, strip = strip)
}


figlet_render_layout_template <- function(text, hard_blank,
                                          offset_char = 0,
                                          offset_text = 0) {
  pos <- apply(array(text %in% c(" ", hard_blank), dim(text)), 2, all)
  map <- c(1:9, letters, LETTERS)
  n <- cumsum(pos) + 1L + offset_char
  template <- map[n]
  template[pos] <- " "
  start <- c(1, which(pos) + 1L) + offset_text
  end <- c(which(pos) - 1L, ncol(text)) + offset_text
  list(template = paste(template, collapse = ""),
       start = start,
       end = end)
}


figlet_render_layout <- function(buffer, hard_blank, width, justify, absolute) {
  if (is.list(buffer)) {
    ## We have hard breaks
    template <- list(template = character(), start = integer(), end = integer())
    for (line in buffer) {
      tmp <- figlet_render_layout_template(line, hard_blank,
                                           length(template$start),
                                           sum(nchar(template$template)))
      template <- Map(c, template, tmp)
    }
    buffer <- do.call(cbind, buffer)
  } else {
    template <- figlet_render_layout_template(buffer, hard_blank)
  }

  ## Conecutive whitespace is *hard* and might be important for
  ## kerning. We could wrap based on the first or the last space (the
  ## two regexps are "(<= ) " and " (?= )"). Fairest might be to
  ## alternate
  template$template <- gsub("(?<= ) ", "-", template$template, perl = TRUE)

  ## Use R's wrap and format functions to do all the heavy lifting
  tmp <- trimws(gsub("-", " ", strwrap(template$template, width = width)))
  lpad <- nchar(sub("[^ ].*", "", format(tmp, justify = justify)))
  rpad <- max(nchar(tmp) + lpad) - nchar(tmp) - lpad
  if (absolute && max(nchar(tmp)) < width) {
    if (justify == "right") {
      lpad <- lpad + (width - max(nchar(tmp)))
    } else if (justify == "centre") {
      lpad <- lpad + ceiling((width - max(nchar(tmp))) / 2)
    }
  }

  map <- c(1:9, letters, LETTERS)
  words <- lapply(strsplit(trimws(tmp), " +"), function(x)
    range(match(substr(x, 1, 1), map)))

  line <- function(i) {
    j <- template$start[words[[i]][1]]:template$end[words[[i]][2]]
    text <- buffer[, j, drop = FALSE]
    if (lpad[i] > 0) {
      text <- cbind(matrix(" ", nrow(text), lpad[i]), text)
    }
    if (rpad[i] > 0) {
      text <- cbind(text, matrix(" ", nrow(text), rpad[i]))
    }
    text
  }

  lapply(seq_along(words), line)
}


figlet_render_vertical <- function(lines, font) {
  if (length(lines) == 1L) {
    return(lines[[1]])
  }

  buffer <- lines[[1]]
  width <- ncol(buffer)
  for (line in 2:length(lines)) {
    cur_line <- lines[[line]]
    max_smush <- vsmush_amount(buffer, cur_line, font$options)
    add_above <- buffer
    add_below <- cur_line
    n_above <- nrow(add_above)
    n_below <- nrow(add_below)

    for (i in seq_len(max_smush)) {
      idx <- n_above - max_smush + i
      above <- add_above[idx, ]
      below <- add_below[i, ]

      smushed <- vcapply(seq_len(width), function(col)
        vsmush_chars(above[col], below[col], font$options))

      idx <- n_above - max_smush + i
      if (idx >= 1L && idx <= n_above) {
        add_above[idx, ] <- smushed
      }
    }
    if (n_below > 0) {
      add_below <- add_below[(max_smush + 1):n_below, , drop = FALSE]
    }
    buffer <- rbind(add_above, add_below, deparse.level = 0)
  }

  buffer
}


figlet_render_horizontal <- function(text, font) {
  state <- list(curr_char_width = 0L,
                prev_char_width = 0L)

  height <- font$options$height

  buffer <- matrix(character(), height, 0)

  char_index <- asc(text)
  for (char in seq_along(char_index)) {
    cur_char <- font$chars[[char_index[[char]]]]
    if (is.null(cur_char)) {
      stop(sprintf("The font '%s' does not contain the characters '%s'",
                   font$name, substr(text, char, char)))
    }

    state$curr_char_width <- cur_char$width
    max_smush <- smush_amount(buffer, cur_char$data, font$options, state)

    if (font$options$right_to_left) {
      add_left <- cur_char$data
      add_right <- buffer
    } else {
      add_left <- buffer
      add_right <- cur_char$data
    }

    n_left <- ncol(add_left)
    n_right <- ncol(add_right)

    for (i in seq_len(max_smush)) {
      idx <- n_left - max_smush + i
      if (idx >= 1L && idx <= n_left) {
        left <- add_left[, idx]
      } else {
        left <- rep("", height)
      }
      right <- add_right[, i]

      smushed <- vcapply(seq_len(height), function(row)
        smush_chars(left[row], right[row], font$options, state))

      idx <- n_left - max_smush + i
      if (idx >= 1L && idx <= n_left) {
        add_left[, idx] <- smushed
      }
    }
    if (n_right > 0) {
      add_right <- add_right[, (max_smush + 1):n_right, drop = FALSE]
    }

    buffer <- cbind(add_left, add_right, deparse.level = 0)

    state$prev_char_width <- state$curr_char_width
  }

  ## No need for hard blanks now
  buffer[buffer == font$options$hard_blank] <- " "

  buffer
}


## Kerning engine constants:
kern <- list(
  equal      = 1L,    # smush equal chars (not hardblanks)
  lowline    = 2L,    # smush _ with any char in hierarchy
  hierarchy  = 4L,    # hierarchy: |, /\, [], {}, (), <>
  pair       = 8L,    # hierarchy: [ + ] -> |, { + } -> |, ( + ) -> |
  bigx       = 16L,   # / + \ -> x, > + < -> x
  hardblank  = 32L,   # hardblank plus hardblank -> hardblank
  kern       = 64L,
  smush      = 128L,
  vequal     = 256L,  # smush equal chars
  vlowline   = 512L,  # smush _ with any char in hierarchy
  vhierarchy = 1024L, # hierarchy: |, /\, [], {}, (), <>
  vhoriz     = 2048L, # smush combinations of - _ to =
  vsuper     = 4096L, # (not supported)
  vkern      = 8192L,
  vsmush     = 16384L)


smush_chars <- function(left, right, options, state) {
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
  if (state$prev_char_width < 2 || state$curr_char_width < 2) {
    return(NULL)
  }

  smush_mode <- options$smush_mode

  ## kerning only
  if ((smush_mode %&% kern$smush) == 0) {
    return(NULL)
  }

  ## smushing by universal overlapping
  hard_blank <- options$hard_blank
  if ((smush_mode %&% 63) == 0) {
    ## Ensure preference to visible characters.
    if (left == hard_blank) {
      return(right)
    }
    if (right == hard_blank) {
      return(left)
    }
  }

  if (left == hard_blank && right == hard_blank) {
    if (smush_mode %&% kern$hardblank > 0) {
      return(left)
    } else {
      return(NULL)
    }
  }

  if (smush_mode %&% kern$equal) {
    if (left == right) {
      return(left)
    }
  }

  smushes <- list()
  if (smush_mode %&% kern$lowline) {
    smushes <- c(smushes, list(c("_", "|/\\[]{}()<>")))
  }

  if (smush_mode %&% kern$hierarchy) {
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
    ## This is where things gets annoying; we don't have character-wise
    ## "in" like python.
    if (left %cin% a && right %cin% b) {
      return(right)
    }
    if (right %cin% a && left %cin% b) {
      return(left)
    }
  }

  if (smush_mode %&% kern$pair) {
    for (pair in paste0(c(right, left), c(left, right))) {
      if (pair %in% c("[]", "{}", "()")) {
        return("|")
      }
    }
  }

  if (smush_mode %&% kern$bigx) {
    if (left == "/" && right == "\\") {
      return("|")
    }
    if (left == "\\" && right == "/") {
      return("Y")
    }
    if (left == ">" && right == "<") {
      return("X")
    }
  }
  NULL
}


smush_amount <- function(buffer, cur_char, options, state) {
  ## Calculate the amount of smushing we can do between this char and
  ## the last If this is the first char it will throw a series of
  ## exceptions which are caught and cause appropriate values to be
  ## set for later.

  ## This differs from C figlet which will just get bogus values from
  ## memory and then discard them after.

  if ((options$smush_mode %&% (kern$smush %|% kern$kern)) == 0) {
    return(0L)
  }

  ## It's possible that we could do this in one go but it would take
  ## some fiddling as it's all corner cases.

  max_smush <- state$curr_char_width

  for (row in seq_len(options$height)) {
    line_left  <- buffer[row, ]
    line_right <- cur_char[row, ]
    if (options$right_to_left) {
      ll <- line_left
      line_left <- line_right
      line_right <- ll
    }

    linebd <- length_rstrip(line_left)
    if (linebd < 1L) {
      linebd <- 1L
    }

    if (linebd <= length(line_left)) {
      ch1 <- line_left[linebd]
    } else {
      linebd <- 1L
      ch1 <- ""
    }

    charbd <- length(line_right) - length_lstrip(line_right) + 1L
    if (charbd <= length(line_right)) {
      ch2 <- line_right[charbd]
    } else {
      charbd <- length(line_right) + 1L
      ch2 <- ""
    }

    amt <- (charbd - 1L) + length(line_left) - 1L - (linebd - 1L)

    if (ch1 == "" || ch1 == " ") {
      amt <- amt + 1L
    } else if (ch2 != "" && !is.null(smush_chars(ch1, ch2, options, state))) {
      amt <- amt + 1L
    }

    if (amt < max_smush) {
      max_smush <- amt
    }
  }

  max_smush
}


vsmush_amount <- function(buffer, cur_line, options) {
  max_smush <- options$height
  for (col in seq_len(min(ncol(buffer), ncol(cur_line)))) {
    line_above <- buffer[, col]
    line_below <- cur_line[, col]

    linebd <- length_rstrip(line_above)
    if (linebd < 1L) {
      linebd <- 1L
    }
    ch1 <- line_above[linebd]

    charbd <- length(line_below) - length_lstrip(line_below) + 1L
    if (charbd <= length(line_below)) {
      ch2 <- line_below[charbd]
    } else {
      charbd <- length(line_below) + 1L
      ch2 <- ""
    }

    amt <- (charbd - 1L) + length(line_above) - 1L - (linebd - 1L)

    if (ch1 == "" || ch1 == " ") {
      amt <- amt + 1L
    } else if (ch2 != "" && !is.null(vsmush_chars(ch1, ch2, options))) {
      amt <- amt + 1L
    }

    if (amt < max_smush) {
      max_smush <- amt
    }
  }

  max_smush
}


vsmush_chars <- function(above, below, options) {
  ## Given 2 characters which represent the edges rendered figlet
  ## fonts where they would touch, see if they can be smushed
  ## together.  Returns NULL if this cannot or should not be done.
  if (is_space(above)) {
    return(below)
  } else if (is_space(below)) {
    return(above)
  }

  smush_mode <- options$smush_mode

  ## kerning only
  if ((smush_mode %&% kern$vsmush) == 0) {
    return(NULL)
  }

  if (smush_mode %&% kern$vequal) {
    if (above == below) {
      return(above)
    }
  }

  smushes <- list()
  if (smush_mode %&% kern$vlowline) {
    smushes <- c(smushes, list(c("_", "|/\\[]{}()<>")))
  }

  if (smush_mode %&% kern$vhierarchy) {
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
    ## This is where things gets annoying; we don't have character-wise
    ## "in" like python.
    if (above %cin% a && below %cin% b) {
      return(below)
    }
    if (below %cin% a && above %cin% b) {
      return(above)
    }
  }

  if (smush_mode %&% kern$vhoriz) {
    if ((above == "-" && below == "_") || (above == "_" && below == "-")) {
      return("=")
    }
  }

  NULL
}
