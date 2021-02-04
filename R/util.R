vcapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, character(1), ...)
}


last <- function(x) {
  x[[length(x)]]
}


read_lines <- function(path) {
  drop_trailing_empty(readLines(path, warn = FALSE))
}


drop_trailing_empty <- function(x) {
  nonempty <- which(nzchar(x))
  if (length(nonempty) > 1 && last(nonempty) < length(x)) {
    x <- x[seq_len(last(nonempty))]
  }
  x
}


drop_leading_empty <- function(x) {
  nonempty <- which(nzchar(x))
  if (length(nonempty) > 1 && nonempty[[1]] > 1) {
    x <- x[-seq_len(nonempty[[1]] - 1L)]
  }
  x
}


rfiglet_file <- function(path) {
  system.file(path, package = "rfiglet", mustWork = TRUE)
}


`%&%` <- function(a, b) { # nolint
  bitwAnd(a, b)
}


`%|%` <- function(a, b) { # nolint
  bitwOr(a, b)
}


## "character in"
`%cin%` <- function(c, set) {
  grepl(c, set, fixed = TRUE)
}


## This will not at all cope with UTF-8, but we spend our time reading
## character support for it!
asc <- function(x) {
  strtoi(charToRaw(x), 16L)
}


is_space <- function(x) {
  grepl("\\s", x, perl = TRUE)
}


length_lstrip <- function(x) {
  if (length(x) == 0) {
    return(0L)
  }
  i <- x != " "
  if (i[[1]]) {
    return(length(i))
  }
  if (!any(i)) {
    return(0L)
  }
  length(i) - which(i)[[1]] + 1L
}


length_rstrip <- function(x) {
  if (length(x) == 0) {
    return(0L)
  }
  i <- x != " "
  n <- length(x)
  if (i[[n]]) {
    return(n)
  }
  if (!any(i)) {
    return(0L)
  }
  last(which(i))
}


matrix_to_text <- function(m, strip) {
  ret <- apply(m, 1, paste, collapse = "")
  if (strip) {
    ret <- sub(" +$", "", ret)
    ret <- drop_trailing_empty(ret)
    ret <- drop_leading_empty(ret)
  }
  ret
}


message_verbose <- function(msg, verbose) {
  if (verbose) {
    message(msg)
  }
}


copyenv <- function(from, to) {
  for (i in names(from)) {
    to[[i]] <- from[[i]]
  }
}


with_dir <- function(path, code) {
  owd <- setwd(path)
  on.exit(setwd(owd))
  force(code)
}


`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}


download_file <- function(url, dest, verbose) {
  utils::download.file(url, dest, mode = "wb", quiet = !verbose)
  dest
}
