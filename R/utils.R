## Utilities:
is_space <- function(x) {
  grepl("\\s", x, perl=TRUE)
}
rstrip <- function(x) {
  sub("\\s+$", "", x, perl=TRUE)
}
lstrip <- function(x) {
  sub("^\\s+", "", x, perl=TRUE)
}
asc <- function(x) {
  strtoi(charToRaw(x), 16L)
}

## Bit operation helpers:
##' @importFrom bitops bitAnd bitOr
`%&%` <- bitops::bitAnd
`%|%` <- bitops::bitOr

## "character in"
`%cin%` <- function(c, set) {
  grepl(c, set, fixed=TRUE)
}

is_directory <- function(x) {
  file.info(x)$isdir
}

## Like readLines, but ignore incomplete final lines (still warn about
## embedded nuls though)
read_lines <- function(...) {
  ignore_incomplete <- function(e) {
    if (!grepl("^incomplete final line found on", e$message)) {
      warning(e)
    }
  }
  tryCatch(readLines(...), warning=ignore_incomplete)
}
