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
`%&%` <- bitops::bitAnd
`%|%` <- bitops::bitOr

## "character in"
`%cin%` <- function(c, set) {
  grepl(c, set, fixed=TRUE)
}
