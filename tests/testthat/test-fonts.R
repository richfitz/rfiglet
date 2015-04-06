context("fonts")

## Ways this can fail:

## 1. Can't open the font

test_that("load", {
  for (font in figlet_fonts()) {
    expect_is(figlet_font(font), "figlet_font")
  }
})

test_that("fonts", {
  text <- "This is a test"

  fonts <- ls(font_cache)
  res <- vector("list", length(fonts))
  names(res) <- fonts

  for (font in fonts) {
    res[[font]] <- try(figlet(text, font))
    expect_is(res[[font]], "figlet_string")
  }

  skip_if_no_cfiglet()

  trim_left <- function(x) {
    m <- strsplit(x, NULL)
    m <- do.call("rbind", m, quote=TRUE)
    i <- min(which(colSums(m != " ") > 0))
    m <- m[, i:ncol(m), drop=FALSE]
    apply(m, 1, paste0, collapse="")
  }

  cmp <- res
  cmp[] <- list(NULL)
  for (font in fonts) {
    cmp[[font]] <- cfiglet(text, font)
    if (font_is_r2l(font_cache[[font]])) {
      cmp[[font]] <- trim_left(cmp[[font]])
    }
  }

  join <- function(x) paste0(x, collapse="\n")
  f <- function(a, b) {
    as.character(a) == join(b)
  }

  for (font in fonts) {
    if (font_is_r2l(font_cache[[font]])) {
      # skip("r2l not implemented properly")
      next
    }
    expect_identical(as.character(res[[font]]),
                     join(cmp[[font]]))
  }
})
