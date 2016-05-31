context("fonts")

test_that("load (ours)", {
  for (font in figlet_base_fonts()) {
    expect_is(figlet_font(font), "figlet_font")
  }
})

## TODO: dropped extra characters for
##   - cns
##   - jiskan16
##   - jacky
## NOTE: This generates ~150MB of cached data.
test_that("load (extended)", {
  for (font in figlet_fonts()) {
    expect_is(figlet_font(font), "figlet_font")
  }
})

test_that("fonts", {
  text <- "This is a test"

  fonts <- ls(font_cache)
  res <- vector("list", length(fonts))
  names(res) <- fonts

  ## Looks like this is loading again, when it should be fetching...
  for (font in fonts) {
    message(font)
    res[[font]] <- try(figlet(text, font))
    expect_is(res[[font]], "figlet_string")
  }

  skip_if_no_cfiglet()

  cmp <- res
  cmp[] <- list(NULL)
  for (font in fonts) {
    cmp[[font]] <- cfiglet(text, font)
    if (font_is_r2l(font_cache[[font]])) {
      cmp[[font]] <- trim_left(cmp[[font]])
    }
  }

  for (font in fonts) {
    if (font_is_r2l(font_cache[[font]])) {
      # skip("r2l not implemented properly")
      next
    }
    if (length(cmp[[font]]) > length(res[[font]]$string)) {
      next
    }
    if (any(grepl("\n", cmp[[font]], fixed=TRUE))) {
      # skip("newlines not implemented yet")
      next
    }
    expect_identical(as.character(res[[font]]),
                     paste0(cmp[[font]], collapse="\n"))
  }
})

test_that("load nonexistant fonts", {
  expect_error(figlet("foo", "asfA"), "Font asfA not found")
  expect_error(figlet("foo", "bdffonts::chartri"), NA)
  expect_error(figlet("foo", "bdffonts::chartri2"), "Font not found")
  expect_error(figlet("foo", "bdffonts2::chartri"), "Font not found")
})
