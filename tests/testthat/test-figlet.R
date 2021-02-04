context("figlet")

test_that("basic use", {
  res <- figlet_render("Hello!", figlet_font("standard"), strip = FALSE)
  expect_equal(
    res,
    c(" _   _      _ _       _ ",
      "| | | | ___| | | ___ | |",
      "| |_| |/ _ \\ | |/ _ \\| |",
      "|  _  |  __/ | | (_) |_|",
      "|_| |_|\\___|_|_|\\___/(_)",
      "                        "))
})


test_that("Don't smush non-smushable fonts", {
  expect_equal(figlet_render(")(", figlet_font("mnemonic")), ")(")
})


test_that("Collapse to pipe", {
  expect_equal(
    figlet_render("ii", figlet_font("standard"), strip = FALSE),
    c(" _ _ ", "(_|_)", "| | |", "| | |", "|_|_|", "     "))
  ##              ^-- was )(
})


test_that("Big-X kerning", {
  skip_if_no_extra_fonts("avatar")
  ## This requires a different font that will not always be installed
  ca <- c(" ____ ____ ", "/   _Y  _ \\", "|  / | / \\|", "|  \\_| |-||",
          "\\____|_/ \\|", "           ")

  font <- figlet_font("avatar")
  expect_equal(figlet_render("ca", font, strip = FALSE), ca)

  state <- list(curr_char_width = 6, prev_char_width = 6)
  expect_equal(smush_chars(">", "<", font$options, state), "X")
  expect_equal(smush_chars("\\", "/", font$options, state), "Y")
  expect_equal(smush_chars("/", "\\", font$options, state), "|")
})


test_that("universal overlapping", {
  ab <- c("       |    ", "  _` | __ \\ ", " (   | |   |", "\\__,_|_.__/ ",
          "            ")
  font <- figlet_font("shadow")
  expect_equal(figlet_render("ab", font, strip = FALSE), ab)

  state <- list(curr_char_width = 6, prev_char_width = 6)
  expect_equal(smush_chars(".", "$", font$options, state), ".")
  expect_equal(smush_chars("$", ".", font$options, state), ".")
})


test_that("kerning only", {
  skip_if_no_extra_fonts("slscript")
  fj <- c("       ", "    /) ", "   // o", "  //_/_", " /> /  ", "</-'   ")
  font <- figlet_font("slscript")
  expect_equal(figlet_render("fj", font, strip = FALSE), fj)
  state <- list(curr_char_width = 6, prev_char_width = 6)
  expect_null(smush_chars(".", ".", font$options, state))
})


test_that("Treatment of adjoining hardblanks", {
  state <- list(curr_char_width = 6, prev_char_width = 6)
  options <- list(hard_blank = "$", smush_mode = 129L, right_to_left = FALSE)
  expect_null(smush_chars("$", "$", options, state))
  options$smush_mode <- 8127
  expect_equal(smush_chars("$", "$", options, state), "$")
})


test_that("Treatment of short characters", {
  skip_if_no_extra_fonts("slscript")
  ref <- c("       ", "  / o  ", " /__'_.", "/_) (__", "       ", "       ")
  font <- figlet_font("slscript")
  expect_equal(figlet_render("b'c", font, strip = FALSE), ref)
  expect_null(smush_chars(".", ".", font$options,
                          list(prev_char_width = 5, curr_char_width = 1)))
  expect_null(smush_chars(".", ".", font$options,
                          list(prev_char_width = 1, curr_char_width = 5)))
})


test_that("Right-to-left", {
  skip_if_no_extra_fonts("mirror")
  mirror <- figlet_render_horizontal("hello", figlet_font("mirror"))
  std <- figlet_render_horizontal("hello", figlet_font("standard"))

  i <- rev(seq_len(ncol(std)))
  str_cmp <- chartr("/\\()`'", "\\/)('`", std[, i])

  expect_equal(mirror, str_cmp)
})


test_that("Error if font does not contain characters", {
  font <- figlet_font("standard")
  font$chars[120] <- list(NULL)
  expect_error(
    figlet_render("exact", font),
    "The font 'standard' does not contain the characters 'x'")
  expect_silent(
    figlet_render("eact", font))
})


test_that("vertical layout", {
  text <- "a quick brown fox"
  expected <- c(
    "                     _      _",
    "  __ _    __ _ _   _(_) ___| | __",
    " / _` |  / _` | | | | |/ __| |/ /",
    "| (_| | | (_| | |_| | | (__|   <",
    " \\__,_|  \\__, |\\__,_|_|\\___|_|\\_\\",
    " | |__  _ __|_|___      ___ __",
    " | '_ \\| '__/ _ \\ \\ /\\ / / '_ \\",
    " | |_) | | | (_) \\ V  V /| | | |",
    " |_.__/|_| _\\___/ \\_/\\_/ |_| |_|",
    "          / _| _____  __",
    "         | |_ / _ \\ \\/ /",
    "         |  _| (_) >  <",
    "         |_|  \\___/_/\\_\\")

  font <- figlet_font("standard")
  expect_equal(
    figlet_render(text, font, width = 45, justify = "centre", strip = TRUE),
    expected)
})


test_that("strip", {
  ace1 <- c("  __ _  ___ ___", " / _` |/ __/ _ \\", "| (_| | (_|  __/",
            " \\__,_|\\___\\___|")
  ace2 <- c("                ", "  __ _  ___ ___ ", " / _` |/ __/ _ \\",
            "| (_| | (_|  __/", " \\__,_|\\___\\___|", "                ")
  text <- "ace"
  font <- figlet_font("standard")
  expect_equal(figlet_render(text, font), ace1)
  expect_equal(figlet_render(text, font, strip = FALSE), ace2)
})


test_that("force newlines", {
  text1 <- "a quick\nbrown\nfox"
  text2 <- c("a quick", "brown", "fox")
  text3 <- sub("\n", " ", text1)
  font <- figlet_font("standard")

  expect_equal(
    figlet_render(text1, font, justify = "centre", strip = TRUE),
    figlet_render(text3, font, justify = "centre", strip = TRUE, width = 45))
  expect_equal(
    figlet_render(text2, font, justify = "centre", strip = TRUE),
    figlet_render(text3, font, justify = "centre", strip = TRUE, width = 45))
})



test_that("vertical combination", {
  ## I can't find this obviously within our fonts
  font <- figlet_font("standard")

  expect_equal(vsmush_chars("_", "_", font$options), "_")
  expect_equal(vsmush_chars("_", "-", font$options), "=")
  expect_equal(vsmush_chars("-", "_", font$options), "=")
  expect_equal(vsmush_chars("-", "-", font$options), "-")
})


test_that("kerning only", {
  skip_if_no_extra_fonts("slscript")
  font <- figlet_font("slscript")
  expect_equal(figlet_render("=\n=", font, strip = TRUE), rep("---", 4))
  expect_null(vsmush_chars(".", ".", font$options))
})


test_that("print strings", {
  res <- figlet("Hello!", strip = FALSE)
  expect_s3_class(res, "figlet_text")
  expect_equal(attr(res, "text"), "Hello!")
  expect_equal(attr(res, "font"), "standard")
  expect_output(
    print.figlet_text(res),
    paste0(res, "\n", collapse = ""))
})


test_that("Coersion", {
  res <- figlet("Hello!", strip = TRUE)
  expect_equal(as.character(res), paste0(res, collapse = "\n"))
})


## TODO: we might be over-agressively stripping off the left here
test_that("allow consecutive spaces in template", {
  expected <- c(
    " |   |      | |               |               |",
    " |   |  _ \\ | |  _ \\      __| __ \\   _` |  _` |  _ \\\\ \\  \\   /",
    " ___ |  __/ | | (   |   \\__ \\ | | | (   | (   | (   |\\ \\  \\ /",
    "_|  _|\\___|_|_|\\___/    ____/_| |_|\\__,_|\\__,_|\\___/  \\_/\\_/",
    "                     |     | |",
    "\\ \\  \\   / _ \\   __| |  _` | |",
    " \\ \\  \\ / (   | |    | (   |_|",
    "  \\_/\\_/ \\___/ _|   _|\\__,_|_)")
  font <- figlet_font("shadow")
  expect_equal(
    figlet_render("Hello shadow world!", font = font, width = 80),
    expected)
})


test_that("alignment", {
  text <- "Hello!"
  font <- figlet_font("standard")
  res <- figlet_render(text, font, strip = FALSE)
  expect_equal(
    figlet_render(text, font, strip = FALSE, justify = "right", width = 50),
    res)
  expect_equal(
    figlet_render(text, font, strip = FALSE, justify = "centre", width = 50),
    res)
  expect_equal(
    figlet_render(text, font, strip = FALSE, justify = "right", width = 50,
                  absolute = TRUE),
    paste0(strrep(" ", 26), res))
  expect_equal(
    figlet_render(text, font, strip = FALSE, justify = "centre", width = 50,
                  absolute = TRUE),
    paste0(strrep(" ", 13), res))
})
