context("font")

test_that("can load all system fonts", {
  path <- rfiglet_file("fonts")
  fonts <- dir(path, full.names = TRUE)
  for (f in fonts) {
    font <- figlet_font_read(f)
    expect_s3_class(font, "figlet_font")
  }
})


test_that("Sensible error messages on malformed fonts", {
  p <- tempfile(fileext = ".flf")
  expect_error(
    figlet_font_read(p),
    "'file.*?' \\(.+?\\) does not exist")
  file.create(p)
  expect_error(
    figlet_font_read(p),
    "'file.*?' \\(.+?\\) is empty")
  writeLines("not a real font", p)
  expect_error(
    figlet_font_read(p),
    "'file.*?' \\(.+?\\) is not a valid font")
  writeLines("flf2a$ 8 6 59", p)
  expect_error(
    figlet_font_read(p),
    "'file.*?' \\(.+?\\) has a malformed header")
})


test_that("Correct options for font read corner cases", {
  ## extra/bdffonts/5x7.flf
  expect_equal(
    figlet_font_options("flf2a$ 7 6 25 -1 46", "5x7.flf", "5x7"),
    list(hard_blank = "$", height = 7, base_line = 6, max_length = 25,
         old_layout = -1, comment_lines = 46,
         print_direction = NA_integer_, full_layout = 0, smush_mode = 0,
         right_to_left = FALSE))
  ## extra/C64-fonts/1943____.flf
  expect_equal(
    figlet_font_options("flf2a$ 8 7 11 1 7", "1943____.flf", "1943"),
    list(hard_blank = "$", height = 8, base_line = 7, max_length = 11,
         old_layout = 1, comment_lines = 7,
         print_direction = NA_integer_, full_layout = 129L, smush_mode = 129L,
         right_to_left = FALSE))
  ## in extra/contributed/acrobatic.flf
  expect_equal(
    figlet_font_options("flf2a$ 12 9 25 0 15 0", "acrobatic.flf", "acrobatic"),
    list(hard_blank = "$", height = 12, base_line = 9, max_length = 25,
         old_layout = 0, comment_lines = 15,
         print_direction = 0, full_layout = 64L, smush_mode = 64L,
         right_to_left = FALSE))
})


test_that("Escape special symbols when handling characters", {
  ## See toilet/emboss for an example of this, from which this test is
  ## derived (though using the more typical symbols in the "big" font)
  x <- c("    _  _   @", "  _| || |_ @", " |_  __  _|@", "  _| || |_ @",
         " |_  __  _|@", "   |_||_|  @", "           @", "           @@")
  expected <- list(
    width = 11,
    data = matrix(unlist(strsplit(gsub("@", "", x), NULL)), length(x),
                  byrow = TRUE))
  options <- list(height = length(x), hard_blank = "$")
  expect_equal(figlet_font_character(x, options),
               expected)
  expect_equal(figlet_font_character(gsub("@", "^", x, fixed = TRUE), options),
               expected)
  expect_equal(figlet_font_character(gsub("@", "\\", x, fixed = TRUE), options),
               expected)
})


test_that("Detect corrupt extended region", {
  src <- rfiglet_file("fonts/big.flf")
  dst <- tempfile()
  on.exit(unlink(dst))
  writeLines(readLines(src)[-2000], dst)
  expect_message(
    f <- figlet_font_read(dst, extended = TRUE),
    "Possibly corrupt font '.*?' \\(.+?\\); discarding extra characters")
  cmp <- figlet_font_read(rfiglet_file("fonts/big.flf"), extended = TRUE)
  expect_lt(length(f$chars), length(cmp$chars))
  expect_lt(length(f$chars), 256)
})


test_that("print font", {
  font <- figlet_font("standard")
  expect_output(
    print(font),
    "<figlet_font object: standard>",
    fixed = TRUE)
  expect_output(
    print(font),
    paste0(figlet_render("standard", font), collapse = "\n"),
    fixed = TRUE)
  expect_length(capture.output(print(font, FALSE)), 1)
  expect_length(capture.output(print(font)), 6)
})



test_that("can load all system fonts into registry", {
  registry_clear()
  expect_equal(figlet_font_list(), character(0))

  path <- rfiglet_file("fonts")
  registry_load_dir(path, verbose = TRUE)
  expected <- c("banner", "big", "block", "bubble", "digital", "ivrit", "lean",
                "mini", "mnemonic", "script", "shadow", "slant", "small",
                "smscript", "smshadow", "smslant", "standard", "term")
  expect_setequal(figlet_font_list(), expected)

  registry_clear()
  expect_equal(figlet_font_list(), character(0))
  rfiglet:::.onLoad()
  expect_setequal(figlet_font_list(), expected)
})


test_that("Sensible errors when not finding fonts", {
  expect_error(
    figlet_font("asdfa"),
    "Font 'asdfa' not found in registry")
  expect_error(
    figlet_font("asdfa.flf"),
    "File 'asdfa.flf' not found")
  expect_error(
    figlet_font("a/asdfa"),
    "File 'a/asdfa' not found")
})


test_that("Load font from file", {
  skip_if_no_extra_fonts("avatar")
  registry_clear()
  path <- file.path(extra_font_dir(), "avatar.flf")
  font <- figlet_font(path)
  expect_is(font, "figlet_font")
  expect_message(figlet_font(path, TRUE), "already loaded")
})
