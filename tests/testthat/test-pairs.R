context("character pairs")

test_that("character pairs", {
  skip_if_no_cfiglet()
  chars <- c(letters, LETTERS, 0:9)
  ab <- expand.grid(chars, chars, stringsAsFactors=FALSE)
  ab <- apply(ab, 1, paste0, collapse="")

  r <- make_figlet()

  for (pair in ab) {
    cmp <- paste(cfiglet(pair), collapse="\n")
    expect_identical(as.character(r(pair)), cmp)
  }
})
