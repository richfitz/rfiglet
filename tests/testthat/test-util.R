context("util")

test_that("read_lines discards trailing whitespace", {
  p <- tempfile()
  on.exit(unlink(p))
  writeLines(c(letters, "", "", ""), p)
  expect_equal(read_lines(p), letters)
})


test_that("Can download", {
  testthat::skip_if_offline()
  dest <- tempfile()
  res <- download_file("https://httpbin.org/get", dest, FALSE)
  expect_equal(res, dest)
  expect_true(file.exists(res))
})


test_that("null-or-other", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
  expect_equal(1 %||% NULL, 1)
  expect_null(NULL %||% NULL)
})
