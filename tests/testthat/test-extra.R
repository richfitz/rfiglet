context("extra")

test_that("Can find fonts in extra", {
  testthat::skip_if_offline()
  figlet_download_fonts(verbose = FALSE)
  font <- figlet_font("emboss")
  expect_s3_class(font, "figlet_font")
})


test_that("Can download fonts to directory", {
  skip_if_not_installed("mockery")
  skip_if_no_extra_fonts()
  path_extra <- extra_font_dir()
  tmp <- tempfile(fileext = ".zip")
  fonts <- dir(path_extra)[1:10]
  with_dir(dirname(path_extra),
           zip(tmp, file.path(basename(path_extra), fonts),
               "-qr9X"))
  mock_download_file <- mockery::mock(tmp)

  path <- tempfile()
  with_mock(
    "rfiglet:::download_file" = mock_download_file,
    download_fonts(path, FALSE))

  expect_setequal(dir(path), fonts)
  expect_message(download_fonts(path, TRUE),
                 "Fonts already exist at")
  mockery::expect_called(mock_download_file, 1)
})
