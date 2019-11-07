context("test_archivr.R - Test the Archivr Package.")

test_that("Test setting api key",{
  key <- "BOGUSKEY"
  test <- set_api_key(key)
  expect_equal(key, get_api_key())
})

test_that("Parses links from markdown text", {
  md <- paste0('# A HEADING',
               '\n',
               'Some text with a [url](http://www.example.com)\n',
               'Some text with a [url](http://www.github.com "and a title")\n',
               'And a url by itself http://www.google.com\n',
               'A url enclosed in diamond brackets <http://www.apple.com\n'
  )
})