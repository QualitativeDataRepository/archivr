test_that("Test setting api key",{
  key <- "BOGUSKEY"
  test <- set_api_key(key)
  expect_equal(key, .perma_cc_key)
})

test_that("Test getting real url from Wayback", {
  url <- "www.example.com"
  test <- from_wayback(url)
  status <- list ("200", TRUE, "http://web.archive.org/web/20181214200505/http://Example.com")
  expect_equal(test$url, url)
  expect_equal(list(test$archived_snapshots$closest$status,
    test$archived_snapshots$closest$available,
    test$archived_snapshots$closest$url), status)
})

test_that("Archivr function returns proper df", {
  lurls <- c("www.example.com", "NOTAURL", "www.github.com")
  test <- archiv(lurls, "wayback")
  expectedA <- as.vector(lurls)
  expectedB <- c("200", "000", "200")
  expectedC <- c("http://web.archive.org/web/20181214200505/http://Example.com",
    "url not found", "http://web.archive.org/web/20181214210708/https://github.com/")
  expect_equal(as.vector(test[,"url"]), expectedA)
  expect_equal(as.vector(test$status), expectedB)
  ## Cannot check way_back_urls due to timestamp
  ## expect_equal(as.vector(test$wayback_url), expectedC)
})

test_that("Parses links from markdown text", {
  md <- paste0('# A HEADING',
    '\n',
    'Some text with a [url](http://www.example.com)\n',
    'Some text with a [url](http://www.github.com "and a title")\n',
    'And a url by itself http://www.google.com\n',
    'A url enclosed in diamond brackets <http://www.apple.com\n',

)
})
