test_that("Test setting api key",{
  key <- "BOGUSKEY"
  test <- set_api_key(key)
  expect_equal(key, .perma_cc_key)
})

test_that("Test getting real url from Wayback", {
  url <- "www.example.com"
  test <- from_wayback(url)
  status <- list ("200", TRUE)
  expect_equal(test$url, url)
  expect_equal(list(test$archived_snapshots$closest$status,
    test$archived_snapshots$closest$available), status)
  expect_true(!is.na(test$archived_snapshots$closest$url))
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
  expect_equal(length(as.vector(test$wayback_url)), 3)
  ## Cannot check way_back_urls due to timestamp
  ## expect_equal(as.vector(test$wayback_url), expectedC)
})

test_that("Parses links from markdown text", {
  md <- paste0('# A HEADING',
    '\n',
    'Some text with a [url](http://www.example.com)\n',
    'Some text with a [url](http://www.github.com "and a title")\n',
    'And a url by itself http://www.google.com\n',
    'A url enclosed in diamond brackets <http://www.apple.com>\n'
  )
  test <- extract_urls_from_text(md)
  expect_equal(unname(test), c("http://www.example.com",
    "http://www.github.com",
    "http://www.google.com", "http://www.apple.com"))
  test2 <- archiv.fromText(md)
  expect_equal(length(as.vector(test2$wayback_url)), 4)
})

test_that("Parses links from Latex", {
  latex <- paste0 ('\\begin{equation}\n \\label{eq:1}\\sum_{i=0}^{\\infty} a_i x^i',
    '\\end{equation}\nThe equation \\ref{eq:1} shows a sum that is divergent.',
    'This formula will later be used in the page \\pageref{second}.',
    'For further references see \\href{http://www.sharelatex.com}{Something',
    'Linky} or go to the next url: \\url{http://www.sharelatex.com} or open',
    'the next file \\href{run:./file.txt}{File.txt}',
    '\\url{https://www.google.com/file/path.html}',
    'Its also possible to link directly any word or',
    '\\hyperlink{thesentence}{any sentence} in your document.',
    '\\end{document}')
  test <- extract_urls_from_text(latex)
  expect_equal(unname(test), c("http://www.sharelatex.com", "https://www.google.com/file/path.html"))
  test2 <- archiv.fromText(latex)
  expect_equal(length(as.vector(test2$wayback_url)), 2)
})

test_that("Reads docx etc. from folders", {
  filepath <- "./data/*.docx"
  test <- extract_urls_from_folder(filepath)
  expect_equal(unname(test), c("https://www.google.com/path/to/something",
    "http://www.apple.com", "http://www.example.com.", "http://qdr.syr.edu.",
  "http://www.uwaterloo.ca."))
})
