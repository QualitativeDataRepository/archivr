context("Test extracting from document")

test_that("extracting URL from a string works", {
  text <- "Lorem ipsum dolor www.example.com sit amet, consectetur adipiscing elit. Aenean a posuere ex. Ut viverra mattis ante a https://qdr.syr.edu venenatis. Sed elementum quis ipsum eu cursus. Curabitur dapibus augue in elementum lobortis. Duis vitae quam ipsum. Fusce maximus massa urna, sit amet finibus dui http://www.archive.org maximus ornare. Duis enim magna, sollicitudin aliquam auctor id, efficitur vel quam. Cras egestas luctus massa eu porta. Etiam metus neque, hendrerit eu ornare eu, porta sit amet elit. Suspendisse potenti. Aenean id tincidunt turpis."
  urlList <- extract_urls_from_text(text)
  expect_length(urlList, 3)
  expect_equal(urlList[[1]], "www.example.com")
})

test_that("cleaning final characters in extracted URLs works", {
  text <- "Lorem ipsum dolor www.example.com, sit amet, consectetur adipiscing elit. Aenean a posuere ex. Ut viverra mattis ante a <https://qdr.syr.edu> venenatis. Sed elementum quis ipsum eu cursus. Curabitur dapibus augue in elementum lobortis. Duis vitae quam ipsum. Fusce maximus massa urna, sit amet finibus dui (http://www.archive.org) maximus ornare. Duis enim magna, sollicitudin aliquam auctor id, efficitur vel quam. Cras egestas luctus massa eu porta. Etiam metus neque, hendrerit eu ornare eu, porta sit amet elit. Suspendisse potenti. Aenean id tincidunt turpis."
  urlList <- extract_urls_from_text(text)
  expect_length(urlList, 3)
  expect_equal(urlList[[1]], "www.example.com")
  expect_equal(urlList[[2]], "https://qdr.syr.edu")
  expect_equal(urlList[[3]], "http://www.archive.org")
})

test_that("extracing from a .docx works, including except", {
  urlList <- extract_urls_from_text("../data/testdoc.docx", except="doi\\.org\\/")
  expect_length(urlList, 3)
  expect_equal(urlList[[1]], "www.example.com")
})

test_that("extracing from a .pdf works, including except", {
  urlList <- extract_urls_from_text("../data/testpdf.pdf", except="doi\\.org\\/")
  expect_length(urlList, 3)
  expect_equal(urlList[[1]], "www.example.com")
})

test_that("extracting from an XML file works", {
  urlList <- extract_urls_from_text("../data/testxml.xml", except="doi\\.org\\/")
  expect_length(urlList, 5)
  expect_equal(urlList[[1]], "http://ifl.sagepub.com/content/42/4/292")
})

test_that("extracting from a URL works, including except", {
  urlList <- extract_urls_from_webpage(
        "https://www-cs-faculty.stanford.edu/~knuth/retd.html",
        except="validator\\.w3\\.org")
  expect_equal(urlList[1], "http://www-sul.stanford.edu")
})

test_that("extract from non-existing URL fails gracefully",{
  urlList <- extract_urls_from_webpage(
    "http://does-not-exist.r-project.org",
    except="validator\\.w3\\.org")
  expect_length(urlList, 0)
})

