context("Test archving core functionality")
# We're only running the essential archiving functions on CRAN and Travis to not unecessarily spam the IA

test_that("archiving a list of URLs in Wayback works", {
  skip_on_cran()
  skip_on_travis()
  urls <- c("https://qdr.syr.edu", "https://cran.r-project.org/")
# archive in Wayback machine
  archivedURLs <- archiv(urls)
  expect_true(is.data.frame((archivedURLs)))
  expect_equal(nrow(archivedURLs), 2) # two URLs
  expect_equal(ncol(archivedURLs), 4) # four observations
  expect_true(as.logical(archivedURLs$available[1])) # Existing URL works fine
})

test_that("including a broken URL fails gracefully", {
  skip_on_cran()
  skip_on_travis()
  urls <- c("https://qdr.syr.edu", "doesnotexist.r-project.org/")
  # archive in Wayback machine
  archivedURLs <- archiv(urls)
  expect_true(is.data.frame((archivedURLs)))
  expect_equal(nrow(archivedURLs), 2) # two URLs
  expect_equal(ncol(archivedURLs), 4) # four observations
  expect_true(as.logical(archivedURLs$available[1])) # Existing URL works fine
  expect_false(as.logical(archivedURLs$available[2])) # non-existing URLs produce FALSE
  
})

test_that("produces warning message for invalid URL", {
  expect_message(archiv(c("doesnotexist.r-project.org")), "Discovered an error in saving the url")
})

test_that("archiving a single URL in Wayback works", {
  skip_on_cran()
  skip_on_travis()
  archivedURL <- archiv_wayback("https://qdr.syr.edu")
  expect_length(archivedURL, 4)
  expect_true(as.logical(archivedURL[2])) # This should work
})


test_that("archiving from a URL in  Wayback works, including excepting URL ", {
  skip_on_cran()
  skip_on_travis()
  archivedURLs <- archiv.fromUrl(
    "https://www-cs-faculty.stanford.edu/~knuth/retd.html",
    except="validator\\.w3\\.org|stanford"
  )
  expect_true(is.data.frame((archivedURLs)))
  expect_equal(nrow(archivedURLs), 1) # one URL
  expect_equal(ncol(archivedURLs), 4) # four variables
  expect_true(as.logical(archivedURLs$available[1])) # this should work
})

# More relevant testing on this in test-extract.R
test_that("archiving from a document works, including excepting URL patters", {
  skip_on_cran()
  skip_on_travis()
  archivedURLs <- archiv.fromText("../data/testdoc.docx", except="doi\\.org\\/")
  expect_true(is.data.frame((archivedURLs)))
  expect_equal(nrow(archivedURLs), 3) # three URLs
  expect_equal(ncol(archivedURLs), 4) # four observations
  expect_true(as.logical(archivedURLs$available[1])) # this should work
})

