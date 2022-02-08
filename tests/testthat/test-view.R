context("Checking view_ functionality")
test_that("checking mixed list in both archives", {
  urls <- c("https://qdr.syr.edu", "https://cran.r-project.org/", "https://apsa.net")
  checkArchiveStatus <- view_archiv(urls, method="all")
  expect_true(is.data.frame((checkArchiveStatus)))
  expect_equal(nrow(checkArchiveStatus), 3) # three URLs
  expect_equal(ncol(checkArchiveStatus), 8) # EIGHT variables (up from six before archive.md)
  expect_true(as.logical(checkArchiveStatus$available[1])) # Existing URL works fine
  expect_true(as.logical(checkArchiveStatus$available[3])) # URL only in WB still returns true
  expect_equal(as.character(checkArchiveStatus$perma_cc_url[3]), "url not found")
})

test_that("checking mixed list in just wayback", {
  urls <- c("https://qdr.syr.edu", "https://doesnotexist.r-project.org/", "https://apsa.net")
  checkArchiveStatus <- view_archiv(urls, method="wayback")
  expect_true(is.data.frame((checkArchiveStatus)))
  expect_equal(nrow(checkArchiveStatus), 2) # three URLs
  expect_equal(ncol(checkArchiveStatus), 4) # four variables
  expect_true(as.logical(checkArchiveStatus$available[1])) # Existing URL works fine
  #expect_false(as.logical(checkArchiveStatus$available[2])) # URL doesn't exist
})

test_that("checking mixed list in just perma.cc", {
  urls <- c("https://qdr.syr.edu", "https://cran.r-project.org/", "https://www.example.com")
  checkArchiveStatus <- view_archiv(urls, method="perma_cc")
  expect_true(is.data.frame((checkArchiveStatus)))
  expect_equal(nrow(checkArchiveStatus), 3) # three URLs
  expect_equal(ncol(checkArchiveStatus), 4) # four variables
  expect_true(as.logical(checkArchiveStatus$available[1])) # Existing URL works fine
  expect_match(as.character(checkArchiveStatus$perma_cc_url[1]), "https://perma.cc", fixed = TRUE)
  #expect_false(as.logical(checkArchiveStatus$available[3])) # URL does not exist
  #expect
  #expect_equal(as.character(checkArchiveStatus$perma_cc_url[3]), "url not found")
})

test_that("checking non-existing URL in both archives", {
  urls <- c("https://qdr.syr.edu",  "https://doesnotexist.r-project.org")
  checkArchiveStatus <- view_archiv(urls, method="all")
  expect_true(is.data.frame((checkArchiveStatus)))
  expect_equal(nrow(checkArchiveStatus), 1) # One URL, because the other one gets dropped
  expect_equal(ncol(checkArchiveStatus), 8) # EIGHT variables
  expect_true(as.logical(checkArchiveStatus$available[1])) # Existing URL works fine
  #expect_false(as.logical(checkArchiveStatus$available[2])) # URL doesn't exist, shouldn't be anywhere
})

test_that("viewing archiving status from document works", {
  checkArchiveStatus <- view_archiv.fromText("../data/testdoc.docx")
  expect_true(is.data.frame((checkArchiveStatus)))
  expect_equal(nrow(checkArchiveStatus), 3) # THREE URLs, one is fake
  expect_equal(ncol(checkArchiveStatus), 4) # four variables
  expect_true(as.logical(checkArchiveStatus$available[1])) # Existing URL works fine
  #expect_false(as.logical(checkArchiveStatus$available[3])) # URL doesn't exist
})

test_that("viewing archiving status from URL works", {
  checkArchiveStatus <- view_archiv.fromUrl("https://www-cs-faculty.stanford.edu/~knuth/retd.html")
  expect_true(is.data.frame((checkArchiveStatus)))
  expect_equal(nrow(checkArchiveStatus), 4) # four URLs
  expect_equal(ncol(checkArchiveStatus), 4) # four variables
  expect_true(as.logical(checkArchiveStatus$available[1])) # Existing URL works fine
})
