context("Checking view_ functionality")
test_that("checking mixed list in both archives", {
  urls <- c("https://qdr.syr.edu", "https://cran.r-project.org/", "https://apsa.net")
  checkArchiveStatus <- view_archiv(urls, method="both")
  expect_true(is.data.frame((checkArchiveStatus)))
  expect_equal(nrow(checkArchiveStatus), 3) # two URLs
  expect_equal(ncol(checkArchiveStatus), 6) # six variables
  expect_true(as.logical(checkArchiveStatus$available[1])) # Existing URL works fine
  expect_true(as.logical(checkArchiveStatus$available[3])) # URL only in WB still returns true
  expect_equal(as.character(checkArchiveStatus$perma_cc_url[3]), "url not found")
})


test_that("checking non-existing URL in both archives", {
  urls <- c("https://qdr.syr.edu", "https://cran.r-project.org/", "https://doesnotexist.r-project.org")
  checkArchiveStatus <- view_archiv(urls, method="both")
  expect_true(is.data.frame((checkArchiveStatus)))
  expect_equal(nrow(checkArchiveStatus), 3) # two URLs
  expect_equal(ncol(checkArchiveStatus), 6) # six variables
  expect_true(as.logical(checkArchiveStatus$available[1])) # Existing URL works fine
  expect_false(as.logical(checkArchiveStatus$available[3])) # URL doesn't exist, shouldn't be anywhere
})

