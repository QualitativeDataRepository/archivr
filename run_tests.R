if(!"testthat" %in% rownames(installed.packages())) {
  install.packages("testthat", repos="http://cran.us.r-project.org")
}
library(testthat)
source("archivr.R")
test_results <- test_file("archivr_test.R", reporter="summary")
