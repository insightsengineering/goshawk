# testthat is a suggested package: only run the tests when it is available.
if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(goshawk)
  test_check("goshawk")
}
