# Requirement: Function to create a scatter plot (deprecated, replaced by g_correlationplot).

test_that("g_scatterplot is defunct and points to g_correlationplot", {
  expect_error(g_scatterplot(), "g_scatterplot")
  expect_error(g_scatterplot(data = mtcars, x = "wt"), "defunct")
})
