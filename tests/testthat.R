library(testthat)

test_results <- test_check("goshawk")
saveRDS(test_results, "unit_testing_results.rds")