#' This tests the scripts that are supposed to be used to run modules/models of DESIRE (used in Kikstra et al. 2024 in ERL)
#'
#' Currently passes locally.

library(testthat)
TEST <<- TRUE

test_that("calculator_run.R runs without errors",{
  expect_no_error(source(here::here('R','calculator_run.R')))
})

# End the test file by removing all test-data
TEST <<- FALSE
set_data_location(test=TEST)
remove_test_data_output()
