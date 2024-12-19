#' This test tests:
#' calculator_output-lognormal-calculation
#' Testing:
#' - Input data available: activity [scenario_FE], inequality [projected_energy_inequality], and needs [projected_dle-total-and-sectoral-scaled]
#' - Input data in the right format: IAM downscaled final energy by sectors [IAM.SCENARIO.DATA.FILE], in the format of "iiasa/downscaler_repo" output
#' - Functioning of depth-of-deficit calculation function (unit test)
#' - Functioning of lognormal calculation function (unit test)
#' - For SHAPE data (as used for IAMC2022); is the expected output the same as the output produced in this test run?
#'   * same number of rows & columns
#'   * same number of NAs
#'   * share of population deprived: >=0 [both with and without efficiency effects]
#'
#'
#'# Input test, output test, unit tests (DoD and inverseCDF_lognormal), integration test
#'
#'
#'
#'  Currently passes locally;
#'
library(testthat)
source(here::here('R','utils.R'))

# runs the entire calculator below
source(here('R', 'calculator_input-options.R'))
TEST <<- TRUE
set_data_location(test=TEST)

# IAM.OUTPUT.SHORT.FILENAME <- "SHAPE-final" # checks only for SHAPE-final workflow

source(here("R", "calculator_population.R")) # required for going from downscaled EJ to GJ/cap
source(here("R", "calculator_activity-fe-process-downscaled.R")) # now called before DLE mapping, because its final energy is used to split DLE threshold mapping splits within regions for the MRIO table mapping
source(here("R", "calculator_needs-dle-input.R"))
source(here("R", "calculator_needs-dle-mapping.R"))
source(here("R", "calculator_inequality-fe-process-starting.R")) # could consider splitting processing and mapping?
source(here("R", "calculator_inequality-simple-filling-fe-process-starting.R"))
source(here("R", "calculator_needs-dle-efficiency.R"))
source(here("R", "calculator_inequality-process-income-driver.R"))
source(here("R", "calculator_inequality-project-gini-to-gini.R"))
source(here("R", "calculator_output_lognormal-calculation.R"))




# Input Test
test_that("Check input file exists.",{
  expect_true(file.exists(here(DATA.LOCATION, paste0("scenario_FE", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData"))))
  expect_true(file.exists(here(DATA.LOCATION, paste0("projected_energy_inequality", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData"))))
  expect_true(file.exists(here(DATA.LOCATION, paste0("projected_dle-total-and-sectoral-scaled", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData"))), "FE_gini_iso.RData file does not exist.")
})

#Unit Test
test_that("Depth of Deficit calculator returns NA when input data does not make sense", {
  expect_identical(GetDepthofDeficit_lognormal(NA, 1, 1), NA)
  expect_identical(GetDepthofDeficit_lognormal(1, NA, 1), NA)
  expect_identical(GetDepthofDeficit_lognormal(1, 1, NA), NA)
})

test_that("GetInverseCDF_lognormal gives the correct inverse", {
  expect_equal(GetInverseCDF_lognormal(0.1), 0.17771198)
})

##Test calculator output (integration test)
#This part tests if running the calculator_run.R gives the correct output. It combines all functions and scripts.
#Run file and create output


received.output <- read_rds(
  here(DATA.LOCATION, paste0("RESULTS-DLE-emulator-", IAM.OUTPUT.SHORT.FILENAME, ".RData"))
)

# test_that("Test that rows and columns are the same", {
#   expect_equal(ncol(received.output), ncol(expected.output))
#   expect_equal(nrow(received.output), nrow(expected.output))
# })
#
#
# test_that("Check that there are as many NAs in the output data", {
#   expect_equal(nrow(drop_na(expected.output)), nrow(drop_na(received.output)))
# })

test_that("Check if share.below.projected.curtech is non-negative", {
  share.below.projected.curtech <- received.output$share.below.projected.curtech
  expect_true(all(share.below.projected.curtech >= 0))
})

test_that("Check if share.below.projected.adjusted is non-negative", {
  share.below.projected.adjusted <- received.output$share.below.projected.adjusted
  expect_true(all(share.below.projected.adjusted >= 0))
})

TEST <<- FALSE
set_data_location(test=TEST)
remove_test_data_output()
