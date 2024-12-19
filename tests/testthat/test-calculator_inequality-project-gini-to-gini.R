#' This test tests:
#' calculator_inequality-project-gini-to-gini.R: projecting energy gini based on projections of income gini
#' Testing:
#' - Input data available: projection of income Gini, processed before [scenario_income_gini.RData], starting year final energy Gini, processed before [FE_gini_iso.RData], scenario data for a template [paste0("scenario_FE", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData")]
#' - Output files exist [gini_to_gini_data.RData]
#' - Output file does not contain gini values that are out of what is deemed reasonable.
#'
#' Possible additional tests:
#' - sector-specific energy gini tests
#' - maybe `expect_true(file.exists(here(DATA.LOCATION, paste0("scenario_FE", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData"))))` is only to create a template; maybe you can add a test on the expected rows and columns of the template/outcome.
#'
#' Tests existence of input and output files as well as gini coefficient bounds
#'
#' Currently passes locally.

library(testthat)
source(here::here('R','utils.R'))
source(here::here('R','calculator_input-options.R'))
TEST <<- TRUE
set_data_location(test=TEST)
source(here::here("R", "calculator_population.R"))
source(here::here("R", "calculator_activity-fe-process-downscaled.R"))
source(here::here("R", "calculator_inequality-process-income-driver.R"))
source(here::here("R", "calculator_inequality-fe-process-starting.R"))
source(here::here("R", "calculator_inequality-project-gini-to-gini.R"))


# Input test
test_that("Check all input files exist",{
  expect_true(file.exists(here(DATA.LOCATION, paste0("scenario_FE", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData"))))
  expect_true(file.exists(here(DATA.LOCATION, "FE_gini_iso.RData")))
  expect_true(file.exists(here(DATA.LOCATION, "scenario_income_gini.RData")))
})

# Output test
test_that("Inequality projection output header", {
  # Load output data
  inequality.projection.iv <- read_rds(
    here(DATA.LOCATION, paste0("projected_energy_inequality", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData"))
  )
  # Define the expected output header
  expected_header <- c("model", "scenario", "iso", "variable", "year", "energy.gini", "gini.to.gini.elasticities.by.country.down", "gini.to.gini.elasticities.by.country.up", "gini", "scenario.mapping")

  # Compare the output header
  expect_equal(colnames(inequality.projection.iv), expected_header, "Output headers do not match")
})

rm(IAM.OUTPUT.SHORT.FILENAME) # to not interfere with the normal runcode

# Energy gini within bounds
test_that("Gini coefficients are within reasonable bounds", {
  gini_withinbounds <- all(inequality.projection.iv$gini >= 0.1 & inequality.projection.iv$gini <= 0.8)
  # Assert that gini_normalized is TRUE
  expect_true(gini_withinbounds)
})

TEST <<- FALSE
set_data_location(test=TEST)
remove_test_data_output()
