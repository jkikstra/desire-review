#' This test tests:
#' calculator_inequality-process-income-driver.R
#' Testing:
#' - Input data available: projection of income Gini for all countries [GINI.PROJECTION.FILE]
#' - Check that Gini values are not unreasonable, for the full projection, and tighter bounds for the year 2020
#' - Output files exist
#'
#' Tests existence of input and output files as well as gini coefficient bounds
#'
#' Currently passes locally.
#'
library(testthat)
source(here::here('R','utils.R'))
source(here::here('R','calculator_input-options.R'))
TEST <<- TRUE
set_data_location(test=TEST)
source(here::here("R", "calculator_inequality-process-income-driver.R"))

# Input test
test_that("Input files exist", {
  if (GINI.PROJECTION.FILE=="gini_ssp.csv"){
    expect_true(file.exists(here("data-raw", GINI.PROJECTION.FILE)), "GINI.PROJECTION.FILE does not exist.")
  } else if (GINI.PROJECTION.FILE=="SHAPE-and-SSP"){
    expect_true(file.exists(here("data-raw", "gini_ssp.csv")), "Gini file for SSP does not exist.")
    expect_true(file.exists(here("data-raw", "SDP Gini_GDP pathways v1.01.csv")), "Gini file for SHAPE does not exist.")
    expect_true(file.exists(here("data-raw", "SHAPE-final_gini_v3.csv")), "Gini file for SHAPE Vietnam does not exist.")
  }

})

# Gini bounds test (over all points in time)
test_that("Gini coefficients are within reasonable bounds", {

  income.gini.data <- readRDS(here(DATA.LOCATION, "scenario_income_gini.RData"))
  gini_withinbounds <- all(income.gini.data$gini >= 0.1 & income.gini.data$gini <= 0.8)
  expect_true(gini_withinbounds)

})

# Gini bounds test (for 2020)
test_that("Gini coefficients are within reasonable bounds in 2020", {

  income.gini.data <- readRDS(here(DATA.LOCATION, "scenario_income_gini.RData")) %>% filter(year==2020) %>%
    filter(iso!="AZE") # in the Gini data, we know that Azerbaijan has a Gini of <0.2 (0.17) in 2020, which is very low
  gini_withinbounds <- all(income.gini.data$gini >= 0.2 & income.gini.data$gini <= 0.75)
  expect_true(gini_withinbounds)

})

# Output test
test_that("Output files exist", {
  expect_true(file.exists(here(DATA.LOCATION, "scenario_income_gini.RData")))
})

TEST <<- FALSE
set_data_location(test=TEST)
remove_test_data_output()
