#' This test tests:
#' calculator_activity-fe-process-downscaled.R
#' Testing:
#' - Input data available: final energy distribution file [ENERGY.DISTRIBUTION.FILE]
#' - Output files exist, and do not contain NAs or otherwise unreasonable numbers, i.e. no gini outside [0,1]
#' - Check that there is no NA values in the processed final energy distribution data
#' - [CURRENTLY FAILS] Check if any energy value is 0 for non-zero population --- [FAILS for Oswald 2021 data, because of transport in Benin (except high, which is zero population in Benin) & transport in lowest in Belarus]
#' - Check if any energy value is non-zero for 0 population
#'
#' Tests existence of input and output files as well as no N/As in prepared outputs
#'
#' Currently passes locally.
#'
library(testthat)
source(here::here('R','utils.R'))
source(here::here('R','calculator_input-options.R'))
TEST <<- TRUE
set_data_location(test=TEST)
source(here::here("R", "calculator_inequality-fe-process-starting.R"))

handler <- function(condition) {
  invokeRestart("muffleWarning")
}
options(error = handler)

#Input test
test_that("Input files exist", {
  expect_true(file.exists(here("data-raw", ENERGY.DISTRIBUTION.FILE)))
})

#Output test
test_that("Output files exist and contain no NA values", {
  # Check that each file exists
  expect_true(file.exists(here(DATA.LOCATION, "FE_quantile_iso.RData")), "FE_quantile_iso.RData file does not exist.")
  expect_true(file.exists(here(DATA.LOCATION, "FE_quantile_iso_sector.RData")), "FE_quantile_iso_sector.RData file does not exist.")
  expect_true(file.exists(here(DATA.LOCATION, "FE_gini_iso.RData")), "FE_gini_iso.RData file does not exist.")
  expect_true(file.exists(here(DATA.LOCATION, "FE_average_iso.RData")), "FE_average_iso.RData file does not exist.")

  # Load each file
  quantile_iso_data <- readRDS(here(DATA.LOCATION, "FE_quantile_iso.RData"))
  quantile_iso_sector_data <- readRDS(here(DATA.LOCATION, "FE_quantile_iso_sector.RData"))
  gini_iso_data <- readRDS(here(DATA.LOCATION, "FE_gini_iso.RData"))
  average_iso_data <- readRDS(here(DATA.LOCATION, "FE_average_iso.RData"))

  # Check that there are no NA values in each file
  expect_true(!any(is.na(quantile_iso_data)),
              info = "There are NA values in the FE_quantile_iso.RData file.")
  expect_true(quantile_iso_sector_data %>% filter(is.na(fe.transport)) %>% nrow() == 4,
              info = "There are unexpected NA values for transport in the FE_quantile_iso_sector.RData file.")
  expect_true(!any(is.na(quantile_iso_sector_data %>% select(-fe.transport))),
              info = "There are NA values in the FE_quantile_iso_sector.RData file.")
  expect_true(!any(is.na(gini_iso_data)),
              info = "There are NA values in the FE_gini_iso.RData file.")
  expect_true(!any(is.na(average_iso_data %>% filter(iso!="BEN"))),
              info = "There are NA values in the FE_average_iso.RData file.")
})


# Check that there is no NA values in the processed final energy distribution data
test_that("Check if there are any NA values in columns", {
  failed_rows <- fe.by.income %>% filter(if_any(c("fe", "fe.rescom", "fe.transport", "fe.industry", "population", "quantile"),
                                                ~ is.na(.)))
  expect_true(nrow(failed_rows) == 4)
})

# Check if any energy value is 0 for non zero population
# The test will fail if using Oswald 2021 data. Needs to be checked and handled appropriately to resolve this failure.
test_that("Check if any energy value is 0 for non-zero population",{

  failed_rows <- fe.by.income %>% filter(if_any(c("fe", "fe.rescom", "fe.transport", "fe.industry"),
                                                ~ . == 0) & population != 0)
  expect_equal(nrow(failed_rows), nrow(data.frame()))
  # show_failure(expect_equal(nrow(failed_rows), nrow(data.frame())))
})

# Check if any energy value is non zero for zero population
#print(fe.by.income[fe.by.income$population==0, ])
test_that("Check if any energy value is non-zero for zero population",{
  failed_rows <- fe.by.income %>% filter(if_any(c("fe", "fe.rescom", "fe.transport", "fe.industry"),
                                                ~ . != 0) & population == 0)
  expect_equal(nrow(failed_rows), nrow(data.frame()))
})

TEST <<- FALSE
set_data_location(test=TEST)
options(error = NULL)
remove_test_data_output()
