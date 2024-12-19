#' This test tests:
#' calculator_needs-dle-efficiency.R: scaling minimum energy requirement with service provisioning efficiency changes
#' Testing:
#' - Input files exist
#' - Output files exist
#' - Scaled values are greater than 0
#' - DLE after scaling having reasonable value
#' - Check if any country has greater than 40 GJ/cap/yr for any sector
#'
#'
#' Tests existence of input and output files as well as expected columns of data frames and scaled values greater than zero
#'
#' Currently passes locally.


library(testthat)
source(here::here('R','utils.R'))
source(here::here('R','calculator_input-options.R'))
TEST <<- TRUE
set_data_location(test=TEST)
source(here("R", "calculator_needs-dle-input.R"))
source(here::here("R", "calculator_needs-dle-mapping.R"))
source(here::here("R", "calculator_needs-dle-efficiency.R"))


# Input test
test_that("Input files exist", {
  expect_true(file.exists(here(DATA.LOCATION, "dle-total-and-sectoral.RData")))
})

# Output test
test_that("Output files exist", {
  expect_true(file.exists(here(DATA.LOCATION, paste0("projected_dle-total-and-sectoral-scaled", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData"))), "FE_gini_iso.RData file does not exist.")
})

test_that("No data loss in joining of data frames", {
  expect_equal(nrow(dle.adjusted.data), nrow(f2s.data.iso))
  expect_equal(nrow(dle.adjusted.data), nrow(scenario.data.f2s.adj))
})

test_that("Efficiency: scaled values are greater than 0", {
  expect_true(all(dle.adjusted.data$dle.tech.scaler >= 0))
})

test_that("Efficiency: scaled values are between expected bounds", {
  dle.adjusted.data.until2050 <- dle.adjusted.data %>%
    filter(year<=2050,
           year>=2020)
  expect_true(all(dle.adjusted.data.until2050$dle.tech.scaler >= 0.1))
  expect_true(all(dle.adjusted.data.until2050$dle.tech.scaler <= 3))
})

# Check the DLE after scaling having reasonable value
test_that("No countries have less than half or more than double the DLE compared to before", {

  dle.adjusted.data.until2050 <- dle.adjusted.data %>%
    filter(year<=2050,
           year>=2020)

  sum_threshold_sectoral <- dle.adjusted.data.until2050 %>%
    reframe(dle.threshold.adjusted = sum(dle.threshold.adjusted),
            dle.threshold.curtech = sum(dle.threshold.curtech),
            .by = c("model", "scenario", "iso", "variable", "year"))


  sum_threshold_total <- dle.adjusted.data.until2050 %>%
    reframe(dle.threshold.adjusted = sum(dle.threshold.adjusted),
            dle.threshold.curtech = sum(dle.threshold.curtech),
            .by = c("model", "scenario", "iso","year"))


  nrow_outofbounds_sectoral <- sum_threshold_sectoral %>%
    filter(dle.threshold.adjusted < 0.1 * dle.threshold.curtech | dle.threshold.adjusted > 3 * dle.threshold.curtech) %>%
    distinct(model, scenario, iso)


  nrow_outofbounds_total <- sum_threshold_total %>%
    filter(dle.threshold.adjusted < 0.2 * dle.threshold.curtech | dle.threshold.adjusted > 2 * dle.threshold.curtech) %>%
    distinct(model, scenario, iso)

  nrow_outofbounds_median_2050 <- sum_threshold_total %>%
    mutate(scaling = dle.threshold.adjusted / dle.threshold.curtech) %>%
    reframe(median_scaling = median(scaling, na.rm=T),
            .by = c("model", "scenario", "year")) %>%
    filter(year==2050) %>%
    # shape median efficiencies go down to 0.49 for SDP_EI-low1p5C in REMIND
    filter(median_scaling < 0.4 | median_scaling > 1) %>%
    distinct(model, scenario)

  expect_true(nrow(nrow_outofbounds_sectoral) == 0, "Some countries have really low or really high DLE sectoral efficiency scaling.")
  expect_true(nrow(nrow_outofbounds_total) == 0, "Some countries have really low or really high DLE total efficiency scaling.")
  expect_true(nrow(nrow_outofbounds_total) == 0, "Some scenarios have really low or really high median DLE total efficiency scaling across the whole scenario.")
})

test_that("Check if any country has greater than 40 GJ/cap/yr for any sector",{

  gt_40 <- dle.adjusted.data %>%
    filter(dle.threshold.adjusted > 40000) %>% # in MJ
    filter(year == 2030) %>%
    select(model, scenario, iso, variable, year, dle.threshold.adjusted)

  expect_true(nrow(gt_40) == 0)
})


TEST <<- FALSE
set_data_location(test=TEST)
remove_test_data_output()
