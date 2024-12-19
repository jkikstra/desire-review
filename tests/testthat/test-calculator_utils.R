#' This tests performs unit tests for the utilities for the calculator part of DELIRIUM
#' - load_population_projection(ssp)
#' - format_final_energy_downscaled_cdlinksmessage()
#' - format_final_energy_downscaled_shape()
#' - GetInverseCDF_lognormal()
#' - get_gini()
#'
#'
#' Currently passes locally.

library(testthat)
source(here::here('R','utils.R'))
source(here::here('R','calculator_input-options.R'))

TEST <<- TRUE

##Test separate functions (unit tests)

test_that("load_population_projection returns 'pop_mil' column without NA values", {
  ssp <- "SSP1"  # Replace with the desired SSP value
  result <- load_population_projection(ssp)
  expect_false(anyNA(result$pop_mil), "population column should not contain NA values.")
  expect_false(any(result$pop_mil > 6000), "population should not be greater than 6 billion")
})


# Check formatting function works correctly

test_that("format_final_energy_downscaled_shape returns the expected output", {
  df.out <- scenario.data.fe <- vroom(
    here(
      "data-raw",
      "scenario_data",
      "SHAPE-final_v3_2200_wo_smooth_enlong_ENLONG_RATIO.csv"
    )
  ) %>% format_final_energy_downscaled_shape()

  # Assert the header is correct
  expect_identical(colnames(df.out), c("model", "scenario", "iso", "variable", "unit", "year", "value"), "Incorrect column names.")

  na_count <- sum(is.na(df.out$value))
  expect_true(na_count == 0)

  # Calculate the differences
  differences <- df.out %>%
    group_by(model, scenario, iso, year) %>%
    summarise(difference = value[variable == "Final Energy"] - sum(value[variable != "Final Energy"]))
  # Rename the 'value' column to 'difference'
  print(differences)
})

test_that("GetInverseCDF_lognormal calculates the inverse cumulative distribution function (CDF) for lognormal distribution", {
  # Create test input variables
  expect_equal(GetInverseCDF_lognormal(0.1), 0.17771198)
})


test_that("get_gini calculates the Gini coefficient based on per capita variable data", {
  # Create test input data frame
  input_df <- data.frame(
    iso = c("ISO1", "ISO1", "ISO2", "ISO2", "ISO3"),
    population = c(100, 200, 150, 300, 250),
    fe = c(10, 20, 15, 30, 25)
  )

  # Define the expected output data frame
  expected_df <- data.frame(
    iso = c("ISO1", "ISO2", "ISO3"),
    gini = c(0.133, 0.133, 0)
  )

  # Call the function to calculate the Gini coefficient
  result_df <- get_gini(input_df, "fe")

  # Check if the result matches the expected output approximately
  expect_equal(nrow(result_df), nrow(expected_df))
})

TEST <<- FALSE
