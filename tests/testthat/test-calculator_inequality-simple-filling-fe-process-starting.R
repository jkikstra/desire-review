#' These tests should test that the filling of Final Energy Gini values missing in the raw input data works as expected:
#' - Check input files exist (i.e., raw final energy gini data has been processed)
#' - No NAs in the existing final energy ginis
#' - No NAs in the new predicted final energy ginis
#' - New predicted final energy ginis withn bounds [0,1]
#' - Make sure no new different final energy gini has replaced existing gini values
#' -

#' Ideas for these tests in the future:
#' - all countries expected are filled in (how to test this?)
#' - the filled in numbers are not out of bounds? (can this be tested at all, with regression?); maybe just a check for not too many values outside of the min/max of the original data?
#'
#' Currently passes locally.


# Tests existence of input file as well as gini coefficient within bounds and conservation of total energy gini values (Output is not tested since it has the same file name as input)
library(testthat)
source(here::here('R','utils.R'))
source(here::here('R','calculator_input-options.R'))
TEST <<- TRUE
set_data_location(test=TEST)
source(here::here("R", "calculator_inequality-fe-process-starting.R"))
source(here::here("R", "calculator_inequality-simple-filling-fe-process-starting.R"))


# Input test
test_that("Check input files exist",{
  expect_true(file.exists(here(DATA.LOCATION, "FE_gini_iso.RData")))
})

# Check no N/A for existing energy gini values
# Since BEN was deleted, the test succeeds now.
na.entries.data.based.ginis <- which(is.na(data.based.ginis$energy.gini))

if(length(na.entries.data.based.ginis) > 0) {
  print("The following entries in data.based.ginis have NA values for energy.gini:")
  print(data.based.ginis[na.entries.data.based.ginis, c("iso", "variable", "energy.gini")])
}

# Unit test- No N/As in predicted energy gini values
test_that("No NA in final energy ginis", {
  expect_false(anyNA(new.ginis$energy.gini), label = "Missing values")
  expect_true(nrow(new.ginis) %% 4 == 0, label = "Number of rows is not a multiple of 4 (total, transport, buildings, industry)")
})

# Unit test for Gini coefficient values within bounds
test_that("Gini coefficients are within reasonable bounds", {
  gini_valid <- all(new.ginis$energy.gini >= 0 & new.ginis$energy.gini <= 1)
  expect_true(gini_valid, label = "Gini out of range")
})

# Unit test for no change in existing energy gini value
test_that("same iso and variable have the same energy.gini value before and after filling", {
  merged.ginis <- inner_join(data.based.ginis, new.ginis, by = c("iso", "variable"),
                             suffix = c(".data.based.ginis", ".new.ginis"))

  na.entries <- which(is.na(merged.ginis$energy.gini.data.based.ginis) | is.na(merged.ginis$energy.gini.new.ginis))

  if(length(na.entries) > 0) {
    print("The following entries have NA values:")
    print(merged.ginis[na.entries, c("iso", "variable")])
  }
  expect_true(length(na.entries) == 0,
              info = "There are NA values in the merged.ginis dataframe")

  for (i in seq_len(nrow(merged.ginis))) {
    expect_equal(
      merged.ginis$energy.gini.data.based.ginis[i],
      merged.ginis$energy.gini.new.ginis[i],
      tolerance = 1e-8,
      label = paste("Mismatch for iso =", merged.ginis$iso[i], "variable =", merged.ginis$variable[i])
    )
  }
})



TEST <<- FALSE
set_data_location(test=TEST)
remove_test_data_output()
