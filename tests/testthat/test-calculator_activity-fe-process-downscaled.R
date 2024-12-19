#' This test tests:
#' calculator_activity-fe-process-downscaled.R
#' Testing:
#' - Input data available: IAM downscaled final energy by sectors [IAM.SCENARIO.DATA.FILE]
#' - Input data in the right format: IAM downscaled final energy by sectors [IAM.SCENARIO.DATA.FILE], in the format of "iiasa/downscaler_repo" output
#' - "Final Energy|Residential" + "Final Energy|Commercial" == "Final Energy|Residential and Commercial"
#' - Functioning of commercial/residential correction.
#' - Functioning of biomass correction.
#' - Did the processing of the final energy downscaled data produce an output?
#' - Check that population is not impossibly large, or NA
#'
#' Tests existence of input and output as well as total final energy conservation before and after split
#'
#' Currently passes locally.
#'
#' Note some improvements that can be made in the future:
#' - #242: handling the sum of final energy when more than the standard 3 Final Energy categories are reported

library(testthat)
source(here::here('R','utils.R'))
source(here::here('R','calculator_input-options.R'))
TEST <<- TRUE
set_data_location(test=TEST)
source(here::here('R','calculator_population.R'))
source(here::here("R", "calculator_activity-fe-process-downscaled.R"))

# Input test
test_that("Input files exist", {
  expect_true( file.exists(paste0(here("data-raw", "scenario_data", IAM.SCENARIO.DATA.FILE), ".csv")), "scenario data file does not exist.")
})

test_that("Input file has correct format",{
  df <- read_csv(paste0(here("data-raw", "scenario_data", IAM.SCENARIO.DATA.FILE), ".csv"))
  required_columns <- c("MODEL", "SCENARIO", "ISO", "VARIABLE", "UNIT")
  year_columns <- as.character(seq(2010, 2100, 5))
  required_columns <- c(required_columns, year_columns)
  expect_true(all(tolower(required_columns) %in% tolower(colnames(df))))
})

split.fe.df <- readRDS(
  here(DATA.LOCATION, paste0("scenario_FE", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData"))
) %>% drop_na(pop_mil)

test_that("Check energy per capita demand correction did not introduce NAs", {

  energy.corrected.NAs <- split.fe.df %>%
    filter(is.na(energy.per.capita))

  # Print the tibble if there are any NAs
  expect_true(nrow(energy.corrected.NAs) == 0, info = {
    cat("Rows with missing energy per capita:\n")
    print(energy.corrected.NAs %>% distinct(model, iso, variable))
  })
})

test_that("Final energy calculation after residential and commercial split is correct", {

  split.fe.df <- split.fe.df %>%
    filter(!(model=="MESSAGEix-GLOBIOM 1.0 R12 adhoc" & year%in%c(2020,2025)))

  expect_true(all(split.fe.df$res.ratio >= 0 & split.fe.df$res.ratio < 1))
})

test_that("Final energy calculation after traditional biomass split is correct", {
  expect_true(all(split.fe.df$res.tradbio.ratio >= 0 & split.fe.df$res.tradbio.ratio < 1))
})

test_that("Final energy calculation after aviation is correct", {
  expect_true(all(split.fe.df$aviation.share.energy >= 0 & split.fe.df$aviation.share.energy < 1))
})

test_that("Check that corrections for DLE make energy.per.capita lower, not higher",{
  split.fe.df <- split.fe.df %>%
    filter(!is.na(energy.per.capita), !is.na(energy.per.capita.uncorrected)) %>% # capture NAs in another test (above)
    filter(# While this generally does not happen, there is the possibility in downscaling final energy for R12_RCPA where the total can be lower than the sum of Ind+ResCom+Tran
      !(variable=="Final Energy" & model=="IMAGE 3.3"),
      !(variable=="Final Energy" & model=="MESSAGEix-GLOBIOM 1.0 R12 adhoc" & iso %in%c("MAC", "MNG") ))
  expect_true(all((split.fe.df$energy.per.capita - split.fe.df$energy.per.capita.uncorrected) < 1e-8), info = {
    cat("Rows where correct energy is higher than uncorrected energy:\n")
    print(split.fe.df %>% filter(energy.per.capita-energy.per.capita.uncorrected > 1e-8) %>% distinct(model, iso, variable))
  })
})



# Output test
test_that("Output files exist", {
  expect_true(file.exists(here(DATA.LOCATION, paste0("scenario_FE", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData"))), "scenario_FE_*.Rdata (i.e., final energy demand by country and sector [after residential-energy correction]) file does not exist.")
})



test_that("Check sum of all sectors less than the total",{

  sum_other_values <- split.fe.df %>%
    group_by(model, scenario, iso, year) %>%
    filter(variable != "Final Energy") %>%
    summarize(sum_other = sum(energy.per.capita.uncorrected))

  comparison_data <- split.fe.df %>%
    filter(variable == "Final Energy") %>%
    left_join(sum_other_values, by = c("model", "scenario", "iso", "year")) %>%
    filter(complete.cases(.))

  comparison_data <- comparison_data %>%
    filter(model!="IMAGE 3.3") %>%
    mutate(diff = energy.per.capita.uncorrected - sum_other)

  # keep only the rows where the sum is higher than the total, acknowledging that there may be zeros, and since we do not mind arbitrary minor numerical differences, nor about small differences caused by the downscaling algorithm that is producing the intput data, we delete a small value (e.g. 5% difference)
  failed_rows <- comparison_data[!(comparison_data$energy.per.capita.uncorrected > comparison_data$sum_other * (1 - 5e-2)), ]

  expect_equal(nrow(failed_rows), 0)

})


test_that("No duplicate rows",{
  duplicate_rows <- split.fe.df %>%
    group_by(model, scenario, iso, variable, year) %>%
    filter(n() != 1) %>%
    ungroup()
  expect_equal(nrow(duplicate_rows), 0)
})


test_that("Population size makes sense",{
  global.population.by.scenario <- split.fe.df %>% reframe(
    global.pop = sum(pop_mil),
    .by = c("model", "scenario", "variable", "year")
  )
  expect_true(all(global.population.by.scenario$global.pop > 6900), label = "Global population is less than 6.9 billion (rough 2010 population)")
  expect_true(all(global.population.by.scenario$global.pop < 13000), label = "Global population is more than 13 billion (SSP3 original max population in 2100 is 12.6)")
  expect_true(all(split.fe.df$pop_mil < 2700), label = "A country has more than 2700 billion (SSP3 original max population in India in 2100 is 2609)")
})


TEST <<- FALSE
set_data_location(test=TEST)
remove_test_data_output()
