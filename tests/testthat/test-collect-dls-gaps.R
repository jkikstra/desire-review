#' This tests the script that utilities for the calculator part of DELIRIUM
#'
#' Tests below:
#' - Are all files produced? Test all expected outcome files are there.
#' - Do the numbers make sense? Test per outcome file what kind of outputs are created
#' - Are the units as expected? Or did they change?
#'
#'
#' Currently passes locally.



library(testthat)
source(here::here('R','utils.R'))
TEST <<- TRUE

# record what's already in the test output folder
output.files.before.running.dlsgaps <- list.files(here(get_data_location(test = TEST)))

# testing body: run script to be tested
source(here::here('R','collect-dls-gaps.R'))

# test set 1: Are all files produced?
output.files.from.running.dlsgaps <- list.files(here(DATA.LOCATION))
output.files.from.running.dlsgaps <- output.files.from.running.dlsgaps[!(output.files.from.running.dlsgaps %in% output.files.before.running.dlsgaps)]


expected.output.files <- c(
  "DLS_dataproducts_combined.csv",
  "DLS_depth_of_deficit.csv",
  "DLS_headcount_below_threshold.csv",
  "DLS_headcount_below_threshold_pivoted_no_units.csv",
  "DLS_household_size.csv",
  "DLS_modal_share.csv",
  "DLS_service_gaps.csv",
  "DLS_service_gaps_pivoted_no_units.csv",
  "DLS_threshold.csv",
  "DLS_threshold_pivoted_no_units.csv"
)


test_that("Are all output files in the testdata/output folder?",{
  expect_contains(output.files.from.running.dlsgaps, expected.output.files)
})

test_that("Are ONLY the output files in the testdata/output folder?",{
  expect_true(setequal(output.files.from.running.dlsgaps, expected.output.files))
})



# test 2: Do the numbers make sense? Are the units correct?
dls.expected.columns <- c(
  "Depth-of-deficit",
  "Decent Living Standard threshold",
  "Deprivation headcount",
  "Service gap"
)
dls.headcount.expected.unit <- "Share of total population living below their decent living standard"

dls.expected.variables.and.units <- tibble(
  variable = c(
    "Appliance|clean_cooking_fuel",
    "Appliance|mobile_telephone",
    "Appliance|refrigerator",
    "Appliance|television",

    "Clothing|clothing",
    "Clothing|footwear",

    "Cooling CON|rural",
    "Cooling CON|total",
    "Cooling CON|urban",

    "Education|lower_secondary",
    "Education|primary",

    "Health care",

    "Heating CON|rural",
    "Heating CON|total",
    "Heating CON|urban",

    "Hot Water OP|rural",
    "Hot Water OP|total",
    "Hot Water OP|urban",

    "Housing|rural",
    "Housing|total",
    "Housing|urban",

    "Nutrition",

    "Roads",

    "Sanitation",

    "Transport",

    "Water"

  ),
  unit = c(

    # appliances
    "unit/cap",
    "unit/cap",
    "unit/cap",
    "unit/cap",

    # clothing
    "kg/cap/year",
    "kg/cap/year",

    # cooling
    "Share of population requiring cooling.", # N.B. to be changed to a better unit (see e.g. PR #183 and #184)
    "Share of population requiring cooling.", # N.B. to be changed to a better unit (see e.g. PR #183 and #184)
    "Share of population requiring cooling.", # N.B. to be changed to a better unit (see e.g. PR #183 and #184)

    # education
    "$/cap/year",
    "$/cap/year",

    # health care
    "$/cap/year",

    # heating
    "Share of population requiring heating.", # N.B. to be changed to a better unit (see e.g. PR #183 and #184)
    "Share of population requiring heating.", # N.B. to be changed to a better unit (see e.g. PR #183 and #184)
    "Share of population requiring heating.", # N.B. to be changed to a better unit (see e.g. PR #183 and #184)

    # hot water
    "Share of population requiring hot water services.",
    "Share of population requiring hot water services.",
    "Share of population requiring hot water services.",

    # housing
    "m2/cap",
    "m2/cap",
    "m2/cap",

    # nutrition
    "kcal/cap/day", # should we translate this to per year?

    # roads
    "km/cap", # seems to be missing from threshold?

    # sanitation
    "Share of population requiring safely managed sanitation services.",

    # transport
    "pkm/cap/year",

    # water
    "m3/cap/year"

  )
) %>% arrange(variable,unit)


dls.combined <- read.csv(
  here(DATA.LOCATION, "DLS_dataproducts_combined.csv")
) %>% tibble()

test_that("The combined DLS file holds all expected products",{
  expect_true(setequal(dls.combined %>% pull(type) %>% unique(),
                       dls.expected.columns))
})



dls.dod <- dls.combined %>% filter(type == dls.expected.columns[1])
dls.thres <- dls.combined %>% filter(type == dls.expected.columns[2])
dls.headcount <- dls.combined %>% filter(type == dls.expected.columns[3])
dls.servicegap <- dls.combined %>% filter(type == dls.expected.columns[4])

test_that("The units and variables of depth-of-deficit are also in the service gap units",{
  expect_contains((dls.servicegap %>% pull(unit) %>% unique()),
                  (dls.dod %>% pull(unit) %>% unique()))
  expect_contains((dls.servicegap %>% pull(variable) %>% unique()),
                  (dls.dod %>% pull(variable) %>% unique()))

})
test_that("The units and variables of DLS thresholds are also in the service gap units",{
  expect_equal((dls.thres %>% pull(unit) %>% unique()),
                  (dls.servicegap %>% pull(unit) %>% unique()))
  expect_equal((dls.thres %>% pull(variable) %>% unique()),
               (dls.servicegap %>% pull(variable) %>% unique()))
})

test_that("All values are 0 or higher, and the headcount values are also 1 or lower",{
  expect_equal((dls.combined %>% filter(value<0) %>% nrow()), 0)
  expect_equal((dls.combined %>% filter(value>1, type == dls.expected.columns[3]) %>% nrow()), 0)
})


test_that("All headcount units are the same, and as expected",{
  expect_equal((dls.headcount %>% pull(unit) %>% unique()),
               dls.headcount.expected.unit)
})

test_that("Decent Living Standards thresholds and service gaps units are there for all dimensions, and are as expected",{
  expect_equal((dls.thres %>% distinct(variable,unit) %>% arrange(variable,unit)),
               dls.expected.variables.and.units)
  expect_equal((dls.servicegap %>% distinct(variable,unit) %>% arrange(variable,unit)),
               dls.expected.variables.and.units)
})




# End the test file by removing all test-data
TEST <<- FALSE
set_data_location(test=TEST)
remove_test_data_output()

