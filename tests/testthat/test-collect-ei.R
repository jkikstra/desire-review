#' This tests the script that is supposed to collect the energy intensities of the DLE starting year
#'
#'
#' Extra tests that could be added:
#' - [%%%] units correct
#' - [%%%] values within certain bounds
#'
#' Currently passes locally.

library(testthat)
source(here::here('R','utils.R'))
TEST <<- TRUE

# record what's already in the test output folder
output.files.before.running.collectei <- list.files(here(get_data_location(test = TEST)))

expected.output.files <- c(
  "EI_combined_long_with_units.csv",
  "EI_con_new.csv",
  "EI_con_new_pivoted_no_units.csv",
  "EI_con_rep.csv",
  "EI_con_rep_pivoted_no_units.csv",
  "EI_op.csv",
  "EI_op_pivoted_no_units.csv"
)

# just test whether the run works
test_that("collect-ei.R runs without errors",{
  expect_no_error(source(here::here("R", "collect-ei.R")))
})

# test set 1: Are all files produced?
output.files.from.running.collectei <- list.files(here(DATA.LOCATION))
output.files.from.running.collectei <- output.files.from.running.collectei[!(output.files.from.running.collectei %in% output.files.before.running.collectei)]


test_that("Are all output files in the testdata/output folder?",{
  expect_contains(output.files.from.running.collectei, expected.output.files)
})

test_that("Are ONLY the output files in the testdata/output folder?",{
  expect_true(setequal(output.files.from.running.collectei, expected.output.files))
})

# test set 2: no unexpected values
all.ei.data <- read_csv(here(DATA.LOCATION, "EI_combined_long_with_units.csv"))
test_that("No negative values",{
  expect_equal((all.ei.data %>% filter(e.int<0) %>% nrow()), 0)
})
test_that("If the unit is undefined (e.g. operation energy for Housing) then e.int=0",{
  expect_equal((all.ei.data %>% filter(is.na(unit), e.int!=0) %>% nrow()), 0)
})
test_that("If the unit is defined (e.g. construction energy for Housing) then e.int>0 for elec=='total', and the variable not being Heating or Cooling",{
  expect_equal((all.ei.data %>% filter(!is.na(unit), elec=="total", e.int<=0, !grepl(x = variable, pattern="Cooling"), !grepl(x = variable, pattern="Heating")) %>% nrow()), 0)
})

# test set 3: same number of dimensions
expected.variable.number <- 36
expected.energytype.number <- 3

test_that("If the unit is defined (e.g. construction energy for Housing) then e.int>0 for elec=='total', and the variable not being Heating or Cooling",{
  expect_equal((all.ei.data %>% distinct(`Purpose of energy flow or investment`) %>% nrow()), expected.energytype.number)
})
test_that("Test number of variables reported",{
  expect_equal((all.ei.data %>% filter(grepl(`Purpose of energy flow or investment`,pattern="(OP)")) %>% distinct(variable) %>% nrow()), expected.variable.number)
  expect_equal((all.ei.data %>% filter(grepl(`Purpose of energy flow or investment`,pattern="(CON.rep)")) %>% distinct(variable) %>% nrow()), expected.variable.number)
  expect_equal((all.ei.data %>% filter(grepl(`Purpose of energy flow or investment`,pattern="(CON.new)")) %>% distinct(variable) %>% nrow()), expected.variable.number)
})

# test set 4: some logical bounds (e.g. CON.rep smaller than CON.new)
con.rep <- read_csv(here(DATA.LOCATION, "EI_con_rep.csv"))
con.new <- read_csv(here(DATA.LOCATION, "EI_con_new.csv"))

con.both <- con.rep %>% rename(con.rep=e.int) %>%
  left_join(con.new %>% rename(con.new=e.int))

test_that("CON.rep and CON.new merge fine: same number of rows",{
  expect_equal((con.both %>% nrow()),
               (con.rep %>% nrow()))
  expect_equal((con.both %>% nrow()),
               (con.new %>% nrow()))
})

test_that("CON.rep always smaller than CON.new",{
  expect_equal((con.both %>% filter(con.rep>=con.new, !is.na(unit)) %>% nrow()),
               0)
})

# test set 5: units
# ...tbd...

# test set 6: energy intensity expected bounds (building on units)
# ...tbd...



# End the test file by removing all test-data
TEST <<- FALSE
set_data_location(test=TEST)
remove_test_data_output()
