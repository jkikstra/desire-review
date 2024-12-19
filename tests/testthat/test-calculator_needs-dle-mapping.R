#' This test tests:
#' calculator_needs-dle-mapping.R: how to map detailed bottom-up DLE estimates to IAM categories
#' Testing:
#' - Input data available: 4 files; two MRIO mapping files, 1 sectoral mapping file, 1 simple mapping assumption, and 1 detailed DLE file
#' - Output files exist: DLE sectoral timeseries ["dle-total-and-sectoral.RData"]
#' - Mulitple tests on whether mapping files make sense (not losing any energy - rather summing input and output to be the same)
#'
#' Tests existence of input and output files as well as conservation of DLEs, sum of coefficients =1, and threshold under reasonable bounds
#'
#' Currently passes locally.
#'

library(testthat)
source(here::here('R','utils.R'))
source(here::here('R','calculator_input-options.R'))
TEST <<- TRUE
set_data_location(test=TEST)
source(here::here("R", "calculator_needs-dle-input.R"))
source(here::here("R", "calculator_needs-dle-mapping.R"))

test_that("Input files exist", {
  expect_true(file.exists(here(DATA.LOCATION, "dle-threshold-detail.RData")))
  expect_true(file.exists(here("data-raw", "DLE_EXIOmapping_non_elec_share_kikstra-Jan2024.csv")), "for the DLE dimensions derived using input-output tables.")
  expect_true(file.exists(here("data-raw", "DLE_EXIOmapping_elec_share_kikstra-Jan2024.csv")), "for the DLE dimensions derived using input-output tables.")
  expect_true(file.exists(here("data-raw", DLE.SECTORAL.MAPPING)), "somewhat unnecessary input file with simple name-mapping")
  expect_true(file.exists(here("data-raw", DLE.MESSAGE.MAPPING.ASSUMPTIONS)),"for DLE dimensions not derived using input-output tables.")
})

test_that("Output files exist ", {
  expect_true(file.exists(here(DATA.LOCATION, "dle-total-and-sectoral.RData")), "DLE timeseries do not exist.")
})

# Tests for no loss of DLE
test_that("No NAs in dle.scen.mrio and dle.scen.lca", {
  expect_false(anyNA(dle.scen.mriomethods), label = "dle.scen.mrio contains NAs")
  expect_false(anyNA(dle.scen.lcamethods$dle.scen.lca), label = "dle.scen.lca contains NAs")
})

test_that("Logical number of rows for dle.scen.sector.split", {
  # is_distinct <- dle.scen.sector.split %>% distinct(model, scenario, iso, year, variable)
  is_distinct <- dle.scen.sector.split %>% distinct(iso, variable)
  expect_equal(nrow(dle.scen.sector.split), nrow(is_distinct), info="Number of rows should equal unique combinations of model, scenario, iso, variable, years")
})

test_that("Sum of mapping.coefficient is 1 for grouped rows", {
  grouped_data.op <- dle.mapping.lca.manual %>%
    filter(!is.na(op)) %>%
    summarise(sum_coefficients = sum(op),
              .by = c("iso", "variable", "elec"))
  grouped_data.con <- dle.mapping.lca.manual %>%
    filter(!is.na(con)) %>%
    summarise(sum_coefficients = sum(con),
              .by = c("iso", "variable", "elec"))

  expect_true(all(grouped_data.op$sum_coefficients == 1),
              info = "Sum of mapping.coefficient (op) should be 1 for all grouped rows")
  expect_true(all(grouped_data.con$sum_coefficients == 1),
              info = "Sum of mapping.coefficient (con) should be 1 for all grouped rows")
})

test_that("Sum of sector.mapping (MRIO) for same region adds up to 1", {
  grouped_data <- mrio.sector.mapping.domestic.r12 %>%
    reframe(sum_mapping = sum(sector.mapping),
              .by = c("dle_dim", "elec", "region.message.r12"))
  for (i in 1:nrow(grouped_data)) {
    expect_equal(grouped_data$sum_mapping[i], 1, info = paste("Row", i, "- sum_mapping should be equal to 1"))
  }
})

test_that("Adding two methods yields the same total DLE as the original", {
  grouped_data1 <- dle.scen.mriomethods %>%
    group_by(iso) %>%
    summarise(sum_mapping = sum(dle.scen.mrio, na.rm=TRUE))

  grouped_data2 <- dle.scen.lcamethods %>%
    group_by(iso) %>%
    summarise(sum_mapping = sum(dle.scen.lca, na.rm=TRUE))

  grouped_data3 <- dle.neat %>%
    group_by(iso) %>%
    summarise(sum_mapping = sum(thres.energy, na.rm=TRUE))

  # Combine datasets by iso
  combined_data <- grouped_data1 %>%
    inner_join(grouped_data2, by="iso", suffix=c("_mrio", "_lca")) %>%
    inner_join(grouped_data3, by="iso")

  # Loop through combined data
  for (i in 1:nrow(combined_data)) {
    expect_equal(combined_data$sum_mapping_mrio[i] + combined_data$sum_mapping_lca[i], combined_data$sum_mapping[i])
  }
})

# Flag energy cap
test_that("Check if any country has greater than 50 GJ/cap/yr for any sector",{
  gt_50 <- dle.scen.sector.split %>%
    filter(dle > 50000) %>%
    select(iso, variable)
  expect_equal(length(unique(gt_50$iso)), 0, label = "No country should have greater than 50 GJ/cap/yr for any sector.")
})


#Test expected to fail until Issue #81 gets resolved
test_that("Total DLE calculation is correct", {

  total_dle <- aggregate(dle ~ iso, dle.scen.sector.split, sum)
  total_dle_scen_lca <- aggregate(dle.scen.lca ~ iso, dle.scen.lcamethods, sum)
  total_dle_scen_mrio <- aggregate(dle.scen.mrio ~ iso, dle.scen.mriomethods, sum)

  # Combine total_dle_scen_lca and total_dle_scen_mrio by ISO
  combined <- full_join(total_dle_scen_lca, total_dle_scen_mrio, by = "iso") %>%
    rename(dle_scen_lca = `dle.scen.lca`, dle_scen_mrio = `dle.scen.mrio`)

  total_unique_iso <- length(unique(total_dle$iso))
  print(total_unique_iso)

  # Check if the values in combined sum up correctly
  combined$total_dle <- combined$dle_scen_lca + combined$dle_scen_mrio
  mismatches <- combined[combined$total_dle != total_dle$dle.scen, "iso"]
  mismatch_count <- length(mismatches)
  expect_true(mismatch_count == 0,
              info = paste("Values do not sum up correctly for", mismatch_count, "ISO(s):", toString(mismatches)))
})

TEST <<- FALSE
set_data_location(test=TEST)
remove_test_data_output()
