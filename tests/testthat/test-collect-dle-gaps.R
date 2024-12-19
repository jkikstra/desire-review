#' This tests the script that collects national DLE gaps
#'
#' Currently passes locally. (Note: still very high gap in South Korea; TESTSKIP.HEATINGHOUSINGKOREA.FIXED, Issue #220)

library(testthat)
source(here::here('R','utils.R'))
TEST <<- TRUE

# testing body: run script to be tested
source(here::here('R','collect-dle-gaps.R'))

dle.gap.detail <- read_csv(here(DATA.LOCATION, "DLE_gaps_alldata.csv"))
dle.gap <- read_csv(here(DATA.LOCATION, "DLE_gaps.csv"))
dle.gap.totals <- read_csv(here(DATA.LOCATION, "DLE_gaps_totals.csv"))



# total should be lower than 80-90 EJ (as reported in ERL2021), due to no population growth so let's say, between 30 and 80 EJ
test_that("total energy gap is between 30 and 80 EJ", {

  total.dle.gap <- dle.gap.totals %>% left_join(pop %>%
                                                  filter(year==year.base) %>%
                                                  select(iso,population)) %>%
    mutate(gap.ej = gap*population*mega/exa) %>%
    reframe(total.gap=sum(gap.ej))

  expect_lt(
    object = total.dle.gap$total.gap,
    expected = 80
  )

  expect_gt(
    object = total.dle.gap$total.gap,
    expected = 30
  )

})

# no gap is higher than 20
test_that("no energy gap is higher than 20 GJ/cap/yr", {

  max.dle.gap <- dle.gap.totals %>%
    reframe(max.gap=max(gap)*mega/giga)

  expect_lt(
    object = max.dle.gap$max.gap,
    expected = 20
  )

})


# highest gap is expected in transport for many
test_that("Largest energy gap is expected in transport (for more than half)", {

  max.dle.gap.dimension <- dle.gap %>%
    left_join(
      dle.gap %>%
        reframe(max.gap=max(gap), .by=c("iso"))
    ) %>% filter(gap==max.gap)

  #
  expect_lt(
    object = max.dle.gap.dimension %>% filter(variable!="Transport") %>%
      nrow(),
    expected = max.dle.gap.dimension %>%
      nrow() / 2
  )

})

# test that there should not be any high-income countries with large gaps?
high.income.countries <- load_official_country_grouping(grouping.to.load = "Income _status_WB") %>%
  filter(`Income _status_WB`=="High")

test_that("None of the 10 highest be any high-income countries with large gaps", {

  exceptions <- c("KOR") # ~11.1 GJ/cap # South Korea until Issue #220 has been resolved

  highest.dle.gaps <- dle.gap.totals %>% filter(iso%nin%exceptions) %>%
    slice_max(gap, n = 10)

  expect_equal(
    object = highest.dle.gaps %>% left_join(high.income.countries) %>%
      filter(!is.na(`Income _status_WB`)) %>%
      nrow(),
    expected = 0,
    label = highest.dle.gaps %>% left_join(high.income.countries) %>%
      filter(!is.na(`Income _status_WB`)) %>% pull(iso)
  )

})

test_that("No high-income country has a gap larger than 15 GJ/cap/yr", {

  exceptions <- c("KOR")

  expect_lt(
    object = dle.gap.totals %>% left_join(high.income.countries) %>%
      filter(!is.na(`Income _status_WB`)) %>%
      filter(!(iso %in% exceptions)) %>%
      pull(gap) %>% max(),
    expected = 15e3,
    label = dle.gap.totals %>% left_join(high.income.countries) %>%
      filter(!is.na(`Income _status_WB`)) %>%
      filter(!(iso %in% exceptions),
             gap>15e3) %>% pull(iso)
  )

})


# End the test file by removing all test-data
TEST <<- FALSE
set_data_location(test=TEST)
remove_test_data_output()
