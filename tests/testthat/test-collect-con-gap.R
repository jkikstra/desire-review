#' This tests the script that test the national new construction of infrastructure energy/DLE gaps
#'
#' Currently passes locally.

library(testthat)
source(here::here('R','utils.R'))
TEST <<- TRUE

# testing body: run script to be tested
source(here::here('R','collect-con-gap.R'))

DLE_con_gap_tot <- read_csv(here(DATA.LOCATION, "DLE_con_gap_total.csv")) %>%
  drop_na(con_new, unit.gap, unit.con_new_ei) %>%
  filter(elec == "total")

# Are all variables present? ---------------------------------------------------

expected.construction.gap.dimensions <- c(

  "Appliance|clean_cooking_fuel",
  "Appliance|mobile_telephone",
  "Appliance|refrigerator",
  "Appliance|television",

  "Cooling CON|total",
  "Cooling CON|urban",
  "Cooling CON|rural",

  "Heating CON|total",
  "Heating CON|urban",
  "Heating CON|rural",

  "Housing|total",
  "Housing|urban",
  "Housing|rural",

  "Roads",

  "Sanitation",

  "Transport",

  "Water"
)

# expected.zero.construction.gap.dimensions <- c(
#
#   "Clothing|clothing", # due to having energy intensity from EXIOBASE, we only have OP energy for [Health, Education|primary, Education|lower_secondary, Clothing|clothing, Clothing|footwear]
#   "Clothing|footwear", # due to having energy intensity from EXIOBASE, we only have OP energy for [Health, Education|primary, Education|lower_secondary, Clothing|clothing, Clothing|footwear]
#
#   "Education|primary", # due to having energy intensity from EXIOBASE, we only have OP energy for [Health, Education|primary, Education|lower_secondary, Clothing|clothing, Clothing|footwear]
#   "Education|lower_secondary", # due to having energy intensity from EXIOBASE, we only have OP energy for [Health, Education|primary, Education|lower_secondary, Clothing|clothing, Clothing|footwear]
#
#   "Health care", # due to having energy intensity from EXIOBASE, we only have OP energy for [Health, Education|primary, Education|lower_secondary, Clothing|clothing, Clothing|footwear]
#
#   "Hot Water OP|total", # since we assume that hot water provisioning is done with the same boiler as for heating, there is a zero construction gap for hotwater too (as we assign all of it to the heating gap).
#   "Hot Water OP|urban", # since we assume that hot water provisioning is done with the same boiler as for heating, there is a zero construction gap for hotwater too (as we assign all of it to the heating gap).
#   "Hot Water OP|rural", # since we assume that hot water provisioning is done with the same boiler as for heating, there is a zero construction gap for hotwater too (as we assign all of it to the heating gap).
#
#   "Nutrition" # due to having energy intensity from EXIOBASE, we only have OP energy for [Health, Education|primary, Education|lower_secondary, Clothing|clothing, Clothing|footwear]
#
# )
#
# expected.nonzero.construction.gap.dimensions <- expected.construction.gap.dimensions[expected.construction.gap.dimensions %nin% expected.zero.construction.gap.dimensions]

# What dimensions must be there?
test_that("Expected number of dimensions available", {

  expect_true(setequal(expected.construction.gap.dimensions,
                       DLE_con_gap_tot %>% pull(variable) %>% unique()))
})

total.con.new.by.dimension.global <- DLE_con_gap_tot %>%
  filter(variable %nin% c(
    "Housing|rural",
    "Housing|urban",

    "Heating CON|rural",
    "Heating CON|urban",

    "Cooling CON|rural",
    "Cooling CON|urban"
  )) %>%
  reframe(con.total=sum(con_new),
          .by = "variable")
total.con.new.global <- DLE_con_gap_tot %>%
  filter(variable %nin% c(
    "Housing|rural",
    "Housing|urban",

    "Heating CON|rural",
    "Heating CON|urban",

    "Cooling CON|rural",
    "Cooling CON|urban"
  )) %>%
  reframe(con.total=sum(con_new)) %>% pull(con.total)




# Do global values make sense? -------------------------------------------------

# Expect that all dimensions have >0 values
test_that("Expected dimensions with non-zero construction gap", {

  expect_equal(expected.construction.gap.dimensions %>% length()-6,
               total.con.new.by.dimension.global %>% filter(con.total>0) %>% nrow()
               )
})

# No NAs in construction gap data
test_that("No NAs in data where they should not be ", {

  expect_true(
    sum(is.na(DLE_con_gap_tot))==0
  )

})

# The total construction gap should be in the order of magnitude, but smaller than, 290 EJ (as reported in ERL2021, which sees significant population growth), so let's say,
test_that("total construction gap is between 150 and 290", {

  print(total.con.new.global)
  expect_lt(
    object = total.con.new.global,
    expected = 290
  )

  expect_gt(
    object = total.con.new.global,
    expected = 150
  )

})


# Do regional and national values make sense? ----------------------------------

total.con.new.by.dimension.iso <- DLE_con_gap_tot %>%
  filter(variable %nin% c(
    "Housing|rural",
    "Housing|urban",

    "Heating CON|rural",
    "Heating CON|urban",

    "Cooling CON|rural",
    "Cooling CON|urban"
  )) %>%
  reframe(con.total=sum(con_new),
          .by = c("variable", "iso") )
total.con.new.iso <- DLE_con_gap_tot %>%
  filter(variable %nin% c(
    "Housing|rural",
    "Housing|urban",

    "Heating CON|rural",
    "Heating CON|urban",

    "Cooling CON|rural",
    "Cooling CON|urban"
  )) %>%
  reframe(con.total=sum(con_new),
          .by = "iso") %>%
  rename(con.totalsum=con.total)
total.con.new.share.by.dimension.iso <- total.con.new.by.dimension.iso %>%
  left_join(total.con.new.iso) %>%
  mutate(share=con.total/con.totalsum)

### Do national values make sense? ---------------------------------------------
# Housing, globally, should be the largest share of the construction gap, so let's say, at minimum 40%, for more than 90% of the countries
test_that("Housing should be the largest share of the construction gap (global, afr, sas, cpa)", {

  total.con.new.share.housing.global <- (total.con.new.by.dimension.global %>%
                                           filter(variable=="Housing|total") %>%
                                           pull(con.total))/(
    total.con.new.global
  )

  expect_gt(
    object = total.con.new.share.housing.global,
    expected = 0.3
  )


  total.con.new.afr <- total.con.new.share.by.dimension.iso %>% left_join(
    message.R11 %>% select(-country_name)
  ) %>% filter(R11.region=="AFR")
  total.con.new.share.housing.afr <- (total.con.new.afr %>%
                                        filter(variable=="Housing|total") %>%
                                        pull(con.total) %>% sum()
                                      )/(
                                        total.con.new.afr %>%
                                          pull(con.total) %>% sum()
                                        )

  expect_gt(
    object = total.con.new.share.housing.afr,
    expected = 0.4
  )

  total.con.new.sas <- total.con.new.share.by.dimension.iso %>% left_join(
    message.R11 %>% select(-country_name)
  ) %>% filter(R11.region=="SAS")
  total.con.new.share.housing.sas <- (total.con.new.sas %>%
                                        filter(variable=="Housing|total") %>%
                                        pull(con.total) %>% sum()
  )/(
    total.con.new.sas %>%
      pull(con.total) %>% sum()
  )

  expect_gt(
    object = total.con.new.share.housing.sas,
    expected = 0.5
  )

  total.con.new.cpa <- total.con.new.share.by.dimension.iso %>% left_join(
    message.R11 %>% select(-country_name)
  ) %>% filter(R11.region=="CPA")
  total.con.new.share.housing.cpa <- (total.con.new.cpa %>%
                                        filter(variable=="Housing|total") %>%
                                        pull(con.total) %>% sum()
  )/(
    total.con.new.cpa %>%
      pull(con.total) %>% sum()
  )

  expect_gt(
    object = total.con.new.share.housing.cpa,
    expected = 0.3
  )

})

### Do national values make sense? ---------------------------------------------


# CONTINUE HEREEEE......
# total.con.new.share.by.dimension.iso...


# Housing, globally, should be a large share of the construction gap, so let's say, at minimum 30%, in most of the sub-saharan African countries
test_that("Housing should a large share of the construction gap, in most of the sub-saharan African countries", {

  housing.too.low <- total.con.new.share.by.dimension.iso %>%
    filter(variable=="Housing|total") %>%
    mutate(
      housing.percentage.too.low = ifelse(share < 0.3 ,
                                          1 , 0)
    ) %>%
    left_join(
      message.R11 %>% select(-country_name)
    ) %>% filter(R11.region=="AFR")

  expect_lt(
    object = housing.too.low %>% pull(housing.percentage.too.low) %>% sum(),
    expected = housing.too.low %>% nrow() * 0.2 # for less than 20% of the countries in R11_AFR
  )

})

# Housing+Transport infrastructure should be the majority, so let's say, at minimum 60%, for a large share of the countries
test_that("Housing, Transport, and Roads are expected to be the largest share of the construction gap for most countries", {

  ## Housing + Transport + Roads
  total.con.new.housing.and.transport.and.roads.shares.high.enough <- total.con.new.share.by.dimension.iso %>%
    filter(variable%in%c(
      "Housing|total",
      "Transport",
      "Roads"
    )) %>%
    reframe(
      housing.and.transport.and.roads=sum(share),
      .by = "iso"
    ) %>%
    mutate(
      housing.and.transport.and.roads.high.enough = ifelse(housing.and.transport.and.roads > 0.8 ,
                                                 1 , 0)
    )

  # all countries higher than 80%
  expect_equal(
    object = total.con.new.housing.and.transport.and.roads.shares.high.enough %>% pull(housing.and.transport.and.roads.high.enough) %>% sum(),
    expected = total.con.new.housing.and.transport.and.roads.shares.high.enough %>% nrow() * 1
  )


  ## Housing + Transport
  total.con.new.housing.and.transport.shares.high.enough <- total.con.new.share.by.dimension.iso %>%
    filter(variable%in%c(
      "Housing|total",
      "Transport"
    )) %>%
    reframe(
      housing.and.transport=sum(share),
      .by = "iso"
    ) %>%
    mutate(
      housing.and.transport.high.enough = ifelse(housing.and.transport > 0.6 ,
                                          1 , 0)
    )

  # most countries globally
  expect_gt(
    object = total.con.new.housing.and.transport.shares.high.enough %>% pull(housing.and.transport.high.enough) %>% sum(),
    expected = total.con.new.housing.and.transport.shares.high.enough %>% nrow() * 0.8
  )

  # all countries in South Asia
  expect_equal(
    object = total.con.new.housing.and.transport.shares.high.enough %>% left_join(message.R11) %>% filter(R11.region=="SAS") %>%
      pull(housing.and.transport.high.enough) %>% sum(),
    expected = total.con.new.housing.and.transport.shares.high.enough %>% left_join(message.R11) %>% filter(R11.region=="SAS") %>% nrow()
  )

  # most countries in Sub-Saharan Africa (noting that at times roads shares can be high)
  expect_gt(
    object = total.con.new.housing.and.transport.shares.high.enough %>% left_join(message.R11) %>% filter(R11.region=="AFR") %>%
      pull(housing.and.transport.high.enough) %>% sum(),
    expected = total.con.new.housing.and.transport.shares.high.enough %>% left_join(message.R11) %>% filter(R11.region=="AFR") %>% nrow() * 0.7
  )

})


# End the test file by removing all test-data
TEST <<- FALSE
set_data_location(test=TEST)
remove_test_data_output()
