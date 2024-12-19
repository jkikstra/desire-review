#' This tests the script that collects national DLE thresholds
#'
#' Currently passes locally.

library(testthat)
source(here::here('R','utils.R'))
TEST <<- TRUE

# testing body: run script to be tested
source(here::here('R','calculator_input-options.R'))
source(here::here('R','collect-dle-threshold-input.R')) # created `DLE_combined`


# Things that can be improved:
# - [ ] a check & post-processing of EXIOBASE energy intensity for nutrition. Currently, there's a wide range in food energy intensity (0.43-8.0 GJ/cap/yr), with highest energy footprints for European countries


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
    "Cooling OP|rural",
    "Cooling OP|total",
    "Cooling OP|urban",

    "Education|lower_secondary",
    "Education|primary",

    "Health care",

    "Heating CON|rural",
    "Heating CON|total",
    "Heating CON|urban",
    "Heating OP|rural",
    "Heating OP|total",
    "Heating OP|urban",

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
  unit = "MJ/cap/year"
) %>% arrange(variable,unit)


expected.dle.sectors.minimum.pc <- c(
  # minimum (total per country) in GJ/cap/yr per dimension
  0.25, # cooking (corresponding to roughly 9 people in a household)
  0.005, # mobile phone
  0.06, # fridge
  0.09, # television (corresponding to roughly 9 people in a household)

  0.005, # clothes
  0.001, # shoes

  0, # cooling construction
  0, # cooling construction
  0, # cooling construction
  0, # cooling operation
  0, # cooling operation
  0, # cooling operation

  0.01, # education - lower secondary
  0.04, # education - primary

  1, # health care

  0, # heating construction
  0, # heating construction
  0, # heating construction
  0, # heating operation
  0, # heating operation
  0, # heating operation

  0, # hot water (*operation)
  0, # hot water (*operation)
  0, # hot water (*operation)

  0.5, # housing (construction)
  0.5, # housing (construction)
  0.5, # housing (construction)

  0.4, # food

  0, # roads

  0.4, # sanitation

  3, # transport (e.g. India has very low energy per capita)

  0.05 # water supply
)
expected.dle.sectors.maximum.pc <- c(
  2.23, # cooking
  0.5, # mobile phone
  1, # fridge
  1, # television

  2, # clothes (e.g. ISR and GBR very high?)
  0.5, # shoes

  0.1, # cooling construction
  0.1, # cooling construction
  0.1, # cooling construction
  2, # cooling operation
  2, # cooling operation
  2, # cooling operation

  1, # education - lower secondary (can be above 0.5 GJ/cap/yr when a large share of the population, e.g. >6%, is school-going in this level of education)
  1, # education - primary (can be above 0.5 GJ/cap/yr when a large share of the population, e.g. >10%, is school-going in this level of education)

  4, # health care (can be quite energy intensive; see e.g. https://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(23)00169-9/fulltext#fig3)

  0.1, # heating construction
  0.1, # heating construction
  0.1, # heating construction
  10, # heating operation
  10, # heating operation
  10, # heating operation

  4, # hot water (operation)
  4, # hot water (operation)
  4, # hot water (operation)

  1, # housing (construction; but only replacement, not new buildings)
  1, # housing (construction; but only replacement, not new buildings)
  1, # housing (construction; but only replacement, not new buildings)


  9, # food (very high due to rich countries)

  7, # roads (very high due to just Finland; other countries at max 2.7 for AUS, and the rest <2)

  1, # sanitation

  25, # transport

  0.1 # water supply
)
expected.dle.df <- tibble(
  variable = dls.expected.variables.and.units$variable,
  DLE.unit = dls.expected.variables.and.units$unit,
  DLE.min = expected.dle.sectors.minimum.pc*1e3,
  DLE.max = expected.dle.sectors.maximum.pc*1e3
)


# load data
dle.threshold.detail <- read_csv(here(DATA.LOCATION, "DLE_threshold_input.csv"))
dle.threshold <- read_csv(here(DATA.LOCATION, "DLE_threshold_input_simplified.csv"))

dle.threshold.detail.with.expectations <- dle.threshold.detail %>% left_join(expected.dle.df)
dle.threshold.with.expectations <- dle.threshold %>% left_join(expected.dle.df)

# Test set 1. DLE thresholds within within expected minimum and maximum bounds
test_that("DLE per capita thresholds for each country and sector are within the expected ranges",{

  # check that there are not any dimensions where we do not specify min/max thresholds
  expect_equal((dle.threshold.with.expectations %>% filter(is.na(DLE.min)) %>% nrow()),0, info = {
    cat("Rows where we have not specified a minimum threshold:\n")
    print(dle.threshold.with.expectations %>% filter(is.na(DLE.min)) %>% distinct(iso, variable))
  })
  expect_equal((dle.threshold.with.expectations %>% filter(is.na(DLE.max)) %>% nrow()),0, info = {
    cat("Rows where we have not specified a minimum threshold:\n")
    print(dle.threshold.with.expectations %>% filter(is.na(DLE.max)) %>% distinct(iso, variable))
  })

  # check that no threshold is lower than zero (for elec and non-elec)
  expect_equal((dle.threshold.detail %>% filter(thres.energy<0, elec%in%c("elec","non.elec")) %>% nrow()),0)

  # check minimum (for total)
  expect_equal((dle.threshold.detail.with.expectations %>% filter(thres.energy<DLE.min, elec%in%c("total")) %>% nrow()),0, info = {
    cat("Rows where we go below the expected a minimum threshold:\n")
    print(dle.threshold.detail.with.expectations %>% filter(thres.energy<DLE.min, elec%in%c("total")) %>% distinct(iso, variable, thres.energy))
  })
  # check maximum (for total, elec, and non-elec)
  expect_equal((dle.threshold.detail.with.expectations %>% filter(thres.energy>DLE.max, elec%in%c("total","elec","non.elec"),
                                                                  !(iso=="MNG"&grepl(variable,pattern="Heating OP",fixed=T))) %>% nrow()),0, info = {
    cat("Rows where we go above the expected a maximum threshold:\n")
    print(dle.threshold.detail.with.expectations %>% filter(thres.energy>DLE.max, elec%in%c("total")) %>% distinct(iso, variable, thres.energy))
  })
  # expect_failure((dle.threshold.expectationmatching %>% filter(DLE>DLE.max, elec%in%c("total","elec","non.elec")) %>% nrow()),0)
})

# Test set 2. consistency of threshold data
# more possible tests to add here later:
# - [ ] elec and non-elec sum up to total
# - [ ] op and con sum up to total


# Test set 3. mapping fit & threshold before and after mapping
dle.to.iam.mapping <- read_csv(here(get_data_location_raw(test = TRUE), "dle_threshold", "DLE_to_IAM_simplified_withguessedsimpleeemriofactors.csv"))

dle.threshold.detail.with.mapping <- dle.threshold.detail %>%
  select(iso,variable,elec,thres.energy.conrep,unit.energy) %>% rename(thres.energy=thres.energy.conrep) %>%
  left_join(dle.to.iam.mapping %>% filter(energy.type=="con")) %>%
  bind_rows(
    dle.threshold.detail %>%
      select(iso,variable,elec,thres.energy.op,unit.energy) %>% rename(thres.energy=thres.energy.op) %>%
      left_join(dle.to.iam.mapping %>% filter(energy.type=="op"))
  ) %>% drop_na()

dle.threshold.mapped <- dle.threshold.detail.with.mapping %>%
  mutate(
    rescom = thres.energy * rescom,
    industry = thres.energy * industry,
    transport = thres.energy * transport
  ) %>%
  reframe(
    thres.energy.rescom = sum(rescom),
    thres.energy.industry = sum(industry),
    thres.energy.transport = sum(transport),
    .by = c("iso")
  )

expected.dle.mapped.minimum.pc <- c(
  3, # residential and commercial - PNG, VUT, MDV, are the lowest; they all have high household sizes, and low heating and cooling needs
  3.5, # industry - NPL, PAK, BGD are the lowest; low food footprint and low transport needs
  3 # transport - AFG, PAK, NPL are the lowest; transport energy is low because high public share
)
expected.dle.mapped.maximum.pc <- c(
  15, # MNG (13.5), CAN (13), FIN (12) are the highest; a lot of heating
  20, # FIN (19; high roads at 6.1 + high nutrition at 4.5 and high transport construction), LUX (15; high nutrition at 6.3 + high transport construction), AUS (15; also high nutition 4.2, transport 3.1, and roads 2.1) are the highest
  25 # USA, CAN are the highest (21); cars everywhere
)
expected.mapped.dle.df <- tibble(
  variable = c("thres.energy.rescom","thres.energy.industry","thres.energy.transport"),
  DLE.min = expected.dle.mapped.minimum.pc*1e3,
  DLE.max = expected.dle.mapped.maximum.pc*1e3
)

dle.mapped.with.expectations <- dle.threshold.mapped %>% pivot_longer(cols = c("thres.energy.rescom","thres.energy.industry","thres.energy.transport"), values_to = "thres.energy", names_to = "variable") %>%
  left_join(expected.mapped.dle.df)


test_that("DLE per capita thresholds, after mapping, are within the expected ranges",{

  # check that there are not unchecked NAs in the energy values
  expect_equal((dle.mapped.with.expectations %>% filter(is.na(DLE.min)) %>% nrow()),0)
  expect_equal((dle.mapped.with.expectations %>% filter(is.na(DLE.max)) %>% nrow()),0)

  # check minimum
  expect_equal((dle.mapped.with.expectations %>% filter(thres.energy<DLE.min) %>% nrow()),0)
  # check maximum
  expect_equal((dle.mapped.with.expectations %>% filter(thres.energy>DLE.max) %>% nrow()),0)
})



TEST <<- FALSE
set_data_location(test=TEST)
remove_test_data_output()
