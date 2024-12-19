# Script for preparing population data for DLE-emulator/assessor

if (POPULATION.PROJECTION.DATA.VERSION == "SHAPE"){
  population.data <- vroom(
    here(get_data_location_raw(test=FALSE),
         "scenario_data",
         "population",
         "SHAPE_pop_sep2023update_fixssp2CYP.csv"), show_col_types=FALSE
  ) %>%
    iamc_wide_to_long(upper.to.lower = F)

  saveRDS(population.data,
          file = here(DATA.LOCATION, "scenario_POPULATION_SHAPEv3.RData")
  )
}

if (POPULATION.PROJECTION.DATA.VERSION == "ssp-v301-2024_ENGAGE_for_MESSAGE_DLE"){

  # NB. below is only for SSP2!
  population.data  <- read_excel(here("data-raw", "1710759470883-ssp_population_release_3.0.1.xlsx"),
                                 sheet = "data") %>%
    iamc_wide_to_long(upper.to.lower = T) %>%
    filter(model == "IIASA-WiC POP 2023", scenario == "SSP2") %>%
    filter(
      !grepl(region, pattern="(R5)", fixed=T),
      !grepl(region, pattern="(R9)", fixed=T),
      !grepl(region, pattern="(R10)", fixed=T),
    ) %>%
    rename(name = region) %>%
    mutate_cond(name=="Micronesia", name="Micronesia (Federated States of)") %>% # countrycode package doesn't recognise "Micronesia" by itself
    mutate(region = countrycode(name, origin = "country.name", destination = "iso3c")) %>%
    select(-name) %>%
    filter(!is.na(region),
           year>=2020) %>%
    mutate(model = "ENGAGE_SSP2_v4.1.8.3.1_T4.5_r3.1",
           scenario = "baseline") %>%
    select(model, scenario, region, variable, unit, year, value)

  saveRDS(population.data,
          file = here(DATA.LOCATION, "scenario_POPULATION_ENGAGE_for_MESSAGE_DLE.RData")
  )
}

if (POPULATION.PROJECTION.DATA.VERSION == "ssp-v301-2024_JUSTMIP"){

  # NB. below is only for SSP2!
  population.data  <- read_excel(here("data-raw", "1710759470883-ssp_population_release_3.0.1.xlsx"),
                                 sheet = "data") %>%
    iamc_wide_to_long(upper.to.lower = T) %>%
    filter(
      !grepl(region, pattern="(R5)", fixed=T),
      !grepl(region, pattern="(R9)", fixed=T),
      !grepl(region, pattern="(R10)", fixed=T),
    ) %>%
    rename(name = region) %>%
    mutate(region = countrycode(name, origin = "country.name", destination = "iso3c")) %>%
    select(-name) %>%
    filter(!is.na(region),
           year>=2020)

  original.regionallevel.iam.data.modscen <- read_csv(
    here(
      get_data_location_raw(test=FALSE),
      "scenario_data",
      "original_regional",
      ORIGINAL.IAM.DATA.FILE
    )
  ) %>% upper_to_lower() %>%
    distinct(model,scenario)

  population.data <- original.regionallevel.iam.data.modscen %>%
    crossing(
      population.data %>% filter(model == "IIASA-WiC POP 2023", scenario == "SSP2") %>% select(-model,-scenario)
    ) %>%
    select(model, scenario, region, variable, unit, year, value)

  saveRDS(population.data,
          file = here(DATA.LOCATION, "scenario_POPULATION_JUSTMIP.RData")
  )
}


#' NOTE:
#' - for (original) SSP population data, one can now use utils.R function `load_population_projection(ssp="all")`


# NOTE:

# - in principle, all IAMs report population too.
# - perhaps best to move to directly dividing by population per direct input.
# - but then population would need to be downscaled too -- not sure this is done, and not sure it would actually be better than just taking SSP population myself.
