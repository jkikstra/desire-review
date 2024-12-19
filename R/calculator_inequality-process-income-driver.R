# Script for preparing income inequality driver input data for DLE-emulator/assessor

if (GINI.PROJECTION.FILE == "SHAPE-and-SSP") {

  # Part 3.3.2: process gini data
  income.gini.data.shape <- vroom(
    here("data-raw", "SDP Gini_GDP pathways v1.01.csv"), show_col_types=FALSE # file DOI link: https://doi.org/10.5281/zenodo.13819628
  ) %>%
    rename(scenario=Scenario, iso=iso3c, year=Year, gini=`Gini pathway`) %>%
    filter(
      #  do not use "SSP1" and "SSP2" because they are normative-variants in this file
      scenario %nin% c("SSP1", "SSP2")
    ) %>% mutate(scenario=substr(scenario, nchar(scenario)-1, nchar(scenario))) %>% # keep only EI/RC/MC
    select(scenario, iso, year, !!as.symbol(GINI.PROJECTION.VARIABLE)) %>%
    mutate(gini = gini / 100)
  income.gini.data.shape.aux <- vroom(
    here("data-raw", "SHAPE-final_gini_v3.csv"), show_col_types=FALSE # file produced by a script in data-raw called `prep_shape_gini_data.R` (last update: SHAPE_Gini_v1p3_annual.csv) # for adding VNM
  ) %>%
    rename(iso=region, gini=value) %>%
    filter(iso=="VNM") %>%
    select(scenario, iso, year, !!as.symbol(GINI.PROJECTION.VARIABLE)) %>%
    mutate(gini = gini / 100)
  income.gini.data.shape <- income.gini.data.shape %>% bind_rows(income.gini.data.shape.aux)

  income.gini.data.ssp <- vroom(
    here("data-raw", "gini_ssp.csv"), show_col_types=FALSE
  ) %>%
    rename(iso = country) %>%
    select(scenario, iso, year, !!as.symbol(GINI.PROJECTION.VARIABLE)) %>%
    mutate(gini = gini / 100) %>%
    drop_na()

  income.gini.data <- bind_rows(
    income.gini.data.shape,
    income.gini.data.ssp
  )

}else if (GINI.PROJECTION.FILE == "gini_ssp.csv") {
  # file taken from DLE ERL study data

  # Part 3.3.2: process population data
  income.gini.data <- vroom(
    here("data-raw", GINI.PROJECTION.FILE), show_col_types=FALSE
  ) %>%
    rename(iso = country) %>%
    select(scenario, iso, year, !!as.symbol(GINI.PROJECTION.VARIABLE)) %>%
    mutate(gini = gini / 100) %>%
    drop_na()

}


saveRDS(income.gini.data,
        file = here(DATA.LOCATION, "scenario_income_gini.RData")
)
