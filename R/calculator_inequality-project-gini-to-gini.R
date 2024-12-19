# Script for projecting energy gini based on income gini projections

# model-scenario-iso-year list
scenario.template <- readRDS(
  here(DATA.LOCATION, paste0("scenario_FE", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData"))) %>%
  ungroup() %>%
  select(model, scenario, iso, variable, year) %>%
  distinct() %>%
  arrange(model, scenario, iso, variable, year)
if ( IAM.OUTPUT.SHORT.FILENAME == "SHAPE-final" & grepl(IAM.SCENARIO.DATA.FILE, pattern="v3", fixed=T) ){
  scenario.template <- scenario.template %>%
    add_shape_mapping_gini(default = DEFAULT.SSP.MAPPING.GINI,
                           version = "v3") %>%
    group_by(model, scenario, iso, variable)
} else if ( IAM.OUTPUT.SHORT.FILENAME == "JUSTMIP"){
  scenario.template <- scenario.template %>%
    mutate(scenario.mapping = "SSP2") %>%
    group_by(model, scenario, iso, variable)
} else {
  stop("Cannot map income gini pathways on scenarios. Project definition is unclear? Check your 'IAM.OUTPUT.SHORT.FILENAME' setting.")
}

# load and combine data:
inequality.starting.data <- scenario.template %>%
  # energy inequality starting year
  left_join(
    readRDS(here(DATA.LOCATION,"FE_gini_iso.RData"))%>% mutate(year = SCENARIO.START.YEAR),
    by = c("iso", "variable", "year")
  ) %>%
  left_join(
    readRDS(here(DATA.LOCATION, "FE_gini_iso.RData")) %>% rename(energy.gini.start = energy.gini),
    by = c("iso", "variable")
  ) %>%
  # income projection
  left_join(
    readRDS(here(DATA.LOCATION, "scenario_income_gini.RData")) %>% rename(scenario.mapping = scenario),
    by = c("iso", "year", "scenario.mapping")
  ) %>%
  left_join(
    readRDS(here(DATA.LOCATION, "scenario_income_gini.RData")) %>% rename(scenario.mapping = scenario, gini.start = gini) %>% filter(year == SCENARIO.START.YEAR) %>% select(-year),
    by = c("iso", "scenario.mapping")
  )

if (GINI.TO.GINI.METHOD == "pure path depency") {
  inequality.projection.i <- inequality.starting.data %>%
    drop_na(gini) %>%
    mutate_cond(gini == gini.start, energy.gini = energy.gini.start) %>%
    mutate_cond(gini < gini.start, energy.gini = energy.gini.start + (gini - gini.start) * (energy.gini.start / gini.start)) %>% # linear scaling from energy.gini.start to zero.
    mutate_cond(gini > gini.start, energy.gini = energy.gini.start + (gini - gini.start) * ((100 - energy.gini.start) / (100 - gini.start))) %>% # linear scaling from energy.gini.start to one.
    mutate(gini.to.gini.elasticities.by.country.down = energy.gini.start / gini.start) %>%
    mutate(gini.to.gini.elasticities.by.country.up = (100 - energy.gini.start) / (100 - gini.start))

  inequality.projection.iv <- inequality.projection.i %>%
    select(model, scenario, iso, variable, year, energy.gini, gini.to.gini.elasticities.by.country.down, gini.to.gini.elasticities.by.country.up, gini, scenario.mapping) # gini.to.gini.elasticities.by.country.up/down and scenario.mapping not essential for further processing in the calculator, but good for debugging, understanding and plots
}


log_info("Energy inequality: projection finished")

# save inequality projection
saveRDS(inequality.projection.iv,
  file = here(DATA.LOCATION, paste0("projected_energy_inequality", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData"))
)
