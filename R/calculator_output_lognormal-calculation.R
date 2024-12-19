# Script for preparing input data for DLE-emulator/assessor

log_info("DLE calculator: all variables have been prepared and projected, combining data now.")

# load data
load_scenario_assessment_data <- function(f.f=here(DATA.LOCATION)){
  #' - `f.f` =  file folder
  scenario.assessment.data <- # 124k rows, for 20 scenarios? 124488/20/9/4 = 172.9, so something's a bit fishy in the data somewhere!
    # final energy per capita (+population)
    readRDS(
      file.path(
        f.f,
        paste0("scenario_FE", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData")
      )
    ) %>% rename_remind_scenarios() %>% # rename_remind probably not necessary anymore since late 2023
    # energy inequality
    left_join(
      readRDS(
        file.path(
          f.f,
          paste0("projected_energy_inequality", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData")
        )
      ) %>% select(-scenario.mapping) %>% ungroup() %>% rename_remind_scenarios(), # rename_remind probably not necessary anymore since late 2023
      by = c("model", "scenario", "iso", "variable", "year")
    ) %>%
    # dle
    left_join(
      readRDS(
        file.path(
          f.f,
          paste0("projected_dle-total-and-sectoral-scaled", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData")
        )
      ),
      by = c("model", "scenario", "iso", "variable", "year")
    ) %>%
    # drop data where missing essential data
    drop_na(energy.per.capita, energy.gini, dle.threshold.curtech, dle.threshold.adjusted) %>%
    # reorder again
    arrange(model, scenario, iso, variable, unit, year)
}

scenario.assessment.data <- load_scenario_assessment_data(f.f = here(DATA.LOCATION)) %>%
  mutate(
    dle.threshold.curtech = dle.threshold.curtech * mega / giga,
    dle.threshold.adjusted = dle.threshold.adjusted * mega / giga
  )

log_info("DLE calculator: obtain share below threshold and depth of deficit - assuming lognormal distribution")

# calculate share of people below threshold and depth of deficit

calculate_scenario_assessment <- function(data){
  data %>%
    mutate(
      sig = GetInverseCDF_lognormal(energy.gini)
    ) %>%
    mutate(nu = log(energy.per.capita) - (sig^2) / 2) %>%
    mutate(share.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "share")) %>%
    mutate(depth.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "DoD")) %>%
    mutate(share.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "share")) %>%
    mutate(depth.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "DoD")) %>%
    select(-sig, -nu) %>%
    return()
}


scenario.assessment.calculated <- calculate_scenario_assessment(data = scenario.assessment.data)

log_info(paste0("DLE calculator: write output to",
                ifelse(exists("OUT.NAME"),
                       here("analyses", "tables", OUT.NAME),
                       here("analyses", "tables", paste0("RESULTS-DLE-emulator-", IAM.OUTPUT.SHORT.FILENAME, "_", Sys.Date()))),
                " in RData format."))
# write out output
if (TEST==FALSE){
  output.folder <- here("analyses", "tables")
} else {
  output.folder <- here(DATA.LOCATION)
}
# write_delim(scenario.assessment.calculated,
#                 file = file.path(output.folder, paste0("RESULTS-DLE-emulator-", IAM.OUTPUT.SHORT.FILENAME, ifelse(
#                   TEST,
#                   "",
#                   paste0("_", Sys.Date())
#                 ), ".csv")),
#                 delim = ","
# )
saveRDS(scenario.assessment.calculated,
        file = file.path(output.folder, paste0("RESULTS-DLE-emulator-", IAM.OUTPUT.SHORT.FILENAME, ifelse(
          TEST,
          "",
          paste0("_", Sys.Date())
        ), ".RData"))
)

