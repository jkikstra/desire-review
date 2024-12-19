# Script for running DLE-calculator ====

# Part 0.0: loading packages ====
old.time.calculator.all <- Sys.time() # get start time

# Part 0.1: load utils functions ====
if(!require(here)){ source(file.path("R","utils.R"))} else {source(here("R","utils.R"))} # common utils for this repository



# Part 0.2: set input options ====
if (exists("CALCULATOR.INPUT.OPTIONS.OVERRIDE.CALLED")) {
  if (CALCULATOR.INPUT.OPTIONS.OVERRIDE.CALLED == TRUE) {
    # skip loading input options
  }
} else {
  source(here("R", "calculator_input-options.R"))
  set_data_location(test = TEST)
}

# Part 1.0: run modules ====
# NB. it is possible that the order would need to change later in development

# 1.0.1 load other variables
source(here("R", "calculator_population.R")) # required for going from downscaled EJ to GJ/cap

# 1.1 data processing:

# energy activity data
# - run processing of the scenarios
source(here("R", "calculator_activity-fe-process-downscaled.R")) # now called before DLE mapping, because its final energy is used to split DLE threshold mapping splits within regions for the MRIO table mapping

# DLE data
# print(WITH.IMPACTS) # for calculator_needs-dle-input.R
source(here("R", "calculator_needs-dle-input.R"))
if (WITH.IMPACTS){
  log_error("The climate impacts code is not up-to-date.")
}
gc() # free unused r memory

source(here("R", "calculator_needs-dle-mapping.R"))
rm(dle.neat) #
gc() # free unused r memory

# energy inequality starting point data
source(here("R", "calculator_inequality-fe-process-starting.R")) # could consider splitting processing and mapping?
source(here("R", "calculator_inequality-simple-filling-fe-process-starting.R"))


# 1.2 service provisioning efficiency for scaling decent living energy thresholds:
source(here("R", "calculator_needs-dle-efficiency.R"))


# 1.3 energy inequality projection:

# project energy inequality:
source(here("R", "calculator_inequality-process-income-driver.R"))
source(here("R", "calculator_inequality-project-gini-to-gini.R"))


# 2.0 get results calculate output variables:
source(here("R", "calculator_output_lognormal-calculation.R"))


# print out the time
total.time.calculator.all <- difftime(Sys.time(), old.time.calculator.all, units = "mins") %>%
  as.character() %>%
  substr(0, 4) # calculate time spent in minutes
log_info(paste0(
  "DLE calculator: finished.\n",
  "Time taken: ", as.character(total.time.calculator.all), " minutes.\n",
  "Number of scenarios assessed: ", scenario.assessment.calculated %>% ungroup() %>% select(model, scenario) %>% distinct() %>% nrow() %>% as.character(), "\n",
  "Number of countries included in assessment: ", scenario.assessment.calculated %>% ungroup() %>% select(iso) %>% distinct() %>% nrow() %>% as.character(), "\n"
))
