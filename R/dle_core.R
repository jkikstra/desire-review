# core code for running DLE-machinery (ERL version) ====
# not to be run on its own (e.g. because it doesn't set DATA.LOCATION. Run from `dle_run.R` instead)

# Part 0.0: loading packages ====
# record how long this script takes
old.time <- Sys.time() # get start time

#source(file.path("R","utils.R")) # uncomment if running from this script, instead of from "dle_run"
# source("input-options.R") # uncomment if running from this script, instead of from "dle_run"

out.path <<- here(DATA.LOCATION)
data.path <<- DLE.INPUT.DATA.PATH
year.end <<- DLE.SCENARIO.YEAR.END
year.base <<- DLE.SCENARIO.YEAR.BASE
timestep <<- 5
if (SUPPRESS.DLE.PRINTLOG) {
  quiet(
    load_dimensions()
  )
} else {
  load_dimensions()
}



# with lct (low carbon technologies) options
year.target <<- DLE.TARGET.YEAR
ssp <<- DLE.SSP.SCENARIO.ASSUMPTION
cat(paste0(
  "\n-------------\nPreparing a DLE scenario that achieves DLS for all in ", as.character(DLE.TARGET.YEAR),
  ".\n  Transport mode convergence option: ", as.character(TRANSPORT.MODAL.CONVERGENCE),
  ".\n  Population projection based on: ", as.character(DLE.SSP.SCENARIO.ASSUMPTION),
  "\n-------------\n"
))

# setup
run_initiatialization_input_data(ssp = ssp)

# option A: run and write out
run_accel_scenario_wrapper(ssp = ssp, year.target = year.target, lct = TRANSPORT.MODAL.CONVERGENCE, save.option = DLE.SCENARIO.OUTPUT.FORMAT) # creates iso level data with full energy information.


# # option B: run and investigate in-line (first uncomment this)
# run_accel_scenario(ssp=ssp, year.target=year.target, lct=T, year.start.rollout=2015)
# # option B usage example: write out energy intensity assumptions
# xdf <- DLE.ACCEL$CollectAllEI()
# DLE.ACCEL$SaveAllEI(path = here("analyses","tables", "energy_intensities.csv"))

# END ----
# print elapsed time
new.time <- Sys.time() - old.time # calculate difference
print(paste0("Finished running a DLE scenario. Saved to: ", out.path))
print(new.time) # print in nice format
