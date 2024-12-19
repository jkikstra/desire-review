# Script for running DLE-machinery (ERL version) ====

# Part 0.1: load utils functions ====
if(!require(here)){ source(file.path("R","utils.R"))} else {source(here("R","utils.R"))}




# Part 0.2: set input options ====
source(here("R", "input-options.R"))
set_data_location(test = TEST)

# Part 1.0: run DLE machinery for one scenario ====
source(here("R", "dle_core.R"), echo = !SUPPRESS.DLE.PRINTLOG) # NB. requires access to the P-drive
