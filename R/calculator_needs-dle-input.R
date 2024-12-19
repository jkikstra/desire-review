log_info("Decent Living minimum energy requirement: load data")

# init -------------------------------------------------------------------------
# ... not needed in this module

# (optional) Update DLE threshold ----------------------------------------------
if (exists("UPDATE.DLE.THRESHOLD")){
  if (UPDATE.DLE.THRESHOLD){
    # run DLE threshold data anew
    source(here("R","collect-dle-threshold-input.R"))
    # write new threshold data
    write_delim(
      x = dle.thres.data,
      here(get_data_location_raw(test = TEST),
           "dle_threshold",
           "DLE_threshold_input.csv"),
      delim = ","
    )
  }
}

# Load data file ---------------------------------------------------------------

dle.pc <- read_csv(here(get_data_location_raw(test = TEST),
                     "dle_threshold",
                     "DLE_threshold_input.csv"), show_col_types=FALSE)

if (WITH.IMPACTS) {
  # The line below is commented out as it is indicated that it doesn't do anything for now.
  # source(here("R", "WIP_calculator_needs-climate-run.R"))
  log_error("The climate impacts code is not up-to-date.")

  # If the integration with climate run is needed in the future, consider uncommenting and using the above line.
  source(here("R", "calculator_needs-climate-scale.R"))

  saveRDS(dle.pc,
          file = here(DATA.LOCATION, "dle-threshold-detail.RData")
  )
} else {
  saveRDS(dle.pc,
          file = here(DATA.LOCATION, "dle-threshold-detail.RData")
  )
}
