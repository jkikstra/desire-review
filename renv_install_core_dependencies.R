renv::init()

install.packages(c(
  "here",
  "tidyverse", # for data wrangling
  "vroom", # for loading in CSV files fast
  "zoo", # for imputation using `na.approx`
  "lestat", # for inverse cumulative distribtuion for depth of deficit (note - also loads the MASS function `select`, so tidyverse needs to be loaded later)
  "countrycode", # for working easily with country codes
  "readxl", # for reading in excel files
  "plotly", # for interactive plots; used in `R/DLE_integration_data_structure.R`
  "htmlwidgets", # for saving interactive plots, not always used, but part of `R/utils.R` under function save_scenario_to_html_timeseries()
  "sitools", # for clear unit changes, required by DLE_integration_data_structure.R
  # "here", # for specifying relative paths; commented out and added to DESCRIPTION
  "lme4", # for LmList
  "scales", # creat lockfile script did not work when not separately specified
  "logger", # for info (log_info), warnings (log_warn), and debugging (log_debug) -- with the option set in log_threshold, i.e. `log_threshold(TRACE)` to log everything

  # packages for developers of this code:
  "testthat", #for code testing
  "styler" #for styling code
))  # Add your core packages here

renv::snapshot()
