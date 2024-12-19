# Dependencies for different lock files
pkgs <<- c(
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
)

plot.pkgs <<- c(
  "patchwork", # for multipanel handling
  "ggsci", # for some nice colour palettes
  "treemapify", # for making treemaps
  "scales", # for scaling axes of ggplot objects
  "rvg", # to make ggplot objects into editable DML objects to write out to powerpoint
  "writexl", # for writing out excel files
  "officer", # e.g. to write out plots as editable figures in powerpoint
  "ggside", # for marginal distributions
  "psych", # for correlation matrix plots in supplement
  "ggthemes", # for nice themes
  "ggh4x", # for doing coord_cartesian like behaviour in a faceted plot
  "qqplotr", # for Q-Q (quantile-quantile) plots

  "geomtextpath", # for lines with text
  "see", # for a nicer-looking geom_pointrange2, and scale_color_bluebrown(), and other sweet things
  "ggridges", # for geom_density_ridges
  "progress", # create a progress bar
  "pracma", # for trapezoidal integration, use the function pracma::cumtrapz
  "treemapify", # for treemaps
  "ggrepel", # for text labels that don't overlap
  "psych", # for pairs.panels
  "goft" # -- simple p.value, based on shapiro-wilk test, like lnorm_test() for SI.21
)

