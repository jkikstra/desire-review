# Optional: Plotting-related packages
plotting_packages <- c(

  "patchwork", # for multipanel handling
  "ggsci", # for some nice colour palettes
  "treemapify", # for making treemaps
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

install_plot_packages <- function() {
  missing_packages <- plotting_packages[!plotting_packages %in% installed.packages()[, "Package"]]

  if (length(missing_packages)) {
    install.packages(missing_packages)
    renv::snapshot()  # Update renv with newly installed packages
  } else {
    message("All plotting packages are already installed.")
  }
}
