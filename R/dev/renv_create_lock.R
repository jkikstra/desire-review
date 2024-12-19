# Create updated lock files for renv




# code inspiration from: https://rstudio.github.io/renv/reference/snapshot.html

# Default ====
renv::init(bare = TRUE) # initialize a new project (with an empty R library)
install_and_load_reqs <- function(){
  if(!require(here)) {
    install.packages("here");
    library(here)
  }
  here::i_am("README.md")
  source(here("R", "dev", "renv_dependencies.R"))  # `selection 3` (3: Cancel, and resolve the situation on your own.)
}
auto.snapshot <- getOption("renv.config.auto.snapshot") # save library state to lockfile; `selection 3` (3: Cancel, and resolve the situation on your own.)
options(renv.config.auto.snapshot = FALSE)
custom_filter <- function(here){
  loaded_packages <- (.packages()) # make sure that only loaded packages (and their dependencies) are saved to the lockfile via custom option in snapshot
}
options(renv.snapshot.filter = custom_filter) # use a specific filter to only save packages in lockfile that are loaded to the library
install_and_load_reqs()
invisible(lapply(pkgs, install.packages, prompt=FALSE, character.only = TRUE)) # install packages with renv
invisible(lapply(pkgs, library, character.only = TRUE)) # install packages with renv
renv::snapshot(lockfile = here("renv.lock"), type = "custom") # save library state to lockfile; `selection 2` (2: Install the packages, then snapshot.)
invisible(renv::remove(packages = pkgs)) # remove packages from library with renv


# Plot packages ====
renv::init(bare = TRUE) # initialize a new project (with an empty R library)
install_and_load_reqs <- function(){
  if(!require(here)) {
    install.packages("here");
    library(here)
  }
  here::i_am("README.md")
  source(here("R", "dev", "renv_dependencies.R"))  # `selection 3` (3: Cancel, and resolve the situation on your own.)
}
auto.snapshot <- getOption("renv.config.auto.snapshot") # save library state to lockfile; `selection 3` (3: Cancel, and resolve the situation on your own.)
options(renv.config.auto.snapshot = FALSE)
custom_filter <- function(here){
  loaded_packages <- (.packages()) # make sure that only loaded packages (and their dependencies) are saved to the lockfile via custom option in snapshot
}
options(renv.snapshot.filter = custom_filter) # use a specific filter to only save packages in lockfile that are loaded to the library
install_and_load_reqs()
invisible(lapply(c(pkgs,plot.pkgs), install.packages, prompt=FALSE, character.only = TRUE)) # install packages with renv
invisible(lapply(c(pkgs,plot.pkgs), library, character.only = TRUE)) # install packages with renv
renv::snapshot(lockfile = here("renv-plot.lock"), type = "custom") # save library state to lockfile; `selection 2` (2: Install the packages, then snapshot.)
invisible(renv::remove(packages = c(pkgs,plot.pkgs))) # remove packages from library with renv

