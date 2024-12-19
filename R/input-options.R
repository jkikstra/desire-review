# File where input options can be set for the DLE-emulator
# Set testing variable
if(!exists("TEST")){TEST <<- FALSE}

# NB. The parameters below are the default settings, and should be consistent to do multiple module runs as well.
#     In each of the `*_run.R` files, directly after sourcing this file, one can choose to overwrite these settings.


here::i_am("decent.Rproj")

# Part 0.1: import functions ####
# source(here("R","utils.R")) # it is expected that this is often already loaded before

# |||| ---------------------------------
# Decent Living Energy machinery -------
# |||| ---------------------------------
# Part 1.0: data location ====
DLE.INPUT.DATA.PATH <<- here::here("data-raw", "DLE_scaleup", "Paper_data_final", "input") # to be updated with newer input data handling

# Part 2.0: scenario parameters ====
DLE.SCENARIO.YEAR.END <<- 2100
DLE.SCENARIO.YEAR.BASE <<- 2015
# timestep <<- 5 # currently not tweakable

# basic scenarios options
DLE.TARGET.YEAR <<- 2040
DLE.SSP.SCENARIO.ASSUMPTION <<- "SSP2"

# policy options
TRANSPORT.MODAL.CONVERGENCE <<- FALSE # might need to be mapped based on the scenario assumption above.

# Part 3.0: output preferences ====
# options: c("national.csvdims", "R11.csvdims", "R11.csvneeds", "R11.htmldims", "R11.htmlneeds", "global.htmlneeds")
DLE.SCENARIO.OUTPUT.FORMAT <<- c("national.csvdims", "R11.csvneeds") # if we do "multiplemodulerun_dle_calculator_run.R" then this must at least include "national.csvdims"

SAVE.DLE.HOUSING.STOCK <<- TRUE # ad-hoc fix to save out new and old stock for climate scaling for cooling needs (to be replaced/adjusted when doing default dle runs without housing efficiency improvements)

SUPPRESS.DLE.PRINTLOG <<- TRUE

# |||| ---------------------------------
# Decent Living Calculator -------------
# |||| ---------------------------------

# Part 0.0: general input options ====
NUMERICAL.DETAIL.STEPS <<- 6

# Part 1.0: Raw input data (`R/calculator_prepare-data.R`) ====

# Part 1.1: DLE related data and input assumption ####
# folder `R/raw-data`
DLE.PROJECTION.FILE <<- paste0(DLE.SSP.SCENARIO.ASSUMPTION, "_", as.character(DLE.TARGET.YEAR), "_lctTRUE_country.csv") # used in the calculator, needs to be CSV file, should be consistent with DLE.SSP.SCENARIO.ASSUMPTION and thus also ASSUMED.SSP
ASSUMED.SSP <<- DLE.SSP.SCENARIO.ASSUMPTION # used for where population/gini/gdp data needs to be used
DLE.MESSAGE.MAPPING.ASSUMPTIONS <<- "DLE_to_IAM_simplified.csv" # manually created file
# DLE.THRESHOLD.YEAR <<- 2050 # used to determine how to calculate the DLE threshold. NB. for consistency, this should be after DLE.TRANSPORTCONVERGENCE.YEAR

# Part 1.2: Empirical data on energy distributions ####
# folder `R/raw-data`
ENERGY.DISTRIBUTION.FILE <<- "data_to_global_redistribution_of_income_and_household_energy_footprints.xlsx"
ENERGY.DISTRIBUTION.FILE.SHEET <<- "ene.pc"

# Part 1.3: Empirical data on energy/income distribution effects: gini-to-gini method ####
GINI.TO.GINI.FILE <<- "energy_gini_income_gini_static_data.xlsx"
GINI.TO.GINI.FILE.SHEET <<- "gini_to_gini"

# Part 1.4: Empirical data on energy/income distribution effects: income elasticity method (not used yet ####
ENERGY.ELASTICITY.FILE <<- "energy_gini_income_gini_static_data.xlsx"
ENERGY.ELASTICITY.FILE.SHEET <<- "income_elaticities"


# Part 1.5: Model region definitions, mapping to ISO ####
# subfolder `R/raw-data/iam_regions`
MESSAGE.REGION.MAPPING <<- "region_definitions_message.csv"
IMAGE.REGION.MAPPING <<- "region_definitions_image.csv"
REMIND.REGION.MAPPING <<- "region_definitions_remind.csv"





# Part 2.0: projections data (`R/calculator_core.R`) ====

# Part 2.1: Inequality ####
# folder `R/raw-data`
GINI.PROJECTION.FILE <<- "SHAPE-and-SSP" #"gini_ssp.csv"
GINI.PROJECTION.VARIABLE <<- "gini"

# Part 2.2: Demographics ####
# folder `R/raw-data`
POPULATION.PROJECTION.DATA.VERSION <<- "SHAPE"
POPULATION.PROJECTION.VARIABLE <<- "pop_mil"

# Part 2.3: IAM scenario data ####
IAM.SCENARIO.DATA.FILE <<- "shapefilename" #"CD_LINKS_ssps_downscaled_v3.csv"
IAM.SCENARIO.DATA.AGGREGATION.CHECK <<- FALSE
# ADDITIONAL.REGIONAL.IAM.SCENARIO.DATA.FILE <<- "shapefilename" #"CD_LINKS_ssps_usefulandfinalenergy.csv"
ADDITIONAL.REGIONAL.IAM.SCENARIO.DATA.AGGREGATION.CHECK <<- FALSE
IAM.SCENARIO.DATA.STARTYEAR <<- 2010
IAM.SCENARIO.DATA.ENDYEAR <<- 2100
# IAM.OUTPUT.SHORT.FILENAME <<- "cdlinks_ssps" # provide a short name for being able to identify output filestrings
# IAM.OUTPUT.SHORT.FILENAME <<- "SHAPE-final"
# IAM.OUTPUT.SHORT.FILENAME <<- "mock_FE_scenariodata_ngfsbased"


# Part 2.4: projections parameters ####
SCENARIO.START.YEAR <<- 2020 # gini data only starts in 2015, and cdlinks regional scenario data not reported for 2015

# Part 2.5: countries to visualise in output figures ####
COUNTRIES.TO.VISUALISE <- c(
  "IND",
  "BRA",
  "BLR",
  "ZAF",
  "MDG"
)
LAST.YEAR.TO.VISUALISE.TIMESERIES <- 2050
YEAR.XAXIS.BREAKS.TO.VISUALISE.TIMESERIES <- c(2030, 2050)
YEARS.XAXIS.LIMITS.TO.VISUALISE.TIMESERIES <- c(2010, LAST.YEAR.TO.VISUALISE.TIMESERIES + 6)
YEARS.TO.VISUALISE.MAP <- c(2030, 2050)

FIGURE.TYPES.TO.RUN <- c("maps.total", "timeseries.total", "maps.sectoral", "timeseries.sectoral")
