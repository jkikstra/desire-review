# File where input options can be set for the DLE-emulator

# NB. The parameters below are the default settings, and should be consistent to do multiple module runs as well.
#     In each of the `*_run.R` files, directly after sourcing this file, one can choose to overwrite these settings.

here::i_am("decent.Rproj")

# |||| ---------------------------------
# Decent Living Calculator -------------
# |||| ---------------------------------

# Part 0.0: general input options ====
NUMERICAL.DETAIL.STEPS <<- 6

if(!exists("TEST")){TEST <<- FALSE}

# Part 1.0: Raw input data (`R/calculator_prepare-data.R`) ====

# Part 1.1: DLE related data and input assumption ####
# folder `R/raw-data`
DLE.TARGET.YEAR <<- 2040
# DLE.PROJECTION.FILE <<- paste0(DLE.SSP.SCENARIO.ASSUMPTION, "_", as.character(DLE.TARGET.YEAR), "_lctTRUE_country.csv") # used in the calculator, needs to be CSV file, should be consistent with DLE.SSP.SCENARIO.ASSUMPTION and thus also ASSUMED.SSP
# DLE.PROJECTION.MAPPING.FILE <<- "SHAPE-final_dle-mapping.csv" #"cdlinks_ssps_dle-mapping.csv"
DLE.ASSUMED.SSP <<- "SSP2" # used for where DLE data needs to be  population/gini/gdp data needs to be used
DLE.MESSAGE.MAPPING.ASSUMPTIONS <<- "DLE_to_IAM_simplified.csv" # manually created file
# DLE.THRESHOLD.YEAR <<- DLE.TARGET.YEAR + 10 # used to determine how to calculate the DLE threshold. NB. for consistency, this should be after DLE.TRANSPORTCONVERGENCE.YEAR

DLE.SECTORAL.MAPPING <<- "Mdemand_to_3sector_mapping.csv"

# Part 1.2: Empirical data on energy distributions ####
# folder `R/raw-data`
ENERGY.DISTRIBUTION.FILE <<- "data_to_global_redistribution_of_income_and_household_energy_footprints.xlsx"
ENERGY.DISTRIBUTION.FILE.SHEET <<- "ene.pc"

# Part 1.3: Empirical data on energy/income distribution effects: gini-to-gini method ####
GINI.TO.GINI.METHOD <<- "pure path depency"
if (GINI.TO.GINI.METHOD == "from oswald fig 5") {
  GINI.TO.GINI.FILE <<- "energy_gini_income_gini_static_data.xlsx"
  GINI.TO.GINI.FILE.SHEET <<- "gini_to_gini"
}


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
GINI.PROJECTION.FILE <<- "SHAPE-and-SSP"  #"gini_ssp.csv"
GINI.PROJECTION.VARIABLE <<- "gini"

# Part 2.2: Demographics ####
# folder `R/raw-data`
POPULATION.PROJECTION.FILE <<- "SHAPE_pop_sep2023update_fixssp2CYP.csv"
# POPULATION.PROJECTION.VARIABLE <<- "pop_mil"

# Part 2.3: IAM scenario data ####
IAM.SCENARIO.DATA.FILE <<- "SHAPE-final_v3_2100_wo_smooth_enlong_ENLONG_RATIO"  #"SHAPE-final_v3_2100_GDPCAP_False_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO" # "SHAPE-final_v2" # "CD_LINKS_ssps_downscaled_v3.csv"
DEFAULT.SSP.MAPPING.GINI <<- "NA"
IAM.SCENARIO.DATA.AGGREGATION.CHECK <<- FALSE
ADDITIONAL.REGIONAL.IAM.SCENARIO.DATA.AGGREGATION.CHECK <<- FALSE
IAM.SCENARIO.DATA.STARTYEAR <<- 2010
IAM.SCENARIO.DATA.ENDYEAR <<- 2100
IAM.OUTPUT.SHORT.FILENAME <<- "SHAPE-final" # "cdlinks_ssps" # provide a short name for being able to identify output filestrings
IAM.MAPPING.RESCOM.SPLIT.FILENAME <<- "mapping_shape_rescomsplits_v3_self.csv" # for calculator_activity-fe-process-downscaled.R
IAM.MAPPING.DIRTYRES.SPLIT.FILENAME <<- "mapping_shape_trdaitional_biomass_splits_v3_regional.csv" # for calculator_activity-fe-process-downscaled.R
COMMERCIAL.SHARE.TO.KEEP <<- 0.21 # based on US Commercial Buildings Energy Consumption Survey (CBECS) 2018
RES.TRADBIO.SPLIT.METHOD.CHOICE <- "image-SHAPE-mapped" # SHAPE SDP project

# Part 2.4: technology assumptions
# ADDITIONAL.REGIONAL.IAM.SCENARIO.DATA.FILE <<- paste0(IAM.OUTPUT.SHORT.FILENAME, "_usefulandfinalenergy.csv")
RHO <<- 1

# Part 2.5: projections parameters ####
SCENARIO.START.YEAR <<- 2020 # gini data only starts in 2015, and cdlinks regional scenario data not reported for 2015

# Part 2.6: climate impacts assumptions ####
IMPACTS.COOLING.FILE <<- "cooling_scaling_factor.csv"
WITH.IMPACTS <<- FALSE # haven't run the climate assessment pipeline for this one yet
WITH.DLE.HOUSING.STOCK.CHANGES <<- TRUE # ad-hoc fix to based on save out new and old stock ("SAVE.DLE.HOUSING.STOCK") (could be replaced/adjusted when doing default dle runs without housing efficiency improvements)

# Part 2.7: climate emulator data ####
CLIMATE.EMULATOR.FILE <<- paste0(IAM.OUTPUT.SHORT.FILENAME, "_temperatures_magicc7.csv")

