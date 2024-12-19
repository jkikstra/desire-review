library(here) # for easy and clear relative paths
library(vroom) # for reading csv files fast
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
here::i_am("decent.Rproj")

# simple progress bar
# install.packages("progress")
library(progress)
# set vroom load progress bars to false
# Sys.getenv("VROOM_SHOW_PROGRESS") # check current setting
Sys.setenv("VROOM_SHOW_PROGRESS"="false") # change to not seeing a progress bar when reading data with vroom
# Sys.getenv("VROOM_SHOW_PROGRESS") # check new setting

if(!require(here)){ source(file.path("R","utils.R"))} else {source(here("R","utils.R"))} # common utils for this repository
# source(here("R", "calculator_utils.R"))

source(here("R", "calculator_input-options.R"))
set_data_location(test = TEST)

CALCULATOR.INPUT.OPTIONS.OVERRIDE.CALLED <<- TRUE

DEFAULT.OR.ALLSENSITIVITY <<- "all-downscaling" # c("default" DONE, "all-downscaling" RUNNING, "all-rho", NOT USED)
GINI.SCALING <<- "default" # c("default" DONE, "90p" DONE)

# SHAPE versions ---------------------------------------------------------------
# SHAPE_version <- "shape_v0_1" # original submission
SHAPE_version <- "shape_v1_1_0" # resubmission

# RUN.DATE <- "2024-04-08" # original submission
RUN.DATE <- "2024-11-15" # resubmission


if (DEFAULT.OR.ALLSENSITIVITY=="default"){
  rho_options <- c(1)
  downscaling_options <- c(
    "SHAPE-final_v3_2100_GDPCAP_True_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO.csv", # preferred downscaling method (fast convergence)
    "SHAPE-final_v3_2150_GDPCAP_True_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO.csv",  # preferred downscaling method (medium convergence)
    "SHAPE-final_v3_2200_GDPCAP_True_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO.csv"  # preferred downscaling method (slow convergence)
  )
  # default method "GDPCAP_True_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO" discussed in email from Fabio Sferra on Tue 19/12/2023 10:02, called "RE: Downscaling: performance for in 2015"
  # default convergence is in scenario_config; "C:/Users/kikstra/IIASA/ECE.prog - Documents/Projects/Decent Living/DELIRIUM/2023 manuscript shape/current_manuscript_files/input_data/downscaled_data/scenario_config_20230823.csv" or "data-raw\data-after-downscaling\shape-final-v3\scenario_config_20230823.csv"
} else if (DEFAULT.OR.ALLSENSITIVITY=="all-rho"){
  # testing efficiency scaling using rho
  # NB. only for "fast convergence", like SDP -- NOT for e.g. SSP2-Ref/SSP2-NPi
  rho_options <- c(0.25,0.5,0.75,1,1.25,1.5)
  downscaling_options <- c(
    "SHAPE-final_v3_2100_GDPCAP_True_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO.csv" # preferred downscaling method (fast convergence)
  )
} else if (DEFAULT.OR.ALLSENSITIVITY=="all-downscaling"){
  rho_options <- c(1)
  downscaling_options <- c(
    "SHAPE-final_v3_2100_GDPCAP_False_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2100_GDPCAP_False_ENSHORT_REF_to_ENLONG_RATIO_2100_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2100_GDPCAP_True_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO.csv", # preferred downscaling method (fast convergence)
    "SHAPE-final_v3_2100_GDPCAP_True_ENSHORT_REF_to_ENLONG_RATIO_2100_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2100_TIME_False_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2100_TIME_False_ENSHORT_REF_to_ENLONG_RATIO_2100_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2100_TIME_True_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2100_TIME_True_ENSHORT_REF_to_ENLONG_RATIO_2100_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2100_wo_smooth_enlong_ENLONG_RATIO.csv",
    "SHAPE-final_v3_2150_GDPCAP_False_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2150_GDPCAP_False_ENSHORT_REF_to_ENLONG_RATIO_2100_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2150_GDPCAP_True_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO.csv",  # preferred downscaling method (medium convergence)
    "SHAPE-final_v3_2150_GDPCAP_True_ENSHORT_REF_to_ENLONG_RATIO_2100_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2150_TIME_False_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2150_TIME_False_ENSHORT_REF_to_ENLONG_RATIO_2100_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2150_TIME_True_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2150_TIME_True_ENSHORT_REF_to_ENLONG_RATIO_2100_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2150_wo_smooth_enlong_ENLONG_RATIO.csv",
    "SHAPE-final_v3_2200_GDPCAP_False_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2200_GDPCAP_False_ENSHORT_REF_to_ENLONG_RATIO_2100_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2200_GDPCAP_True_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO.csv",  # preferred downscaling method (slow convergence)
    "SHAPE-final_v3_2200_GDPCAP_True_ENSHORT_REF_to_ENLONG_RATIO_2100_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2200_TIME_False_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2200_TIME_False_ENSHORT_REF_to_ENLONG_RATIO_2100_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2200_TIME_True_ENSHORT_REF_to_ENLONG_RATIO_2050_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2200_TIME_True_ENSHORT_REF_to_ENLONG_RATIO_2100_thenENLONG_RATIO.csv",
    "SHAPE-final_v3_2200_wo_smooth_enlong_ENLONG_RATIO.csv"
  )
}






total.options <- length(rho_options)*length(downscaling_options)
pb <- progress_bar$new(total = total.options, format = "Running all options [:bar] :current/:total (:percent)")


rho_option_number <- 0
downscaling_option_number <- 0

pb$tick(0)

for (r in rho_options) {
  RHO <<- r
  rho_option_number <- rho_option_number + 1

  downscaling_option_number <- 0
  for (d in downscaling_options){
    downscaling_option_number <- downscaling_option_number + 1


    IAM.SCENARIO.DATA.FILE <<- substr(d, start=0, stop = nchar(d)-nchar(".csv") )


    # run the scenario
    if (GINI.SCALING == "default"){
      source(here("R", "calculator_run.R")) # N.B.; since rho only comes in at a specific place, this loop could be made quite a bit faster if you like, by not rerunning processing steps that come before
    } else if (GINI.SCALING=="90p"){
      source(here("R", "sensitivity", "calculator_run_90p.R")) # N.B.; since rho only comes in at a specific place, this loop could be made quite a bit faster if you like, by not rerunning processing steps that come before
    }

    # rename the output file by rereading and rewriting
    output.name.i <- file.path(output.folder, paste0("RESULTS-DLE-emulator-", IAM.OUTPUT.SHORT.FILENAME, ifelse(
      TEST,
      "",
      paste0("_", Sys.Date())
    ),
    "r", as.character(rho_option_number),
    "d", as.character(downscaling_option_number)))

    write_delim(readRDS(
      file.path(output.folder, paste0("RESULTS-DLE-emulator-", IAM.OUTPUT.SHORT.FILENAME, ifelse(
        TEST,
        "",
        paste0("_", Sys.Date())
      ), ".RData"))
    ),
    file = paste0(output.name.i,
                  ".csv"),
    delim = ","
    )

    saveRDS(readRDS(
      file.path(output.folder, paste0("RESULTS-DLE-emulator-", IAM.OUTPUT.SHORT.FILENAME, ifelse(
        TEST,
        "",
        paste0("_", Sys.Date())
        ), ".RData"))),
      file = paste0(output.name.i,
                    ".RData"))
    write_csv(
      x = data.frame(
        downscaling_file = d,
        rho_setting = r
      ),
      file = paste0(output.name.i,
             "_inputoptions.csv")
    )


    pb$tick()


    FIRST.TIME.RUN.PREPARE.REGIONAL.DATA <<- FALSE # save time in next runs (used in `calculator_needs-dle-efficiency.R`)

  }
}

# If default, recombine data to a default data set
if (DEFAULT.OR.ALLSENSITIVITY=="default"){
  OUTPUT.FILE.NAME <- "shape_submission_results_default_version_1_1_0_rc2_20241114.csv"

  scenario.config.file <- read_csv(here("data-raw", "data-after-downscaling", "shape-final-v3", "scenario_config_20230823.csv")) %>%
    mutate(
      default.results.file = NA_character_
    ) %>%
    mutate_cond(convergence=="fast"&sensitivity=="preferred", default.results.file = paste0("RESULTS-DLE-emulator-SHAPE-final_",RUN.DATE,"r1d1")) %>%
    mutate_cond(convergence=="medium"&sensitivity=="preferred", default.results.file = paste0("RESULTS-DLE-emulator-SHAPE-final_",RUN.DATE,"r1d2")) %>%
    mutate_cond(convergence=="slow"&sensitivity=="preferred", default.results.file = paste0("RESULTS-DLE-emulator-SHAPE-final_",RUN.DATE,"r1d3")) %>%
    select(Model,Scenario,default.results.file) %>%
    rename(model=Model,scenario=Scenario) %>%
    drop_na()

  default.rawdata <- readRDS(file.path(output.folder, paste0("RESULTS-DLE-emulator-SHAPE-final_",RUN.DATE,"r1d1.RData"))) %>%
    mutate(data.file=paste0("RESULTS-DLE-emulator-SHAPE-final_",RUN.DATE,"r1d1")) %>%
    bind_rows(
      readRDS(file.path(output.folder, paste0("RESULTS-DLE-emulator-SHAPE-final_",RUN.DATE,"r1d2.RData"))) %>%
        mutate(data.file=paste0("RESULTS-DLE-emulator-SHAPE-final_",RUN.DATE,"r1d2"))
    ) %>%
    bind_rows(
      readRDS(file.path(output.folder, paste0("RESULTS-DLE-emulator-SHAPE-final_",RUN.DATE,"r1d3.RData"))) %>%
        mutate(data.file=paste0("RESULTS-DLE-emulator-SHAPE-final_",RUN.DATE,"r1d3"))
    )

  scenario.config.file %>% scenario_unique() == default.rawdata %>% scenario_unique()

  default.data <- scenario.config.file %>%
    left_join(default.rawdata,
              # relationship = "many-to-many",
              by=c("model","scenario")
              ) %>%
    filter(default.results.file==data.file) %>%
    select(-data.file)


  if (GINI.SCALING == "default"){
    write_delim(
      default.data,
      file = here("analyses", "tables", SHAPE_version, OUTPUT.FILE.NAME),
      delim = ","
    )
  } else if (GINI.SCALING=="90p"){
    write_delim(
      default.data,
      file = here("analyses", "tables", SHAPE_version, paste0("90p_", OUTPUT.FILE.NAME)),
      delim = ","
    )
  }




}

