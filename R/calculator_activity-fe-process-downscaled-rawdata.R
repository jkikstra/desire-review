# Recombine downscaled data files into standard IAMC format.
#' is model specific, depending on the model regions.
#' Two options, if we need to loop over the model region names:
#' 1) write down the model regions here
#' 2) take the model regions from the `load iam regions function.
#'
#'
#' Last updated: 15.11.2023: to enable loading downscaled SHAPE files with downscaling-sensitivity runs; without specifying the model
#' Last updated: 18.12.2023: to include "Final Energy|Commercial" now too in the latest downscaling of IMAGE
#' Last updated: 26.01.2024: to update the scenario data (& include the new default/sensitivity downscaling files from Fabio - which also include emissions for the default); sent over on 26/01/2024
#' Last updated: 27.02.2024: to add an ad-hoc script to not accidentally load in unwanted data & update to the latest submission-ready default/sensitivity downscaling files from Fabio - which also include emissions for the default; was said to be ready by Fabio on 12 February, on Teams, to Jarmo

# early work was already done in the file:
#   "C:\Users\kikstra\IIASA\ECE.prog - Documents\Projects\Decent Living\DLE Calculator (DLE Provisioning Risk)\data\downscaling\output\MESSAGEix-GLOBIOM 1.0_CD_LINKS_JARMO_v3\combine_IAMC_DATA.R"

DOWNSCALING.VARIABLES <- c(
  "Final Energy",
  "Final Energy|Transportation",
  "Final Energy|Residential and Commercial",
  "Final Energy|Industry",
  "Final Energy|Residential",
  "Final Energy|Commercial"
)

IAM.OUTPUT.SHORT.FILENAME <- "SHAPE-final"

if(IAM.OUTPUT.SHORT.FILENAME == "SHAPE-final"){
  DATA.PATH.RAW.DOWNSCALED <<- here(
    "data-raw",
    "data-after-downscaling",
    "shape-final-v3",
    "sensitivity"
  ) # N.B. should have all files for the scenarios you want to run, with now subfolders
  data_path <- DATA.PATH.RAW.DOWNSCALED
}

# get all filenames
files <- dir(data_path, pattern = "*.csv") # get file names
# remove file data if it is incomplete
files.downscaled <- files[!grepl(files, pattern = "_iea_countries", fixed = T)]
files.downscaled <- files[!grepl(files, pattern = "_test", fixed = T)]
# split in downscaled, and native files
files.downscaled <- files[!grepl(files, pattern = "_native.csv", fixed = T)]
files.native <- files[files %>% grepl(pattern = "_native.csv", fixed = T)]

#Load all files and bind into one
if(IAM.OUTPUT.SHORT.FILENAME == "SHAPE-final"){


  all_scenarios_downscaled <- files.downscaled %>%
    map(~ (vroom(file.path(data_path, .), show_col_types=FALSE) %>%
             mutate(CONVERGENCE = as.character(CONVERGENCE)) %>%
             filter(VARIABLE%in%c(
               DOWNSCALING.VARIABLES
             ))) ) %>%
    reduce(rbind)

  # bring back in native countries (for the native countries, the CONVERGENCE and METHOD columns are empty)

  native_data <- files.native %>%
    map(~ (vroom(file.path(data_path, .), show_col_types=FALSE) %>%
             mutate(CONVERGENCE = as.character(CONVERGENCE)) %>%
             filter(VARIABLE%in%c(
               DOWNSCALING.VARIABLES
             ))) ) %>%
    reduce(rbind) %>%
    select(-CONVERGENCE,-METHOD)

  downscaling_methods <- all_scenarios_downscaled %>% distinct(CONVERGENCE,METHOD)

  native_data_for_recombining <- native_data  %>%
    crossing(downscaling_methods)

  all_scenarios <- bind_rows(
    native_data_for_recombining,
    all_scenarios_downscaled
  ) %>% arrange(MODEL,TARGET,ISO,VARIABLE,CONVERGENCE,METHOD)

}


#clean format datafrane
if(IAM.OUTPUT.SHORT.FILENAME == "SHAPE-final"){

  all_scenarios_out <- all_scenarios %>%
    mutate(UNIT = "EJ/yr") %>%
    rename(
      SCENARIO=TARGET
    ) %>%
    filter(SCENARIO %in% return_key_scenarios_shape()) %>%
    select(MODEL,SCENARIO,ISO,VARIABLE,UNIT,`2010`:`2100`, CONVERGENCE, METHOD)

}



if(IAM.OUTPUT.SHORT.FILENAME == "SHAPE-final"){

  for (m in (all_scenarios_out %>% pull(METHOD) %>% unique())){
    for (c in (all_scenarios_out %>% pull(CONVERGENCE) %>% unique())){
      write_delim(
        all_scenarios_out %>% filter(METHOD==m,CONVERGENCE==c) %>% select(-METHOD,-CONVERGENCE),
        file = here(
          "data-raw",
          "scenario_data",
          paste0("SHAPE-final_v3","_",c,"_",m,".csv")
        ),
        delim = ","
      )
    }
  }

}

