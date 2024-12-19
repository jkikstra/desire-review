# Create one file to do IMAGE and REMIND aviation correction for final energy in transport part of `calculator_activity-fe-process-downscaled.R`
#' - allSHAPE_Aviation_Transportation_v3.csv
#'
#'
#' Then, store in: here("data-raw", "scenario_data", "transport_split")

original.regionallevel.iam.data.path <- here(
  get_data_location_raw(test=FALSE),
  "scenario_data",
  "original_regional"
)

original.regionallevel.iam.data.files <- dir(original.regionallevel.iam.data.path, pattern = "*.xlsx") %>%  # get file names
  # remove files that are only Excel backups or temporary files
  str_subset(pattern = "^[^~$]", negate = FALSE)

transport.correction.vars <- c(
  "Final Energy|Transportation|Domestic Aviation",
  "Final Energy|Transportation"#,

  # "Energy Service|Transportation|Passenger",
  # "Energy Service|Transportation|Passenger|Domestic Aviation"

)

original.regionallevel.iam.data.aviation <- original.regionallevel.iam.data.files %>%
  map(~ (read_excel(file.path(original.regionallevel.iam.data.path,.)) %>%
           filter(Variable%in%transport.correction.vars) %>%
           iamc_wide_to_long(upper.to.lower = F)) ) %>%
  reduce(rbind) %>%
  pivot_wider(names_from = year, values_from = value)

# add (only) missing columns 5-yearly data
potentially.missing.cols <- c(`2025` = NA_real_,
                              `2035` = NA_real_,
                              `2045` = NA_real_,
                              `2055` = NA_real_,
                              `2065` = NA_real_,
                              `2075` = NA_real_,
                              `2085` = NA_real_,
                              `2095` = NA_real_)
original.regionallevel.iam.data.aviation <- original.regionallevel.iam.data.aviation %>%
  add_column(!!!potentially.missing.cols[setdiff(names(potentially.missing.cols),
                                                 names(original.regionallevel.iam.data.aviation))])

# interpolate missing 5-yearly data
original.regionallevel.iam.data.aviation.ip <- original.regionallevel.iam.data.aviation %>%

  # simple averages to interpolate
  mutate_cond(is.na(`2025`), `2025` = (`2020`+`2030`)/2) %>%
  mutate_cond(is.na(`2035`), `2035` = (`2030`+`2040`)/2) %>%
  mutate_cond(is.na(`2045`), `2045` = (`2040`+`2050`)/2) %>%
  mutate_cond(is.na(`2055`), `2055` = (`2050`+`2060`)/2) %>%
  mutate_cond(is.na(`2065`), `2065` = (`2060`+`2070`)/2) %>%
  mutate_cond(is.na(`2075`), `2075` = (`2070`+`2080`)/2) %>%
  mutate_cond(is.na(`2085`), `2085` = (`2080`+`2090`)/2) %>%
  mutate_cond(is.na(`2095`), `2095` = (`2090`+`2100`)/2) %>%

  # reorder
  select(Model,Scenario,Region,Variable,Unit,
         `2005`,`2010`,`2015`,`2020`,
         `2025`,`2030`,`2035`,`2040`,
         `2045`,`2050`,`2055`,`2060`,
         `2065`,`2070`,`2075`,`2080`,
         `2085`,`2090`,`2095`,`2100`)


saveRDS(
  original.regionallevel.iam.data.aviation.ip,
  file = here("data-raw", "scenario_data", "transport_split",
              "allSHAPE_Aviation_Transportation_v3.RData")
)
