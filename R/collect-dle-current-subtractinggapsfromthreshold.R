#' Collect current energy used for DLE
#' (script is a combination of `collect-dle-gaps.R` and `collect-dle-threshold-input.R`)
#'
#'
#'
#'
#'
#'
#'

# Run DLE threshold and DLE gap scripts first ----------------------------------
if(!require(here)){ source(file.path("R","utils.R"))} else {source(here("R","utils.R"))}
source(here("R", "collect-dle-threshold-input.R"))
source(here("R", "collect-dle-gaps.R"))

# Load DLE threshold -----------------------------------------------------------
dle.threshold.all <- read_csv(file = here(DATA.LOCATION, "DLE_threshold_input.csv"))

dle.threshold.op_and_con <- read_csv(file = here(DATA.LOCATION, "DLE_threshold_input_opcon_elecnonelec.csv"))

dle.threshold.dimensions <- read_csv(file = here(DATA.LOCATION, "DLE_threshold_input_simplified.csv"))

dle.threshold.dimensions.op_and_con <- read_csv(file = here(DATA.LOCATION, "DLE_threshold_input_opcon_elecnonelec_dimensions.csv"))

dle.threshold.totals <- read_csv(file = here(DATA.LOCATION, "DLE_threshold_input_simplified_totals.csv"))

# Load DLE gap -----------------------------------------------------------------
dle.gaps.all <- read_csv(file = here(DATA.LOCATION, "DLE_gaps_alldata.csv"))

dle.gaps.op_and_con <- read_csv(file = here(DATA.LOCATION, "DLE_gaps_opcon_elecnonelec.csv"))

dle.gaps.dimensions <- read_csv(file = here(DATA.LOCATION, "DLE_gaps.csv"))

dle.gaps.dimensions.op_and_con <- read_csv(file = here(DATA.LOCATION, "DLE_gaps_opcon_elecnonelec_dimensions.csv"))

dle.gaps.totals <- read_csv(file = here(DATA.LOCATION, "DLE_gaps_totals.csv"))

# save out current DLE ---------------------------------------------------------

### All data -------------------------------------------------------------------
dle.current.all <- dle.threshold.all %>%
  left_join(dle.gaps.all) %>%
  mutate(current.dle = thres.energy - gap.energy,
         current.dle.op = thres.energy.op - gap.energy.op,
         current.dle.conrep = thres.energy.conrep - gap.energy.conrep) %>%
  select(iso,elec,variable,current.dle,current.dle.op,current.dle.conrep,unit.energy)

write_delim(x = dle.current.all,
            file = here(DATA.LOCATION, "DLE_current_alldata.csv"),
            delim = ",")

### Totals by country, operation and construction replacement, electricity -----
dle.current.op_and_con <- dle.threshold.op_and_con %>%
  left_join(dle.gaps.op_and_con) %>%
  mutate(current.dle.op = thres.energy.op - gap.energy.op,
         current.dle.conrep = thres.energy.conrep - gap.energy.conrep) %>%
  select(iso,elec,current.dle.op,current.dle.conrep,unit.energy)

write_delim(x = dle.current.op_and_con,
            file = here(DATA.LOCATION, "DLE_current_opcon_elecnonelec.csv"),
            delim = ",")

### Dimension by country, operation and construction replacement, electricity -----
dle.current.dimensions.op_and_con <- dle.threshold.dimensions.op_and_con %>%
  left_join(dle.gaps.dimensions.op_and_con) %>%
  mutate(current.dle.op = thres.energy.op - gap.energy.op,
         current.dle.conrep = thres.energy.conrep - gap.energy.conrep) %>%
  select(iso,elec,variable,current.dle.op,current.dle.conrep,unit.energy)

write_delim(x = dle.current.dimensions.op_and_con,
            file = here(DATA.LOCATION, "DLE_current_opcon_elecnonelec_dimensions.csv"),
            delim = ",")

### Totals by country, DLS dimension -------------------------------------------
dle.current.dimensions <- dle.threshold.dimensions %>%
  left_join(dle.gaps.dimensions) %>%
  mutate(current.dle = thres - gap) %>%
  select(iso,variable,current.dle,unit)

write_delim(x = dle.current.dimensions,
            file = here(DATA.LOCATION, "DLE_current_dimensions.csv"),
            delim = ",")

### Totals by country ----------------------------------------------------------
dle.current.totals <- dle.threshold.totals %>%
  left_join(dle.gaps.totals) %>%
  mutate(current.dle = thres - gap) %>%
  select(iso,current.dle,unit)

write_delim(x = dle.current.totals,
            file = here(DATA.LOCATION, "DLE_current_totals.csv"),
            delim = ",")
