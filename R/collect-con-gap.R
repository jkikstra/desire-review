#' Script for creating static, base year files, for total construction energy required for achieving full DLS ====
#' Collect all new infrastructure construction gaps.
#' - NOTE: transport is summed (using weighted energy intensities) over all modes of transport.
#'

# Source other code
if(!require(here)){ source(file.path("R","utils.R"))} else {source(here("R","utils.R"))}
# source(here("R", "input-options.R")) # will already be run in `collect-dls-gaps.R` (and again in `collect-ei.R`)
# source(here("R", "DLE_integration_data_structure.R")) # will already be run in `collect-dls-gaps.R` (and again in `collect-ei.R`)
source(here("R", "collect-dls-gaps.R"))
source(here("R", "collect-ei.R"))

elec.types <- c("total", "non-elec", "elec")

EI_con_new <- read_csv(here(DATA.LOCATION, "EI_con_new.csv"))
gap <- read_csv(here(DATA.LOCATION, "DLS_service_gaps.csv"))

# Construction energy intensity (MJ/m2): multiply EI * DLS gap ====
DLE_con_gap_per_capita <- EI_con_new %>% rename(unit.con_new_ei=unit) %>%
  left_join(gap %>% rename(unit.gap=unit), by = c("iso", "variable")) %>%
  mutate(con_new = e.int * gap) %>%
  # filter(is.na(con_new)) %>% distinct(variable) %>%
  # drop_na(con_new) %>%  # what would fall out is: Transport (modal shares; only keep total) and Heating OP and Cooling OP
  mutate(unit = "MJ/cap")

DLE_con_gap_total <- DLE_con_gap_per_capita %>%
  left_join(pop %>% filter(year==year.base) %>% select(iso, population)) %>%
  mutate(con_new = con_new * population * mega / exa,
         unit = "EJ")

# check the total, note simply summing over all variables
# DLE_con_gap_total %>% filter(variable %nin% c(
#   "Housing|rural",
#   "Housing|urban",
#
#   "Heating CON|rural",
#   "Heating CON|urban",
#
#   "Cooling CON|rural",
#   "Cooling CON|urban",
#
#   "Hot Water OP|rural",
#   "Hot Water OP|urban"
# ),
# elec == "total"
# ) %>% summarise(sum(con_new))

write_delim(x = DLE_con_gap_per_capita,
            file = here(DATA.LOCATION, "DLE_con_gap_per_capita.csv"),
            delim = ",")
write_delim(x = DLE_con_gap_total,
            file = here(DATA.LOCATION, "DLE_con_gap_total.csv"),
            delim = ",")

