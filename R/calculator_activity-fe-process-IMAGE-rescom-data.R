# Create two files to do IMAGE-based residential and commercial corrections
#' - IMAGE_allSHAPE_ResidentialCommercialSplit_v3.csv
#' - IMAGE_allSHAPE_TraditionalBiomass_Residential_v3.csv
#'
#' Requires:
#' - selecting the right variables
#' - interpolating 2055, 2065, 2075, 2085, 2095
#'
#' Then, store in: here("data-raw", "scenario_data", "mapping_rescom_splits")

# Load IMAGE regional data -----------------------------------------------------
IMAGE.rescom.data <-
  read_excel(
    here("data-raw", "scenario_data", "original_regional",
         "1702306278226-Consolidated_IMAGE_SHAPE_results.xlsx")
  ) %>%
  filter(
    Variable %in% c(
      "Final Energy|Residential and Commercial",
      "Final Energy|Residential",
      "Final Energy|Residential|Solids|Biomass|Traditional"
    )
  )

# Interpolate 5-yearly data ----------------------------------------------------
# IMAGE.rescom.data %>% colnames()
# missing.5years <- c(2055, 2065, 2075, 2085, 2095)

IMAGE.rescom.data.ip <- IMAGE.rescom.data %>%
  # simple averages to interpolate
  mutate(
    `2055` = (`2050`+`2060`)/2,
    `2065` = (`2060`+`2070`)/2,
    `2075` = (`2070`+`2080`)/2,
    `2085` = (`2080`+`2090`)/2,
    `2095` = (`2090`+`2100`)/2
  ) %>%
  # reorder
  select(Model,Scenario,Region,Variable,Unit,
         `2005`,`2010`,`2015`,`2020`,
         `2025`,`2030`,`2035`,`2040`,
         `2045`,`2050`,`2055`,`2060`,
         `2065`,`2070`,`2075`,`2080`,
         `2085`,`2090`,`2095`,`2100`)

# Commercial final energy as shared of residential and commercial --------------
# We are not using the downscaled data because:
# - if available, we overwrite
# - the downscaling data itself may introduce errors, so we use regional correction factors, applied on national downscaled data


write_delim(
  x = IMAGE.rescom.data.ip %>% filter(Variable %in% c(
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential"
  )),
  file = here("data-raw", "scenario_data", "mapping_rescom_splits",
              "IMAGE_allSHAPE_ResidentialCommercialSplit_v3.csv"),
  delim = ","
)


# Load IMAGE regional data (for residential biomass) ---------------------------

write_delim(
  x = IMAGE.rescom.data.ip %>% filter(Variable %in% c(
    "Final Energy|Residential",
    "Final Energy|Residential|Solids|Biomass|Traditional"
  )),
  file = here("data-raw", "scenario_data", "mapping_rescom_splits",
              "IMAGE_allSHAPE_TraditionalBiomass_Residential_v3.csv"),
  delim = ","
)


#' Useful energy (missing from scenario explorer data - ad-hoc fix) ------------
#' - Consolidated_IMAGE_SHAPE_DLS_results.xlsx (useful energy data)
#'

# raw.ue.buildings.data.image <- read_excel(
#   here("data-raw", "scenario_data", "original_regional", "IMAGE-original-files", "Consolidated_IMAGE_SHAPE_DLS_results.xlsx"),
#   sheet = "data"
# ) %>%
#   iamc_wide_to_long()
# raw.ue.buildings.data.image %>% Variable_unique()
#
# ### Make summation variables ---------------------------------------------------
# ue.buildings.image <- raw.ue.buildings.data.image %>%
#   reframe(
#     value = sum(value),
#     .by = c("Model", "Scenario", "Region", "Unit", "year")
#   ) %>%
#   mutate(Variable = "Useful Energy|Residential and Commercial")
#
# ue.buildings.commercial.image <- raw.ue.buildings.data.image %>%
#   filter(Variable%in%c(
#     "Useful Energy|Commercial|Cooling",
#     "Useful Energy|Commercial|Heating"
#   )) %>%
#   reframe(
#     value = sum(value),
#     .by = c("Model", "Scenario", "Region", "Unit", "year")
#   ) %>%
#   mutate(Variable = "Useful Energy|Commercial")
#
# ue.buildings.residential.image <- raw.ue.buildings.data.image %>%
#   filter(Variable%in%c(
#     "Useful Energy|Residential|Cooking",
#     "Useful Energy|Residential|Cooling",
#     "Useful Energy|Residential|Heating",
#     "Useful Energy|Residential|Lighting",
#     "Useful Energy|Residential|Space Heating",
#     "Useful Energy|Residential|Water Heating"
#   )) %>%
#   reframe(
#     value = sum(value),
#     .by = c("Model", "Scenario", "Region", "Unit", "year")
#   ) %>%
#   mutate(Variable = "Useful Energy|Residential")
#
# ### Add Useful Energy data to new IMAGE scenario file --------------------------
# all.other.image.data <- read_excel(
#   here("data-raw", "scenario_data", "original_regional", "IMAGE-original-files",
#        "1702306278226-Consolidated_IMAGE_SHAPE_results.xlsx"),
#   sheet = "data"
# )
#
# image.data.with.ue.buildings <- all.other.image.data %>%
#   bind_rows(ue.buildings.image %>% iamc_long_to_wide()) %>%
#   bind_rows(ue.buildings.commercial.image %>% iamc_long_to_wide()) %>%
#   bind_rows(ue.buildings.residential.image %>% iamc_long_to_wide())
#
#
# ### Write out IMAGE file (with Useful Energy Buildings) to the raw-data folder ----
# library(writexl)
# write_xlsx(x = image.data.with.ue.buildings,
#            path = here("data-raw",
#                        "scenario_data",
#                        "original_regional",
#                        "1702306278226-Consolidated_IMAGE_SHAPE_results.xlsx")
# )
#
# # quick visual analysis
# ggplot(mapping=aes(x=year,y=value, colour=Variable)) +
#   facet_grid(Scenario~Region) +
#   geom_line(data=ue.buildings.residential.image %>%
#               filter(Scenario%in%shape.core.scenarios,
#                      Region!="World")) +
#   geom_line(data=ue.buildings.commercial.image %>%
#               filter(Scenario%in%shape.core.scenarios,
#                      Region!="World")) +
#   theme_classic() +
#   theme_hc()
#
# ggplot(mapping=aes(x=year,y=value, colour=Variable)) +
#   facet_grid(Scenario~Region) +
#   geom_line(data=all.other.image.data %>% filter(Variable%in%c("Final Energy|Residential", "Final Energy|Commercial")) %>% iamc_wide_to_long() %>%
#               filter(Scenario%in%shape.core.scenarios,
#                      Region!="World")) +
#   theme_classic() +
#   theme_hc()
#
# image.data.with.ue.buildings %>% filter(Variable%in%c("Final Energy|Residential and Commercial", "Useful Energy|Residential and Commercial")) %>%
#   iamc_wide_to_long() %>%
#   pivot_wider(names_from = Variable, values_from = value) %>%
#   mutate(value = `Final Energy|Residential and Commercial`/`Useful Energy|Residential and Commercial`)  %>%
#   mutate(Variable = "FE-to-UE") %>%
#   filter(Scenario%in%shape.core.scenarios) %>%
#   upper_to_lower() %>%
#   normalise_iamc_long(starting.year = 2020) %>%
#   lower_to_upper() %>%
#   rename(fe.to.ue=value) %>%
#   ggplot(data = .,
#     mapping=aes(x=year,y=fe.to.ue)) +
#   facet_grid(Scenario~Region) +
#   geom_line() +
#   theme_classic() +
#   theme_hc() +
#   scale_x_continuous(breaks = c(2020,2050,2100)) +
#   labs(title = "`Final Energy|Residential and Commercial`/`Useful Energy|Residential and Commercial`",
#        subtitle = "Normalised to 2020") +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )

