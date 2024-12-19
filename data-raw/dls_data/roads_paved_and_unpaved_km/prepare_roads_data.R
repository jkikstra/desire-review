# Load and convert raw road data prepared by Alessio Mastrucci and shared with Jarmo Kikstra over Teams on 04.09.2023 at 15.42

if(!require(here)){ source(file.path("R","utils.R"))} else {source(here("R","utils.R"))}


# library(vroom)
# library(tidyverse)
# library(readxl)
# library(here)
# library(countrycode)

TEST <<- FALSE
set_data_location()
source(here("R", "input-options.R"))
out.path <<- here(DATA.LOCATION)
data.path <<- DLE.INPUT.DATA.PATH
year.end <<- DLE.SCENARIO.YEAR.END
year.base <<- DLE.SCENARIO.YEAR.BASE
run_initiatialization_input_data()


roads_cia <- vroom(
  here("data-raw", "dls_data", "roads_paved_and_unpaved_km",
       "cia_roads_exp.csv")
) %>% mutate(iso=countrycode(sourcevar=country,origin="country.name",destination="iso3c")) %>%
  filter(`country`!="Cyprus Government Control", `country`!="Cyprus Turkish Cypriot control") %>%
  left_join(message.R11.all %>% select(-country_name)) %>%
  select(iso,road_paved,length_km,R11.region) %>% drop_na() %>%
  pivot_wider(names_from = road_paved, values_from = length_km) %>%
  mutate(unit = "km (length)") %>%

  # add data if only one of [total, paved, unpaved] is missing
  mutate_cond(
    (is.na(total))&(!is.na(paved))&(!is.na(unpaved)), total=paved+unpaved,
  ) %>%
  mutate_cond(
    (!is.na(paved))&(is.na(unpaved)), unpaved=total-paved,
  ) %>%
  mutate_cond(
    (is.na(paved))&(!is.na(unpaved)), paved=total-unpaved,
  ) %>%

  # add data if total is there, but [paved, unpaved] is missing: Global North
  mutate_cond(
    (is.na(paved))&(is.na(unpaved))&(
      R11.region %in% c("EEU", "WEU", "FSU", "PAO", "NAM") | (
        iso %in% c(
          "ABW", # Aruba
          "QAT" # Qatar
          # "CHL", # added to other dataset
          # "GNQ", # only total, no paved estimate
          # "LBN", # only total, no paved estimate
          # "NCL" # only total, no paved estimate
          # "THA" # added to other dataset
        )
      )
    ), unpaved=0,
  ) %>%
  mutate_cond(
    (is.na(paved))&(unpaved==0)&(
      R11.region %in% c("EEU", "WEU", "FSU", "PAO", "NAM") | (
        iso %in% c(
          "ABW", # Aruba
          "QAT" # Qatar
          # "CHL", # added to other dataset
          # "GNQ", # only total, no paved estimate (will be filled in by DLE_roads.R code)
          # "LBN", # only total, no paved estimate (will be filled in by DLE_roads.R code)
          # "NCL" # only total, no paved estimate (will be filled in by DLE_roads.R code)
          # "THA" # added to other dataset
        )
      )
    ), paved=total,
  ) %>%

  # override Global North data, based on the assumption that it has enough paved roads and only should replace it
  mutate_cond(
    (R11.region %in% c("EEU", "WEU", "FSU", "PAO", "NAM")), total=paved,
  ) %>%
  mutate_cond(
    (R11.region %in% c("EEU", "WEU", "FSU", "PAO", "NAM")), unpaved=0,
  )



roads_other <- read_excel(
  here("data-raw", "dls_data", "roads_paved_and_unpaved_km",
       "data_road_add.xlsx")
) %>% select(iso,total,paved,unpaved) %>%
  mutate(unit = "km (length)") %>% left_join(message.R11.all %>% select(-country_name))

roads_other_iso <- roads_other %>% pull(iso) %>% unique()


# combine
roads <- roads_cia %>% mutate(source="cia") %>% filter(!(iso%in%roads_other_iso)) %>%
  bind_rows(roads_other %>% mutate(source="other"))

write_delim(
  file = here("data-raw", "dls_data", "roads_paved_and_unpaved_km",
              "data_road_processed.csv"),
  x = roads,
  delim = ","
)

# compared to previousdata
roads_previous_alessio <- vroom(
  here("data-raw", "DLE_scaleup", "Paper_data_final",
       "input", "Roads", "data_road_processed.csv")
)

roads_previous_alessio %>% nrow()
roads %>% nrow()

roads_previous_alessio %>% drop_na() %>% nrow()
roads %>% drop_na() %>% nrow()

roads_previous_alessio %>% drop_na(total) %>% nrow()
roads %>% drop_na(total) %>% nrow()

roads_compared <- roads %>% full_join(roads_previous_alessio %>% rename(
  total.alessio=total,
  unpaved.alessio=unpaved,
  paved.alessio=paved
),by="iso")
View(roads_compared)

