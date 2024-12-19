# Script for creating static, base year files, for DLS deprivation headcount, DLS thresholds, DLS depth-of-deficits, and DLS gaps ====


# Source other code
if(!require(here)){ source(file.path("R","utils.R"))} else {source(here("R","utils.R"))}
if(!exists("TEST")){
  TEST <<- "FALSE"
}
set_data_location(test = TEST)
source(here("R", "input-options.R"))
source(here("R", "DLE_integration_data_structure.R"))

# Run DLS code as done under `DLE_*.R` code ====

# set data-output location


# set necessary DLE settings
data.path <<- DLE.INPUT.DATA.PATH # where is the input data for DLS and DLE hosted?
year.end <<- DLE.SCENARIO.YEAR.END # if we run scenarios, until when?
year.base <<- DLE.SCENARIO.YEAR.BASE # what is the base year for DLS (and DLE scenarios)?
timestep <<- 5 # if we run scenarios, what is the timestep? -> required in `generate_all_dimensions` - the construction of a DLE object (even if we do not do rollout)

# setup DLE object
run_initiatialization_input_data()
load_dimensions()
dims <<- generate_all_dimensions() # create DLE object

# run threshold and gap analysis
setup_threshold_and_gap_helper(dims=dims)
collect_all_gap_helper(dims=dims)

# ADD TOTALS OF HOUSING BASED DIMENSIONS

# Add housing *total* share of population by producing a weighted average of the grp==urban and grp==rural rows
# get population data
pop.base <- pop %>%
  filter(year == year.base) %>%
  select(-c(year, population, urb.rate, hh_size, n_hh)) %>% # keep pop by urb/rur
  pivot_longer(cols = c(population.urb, population.rur), names_to = "grp", values_to = "population") %>%
  mutate(grp = ifelse(grp == "population.urb", "urban", "rural"))

if (dims$housing$DF.DLS %>% filter(grp=="total") %>% nrow()==0){ # if statement to avoid accidentally adding more than once (since the input and output df are the same)
  housing_sharepop_total <- dims$housing$DF.DLS %>%
    left_join(select(pop.base, iso, grp, population), by = c("iso", "grp")) %>%
    group_by(iso) %>%
    mutate(total.country.pop = sum(population)) %>%
    mutate(population.urb.rur.share.of.total = population/total.country.pop) %>%
    reframe(grp = "total",
              deprivation.headcount = sum(deprivation.headcount * population.urb.rur.share.of.total),
              population = first(total.country.pop),
              thres = first(thres),
              gap = sum(gap * population.urb.rur.share.of.total),
              unit = first(unit),
              R11.region = first(R11.region),
              gap_perc = sum(gap_perc * population.urb.rur.share.of.total),
              gap_tot = sum(gap_tot))
  dims$housing$DF.DLS <- dims$housing$DF.DLS %>% # Combine original housing data with the new total rows
    bind_rows(housing_sharepop_total)
}
if (dims$heating_con$DF.DLS %>% filter(grp=="total") %>% nrow()==0){ # if statement to avoid accidentally adding more than once (since the input and output df are the same)
  heating_con_sharepop_total <- dims$heating_con$DF.DLS %>%
    left_join(select(pop.base, iso, grp, population), by = c("iso", "grp")) %>%
    group_by(iso) %>%
    mutate(total.country.pop = sum(population)) %>%
    mutate(population.urb.rur.share.of.total = population/total.country.pop) %>%
    reframe(grp = "total",
            deprivation.headcount = sum(deprivation.headcount * population.urb.rur.share.of.total),
            population = first(total.country.pop),
            thres = first(thres),
            gap = sum(gap * population.urb.rur.share.of.total),
            unit = first(unit))
  dims$heating_con$DF.DLS <- dims$heating_con$DF.DLS %>% # Combine original heating_con data with the new total rows
    bind_rows(heating_con_sharepop_total)
}
if (dims$hotwater_op$DF.DLS %>% filter(grp=="total") %>% nrow()==0){ # if statement to avoid accidentally adding more than once (since the input and output df are the same)
  hotwater_op_sharepop_total <- dims$hotwater_op$DF.DLS %>%
    left_join(select(pop.base, iso, grp, population), by = c("iso", "grp")) %>%
    group_by(iso) %>%
    mutate(total.country.pop = sum(population)) %>%
    mutate(population.urb.rur.share.of.total = population/total.country.pop) %>%
    reframe(grp = "total",
            deprivation.headcount = sum(deprivation.headcount * population.urb.rur.share.of.total),
            population = first(total.country.pop),
            thres = first(thres),
            gap = sum(gap * population.urb.rur.share.of.total),
            unit = first(unit))
  dims$hotwater_op$DF.DLS <- dims$hotwater_op$DF.DLS %>% # Combine original hotwater_op data with the new total rows
    bind_rows(hotwater_op_sharepop_total)
}
if (dims$cooling_con$DF.DLS %>% filter(grp=="total") %>% nrow()==0){ # if statement to avoid accidentally adding more than once (since the input and output df are the same)
  cooling_con_sharepop_total <- dims$cooling_con$DF.DLS %>%
    left_join(select(pop.base, iso, grp, population), by = c("iso", "grp")) %>%
    group_by(iso) %>%
    mutate(total.country.pop = sum(population)) %>%
    mutate(population.urb.rur.share.of.total = population/total.country.pop) %>%
    reframe(grp = "total",
            deprivation.headcount = sum(deprivation.headcount * population.urb.rur.share.of.total),
            population = first(total.country.pop),
            thres = first(thres),
            gap = sum(gap * population.urb.rur.share.of.total),
            unit = first(unit))
  dims$cooling_con$DF.DLS <- dims$cooling_con$DF.DLS %>% # Combine original cooling_con data with the new total rows
    bind_rows(cooling_con_sharepop_total)
}
# Add total transport gap (since the gap itself is not calculated on a modal level we shouldn't keep that information)
if (dims$transport$DF.DLS %>% filter(grp=="total") %>% nrow()==0){ # if statement to avoid accidentally adding more than once (since the input and output df are the same)
  transport_total <- dims$transport$DF.DLS %>%
    group_by(iso) %>%
    summarise(grp = "total",
              deprivation.headcount = first(deprivation.headcount),
              thres = sum(thres),
              gap = sum(gap),
              unit = first(unit))
  dims$transport$DF.DLS <- dims$transport$DF.DLS %>% # Combine original housing data with the new total rows
    bind_rows(transport_total)
}
# update Gap.alldims with totals
collect_all_gap_helper(dims=dims)


# DLS Gaps ====

# clean gaps
gaps_cleaned <- Gap.alldims %>% tibble() %>%


  # 1. SELECT WHAT TO KEEP
  # keep only total transport
  filter(!(dim=="Transport"&!(grp=="total"))) %>%
  # clothing has no gap analysis (so show assumption: gap is zero)
  # filter(!(dim=="Clothing")) %>%
  # heating and cooling gaps are only kept track of in CON, not OP
  filter(!(dim=="Heating OP"), !(dim=="Cooling OP")) %>%
  # # hot water gap analysis is a combination of heating and water access, not a separate analysis
  # filter(!(dim=="Hot Water OP")) %>%

  # 2. USE IAMC-style COLUMN (and variable) NAMES
  mutate(
    variable = ifelse(is.na(grp), dim, paste0(dim,"|",grp))
  ) %>%
  rename(unit=indicator) %>%

  # 3. CLEAN UP VARIABLE NAMES
  streamline_variable_names_dls(type = "service.gaps") %>%

  # 4. CLEAN UP UNIT NAMES (and do changes in values where necessary)

  # 4.1 go from per household (hh) to per capita
  from_hh_to_cap_dls(type = "service.gaps") %>%

  # 4.2 update unit (where still incorrect)
  # ... all correct ...

  select(iso,variable,gap,unit) %>%
  arrange(iso,variable)

gaps_cleaned %>% distinct(variable,unit) %>% print(n=30)

# save gaps (i.e. Threshold - current DLS achievement) for each dimension as in DLE object dataframe
write_delim(x = gaps_cleaned,
            file = here(DATA.LOCATION, "DLS_service_gaps.csv"),
            delim = ",")

gap_pivot <- gaps_cleaned %>%
  select(-unit) %>%
  pivot_wider(names_from = variable, values_from = gap)

write_delim(x = gap_pivot,
            here(DATA.LOCATION, "DLS_service_gaps_pivoted_no_units.csv"),
            delim = ",")

# DLS Thresholds ====

# create a threshold dataframe by extract all `thres` from the DLE object
Threshold <- list()

for (x in names(dims)) {
  print(x)

  df.dls <- dims[[x]]$DF.DLS # class instance of this dimension

  if ("thres" %in% names(df.dls)) { # Check if 'deprivation.headcount' column exists
    if (names(df.dls)[1] != "iso") { # When the regional resolution is for R11
      df.thres <- message.R11 %>%
        left_join(df.dls, by = "R11.region") %>%
        select(iso, thres, grp)
    } else {
      df.thres <- df.dls %>% select(iso, thres, grp)
    }
    df.thres <- df.thres %>% mutate(dim = dims[[x]]$name_dim, indicator = dims[[x]]$indicator)
    Threshold[[x]] <- df.thres
  } else {
    cat("Column 'thres' does not exist in dataframe '", x, "'.\n")
  }
}

Threshold.alldims <- do.call("rbind", Threshold)

# clean thresholds
thresholds_cleaned <- Threshold.alldims %>% tibble() %>%
  # 1. SELECT WHAT TO KEEP
  # keep only total transport
  filter(!(dim=="Transport"&!(grp=="total"))) %>%
  # clothing has no gap analysis (so show assumption: gap is zero)
  # filter(!(dim=="Clothing")) %>%
  # heating and cooling gaps are only kept track of in CON, not OP
  filter(!(dim=="Heating OP"), !(dim=="Cooling OP")) %>%
  # # hot water gap analysis is a combination of heating and water access, not a separate analysis
  # filter(!(dim=="Hot Water OP")) %>%

  # 2. USE IAMC-style COLUMN (and variable) NAMES
  mutate(
    variable = ifelse(is.na(grp), dim, paste0(dim,"|",grp))
  ) %>%
  rename(unit=indicator) %>%

  # 3. CLEAN UP VARIABLE NAMES
  streamline_variable_names_dls(type = "thresholds") %>%

  # 4. CLEAN UP UNIT NAMES (and do changes in values where necessary)

  # 4.1 go from per household (hh) to per capita
  from_hh_to_cap_dls(type = "thresholds") %>%

  # 4.2 implement hot water threshold (not [yet] done in core DLE code)
  mutate_cond(dim=="Hot Water OP",
              thres = 1) %>% # do not have the threshold for operating energy here dependent on the air conditioning heating need

  # 4.3 update unit (where still incorrect)
  # ... all correct ...

  select(iso,variable,thres,unit) %>%
  arrange(iso,variable)


write_delim(x = thresholds_cleaned,
            file = here(DATA.LOCATION, "DLS_threshold.csv"),
            delim = ",")

thres_pivot <- thresholds_cleaned %>%
  select(-unit) %>%
  # pivot to wide format
  pivot_wider(names_from = variable, values_from = thres)

write_delim(x = thres_pivot,
            file = here(DATA.LOCATION, "DLS_threshold_pivoted_no_units.csv"),
            delim = ",")






# DLS headcount: deprivation share (% pop below threshold) ====
#' - extract deprivation.headcount and expand upon this
#' - expansions:
#'   - add "total" deprivation.headcount for housing
#'   - update education headcount detail (9-yr completion rate) and primary too


# create a headcount dataframe by extract all `deprivation.headcount` from the DLE object
Share <- list()

for (x in names(dims)) {
  print(x)

  df.dls <- dims[[x]]$DF.DLS # class instance of this dimension

  if ("deprivation.headcount" %in% names(df.dls)) { # Check if 'deprivation.headcount' column exists
    if (names(df.dls)[1] != "iso") { # When the regional resolution is for R11
      df.share <- message.R11 %>%
        left_join(df.dls, by = "R11.region") %>%
        select(iso, deprivation.headcount, grp)
    } else {
      df.share <- df.dls %>% select(iso, deprivation.headcount, grp)
    }
    df.share <- df.share %>% mutate(dim = dims[[x]]$name_dim, indicator = dims[[x]]$indicator)
    Share[[x]] <- df.share
  } else {
    cat("Column 'deprivation.headcount' does not exist in dataframe '", x, "'.\n")
  }
}

Share.alldims <- do.call("rbind", Share)

headcounts_cleaned <- Share.alldims %>% tibble() %>%
  # 1. SELECT WHAT TO KEEP
  # keep only total transport
  filter(!(dim=="Transport"&!(grp=="total"))) %>%
  # heating and cooling thresholds are not yet defined
  filter(!(dim=="Heating OP"), !(dim=="Cooling OP")) %>%
  # Roads headcount is not defined
  filter(!(dim=="Roads")) %>%
  # # hot water gap analysis is the same as heating threshold (for boiling water), not a separate analysis
  # filter(!(dim=="Hot Water OP")) %>%
  # only keep Education lower secondary, not primary
  filter(!((dim=="Education")&(grp=="primary"))) %>%
  # health care not defined:
  # ... but also no headcount reported?

  # 2. USE IAMC-style COLUMN (and variable) NAMES
  mutate(
    variable = ifelse(is.na(grp), dim, paste0(dim,"|",grp))
  ) %>%
  # rename(unit = indicator) %>% # DLS unit not relevant for headcount



  # 3. CLEAN UP VARIABLE NAMES
  streamline_variable_names_dls(type = "headcounts") %>%

  # 4. CLEAN UP UNIT NAMES (and do changes in values where necessary)

  # 4.1 go from per household (hh) to per capita
  # ...not relecant for headcount

  # 4.2 update unit (where still incorrect)
  mutate(unit = "Share of total population living below their decent living standard") %>%

  select(iso,variable,unit,deprivation.headcount) %>%
  arrange(iso,variable)


write_delim(x = headcounts_cleaned,
            file = here(DATA.LOCATION, "DLS_headcount_below_threshold.csv"),
            delim = ",")

headcounts_pivot <- headcounts_cleaned %>%
  # pivot to wide format
  pivot_wider(names_from = variable, values_from = deprivation.headcount)

write_delim(x = headcounts_pivot,
            file = here(DATA.LOCATION, "DLS_headcount_below_threshold_pivoted_no_units.csv"),
            delim = ",")





# Depth-of-deficit: how much are the deprived on average below the threshold (Transport and nutrition) ====

# DoD for nutrition
dls.dod_nutri <- dims$nutrition$DF.nutri.base%>% select(iso, dod.avg.mder)

# DoD for transport
dls.dod_transport_noaffluencecutoff <- dims$transport$pkm.data.gap.base %>% group_by(iso) %>%
  summarise(dod=sum(dod))

depth.of.deficit <- dls.dod_nutri %>% rename(`Nutrition (kcal/cap/day)`=dod.avg.mder) %>%
  left_join(dls.dod_transport_noaffluencecutoff %>% rename(`Transport (pkm/cap/year)`=dod), by = "iso")

write_delim(x = depth.of.deficit,
            file = here(DATA.LOCATION, "DLS_depth_of_deficit.csv"),
            delim = ",")



# combine DLS data products (long) ====
dls.products.combined <- depth.of.deficit %>% pivot_longer(cols = -iso, names_to = c("variable","unit"), values_to = "value", names_sep = " \\(") %>% mutate(unit = substr(unit,1,nchar(unit)-1)) %>%
  mutate(type = "Depth-of-deficit", note = "The depth-of-deficit is calculated as the average deprivation among those below the Decent Living Standard threshold.") %>%
  bind_rows(
    thresholds_cleaned %>% rename(value=thres) %>% mutate(type = "Decent Living Standard threshold", note = "This normative threshold indicates one interpretation of the minimum material prerequisite for human well-being.")
  ) %>%
  bind_rows(
    headcounts_cleaned %>% rename(value=deprivation.headcount) %>% mutate(type = "Deprivation headcount", note = "The deprivation headcount is the estimated number of people below the decent living standard threshold.")
  ) %>%
  bind_rows(
    gaps_cleaned %>% rename(value=gap) %>% mutate(type = "Service gap", note = "The service gap is the additional service that would need to be provide to lift all those that are currently deprived up to the decent living standard.")
  ) %>%
  mutate(year = "2015 (estimated)",
         version = Sys.time(),
         author = "Jarmo S. Kikstra")

write_delim(
  x = dls.products.combined,
  file = here(DATA.LOCATION, "DLS_dataproducts_combined.csv"),
  delim = ","
)

# save out additional data: household size ====
write_delim(
  x = hh_size,
  file = here(DATA.LOCATION, "DLS_household_size.csv"),
  delim = ","
)

# save out additional data: MODAL SHARE ====
write_delim(
  x = dims$transport$modal.share.base %>% arrange(iso),
  file = here(DATA.LOCATION, "DLS_modal_share.csv"),
  delim = ","
)

