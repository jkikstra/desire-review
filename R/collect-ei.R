#' Script for creating static, base year files, for energy intensity of decent living standards ====
#' This script does the following:
#' - run the data as it is in the original DLE code. NOTE: the units are compatible to what is traced in the DLE object, to keep multiplications easy
#' - supplement with 'total' energy intensity for housing
#' - add construction energy
#' - add units in a combined file for reporting




# Source other code
if(!require(here)){ source(file.path("R","utils.R"))} else {source(here("R","utils.R"))}
source(here("R", "input-options.R"))
source(here("R", "DLE_integration_data_structure.R"))

# Run DLS code as done under `DLE_*.R` code ====

# set data-output location
if(!exists("TEST")){
  TEST <<- "FALSE"
}
set_data_location(test = TEST)

# set necessary DLE settings
data.path <<- DLE.INPUT.DATA.PATH # where is the input data for DLS and DLE hosted?
year.end <<- DLE.SCENARIO.YEAR.END # if we run scenarios, until when?
year.base <<- DLE.SCENARIO.YEAR.BASE # what is the base year for DLS (and DLE scenarios)?
timestep <<- 5 # if we run scenarios, what is the timestep? -> required in `generate_all_dimensions` - the construction of a DLE object (even if we do not do rollout)


# setup DLE object
run_initiatialization_input_data()
load_dimensions()
dims <<- generate_all_dimensions() # create DLE object

# first run threshold and gap analysis for housing
dims$housing$DeriveThreshold()
dims$housing$IdentifyGap()

# run energy intensity analysis
call_derive_energy_intensity_helper(dims=dims)
collect_all_ei_helper(dims=dims)

# # Save results
# write_delim(
#   EI.alldims,
#   file = here(DATA.LOCATION, "energy_intensities_raw_incomplete.csv"),
#   delim = ","
# )

#

# Operation energy intensity ====

# ADD TOTALS OF HOUSING BASED DIMENSIONS

# Add housing total energy intensity by producing a weighted average of the grp==urban and grp==rural rows
if (dims$housing$DF.tei %>% filter(grp=="total") %>% nrow()==0){ # if statement to avoid accidentally adding more than once (since the input and output df are the same)
  housing_con_ei_total <- dims$housing$DF.tei %>%
    group_by(iso, type, elec) %>%
    mutate(total.country.pop = sum(population)) %>%
    mutate(population.urb.rur.share.of.total = population/total.country.pop) %>%
    reframe(grp = "total",
            e.int = sum(e.int * population.urb.rur.share.of.total),
            population = first(total.country.pop))
    dims$housing$DF.tei <- dims$housing$DF.tei %>% # Combine original heating_con data with the new total rows
      bind_rows(housing_con_ei_total)
}

if (dims$heating_con$DF.tei %>% filter(grp=="total") %>% nrow()==0){ # if statement to avoid accidentally adding more than once (since the input and output df are the same)
  heating_con_ei_total <- dims$heating_con$DF.tei %>%
    group_by(iso, type, elec) %>%
    mutate(total.country.pop = sum(population)) %>%
    mutate(population.urb.rur.share.of.total = population/total.country.pop) %>%
    reframe(grp = "total",
            e.int = sum(e.int * population.urb.rur.share.of.total),
            e.int_unit = sum(e.int_unit * population.urb.rur.share.of.total),
            population = first(total.country.pop),
            hh_size = first(hh_size))
    dims$heating_con$DF.tei <- dims$heating_con$DF.tei %>% # Combine original heating_con data with the new total rows
      bind_rows(heating_con_ei_total)
}

if (dims$cooling_con$DF.tei %>% filter(grp=="total") %>% nrow()==0){ # if statement to avoid accidentally adding more than once (since the input and output df are the same)
  cooling_con_ei_total <- dims$cooling_con$DF.tei %>%
    group_by(iso, type, elec) %>%
    mutate(total.country.pop = sum(population)) %>%
    mutate(population.urb.rur.share.of.total = population/total.country.pop) %>%
    reframe(grp = "total",
            e.int = sum(e.int * population.urb.rur.share.of.total),
            e.int_unit = sum(e.int_unit * population.urb.rur.share.of.total),
            population = first(total.country.pop),
            hh_size = first(hh_size))
    dims$cooling_con$DF.tei <- dims$cooling_con$DF.tei %>% # Combine original heating_con data with the new total rows
      bind_rows(cooling_con_ei_total)
}

if (dims$heating_op$DF.tei %>% filter(grp=="total") %>% nrow()==0){ # if statement to avoid accidentally adding more than once (since the input and output df are the same)
  heating_op_ei_total <- dims$heating_op$DF.tei %>%
    group_by(iso, type, elec) %>%
    mutate(total.country.pop = sum(population)) %>%
    mutate(population.urb.rur.share.of.total = population/total.country.pop) %>%
    reframe(grp = "total",
            e.int = sum(e.int * population.urb.rur.share.of.total),
            population = first(total.country.pop))
  dims$heating_op$DF.tei <- dims$heating_op$DF.tei %>% # Combine original heating_con data with the new total rows
    bind_rows(heating_op_ei_total)
}

if (dims$cooling_op$DF.tei %>% filter(grp=="total") %>% nrow()==0){ # if statement to avoid accidentally adding more than once (since the input and output df are the same)
  cooling_op_ei_total <- dims$cooling_op$DF.tei %>%
    group_by(iso, type, elec) %>%
    mutate(total.country.pop = sum(population)) %>%
    mutate(population.urb.rur.share.of.total = population/total.country.pop) %>%
    reframe(grp = "total",
            e.int = sum(e.int * population.urb.rur.share.of.total),
            population = first(total.country.pop))
  dims$cooling_op$DF.tei <- dims$cooling_op$DF.tei %>% # Combine original heating_con data with the new total rows
    bind_rows(cooling_op_ei_total)
}

if (dims$hotwater_op$DF.tei %>% filter(grp=="total") %>% nrow()==0){ # if statement to avoid accidentally adding more than once (since the input and output df are the same)
  hotwater_op_ei_total <- dims$hotwater_op$DF.tei %>%
    group_by(iso, type, elec) %>%
    mutate(total.country.pop = sum(population)) %>%
    mutate(population.urb.rur.share.of.total = population/total.country.pop) %>%
    reframe(grp = "total",
            e.int = sum(e.int * population.urb.rur.share.of.total),
            population = first(total.country.pop))
  dims$hotwater_op$DF.tei <- dims$hotwater_op$DF.tei %>% # Combine original heating_con data with the new total rows
    bind_rows(hotwater_op_ei_total)
}

# update EI.alldims with totals
collect_all_ei_helper(dims=dims)

# Energy Intensity ====


# Energy Intensity (Operational) ====


# clean operation energy intensity
EI_op_cleaned <- EI.alldims %>% tibble() %>%

  rename(iso = "country.reg") %>%

  # 1. SELECT WHAT TO KEEP (part 1)
  # only operation
  filter(year == "Entire period" & type == "OP" & elec %in% c("total", "elec", "non.elec")) %>%

  # 2. USE IAMC-style COLUMN (and variable) NAMES
  # translate to IAMC-style reporting
  mutate(variable = if_else(is.na(grp), dim, paste(dim, grp, sep = "|"))) %>%
  select(-grp, -dim, -year, -type) %>%

  # 2.1 SELECT WHAT TO KEEP (part 2)
  # no need for modal split in first threshold (as long as DLE to IAM mapping is not separated by modal share; but keeping it in doesn't hurt)
  # filter(!(variable %in% c("Transport|car","Transport|bus", "Transport|rail", "Transport|twothree"))) %>%
  # # housing and road are only kept track of in CON, not OP -- but easier to just keep them in, being zero.
  # filter(!(variable %in% c("Heating CON|urban",
  #                          "Heating CON|rural",
  #                          "Cooling CON|urban",
  #                          "Cooling CON|rural",
  #                          "Heating CON|total",
  #                          "Cooling CON|total",
  #                          "Roads|roads",
  #                          "Housing|rural",
  #                          "Housing|urban",
  #                          "Housing|total"))) %>%

  # 3. ADD TRANSPORT (modal-WEIGHTED) TOTALS

  # add transport total (which was not added with the housing bit above) - check later if it can be moved back up (like in collect-dls-gaps)
  bind_rows(dims$transport$ei_data_all.with.totals %>%
              select(iso, total_ei_op, total_ei_op.elec, total_ei_op.nonelec) %>%
              pivot_longer(cols = total_ei_op:total_ei_op.nonelec, names_to = "ei.type", values_to = "e.int") %>%
              mutate(elec = NA,
                     variable = "Transport|total") %>%
              mutate_cond(ei.type=="total_ei_op", elec = "total") %>%
              mutate_cond(ei.type=="total_ei_op.elec", elec = "elec") %>%
              mutate_cond(ei.type=="total_ei_op.nonelec", elec = "non.elec") %>%
              select(-ei.type)
            ) %>%

  # 4. CLEAN UP VARIABLE NAMES
  streamline_variable_names_dls(type = "ei") %>%

  # 5. CLEAN UP UNITS
  # 5.1 go from per household (hh) to per capita
  # from_hh_to_cap_dls(type = "ei") %>% # don't double count

  # 5.2 update unit (where still incorrect)
  add_ei_units(type = "op")


write_delim(x = EI_op_cleaned,
            file = here(DATA.LOCATION, "EI_op.csv"),
            delim = ","
)

EI_op_pivot <- EI_op_cleaned %>% select(-unit) %>%
  pivot_wider(names_from = variable, values_from = e.int)

# save operation energy intensity for each dimension as in DLE object dataframe
write_delim(x = EI_op_pivot,
            file = here(DATA.LOCATION, "EI_op_pivoted_no_units.csv"),
            delim = ","
)

# Energy Intensity (CON.rep) ====
# Construction energy intensity [replacement]

# clean construction energy intensity

# get list of replacement rates [=inverse of lifetime lt] for all dimensions
r.rep.list <- list(
  `Appliance|clean_cooking_fuel` = 1 / dims$appliances$lifetime.list$lt.clean_cooking_fuel,
  `Appliance|mobile_telephone` = 1 / dims$appliances$lifetime.list$lt.mobile_telephone,
  `Appliance|refrigerator` = 1 / dims$appliances$lifetime.list$lt.refrigerator,
  `Appliance|television` = 1 / dims$appliances$lifetime.list$lt.television,

  `Clothing|clothing` = NA,
  `Clothing|footwear` = NA,

  `Cooling CON|rural` = dims$cooling_con$r.rep,
  `Cooling CON|total` = dims$cooling_con$r.rep,
  `Cooling CON|urban` = dims$cooling_con$r.rep,
  `Cooling OP|rural` = NA,
  `Cooling OP|total` = NA,
  `Cooling OP|urban` = NA,

  `Education|lower_secondary` = NA,
  `Education|primary` = NA,

  `Health care` = NA,

  `Heating CON|rural` = dims$heating_con$r.rep,
  `Heating CON|total` = dims$heating_con$r.rep,
  `Heating CON|urban` = dims$heating_con$r.rep,
  `Heating OP|rural` = NA,
  `Heating OP|total` = NA,
  `Heating OP|urban` = NA,

  `Hot Water OP|rural` = NA,
  `Hot Water OP|total` = NA,
  `Hot Water OP|urban` = NA,

  `Housing|rural` = dims$housing$r.rep,
  `Housing|total` = dims$housing$r.rep,
  `Housing|urban` = dims$housing$r.rep,

  `Nutrition` = NA,

  `Roads` = dims$roads$r.rep,

  `Sanitation` = dims$sanit$r.rep,

  `Transport` = NA,
  `Transport|bus` = dims$transport$r.rep.bus,
  `Transport|car` = dims$transport$r.rep.car,
  `Transport|rail` = dims$transport$r.rep.rail,
  `Transport|twothree` = dims$transport$r.rep.twothree,

  `Water` = dims$water$r.rep

)
r.rep.df <- tibble(variable=names(r.rep.list), r.rep=as.double(r.rep.list))


# modify energy intensity by multiplying respective replacement rate
EI_con_rep_cleaned <- EI.alldims %>% tibble() %>%

  rename(iso = "country.reg") %>%

  # 1. SELECT WHAT TO KEEP (part 1)
  # only replacement construction (at the moment, in EI.alldims, CON.new and CON.rep are generally the same energy intensity [for housing it could differ, due to newer stock vs existing stock])
  filter(year == "Entire period", type == "CON.rep", elec %in% c("total", "elec", "non.elec")) %>%

  # 2. USE IAMC-style COLUMN (and variable) NAMES
  mutate(variable = if_else(is.na(grp), dim, paste(dim, grp, sep = "|"))) %>%
  select(-grp, -dim, -year, -type) %>%

  # 2.1 SELECT WHAT TO KEEP (part 2)
  # no need for modal split in first threshold (as long as DLE to IAM mapping is not separated by modal share; but keeping it in doesn't hurt)
  # filter(!(variable %in% c("Transport|car","Transport|bus", "Transport|rail", "Transport|twothree"))) %>%
  # filter out variables with only OP -- easier/better to just keep in, as zeros.
  # filter(!(variable %in% c("Clothing|clothing",
  #                          "Clothing|footwear",
  #                          "Cooling OP|urban",
  #                          "Cooling OP|total",
  #                          "Cooling OP|rural",
  #                          "Education|lower_secondary",
  #                          "Education|primary",
  #                          "Health|health",
  #                          "Heating OP|urban",
  #                          "Heating OP|total",
  #                          "Heating OP|rural",
  #                          "Hot Water OP|urban",
  #                          "Hot Water OP|total",
  #                          "Hot Water OP|rural"
  #                          ))) %>%

  # 3. ADD TRANSPORT (modal-WEIGHTED) TOTALS
  # add transport total (which was not added with the housing bit above) - check later if it can be moved back up (like in collect-dls-gaps)
  # note that con.rep values here have already been adjusted for lifetime
  bind_rows(dims$transport$ei_data_all.with.totals %>%
              select(iso, total_ei_con.rep, total_ei_con.rep.elec, total_ei_con.rep.nonelec) %>%
              pivot_longer(cols = total_ei_con.rep:total_ei_con.rep.nonelec,
                           names_to = "ei.type", values_to = "e.int") %>%
              mutate(elec = NA,
                     variable = "Transport|total") %>%
              mutate_cond(ei.type=="total_ei_con.rep", elec = "total") %>%
              mutate_cond(ei.type=="total_ei_con.rep.elec", elec = "elec") %>%
              mutate_cond(ei.type=="total_ei_con.rep.nonelec", elec = "non.elec") %>%
              select(-ei.type)
  ) %>%

  # 4. CLEAN UP VARIABLE NAMES
  streamline_variable_names_dls(type = "ei") %>%

  # 5. APPLY REPLACEMENT RATE FOR ALL VARIABLES (where necessary -- not for transport|total, as already done [which is weighted])
  left_join(r.rep.df) %>%
  mutate(e.int.con.rep = e.int * r.rep) %>%
  mutate(e.int = ifelse(is.na(e.int.con.rep), e.int, e.int.con.rep)) %>%
  select(-r.rep, -e.int.con.rep) %>%

  # 6. CLEAN UP UNITS
  # 6.1 go from per household (hh) to per capita
  # from_hh_to_cap_dls(type = "ei") # don't double count (this fits better in for activity)

  # 6.2 add units
  add_ei_units(type = "con") %>% mutate_cond(!is.na(unit), unit = paste0(unit,"/year"))


write_delim(x = EI_con_rep_cleaned,
            file = here(DATA.LOCATION, "EI_con_rep.csv"),
            delim = ","
)

EI_con_rep_pivot <- EI_con_rep_cleaned %>% select(-unit) %>%
  pivot_wider(names_from = variable, values_from = e.int)

# save construction energy intensity for each dimension as in DLE object dataframe
write_delim(x = EI_con_rep_pivot,
            file = here(DATA.LOCATION, "EI_con_rep_pivoted_no_units.csv"),
            delim = ","
)



# Energy Intensity (CON.new) ====
# Side-product: CON.new Energy intensity
# Load and pivot energy intensity from the runs as under `collect-ei.R`
# modify energy intensity by multiplying respective replacement rate
EI_con_new_cleaned <- EI.alldims %>% tibble() %>%

  rename(iso = "country.reg") %>%

  # 1. SELECT WHAT TO KEEP (part 1)
  # only replacement construction (at the moment, in EI.alldims, CON.new and CON.rep are generally the same energy intensity [for housing it could differ, due to newer stock vs existing stock])
  filter(year == "Entire period", type == "CON.new", elec %in% c("total", "elec", "non.elec")) %>%

  # 2. USE IAMC-style COLUMN (and variable) NAMES
  mutate(variable = if_else(is.na(grp), dim, paste(dim, grp, sep = "|"))) %>%
  select(-grp, -dim, -year, -type) %>%

  # 2.1 SELECT WHAT TO KEEP (part 2)
  # no need for modal split in first threshold (as long as DLE to IAM mapping is not separated by modal share; but keeping it in doesn't hurt)
  # filter(!(variable %in% c("Transport|car","Transport|bus", "Transport|rail", "Transport|twothree"))) %>%
  # filter out variables with only OP -- easier/better to just keep in, as zeros.
  # filter(!(variable %in% c("Clothing|clothing",
  #                          "Clothing|footwear",
  #                          "Cooling OP|urban",
  #                          "Cooling OP|total",
  #                          "Cooling OP|rural",
  #                          "Education|lower_secondary",
  #                          "Education|primary",
  #                          "Health|health",
  #                          "Heating OP|urban",
  #                          "Heating OP|total",
  #                          "Heating OP|rural",
  #                          "Hot Water OP|urban",
  #                          "Hot Water OP|total",
  #                          "Hot Water OP|rural"
  #                          ))) %>%

  # 3. ADD TRANSPORT (modal-WEIGHTED) TOTALS
  # add transport total (which was not added with the housing bit above) - check later if it can be moved back up (like in collect-dls-gaps)
  # note that con.rep values here have already been adjusted for lifetime
  bind_rows(dims$transport$ei_data_all.with.totals %>%
              select(iso, total_ei_con.new, total_ei_con.new.elec, total_ei_con.new.nonelec) %>%
              pivot_longer(cols = total_ei_con.new:total_ei_con.new.nonelec,
                           names_to = "ei.type", values_to = "e.int") %>%
              mutate(elec = NA,
                     variable = "Transport|total") %>%
              mutate_cond(ei.type=="total_ei_con.new", elec = "total") %>%
              mutate_cond(ei.type=="total_ei_con.new.elec", elec = "elec") %>%
              mutate_cond(ei.type=="total_ei_con.new.nonelec", elec = "non.elec") %>%
              select(-ei.type)
  ) %>%

  # 4. CLEAN UP VARIABLE NAMES
  streamline_variable_names_dls(type = "ei") %>%

  # 5. CLEAN UP UNITS
  # 5.1 go from per household (hh) to per capita
  # from_hh_to_cap_dls(type = "ei") # don't double count (this fits better in for activity)

  # 5.2 add units
  add_ei_units(type = "con")


write_delim(x = EI_con_new_cleaned,
            file = here(DATA.LOCATION, "EI_con_new.csv"),
            delim = ","
)

EI_con_new_pivot <- EI_con_new_cleaned %>% select(-unit) %>%
  pivot_wider(names_from = variable, values_from = e.int)

# save construction energy intensity for each dimension as in DLE object dataframe
write_delim(x = EI_con_new_pivot,
            file = here(DATA.LOCATION, "EI_con_new_pivoted_no_units.csv"),
            delim = ","
)


# Save all (OP, CON.rep, and CON.new), with units and labels ====
df.combined.ei <- EI_op_cleaned %>% mutate(`Purpose of energy flow or investment` = "Operation of services (OP)") %>%
  bind_rows(
    EI_con_rep_cleaned %>% mutate(`Purpose of energy flow or investment` = "Maintenance of infrsatructure (CON.rep) of an existing stock")
  ) %>%
  bind_rows(
    EI_con_new_cleaned %>% mutate(`Purpose of energy flow or investment` = "Energy investment - extending infrsatructure (CON.new) building new stock (at the current energy intensity)") # note: could vary this for Housing, where there is the difference between new and existing stock.
  )

write_delim(x = df.combined.ei,
            file = here(DATA.LOCATION, "EI_combined_long_with_units.csv"),
            delim = ","
)




# # add one that is simplified and summarised for dle gaps and dle input?
# df.combined.ei %>% distinct(variable,`Purpose of energy flow or investment`,unit) %>% View()
# dls.products.combined %>% filter(type=="Decent Living Standard threshold") %>% distinct(variable,unit)
# dls.products.combined %>% filter(type=="Service gap") %>% distinct(variable,unit)
#
# df.combined.ei %>% filter(grepl(`Purpose of energy flow or investment`,pattern="Operation")) %>% distinct(variable,unit) %>%
#   left_join(
#     dls.products.combined %>% filter(type=="Service gap") %>% distinct(variable) %>% mutate(with.threshold="yes")
#   ) %>% View()
# missing in dls:
# - transport (modal): 4
# - heating OP: 3
# - cooling OP: 3
# --> transport: OK (replaced by total)
# --> heating and cooling: replace by correct units - i.e. the floorspace housing threshold
