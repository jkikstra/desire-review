# Script for creating static, base year files for DLE thresholds ====
#' Output units: in MJ/yr/cap
#'




# Script for creating static, base year files, for DLE gap (product of DLS gaps and energy intensity) ====

if(!require(here)){ source(file.path("R","utils.R"))} else {source(here("R","utils.R"))}
source(here("R", "input-options.R"))
source(here("R", "collect-dls-gaps.R"))
source(here("R", "collect-ei.R"))


# load data:
ei.op <- read_csv(here(DATA.LOCATION, "EI_op.csv"))
ei.conrep <- read_csv(here(DATA.LOCATION, "EI_con_rep.csv"))
service.thresholds <- read_csv(here(DATA.LOCATION, "DLS_threshold.csv"))

service.thresholds.housing <- read_csv(here(DATA.LOCATION, "DLS_threshold.csv")) %>%
  filter(variable%in%c("Housing|rural",
                       "Housing|total",
                       "Housing|urban"))

# join data
df.data <- ei.op %>% rename(ei.op = e.int, unit.op = unit) %>%
  left_join(ei.conrep %>% rename(ei.conrep = e.int, unit.conrep = unit)) %>%
  left_join(service.thresholds %>% rename(unit.thres=unit))

# filter out data that we do not use
df.data.filtered <- df.data %>%
  filter(!(variable %in% c("Transport|bus",
                           "Transport|car",
                           "Transport|rail",
                           "Transport|twothree")))
df.data.filtered %>% distinct(variable, thres, unit.thres) %>% reframe(first(thres), .by = c("variable", "unit.thres")) %>% print(n=32)

# fill in missing data - heating and cooling: assume m2 threshold is the household size?? -> fixes in #183 and #184
df.data.heating.cooling.thres.floorspace <- df.data.filtered %>%
  filter(variable %in% c("Cooling CON|rural",
                         "Cooling CON|total",
                         "Cooling CON|urban",
                         "Heating CON|rural",
                         "Heating CON|total",
                         "Heating CON|urban")) %>% distinct(iso,variable,thres) %>%
  mutate_cond(variable=="Cooling CON|rural", variable = "Cooling OP|rural") %>%
  mutate_cond(variable=="Cooling CON|total", variable = "Cooling OP|total") %>%
  mutate_cond(variable=="Cooling CON|urban", variable = "Cooling OP|urban") %>%
  mutate_cond(variable=="Heating CON|rural", variable = "Heating OP|rural") %>%
  mutate_cond(variable=="Heating CON|total", variable = "Heating OP|total") %>%
  mutate_cond(variable=="Heating CON|urban", variable = "Heating OP|urban") %>% rename(thres.share = thres) %>%
  left_join(
    bind_rows(
      service.thresholds.housing %>%
        mutate_cond(variable=="Housing|rural", variable = "Cooling OP|rural") %>%
        mutate_cond(variable=="Housing|total", variable = "Cooling OP|total") %>%
        mutate_cond(variable=="Housing|urban", variable = "Cooling OP|urban"),
      service.thresholds.housing %>%
        mutate_cond(variable=="Housing|rural", variable = "Heating OP|rural") %>%
        mutate_cond(variable=="Housing|total", variable = "Heating OP|total") %>%
        mutate_cond(variable=="Housing|urban", variable = "Heating OP|urban")
    )
  ) %>%
  mutate(thres.thermal.space = thres * thres.share,
         unit.thermal.space = unit) %>%
  select(-unit,-thres.share,-thres)

df.data.filledmissingdata <- df.data.filtered %>%
  left_join(df.data.heating.cooling.thres.floorspace) %>%
  mutate_cond(variable %in% c("Cooling OP|rural",
                              "Cooling OP|total",
                              "Cooling OP|urban",
                              "Heating OP|rural",
                              "Heating OP|total",
                              "Heating OP|urban"),
              thres = thres.thermal.space) %>%
  mutate_cond(variable %in% c("Cooling OP|rural",
                              "Cooling OP|total",
                              "Cooling OP|urban",
                              "Heating OP|rural",
                              "Heating OP|total",
                              "Heating OP|urban"),
              unit.thres = unit.thermal.space) %>%
  select(-unit.thermal.space, -thres.thermal.space)

# make data consistent
units <- df.data.filledmissingdata %>% distinct(variable,unit.op,unit.conrep,unit.thres)
# units, N=32: i.e. 36 minus 4 modal shares.
units %>% print(n=32)

di <- df.data.filledmissingdata

dle.thres.data <- di %>%
  mutate(thres.energy = NA,
         thres.energy.op = NA,
         thres.energy.conrep = NA) %>%
  mutate(unit.energy = "MJ/cap/year") %>%
  # left_join(pop.baseyear) %>% mutate(pop=population) %>%
  # mutate_cond(grepl(variable,pattern="urban",fixed=T), pop=population.urb) %>%
  # mutate_cond(grepl(variable,pattern="urban",fixed=T), pop=population.rur) %>%
  # select(-population,-population.urb,-population.rur) %>%
  mutate_cond((variable%in%c("Appliance|clean_cooking_fuel",
                             "Appliance|television",
                             "Appliance|mobile_telephone",
                             "Appliance|refrigerator")&(
                               unit.op=="MJ/unit/year"
                             )&(
                               unit.conrep=="MJ/unit/year"
                             )&(
                               unit.thres%in%c("unit/cap")
                             )),
              thres.energy = ei.op*thres + ei.conrep*thres,
              thres.energy.op = ei.op*thres,
              thres.energy.conrep = ei.conrep*thres
              ) %>%
  mutate_cond((variable%in%c("Water")&(
                                unit.op=="MJ/m3"
                              )&(
                                unit.conrep=="MJ/m3/year"
                              )&(
                                unit.thres%in%c("m3/cap/year")
                              )),
              thres.energy = ei.op*thres + ei.conrep*thres,
              thres.energy.op = ei.op*thres,
              thres.energy.conrep = ei.conrep*thres) %>%
  mutate_cond((variable%in%c("Sanitation")&(
                                unit.op=="MJ/cap/year"
                              )&(
                                unit.conrep=="MJ/cap/year"
                              )&(
                                unit.thres%in%c("Share of population requiring safely managed sanitation services.")
                              )),
              thres.energy = ei.op*thres + ei.conrep*thres,
              thres.energy.op = ei.op*thres,
              thres.energy.conrep = ei.conrep*thres) %>%
  mutate_cond((variable%in%c("Nutrition")&(
                                unit.op=="MJ/kcal"
                              )&(
                                is.na(unit.conrep)
                              )&(
                                unit.thres%in%c("kcal/cap/day")
                              )),
              thres.energy = ei.op*thres*365,
              thres.energy.op = ei.op*thres*365,
              thres.energy.conrep = 0) %>%
  mutate_cond((variable%in%c("Clothing|clothing",
                             "Clothing|footwear")&(
                               unit.op=="MJ/kg"
                             )&(
                               is.na(unit.conrep)
                             )&(
                               unit.thres%in%c("kg/cap/year")
                             )),
              thres.energy = ei.op*thres,
              thres.energy.op = ei.op*thres,
              thres.energy.conrep = 0) %>%
  mutate_cond((variable%in%c("Health care",
                             "Education|primary",
                             "Education|lower_secondary")&(
                               unit.op=="MJ/$"
                             )&(
                               is.na(unit.conrep)
                             )&(
                               unit.thres%in%c("$/cap/year")
                             )),
              thres.energy = ei.op*thres,
              thres.energy.op = ei.op*thres,
              thres.energy.conrep = 0) %>%
  mutate_cond((variable%in%c("Housing|rural",
                             "Housing|urban",
                             "Housing|total")&(
                               is.na(unit.op)
                             )&(
                               unit.conrep=="MJ/m2/year"
                             )&(
                               unit.thres%in%c("m2/cap")
                             )),
              thres.energy = ei.conrep*thres,
              thres.energy.op = 0,
              thres.energy.conrep = ei.conrep*thres) %>%
  mutate_cond((variable%in%c("Cooling CON|rural",
                             "Cooling CON|total",
                             "Cooling CON|urban",
                             "Heating CON|rural",
                             "Heating CON|total",
                             "Heating CON|urban")&(
                               is.na(unit.op)
                             )&(
                               #MJ/unit/year?? or "MJ/unit/cap/year" --> heating and cooling con vs appliances: where's the household size correction?
                               unit.conrep=="MJ/unit/year"
                             )&(
                               unit.thres%in%c("Share of population requiring cooling.",
                                             "Share of population requiring heating.")
                             )),
              thres.energy = ei.conrep*thres,
              thres.energy.op = 0,
              thres.energy.conrep = ei.conrep*thres) %>%
  mutate_cond((variable%in%c("Cooling OP|rural",
                             "Cooling OP|total",
                             "Cooling OP|urban",
                             "Heating OP|rural",
                             "Heating OP|total",
                             "Heating OP|urban")&(
                               unit.op=="MJ/m2/year"
                             )&(
                               is.na(unit.conrep)
                             )&(
                               unit.thres%in%c("m2/cap")
                             )),
              thres.energy = ei.op*thres,
              thres.energy.op = ei.op*thres,
              thres.energy.conrep = 0) %>%
  mutate_cond((variable%in%c("Roads")&(
                                is.na(unit.op)
                              )&(
                                unit.conrep=="MJ/km/year"
                              )&(
                                unit.thres%in%c("km/cap")
                              )),
              thres.energy = ei.conrep*thres,
              thres.energy.op = 0,
              thres.energy.conrep = ei.conrep*thres) %>%
  mutate_cond((variable%in%c("Hot Water OP|rural",
                             "Hot Water OP|total",
                             "Hot Water OP|urban")&(
                               unit.op=="MJ/cap/year"
                             )&(
                               is.na(unit.conrep)
                             )&(
                               unit.thres%in%c("Share of population requiring hot water services.")
                             )),
              thres.energy = ei.op*thres,
              thres.energy.op = ei.op*thres,
              thres.energy.conrep = 0) %>%
  mutate_cond((variable%in%c("Transport")&(
                                unit.op=="MJ/pkm"
                              )&(
                                unit.conrep=="MJ/pkm/year" # unit?
                              )&(
                                unit.thres%in%c("pkm/cap/year")
                              )),
              thres.energy = ei.op*thres + ei.conrep*thres,
              thres.energy.op = ei.op*thres,
              thres.energy.conrep = ei.conrep*thres)


write_delim(x = dle.thres.data,
            file = here(DATA.LOCATION, "DLE_threshold_input.csv"),
            delim = ",")

dle.thres.data.op.con <- dle.thres.data %>%
  filter(!grepl(variable, pattern="urban",fixed=T),
         !grepl(variable, pattern="rural",fixed=T)) %>%
  reframe(thres.energy.op = sum(thres.energy.op),
          thres.energy.conrep = sum(thres.energy.conrep),
          .by=c("iso", "elec", "unit.energy")) %>%
  select(iso,elec,thres.energy.op,thres.energy.conrep,unit.energy)

write_delim(x = dle.thres.data.op.con,
            file = here(DATA.LOCATION, "DLE_threshold_input_opcon_elecnonelec.csv"),
            delim = ",")

dle.thres.data.simplified <- dle.thres.data %>%
  filter(elec=="total",
         !grepl(variable, pattern="urban",fixed=T),
         !grepl(variable, pattern="rural",fixed=T)) %>%
  select(iso,variable,thres.energy,unit.energy) %>%
  rename(thres=thres.energy, unit=unit.energy)

write_delim(x = dle.thres.data.simplified,
            file = here(DATA.LOCATION, "DLE_threshold_input_simplified.csv"),
            delim = ",")

dle.thres.data.simplified.totals <- dle.thres.data.simplified %>%
  reframe(thres = sum(thres), .by=c("iso", "unit"))

write_delim(x = dle.thres.data.simplified.totals,
            file = here(DATA.LOCATION, "DLE_threshold_input_simplified_totals.csv"),
            delim = ",")

### For DLS Materials - by dimension, clean up heating and cooling OP/CON splits -----
##### Threshold:
dle.threshold.dimensions.op_and_con.prep <- dle.thres.data %>%
  filter(!grepl(variable, pattern="urban",fixed=T),
         !grepl(variable, pattern="rural",fixed=T)) %>%
  reframe(thres.energy.op = sum(thres.energy.op),
          thres.energy.conrep = sum(thres.energy.conrep),
          .by=c("iso", "variable", "elec", "unit.energy")) %>%
  select(iso,
         variable,
         elec,
         thres.energy.op,thres.energy.conrep,
         unit.energy)

dle.threshold.cleanopconthermal.forJanStreeck <- dle.threshold.dimensions.op_and_con.prep %>%
  filter(grepl(x=variable, pattern = "Heating") | grepl(x=variable, pattern = "Cooling")) %>%
  mutate_cond(variable=="Heating CON|total", variable = "Heating") %>%
  mutate_cond(variable=="Heating OP|total", variable = "Heating") %>%
  mutate_cond(variable=="Cooling CON|total", variable = "Cooling") %>%
  mutate_cond(variable=="Cooling OP|total", variable = "Cooling") %>%
  reframe(thres.energy.op = sum(thres.energy.op),
          thres.energy.conrep = sum(thres.energy.conrep),
          .by=c("iso", "variable", "elec", "unit.energy"))

dle.threshold.dimensions.op_and_con <- dle.threshold.dimensions.op_and_con.prep %>%
  filter(!grepl(variable, pattern="Heating",fixed=T),
         !grepl(variable, pattern="Cooling",fixed=T)) %>%
  bind_rows(dle.threshold.cleanopconthermal.forJanStreeck) %>%
  mutate(thres.energy = thres.energy.op + thres.energy.conrep) %>%
  select(iso,variable,elec,thres.energy,thres.energy.op,thres.energy.conrep,unit.energy)

write_delim(x = dle.threshold.dimensions.op_and_con,
            file = here(DATA.LOCATION, "DLE_threshold_input_opcon_elecnonelec_dimensions.csv"),
            delim = ",")



