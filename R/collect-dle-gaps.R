# Script for creating static, base year files, for DLE gap (product of DLS gaps and energy intensity) ====


if(!require(here)){ source(file.path("R","utils.R"))} else {source(here("R","utils.R"))}
source(here("R", "input-options.R"))
source(here("R", "collect-dls-gaps.R"))
source(here("R", "collect-ei.R"))


# load data:
ei.op <- read_csv(here(DATA.LOCATION, "EI_op.csv"))
ei.conrep <- read_csv(here(DATA.LOCATION, "EI_con_rep.csv"))
service.gaps <- read_csv(here(DATA.LOCATION, "DLS_service_gaps.csv"))

service.thresholds.housing <- read_csv(here(DATA.LOCATION, "DLS_threshold.csv")) %>%
  filter(variable%in%c("Housing|rural",
                       "Housing|total",
                       "Housing|urban"))

# join data
df.data <- ei.op %>% rename(ei.op = e.int, unit.op = unit) %>%
  left_join(ei.conrep %>% rename(ei.conrep = e.int, unit.conrep = unit)) %>%
  left_join(service.gaps %>% rename(unit.gap=unit))

# filter out data that we do not use
df.data.filtered <- df.data %>%
  filter(!(variable %in% c("Transport|bus",
                           "Transport|car",
                           "Transport|rail",
                           "Transport|twothree")))

# fill in missing data - heating and cooling: assume m2 gap is the construction gap * household size
df.data.heating.cooling.gaps.floorspace <- df.data.filtered %>%
  filter(variable %in% c("Cooling CON|rural",
                         "Cooling CON|total",
                         "Cooling CON|urban",
                         "Heating CON|rural",
                         "Heating CON|total",
                         "Heating CON|urban")) %>% distinct(iso,variable,gap) %>%
  mutate_cond(variable=="Cooling CON|rural", variable = "Cooling OP|rural") %>%
  mutate_cond(variable=="Cooling CON|total", variable = "Cooling OP|total") %>%
  mutate_cond(variable=="Cooling CON|urban", variable = "Cooling OP|urban") %>%
  mutate_cond(variable=="Heating CON|rural", variable = "Heating OP|rural") %>%
  mutate_cond(variable=="Heating CON|total", variable = "Heating OP|total") %>%
  mutate_cond(variable=="Heating CON|urban", variable = "Heating OP|urban") %>%
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
  mutate(gap.thermal.space = gap*thres,
         unit.thermal.space = unit) %>%
  select(-unit,-thres,-gap)

df.data.filledmissingdata <- df.data.filtered %>%
  left_join(df.data.heating.cooling.gaps.floorspace) %>%
  mutate_cond(variable %in% c("Cooling OP|rural",
                              "Cooling OP|total",
                              "Cooling OP|urban",
                              "Heating OP|rural",
                              "Heating OP|total",
                              "Heating OP|urban"),
              gap = gap.thermal.space) %>%
  mutate_cond(variable %in% c("Cooling OP|rural",
                              "Cooling OP|total",
                              "Cooling OP|urban",
                              "Heating OP|rural",
                              "Heating OP|total",
                              "Heating OP|urban"),
              unit.gap = unit.thermal.space) %>%
  select(-unit.thermal.space, -gap.thermal.space)

# make data consistent
units <- df.data.filledmissingdata %>% distinct(variable,unit.op,unit.conrep,unit.gap)
# units, N=32: i.e. 36 minus 4 modal shares.
units %>% print(n=32)

di <- df.data.filledmissingdata
# calculate energy gaps
multiply_ei_activity_gap <- function(df.dle.gap.data){

}

dle.gap.data <- di %>%
  mutate(gap.energy = NA) %>% mutate(unit.energy = "MJ/cap/year") %>%
  mutate(gap.energy.op = NA) %>%
  mutate(gap.energy.conrep = NA) %>%
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
                                   unit.gap%in%c("unit/cap")
                                   )),
              gap.energy = ei.op*gap + ei.conrep*gap,
              gap.energy.op = ei.op*gap,
              gap.energy.conrep = ei.conrep*gap) %>%
  mutate_cond((variable%in%c("Water")&(
                               unit.op=="MJ/m3"
                             )&(
                               unit.conrep=="MJ/m3/year"
                             )&(
                               unit.gap%in%c("m3/cap/year")
                             )),
              gap.energy = ei.op*gap + ei.conrep*gap,
              gap.energy.op = ei.op*gap,
              gap.energy.conrep = ei.conrep*gap) %>%
  mutate_cond((variable%in%c("Sanitation")&(
                               unit.op=="MJ/cap/year"
                             )&(
                               unit.conrep=="MJ/cap/year"
                             )&(
                               unit.gap%in%c("Share of population requiring safely managed sanitation services.")
                             )),
              gap.energy = ei.op*gap + ei.conrep*gap,
              gap.energy.op = ei.op*gap,
              gap.energy.conrep = ei.conrep*gap) %>%
  mutate_cond((variable%in%c("Nutrition")&(
                               unit.op=="MJ/kcal"
                             )&(
                               is.na(unit.conrep)
                             )&(
                               unit.gap%in%c("kcal/cap/day")
                             )),
              gap.energy = ei.op*gap*365,
              gap.energy.op = ei.op*gap*365,
              gap.energy.conrep = 0) %>%
  mutate_cond((variable%in%c("Clothing|clothing",
                             "Clothing|footwear")&(
                               unit.op=="MJ/kg"
                             )&(
                               is.na(unit.conrep)
                             )&(
                               unit.gap%in%c("kg/cap/year")
                             )),
              gap.energy = ei.op*gap,
              gap.energy.op = ei.op*gap,
              gap.energy.conrep = 0) %>%
  mutate_cond((variable%in%c("Health care",
                             "Education|primary",
                             "Education|lower_secondary")&(
                               unit.op=="MJ/$"
                             )&(
                               is.na(unit.conrep)
                             )&(
                               unit.gap%in%c("$/cap/year")
                             )),
              gap.energy = ei.op*gap,
              gap.energy.op = ei.op*gap,
              gap.energy.conrep = 0) %>%
  mutate_cond((variable%in%c("Housing|rural",
                             "Housing|urban",
                             "Housing|total")&(
                               is.na(unit.op)
                             )&(
                               unit.conrep=="MJ/m2/year"
                             )&(
                               unit.gap%in%c("m2/cap")
                             )),
              gap.energy = ei.conrep*gap,
              gap.energy.op = 0,
              gap.energy.conrep = ei.conrep*gap) %>%
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
                               unit.gap%in%c("Share of population requiring cooling.",
                                             "Share of population requiring heating.")
                             )),
              gap.energy = ei.conrep*gap,
              gap.energy.op = 0,
              gap.energy.conrep = ei.conrep*gap) %>%
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
                               unit.gap%in%c("m2/cap")
                             )),
              gap.energy = ei.op*gap,
              gap.energy.op = ei.op*gap,
              gap.energy.conrep = 0) %>%
  mutate_cond((variable%in%c("Roads")&(
                               is.na(unit.op)
                             )&(
                               unit.conrep=="MJ/km/year"
                             )&(
                               unit.gap%in%c("km/cap")
                             )),
              gap.energy = ei.conrep*gap,
              gap.energy.op = 0,
              gap.energy.conrep = ei.conrep*gap) %>%
  mutate_cond((variable%in%c("Hot Water OP|rural",
                             "Hot Water OP|total",
                             "Hot Water OP|urban")&(
                               unit.op=="MJ/cap/year"
                             )&(
                               is.na(unit.conrep)
                             )&(
                               unit.gap%in%c("Share of population requiring hot water services.")
                             )),
              gap.energy = ei.op*gap,
              gap.energy.op = ei.op*gap,
              gap.energy.conrep = 0) %>% #gap) %>%
  mutate_cond((variable%in%c("Transport")&(
                               unit.op=="MJ/pkm"
                             )&(
                               unit.conrep=="MJ/pkm/year" # unit?
                             )&(
                               unit.gap%in%c("pkm/cap/year")
                             )),
              gap.energy = ei.op*gap + ei.conrep*gap,
              gap.energy.op = ei.op*gap,
              gap.energy.conrep = ei.conrep*gap)
# dle.gap.data %>% filter(is.na(gap.energy))

write_delim(x = dle.gap.data,
            file = here(DATA.LOCATION, "DLE_gaps_alldata.csv"),
            delim = ",")

dle.gap.data.op.con <- dle.gap.data %>%
  filter(!grepl(variable, pattern="urban",fixed=T),
         !grepl(variable, pattern="rural",fixed=T)) %>%
  reframe(gap.energy.op = sum(gap.energy.op),
          gap.energy.conrep = sum(gap.energy.conrep),
          .by=c("iso", "elec", "unit.energy")) %>%
  select(iso,elec,gap.energy.op,gap.energy.conrep,unit.energy)

write_delim(x = dle.gap.data.op.con,
            file = here(DATA.LOCATION, "DLE_gaps_opcon_elecnonelec.csv"),
            delim = ",")

dle.gap.data.simplified <- dle.gap.data %>%
  filter(elec=="total", !grepl(variable, pattern="urban",fixed=T), !grepl(variable, pattern="rural",fixed=T)) %>%
  select(iso,variable,gap.energy,unit.energy) %>%
  rename(gap=gap.energy, unit=unit.energy)

write_delim(x = dle.gap.data.simplified,
            file = here(DATA.LOCATION, "DLE_gaps.csv"),
            delim = ",")

dle.gap.data.simplified.totals <- dle.gap.data.simplified %>%
  reframe(gap = sum(gap), .by=c("iso", "unit"))

write_delim(x = dle.gap.data.simplified.totals,
            file = here(DATA.LOCATION, "DLE_gaps_totals.csv"),
            delim = ",")


### For DLS Materials - by dimension, clean up heating and cooling OP/CON splits -----
##### Gaps:
dle.gap.dimensions.op_and_con.prep <- dle.gap.data %>%
  filter(!grepl(variable, pattern="urban",fixed=T),
         !grepl(variable, pattern="rural",fixed=T)) %>%
  reframe(gap.energy.op = sum(gap.energy.op),
          gap.energy.conrep = sum(gap.energy.conrep),
          .by=c("iso", "variable", "elec", "unit.energy")) %>%
  select(iso,
         variable,
         elec,
         gap.energy.op,gap.energy.conrep,
         unit.energy)

dle.gap.cleanopconthermal.forJanStreeck <- dle.gap.dimensions.op_and_con.prep %>%
  filter(grepl(x=variable, pattern = "Heating") | grepl(x=variable, pattern = "Cooling")) %>%
  mutate_cond(variable=="Heating CON|total", variable = "Heating") %>%
  mutate_cond(variable=="Heating OP|total", variable = "Heating") %>%
  mutate_cond(variable=="Cooling CON|total", variable = "Cooling") %>%
  mutate_cond(variable=="Cooling OP|total", variable = "Cooling") %>%
  reframe(gap.energy.op = sum(gap.energy.op),
          gap.energy.conrep = sum(gap.energy.conrep),
          .by=c("iso", "variable", "elec", "unit.energy"))

dle.gap.dimensions.op_and_con <- dle.gap.dimensions.op_and_con.prep %>%
  filter(!grepl(variable, pattern="Heating",fixed=T),
         !grepl(variable, pattern="Cooling",fixed=T)) %>%
  bind_rows(dle.gap.cleanopconthermal.forJanStreeck) %>%
  mutate(gap.energy = gap.energy.op + gap.energy.conrep) %>%
  select(iso,variable,elec,gap.energy,gap.energy.op,gap.energy.conrep,unit.energy)

write_delim(x = dle.gap.dimensions.op_and_con,
            file = here(DATA.LOCATION, "DLE_gaps_opcon_elecnonelec_dimensions.csv"),
            delim = ",")
