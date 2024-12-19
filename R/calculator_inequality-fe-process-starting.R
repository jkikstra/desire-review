# Script for preparing input energy inequality data for DLE-emulator/assessor


if (exists("ENERGY.DISTRIBUTION.FILE")) {
  f.fe <- here("data-raw", ENERGY.DISTRIBUTION.FILE)

  if (ENERGY.DISTRIBUTION.FILE == "data_to_global_redistribution_of_income_and_household_energy_footprints.xlsx") {
    # based on GTAP-9, so 2011 data
    # link to data download: https://github.com/eeyouol/global_redistribution_income_energy_footprints/blob/main/data_to_global_redistribution_of_income_and_household_energy_footprints.xlsx
    # as described in: https://www.nature.com/articles/s41560-020-0579-8

    # for a categorical splitting/mapping, we follow IEA:
    # https://www.iea.org/sankey/#?c=World&s=Final%20consumption
    #   where:
    #     - industry -> industry
    #     - non-energy-use -> industry, feedstock (except the small bit for transport)
    #     - transport -> Transport
    #     - other -> Residential and Commercial

    fe <- read_excel(f.fe, sheet = ENERGY.DISTRIBUTION.FILE.SHEET)
    fe[is.na(fe)] <- 0

    # for a few countries (5; BEN, GIN, NIC, SEN, TGO), population in the highest income group is 0, with 0 energy footrpint, so we delete those data rows
    fe <- fe %>% filter(population > 0)

    # all what is in the data.
    fe.total <- fe %>%
      mutate(
        total =
          `Food` +
            `Alcohol and Tobacco` +
            `Wearables` +
            `Other housing` +
            `Heating and Electricity` +
            `Household Appliances and Services` +
            `Health` +
            `Vehicle Purchase` +
            `Vehicle Fuel and Maintenance` +
            `Other transport` +
            `Communication` +
            `Recreational items` +
            `Package Holiday` +
            `Education & Finance & Other Luxury`
      ) %>%
      mutate(total = total / 1000) %>%
      mutate(unit = "GJ/cap") %>%
      rename(iso = `country Code`) %>%
      select(iso, `income group`, total, population)

    # Residential and commercial (direct) consumption categories
    fe.rescom <- fe %>%
      mutate(
        rescom =
        # `Food` +
        # N.B. we split this over rescom and Industry, using a global factor (can make this split more detailed by doing regional- or country-level split)
        #   using https://www.iea.org/sankey/#?c=World&s=Final%20consumption, where [in 2011](in 2019):
        #   food and tobacco (under industry): [181](185) Mtoe
        #   Agriculture and Forestry (under other): [189](212) Mtoe
        #   Fisheries (under other): [8](7) Mtoe
          `Food` * (189 + 8) / (181 + 189 + 8) +
            `Heating and Electricity`
      ) %>%
      mutate(rescom = rescom / 1000) %>%
      mutate(unit = "GJ/cap") %>%
      rename(iso = `country Code`) %>%
      select(iso, `income group`, rescom, population)

    # Transportation direct consumption categories
    fe.transport <- fe %>%
      mutate(
        transport =
          `Vehicle Fuel and Maintenance`
      ) %>%
      mutate(transport = transport / 1000) %>%
      mutate(unit = "GJ/cap") %>%
      rename(iso = `country Code`) %>%
      select(iso, `income group`, transport, population) %>%
      filter(
        !(population != 0 & transport==0) # remove transport in Benin, and high income group in Belarus (for transport)
      )


    # Industry, indirect energy flow consumption categories
    # -- take all indirect energy estimates from Oswald 2020/2021, with a split for food

    fe.industry <- fe %>%
      mutate(
        industry =
        # N.B. we split this over rescom and Industry, using a global factor (can make this split more detailed by doing regional- or country-level split)
        #   using https://www.iea.org/sankey/#?c=World&s=Final%20consumption, where [in 2011](in 2019):
        #   food and tobacco (under industry): [181](185) Mtoe
        #   Agriculture and Forestry (under other): [189](212) Mtoe
        #   Fisheries (under other): [8](7) Mtoe
          `Food` * (181) / (181 + 189 + 8) +
            `Alcohol and Tobacco` +
            `Wearables` +
            `Other housing` +
            `Household Appliances and Services` +
            `Health` +
            `Vehicle Purchase` +
            `Other transport` +
            `Communication` +
            `Recreational items` +
            # `Package Holiday` +
            `Education & Finance & Other Luxury` +
            `Package Holiday` # is under "indirect" in Oswald et al. 2020,
        # they use sectors 48 (Transport not elsewhere classified; Other Transport: road, rail ; pipelines, auxiliary transport activities; travel agencies) and 50 (Air transport) from GTAP-9 for this.
        # In the text, they write the following sentence: "Package holidays, for instance, comprise all sorts of transport services, including flights, and thus exhibits large energy intensities and large variation."
      ) %>%
      mutate(industry = industry / 1000) %>%
      mutate(unit = "GJ/cap") %>%
      rename(iso = `country Code`) %>%
      select(iso, `income group`, industry, population)

    # population for calculating national weighted average per capita numbers
    population.fe <- fe.total %>%
      group_by(iso) %>%
      summarise(total.pop = sum(population)) %>%
      select(iso, total.pop)

    # TOTAL consumption
    fe.total.by.income <- fe.total %>%
      rename(quantile = `income group`, fe = total)
    fe.total.avg <- fe.total.by.income %>%
      left_join(population.fe, by = "iso") %>%
      group_by(iso) %>%
      summarise(fe = sum(fe * population / total.pop))
    fe.total.gini <- get_gini(fe.total.by.income)

    # RESCOM
    fe.rescom.by.income <- fe.rescom %>%
      rename(quantile = `income group`, fe.rescom = rescom)
    fe.rescom.avg <- fe.rescom.by.income %>%
      left_join(population.fe, by = "iso") %>%
      group_by(iso) %>%
      summarise(fe.rescom = sum(fe.rescom * population / total.pop))
    fe.rescom.gini <- get_gini(fe.rescom.by.income, var = "fe.rescom")

    # TRANSPORT
    fe.transport.by.income <- fe.transport %>%
      rename(quantile = `income group`, fe.transport = transport) %>%
      group_by(iso) %>%
      filter(!all(is.na(fe.transport))) %>%
      ungroup()
    fe.transport.avg <- fe.transport.by.income %>%
      left_join(population.fe, by = "iso") %>%
      group_by(iso) %>%
      summarise(fe.transport = sum(fe.transport * population / total.pop))
    fe.transport.gini <- get_gini(fe.transport.by.income, var = "fe.transport")

    # INDUSTRY
    fe.industry.by.income <- fe.industry %>%
      rename(quantile = `income group`, fe.industry = industry)
    fe.industry.avg <- fe.industry.by.income %>%
      left_join(population.fe, by = "iso") %>%
      group_by(iso) %>%
      summarise(fe.industry = sum(fe.industry * population / total.pop))
    fe.industry.gini <- get_gini(fe.industry.by.income, var = "fe.industry")


    # COMBINE
    fe.by.income <- fe.total.by.income %>%
      left_join(fe.rescom.by.income, by = c("iso", "quantile", "population")) %>%
      left_join(fe.transport.by.income, by = c("iso", "quantile", "population")) %>%
      left_join(fe.industry.by.income, by = c("iso", "quantile", "population"))

    fe.ginis <- fe.total.gini %>%
      rename(fe.gini.total = gini) %>%
      left_join(fe.rescom.gini %>% rename(fe.gini.rescom = gini), by = "iso") %>%
      left_join(fe.transport.gini %>% rename(fe.gini.transport = gini), by = "iso") %>%
      left_join(fe.industry.gini %>% rename(fe.gini.industry = gini), by = "iso") %>%
      drop_na()

    # assume that gini of industry follows household total gini
    fe.ginis <- fe.ginis %>%
      pivot_longer(
        cols = fe.gini.total:fe.gini.industry,
        names_to = "variable",
        values_to = "energy.gini"
      ) %>%
      mutate(variable = ifelse(variable == "fe.gini.total", "Final Energy", variable)) %>%
      mutate(variable = ifelse(variable == "fe.gini.rescom", "Final Energy|Residential and Commercial", variable)) %>%
      mutate(variable = ifelse(variable == "fe.gini.transport", "Final Energy|Transportation", variable)) %>%
      mutate(variable = ifelse(variable == "fe.gini.industry", "Final Energy|Industry", variable))

    fe.national.avg <- fe.total.avg %>%
      left_join(fe.rescom.avg, by = "iso") %>%
      left_join(fe.transport.avg, by = "iso") %>%
      left_join(fe.industry.avg, by = "iso")

  }
}

# quantiles (not {yet} used by emulator, only intermediary for calculating ginis, and could potentially be used/generalised if we were to start doing projections using percentile groups)
saveRDS(fe.total.by.income,
  file = here(DATA.LOCATION, "FE_quantile_iso.RData")
)
saveRDS(fe.by.income,
  file = here(DATA.LOCATION, "FE_quantile_iso_sector.RData")
)


# final energy ginis
saveRDS(fe.ginis,
  file = here(DATA.LOCATION, "FE_gini_iso.RData")
)

# national average final energy per capita, from household consumption data
saveRDS(fe.national.avg,
  file = here(DATA.LOCATION, "FE_average_iso.RData")
)
