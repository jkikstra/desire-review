#' Class DLE.dimension.hotwater_op
#'
#' @description subclass for hotwater_op dimension (inheriting DLE.dimension)

DLE.dimension.hotwater_op <-
  setRefClass("DLE.dimension.hotwater_op",
    contains = "DLE.dimension", # inherit the DLE.dimension class

    methods = list(

      #' @description
      #' Generic function to manipulate subsets of data with dplyr
      mutate_cond = function(.data, condition, ..., envir = parent.frame()) {
        condition <- eval(substitute(condition), .data, envir)
        .data[condition, ] <- .data[condition, ] %>% mutate(...)
        .data
      },
      initialize = function(...) { # df.input = data.frame(expenditure=0, HDD=0, hh_size=0)) {
        print("initialize: hotwater_op")
        callSuper(...)

        # hotwater_op dimension requires additioal information to derive DLS thresholds by country/region.
        DF.DLS <<- data.frame(DF.DLS) # , df.input)
      },

      #' @description
      #' Derive DLS thresholds from data if not pre-determined
      DeriveThreshold = function() {
        print("DeriveThreshold: hotwater_op")

        DF.DLS <<- DF.DLS %>% mutate(thres = NA) # No threshold for thermal comfort
      },

      #' @description
      #' Overwrite the same-named function in the superclass to be specific to this dimension
      IdentifyGap = function(gap_water.df, gap_heating.df) {
        print("IdentifyGap: hotwater_op")

        # water gaps
        gap_water.df <- gap_water.df %>%
          select(iso, deprivation.headcount) %>%
          rename(gap_water = deprivation.headcount)
        # print(head(gap_housing))

        # heating gap
        # thres is assumed same as in heating
        gap_heating.df <- gap_heating.df %>%
          select(iso, grp, thres, gap) %>%
          rename(
            gap_heating = gap,
            thres_heating = thres
          )

        # Join data and calculate gap (all gaps per-cap)
        print("Join data and calculate gap")
        DF.DLS <<- DF.DLS %>%
          # left_join(message.R11, by = "iso") %>%
          left_join(gap_heating.df, by = c("iso", "grp")) %>%
          left_join(gap_water.df, by = "iso") %>% # Join data housing gap
          mutate(thres = thres_heating) %>% # threshold same as for heating
          mutate(gap = pmax(gap_water, gap_heating)) %>% # Take max gap
          mutate(gap = ifelse(gap < thres, gap, thres)) %>% # compare with threshold
          mutate(deprivation.headcount = gap) # %>%# share of pop in the gap
        # select(-c(country_name,R11.region))

        print("Gap analysis completed!")
      },

      #' @description
      #' Derive (or import) final energy intensities from data if not pre-determined
      DeriveEnergyIntensity = function(rollout_housing.df) {
        print("DeriveEnergyIntensity: hotwater_op")

        # Filenames - energy demand results
        fname_ei <- "/hot_water_energy_demand.xlsx" # Energy intensity data
        fname_eff <- "/heating_efficiency.xlsx" # Efficiency data
        fname_fuel_share <- "/heating_fuel_share.csv"

        # Load fuel share data
        fuel_share <- read.csv(paste0(data.path, "/Housing", fname_fuel_share), stringsAsFactors = FALSE, header = T)

        # Settings for final energy demand calculation
        eff <- read_excel(paste0(data.path, "/Housing", fname_eff), sheet = "eff_heat", col_names = TRUE)

        u <- 1 # MJ # Unit conversion factor
        # u <- 1/3.6 # MJ to kWh # Unit conversion factor

        # Useful energy demand intensity [MJ/cap/yr]
        # Hot water demand for showering only (30 l/day/cap)
        ei_useful <- read_excel(paste0(data.path, "/Housing", fname_ei), sheet = "ei_useful", col_names = TRUE)

        ## Energy intensity (final) old buildings [MJ/cap/yr]
        ei_old <- message.R11 %>%
          mutate(rural = NA, urban = NA) %>% # Create grp column (urban/rural)
          pivot_longer(cols = c("rural", "urban"), names_to = "grp", values_to = "temp") %>%
          select(-temp) %>%
          left_join(ei_useful, by = c("R11.region" = "region_gea")) %>%
          # filter(pop %in% c("rural", "urban")) %>%
          # rename(grp = pop) %>%
          left_join(fuel_share, by = c("R11.region" = "region_gea", "grp" = "grp")) %>% # Join fuel share data
          mutate(elec = as.numeric(select(filter(eff, vintage == "old" & elec == "elec"), eff))) %>%
          mutate(non.elec = as.numeric(select(filter(eff, vintage == "old" & elec == "non.elec"), eff))) %>%
          pivot_longer(cols = c("elec", "non.elec"), names_to = "elec", values_to = "eff") %>%
          mutate(shr_carrier = ifelse(elec == "elec", # Share of electric VS other fuels on clean fuels
            electricity / (electricity + other_clean),
            other_clean / (electricity + other_clean)
          )) %>%
          mutate(ei_old = ei_useful * u * shr_carrier / eff) %>% # Calculate final energy demand [MJ/cap/yr] - weighted on elec / non.elec
          # group_by(R11.region, grp, elec) %>% # Fill NA values with regions averages [NOT NEEDED - values are by MESSAGE]
          # mutate(ei_old_avg = mean(na.omit(ei_old))) %>%
          # ungroup() %>%
          # mutate(ei_old = ifelse(is.na(ei_old),ei_old_avg,ei_old)) %>%
          select(iso, grp, elec, ei_old)

        ## Energy intensity (final) new buildings [MJ/cap/yr]
        ei_new <- message.R11 %>%
          mutate(rural = NA, urban = NA) %>% # Create grp column (urban/rural)
          pivot_longer(cols = c("rural", "urban"), names_to = "grp", values_to = "temp") %>%
          select(-temp) %>%
          left_join(ei_useful, by = c("R11.region" = "region_gea")) %>%
          # filter(pop %in% c("rural", "urban")) %>%
          # rename(grp = pop) %>%
          left_join(fuel_share, by = c("R11.region" = "region_gea", "grp" = "grp")) %>% # Join fuel share data
          mutate(elec = as.numeric(select(filter(eff, vintage == "new" & elec == "elec"), eff))) %>%
          mutate(non.elec = as.numeric(select(filter(eff, vintage == "new" & elec == "non.elec"), eff))) %>%
          pivot_longer(cols = c("elec", "non.elec"), names_to = "elec", values_to = "eff") %>%
          mutate(shr_carrier = ifelse(elec == "elec", # Share of electric VS other fuels on clean fuels
            electricity / (electricity + other_clean),
            other_clean / (electricity + other_clean)
          )) %>%
          mutate(ei_new = ei_useful * u * shr_carrier / eff) %>% # Calculate final energy demand [MJ/cap/yr] - weighted on elec / non.elec
          # group_by(R11.region, grp, elec) %>% # Fill NA values with regions averages [NOT NEEDED - values are by MESSAGE]
          # mutate(ei_new_avg = mean(na.omit(ei_new))) %>%
          # ungroup() %>%
          # mutate(ei_new = ifelse(is.na(ei_new),ei_new_avg,ei_new)) %>%
          select(iso, grp, elec, ei_new)



        # Import housing stock data (for weighting energy demand on new/old buildings)
        stock <- rollout_housing.df %>%
          ungroup() %>%
          filter(type == "OP") %>%
          select(iso, grp, type, year, stock_new_pcap, stock_old_pcap) %>%
          mutate(elec = NA, non.elec = NA) %>% # Create elec column (elec/non.elec)
          pivot_longer(cols = c("elec", "non.elec"), names_to = "elec", values_to = "temp") %>%
          select(-temp) %>%
          mutate(share_new = stock_new_pcap / (stock_new_pcap + stock_old_pcap)) %>% # share of new buildings in the stock
          left_join(ei_old, by = c("iso", "grp", "elec")) %>%
          left_join(ei_new, by = c("iso", "grp", "elec")) %>%
          mutate(e.int = (ei_old * (1 - share_new)) + (ei_new * share_new))

        # prepare population data
        pop.scen <- pop %>%
          filter(year >= year.base & year <= year.end) %>%
          select(-c(population, urb.rate, hh_size, n_hh)) %>% # keep pop by urb/rur
          pivot_longer(cols = c(population.urb, population.rur), names_to = "grp", values_to = "population") %>%
          mutate(grp = ifelse(grp == "population.urb", "urban", "rural"))

        # Dataset: energy intensity
        DF.tei <<- DF.tei %>%
          select(-e.int) %>% # remove to run left_join and add e.int values
          left_join(pop.scen) %>% # years are added here
          left_join(stock, by = c("iso", "grp", "year", "type", "elec")) %>% # e.int = energy per m2 housing [MJ/m2/y]
          # mutate_cond(elec == "non.elec", e.int = 0) %>%
          mutate_cond(type != "OP", e.int = 0) %>%
          group_by(iso, grp, type, year) %>%
          mutate(e.int.tot = sum(na.omit(e.int))) %>% # calculate total energy (elec + non.elec)
          ungroup() %>%
          mutate_cond(elec == "total", e.int = e.int.tot) %>%
          select(-c(e.int.tot, population, stock_new_pcap, stock_old_pcap, share_new, ei_old, ei_new)) %>%
          filter(year==2015) %>%
          select(-year) %>%
          left_join(pop %>%
                      filter(year == year.base) %>%
                      select(-c(year, population, urb.rate, hh_size, n_hh)) %>% # keep pop by urb/rur
                      pivot_longer(cols = c(population.urb, population.rur), names_to = "grp", values_to = "population") %>%
                      mutate(grp = ifelse(grp == "population.urb", "urban", "rural")))
      },


      #' @description
      #' Construct rollout scenario based on identified gap and other assumptions.
      #' This can be coded differently for different scenarios.
      ConstructRolloutScenario = function(scen, yr.tgt = year.target, rollout.df,
                                          yr.st.ro=year.base) {
        # callSuper(scen)



        if (scen == "Income") {
          print("ConstructRolloutScenario:  hotwater_op - Income")

          callSuper(scen, yr.tgt, urbrur = TRUE)
        } else if (scen == "Income.regression") {
          print("ConstructRolloutScenario:  hotwater_op - Income Regression")

          callSuper(scen, yr.tgt, urbrur = TRUE)
        } else if (
          scen == "ACCEL"
        ) {
          # else {
          print(paste("ConstructRolloutScenario: hotwater_op -", scen, "by", yr.tgt))
          print("Running rollout - hotwater_op - Unit:cap")

          callSuper(scen, yr.tgt, urbrur = TRUE,
                    year.start.rollout=yr.st.ro)
        }
      },

      #' @description
      #' Run sensitivity case (= UpdateParameter + UpdateDLE)
      #' @param params list of fields and values to be updated
      SensitivityRun = function(params, scen, gap_water.df,
                                gap_heating.df, rollout_housing.df, rollout.df) {
        # Replace the DFs to the bases
        DF.DLS <<- DF.DLS.base
        DF.rollout <<- DF.rollout.base
        DF.tei <<- DF.tei.base

        UpdateParameter(params)

        # Each dimension can do more here. e.g. DeriveThreshold()
        DeriveThreshold()
        IdentifyGap(gap_water.df, gap_heating.df)
        ConstructRolloutScenario(scen, rollout.df = rollout.df)
        DeriveEnergyIntensity(rollout_housing.df)

        UpdateDLE()
      }
    )
  )
