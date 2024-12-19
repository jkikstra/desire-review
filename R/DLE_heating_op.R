#' Class DLE.dimension.heating_op
#'
#' @description subclass for heating_op dimension (inheriting DLE.dimension)

DLE.dimension.heating_op <-
  setRefClass("DLE.dimension.heating_op",
    contains = "DLE.dimension", # inherit the DLE.dimension class

    fields = list(
      f = "numeric" # h/day  # Operation schedules (desired settings: h/day) # % time of heating usage
    ),
    methods = list(

      #' @description
      #' Generic function to manipulate subsets of data with dplyr
      mutate_cond = function(.data, condition, ..., envir = parent.frame()) {
        condition <- eval(substitute(condition), .data, envir)
        .data[condition, ] <- .data[condition, ] %>% mutate(...)
        .data
      },
      initialize = function(...) { # df.input = data.frame(expenditure=0, HDD=0, hh_size=0)) {
        print("initialize: heating_op")
        callSuper(...)

        # heating_op dimension requires additioal information to derive DLS thresholds by country/region.
        DF.DLS <<- data.frame(DF.DLS) # , df.input)

        f <<- 0.18
      },

      #' @description
      #' Derive DLS thresholds from data if not pre-determined
      DeriveThreshold = function() {
        print("DeriveThreshold: heating_op")

        DF.DLS <<- DF.DLS %>% mutate(thres = NA) # No threshold for thermal comfort
      },

      #' @description
      #' Overwrite the same-named function in the superclass to be specific to this dimension
      IdentifyGap = function() {
        print("IdentifyGap: heating_op")
        print("No gap analysis in this dimension!")
      },

      #' @description
      #' Derive (or import) final energy intensities from data if not pre-determined
      DeriveEnergyIntensity = function(rollout1.df) {
        print("DeriveEnergyIntensity: heating_op")

        # Filenames - energy demand results
        fname_ei_old <- "/ISO_agg_data_v14_DLE_ssp2_2010_hist_exist.csv" # average for the existing building stock
        fname_ei_new <- "/ISO_agg_data_v14_DLE_ssp2_2010_hist_new.csv" # current practice for new construction
        # #old files
        # fname_ei_old <- "/ISO_agg_data_v15_glob_ssp2_2010_hist_s3.csv" # average for the existing building stock
        # fname_ei_new <- "/ISO_agg_data_v15_glob_ssp2_2010_hist_s4.csv" # current practice for new construction
        fname_eff <- "/heating_efficiency.xlsx"
        fname_heating_data <- "/heating_data_processed.csv" ## NEED FOR HEATING
        fname_fuel_share <- "/heating_fuel_share.csv"

        # Load heating data
        heating_data <- read.csv(paste0(data.path, "/Housing", fname_heating_data), stringsAsFactors = FALSE, header = T)

        # Load fuel share data
        fuel_share <- read.csv(paste0(data.path, "/Housing", fname_fuel_share), stringsAsFactors = FALSE, header = T)

        # Settings for final energy demand calculation
        eff <- read_excel(paste0(data.path, "/Housing", fname_eff), sheet = "eff_heat", col_names = TRUE)

        fl_cnd <- 1 # % of heated floor - # Conditioned floor area (desired settings)
        fl_cnd_calc <- 1 # # Conditioned floor area used in energy calculations - assumed = 1 in calc (entire appart. conditioned)

        # f <- 0.43 #h/day  # Operation schedules (desired settings: h/day) # % time of heating usage
        # f <- 0.18 #h/day  # Operation schedules (desired settings: h/day) # % time of heating usage
        # f_calc <- 0.333 # operation schedules (used in energy calculations) # time of heating usage (0.333 in calculations = 8 h)
        f_calc <- 1 # operation schedules (used in energy calculations) # time of heating usage (0.333 in calculations = 8 h)

        floor_cap_calc <- 10 # Floorspace per capita used in energy demand calculations
        # floor_cap <- 10 # Floor surface per capita #m2/cap # Should be same as setting in housing_con

        u <- 1 # MJ # Unit conversion factor
        # u <- 1/3.6 # MJ to kWh # Unit conversion factor

        # energy intensity data - old buildings [MJ/m2/y]
        ei_old_temp <- read.csv(paste0(data.path, "/Housing", fname_ei_old), stringsAsFactors = FALSE)
        ei_old <- message.R11 %>%
          mutate(rural = NA, urban = NA) %>% # Create grp column (urban/rural)
          pivot_longer(cols = c("rural", "urban"), names_to = "grp", values_to = "temp") %>%
          select(-temp) %>%
          left_join(ei_old_temp, by = c("iso" = "ISO", "grp" = "pop")) %>%
          # filter(pop %in% c("rural", "urban")) %>%
          # rename(grp = pop) %>%
          left_join(heating_data) %>%
          left_join(fuel_share, by = c("R11.region" = "region_gea", "grp" = "grp")) %>% # Join fuel share data
          mutate(elec = as.numeric(select(filter(eff, vintage == "old" & elec == "elec"), eff))) %>%
          mutate(non.elec = as.numeric(select(filter(eff, vintage == "old" & elec == "non.elec"), eff))) %>%
          pivot_longer(cols = c("elec", "non.elec"), names_to = "elec", values_to = "eff") %>%
          mutate(shr_carrier = ifelse(elec == "elec", # Share of electric VS other fuels on clean fuels
            electricity / (electricity + other_clean),
            other_clean / (electricity + other_clean)
          )) %>%
          mutate(ei_old = ifelse(heating_need > 0, # Calculate energy demand for heating [MJ/m2/y] - average on pop in need for heating - weighted on elec / non.elec
            E_h_popwei * u / (popsum * heating_need) / floor_cap_calc * (fl_cnd / fl_cnd_calc) * (f / f_calc) * shr_carrier / eff,
            0
          )) %>%
          group_by(R11.region, grp, elec) %>% # Fill NA values with regions averages
          mutate(ei_old_avg = mean(na.omit(ei_old))) %>%
          ungroup() %>%
          mutate(ei_old = ifelse(is.na(ei_old), ei_old_avg, ei_old)) %>%
          select(iso, grp, elec, ei_old)

        # energy intensity data - new buildings [MJ/m2/y]
        ei_new_temp <- read.csv(paste0(data.path, "/Housing", fname_ei_new), stringsAsFactors = FALSE)
        ei_new <- message.R11 %>%
          mutate(rural = NA, urban = NA) %>% # Create grp column (urban/rural)
          pivot_longer(cols = c("rural", "urban"), names_to = "grp", values_to = "temp") %>%
          select(-temp) %>%
          left_join(ei_new_temp, by = c("iso" = "ISO", "grp" = "pop")) %>%
          # filter(pop %in% c("rural", "urban")) %>%
          # rename(grp = pop) %>%
          left_join(heating_data) %>%
          left_join(fuel_share, by = c("R11.region" = "region_gea", "grp" = "grp")) %>% # Join fuel share data
          mutate(elec = as.numeric(select(filter(eff, vintage == "new" & elec == "elec"), eff))) %>%
          mutate(non.elec = as.numeric(select(filter(eff, vintage == "new" & elec == "non.elec"), eff))) %>%
          pivot_longer(cols = c("elec", "non.elec"), names_to = "elec", values_to = "eff") %>%
          mutate(shr_carrier = ifelse(elec == "elec", # Share of electric VS other fuels on clean fuels
            electricity / (electricity + other_clean),
            other_clean / (electricity + other_clean)
          )) %>%
          mutate(ei_new = ifelse(heating_need > 0, # Calculate energy demand for heating [MJ/m2/y] - average on pop in need for heating - weighted on elec / non.elec
            E_h_popwei * u / (popsum * heating_need) / floor_cap_calc * (fl_cnd / fl_cnd_calc) * (f / f_calc) * shr_carrier / eff,
            0
          )) %>%
          group_by(R11.region, grp, elec) %>% # Fill NA values with regions averages
          mutate(ei_new_avg = mean(na.omit(ei_new))) %>%
          ungroup() %>%
          mutate(ei_new = ifelse(is.na(ei_new), ei_new_avg, ei_new)) %>%
          select(iso, grp, elec, ei_new)



        # Import housing stock data (for weighting energy demand on new/old buildings)

        stock <- rollout1.df %>%
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
      ConstructRolloutScenario = function(scen,
                                          yr.tgt = year.target,
                                          rollout2.df, floor.df
                                          # year.start.rollout=year.start.rollout # is inherited from floor_cap and gap_upd from housing; see DLE.ACCEL$dims$housing$DF.rollout
                                          ) {
        # callSuper(scen)
        # floor_cap <- 10 # should be the same as housing
        floor.df <- floor.df %>%
          select(iso, grp, thres) %>%
          rename(floor_cap = thres)

        if (scen == "Income") {
          print("ConstructRolloutScenario:  heating_op - Income")

          print(head(rollout2.df))
          rollout_heating <- rollout2.df %>%
            left_join(floor.df, by = c("iso", "grp")) %>%
            mutate(rollout.pcap = ifelse(type == "OP", floor_cap * (thres - gap_upd), 0)) %>%
            select(-rollout)
          head(rollout_heating)
          print(head(DF.rollout %>%
            left_join(rollout_heating, by = c("iso", "grp", "type", "year"))))
          message("All OK?")
          DF.rollout <<- DF.rollout %>%
            left_join(rollout_heating, by = c("iso", "grp", "type", "year")) %>%
            mutate(rollout = population * rollout.pcap)
        } else if (
          scen == "Income.regression"
        ) {
          print("ConstructRolloutScenario:  heating_op - Income regression")

          head(rollout2.df)
          rollout_heating <- rollout2.df %>%
            left_join(floor.df, by = c("iso", "grp")) %>%
            mutate(rollout.pcap = ifelse(type == "OP", floor_cap * (thres - gap_upd), 0)) %>%
            select(-rollout)
          head(rollout_heating)
          head(DF.rollout %>%
            left_join(rollout_heating, by = c("iso", "grp", "type", "year")))
          message("All OK?")
          DF.rollout <<- DF.rollout %>%
            left_join(rollout_heating, by = c("iso", "grp", "type", "year")) %>%
            mutate(rollout = population * rollout.pcap)
        } else if (
          scen == "ACCEL"
        ) {
          # else {
          print(paste("ConstructRolloutScenario: heating_op -", scen, "by", yr.tgt))
          print("Running rollout ACCEL - heating_op - Unit:m2/cap")

          rollout_heating <- rollout2.df %>%
            left_join(floor.df, by = c("iso", "grp")) %>%
            mutate(rollout.pcap = ifelse(type == "OP", floor_cap * (thres - gap_upd), 0)) %>%
            select(-rollout)

          # print(head(DF.rollout))
          DF.rollout <<- DF.rollout %>%
            left_join(rollout_heating, by = c("iso", "grp", "type", "year")) %>%
            mutate(rollout = population * rollout.pcap)

          print("Rollout runs completed! - heating_op - Unit:m2/cap")
        }
      },

      #' @description
      #' Run sensitivity case (= UpdateParameter + UpdateDLE)
      #' @param params list of fields and values to be updated
      SensitivityRun = function(params, scen, rollout1.df, rollout2.df, floor.df) {
        # Replace the DFs to the bases
        DF.DLS <<- DF.DLS.base
        DF.rollout <<- DF.rollout.base
        DF.tei <<- DF.tei.base

        UpdateParameter(params)

        # Each dimension can do more here. e.g. DeriveThreshold()
        DeriveThreshold()
        IdentifyGap()
        ConstructRolloutScenario(scen, rollout2.df = rollout2.df, floor.df = floor.df)
        DeriveEnergyIntensity(rollout1.df)

        UpdateDLE()
      }
    )
  )
