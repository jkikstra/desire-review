#' Class DLE.dimension.housing
#'
#' @description subclass for housing dimension (inheriting DLE.dimension)

DLE.dimension.heating_con <-
  setRefClass("DLE.dimension.heating_con",
    contains = "DLE.dimension", # inherit the DLE.dimension class

    fields = list(
      r.rep = "numeric"
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
        print("initialize: heating_con")
        callSuper(...)

        # heating_con dimension requires additioal information to derive DLS thresholds by country/region.
        DF.DLS <<- data.frame(DF.DLS) # , df.input)
        # Replacement rate
        r.rep <<- 1 / 30 #  1/lifetime
      },

      #' @description
      #' Derive DLS thresholds from data if not pre-determined
      DeriveThreshold = function() {
        print("DeriveThreshold: heating_con")

        # Load AC needs data (pop share %)
        fname_heating_data <- "/heating_data_processed.csv"
        fname_fuel_share <- "/heating_fuel_share.csv"
        heating_data <- read.csv(paste0(data.path, "/Housing", fname_heating_data), stringsAsFactors = FALSE, header = T)
        fuel_share <- read.csv(paste0(data.path, "/Housing", fname_fuel_share), stringsAsFactors = FALSE, header = T)

        DF.DLS <<- DF.DLS %>%
          left_join(message.R11, by = "iso") %>%
          left_join(heating_data, by = "iso") %>% # Threshold: pop share in need of heating
          group_by(R11.region, grp) %>%
          mutate(heating_need_avg = mean(na.omit(heating_need))) %>% # Calculate regional averages - used when no other data available
          ungroup() %>%
          mutate(heating_need = ifelse(is.na(heating_need), heating_need_avg, heating_need)) %>%
          mutate(thres = heating_need) %>%
          select(-c(heating_need_avg, heating_need, country_name)) %>%
          # add population
          left_join(pop %>%
                      filter(year == year.base) %>%
                      select(-c(year, population, urb.rate, hh_size, n_hh)) %>% # keep pop by urb/rur
                      pivot_longer(cols = c(population.urb, population.rur), names_to = "grp", values_to = "population") %>%
                      mutate(grp = ifelse(grp == "population.urb", "urban", "rural"))) %>%
          select(-population)
      },

      #' @description
      #' Overwrite the same-named function in the superclass to be specific to this dimension
      IdentifyGap = function(DF.DLS.housing) {
        print("IdentifyGap: heating_con")

        # Method for gaps: # heating and housing-based calculation of gaps (gaps computed based on which of the two is larger)

        # # Load heating data (already loaded)
        # fname_heating_data <- "/heating_data_processed.csv" ## ACCESS TO CLEAN HEATING
        # heating_data <- read.csv(paste0(data.path,"/Housing",fname_heating_data),
        #                    stringsAsFactors = FALSE,
        #                    header=T)

        # Define function: manipulate subsets of data with dplyr
        mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
          condition <- eval(substitute(condition), .data, envir)
          .data[condition, ] <- .data[condition, ] %>% mutate(...)
          .data
        }

        # Create a new dataset for housing gaps
        gap_housing <- DF.DLS.housing %>%
          select(iso, grp, gap_perc) %>%
          rename(gap_housing = gap_perc)
        # print(head(gap_housing))

        # Load fuel share data
        fname_fuel_share <- "/heating_fuel_share.csv"
        fuel_share <- read.csv(paste0(data.path, "/Housing", fname_fuel_share), stringsAsFactors = FALSE, header = T)



        # Join data and calculate gap
        print("Join data and calculate gap")
        DF.DLS <<- DF.DLS %>%
          # left_join(heating_data, by = "iso") %>% # Data already joined
          left_join(fuel_share, by = c("R11.region" = "region_gea", "grp" = "grp")) %>%
          left_join(gap_housing, by = c("iso", "grp")) %>% # Join data housing gap
          mutate(gap = ifelse(thres > 0, thres * nonclean, 0)) %>% # Calculate gap per-capita: thres * share non-clean fuels
          ## uncomment the next 2 lines to have comparison housing_gap vs heating_gap
          # mutate(gap_heating = ifelse(thres>0, thres*nonclean,0)) %>% # Calculate gap per-capita: thres * share non-clean fuels
          # mutate(gap = pmax(gap_heating, gap_housing)) #%>% # mutate(gap = gap_heating)
          ## mutate(gap = ifelse(gap < thres, gap, thres))
          # mutate_cond(gap > thres, gap = thres)
          mutate(deprivation.headcount = gap) # share of pop in the gap

        print("Gap analysis completed!")
      },

      #' @description
      #' Derive (or import) final energy intensities from data if not pre-determined
      DeriveEnergyIntensity = function() {
        print("DeriveEnergyIntensity: housing")

        fname_en_int <- "/en_int_heating_CON.xlsx" #
        fname_fuel_share <- "/heating_fuel_share.csv"

        data_en_int <- read_excel(paste0(data.path, "/Housing", fname_en_int), sheet = "en_int", col_names = TRUE)
        data_en_int <- data_en_int %>%
          pivot_wider(names_from = "carrier", values_from = "e.int_unit", names_prefix = "e.int_") %>%
          # project EI of CON.new on CON.new.pop and CON.new.dls
          add_ei_for_CONnewDLSPOPsplit()
        fuel_share <- read.csv(paste0(data.path, "/Housing", fname_fuel_share), stringsAsFactors = FALSE, header = T)

        DF.tei <<- DF.tei %>%
          left_join(message.R11, by = "iso") %>%
          left_join(data_en_int) %>%
          left_join(fuel_share, by = c("R11.region" = "region_gea", "grp" = "grp")) %>% # Join fuel share data
          mutate(e.int_unit = (e.int_elec * electricity + e.int_gas * other_clean) / (electricity + other_clean)) %>%
          left_join(hh_size, by = "iso") %>% # Join data: population
          mutate(e.int = e.int_unit / hh_size) %>% # convert per-unit to per-capita (assuming one heating unit per household)
          select(-c(e.int_gas, e.int_elec, nonclean, electricity, other_clean)) %>% ## Comment this line for full report
          select(-c(country_name, R11.region)) %>%
          left_join(pop %>%
                      filter(year == year.base) %>%
                      select(-c(year, population, urb.rate, hh_size, n_hh)) %>% # keep pop by urb/rur
                      pivot_longer(cols = c(population.urb, population.rur), names_to = "grp", values_to = "population") %>%
                      mutate(grp = ifelse(grp == "population.urb", "urban", "rural")))
      },

      #' @description
      #' Construct rollout scenario based on identified gap and other assumptions.
      #' This can be coded differently for different scenarios.
      ConstructRolloutScenario = function(scen, yr.tgt = year.target, yr.st.ro=year.base) {
        # callSuper(scen)

        if (scen == "Income") {
          print("ConstructRolloutScenario:  Heating Construction - Income")

          callSuper(scen, yr.tgt, r.rep, urbrur = TRUE)

          # Keep track of gap closing progress (gap_upd)
          DF.rollout <<- DF.rollout %>%
            mutate(gap_upd = pmax(0, gap - (cumsum(inc * timestep) - (inc * timestep))))
        } else if (scen == "Income.regression") {
          print("ConstructRolloutScenario:  Heating Construction - Income regression")

          callSuper(scen, yr.tgt, r.rep, urbrur = TRUE)

          # Keep track of gap closing progress (gap_upd)
          DF.rollout <<- DF.rollout %>%
            mutate(gap_upd = pmax(0, gap - (cumsum(inc * timestep) - (inc * timestep))))
        }
        # else if (scen=="ACCEL") {
        else {
          print("ConstructRolloutScenario:  Heating Construction - ACCEL")

          callSuper(scen, yr.tgt, r.rep, urbrur = TRUE, year.start.rollout=yr.st.ro)

          # Keep track of gap closing progress (gap_upd)
          DF.rollout <<- DF.rollout %>%
            mutate(gap_upd = pmax(0, gap - (cumsum(inc * timestep) - (inc * timestep))))


        }
      },

      #' @description
      #' Run sensitivity case (= UpdateParameter + UpdateDLE)
      #' @param params list of fields and values to be updated
      SensitivityRun = function(params, scen, DF.DLS.housing) {
        # Replace the DFs to the bases
        DF.DLS <<- DF.DLS.base
        DF.rollout <<- DF.rollout.base
        DF.tei <<- DF.tei.base

        UpdateParameter(params)

        # Each dimension can do more here. e.g. DeriveThreshold()
        DeriveThreshold()
        IdentifyGap(DF.DLS.housing)
        ConstructRolloutScenario(scen)
        DeriveEnergyIntensity()

        UpdateDLE()
      }
    )
  )
