#' Class DLE.dimension.housing
#'
#' @description subclass for housing dimension (inheriting DLE.dimension)

DLE.dimension.cooling_con <-
  setRefClass("DLE.dimension.cooling_con",
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
        print("initialize: cooling_con")
        callSuper(...)

        # cooling_con dimension requires additioal information to derive DLS thresholds by country/region.
        DF.DLS <<- data.frame(DF.DLS) # , df.input)
        # Replacement rate
        r.rep <<- 1 / 10 #  1/lifetime
      },

      #' @description
      #' Derive DLS thresholds from data if not pre-determined
      DeriveThreshold = function() {
        print("DeriveThreshold: cooling_con")

        # Load AC needs data (pop share %)
        fname_ac_need <- "/ac_need_processed.csv"
        ac_need <- read.csv(paste0(data.path, "/Housing", fname_ac_need),
          stringsAsFactors = FALSE,
          header = T
        )

        DF.DLS <<- DF.DLS %>%
          left_join(ac_need, by = "iso") %>% # Threshold: share of population that needs an AC in the starting year
          mutate(thres = ac_need) %>%
          select(-ac_need)
      },

      #' @description
      #' Overwrite the same-named function in the superclass to be specific to this dimension
      IdentifyGap = function(DF.DLS.housing) {
        print("IdentifyGap: cooling_con")

        # Method for gaps # SELECT METHOD HERE
        # gap_method <- 1 # cooling-based calculation of gaps (housing disregarded)
        gap_method <- 2 # cooling and housing-based calculation of gaps (gaps computed based on which of the two is larger; if there's a high slum population, there's also a high [new] cooling need)

        # Load AC ownership data (pop share %)
        fname_ac_own <- "/ac_own_processed.csv" # DLS input: share of population that owns an AC in the starting year
        ac_own <- read.csv(paste0(data.path, "/Housing", fname_ac_own),
          stringsAsFactors = FALSE,
          header = T
        )

        # Define function: manipulate subsets of data with dplyr
        mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
          condition <- eval(substitute(condition), .data, envir)
          .data[condition, ] <- .data[condition, ] %>% mutate(...)
          .data
        }

        if (gap_method == 1) {
          # Join data and calculate gap
          print("Join data and calculate gap")
          DF.DLS <<- DF.DLS %>%
            left_join(ac_own, by = "iso") %>% # Join data: ac ownership
            left_join(message.R11, by = "iso") %>%
            group_by(R11.region, grp) %>%
            mutate(
              thres_avg = mean(na.omit(thres)), # Calculate regional averages - used when no other data available
              ac_own_avg = mean(na.omit(ac_own_avg))
            ) %>%
            ungroup() %>%
            mutate(
              thres = ifelse(is.na(thres), thres_avg, thres),
              ac_own = ifelse(is.na(ac_own), ac_own_avg, ac_own)
            ) %>%
            mutate(gap = ifelse(thres > ac_own, thres - ac_own, thres)) %>% # Calculate gap per-capita: no access to AC where needed
            select(-c(country_name, R11.region, ac_own_avg, thres_avg)) %>%
            # add population
            left_join(pop %>%
                        filter(year == year.base) %>%
                        select(-c(year, population, urb.rate, hh_size, n_hh)) %>% # keep pop by urb/rur
                        pivot_longer(cols = c(population.urb, population.rur), names_to = "grp", values_to = "population") %>%
                        mutate(grp = ifelse(grp == "population.urb", "urban", "rural")))
        }

        if (gap_method == 2) {
          # Create a new dataset for housing gaps
          gap_housing <- DF.DLS.housing %>%
            select(iso, grp, gap_perc) %>%
            rename(gap_housing = gap_perc)
          # print(head(gap_housing))

          # Join data and calculate gap
          print("Join data and calculate gap")
          DF.DLS <<- DF.DLS %>%
            left_join(ac_own, by = "iso") %>% # Join data: ac ownership
            left_join(gap_housing, by = c("iso", "grp")) %>% # Join data housing gap
            left_join(message.R11, by = "iso") %>%
            group_by(R11.region, grp) %>%
            mutate(
              thres_avg = mean(na.omit(thres)), # Calculate regional averages - used when no other data available
              ac_own_avg = mean(na.omit(ac_own))
            ) %>%
            ungroup() %>%
            mutate(
              thres = ifelse(is.na(thres), thres_avg, thres),
              ac_own = ifelse(is.na(ac_own), ac_own_avg, ac_own)
            ) %>%
            mutate(gap_cooling = ifelse(thres > ac_own, thres - ac_own, 0)) %>% # Calculate gap per-capita: no access to AC where needed
            mutate(gap = pmax(gap_cooling, gap_housing)) %>% # mutate(gap = gap_cooling)
            mutate(gap = ifelse(gap < thres, gap, thres)) %>%
            mutate(deprivation.headcount = gap) %>% # share of pop in the gap
            # mutate_cond(gap > thres, gap = thres)
            select(-c(country_name, R11.region, ac_own_avg, thres_avg)) %>%
            # add population
            left_join(pop %>%
                        filter(year == year.base) %>%
                        select(-c(year, population, urb.rate, hh_size, n_hh)) %>% # keep pop by urb/rur
                        pivot_longer(cols = c(population.urb, population.rur), names_to = "grp", values_to = "population") %>%
                        mutate(grp = ifelse(grp == "population.urb", "urban", "rural"))) %>%
            select(-population)
        }

        print("Gap analysis completed!")
      },

      #' @description
      #' Derive (or import) final energy intensities from data if not pre-determined
      DeriveEnergyIntensity = function() {
        print("DeriveEnergyIntensity: cooling_CON")

        fname_en_int <- "/en_int_cooling_CON.xlsx" #
        data_en_int <- read_excel(paste0(data.path, "/Housing", fname_en_int), sheet = "en_int", col_names = TRUE) %>%
          # project EI of CON.new on CON.new.pop and CON.new.dls
          add_ei_for_CONnewDLSPOPsplit()

        DF.tei <<- DF.tei %>%
          left_join(data_en_int) %>% # e.int_unit = energy per AC unit
          # left_join(pop) %>% #Join data: population # This should be by urb/rur!!!
          left_join(hh_size, by = "iso") %>% # Join data: population
          mutate(e.int = e.int_unit / hh_size) %>% # convert per-unit to per-capita (assuming one AC unit per household)
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

        if (scen == "Income") {
          print("ConstructRolloutScenario:  cooling_con - Income")
          callSuper(scen, yr.tgt, r.rep, urbrur = TRUE)

          # Keep track of gap closing progress (gap_upd)
          DF.rollout <<- DF.rollout %>%
            mutate(gap_upd = pmax(0, gap - (cumsum(inc * timestep) - (inc * timestep))))
        } else if (scen == "Income.regression") {
          print("ConstructRolloutScenario:  cooling_con - Income regression")
          callSuper(scen, yr.tgt, r.rep, urbrur = TRUE)

          # Keep track of gap closing progress (gap_upd)
          DF.rollout <<- DF.rollout %>%
            mutate(gap_upd = pmax(0, gap - (cumsum(inc * timestep) - (inc * timestep))))
        }
        # else if (scen=="ACCEL") {
        else {
          print(paste("ConstructRolloutScenario: cooling_con -", scen, "by", yr.tgt))
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
