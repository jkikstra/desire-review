#' Class DLE.dimension.water
#'
#' @description subclass for water dimension (inheriting DLE.dimension)

DLE.dimension.water <-
  setRefClass("DLE.dimension.water",
    contains = "DLE.dimension", # inherit the DLE.dimension class

    fields = list(

      # m3/cap/year
      thres.vol = "numeric",
      r.rep = "numeric"

    ),
    methods = list(
      initialize = function(...) { # df.input = data.frame(expenditure=0, HDD=0, hh_size=0)) {
        print("initialize: water")
        callSuper(...)

        # water dimension requires additioal information to derive DLS thresholds by country/region.
        DF.DLS <<- data.frame(DF.DLS) # , df.input)
        thres.vol <<- 23.725 # m3/cap/year corresponding to 65 l/cap/day
        r.rep <<- 1 / 40
      },

      #' @description
      #' Derive DLS thresholds from data if not pre-determined
      DeriveThreshold = function() {
        print("DeriveThreshold: water")

        DF.DLS <<- DF.DLS %>% mutate(thres = thres.vol) # Threshold: in m3/cap/year corresponding to 65 l/cap/day
      },

      #' @description
      #' Overwrite the same-named function in the superclass to be specific to this dimension
      IdentifyGap = function() {
        print("IdentifyGap: water")

        fname_water <- "/API_SH.H2O.SMDW.ZS_DS2_en_excel_v2_802993.xls" ## Improved water access
        fname_inf_mort <- "/API_SP.DYN.IMRT.IN_DS2_en_excel_v2_992068.xls" ## Infant mortality data
        year.base <- 2015 # year for data to be loaded

        # Define function: manipulate subsets of data with dplyr
        mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
          condition <- eval(substitute(condition), .data, envir)
          .data[condition, ] <- .data[condition, ] %>% mutate(...)
          .data
        }

        print("Load data: water")
        # Load data: access to water supply
        water_acc <- read_excel(paste0(data.path, "/Water", fname_water), sheet = "Data", skip = 3, col_names = TRUE)
        water_acc <- water_acc %>%
          select_at(c("Country Code", paste(year.base))) %>% # Extract data for the base year
          rename_at(paste(year.base), list(~ paste("water_acc"))) %>% # Rename column
          mutate(water_acc = water_acc / 100) %>% # Convert % numbers to range 0:1
          rename(iso = "Country Code")

        # Load data: Infant mortality
        mort <- read_excel(paste0(data.path, "/Water", fname_inf_mort), sheet = "Data", skip = 3, col_names = TRUE)
        mort <- mort %>%
          select_at(c("Country Code", paste(year.base))) %>% # Extract data for the base year
          rename_at(paste(year.base), list(~ paste("mort"))) %>%
          rename(iso = "Country Code")

        # Join data
        water_acc <- water_acc %>% # Merge data: water and infant mortality
          left_join(mort, by = "iso") %>%
          left_join(message.R11, by = "iso") %>% # message region data
          select(-c(country_name, R11.region))

        # Fit a regression model
        print("Regression model: fitting")
        lm_water <- lm(water_acc ~ log(mort), data = water_acc)
        print(summary(lm_water))

        # Extrapolate results
        print("Start results extrapolation")
        water_extr <- message.R11 %>%
          left_join(water_acc, by = "iso") # initialize
        water_extr <- water_extr %>%
          mutate(water_pred = predict(lm_water, water_extr)) %>% # predicted results
          mutate(water_extr = water_pred) %>% # copy predicted results into a new column for extrapolated results
          mutate_cond(!is.na(water_acc), water_extr = water_acc) %>% # copy original data, where available
          select(iso, R11.region, water_extr) %>% # Keep only iso and extrapolation results
          rename(water_access = water_extr)

        # Calculate average gaps by R11.region and urban/rural
        water_extr_reg <- water_extr %>%
          group_by(R11.region) %>%
          summarise(water_access = mean(na.omit(water_access))) %>%
          rename(water_access_reg_avg = water_access)

        # Fill in data gaps
        water_extr <- water_extr %>%
          left_join(water_extr_reg, by = "R11.region") %>%
          mutate_cond(is.na(water_access), water_access = water_access_reg_avg) %>%
          select(-water_access_reg_avg) %>%
          mutate_cond(water_access > 1, water_access = 1) %>%
          mutate_cond(R11.region %in% c("NAM", "PAO", "WEU", "EEU"), water_access = 1) # impose no gaps for developed countries


        print("Results extrapolation: done!")

        # Join data and calculate gap
        print("Join data and calculate gap")
        DF.DLS <<- DF.DLS %>%
          # left_join(pop, by = "iso") %>% #Join data: population
          # select(-country) %>% # remove country names
          left_join(water_extr, by = "iso") %>% # Join data: water access
          mutate(gap = thres * (1 - water_access)) %>% # Calculate gap (m3 water corresponding to the share of population with no access to water)
          mutate(deprivation.headcount = (1 - water_access))
        # select(-population)
        print("Gap analysis completed!")
      },

      #' @description
      #' Derive (or import) final energy intensities from data if not pre-determined
      DeriveEnergyIntensity = function() {
        print("DeriveEnergyIntensity: water")

        fname_en_int <- "/en_int.xlsx" #
        data_en_int <- read_excel(paste0(data.path, "/Water", fname_en_int), sheet = "en_int", col_names = TRUE) %>%
          # project EI of CON.new on CON.new.pop and CON.new.dls
          add_ei_for_CONnewDLSPOPsplit()

        DF.tei <<- DF.tei %>%
          select(-e.int) %>% # Delete column to be overriden with data from excel
          # mutate(e.int = ifelse(type=="OP", 2.35893, 0.24088)) # average intensities [MJ/m3 water] (elec+others)
          left_join(data_en_int)
      },

      #' @description
      #' Construct rollout scenario based on identified gap and other assumptions.
      #' This can be coded differently for different scenarios.
      ConstructRolloutScenario = function(scen, yr.tgt = year.target, yr.st.ro=year.base) {

        if (scen == "Income") {
          print("ConstructRolloutScenario:  Water - Income")

          callSuper(scen, yr.tgt, r.rep)
        } else if (scen == "Income.regression") {
          print("ConstructRolloutScenario:  Water - Income")

          callSuper(scen, yr.tgt, r.rep)
        } else if (scen == "ACCEL") {
          print("ConstructRolloutScenario:  Water - ACCEL")

          callSuper(scen, yr.tgt, r.rep, year.start.rollout=yr.st.ro)
        } else {
        }
      }
    )
  )
