#' Class DLE.dimension.sanit
#'
#' @description subclass for sanitation dimension (inheriting DLE.dimension)

DLE.dimension.sanit <-
  setRefClass("DLE.dimension.sanit",
    contains = "DLE.dimension", # inherit the DLE.dimension class

    fields = list(
      r.rep = "numeric"
    ),

    methods = list(
      initialize = function(...) { # df.input = data.frame(expenditure=0, HDD=0, hh_size=0)) {
        print("initialize: sanitation")
        callSuper(...)

        # sanit dimension requires additioal information to derive DLS thresholds by country/region.
        DF.DLS <<- data.frame(DF.DLS) # , df.input)
        r.rep <<- 1 / 40 # inverse of lifetime
      },

      #' @description
      #' Derive DLS thresholds from data if not pre-determined
      DeriveThreshold = function() {
        print("DeriveThreshold: sanit")

        DF.DLS <<- DF.DLS %>% mutate(thres = 1) # Threshold: per cap
      },

      #' @description
      #' Overwrite the same-named function in the superclass to be specific to this dimension
      IdentifyGap = function() {
        print("IdentifyGap: sanit")

        fname_sanit <- "/API_SH.STA.SMSS.ZS_DS2_en_excel_v2_804737.xls" ## Improved sanitation data
        fname_inf_mort <- "/API_SP.DYN.IMRT.IN_DS2_en_excel_v2_992068.xls" ## Infant mortality data
        year.base <- 2015 # year for the gap data loading

        # Define function: manipulate subsets of data with dplyr
        mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
          condition <- eval(substitute(condition), .data, envir)
          .data[condition, ] <- .data[condition, ] %>% mutate(...)
          .data
        }

        # Load data: access to sanit supply
        print("Load data")
        sanit_acc <- read_excel(paste0(data.path, "/Sanitation", fname_sanit), sheet = "Data", skip = 3, col_names = TRUE)
        sanit_acc <- sanit_acc %>%
          select_at(c("Country Code", paste(year.base))) %>% # Extract data for the base year
          rename_at(paste(year.base), list(~ paste("sanit_acc"))) %>% # Rename column
          mutate(sanit_acc = sanit_acc / 100) %>% # Convert % numbers to range 0:1
          rename(iso = "Country Code")


        # Load data: Infant mortality
        mort <- read_excel(paste0(data.path, "/Sanitation", fname_inf_mort), sheet = "Data", skip = 3, col_names = TRUE)
        mort <- mort %>%
          select_at(c("Country Code", paste(year.base))) %>% # Extract data for the base year
          rename_at(paste(year.base), list(~ paste("mort"))) %>%
          rename(iso = "Country Code")

        # Join data
        sanit_acc <- sanit_acc %>% # Merge data: sanitation and infant mortality
          left_join(mort, by = "iso") # %>%
        # left_join(message.R11, by="iso") %>% # message region data
        # select(-country_name)

        # Fit a regression model
        print("Regression model: fitting")
        lm_sanit <- lm(sanit_acc ~ log(mort), data = sanit_acc)
        print(summary(lm_sanit))

        # Extrapolate results
        print("Start results extrapolation")
        sanit_extr <- message.R11 %>% left_join(sanit_acc, by = "iso") # initialize
        sanit_extr <- sanit_extr %>%
          mutate(sanit_pred = predict(lm_sanit, sanit_extr)) %>% # predicted results
          mutate(sanit_extr = sanit_pred) %>% # copy predicted results into a new column for extrapolated results
          mutate_cond(!is.na(sanit_acc), sanit_extr = sanit_acc) %>% # copy original data, where available
          select(iso, R11.region, sanit_extr) %>% # Keep only iso and extrapolation results
          rename(sanit_access = sanit_extr)

        # Calculate average gaps by R11.region and urban/rural
        sanit_extr_reg <- sanit_extr %>%
          group_by(R11.region) %>%
          summarise(sanit_access = mean(na.omit(sanit_access))) %>%
          rename(sanit_access_reg_avg = sanit_access)

        # Fill in data gaps
        sanit_extr <- sanit_extr %>%
          left_join(sanit_extr_reg, by = "R11.region") %>%
          mutate_cond(is.na(sanit_access), sanit_access = sanit_access_reg_avg) %>%
          select(-sanit_access_reg_avg) %>%
          mutate_cond(sanit_access > 1, sanit_access = 1) %>%
          mutate_cond(R11.region %in% c("NAM", "PAO", "WEU", "EEU"), sanit_access = 1) # impose no gaps for developed countries

        print("Results extrapolation: done!")

        # Join data and calculate gap (per capita)
        DF.DLS <<- DF.DLS %>%
          # left_join(pop, by = "iso") %>% #Join data: population
          # select(-country) %>% # remove country names
          left_join(sanit_extr, by = "iso") %>% # Join data: sanit access
          mutate(gap = 1 - sanit_access) %>% # Calculate gap (share of population with no access to sanit)
          mutate(deprivation.headcount = gap)
        # select(-population)
        print("Gap calculation: completed!")
      },

      #' @description
      #' Derive (or import) final energy intensities from data if not pre-determined
      DeriveEnergyIntensity = function() {
        print("DeriveEnergyIntensity: sanit")

        fname_en_int <- "/en_int.xlsx" #
        data_en_int <- read_excel(paste0(data.path, "/Sanitation", fname_en_int), sheet = "en_int", col_names = TRUE) %>%
          # project EI of CON.new on CON.new.pop and CON.new.dls
          add_ei_for_CONnewDLSPOPsplit()

        DF.tei <<- DF.tei %>%
          select(-e.int) %>% # Delete column to be overriden with data from excel
          # mutate(e.int = ifelse(type=="OP", 2.35893, 0.24088)) # average intensities [MJ/cap/yr] (elec+others) # construction also on a yearly base currently
          left_join(data_en_int)
      },

      #' @description
      #' Construct rollout scenario based on identified gap and other assumptions.
      #' This can be coded differently for different scenarios.
      ConstructRolloutScenario = function(scen, yr.tgt = year.target, yr.st.ro=year.base) {

        if (scen == "Income") {
          print("ConstructRolloutScenario:  Sanitation - Income")

          callSuper(scen, yr.tgt, r.rep)
        } else if (scen == "Income.regression") {
          print("ConstructRolloutScenario:  Sanitation - Income")

          callSuper(scen, yr.tgt, r.rep)
        } else if (scen == "ACCEL") {
          print("ConstructRolloutScenario:  Sanitation - ACCEL")

          callSuper(scen, yr.tgt, r.rep, year.start.rollout=yr.st.ro)
        }
      }
    )
  )
