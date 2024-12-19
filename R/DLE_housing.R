#' Class DLE.dimension.housing
#'
#' @description subclass for housing dimension (inheriting DLE.dimension)

DLE.dimension.housing <-
  setRefClass("DLE.dimension.housing",
    contains = "DLE.dimension", # inherit the DLE.dimension class

    fields = list(
      floor = "data.frame", # per capita threshold by country
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
        print("initialize: housing")
        callSuper(...)

        # housing dimension requires additioal information to derive DLS thresholds by country/region.
        DF.DLS <<- data.frame(DF.DLS) # , df.input)

        fname_floor <- "/floorcap_thres.csv" ## Import threshold data:country-dependent floor/cap (adjusted on household size)
        floor <<- read.csv(paste0(data.path, "/Housing", fname_floor), stringsAsFactors = FALSE, header = T)
        r.rep <<- 1 / 50
      },

      #' @description
      #' Derive DLS thresholds from data if not pre-determined
      DeriveThreshold = function() {
        print("DeriveThreshold: housing")

        # DF.DLS <<- DF.DLS %>% mutate(thres = 0) #Threshold: no housing gap
        # DF.DLS <<- DF.DLS %>% mutate(thres = 10) #Threshold: in m2/cap of floor surface area - Fixed threshold
        DF.DLS <<- DF.DLS %>%
          left_join(floor, by = "iso") %>% # Threshold: in m2/cap of floor surface area - Country-dependent thres (adjusted on household size)
          left_join(message.R11 %>% select(iso, R11.region), by = "iso") %>%
          group_by(R11.region, grp) %>%
          mutate(thres_floor_avg = mean(thres_floor, na.rm = T)) %>%
          ungroup() %>%
          mutate(thres = ifelse(is.na(thres_floor), thres_floor_avg, thres_floor)) %>%
          select(-c(thres_floor, thres_floor_avg, R11.region))
      },

      #' @description
      #' Overwrite the same-named function in the superclass to be specific to this dimension
      IdentifyGap = function() {
        print("IdentifyGap: housing")

        fname_slum <- "/API_EN.POP.SLUM.UR.ZS_DS2_en_excel_v2_893267.xls" ## slum population (% of urban)
        fname_perm_rur <- "/data_perm_wall_rur.csv" ## pop share with permanent wall (% of rural)
        # fname_perm_urb <- "/data_perm_wall_rur.csv" ## pop share with permanent wall (% of urban)
        fname_pov <- "/API_SI.POV.UMIC.GP_DS2_en_excel_v2_1002313.xls" ## Data: poverty gap below 5.5$ (WB)
        fname_gdp <- "/API_NY.GDP.PCAP.PP.KD_DS2_en_excel_v2_887670.xls" ## Data: GDP 2011 PPP (WB)

        year.base <- 2015 # year for data to be loaded
        year.slum <- 2014 # year for the slum data (different to ensure data availability)

        # Define function: manipulate subsets of data with dplyr
        mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
          condition <- eval(substitute(condition), .data, envir)
          .data[condition, ] <- .data[condition, ] %>% mutate(...)
          .data
        }

        print("Load data: housing")
        # Load data: slum population (% pop)
        slum <- read_excel(paste0(data.path, "/Housing", fname_slum), sheet = "Data", skip = 3, col_names = TRUE)
        slum <- slum %>%
          select_at(c("Country Code", paste(year.slum))) %>% # Extract data for the base year
          rename_at(paste(year.slum), list(~ paste("slum"))) %>% # Rename column
          mutate(slum = slum / 100) %>% # Convert % numbers to range 0:1
          rename(iso = "Country Code")

        # Load data: permanent walls in rural (% pop)
        perm_rur <- read.csv(paste0(data.path, "/Housing", fname_perm_rur),
                             stringsAsFactors = FALSE,
                             header = T
        )
        perm_rur <- perm_rur %>% rename(perm_rur = share_perm)

        # Load data: Poverty gap
        pov <- read_excel(paste0(data.path, "/Housing", fname_pov), sheet = "Data", skip = 3, col_names = TRUE)
        pov <- pov %>%
          select_at(c("Country Code", paste(year.base))) %>% # Extract data for the base year
          rename_at(paste(year.base), list(~ paste("pov"))) %>%
          rename(iso = "Country Code")

        # Join data
        housing <- message.R11 %>%
          select(iso, R11.region) %>%
          left_join(slum, by = "iso") %>% # Merge data: slum pop
          left_join(perm_rur, by = "iso") %>% # Permanent walls - rural
          # left_join(perm_urb, by = "iso") %>% # Permanent walls - urban
          # left_join(gdp, by = "iso") %>% # GDP
          left_join(pov, by = "iso") # Poverty

        # Fit a regression model (Urban)
        print("Regression model: fitting")
        lm_hous_urb <- lm(slum ~ pov, data = housing)
        print(summary(lm_hous_urb))

        # Fit a regression model (Rural)
        print("Regression model: fitting")
        lm_hous_rur <- lm(perm_rur ~ pov, data = housing)
        print(summary(lm_hous_rur))

        # Extrapolate results
        print("Start results extrapolation")
        housing_extr <- housing # initialize
        housing_extr <- housing_extr %>%
          # urban gap
          mutate(gap_urb_pred = predict(lm_hous_urb, housing_extr)) %>% # predicted results
          mutate(gap_urb_extr = gap_urb_pred) %>% # copy predicted results into a new column for extrapolated results
          mutate_cond(!is.na(slum), gap_urb_extr = slum) %>% # copy original data, where available
          # rural gap
          mutate(gap_rur_pred = 1 - predict(lm_hous_rur, housing_extr)) %>% # predicted results
          mutate(gap_rur_extr = gap_rur_pred) %>% # copy predicted results into a new column for extrapolated results
          mutate_cond(!is.na(perm_rur), gap_rur_extr = 1 - perm_rur) %>% # copy original data, where available

          select(iso, R11.region, gap_urb_extr, gap_rur_extr) %>% # Keep only iso and extrapolation results
          rename(urban = gap_urb_extr) %>%
          rename(rural = gap_rur_extr) %>%
          gather(key = "grp", value = "gap_perc", c(urban, rural)) %>%
          # impose no gaps for developed countries
          mutate_cond(R11.region %in% c("NAM", "PAO", "WEU", "EEU"), gap_perc = 0)

        # Calculate average gaps by R11.region and urban/rural
        housing_reg <- housing_extr %>%
          group_by(R11.region, grp) %>%
          summarise(gap_perc = mean(na.omit(gap_perc))) %>%
          rename(gap_reg_avg = gap_perc)

        # Fill in data gaps
        housing_extr <- housing_extr %>%
          left_join(housing_reg, by = c("R11.region", "grp")) %>%
          mutate_cond(is.na(gap_perc), gap_perc = gap_reg_avg) %>%
          select(-gap_reg_avg)

        print("Results extrapolation: done!")

        # Join data and calculate gap
        print("Join data and calculate gap")

        # First edit pop data - extract base year and pivot rur/urb
        pop.base <- pop %>%
          filter(year == year.base) %>%
          select(-c(year, population, urb.rate, hh_size, n_hh)) %>% # keep pop by urb/rur
          pivot_longer(cols = c(population.urb, population.rur), names_to = "grp", values_to = "population") %>%
          mutate(grp = ifelse(grp == "population.urb", "urban", "rural"))

        DF.DLS <<- DF.DLS %>%
          left_join(pop.base, by = c("iso", "grp")) %>% # Join data: population
          left_join(housing_extr, by = c("iso", "grp")) %>% # Join data: housing access
          mutate(gap_tot = population * gap_perc * thres) %>% # Calculate gap: tot m2 missing (population with no access to housing)
          mutate(gap = ifelse(population > 0, gap_perc * thres, 0)) %>% # Calculate gap per-capita: m2/cap missing (population with no access to housing)
          mutate(gap_perc = ifelse(population > 0, gap_perc, 0)) %>% # impose gap_perc=0 where no pop
          mutate(deprivation.headcount = gap_perc) %>% # share of pop in the gap
          select(-population)

        print("Gap analysis completed!")

      },

      #' @description
      #' Derive (or import) final energy intensities from data if not pre-determined
      DeriveEnergyIntensity = function() {
        print("DeriveEnergyIntensity: housing")

        fname_en_int <- "/en_int_housing_CON.xlsx" #
        data_en_int <- read_excel(paste0(data.path, "/Housing", fname_en_int), sheet = "en_int", col_names = TRUE) %>%
          # project EI of CON.new on CON.new.pop and CON.new.dls
          add_ei_for_CONnewDLSPOPsplit()

        DF.tei <<- DF.tei %>%
          select(-e.int) %>% # remove to run left_join and add e.int values
          left_join(data_en_int) %>% # e.int = embodied energy per m2 housing
          left_join(pop %>%
                      filter(year == year.base) %>%
                      select(-c(year, population, urb.rate, hh_size, n_hh)) %>% # keep pop by urb/rur
                      pivot_longer(cols = c(population.urb, population.rur), names_to = "grp", values_to = "population") %>%
                      mutate(grp = ifelse(grp == "population.urb", "urban", "rural")))
          # left_join(pop) %>% #Join data: population # This should be by urb/rur!!!

        # DF.tei <<- DF.tei %>% mutate(e.int = ifelse(type=="OP", 2.35893, 0.24088)) # average intensities [MJ/m3 housing] (elec+others)
      },


      #' @description
      #' Construct rollout scenario based on identified gap and other assumptions.
      #' This can be coded differently for different scenarios.
      ConstructRolloutScenario = function(scen, yr.tgt = year.target, yr.st.ro=year.base) {

        if (scen == "Income") {
          print("ConstructRolloutScenario:  Housing - Income")

          callSuper(scen, yr.tgt, r.rep, urbrur = TRUE)

          # stock tracking: old/new stock
          df.stock.cum <- DF.rollout %>%
            select(-rollout) %>%
            spread(type, "rollout.pcap")
          df.stock.cum <- df.stock.cum %>%
            group_by(iso, grp) %>%
            mutate(gap_upd = pmax(0, gap - (cumsum(inc * timestep) - (inc * timestep)))) %>%
            mutate(stock_new_pcap = pmin(thres, cumsum(CON.new + CON.rep))) %>%
            mutate(stock_old_pcap = pmax(0, thres - gap_upd - stock_new_pcap)) %>%
            select(iso, grp, year, gap_upd, stock_new_pcap, stock_old_pcap)

          # # Add new/old stock information (for OP energy calculation) # m2/cap
          DF.rollout <<- DF.rollout %>%
            left_join(df.stock.cum)
        } else if (scen == "Income.regression") {
          print("ConstructRolloutScenario:  Housing - Income Regression")

          callSuper(scen, yr.tgt, r.rep, urbrur = TRUE)

          # stock tracking: old/new stock
          df.stock.cum <- DF.rollout %>%
            select(-rollout) %>%
            spread(type, "rollout.pcap")
          df.stock.cum <- df.stock.cum %>%
            group_by(iso, grp) %>%
            mutate(gap_upd = pmax(0, gap - (cumsum(inc * timestep) - (inc * timestep)))) %>%
            mutate(stock_new_pcap = pmin(thres, cumsum(CON.new + CON.rep))) %>%
            mutate(stock_old_pcap = pmax(0, thres - gap_upd - stock_new_pcap)) %>%
            select(iso, grp, year, gap_upd, stock_new_pcap, stock_old_pcap)

          # # Add new/old stock information (for OP energy calculation) # m2/cap
          DF.rollout <<- DF.rollout %>%
            left_join(df.stock.cum)
        }
        # else if (scen=="ACCEL") {
        else {
          print(paste("ConstructRolloutScenario: Housing -", scen, "by", yr.tgt))
          print(paste0("Rollout unit: ", unit_rollout))

          callSuper(scen, yr.tgt, r.rep, urbrur = TRUE, year.start.rollout=yr.st.ro)

          # stock tracking: old/new stock
          df.stock.cum <- DF.rollout %>%
            select(-rollout) %>%
            spread(type, "rollout.pcap")
          df.stock.cum <- df.stock.cum %>%
            group_by(iso, grp) %>%
            mutate(gap_upd = pmax(0, gap - (cumsum(inc * timestep) - (inc * timestep)))) %>%
            mutate(stock_new_pcap = pmin(thres, cumsum(CON.new + CON.rep))) %>%
            mutate(stock_old_pcap = pmax(0, thres - gap_upd - stock_new_pcap)) %>%
            select(iso, grp, year, gap_upd, stock_new_pcap, stock_old_pcap)

          # # Add new/old stock information (for OP energy calculation) # m2/cap
          DF.rollout <<- DF.rollout %>%
            left_join(df.stock.cum)

          print("Rollout runs completed! - Housing - Unit:m2/cap")
        }
      }
    )
  )
