#' Class DLE.dimension.roads
#'
#' @description subclass for roads dimension (inheriting DLE.dimension)

DLE.dimension.roads <-
  setRefClass("DLE.dimension.roads",
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
        print("initialize: Roads")
        callSuper(...)

        # roads dimension requires additioal information to derive DLS thresholds by country/region.
        DF.DLS <<- data.frame(DF.DLS) # , df.input)
        # Replacement rate
        r.rep <<- 1 / 50 #  1/lifetime
      },

      #' @description
      #' Derive DLS thresholds from data if not pre-determined
      DeriveThreshold = function() {
        print("DeriveThreshold: Roads")

        DF.DLS <<- DF.DLS %>% mutate(thres = as.numeric(NA)) # target: no road gaps left
        DF.DLS <<- DF.DLS %>% mutate(thres_road_dens = 1.5) # Threshold [km/km2]: length of paved roads on arable land
      },

      #' @description
      #' Overwrite the same-named function in the superclass to be specific to this dimension
      IdentifyGap = function() {
        print("IdentifyGap: Roads")

        fname_land <- "/API_AG.LND.ARBL.HA_DS2_en_excel_v2_1003073.xls" # File: arable land data
        # fname_road <- "/data_road_processed.csv" # File: processed road data (based on CIA. Data refer to different years)
        fname_road <- "data_road_processed.csv" # File: processed road data (based on CIA and a few other sources; see decent\data-raw\dls_data\roads_paved_and_unpaved_km. last updated: 2023.09.05. Data refer to different years)
        year <- year.base # year for data to be loaded (only valid for arable land data)



        # Load data: arable land
        arable.land <- read_excel(paste0(data.path, "/Roads", fname_land), sheet = "Data", skip = 3, col_names = TRUE)
        arable.land <- arable.land %>%
          select_at(c("Country Code", paste(year))) %>% # Extract data for the base year
          rename_at(paste(year), list(~ paste("land"))) %>%
          rename(iso = "Country Code") %>%
          mutate(land = land / 100) # convert Ha to km2

        # Load data: road network length
        roads.data <- vroom(here("data-raw", "dls_data", "roads_paved_and_unpaved_km", fname_road)) %>%
          select(iso,total,paved,unpaved)

        # # Define function: manipulate subsets of data with dplyr
        # mutate_cond = function(.data, condition, ..., envir = parent.frame()) {
        #   condition <- eval(substitute(condition), .data, envir)
        #   .data[condition, ] <- .data[condition, ] %>% mutate(...)
        #   .data
        # }


        # Join data and calculate gap
        DF.DLS <<- DF.DLS %>%
          left_join(select(filter(pop, year == year.base), c(iso, population)), by = "iso") %>%
          left_join(roads.data, by = "iso") %>%
          left_join(arable.land, by = "iso") %>%
          mutate(total = as.numeric(total)) %>%
          mutate(road_dens = paved / land) %>%
          left_join(message.R11, by = "iso") %>%
          group_by(R11.region) %>%
          mutate(
            road_on_land_avg = mean(na.omit(total / land)), # Calculate regional averages - used when no other data available
            paved_on_total_avg = mean(na.omit(paved / total))
          ) %>%
          ungroup() %>%
          mutate_cond(is.na(land), land = 0) %>% # NA values for arable land are for provinces, e.g. France oversea departments - assumed zero
          mutate_cond(is.na(total), total = round(road_on_land_avg * land, 0)) %>% # fill missing total roads values
          mutate_cond(is.na(paved), paved = round(paved_on_total_avg * total, 0)) %>%
          mutate_cond(is.na(unpaved), unpaved = total - paved) %>%
          mutate(gap1 = unpaved) %>% # First gap definition: unpaved roads
          mutate(gap2 = as.numeric(NA)) %>% # Second gap definition: based on road density
          mutate(gap_tot = as.numeric(NA)) %>% # Gap: average between the two definitions [total km]
          mutate_cond(!is.na(road_dens) & !is.na(land) & road_dens < thres_road_dens, # Calculate gap2 when road density < threshold
            gap2 = land * (thres_road_dens - road_dens)
          ) %>%
          mutate_cond(!is.na(road_dens) & !is.na(land) & road_dens >= thres_road_dens, # gap2 = 0 when road density >= threshold
            gap2 = 0
          ) %>%
          mutate_cond(!is.na(gap1) & !is.na(gap2), # Calculate gap (average between gap1 and gap2)
            gap_tot = rowMeans(cbind(gap1, gap2))
          ) %>%
          mutate_cond(!is.na(gap1) & is.na(gap2), gap_tot = gap1) %>% # Assume gap as gap1 if gap2 is NA
          mutate(gap = gap_tot / population) %>% # Gap [km/cap]
          mutate(stock_exist_cap = paved / population) %>%
          mutate(thres = stock_exist_cap + gap) %>% # add derived threshold per country
          # mutate(stock_exist = paved)
          select(-population)
      },

      #' @description
      #' Derive (or import) final energy intensities from data if not pre-determined
      DeriveEnergyIntensity = function() {
        print("DeriveEnergyIntensity: Roads")

        fname_en_int <- "/en_int.xlsx" #
        data_en_int <- read_excel(paste0(data.path, "/Roads", fname_en_int), sheet = "en_int", col_names = TRUE) %>%
          # project EI of CON.new on CON.new.pop and CON.new.dls
          add_ei_for_CONnewDLSPOPsplit()


        DF.tei <<- DF.tei %>%
          select(-e.int) %>% # Delete column to be overriden with data from excel
          # mutate_cond(e.int = ifelse(type=="OP", 0, 4.939e6)) # average intensities [MJ/km road] (elec+others)
          left_join(data_en_int)
      },

      #' @description
      #' Construct rollout scenario based on identified gap and other assumptions.
      #' This can be coded differently for different scenarios.
      ConstructRolloutScenario = function(scen, yr.tgt = year.target,
                                          income.var = "log.gdp.pcap",
                                          method = "linear",
                                          nquants = 4,
                                          yr.st.ro=year.base) {
        # callSuper(scen) ## this populates columns in DF.rollout

        # prepare population data
        pop.scen <- pop %>%
          filter(year >= year.base & year <= year.end) %>%
          select(-c(population.urb, population.rur, urb.rate, hh_size, n_hh)) # keep pop total only


        if (
          scen == "Income"
        ) {
          print("ConstructRolloutScenario:  Roads - Income")

          # gap is pcap in DF
          df.inc <- DF.DLS %>%
            left_join(pov.gap %>% select(iso, year, r.diff), by = c("iso")) %>%
            group_by(iso) %>%
            mutate(inc = gap * r.diff / timestep) %>% # Divide by timestep to get ANNUAL average increment (to be consistent with ACCEL case)
            filter(!is.na(r.diff)) %>%
            select(iso, year, inc)

          df.stock <- DF.DLS %>%
            left_join(df.inc) %>%
            select(iso, year, gap, inc, stock_exist_cap, unit) # linear incremental per year # add "stock_exist"as well to keep track of existing stock

          DF.rollout <<- DF.rollout %>%
            left_join(message.R11, by = "iso") %>%
            left_join(rename(select(filter(pop, year == year.base), c(iso, population)), pop_baseyr = population), by = "iso") %>% # Add population for the base year (to rescale per cap results)
            left_join(pop, by = c("iso", "year")) %>% # Add population projection (need to have by = c('iso', 'year') with proper projection)
            left_join(df.stock, by = c("iso", "year")) %>%
            # group_by(iso, grp, type) %>%
            group_by(iso, type) %>%
            # mutate(rollout = as.numeric(rollout)) %>%
            mutate(rollout.pcap = as.numeric(NA)) %>%
            mutate(type = as.character(type)) %>%
            # mutate(grp = "NA") %>%
            # CON.new : annual new consstruction per capita
            # OP : total DLE stock per capita
            # CON.rep : fixed % of OP (r.rep)

            # we're not increasing the road network goal with higher population, but rather basing it on the absolute threshold
            # - CON.new: increase at a constant rate for absolute buildout, so adjust the per capita rollout by population change
            mutate_cond(type %in% c("CON.new", "CON.new.dls") & year <= yr.tgt,
                        rollout.pcap = lag(inc) * pop_baseyr / population) %>%
            mutate_cond(type %in% c("CON.new", "CON.new.dls") & (year > yr.tgt | year == year.base),
                        rollout.pcap = 0) %>%
            mutate_cond(type == "CON.new.pop",
                        rollout.pcap = 0) %>%


            mutate_cond(type == "CON.rep", rollout.pcap = r.rep * (stock_exist_cap + pmin(lag(cumsum(inc)) * timestep, gap)) * pop_baseyr / population) %>%
            mutate_cond(type == "CON.rep" & year == year.base, rollout.pcap = r.rep * (stock_exist_cap) * pop_baseyr / population) %>%
            mutate_cond(type == "OP", rollout.pcap = (stock_exist_cap + pmin(lag(cumsum(inc)) * timestep, gap)) * pop_baseyr / population) %>% # total stock (existing + new to fill the gap)
            mutate_cond(type == "OP" & year == year.base, rollout.pcap = stock_exist_cap * pop_baseyr / population) %>% # existing stock
            mutate(rollout = rollout.pcap * population) %>%
            select(iso, R11.region, population, type, year, gap, rollout, rollout.pcap, unit)
        } else if (
          scen == "Income.regression"
        ) {
          print("ConstructRolloutScenario:  Roads - Income Regression")


          # get df.inc separately .. (we should prevent duplication of code here) ..
          df.for.regression.all <-
            pov.gap %>%
            select(iso, year, pov.pcap) %>%
            left_join(
              gdp %>% select(iso, year, gdp.pcap) %>% mutate(log.gdp.pcap = log(gdp.pcap)),
              by = c("iso", "year")
            ) %>%
            filter(year != 2010) %>%
            left_join(DF.DLS %>% select(iso, grp, gap),
              by =
                c("iso")
            ) %>%
            mutate(gap = ifelse(gap == "NaN", NA, gap)) %>%
            drop_na(gap) # drop those with no gap, as rollout for those is not important.


          # start loop over grp.dim here.
          df.for.regression.all.grpdim <-
            df.for.regression.all %>%
            mutate(grp.dim = paste0(grp))

          df.new.inc.all <-
            NULL # for binding back together


          ## SIMPLE REGRESSION
          for (gd in unique(df.for.regression.all.grpdim$grp.dim)) {
            # step 2: do fit based on 2015 regression (and save coefficient for later projections)
            f <-
              paste0("gap ~ ", income.var) # formula
            df.find.pred <-
              df.for.regression.all.grpdim %>%
              filter(year == year.base) %>%
              filter(grp.dim ==
                gd)
            # get fit and coefficient
            if (method == "linear") {
              fit <- do.call(
                "lm",
                list(
                  as.formula(f),
                  data = as.name("df.find.pred")
                )
              )
              ft <- fit
              icpt <-
                fit[[1]][1]
              coeff <- fit[[1]][2]
            }

            # step 3: get increase in DLS = c1 * Delta[!!sym(income.var)] -- not annual yet.
            df.new.inc <-
              df.for.regression.all.grpdim %>%
              filter(grp.dim == gd) %>%
              mutate(c1 = coeff[1]) %>%
              mutate(delta.income = (!!sym(income.var) - lead(!!sym(income.var),
                default =
                  tail(!!sym(income.var), 1)
              ))) %>%
              mutate(delta.gap := c1 * delta.income) %>%
              mutate(delta.gap = pmax(0, delta.gap))

            # step 4: bind grp.dim together
            df.new.inc.all <-
              df.new.inc.all %>%
              bind_rows(df.new.inc)
          }

          # step 5: ensure consistency with simple income rollout scenario
          df.inc <- DF.DLS %>%
            left_join(df.new.inc.all %>% select(iso, grp, year, delta.gap),
              by = c("iso", "grp")
            ) %>%
            group_by(iso) %>%
            mutate(inc = delta.gap / timestep) %>% # Divide by timestep to get ANNUAL average increment (to be consistent with ACCEL case)
            filter(!is.na(gap)) %>%
            select(iso, year, inc)

          # take stock tracking and rollout code from simple income tracking scenario
          df.stock <- DF.DLS %>%
            left_join(df.inc) %>%
            select(iso, year, gap, inc, stock_exist_cap, unit) # linear incremental per year # add "stock_exist"as well to keep track of existing stock

          DF.rollout <<- DF.rollout %>%
            left_join(message.R11, by = "iso") %>%
            left_join(rename(select(filter(pop, year == year.base), c(iso, population)), pop_baseyr = population), by = "iso") %>% # Add population for the base year (to rescale per cap results)
            left_join(pop, by = c("iso", "year")) %>% # Add population projection (need to have by = c('iso', 'year') with proper projection)
            left_join(df.stock, by = c("iso", "year")) %>%
            # group_by(iso, grp, type) %>%
            group_by(iso, type) %>%
            # mutate(rollout = as.numeric(rollout)) %>%
            mutate(rollout.pcap = as.numeric(NA)) %>%
            mutate(type = as.character(type)) %>%
            # mutate(grp = "NA") %>%
            # CON.new : annual new constsruction per capita
            # OP : total DLE stock per capita
            # CON.rep : fixed % of OP (r.rep)

            # we're not increasing the road network goal with higher population, but rather basing it on the absolute threshold
            # - CON.new: increase at a constant rate for absolute buildout, so adjust the per capita rollout by population change
            mutate_cond(type %in% c("CON.new", "CON.new.dls") & year <= yr.tgt,
                        rollout.pcap = lag(inc) * pop_baseyr / population) %>%
            mutate_cond(type %in% c("CON.new", "CON.new.dls") & (year > yr.tgt | year == year.base),
                        rollout.pcap = 0) %>%
            mutate_cond(type == "CON.new.pop",
                        rollout.pcap = 0) %>%

            mutate_cond(type == "CON.rep", rollout.pcap = r.rep * (stock_exist_cap + pmin(lag(cumsum(inc)) * timestep, gap)) * pop_baseyr / population) %>%
            mutate_cond(type == "CON.rep" & year == year.base, rollout.pcap = r.rep * (stock_exist_cap) * pop_baseyr / population) %>%
            mutate_cond(type == "OP", rollout.pcap = (stock_exist_cap + pmin(lag(cumsum(inc)) * timestep, gap)) * pop_baseyr / population) %>% # total stock (existing + new to fill the gap)
            mutate_cond(type == "OP" & year == year.base, rollout.pcap = stock_exist_cap * pop_baseyr / population) %>% # existing stock
            mutate(rollout = rollout.pcap * population) %>%
            select(iso, R11.region, population, type, year, gap, rollout, rollout.pcap, unit)
        }
        # else if (scen=="ACCEL") {
        else {
          print(paste("ConstructRolloutScenario: Base -", scen, "by", yr.tgt, "starting from", as.character(yr.st.ro)))
          # print(paste0("Rollout unit: ", unit_rollout))

          if (yr.st.ro == year.base){
            # scenario as in ERL2021: start from year.base, with closing of the gap starting directly year.base

            n.yr.forrollout <- yr.tgt - year.base # year left until the gap is filled, calculated from the start of the simulation

            df.inc <- DF.DLS %>% mutate(inc = gap / n.yr.forrollout) # amount of unit filled in a certain timestep for the gap (per capita, because gap is define per capita)

          } else if (yr.st.ro > year.base){
            # scenario: start from year.base, but closing of the gap only starts to take place from year.start.rollout (yr.st.ro)

            n.yr.forrollout <- yr.tgt - yr.st.ro # years for filling the gap, calculated from the start of the rollout

            df.inc <- DF.DLS %>% mutate(inc = gap / n.yr.forrollout)

          } else {
            log_error("This scenario does not work. Did you set a DLS rollout year that is before the base year?")
          }

          df.stock <- df.inc %>% # inc (km/cap/yr)
            select(iso, gap, inc, stock_exist_cap, unit) # linear incremental per year # add "stock_exist"as well to keep track of existing stock


          print("Running rollout - Roads - Unit:abs")
          # print(head(DF.rollout))
          print(head(DF.rollout))
          DF.rollout <<- DF.rollout %>%
            left_join(message.R11, by = "iso") %>%
            left_join(rename(select(filter(pop, year == year.base), c(iso, population)), pop_baseyr = population), by = "iso") %>% # Add population for the base year (to rescale per cap results)
            left_join(pop, by = c("iso", "year")) %>% # Add population projection (need to have by = c('iso', 'year') with proper projection)
            left_join(df.stock, by = "iso") %>%
            # group_by(iso, grp, type) %>%
            group_by(iso, type) %>%
            # mutate(rollout = as.numeric(rollout)) %>%
            mutate(rollout.pcap = as.numeric(NA)) %>%
            mutate(type = as.character(type)) %>%
            # mutate(grp = "NA")  %>%
            mutate_cond(year<yr.st.ro, inc=0) %>% # set rollout speed to zero before the start of the rollout
            # CON.new : annual new constsruction per capita
            # OP : total DLE stock per capita
            # CON.rep : fixed % of OP (r.rep)

            # we're not increasing the road network goal with higher population, but rather basing it on the absolute threshold
            # - CON.new: increase at a constant rate for absolute buildout, so adjust the per capita rollout by population change
            mutate_cond(type %in% c("CON.new", "CON.new.dls") & year <= yr.tgt,
                        rollout.pcap = lag(inc) * pop_baseyr / population) %>%
            mutate_cond(type %in% c("CON.new", "CON.new.dls") & (year > yr.tgt | year == year.base),
                        rollout.pcap = 0) %>%
            mutate_cond(type == "CON.new.pop",
                        rollout.pcap = 0) %>%

            mutate_cond(type == "CON.rep", rollout.pcap = r.rep * (stock_exist_cap + pmin(lag(cumsum(inc)) * timestep, gap)) * pop_baseyr / population) %>%
            mutate_cond(type == "CON.rep" & year == year.base, rollout.pcap = r.rep * (stock_exist_cap) * pop_baseyr / population) %>%
            mutate_cond(type == "OP", rollout.pcap = (stock_exist_cap + pmin(lag(cumsum(inc)) * timestep, gap)) * pop_baseyr / population) %>% # total stock (existing + new to fill the gap)
            mutate_cond(type == "OP" & year == year.base, rollout.pcap = stock_exist_cap * pop_baseyr / population) %>% # existing stock
            mutate(rollout = rollout.pcap * population) %>%
            select(iso, R11.region, population, type, year, gap, rollout, rollout.pcap, unit)
          # select(iso, R11.region, population, grp, type, year, gap, rollout, rollout.pcap, unit)
          print(head(DF.rollout))
        }
      }
    )
  )
