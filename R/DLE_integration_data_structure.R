# DLE.dimension class definition ====

#' Class DLE.dimension
#'
#' @description class representing each dimension of DLE
#'
#' @details Each dimension needs to inherit DLE.dimension to have dimension-specific methods or fields. Data frame fields are in long format for easier joins.

DLE.dimension <- setRefClass("DLE.dimension",
  fields = list(

    # DLS name of this dimension
    name_dim = "character", # e.g. nutrition, water, etc

    # DLS indicator of this dimension
    indicator = "character", # e.g. pkm/yr, kcal/day, kg/year, etc

    # Dimension-specific sub-group
    grp = "character", # e.g. urban/rural or adult/child

    # Dimension-specific unit for rollout
    unit_rollout = "character", # "cap", "hh", "abs"

    # Master DF keeping DLS thresholds, gaps, and likely some dimension-specific data
    # In this base definition, per-capita threshold and gap are assume, but each dimension can diverge based on data.
    #' @field iso
    #' @field grp
    #' @field thres
    #' @field gap
    #' More dimension-specific fields can be here
    DF.DLS = "data.frame", # by [grp, region] : 'grp' - dimension-specific sub-group (e.g. urban/rural or adult/child)
    DF.DLS.base = "data.frame", # Keep the copy of base DF for sensitivity recalculation

    # How we fill the gap over years
    # I assume this rollout already takes into account region-wide total rollout. (NOT per capita)
    #' @field iso
    #' @field type
    #' @field grp
    #' @field year
    #' @field population optional
    DF.rollout = "data.frame", # by [grp, region, year]
    DF.rollout.base = "data.frame", # Keep the copy of base DF for sensitivity recalculation

    # Indirect final energy intensity - Megajoule by unit of indicator (e.g. MJ/kcal)
    # This can also have a year dimension to accommodate time-varying intensities.
    #' @field iso
    #' @field type
    #' @field elec whether the intensity is for elec/non.elec/total.
    #' @field grp
    #' @field year Optional
    DF.tei = "data.frame", # by [region, type (CON/OP), grp, (year)] : long format (may have elec/non.elec breakdown)
    DF.tei.base = "data.frame", # Keep the copy of base DF for sensitivity recalculation

    # Derived DLE pathway of corresponding scenario (region/nation total)
    DLE.tot = "data.frame", # Most granular geographic scale
    DLE.agg = "data.frame" # Harmonized MESSAGE R11 region

    # These are not defined as global vars.
    # # End of the projection year
    # year.end = "numeric",
    #
    # # Specify a common data path
    # data.path = "character"
  ),


  # Many of these methods will need to be customized (through inheritance) to individual dimensions, while some already defined below will be standard.
  methods = list(
    initialize = function(..., unit_rollout = "cap") { # One way of setting up default value for a field (any better way?)

      print("initialize: Base")

      callSuper(..., unit_rollout = unit_rollout) # Run the method in the super-class first.

      iso <- message.R11$iso # ISO3 for the countries included in MESSAGE R11 definition
      # iso <- codelist$iso3c[!is.na(codelist$iso3c)]  # All countries' ISO3

      if (length(grp) == 0) {
        grp <<- "NA"
      }

      # Skeleton data frame for threshold and gap
      countries <- list(iso = iso, grp = grp)
      DF.DLS <<- data.frame(expand.grid(countries) %>% arrange(iso), # Created all combinations of country & group
        thres = NA,
        gap = NA,
        unit = indicator
      )
      DF.DLS.base <<- DF.DLS # Keep the base DF for sensitivity recalculation

      # Skeleton data frame for energy intensity (MJ/unit) with elec/non.elec divide
      countries$type <- c("OP", "CON.new", "CON.rep",
                          "CON.new.pop", "CON.new.dls")
      countries$elec <- c("elec", "non.elec", "total")
      DF.tei <<- data.frame(expand.grid(countries), e.int = NA) %>% arrange(iso, grp, type)
      DF.tei.base <<- DF.tei

      # Skeleton data frame for rollout
      # nextdecade <- ((as.integer(format(Sys.Date(), "%Y")) %/% 10)+1)*10 # Starting decade of the projection horizon
      # nextdecade <- (as.integer(format(Sys.Date(), "%Y")) %/% 10)*10 # Starting decade of the projection horizon
      horizon <- seq(year.base, year.end, timestep) # 5-yr step following SSP population

      countries$year <- horizon
      countries$elec <- NULL # No need for elec breakdown for rollout info
      DF.rollout <<- data.frame(expand.grid(countries), rollout = NA) %>% arrange(iso, grp, type, year)
      DF.rollout.base <<- DF.rollout # Keep the base DF for sensitivity recalculation
    },

    #' @description
    #' Derive DLS thresholds from data if not pre-determined
    DeriveThreshold = function() {
      print("DeriveThreshold: Base")
    },

    #' @description
    #' Identify DLS gap if not given
    IdentifyGap = function() {
      print("IdentifyGap: Base")
    },

    #' @description
    #' Derive (or import) final energy intensities from data if not pre-determined
    DeriveEnergyIntensity = function() {
      print("DeriveEnergyIntensity: Base")
    },

    #' @description
    #' Extract total (direct+indirect) final energy intensity from EXIO 2015 for all 49 regions (ISO2)
    #' This will be used for four of our dimensions.
    #' i.e. dimname is one of ("Health", "Education", "Nutrition", "Clothing").
    #' @field dimname name of sector
    #' @field mode Intensity (MJ/USD) or Total (EJ)
    #' Good for now for health and education use
    ImportEXIOFinalEnergy = function(dimname = "Health", mode = "Intensity") {
      print("ImportEXIOFInalEneInt: Base")

      load(paste0(data.path, "/IO/EXIO3_energy_ext_2015.Rdata")) # loads a list called EXIO3_data
      load(paste0(data.path, "/IO/EXIO3_sectors_regions.Rdata")) # loads a list called EXIO3_sect_reg

      tfeis <- c("tfei.exio", "tfei.elec", "tfei.non.elec")

      sect_list <- EXIO3_sect_reg$EXIO_sector
      sect_idx <- list(
        "Health" = 175,
        "Education" = 174,
        "Nutrition" = c(2:4, 14, 43:53), # grains, veg/fruit, raw milk, processed food
        "Clothing" = 56:57 # all leather included
      ) # which(grepl(dimname, sect_list))

      int.all <- list()
      for (i in tfeis) {
        tfei <- EXIO3_data[[i]]
        type.int <- case_when(
          i == "tfei.exio" ~ "total",
          i == "tfei.elec" ~ "elec",
          i == "tfei.non.elec" ~ "non.elec"
        )

        # sect_idx_seq <- seq(sect_idx, dim(tfei)[2], length(sect_list))
        sect_idx_seq <- as.vector(sapply(seq(0, dim(tfei)[2] - length(sect_list), length(sect_list)),
          function(x) x + sect_idx[[dimname]],
          simplify = "array"
        ))
        tfei.sect <- tfei[, sect_idx_seq]
        # out <- tfei.sect * usd2eur.baseyr
        # Weight average of intensities by FD (if sect_idx is a single value, this is identical to 'tfei.sect * usd2eur.baseyr')
        out <- (tfei.sect %*% EXIO3_data$EXIO3_FD.hh[sect_idx_seq, ]) %*%
          diag(1 / colSums(EXIO3_data$EXIO3_FD.hh[sect_idx_seq, ])) * usd2eur.baseyr
        val.unit <- "MJ/USD (base year MER)"

        # In 'Total' mode, multiply the final demand (EUR) to get EJ
        if (mode == "Total") {
          df.ene <- tfei.sect %*% EXIO3_data$EXIO3_FD.hh[sect_idx_seq, ] / exa * tera # TJ to EJ
          out <- df.ene
          val.unit <- "EJ/year"
        }

        # Specify EXIO region names in ISO3
        regname <- countrycode(EXIO3_sect_reg$EXIO_region, "iso2c", "iso3c") # apply EXIO regions to the output as columns
        n_reg <- length(regname)
        regname[(n_reg - 4):n_reg] <- c("ASA", "LAM", "EUR", "AFR", "MEA") # Make up names for aggregate regions

        # Raw (total) intensities from EXIO 49 regions
        tei.EXIO <- data.frame(
          iso = regname,
          val.a = colSums(out)
        )

        # Only for 'intensity' mode, tei values to use for missing regions (representative countries or mean of WEU/EEU)
        tei.sub <- message.R11 %>%
          left_join(tei.EXIO) %>%
          filter(iso %in% c("IDN", "CHN", "IND", "RUS", "USA", "AUS") | R11.region %in% c("WEU", "EEU")) %>%
          filter(!is.na(val.a)) %>%
          group_by(R11.region) %>%
          summarise(val.sub = mean(val.a))

        tei.MSG <- message.R11 %>%
          left_join(tei.EXIO) %>%
          left_join(tei.EXIO %>% rename(R11.region = iso, val.b = val.a)) %>% # fill in LAM, AFR, MEA except BRA/MEX/ZAF
          mutate(val = coalesce(val.a, val.b)) %>%
          left_join(tei.sub) %>%
          mutate(val = coalesce(val, val.sub)) %>%
          select(-starts_with("val.")) %>%
          mutate(type = "OP", elec = type.int, unit = val.unit) # Assumes EXIO is used only for "OP"

        # If in 'Total' mode, just return results by 49 EXIO regions
        if (mode == "Total") {
          int.all[[i]] <- tei.EXIO %>%
            rename(val = val.a) %>%
            mutate(type = "OP", elec = type.int, unit = val.unit)
        } else if (mode == "Intensity") {
          int.all[[i]] <- tei.MSG
        }
      }

      tei <- do.call("rbind", int.all)

      return(tei) # returns MJ/USD (2015)
    },

    #' @description
    #' This is for a more generic internal use. Use AggregateResult2MESSAGE.R11 for DLE result aggregation.
    #' Map the results by iso3 to MESSAGE regions
    #' By default, it assumes a weighted average value weighted by population.
    #' @field df data frame to do the regional aggregation on
    #' @field var variable name to aggregate
    AggregateRegion = function(df, summary_vars, weight = population, func = "mean", ...) {
      if (func == "mean") {
        df <- df %>%
          left_join(message.R11, by = "iso") %>%
          ungroup() %>%
          left_join(pop %>% filter(year == year.base) %>% select(-year), by = "iso") %>% # pop info is already there from DF.rollout
          # select(R11.region, matches("grp|type|year|elec"), {{weight}}, {{summary_vars}}) %>%
          group_by_at(vars(R11.region, matches("grp|type|year|elec"))) %>%
          summarise(across({{ summary_vars }}, ~ weighted.mean(., w = {{ weight }}, na.rm = TRUE)))
      } else if (func == "sum") {
        df <- df %>%
          left_join(message.R11, by = "iso") %>%
          ungroup() %>%
          group_by_at(vars(R11.region, matches("grp|type|year|elec"))) %>%
          summarise(across({{ summary_vars }}, ~ sum(., na.rm = TRUE)))
      }
      return(df)
    },

    #' @description
    #' This is specifically for DLE result (DLE.tot) aggregation.
    AggregateDLEoverRegion = function() {
      if ("R11.region" %in% colnames(DLE.tot)) {
        DLE.tot <<- DLE.tot
      } else {
        DLE.tot <<- DLE.tot %>%
          left_join(select(message.R11, -country_name), by = "iso")
      }

      DLE.agg <<- DLE.tot %>%
        ungroup() %>%
        # left_join(pop, by=c("iso", "year")) %>% # pop info is already there from DF.rollout
        select(R11.region, grp, type, elec, year, population, DLE) %>%
        group_by(R11.region, grp, type, elec, year) %>%
        summarise(DLE = sum(DLE, na.rm = TRUE), population = sum(population, na.ra = TRUE)) %>%
        mutate(DLE.pcap = DLE / population * exa / giga)

      return(DLE.agg)
    },
    FillRolloutDF = function(df.inc, df.rollout, yr.tgt = year.target, r.rep = 0.05, r.rep.grp = FALSE, only.OP = FALSE, urbrur = FALSE, year.start.rollout = year.base, ... # note: yr.st.ro is also passed through to this function but are not actually used in this function currently
                             ) {
      print(paste("FillRolloutDF:", name_dim, ", urbrur =", urbrur))
      df <- df.rollout %>% left_join(pop, by = c("iso", "year")) # Add population projection (need to have by = c('iso', 'year') with proper projection)

      # Modify population if urb/rur grouping
      if (urbrur) {
        df <- df %>% mutate(population = ifelse(grp == "urban", population.urb, population.rur))
      }
      # Modify increment rollout if the starting year is later
      if (
        year.start.rollout==year.base
      ){
        df.with.inc <- df %>%
          left_join(df.inc)
      } else if (
        year.start.rollout>year.base
      ){
        df.with.inc <- df %>%
          left_join(df.inc) %>%
          mutate_cond(year<=year.start.rollout, inc=0) # set rollout speed to zero before the start of the rollout
      }


      # dle & dle.pcap for operation (and subsequently con.rep) should be allowed to go down with population decline or lower thresholds
      df.rollout.op <- df.with.inc %>%
        group_by(iso, grp, type) %>%
        # rollout.pcap defintions (but depending on dimensions, only some will be useful. e.g. clothing: no CON)
        # CON.new : annual new construction per capita
        # CON.new.pop : annual new construction per capita, for growing population size (at the same national DLS achievement level)
        # CON.new.dls : annual new construction per capita, for an increase in DLS provisioning (including marginal from population increase)
        #' Let t = time, P = population, and A = DLS achievement
        #' CON.new = P_t*A_t - P_{t-1}*A_{t-1}
        #' CON.new.pop = (P_t - P_{t-1})*A_{t-1}
        #' CON.new.dls = (A_t - A_{t-1})*P_t
        # OP : total DLE stock per capita
        # CON.rep : fixed % of OP (r.rep)


        # N.B. cannot use `gap` column directly under the CON.new calculation, because it is not properly changed over time.
        # -> so we add a calculation here with an extra column, used a few code chunks below
        mutate(gap.tracking.in.rollout =
          pmax(
            gap - cumsum(inc) * timestep,
            0
          )
        ) %>%

        mutate(rollout.pcap = ifelse(((type == "OP" | type == "CON.rep") & year < yr.tgt),
          pmin(lag(cumsum(inc), default = 0) * timestep, gap) + (thres - gap), # linear filling of the gap, speed determined by gap in 2015 (when initiating df.inc)
          ifelse(((type == "OP" | type == "CON.rep") & year >= yr.tgt),
            thres, # after the gap is filled
            as.numeric(NA)
          )
        ))

      # < CON.rep >
      # renovation/replacement energy, determined by current stocks and replacement rate (at time t)
      df.rolloutop.con_rep <- df.rollout.op %>%
        mutate(rollout.pcap = ifelse(type == "CON.rep",
          ifelse(r.rep.grp,
            # take the stock in the current timestep for replacement [?] -> or should we take average with previous timestep `(lag(rollout.pcap, default=0) + rollout.pcap)/2`? or something more advanced?
            rollout.pcap * r.rep.pergrp, # replacement rate per group (N.B. this could in principle also simply be called r.rep as column in DF.rollout, but perhaps making it explicit that r.rep is sometimes a scalar, and sometimes a vector is more clear)
            rollout.pcap * r.rep
          ), # replacement rate for all groups
          rollout.pcap
        ))


      # fill rollout.pcap for CON.new variables
      df.rollout <- df.rolloutop.con_rep %>% # con new should account for increase in both the stock when filling gap and for the increase in population for construction


        # < CON.new >
        # - zero population means zero construction energy (to catch unlikely cases)
        mutate_cond((type == "CON.new" & population == 0), rollout.pcap=0) %>%
        # - phase after rollout: only population growth
        #     * (this is simplified: only if maximum rollout is reached, this is at the level of the threshold, because the gap is zero
        #        in any case that does not have full closure by a certain yr.tgt, we should have `yr.tgt=Inf`)
        mutate_cond((type == "CON.new" & year >= yr.tgt), rollout.pcap=(
          # no negative construction energy possible for population growth
          pmax(0,
               (
                 population - lag(population, default = NA_real_) # change in population
               ) * (
                 (lag(thres, default = NA_real_) - lag(gap.tracking.in.rollout, default = NA_real_)) # achievement in previous timestep
               ) / (
                 population # per capita
               ) / (
                 timestep # per year
               )
          )
        )) %>%
        # - rollout (gap-closing) phase: DLS provisioning and population growth
        # note that here we "overwrite" the yr.tgt entry. We include (some wrong values) in the previous mutate_cond() call, because otherwise the lag() function there does not see yr.tgt, and therefore would produce NA for yr.tgt+timestep
        mutate_cond((type == "CON.new" & year <= yr.tgt), rollout.pcap=(
          # population growth +
          # -> population growth
          # no negative construction energy possible for population growth
          pmax(0,
               (
                 population - lag(population, default = NA_real_) # change in population
               ) * (
                 (lag(thres, default = NA_real_) - lag(gap.tracking.in.rollout, default = NA_real_)) # achievement in previous timestep
               ) / (
                 population # per capita
               ) / (
                 timestep # per year
               )
          ) +
          # -> DLS achievement
          # no negative construction energy possible for dls achievement
          pmax(0,
               (
                 # change in achievement
                 (
                   (thres - gap.tracking.in.rollout) # population in new (current) timestep
                 ) - (
                   (lag(thres, default = NA_real_) - lag(gap.tracking.in.rollout, default = NA_real_)) # population in previous timestep
                 )
               ) * (
                 population # population in new (current) timestep
               ) /  (
                 population # per capita
               ) / (
                 timestep # per year
               )
          )

        )) %>%

        # < CON.new.pop >
        mutate_cond((type == "CON.new.pop" & population == 0), rollout.pcap=0) %>%
        # - throughout the full period, only population growth:
        mutate_cond((type == "CON.new.pop"), rollout.pcap=(
          # no negative construction energy possible for population growth
          pmax(0,
               (
                 population - lag(population, default = NA_real_) # change in population
               ) * (
                 (lag(thres, default = NA_real_) - lag(gap.tracking.in.rollout, default = NA_real_)) # achievement in previous timestep
               ) / (
                 population # per capita
               ) / (
                 timestep # per year
               )
          )
        )) %>%

        # < CON.new.dls >
        mutate_cond((type == "CON.new.dls" & population == 0), rollout.pcap=0) %>%
        # - rollout (gap-closing) phase: DLS provisioning only
        mutate_cond((type == "CON.new.dls"), rollout.pcap=(
          # no negative construction energy possible for dls achievement
          pmax(0,
               (
                 # change in achievement
                 (
                   (thres - gap.tracking.in.rollout) # population in new (current) timestep
                 ) - (
                   (lag(thres, default = NA_real_) - lag(gap.tracking.in.rollout, default = NA_real_)) # population in previous timestep
                 )
               ) * (
                 population # population in new (current) timestep
               ) /  (
                 population # per capita
               ) / (
                 timestep # per year
               )
          )
        ))


      df.return <- df.rollout %>%
        mutate(rollout.pcap = ifelse(population==0, 0, rollout.pcap)) %>% # for construction to be zero if not already accounted for (for (a) if there's no population [e.g., rural population in Hong Kong/Macao/Singapore] ).
        mutate(rollout.pcap = ifelse(((type %in% c("CON.rep", "CON.new", "CON.new.pop", "CON.new.dls")) & only.OP), 0, rollout.pcap)) %>% # for construction to be zero if not already accounted for (for (a) if the dimension only has operational energy, and (b) if there's no population [e.g., rural population in Hong Kong/Macao/Singapore] ).
        # Impose rollout.pcap = 0 in CON.new for starting year
        mutate(rollout.pcap = ifelse(type %in% c("CON.new", "CON.new.pop", "CON.new.dls") & year == year.base, 0, rollout.pcap)) %>% # for construction to be zero if not already accounted for (generally, this should already have been accounted for, but it isn't always - due to NA as default in the lag() function )

        # Total rollout (whole population)
        mutate(rollout = case_when(
          unit_rollout == "hh" ~ rollout.pcap * n_hh,
          unit_rollout == "cap" ~ rollout.pcap * population,
          TRUE ~ rollout.pcap
        ))

      return(df.return)
    },

    #' @description
    #' Construct rollout scenario based on identified gap and other assumptions.
    #' This can be coded differently for different scenarios.
    #' This function is normally called by 'SetupRollout' in DLE.scenario.
    #' @field scen type of scenario this dimension will be part of
    #' @field yr.tgt target time of achieving DLE (Inf for Income)
    ConstructRolloutScenario = function(scen, yr.tgt = year.target, r.rep = 0.05,
                                        r.rep.grp = FALSE,
                                        only.OP = FALSE,
                                        urbrur = FALSE,
                                        income.var = "log.gdp.pcap",
                                        method = "linear",
                                        nquants = 4,
                                        year.start.rollout=2015, # for ACCCEL scenarios
                                        ...) {
      if (scen == "Income") {
        # For Income, for now we equate the gap closing rate for GDP with the rates for all the dimensions.
        print(paste("ConstructRolloutScenario: Base - Income, thres =", pov.thres))

        # gap is pcap in DF
        df.inc <- DF.DLS %>%
          left_join(pov.gap %>% select(iso, year, r.diff), by = c("iso")) %>%
          group_by(iso) %>%
          mutate(inc = gap * r.diff / timestep) %>% # Divide by timestep to get ANNUAL average increment (to be consistent with ACCEL case)
          filter(!is.na(thres)) %>%
          select(-r.diff)

        DF.rollout <<- FillRolloutDF(df.inc, DF.rollout, yr.tgt = Inf, r.rep, r.rep.grp, only.OP, urbrur, ...) # yr.tgt arbitrarily large
      } else if (
        scen == "Income.regression"
      ) {
        # For Income Regression, we built a quantile regression model for each gap, where we let the change in an income indicator
        #  drive the change in DLS provision, but mediated by their cross-sectional relationship in the base-year 2015.
        print(paste(
          "ConstructRolloutScenario: Base - Income regression"
        ))

        # input data:
        # - gdp (ssp) -> basic set up, global variable
        # - pov.gap (selected $, ssp) -> basic set up, global variable
        # - DF.DLS (specific dimension, according to rollout - for thres [which we do not need here - only in rollout] and gap)

        # optional arguments:
        # - qreg.method="linear" vs "non-parametric"
        # - if (non-parametric){lambda=num}
        # - income.var (options: pov.pcap, log.gdp.pcap, gdp.pcap, [combination perhaps later?])


        # step 1: create a dataframe with all possible options
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


        ## QUANTILE REGRESSION
        # for (gd in unique(df.for.regression.all.grpdim$grp.dim)) {
        #   # step 2: do 2015 regression to determine current performance
        #   #     to-do: find a better way to find what quantile of the non-parametric quantile regression an observation is on (e.g. predict.rq.bin https://rdrr.io/rforge/Qtools/man/predict.rq.bin.html )
        #   quants <- 1:nquants / (nquants + 1)
        #   quant.cols <-
        #     sapply(1:nquants, as.character)
        #
        #   # step 2.1: 2015 regression (and save results for later in the case of linear)
        #   f <-
        #     paste0("gap ~ ", income.var) # formula
        #   df.find.pred <-
        #     df.for.regression.all.grpdim %>% filter(year == year.base) %>% filter(grp.dim ==
        #                                                                             gd)
        #   # some preparations for linear model variables
        #   fits <- NULL
        #   intercepts <- NULL
        #   coeffs <- NULL
        #   for (i in 1:nquants) {
        #     # get fit for selected quantile
        #     if (method == "linear") {
        #       fit <-
        #         do.call("rq",
        #                 list(
        #                   as.formula(f),
        #                   data = as.name("df.find.pred"),
        #                   tau = as.numeric(quants[i])
        #                 ))
        #       fits[[i]] <- fit
        #       intercepts[[i]] <-
        #         fit[[1]][1]
        #       coeffs[[i]] <- fit[[1]][2]
        #     }
        #
        #     # for each country, add prediction based on this quantile as a new column to the dataframe
        #     pred <- df.find.pred %>%
        #       add_column(!!sym(quant.cols[i]) := predict(fit, df.find.pred)) %>%
        #       mutate(!!sym(quant.cols[i]) :=
        #                !!sym(quant.cols[i]) - gap)
        #     df.find.pred <-
        #       df.find.pred %>% left_join(pred)
        #     gc()
        #   }
        #   # step 2.2: select performance bin by selecting the column that is closest to the actual value as the q.performance
        #   df.performance <-
        #     df.find.pred %>%
        #     pivot_longer(cols = quant.cols,
        #                  names_to = "quantile",
        #                  values_to = "diff") %>%
        #     mutate(q.performance = as.numeric(NA)) %>%
        #     ungroup() %>% group_by(iso) %>%
        #     mutate(q.performance = ifelse(
        #       diff == min(abs(diff)) |
        #         diff == -min(abs(diff)),
        #       as.numeric(quantile),
        #       q.performance
        #     )) %>%
        #     drop_na(q.performance) %>%
        #     select(iso, q.performance) %>%
        #     distinct(iso, .keep_all = TRUE) # required to add in case there is no difference in the quantile predictions
        #
        #   # step 2.3: add quantile performance to the dataframe
        #   df.found.pred <-
        #     df.for.regression.all.grpdim %>% filter(grp.dim == gd) %>% left_join(df.performance)
        #
        #   # (step 3.alternative): (non-parametric) do in-line regression and prediction
        #   # [placeholder]
        #
        #   # step 3: get increase in DLS = c1 * Delta[!!sym(income.var)] -- not annual yet.
        #   df.new.inc <-
        #     df.found.pred %>% mutate(q.performance = as.numeric(q.performance)) %>%
        #     mutate(c1 = coeffs[q.performance]) %>%
        #     mutate(delta.income = (!!sym(income.var)-lead(!!sym(income.var), default =
        #                                                     tail(!!sym(income.var), 1)))) %>%
        #     mutate(delta.gap := c1 * delta.income) %>%
        #     mutate(delta.gap = pmax(0, delta.gap))
        #
        #   # step 4: bind grp.dim together
        #   df.new.inc.all <-
        #     df.new.inc.all %>%
        #     bind_rows(df.new.inc)
        #
        # }

        # step 5: ensure consistency with simple income rollout scenario
        df.inc <- DF.DLS %>%
          left_join(df.new.inc.all %>% select(iso, grp, year, delta.gap),
            by = c("iso", "grp")
          ) %>%
          group_by(iso) %>%
          mutate(inc = delta.gap / timestep) %>% # Divide by timestep to get ANNUAL average increment (to be consistent with ACCEL case)
          filter(!is.na(thres)) %>%
          select(-delta.gap)

        # pass on to rollout
        DF.rollout <<-
          FillRolloutDF(df.inc,
            DF.rollout,
            yr.tgt = Inf,
            r.rep,
            r.rep.grp,
            only.OP,
            urbrur,
            ...
          )
      } else {
        print(paste("ConstructRolloutScenario: Base -", scen, "starting from", as.character(year.start.rollout), "by", yr.tgt, "r.rep =", r.rep, "urbrur =", urbrur))


        # Calculate ANNUAL increment `inc`; in the ACCEL case, this is the 'speed' of filling the gap
        # NOTE: we don't specify that "inc=0" beyond the point of full achievement; the reason is that we (want to) already specify in the `FillRolloutDF` function; that the provisioning/service level cannot be higher than the threshold. If that changes, the lines below should be adapted.

        if (year.start.rollout==year.base){
          # scenario as in ERL2021: start from year.base, with closing of the gap starting directly year.base

          n.yr.forrollout <- yr.tgt - year.base # year left until the gap is filled, calculated from the start of the simulation

          df.inc <- DF.DLS %>% mutate(inc = gap / n.yr.forrollout) # amount of unit filled in a certain timestep for the gap (per capita, because gap is define per capita)

        } else if (year.start.rollout > year.base){
          # scenario: start from year.base, but closing of the gap only starts to take place from year.start.rollout

          n.yr.forrollout <- yr.tgt - year.start.rollout # years for filling the gap, calculated from the start of the rollout

          df.inc <- DF.DLS %>% mutate(inc = gap / n.yr.forrollout)

        } else {
          log_error("This scenario does not work. Did you set a DLS rollout year that is before the base year?")
        }

        DF.rollout <<- FillRolloutDF(df.inc, DF.rollout, yr.tgt, r.rep, r.rep.grp, only.OP, urbrur, year.start.rollout, ...)# UPDATE
      }
    },

    #' @description
    #' Update certain parameters
    #' @param params list of fields and values to be updated
    UpdateParameter = function(params) {
      lapply(
        names(params),
        function(nm) {
          if (nm %in% names(.self$getRefClass()$fields())) { # Only when 'nm' object is defined in .self environment
            print(paste("UpdateParameter:", nm, "exists in", name_dim, "and replaced by", params[[nm]]))
            assign(nm, params[[nm]], .self)
          }
        }
      )
    },

    #' @description
    #' Run sensitivity case (= UpdateParameter + UpdateDLE)
    #' @param params list of fields and values to be updated
    SensitivityRun = function(params, scen) {
      # Replace the DFs to the bases
      DF.DLS <<- DF.DLS.base
      DF.rollout <<- DF.rollout.base
      DF.tei <<- DF.tei.base

      UpdateParameter(params)

      # Each dimension can do more here. e.g. DeriveThreshold()
      DeriveThreshold()
      IdentifyGap()
      ConstructRolloutScenario(scen)
      DeriveEnergyIntensity()

      UpdateDLE()
    },


    #' @description
    #' Plot results in DLE.tot DF by R11 region
    #' Two plots: Total (EJ) and Per-capita (MJ/unit)
    PlotDim = function() {
      if (nrow(DLE.agg) == 0) {
        df.plot <- DLE.tot
      } else {
        df.plot <- DLE.agg
      }

      df <- df.plot %>%
        filter(elec != "total") %>%
        select(R11.region, grp, type, elec, year, DLE, DLE.pcap) %>%
        pivot_longer(cols = starts_with("DLE"), names_to = "cat", values_to = "val")

      p.tot <- ggplot(
        data = df %>% filter(cat == "DLE"),
        aes(
          x = year, y = val,
          fill = interaction(type, elec, lex.order = T),
          alpha = grp
        )
      ) +
        geom_area() +
        # scale_alpha_manual(values=c(0.5, 1)) +
        facet_wrap(~R11.region) +
        labs(
          title = paste(name_dim),
          y = DLE.tot$unit.DLE[1]
        )
      print(p.tot)
      ggsave(paste0(out.path, "/", name_dim, "_dle_tot.png"),
        width = 19, height = 19, units = "cm"
      ) # Save the last plot

      p.pcap <- ggplot(
        data = df %>% filter(cat == "DLE.pcap"),
        aes(
          x = year, y = val,
          fill = interaction(type, elec, lex.order = T),
          alpha = grp
        )
      ) +
        geom_area() +
        # scale_alpha_manual(values=c(0.5, 1)) +
        facet_wrap(~R11.region) +
        labs(
          title = paste(name_dim),
          y = DLE.tot$unit.DLE.pcap[1]
        )
      print(p.pcap)
      ggsave(paste0(out.path, "/", name_dim, "_dle_cap.png"),
        width = 19, height = 19, units = "cm"
      ) # Save the last plot
    },


    #' @description
    #' Plot global map with gap values
    PlotGapMap = function() {
      df.gap <- filter(DF.DLS) %>% select(iso, gap, grp)
      unit_dim <- DF.DLS$unit[1] # or the field 'indicator'

      grps <- unique(df.gap$grp)

      for (i in grps) {
        png(
          file = paste0(out.path, "/", name_dim, "_gap_dimension_", i, ".png"),
          bg = "white", height = 400, width = 600, unit = "mm", pointsize = 30, res = 300
        )

        # create a spatialpolygonsdataframe with a specific projection
        sPDF <- getMap()[-which(getMap()$ADMIN == "Antarctica"), ]
        sPDF <- spTransform(sPDF, CRS = CRS("+proj=robin +ellps=WGS84"))

        # filter data for the i-th grp
        df.gap.grp <- df.gap %>% filter(grp == i)

        # merge the dataframe with the projection
        polydata <- merge(sPDF, df.gap.grp, by.x = "ISO3", by.y = "iso")

        # plot the map
        map <- mapCountryData(polydata,
          catMethod = "pretty",
          mapTitle = paste(name_dim, "gap for", i, "[", unit_dim, "]"),
          nameColumnToPlot = "gap",
          # colourPalette="palette",
          lwd = 3,
          addLegend = "TRUE"
        )
        print(map)

        dev.off()
      }
    },

    #' @description
    #' (Re)Calculate energy values
    UpdateDLE = function() {
      # print(head(DF.rollout))
      DLE.tot <<- DF.rollout %>% # select(-unit) %>%
        left_join(DF.tei %>% select(-any_of(c("population")))) %>% # Some DF.tei (housing, cooling, heating and hotwater) contain a population column, causing joining troubles
        # e.int is asssumed to be supplied in MJ
        mutate(
          DLE = e.int * rollout / exa * mega,
          unit.DLE = "EJ/year"
        ) %>% # Convert to EJ
        mutate(
          DLE.pcap = ifelse(population > 0, DLE * exa / giga / population, 0),
          unit.DLE.pcap = "GJ/cap/year"
        )

      # If both 'iso' and 'R11.region' are present in the DF, remove 'R11.region' for proper aggregation in SumDLE().
      if (("iso" %in% names(DLE.tot)) & ("R11.region" %in% names(DLE.tot))) {
        DLE.tot <<- DLE.tot %>% select(-R11.region)
      }
    }
  )
)

# DLE.scenario class definition ####

#' Class DLE.scenario
#'
#' @description class representing one DLE scenario
#'
#' @details This class includes all DLS dimensions.


DLE.scenario <- setRefClass("DLE.scenario",
  fields = list(

    #' @field scenario Name of the scenario (e.g. Income, ACCEL, LCT, HIGH, etc.)
    scenario.name = "character",

    #' @field dims List of all DLS dimensions
    dims = "list",

    #' #' @field ssp which SSP this scenario borrows demographic assumptions
    #' ssp = "character",
    #'
    #' #' @field pop.scen demographic info from ssp above
    #' pop.scen = "character",

    #' #' @field pov.thres threshold for (income) poverty headcount which the gaps of this scenario will track [$/day] (1.9, 3.2, 5.5)
    #' pov.thres = "numeric",
    #'
    #' #' @field pov.gap dataframe having a timeseries of poverty gap closing at the given pov.thres
    #' pov.gap   = "data.frame",

    #' @field year.tgt.scen Target year for gap closure for this scenario (for normative scenarios)
    year.tgt.scen = "numeric",

    #' @field increase.public.transport.share Boolean that determines whether or not an alternative transport pathway is simulated
    lct = "logical",

    # Aggregate sectors (OP/CON)
    Shelter = "data.frame",
    Mobility.all = "data.frame",
    Appliance = "data.frame",
    Food = "data.frame",

    #' @field Identified gaps in one DF
    Gap.alldims = "data.frame",

    #' @field Energy intensities of all dimensions in one DF
    EI.alldims = "data.frame",

    #' @field DLE results for all dimensions (at their own geographical resolution)
    DLE.alldims = "data.frame",

    #' @field R11-aggregated DLE results for all dimensions
    DLE.alldims.agg = "data.frame",

    #' @field Dimensions grouped
    DLE.group.agg = "data.frame"
  ),
  methods = list(
    initialize = function(...) {
      callSuper(...)

      SetupThresholdandGap()
    },


    #' @description
    #' Set up thresholds and gaps for all dimensions
    SetupThresholdandGap = function() {
      # The scenario-specific rollout can be taken care of in each dimension.
      setup_threshold_and_gap_helper(dims=dims)
    },


    #' @description
    #' Set up rollout trajectory for a given scenario
    SetupRollout = function(lct = FALSE, year.start.rollout=2015) {
      # The scenario-specific rollout can be taken care of in each dimension.
      lapply(
        names(dims),
        function(x) {
          if (x %in% c("cooling_op")) {
            print("SetupRollout: Special treatment for cooling_op: cooling_con$DF.rollout input")

            dims[[x]]$ConstructRolloutScenario(scenario.name,
              yr.tgt = year.tgt.scen,
              rollout2.df = dims$cooling_con$DF.rollout,
              floor.df = dims$housing$DF.DLS
            )
          } else if (x == c("heating_op")) {
            print("SetupRollout: Special treatment for heating_op: heating_con$DF.rollout input")

            dims[[x]]$ConstructRolloutScenario(scenario.name,
              yr.tgt = year.tgt.scen,
              rollout2.df = dims$heating_con$DF.rollout,
              floor.df = dims$housing$DF.DLS
            )
          } else if (x == c("hotwater_op")) {
            print("SetupRollout: Special treatment for hotwater_op: heating_con$DF.rollout input")

            dims[[x]]$ConstructRolloutScenario(scenario.name,
              yr.tgt = year.tgt.scen,
              rollout.df = dims$heating_con$DF.rollout
            )
          } else if (x == c("transport")) {
            print("SetupRollout: Special treatment for transport: LCT option")

            dims[[x]]$ConstructRolloutScenario(scenario.name,
              yr.tgt = year.tgt.scen,
              lct = lct,
              yr.st.ro=year.start.rollout
            )
          } else {
            dims[[x]]$ConstructRolloutScenario(scenario.name, yr.tgt = year.tgt.scen,
                                               yr.st.ro=year.start.rollout)
          }
        }
      )
    },


    #' @description
    #' Set up rollout trajectory for a given scenario
    CallDeriveEnergyIntensity = function(dims = dims) {
      # The scenario-specific rollout can be taken care of in each dimension.
      call_derive_energy_intensity_helper(dims=dims)
    },


    #' #' @description
    #' #' Aggregate some dimensions in case there's a need.
    #' AggregateDimensions = function() {
    #'
    #'   Shelter <<- Housing$DLE.tot + Clothing$DLE.tot
    #'   Appliance <<- Cooking$DLE.tot + TV$DLE.tot + Fridge$DLE.tot + AC$DLE.tot
    #'
    #' },


    #' @description
    #' Bring together all the dimensions' DLE result calculations. No aggregation happening here
    SumDLE = function() {
      # DLE <<- lapply(dims, function(x) {x$UpdateDLE()})
      DLE <- lapply(
        names(dims),
        function(x) {
          dims[[x]]$UpdateDLE()
          df <- dims[[x]]$DLE.tot
          df$dimension <- x

          if ("R11.region" %in% names(df)) {
            df <- df %>%
              ungroup() %>%
              select(region = R11.region, year, dimension, grp, type, elec, DLE:unit.DLE.pcap) %>%
              mutate(geo.res = "R11")
          } else {
            df <- df %>%
              ungroup() %>%
              select(region = iso, year, dimension, grp, type, elec, DLE:unit.DLE.pcap) %>%
              mutate(geo.res = "Country")
          }

          return(df)
        }
      )
      DLE.alldims <<- do.call("rbind", DLE)
    },


    #' @description
    #' Summarize all the dimensions' DLE
    CollectAllGap = function() {
      # DLE <<- lapply(dims, function(x) {x$UpdateDLE()})
      collect_all_gap_helper(dims=dims)
    },


    #' @description
    #' Create a dataframe with all the dimensions' energy intensities
    CollectAllEI = function() {

      collect_all_ei_helper(dims=dims)
    },

    #' @description
    #' Write out the energy intensities of a specific scenario
    SaveAllEI = function(path) {

      write_delim(
        x = CollectAllEI(),
        file = path,
        delim = ","
      )

    },

    #' @description
    #' Aggregate some dimensions in case there's a need.
    AggregateRegions = function() {
      # NB! this function will override DLE.pcap values in the input

      # DLE total
      DLE.alldims.agg <<- DLE.alldims %>%
        select(-DLE.pcap) %>%
        filter(geo.res == "Country") %>%
        ungroup() %>%
        left_join(message.R11, by = c("region" = "iso")) %>%
        group_by(R11.region, year, dimension, grp, type, elec) %>%
        # DLE total
        summarise(
          DLE = sum(DLE, na.rm = T),
          unit.DLE = first(unit.DLE), unit.DLE.pcap = first(unit.DLE.pcap),
        ) %>%
        ungroup() %>%
        rename(region = R11.region) %>%
        # DLE pcap
        left_join(R11.pop, by = c("region" = "R11.region", "year")) %>%
        mutate(DLE.pcap = DLE / population * exa / giga) %>%
        select(region, dimension, grp, type, elec, year, DLE, unit.DLE, DLE.pcap, unit.DLE.pcap)
    },


    #' @description
    #' Aggregate some dimensions in case there's a need.
    AggregateDimensions = function() {
      # NB! this function will override DLE.pcap values in the input
      # NB! requires DLE.alldims.agg which is created in the AggregateRegions() function

      # Note: Appliance will be further broken down with Nutrition
      # 1  transport    Mobility
      # 2  appliances   Socialization (television, mobile phone), Nutrition (clean cooking, refrigerator),
      # 3  water        Health
      # 4  sanit        Health
      # 5  nutrition    Nutrition
      # 6  clothing     Shelter
      # 7  health       Health
      # 8  education    Socialization
      # 9  housing      Shelter
      # 10 heating_con  Shelter
      # 11 cooling_con  Shelter
      # 12 heating_op   Shelter
      # 13 cooling_op   Shelter
      # 14 roads        Mobility
      # 15 hotwater_op  Health


      mapping.dim <- data.frame(
        dimension = c(
          "transport",
          "appliances",
          "water",
          "sanit",
          "nutrition",
          "clothing",
          "health",
          "education",
          "housing",
          "heating_con",
          "cooling_con",
          "heating_op",
          "cooling_op",
          "roads",
          "hotwater_op"
        ),
        # unique(DLE.alldims.agg %>% select(dimension)),
        group = c(
          "Mobility",
          "Socialization",
          "Health",
          "Health",
          "Nutrition",
          "Shelter",
          "Health",
          "Socialization",
          "Shelter",
          "Shelter",
          "Shelter",
          "Shelter",
          "Shelter",
          "Mobility",
          "Health"
        ), stringsAsFactors = FALSE
      )

      # Refine the mapping with 'grp' info
      dim.grp <- unique(DLE.alldims.agg %>% select(dimension, grp)) %>%
        left_join(mapping.dim) %>%
        mutate(group = ifelse(grp %in% c("clean_cooking_fuel", "refrigerator"), "Nutrition", group)) # Modify cooking appl.

      DLE.group.agg <<- DLE.alldims.agg %>%
        left_join(dim.grp) %>%
        group_by(region, year, group, type, elec) %>%
        summarise(
          DLE = sum(DLE, na.rm = T),
          unit.DLE = first(unit.DLE), unit.DLE.pcap = first(unit.DLE.pcap)
        ) %>%
        ungroup() %>%
        # DLE pcap
        left_join(R11.pop, by = c("region" = "R11.region", "year")) %>%
        mutate(DLE.pcap = DLE / population * exa / giga) %>%
        select(region, group, type, elec, year, DLE, unit.DLE, DLE.pcap, unit.DLE.pcap)
    },


    #' @description
    #' Set up a sensitivity test by changing a variable in a certain dimension
    #' @param config list of new values for fields.
    #' e.g. config = list(clothing=list(grp = c('d'), thres = 88), water=list(grp = c('d'), thres = 88))
    SetupSensitivityTest = function(config) {
      lapply(names(config), function(x) {
        params <- config[[x]]

        dims[[x]]$SensitivityRun(params, scenario.name)

        # Take care of dependencies
        if (x == "housing") {
          dims$heating_con$SensitivityRun(params, scenario.name,
            DF.DLS.housing = dims$housing$DF.DLS
          )
          dims$cooling_con$SensitivityRun(params, scenario.name,
            DF.DLS.housing = dims$housing$DF.DLS
          )
          dims$heating_op$SensitivityRun(params, scenario.name,
            rollout1.df = dims$housing$DF.rollout,
            rollout2.df = dims$heating_con$DF.rollout,
            floor.df = dims$housing$DF.DLS
          )
          dims$cooling_op$SensitivityRun(params, scenario.name,
            rollout1.df = dims$housing$DF.rollout,
            rollout2.df = dims$cooling_con$DF.rollout,
            floor.df = dims$housing$DF.DLS
          )
          dims$hotwater_op$SensitivityRun(params, scenario.name,
            gap_water.df = dims$water$DF.DLS,
            gap_heating.df = dims$heating_con$DF.DLS,
            rollout_housing.df = dims$housing$DF.rollout,
            rollout.df = dims$heating_con$DF.rollout
          )
        } else if (x == "heating_con") {
          dims$heating_op$SensitivityRun(params, scenario.name,
            rollout1.df = dims$housing$DF.rollout,
            rollout2.df = dims$heating_con$DF.rollout,
            floor.df = dims$housing$DF.DLS
          )
          dims$hotwater_op$SensitivityRun(params, scenario.name,
            gap_water.df = dims$water$DF.DLS,
            gap_heating.df = dims$heating_con$DF.DLS,
            rollout_housing.df = dims$housing$DF.rollout,
            rollout.df = dims$heating_con$DF.rollout
          )
        } else if (x == "cooling_con") {
          dims$cooling_op$SensitivityRun(params, scenario.name,
            rollout1.df = dims$housing$DF.rollout,
            rollout2.df = dims$cooling_con$DF.rollout,
            floor.df = dims$housing$DF.DLS
          )
        }
      })

      # Re-compile DLE.alldims
      SumDLE()
      AggregateRegions()
      AggregateDimensions()
    },

    #' @description
    #' Plot per capita per region by dimension (N.B. DLE.pcap seems very off...)

    PlotDLEpcap_ByRegByDim = function(type.remove = "all") {
      df.grp.region.pc <- DLE.alldims.agg %>%
        filter(elec != "total") %>%
        ungroup() %>%
        select(type, elec, year, dimension, region, DLE) %>%
        filter(type != type.remove) %>%
        group_by(year, dimension, region) %>%
        summarise(val = sum(DLE, na.rm = TRUE)) %>%
        left_join(R11.pop %>% rename(region = R11.region)) %>%
        mutate(val = val / population * exa / giga) %>%
        select(-population)

      p.grp.region <- ggplot(
        data = df.grp.region.pc,
        aes(
          x = year,
          y = val,
          fill = dimension
        )
      ) +
        geom_area() +
        facet_wrap(~region) +
        labs(
          title = "Total decent living energy per capita",
          y = "GJ/cap/year"
        )

      ggplotly(p.grp.region)
    },

    #' @description
    #' Plot per capita global by dimension
    PlotDLEpcap_GlobalByDim = function(type.remove = "all") {
      df.grp.global.pc <- DLE.alldims.agg %>%
        filter(elec != "total") %>%
        ungroup() %>%
        select(type, elec, year, dimension, region, DLE) %>%
        filter(type != type.remove) %>%
        group_by(year, dimension) %>%
        summarise(val = sum(DLE, na.rm = TRUE)) %>%
        left_join(G.pop) %>%
        mutate(val = val / population * exa / giga) %>%
        select(-population)


      p.grp <- ggplot(
        data = df.grp.global.pc,
        aes(
          x = year,
          y = val,
          fill = dimension
        )
      ) +
        geom_area() +
        labs(
          title = "Total global decent living energy per capita",
          y = "GJ/cap/year"
        )

      ggplotly(p.grp)
    },

    #' @description
    #' Plot per capita per region by need (N.B. DLE.pcap seems very off...)
    PlotDLEpcap_ByRegByNeed = function(type.remove = "all") {
      df.region.pc <- DLE.group.agg %>%
        filter(elec != "total") %>%
        ungroup() %>%
        select(type, elec, year, group, region, DLE) %>%
        filter(type != type.remove) %>%
        group_by(year, group, region) %>%
        summarise(val = sum(DLE, na.rm = TRUE)) %>%
        left_join(R11.pop %>% rename(region = R11.region)) %>%
        mutate(val = val / population * exa / giga) %>%
        select(-population)

      p.need.region <- ggplot(
        data = df.region.pc,
        aes(
          x = year,
          y = val,
          fill = group
        )
      ) +
        geom_area() +
        facet_wrap(~region) +
        labs(
          title = "Total decent living energy per capita, by need group",
          y = "GJ/cap/year"
        )

      ggplotly(p.need.region)
    },

    #' @description
    #' Plot whatever
    PlotDLEGapAll = function() {


    },

    #' @description
    #' Save the current object on the file in R external object format.
    SaveScenario = function(filename) {
      myMat <- .self
      base::saveRDS(myMat, file = paste0(out.path, "/", filename))
    },

    #' @description
    #' Save summary .
    SaveResultTable = function(filename) {
      write_delim(DLE.alldims.agg, path = paste0(out.path, "/", filename), delim = ",")
    }
  )
)
