#' Class DLE.dimension.health
#'
#' @description subclass for health dimension (inheriting DLE.dimension)

DLE.dimension.health <-
  setRefClass("DLE.dimension.health",
    contains = "DLE.dimension", # inherit the DLE.dimension class

    fields = list(

      # DLS indicator of this dimension
      threshold_hale = "numeric"
    ),
    methods = list(
      initialize = function(..., threshold_hale = 65) { # df.input = data.frame(expenditure=0, HDD=0, hh_size=0)) {
        print("initialize: health")
        callSuper(..., threshold_hale = threshold_hale)

        # health dimension requires additional information to derive global DLS thresholds.
        DF.DLS <<- data.frame(DF.DLS) # , df.input)
      },

      #' @description
      #' Derive DLS thresholds from data if not pre-determined
      DeriveThreshold = function() {
        print("DeriveThreshold: health")

        # required files
        fname_hale <- "/HALE.csv" # processed by hand - original data on the WHO website (https://apps.who.int/gho/data/node.main.HALE?lang=en) seems to be down at 23/04/2020...
        health_hale <- read_csv(paste0(data.path, "/Health", fname_hale))
        fname_exp <- "/API_SH.XPD.CHEX.PP.CD_DS2_en_excel_v2_998503.xls"
        health_exp <- read_excel(paste0(data.path, "/Health", fname_exp), sheet = "Data", skip = 3, col_names = TRUE)
        health_exp <- health_exp[health_exp$`Country Code` %in% health_hale$`Country Code`, ]
        write_csv(health_exp, paste0(data.path, "/Health", "/Health_expenditure_countries.csv"))



        # choose threshold values
        # threshold_hale <<- 65 # threshold to be adjusted for sensitivity analysis
        year.hale <- "2015" # year to derive threshold from - could be expanded to a range of years.


        # find the countries that meet the health thresholds
        health_countries_meet_threshold <- health_hale[[year.hale]] > threshold_hale # boolean list of all 217 countries
        # filter the health expenditure data (keep only countries that meet the threshold)
        health_expenditure_meet_threshold_hale <- select(health_exp, `Country Code`, year.hale) %>%
          filter(health_countries_meet_threshold)
        health_expenditure_meet_threshold_hale <- drop_na(health_expenditure_meet_threshold_hale)

        # find the most efficient half that meets the DLS standards
        health_efficienthalf_meet_threshold <- filter(
          health_expenditure_meet_threshold_hale,
          (health_expenditure_meet_threshold_hale[[year.hale]] <
            median(health_expenditure_meet_threshold_hale[[year.hale]]))
        )

        # DLS - derived Health Threshold
        health_threshold <- median(health_efficienthalf_meet_threshold[[year.hale]])

        DF.DLS <<- DF.DLS %>% mutate(thres = health_threshold) # Threshold: government expenditure on health per year (\$ per cap, PPP)
      },

      #' @description
      #' Overwrite the same-named function in the superclass to be specific to this dimension
      IdentifyGap = function() {
        print("IdentifyGap: Health")

        fname_exp <- "/Health_expenditure_countries.csv"
        year.base <- 2015 # year for the gap data loading

        # load data: health expenditure data
        health_exp <- read_csv(paste0(data.path, "/Health", fname_exp))
        health_exp <- health_exp %>%
          select_at(c("Country Code", paste(year.base))) %>% # Extract data for the base year
          rename_at(paste(year.base), list(~ paste("health_exp"))) %>% # Rename column
          rename(iso = "Country Code")

        # assign average expenditure of R11.region if NA
        health_exp_filled <- select(message.R11, -country_name) %>%
          left_join(health_exp, by = "iso") %>%
          group_by(R11.region) %>%
          mutate(health_exp = ifelse(is.na(health_exp),
            mean(na.omit(health_exp)),
            health_exp
          ))

        # Join data and calculate gap
        DF.DLS <<- DF.DLS %>%
          left_join(pop, by = "iso") %>% # Join data: population
          left_join(health_exp_filled, by = "iso") %>% # Join data: health expenditure
          mutate(gap = (thres - health_exp)) %>% # Calculate gap: total $ that should be spent extra on Health
          mutate(gap = ifelse(gap > thres, thres, gap)) %>% # to-do: ensure better way that regression produced values are not negative or otherwise unreasonably low - constrain by lowest observed value in source dataset.
          mutate(gap = ifelse(gap < 0, 0, gap)) %>% # if negative gap, set to zero
          select(-population, -health_exp) %>% # remove data: population, health_exp
          select(-(year:R11.region)) %>% # JM: Remove unnecessary columns
          unique()
      },


      #' @description
      #' Derive (or import) final energy intensities from data if not pre-determined
      #'
      #' There are three groups of countries:
      #'     1. One country representing one R11 region - PAS (IDN)/CPA (CHN)/SAS (IND)/FSU (RUS)/NAM (USA)
      #'        -> use the country's value to impute all other countries in each region
      #'     2. Multiple (but not all) countries in one R11 region - WEU/EEU/PAO
      #'        -> keep each country's value (as derived from EXIO), and for missing countries, assign the mean of existing countries
      #'        -> also because EXIO 'rest of' regions do not match the R11 regions
      #'     3. Zero, one or two countries not representing a region - MEA/LAM (BRA/MEX)/AFR (ZAF)
      #'        -> keep each country's value, and for the 'rest of' countries, derive intensity directly from EXIO EJ and kcal (minus BRA/MEX/ZAF)
      DeriveEnergyIntensity = function() {
        print("DeriveEnergyIntensity: Health Care")

        # We classify  the emobied energy intensity of education expenditure as operation energy,
        # and thus do not specify any additional construction energy.
        # Return MJ/USD MER (year.base)
        df.tei.health <- ImportEXIOFinalEnergy("Health", mode = "Intensity") %>% # 2015 data ... in MER ... so no conversion needed? Where do we need PPP?
          select(iso, type, elec, unit, e.int = val)


        # translate from MER to PPP

        # Need to convert the price to USD MER base year
        df.price.conv <- calculate_price_index_change()

        # I don't aggregate to R11 to use this for deriving R11 aggregate intensity directly.
        df.tei.health.pre <- df.tei.health

        df.tei.health <- df.tei.health %>%
          left_join(df.price.conv) %>%
          mutate(e.int = e.int * conv) %>%
          select(-conv) %>%
          mutate(unit = "MJ/USD (PPP)")

        # # option 1: take regionally different values and apply them directly:
        #
        # DF.tei <<- DF.tei %>% select(-e.int) %>% left_join(df.tei.health) #%>% left_join(message.R11 %>% select(-country_name), by="iso")
        #
        # DF.tei <<- AggregateRegion(DF.tei, e.int, population)


        # option 2: take median value of most efficient half and apply these globally (as the threshold is also global) --- take the ratio elec and non-elec to split out total.

        df.tei.global.total <- median((df.tei.health %>% na.omit() %>% filter(type == "OP", elec == "total") %>% filter(e.int <= median(e.int)))$e.int)
        df.tei.global.elec <- median((df.tei.health %>% na.omit() %>% filter(type == "OP", elec == "elec") %>% filter(e.int <= median(e.int)))$e.int)
        df.tei.global.non.elec <- median((df.tei.health %>% na.omit() %>% filter(type == "OP", elec == "non.elec") %>% filter(e.int <= median(e.int)))$e.int)
        e.ratio <- df.tei.global.elec / df.tei.global.total
        ne.ratio <- df.tei.global.non.elec / df.tei.global.total
        multiplier <- 1 / (e.ratio + ne.ratio)
        df.tei.global.elec <- multiplier * df.tei.global.elec
        df.tei.global.non.elec <- multiplier * df.tei.global.non.elec

        # print(DF.tei)

        DF.tei <<- DF.tei %>%
          select(-e.int) %>%
          mutate(e.int = ifelse(type == "OP",
            ifelse(elec == "total", df.tei.global.total,
              ifelse(elec == "elec", df.tei.global.elec,
                ifelse(elec == "non.elec", df.tei.global.non.elec,
                  NA
                )
              )
            ), 0
          ))
      },

      #' @description
      #' Construct rollout scenario based on identified gap and other assumptions.
      #' This can be coded differently for different scenarios.
      ConstructRolloutScenario = function(scen, yr.tgt = year.target, yr.st.ro=year.base) {
        print(paste("ConstructRolloutScenario:  Health -", scen))

        if (scen == "Income") {
          callSuper(scen, yr.tgt = yr.tgt, only.OP = TRUE)
        } else if (scen == "Income.regression") {
          callSuper(scen, yr.tgt = yr.tgt, only.OP = TRUE)
        } else if (scen == "ACCEL") {
          callSuper(scen, yr.tgt = yr.tgt, only.OP = TRUE, year.start.rollout=yr.st.ro)
        } else {
        }
      } # ,

      #' @description
      #' Run sensitivity case (= UpdateParameter + UpdateDLE)
      #' @param params list of fields and values to be updated
      # SensitivityRun = function(params, scen) {
      #
      #   UpdateParameter(params)
      #
      #   if ("threshold_hale" %in% names(params)) {
      #     DeriveThreshold()
      #     ConstructRolloutScenario(scen)
      #   }
      #
      #   UpdateDLE()
      #
      # }
    )
  )
