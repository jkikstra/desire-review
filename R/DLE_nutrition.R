#' Class DLE.dimension.health
#'
#' @description subclass for health dimension (inheriting DLE.dimension)

DLE.dimension.nutrition <-
  setRefClass("DLE.dimension.nutrition",
    contains = "DLE.dimension", # inherit the DLE.dimension class

    fields = list(

      # Master DF keeping DLS thresholds, gaps, and likely some dimension-specific data
      # In this base definition, per-capita threshold and gap are assume, but each dimension can diverge based on data.
      #' @field iso
      #' @field CV
      #' @field ADER
      #' @field DES   national mean kcal/day in base year
      #' @field nu    mean of log(kcal) distribution (kcal/day ~ lognormal)
      #' @field sigma sd of log(kcal) distribution
      #' More dimension-specific fields can be here
      DF.nutri.base = "data.frame" # by [grp, region] : 'grp' - dimension-specific sub-group (e.g. urban/rural or adult/child)
    ),
    methods = list(

      # Add
      initialize = function(...) {
        print("initialize: Nutrition")

        callSuper(...) # Run the method in the super-class first.

        # Read FAO
        # coefficient of variation
        df.cv <- read_excel(paste0(data.path, "/Food/Food_Security_Indicators_17Jul2020.xlsx"), sheet = "A_10", skip = 2) %>%
          rename(Country = Regions.Subregions.Countries) %>%
          rename(CV = as.character(year.base)) %>%
          select(FAOST_CODE, Country, CV) %>%
          filter(!is.na(CV))
        # Average Dietary Energy Requirement (ADER)
        df.ader <- read_excel(paste0(data.path, "/Food/Food_Security_Indicators_17Jul2020.xlsx"), sheet = "A_9", skip = 2) %>%
          rename(Country = Regions.Subregions.Countries) %>%
          rename(ADER = as.character(year.base)) %>%
          select(FAOST_CODE, ADER) %>%
          filter(!is.na(ADER))
        # Minimum Dietary Energy Requirement (MDER)
        df.mder <- read_excel(paste0(data.path, "/Food/Food_Security_Indicators_17Jul2020.xlsx"), sheet = "A_8", skip = 2) %>%
          rename(Country = Regions.Subregions.Countries) %>%
          rename(MDER = as.character(year.base)) %>%
          select(FAOST_CODE, MDER) %>%
          filter(!is.na(MDER))
        # Prevalence of undernourishment
        df.pou <- read_excel(paste0(data.path, "/Food/Food_Security_Indicators_17Jul2020.xlsx"), sheet = "I_2.3", skip = 2) %>%
          rename(Country = Regions.Subregions.Countries) %>%
          rename(PoU = `2014-16`) %>% # 3-yr moving average
          select(FAOST_CODE, PoU) %>%
          mutate(PoU = ifelse(PoU == "n.a.", NA, PoU)) %>% # some entries are n.a. and are not recognized as NA in R
          filter(!is.na(PoU)) %>%
          mutate(PoU = as.numeric(ifelse(PoU == "<2.5", 0, PoU)) / 100)
        # dietary energy supply
        df.des <- read_excel(paste0(data.path, "/Food/Food_Security_Indicators_17Jul2020.xlsx"), sheet = "A_12", skip = 2) %>%
          rename(Country = Regions.Subregions.Countries) %>%
          rename(DES = `2014-16`) %>% # 3-yr moving average
          select(FAOST_CODE, DES) %>%
          filter(!is.na(DES))

        df <- df.cv %>%
          left_join(df.ader, by = "FAOST_CODE") %>%
          left_join(df.mder, by = "FAOST_CODE") %>%
          left_join(df.pou, by = "FAOST_CODE") %>%
          left_join(df.des, by = "FAOST_CODE") %>%
          mutate(iso = countrycode(Country, "country.name", "iso3c"))

        df <- message.R11 %>%
          # assume regional average {VARS: CV, ADER, MDER, PoU, and DES} for those countries that do not have those values
          left_join(df, by = "iso") %>% # add message.R11 regions
          group_by(R11.region) %>%
          mutate(CV = ifelse(is.na(CV), mean(CV, na.rm = T), CV)) %>%
          mutate(ADER = ifelse(is.na(ADER), mean(ADER, na.rm = T), ADER)) %>%
          mutate(MDER = ifelse(is.na(MDER), mean(MDER, na.rm = T), MDER)) %>%
          mutate(PoU = ifelse(is.na(PoU), mean(PoU, na.rm = T), PoU)) %>%
          mutate(DES = ifelse(is.na(DES), mean(DES, na.rm = T), DES)) %>%
          ungroup() %>%
          select(-country_name, -R11.region) %>%
          # calculate depth-of-deficit calculation parameters
          mutate(sigma = sqrt(log(CV^2 + 1))) %>%
          mutate(nu = log(DES) - (sigma^2) / 2) # %>%
        # mutate(gap = sapply(DepthofDeficit(nu, sigma, ADER), "[", 1),
        #        deprivation.headcount = sapply(DepthofDeficit(nu, sigma, ADER), "[", 2)) %>% # DoD defined as average depth of deficiency per person under ADER

        DF.nutri.base <<- df %>%
          #   filter(Country != "China, mainland") %>%
          select(-FAOST_CODE, -Country)
      },


      #' @description
      #' Overwrite the same-named function in the superclass to be specific to this dimension
      IdentifyGap = function() {
        print("IdentifyGap: Nutrition")

        # Function deriving Depth of Deficit
        GetDepthofDeficit <- function(nu, sigma, thres) {
          if (is.na(nu) | is.na(thres)) {
            return(NA)
          }

          # Typical lognormal distribution to be used for integration
          f <- function(x, nu, sigma) {
            dlnorm(x, meanlog = nu, sdlog = sigma, log = FALSE)
          }
          xf <- function(x, nu, sigma) {
            x * f(x, nu, sigma)
          }

          mean.subthres <- integrate(xf, 0, thres, nu, sigma)
          sh.subthres <- integrate(f, 0, thres, nu, sigma)

          DoD <- thres - mean.subthres$value / sh.subthres$value

          return(c(DoD, sh.subthres$value)) # Return sub-thres share as well
        }

        # Vectorize it!
        DepthofDeficit <- Vectorize(GetDepthofDeficit)

        # Add columns regarding gap specification
        # NOTE: By definition, PoU and deprivation.headcount should be the same when MDER is used. (But data availability seems different from https://ourworldindata.org/hunger-and-undernourishment#depth-of-the-food-deficit)
        #       I tested multiple combinations trying to find out a metric similar to the one in ourworldindata.
        #       According to the definition of DoD, dod.avg.mder is right, which also gives a smilar range.
        #       For PoU, we also adopt PoU or deprivation.headcount.mder. But from the FAO data, PoU is missing for many countries. So we adopt deprivation.headcount.mder.
        DF.nutri.base <<- DF.nutri.base %>%
          mutate(
            gap.mder = DepthofDeficit(DF.nutri.base$nu, DF.nutri.base$sigma, DF.nutri.base$MDER)[1, ],
            deprivation.headcount.mder = DepthofDeficit(DF.nutri.base$nu, DF.nutri.base$sigma, DF.nutri.base$MDER)[2, ],
            gap.ader = DepthofDeficit(DF.nutri.base$nu, DF.nutri.base$sigma, DF.nutri.base$ADER)[1, ],
            deprivation.headcount.ader = DepthofDeficit(DF.nutri.base$nu, DF.nutri.base$sigma, DF.nutri.base$ADER)[2, ]
          ) %>% # DoD defined as average depth of deficiency per person under ADER
          mutate(
            dod.sub.mder = (ADER - MDER) + gap.mder,
            dod.avg.mder = ((ADER - MDER) + gap.mder) * deprivation.headcount.mder,
            dod.sub.ader = gap.ader,
            dod.avg.ader = gap.ader * deprivation.headcount.ader
          )
        # mutate(gap = gap * deprivation.headcount) # population average gap
        DF.DLS <<- DF.DLS %>%
          left_join(
            DF.nutri.base %>%
              select(iso, gap = dod.avg.mder, deprivation.headcount = deprivation.headcount.mder), # %>%
            # mutate(gap = gap * deprivation.headcount), # dod.avg.mder is already pop average
            by = "iso"
          ) %>% # population average gap (entire population)
          mutate(gap = coalesce(gap.x, gap.y)) %>%
          select(-gap.x, -gap.y)
      },

      #' @description
      #' Derive DLS thresholds from data if not pre-determined
      DeriveThreshold = function() {
        print("DeriveThreshold: Nutrition")

        DF.DLS <<- DF.DLS %>%
          left_join(DF.nutri.base %>% select(iso, thres = MDER), by = "iso") %>% # ASSUME: fixed MDER over time
          mutate(thres = coalesce(thres.x, thres.y)) %>%
          select(-thres.x, -thres.y)
      },

      #' @description
      #' Rollout for nutrition will specify gap closing pathway for the whole population.
      #' (gap is already defined as population average gap.)
      #' So the total rollout here already takes into account the DLE portion of beyond-DLE population.
      #' For non-BAU scenarios, we don't need to consider the BAU gap closing, since it is not compensatory scenario.
      ConstructRolloutScenario = function(...) {
        print("ConstructRolloutScenario: Nutrition")

        callSuper(..., only.OP = TRUE) # Run the method in the super-class first.

        DF.rollout <<- DF.rollout %>%
          filter(type == "OP") %>% # Only OP matters for this dimension
          mutate(rollout = rollout * 365) # gap was per-day, so converto to per-year gap/rollout.
        # The line below not needed because gap is not defined as population average.
        # mutate(rollout = rollout.pcap * population * deprivation.headcount + thres * population * (1 - deprivation.headcount))

        DF.rollout <<- DF.rollout %>%
          bind_rows(DF.rollout %>% mutate(type = "CON.new", rollout = 0)) %>%
          bind_rows(DF.rollout %>% mutate(type = "CON.new.pop", rollout = 0)) %>%
          bind_rows(DF.rollout %>% mutate(type = "CON.new.dls", rollout = 0)) %>%
          bind_rows(DF.rollout %>% mutate(type = "CON.rep", rollout = 0))
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
        print("DeriveEnergyIntensity: Nutrition")
        tot.e <- ImportEXIOFinalEnergy("Nutrition", mode = "Total") %>%
          left_join(message.R11) %>%
          mutate(R11.region = ifelse(iso %in% c("MEA", "AFR", "LAM"), iso, R11.region))

        # Here, I assume MJ per "consumed" kcal. So by applying this intensity to the future,
        # I implicitly assume a fixed share of wasted kcal after purchased.

        # Total kcal for individual countries
        DF.tot.kcal <- DF.nutri.base %>%
          left_join(pop %>% filter(year == year.base) %>% select(iso, population)) %>%
          mutate(tot.kcal = DES * population * 365) %>% # Total yearly kcal intake
          select(iso, tot.kcal)

        # Total kcal for group 3. "Rest of" regions/countries (MEA/AFR/LAM)
        DF.tot.kcal.sub <- DF.tot.kcal %>%
          left_join(message.R11 %>% select(-country_name)) %>%
          filter(R11.region %in% c("MEA", "AFR", "LAM")) %>%
          mutate(R11.region = ifelse(iso %in% c("BRA", "MEX", "ZAF"), iso, R11.region)) %>%
          group_by(R11.region) %>%
          summarise(tot.kcal = sum(tot.kcal, na.rm = TRUE))

        # For these 5 MSG R11 regions, take the representative country value (1 country/region)
        DF.e.int.5 <- tot.e %>%
          left_join(DF.tot.kcal) %>%
          filter(iso %in% c("IDN", "CHN", "IND", "RUS", "USA")) %>%
          mutate(e.int = val / tot.kcal * exa / mega) %>%
          select("iso", "R11.region", "elec", "val", "e.int")

        # For these 3 MSG R11 regions, keep multiple individual countries and, for the rest, take the average of existing countries' values
        DF.e.int.3.N <- tot.e %>%
          left_join(DF.tot.kcal) %>%
          filter(R11.region %in% c("WEU", "EEU", "PAO")) %>%
          mutate(e.int = val / tot.kcal * exa / mega) %>%
          select("iso", "elec", "val", "e.int") %>%
          # Add region average too
          rbind(tot.e %>%
            left_join(DF.tot.kcal) %>%
            filter(R11.region %in% c("WEU", "EEU", "PAO")) %>%
            group_by(R11.region, elec) %>%
            summarise(tot.kcal = sum(tot.kcal, na.rm = TRUE), val = sum(val, na.rm = TRUE)) %>%
            mutate(e.int = val / tot.kcal * exa / mega, iso = R11.region, type = "OP") %>%
            ungroup() %>%
            select("iso", "elec", "val", "e.int"))

        # For these 3 MSG R11 regions, include the 'rest of' regions + a few countries (MEX, BRA, ZAF)
        DF.e.int.3.S <- tot.e %>%
          left_join(DF.tot.kcal.sub, by = c("iso" = "R11.region")) %>%
          filter(R11.region %in% c("MEA", "LAM", "AFR")) %>%
          group_by(iso, elec) %>%
          summarise(tot.kcal = sum(tot.kcal, na.rm = TRUE), val = sum(val, na.rm = TRUE)) %>%
          mutate(e.int = val / tot.kcal * exa / mega) %>%
          mutate(type = "OP") %>%
          select("iso", "elec", "val", "e.int") %>%
          ungroup()

        # e.int is in MJ/kcal (consumed) for EXIO regions.

        DF.tei <<- DF.tei %>%
          left_join(message.R11 %>% select(-country_name)) %>%
          filter(type == "OP") %>%
          select(-e.int) %>%
          left_join(DF.e.int.5 %>% select(R11.region, elec, e.int), by = c("R11.region", "elec")) %>%
          left_join(DF.e.int.3.N %>% select(iso, elec, e.int), by = c("iso", "elec")) %>%
          mutate(e.int = coalesce(e.int.x, e.int.y)) %>%
          select(!c(e.int.x, e.int.y)) %>%
          left_join(DF.e.int.3.N %>% select(iso, elec, e.int), by = c("R11.region" = "iso", "elec")) %>%
          mutate(e.int = coalesce(e.int.x, e.int.y)) %>%
          select(!c(e.int.x, e.int.y)) %>%
          left_join(DF.e.int.3.S %>% select(iso, elec, e.int), by = c("iso", "elec")) %>%
          mutate(e.int = coalesce(e.int.x, e.int.y)) %>%
          select(!c(e.int.x, e.int.y)) %>%
          left_join(DF.e.int.3.S %>% select(iso, elec, e.int), by = c("R11.region" = "iso", "elec")) %>%
          mutate(e.int = coalesce(e.int.x, e.int.y)) %>%
          select(!c(e.int.x, e.int.y))

        # add CON.new and CON.rep for nutrition, both being zero.
        DF.tei <<- DF.tei %>%
          bind_rows(DF.tei %>% mutate(type = "CON.new", e.int = 0)) %>%
          bind_rows(DF.tei %>% mutate(type = "CON.new.pop", e.int = 0)) %>%
          bind_rows(DF.tei %>% mutate(type = "CON.new.dls", e.int = 0)) %>%
          bind_rows(DF.tei %>% mutate(type = "CON.rep", e.int = 0))
      },

      #' @description
      #' Make a table for SI writeup: TOp 10 countries with biggest gaps
      SummaryTable = function() {
        df <- DF.nutri.base %>%
          arrange(-gap.ader) %>%
          slice(10) %>%
          left_join(message.R11, by = "iso") %>%
          select(Country = country_name, "DES [kcal/day]" = DES, "Gap [kcal/day]" = gap.ader, "PoU [%]" = deprivation.headcount.mder)

        write_delim(
          df,
          path = paste0(out.path, "/nutrition_gap.csv"),
          delim = ","
        )

        return(df)
      },

      #' @description
      #' Make a scatterplot of consumption/gap/share
      PlotCountryComparison = function() {
        df <- DF.nutri.base %>% mutate(Continent = countrycode(
          sourcevar = iso,
          origin = "iso3c",
          destination = "continent"
        ))

        p <- ggplot(data = df) +
          geom_point(aes(x = dod.avg.mder, y = DES, size = deprivation.headcount.mder * 100, color = Continent), alpha = 0.5) +
          geom_text_repel(aes(x = dod.avg.mder, y = DES, label = iso)) +
          scale_size(
            range = c(.1, 12),
            name = str_wrap("Prevalence of undernourishment (PoU, %population)", width = 20)
          ) +
          xlab("Average depth of deficiency (DoD) [kcal/day/cap]") +
          ylab("Dietary energy supply (DES) [kcal/day/cap]") +
          guides(colour = guide_legend(override.aes = list(size = 8)))

        pdf(
          file = paste0(out.path, "/Nutrition gap by country.pdf"), # The directory you want to save the file in
          width = 9, height = 7.5
        )
        print(p)
        dev.off()
      }
    )
  )
