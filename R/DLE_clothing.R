#' Class DLE.dimension.clothing
#'
#' @description subclass for clothing dimension (inheriting DLE.dimension)

DLE.dimension.clothing <-
  setRefClass("DLE.dimension.clothing",
    contains = "DLE.dimension", # inherit the DLE.dimension class

    fields = list(

      # DF keeping country-specific price per kg of clothing/footwear (USD MER / kg at year.base)
      #' @field iso
      #' @field clothing
      #' @field footwear
      #' More dimension-specific fields can be here
      DF.price    = "data.frame",

      # kg/cap thresholds from external sources
      thres.clothing = "numeric",
      thres.footware = "numeric",

      # $/kg price assumption
      clothing.price = "numeric",
      footwear.price = "numeric"
    ),
    methods = list(
      initialize = function(..., thres.clothing = 1.3, thres.footware = 0.9) {
        print("initialize: clothing")
        callSuper(..., thres.clothing = thres.clothing, thres.footware = thres.footware)

        # Total household expenditure (PPP (constant 2017 international $))
        # 2017 has decent amount of observations, which were not avaiable before 2015.
        # hh_exp <- WDI(indicator = "NE.CON.PRVT.PP.KD", start = year.base, end = 2017, extra = TRUE) %>%
        #   rename(expenditure = NE.CON.PRVT.PP.KD, iso = iso3c) # %>% select(iso = iso3c, expenditure)
        hh_exp <- read_excel(
          here("data-raw", "dls_data", "monetary", "worldbank",
               "API_NE.CON.PRVT.PP.KD_DS2_en_excel_v2_2073432.xls"),
          sheet = "Data",
          skip = 3
        ) %>% wb_parse(drop.na=F) %>%
          filter(year%in%seq(year.base,2017)) %>% rename(expenditure=value) %>% select(iso, year, expenditure)
        hh_exp_impute <- hh_exp %>%
          group_by(iso) %>%
          summarise(expenditure = mean(expenditure, na.rm = TRUE))
        # Load HDD30 data; heating degree-days information. Not used currently, but in case this will become necessary.
        # DF.HDD <- read_csv(paste0(data.path, "/Clothing/ISO_agg_data_SDD_H_v13_ssp2_2010_hist.csv")) %>%
        #   filter(t_bal_H == 30) %>%
        #   rename(HDD = sdd_h_avg) %>%
        #   select(iso = ISO, HDD)

        DF.DLS <<- DF.DLS %>%
          left_join(hh_exp %>% filter(year == year.base) %>% select(iso, expenditure), by = "iso") %>%
          # Impute missing expenditure values
          left_join(hh_exp_impute, by = "iso") %>%
          mutate(expenditure = coalesce(expenditure.x, expenditure.y)) %>%
          select(-starts_with("expenditure.")) %>%
          left_join(pop %>% filter(!is.na(hh_size), year == year.base) %>% select(iso, hh_size, n_hh)) %>%
          mutate(expenditure = expenditure / n_hh) %>%
          # left_join(DF.HDD) %>%
          select(-hh_size, -n_hh) # Drop these to be consistent with other dims and have them later

        # Derive approximate price by country for intensity calculation

        # Reference price/kg, brought from DLE calculation for India (USD PPP2010/kg)
        clothing.price <<- 51.1
        footwear.price <<- 14.9

        # Need to convert the price to USD MER base year / kg
        df.price.conv <- calculate_price_index_change()

        # I don't aggregate this to R11 yet, to use this later for deriving R11 aggregate intensity in DeriveEnergyIntensity().
        DF.price <<- df.price.conv %>%
          mutate(
            clothing = clothing.price * conv,
            footwear = footwear.price * conv
          )

        # DF.price <<- AggregateRegion(DF.price, c(clothing, footwear))
        #
        # # For this dimension, we only do R11-level calculation
        # # DF.hdd.agg <- AggregateRegion(DF.DLS, HDD)
        # # DF.exp.agg <- AggregateRegion(DF.DLS, expenditure, n_hh)
        # # DF.hhs.agg <- AggregateRegion(DF.DLS, hh_size, n_hh)
        # #
        # # DF.DLS <<- DF.hdd.agg %>% left_join(DF.exp.agg) %>% left_join(DF.hhs.agg)
        # #
        # # DF.rollout <<- AggregateRegion(DF.rollout, rollout) # This is a dummy DF at this stage.

        # # Aggregate DFs from country-level to to R11-level
        # DF.thres.agg <- AggregateRegion(DF.DLS, thres)
        # DF.gap.agg <- AggregateRegion(DF.DLS, gap)
        #
        # DF.DLS <<- DF.thres.agg %>%
        #   left_join(DF.gap.agg) %>%
        #   mutate(unit = indicator)
        # DF.DLS.base <<- DF.DLS
        #
        # DF.rollout <<- AggregateRegion(DF.rollout, rollout, func = "sum") # This is a dummy DF at this stage.
        # DF.rollout.base <<- DF.rollout
      },

      #' @description
      #' Derive DLS thresholds from data if not pre-determined
      DeriveThreshold = function() {
        print("DeriveThreshold: clothing")

        # External thresholds
        DF.DLS <<- DF.DLS %>%
          mutate(thres = ifelse(grp == "clothing", thres.clothing, thres.footware)) %>%
          left_join(message.R11, by = "iso") %>% # add message.R11 regions to do easy and transparent adjustment for colder regions
          mutate(thres = ifelse(grp == "clothing" & R11.region %in% c("WEU", "EEU", "NAM", "FSU", "PAO"), thres + 1.13, thres)) %>%
          select(-country_name, -R11.region) # remove columns again from message.R11
      },

      #' @description
      #' Overwrite the same-named function in the superclass to be specific to this dimension
      IdentifyGap = function() {
        print("IdentifyGap: clothing")

        # No gap for clothing
        DF.DLS <<- DF.DLS %>% mutate(gap = 0)
      },

      #' @description
      #' Derive (or import) final energy intensities from data if not pre-determined
      DeriveEnergyIntensity = function() {
        print("DeriveEnergyIntensity: clothing")

        # Recalculate DF.price for sensitivity case
        DF.price <<- DF.price %>% mutate(clothing = clothing.price * conv, footwear = footwear.price * conv)

        # Return MJ/USD MER (year.base)
        # EXIO doesn't differentiate footwear/clothing
        DF.tei <<- ImportEXIOFinalEnergy("Clothing", mode = "Intensity") %>%
          left_join(DF.price %>% select(-conv)) %>%
          mutate(val.clothing = val * clothing, val.footwear = val * footwear) %>%
          select(-val) %>%
          pivot_longer(cols = starts_with("val."), names_to = "grp", names_prefix = "val.", values_to = "val") %>%
          select(-clothing, -footwear) %>%
          mutate(unit = "MJ/kg") %>%
          select(iso, grp, type, elec, e.int = val) %>%
          # There are countries with abnormal inflation etc., remove their EIs.
          mutate(e.int = ifelse((iso %in% c("MMR", "VEN", "SDN", "LAO", "TKM", "UZB")), NA, e.int))

        # add hard caps for whether data is sensible, and replace values that go over it where necessary, introduced as a fix to counteract the impossibly high energy intensities derived for TKM and UZB
        # ranges from peer-reviewed literature:
        # Van der Velden, Patel, Vogtländer, 2014 (https://link.springer.com/article/10.1007/s11367-013-0626-9); 160-460 MJ/kg - for cotton from 300 dtex to 70 dtex in fig 7 (350-380 MJ/kg for Acryl, Nylon, PET, Elastane; fig 11, at 70 dtex)
        # Cay, 2018 (https://doi.org/10.1016/j.energy.2018.06.128 - Turkey industry); 0.78-1.44 MJ/piece -- would need to be translated to per kilogram to be used here
        upper.cap.clothing <- 750 # [MJ/kg] # informed by upper boundary of ERL 2021 paper
        countries.above.limit <- DF.tei %>% filter(e.int>upper.cap.clothing) %>% pull(iso) %>% unique()
        lower.cap.clothing <- 35 # [MJ/kg] # informed by lower boundary of ERL 2021 paper
        countries.below.limit <- DF.tei %>% filter(type=="OP",elec=="total", e.int<lower.cap.clothing) %>% pull(iso) %>% unique()

        DF.tei <<- DF.tei %>%
          mutate(e.int = ifelse((iso %in% c(countries.above.limit, countries.below.limit)), NA, e.int))

        # resolve NA; use (weighted) average energy intensity of R11 region (weighted by population).
        df.tei.fill <- DF.tei %>%
          pivot_wider(names_from = elec, values_from = e.int) %>%
          left_join(message.R11 %>% select(-country_name), by = "iso") %>%
          drop_na() %>%
          left_join(pop %>% filter(year == year.base) %>% select(iso, population)) %>%
          reframe(e.int.elec.fill = weighted.mean(elec, w = population),
                  e.int.nonelec.fill = weighted.mean(non.elec, w = population),
                  e.int.total.fill = weighted.mean(total, w = population),
                  .by = c("grp", "type", "R11.region"))
        # if Middle East and North Africa (MEA) region is missing, use SAS numbers (similar latitude, most similar climate)
        if ( "MEA" %nin% (df.tei.fill %>% pull(R11.region) %>% unique()) ){
          df.tei.fill <- df.tei.fill %>%
            bind_rows(
              df.tei.fill %>% filter(R11.region=="SAS") %>% mutate(R11.region="MEA")
            )
        }

        # this should be 11*2 = 22 long, otherwise throw an error!
        if (df.tei.fill %>% nrow() != 22){
          log_error("In clothing, there is a region (R11, MESSAGE) that does not have any credible energy intensity estimates.")
        }
        # also throw an error if somehow elec and non.elec do not add up to the total
        if (df.tei.fill %>% mutate(diff = e.int.total.fill - e.int.elec.fill - e.int.nonelec.fill) %>% pull(diff) %>% abs() %>% sum() > 1e-6){
          log_error("In clothing, there is a region (R11, MESSAGE) that has false energy intensity calculations.")
        }


        DF.tei <<- DF.tei %>% left_join(message.R11 %>% select(-country_name), by = "iso") %>%
          left_join(df.tei.fill, by = c("R11.region","type","grp")) %>%
          mutate_cond((is.na(e.int)&elec=="total"), e.int = e.int.total.fill) %>%
          mutate_cond((is.na(e.int)&elec=="elec"), e.int = e.int.elec.fill) %>%
          mutate_cond((is.na(e.int)&elec=="non.elec"), e.int = e.int.nonelec.fill) %>%
          ungroup() %>%
          select(-R11.region, -e.int.elec.fill, -e.int.nonelec.fill, -e.int.total.fill) # remove filler columns again

        # make explicit that we set CON.rep and CON.new to 0 because it is included in OP
        DF.tei <<- DF.tei %>%
          bind_rows(DF.tei %>% mutate(type = "CON.new", e.int = 0)) %>%
          bind_rows(DF.tei %>% mutate(type = "CON.new.pop", e.int = 0)) %>%
          bind_rows(DF.tei %>% mutate(type = "CON.new.dls", e.int = 0)) %>%
          bind_rows(DF.tei %>% mutate(type = "CON.rep", e.int = 0))


      },

      #' @description
      #' Focus only on the OP type here
      ConstructRolloutScenario = function(...) {
        print("ConstructRolloutScenario: Clothing")

        callSuper(..., only.OP = TRUE) # Run the method in the super-class first.
      }
    )
  )
