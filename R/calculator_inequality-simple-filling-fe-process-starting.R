# Script for predicting missing sectoral final energy inequality ginis for countries not in the IO table

TEST.GINI.MODELS <- FALSE


if (exists("ENERGY.DISTRIBUTION.FILE")) {
  f.fe <- here("data-raw", ENERGY.DISTRIBUTION.FILE)

  if (ENERGY.DISTRIBUTION.FILE == "data_to_global_redistribution_of_income_and_household_energy_footprints.xlsx") {
    # read in final energy ginis from data -------------------------------------
    data.based.ginis <- readRDS(fe.ginis, file = here(DATA.LOCATION, "FE_gini_iso.RData"))


    if (TEST.GINI.MODELS) {

      # May or may not be necessary to uncomment the below:
      # source(here("R", "input-options.R"))
      #
      # data.path <<- DLE.INPUT.DATA.PATH
      # run_initiatialization_input_data()

      # Explore Gini data ------------------------------------------------------
      source(here("R", "input-options.R"))
      data.path <<- DLE.INPUT.DATA.PATH # where is the input data for DLS and DLE hosted?
      year.end <<- DLE.SCENARIO.YEAR.END # if we run scenarios, until when?
      year.base <<- DLE.SCENARIO.YEAR.BASE # what is the base year for DLS (and DLE scenarios)?
      timestep <<- 5 # if we run scenarios, what is the timestep? -> required in `generate_all_dimensions` - the construction of a DLE object (even if we do not do rollout)
      run_initiatialization_input_data()


      income.gini.compare <-
        # World Income Inequality Database (WIID Companion) values
        economic_data_helper(fname_gini = "wiid_gini.csv") %>%
        mutate(source = "WIID", year="2015 ([73 countries]; with gaps in 2015 filled using the average of 2010-2020 data; downloaded wiidcountry from WIID companion, on 03/08/2023)") %>%
        # SSP gini values
        bind_rows(
          vroom(here("data-raw", "gini_ssp.csv"), show_col_types=FALSE) %>% # could also be changed to WID database if we want to stick more closely to empirical data, instead of the completness that this source offers
            filter(country != "World") %>%
            filter(year == 2015, scenario == "SSP2") %>%
            select(-year, -scenario) %>%
            rename(iso = country) %>%
            mutate(source = "Rao et al.", year="2015 (SSP2)")
        ) %>%
        # SHAPE Gini paper
        # bind_rows(
        #   vroom(
        #     here("data-raw", "SHAPE-final_gini.csv"), show_col_types=FALSE # file produced by a script in data-raw called `prep_shape_gini_data.R` (last update: SHAPE_gini_v1p1.csv)
        #   ) %>% filter(year==2020, scenario=="RC") %>% select(country,gini) %>%
        #     rename(iso=country) %>%
        #     mutate(source = "Min et al.", year="2020 (SDP_RC-1p5C, version SHAPE_gini_v1p1.csv)")
        # ) %>%

        bind_rows(
          vroom(
            here("data-raw", "SHAPE_Gini_v1p3_annual.csv"), show_col_types=FALSE # file shared by Jihoon over Teams on 17/01/2024 (and earlier to Bjoern by email)
          ) %>% select(-c(`Absolute target achieved`,`Base gini imputed`,`Share of final consumption among GDP imputed`)) %>%
            iamc_wide_to_long(upper.to.lower = T) %>%
            filter(year==2020, scenario=="RC") %>%
            rename(iso=region,gini=value) %>%
            select(iso,gini) %>%
            mutate(source = "Min et al.", year="2020 (SDP_RC, version SHAPE_Gini_v1p3_annual.csv)")
        ) %>%
        # updated SSP (review 1)
        bind_rows(
          vroom(
            here("data-raw", "dls_data", "gini", "poblete_ssp_update", "20231031_ssp_gini.csv"),
            show_col_types=FALSE # file produced by Miguel Poblete, circulated by Oliver Fricko by email on 2 November as part of the SSP update review round 1
          ) %>% iamc_wide_to_long(upper.to.lower = T) %>%
            filter(year==2020, scenario=="SSP2 - Review Phase 1") %>%
            mutate(iso = countrycode(region, "country.name", "iso3c")) %>%
            rename(gini=value) %>%
            select(iso,gini) %>%
            mutate(source = "Poblete et al.", year="2020 (email Oliver Fricko dated 01/11/2023)")
        ) %>%
        # world bank values
        bind_rows(
          vroom(
            here("data-raw", "dls_data", "gini", "world bank", "API_SI.POV.GINI_DS2_en_csv_v2_6299629", "API_SI.POV.GINI_DS2_en_csv_v2_6299629.csv"),
            show_col_types=FALSE,
            skip = 4
          ) %>% wb_parse() %>% group_by(iso) %>%
            slice_max(order_by = year, n = 1) %>%
            rename(gini = value) %>%
            select(iso,gini) %>%
            mutate(source = "World Bank", year="Latest available (downloaded 12/01/2024)")
        )



      p.gini.compare <- ggplot(income.gini.compare %>%
                                 select(-year) %>%
                                 pivot_wider(values_from = gini, names_from = source),
                               aes(x=`Min et al.`,y=`World Bank`)) +
        geom_point() +
        geom_abline(slope = 1) +
        geom_abline(slope = 1, intercept = -10, linetype = "dashed") +
        geom_abline(slope = 1, intercept = 10, linetype = "dashed")
      p.gini.compare

      write_delim(income.gini.compare,
                  here("data-raw", "dls_data", "gini", "gini_compare_collection_v20241111_v2.csv"),
                  delim = ",")



      # Load SHAPE GDP data ----------------------------------------------------
      population.iso <- read_csv(
        here(get_data_location_raw(test=FALSE),
             "scenario_data",
             "population",
             "SHAPE_pop_sep2023update_fixssp2CYP.csv"), show_col_types=FALSE
      ) %>%
        iamc_wide_to_long(upper.to.lower = F) %>% rename(iso=region, pop_mil=value) #df.core %>% select(model,scenario,iso,year,pop_mil) %>% distinct()
      population.iso.2020 <- population.iso %>% filter(year==2020)
      population.iso.2020.base <- population.iso.2020 %>% filter(model == "REMIND-MAgPIE 3.2-4.6", scenario == "SSP2-NPi") %>% select(iso,year,pop_mil)

      gdp.iso <- read_csv(
        here(get_data_location_raw(test=FALSE),
             "scenario_data",
             "population",
             "SHAPE_gdp_sep2023update_fixssp2HRV.csv"), show_col_types=FALSE
      ) %>%
        iamc_wide_to_long(upper.to.lower = F) %>%
        bind_rows(population.iso %>% rename(value=pop_mil, region=iso)) %>%
        to_per_capita() %>% mutate(unit = "thousand $2005/cap/yr") %>%
        rename(iso=region, gdp_thousand_percapita=value)
      gdp.SHAPE.2020 <- gdp.iso %>% filter(
        model == "REMIND-MAgPIE 3.2-4.6",
        scenario == "SDP_RC-1p5C",
      ) %>%
        filter(year==2020) %>% select(iso,gdp_thousand_percapita) %>%
        rename(gdp.pcap=gdp_thousand_percapita)

      # carbon inequality (chancel et al. 2022) ----------------------------------
      install.packages('haven')
      library(haven)
      carbon.data <- read_dta("C:/Users/kikstra/OneDrive - IIASA/_Other/Data/Inequality data/Chancel2022_natsust/Codes & Data/Data/distributional-footprints-1980-2019-scenarios-final.dta") %>%
        mutate(iso = countrycode::countrycode(iso, "iso2c", "iso3c")) %>%
        drop_na(iso) %>%  # pull(iso) %>% unique() # 173 countries
        # # for regional aggregate data, like:regions_scenario_carbon_temp2-tab.dta
        # filter(p %in%c(
        #   "p0p1",
        #   "p1p2",
        #   "p2p3",
        #   "p3p4",
        #   "p4p5",
        #   "p5p6",
        #   "p6p7",
        #   "p7p8",
        #   "p8p9",
      #   "p9p10",
      #   "p10p11",
      #   "p11p12",
      #   "p12p13",
      #   "p13p14",
      #   "p14p15",
      #   "p15p16",
      #   "p16p17",
      #   "p17p18",
      #   "p18p19",
      #   "p19p20",
      #   "p20p21",
      #   "p21p22",
      #   "p22p23",
      #   "p23p24",
      #   "p24p25",
      #   "p25p26",
      #   "p26p27",
      #   "p27p28",
      #   "p28p29",
      #   "p29p30",
      #   "p30p31",
      #   "p31p32",
      #   "p32p33",
      #   "p33p34",
      #   "p34p35",
      #   "p35p36",
      #   "p36p37",
      #   "p37p38",
      #   "p38p39",
      #   "p39p40",
      #   "p40p41",
      #   "p41p42",
      #   "p42p43",
      #   "p43p44",
      #   "p44p45",
      #   "p45p46",
      #   "p46p47",
      #   "p47p48",
      #   "p48p49",
      #   "p49p50",
      #   "p50p51",
      #   "p51p52",
      #   "p52p53",
      #   "p53p54",
      #   "p54p55",
      #   "p55p56",
      #   "p56p57",
      #   "p57p58",
      #   "p58p59",
      #   "p59p60",
      #   "p60p61",
      #   "p61p62",
      #   "p62p63",
      #   "p63p64",
      #   "p64p65",
      #   "p65p66",
      #   "p66p67",
      #   "p67p68",
      #   "p68p69",
      #   "p69p70",
      #   "p70p71",
      #   "p71p72",
      #   "p72p73",
      #   "p73p74",
      #   "p74p75",
      #   "p75p76",
      #   "p76p77",
      #   "p77p78",
      #   "p78p79",
      #   "p79p80",
      #   "p80p81",
      #   "p81p82",
      #   "p82p83",
      #   "p83p84",
      #   "p84p85",
      #   "p85p86",
      #   "p86p87",
      #   "p87p88",
      #   "p88p89",
      #   "p89p90",
      #   "p90p91",
      #   "p91p92",
      #   "p92p93",
      #   "p93p94",
      #   "p94p95",
      #   "p95p96",
      #   "p96p97",
      #   "p97p98",
      #   "p98p99",
      #   "p99p100"
      # ))
      filter(group!="p90p100") %>%
        mutate(
          # add population share as numeric
          # groups are: "p0p50"   "p50p90"  "p90p99"  "p99p100" (and "p90p100"; not used)
          population = ifelse(
            group=="p0p50", 0.5,
            ifelse(
              group=="p50p90", 0.4,
              ifelse(
                group=="p90p99", 0.09,
                ifelse(
                  group=="p99p100", 0.01,
                  NA
                )
              )
            )
          )
        ) %>%
        # use `lpfghgmulti10` (guess from Chancel code, figure 6AB)
        select(iso,year,population,lpfghgmulti10) %>%
        filter(year==2019) # could also do 2010
    ghg_gini <- get_gini(carbon.data, var="lpfghgmulti10") %>%
      rename(ghg.gini=gini)





    # create data set for linear models to test --------------------------------


    gini.data <- vroom(
        here("data-raw", "SHAPE-final_gini_v3.csv"), show_col_types=FALSE # file produced by a script in data-raw called `prep_shape_gini_data.R` (last update: SHAPE_Gini_v1p3_annual.csv)
      ) %>% filter(year==2020, scenario=="RC") %>%
        rename(iso=region, gini=value) %>%
        select(iso,gini)
    load_iam_region_mappings()
    income.and.energy.data <- gini.data %>%
      left_join(gdp.SHAPE.2020, by = "iso") %>%
      left_join(load_urbanisation_projection(ssp="SSP2") %>% filter(year==2015),
                by = "iso") %>%
      mutate(fakecol = 1) %>%
      full_join(tibble(
        variable = data.based.ginis %>% pull(variable) %>% unique(),
        fakecol = 1
      ), relationship = "many-to-many", by = "fakecol") %>%
      select(-fakecol) %>%
      left_join(data.based.ginis, by = c("iso", "variable")) %>%
      left_join(regions.message, by = "iso") %>%
      left_join(regions.image, by = "iso") %>%
      left_join(load_official_country_grouping(grouping.to.load="region_ar6_6_ipcc_fgd"), by = "iso") %>%
      left_join(load_official_country_grouping(grouping.to.load="region_ar6_10_ipcc_fgd"), by = "iso") %>%
      left_join(load_official_country_grouping(grouping.to.load="Income _status_WB"), by = "iso") %>%
      left_join(load_official_country_grouping(grouping.to.load="Former Soviet Union") %>%
                  mutate_cond(is.na(`Former Soviet Union`), `Former Soviet Union`="Non-FSU"), by = "iso") %>%
      rename(income.status = `Income _status_WB`,
             fsu = `Former Soviet Union`) %>%
      left_join(ghg_gini, by = "iso") %>%

      # add non-linear forms
      mutate(
        log.gini = log(gini)
      )



    # create a dataframe to collect --------------------------------------------
    df.predicted.differences <- tibble()

    ### build models -----------------------------------------------------------
    regression.models <- c(

      "energy.gini ~ gini",
      "energy.gini ~ ghg.gini",
      "energy.gini ~ gini + gdp.pcap",
      "energy.gini ~ gini + urb.rate",
      "energy.gini ~ ghg.gini + gdp.pcap",
      "energy.gini ~ ghg.gini + urb.rate",
      "energy.gini ~ gini + ghg.gini",

      "energy.gini ~ gini + region_ar6_6_ipcc_fgd",
      "energy.gini ~ ghg.gini + region_ar6_6_ipcc_fgd",
      "energy.gini ~ gini + gdp.pcap + region_ar6_6_ipcc_fgd",
      "energy.gini ~ gini + urb.rate + region_ar6_6_ipcc_fgd",
      "energy.gini ~ ghg.gini + gdp.pcap + region_ar6_6_ipcc_fgd",
      "energy.gini ~ ghg.gini + urb.rate + region_ar6_6_ipcc_fgd",
      "energy.gini ~ gini + ghg.gini + region_ar6_6_ipcc_fgd",

      "energy.gini ~ gini + region_ar6_10_ipcc_fgd",
      "energy.gini ~ ghg.gini + region_ar6_10_ipcc_fgd",
      "energy.gini ~ gini + gdp.pcap + region_ar6_10_ipcc_fgd",
      "energy.gini ~ gini + urb.rate + region_ar6_10_ipcc_fgd",
      "energy.gini ~ ghg.gini + gdp.pcap + region_ar6_10_ipcc_fgd",
      "energy.gini ~ ghg.gini + urb.rate + region_ar6_10_ipcc_fgd",
      "energy.gini ~ gini + ghg.gini + region_ar6_10_ipcc_fgd",

      "energy.gini ~ gini + income.status",
      "energy.gini ~ ghg.gini + income.status",
      "energy.gini ~ gini + gdp.pcap + income.status",
      "energy.gini ~ gini + urb.rate + income.status",
      "energy.gini ~ ghg.gini + gdp.pcap + income.status",
      "energy.gini ~ ghg.gini + urb.rate + income.status",
      "energy.gini ~ gini + ghg.gini + income.status",

      "energy.gini ~ gini + fsu",
      "energy.gini ~ ghg.gini + fsu",
      "energy.gini ~ gini + gdp.pcap + fsu",
      "energy.gini ~ gini + urb.rate + fsu",
      "energy.gini ~ ghg.gini + gdp.pcap + fsu",
      "energy.gini ~ ghg.gini + urb.rate + fsu",
      "energy.gini ~ gini + ghg.gini + fsu",


      # also add log(gini) for explorative comparison
      "energy.gini ~ log.gini",
      "energy.gini ~ log.gini + income.status"

    )




    for (reg.model in regression.models){

      model.transport <- lm(
        formula = as.formula(reg.model),
        data = income.and.energy.data %>% filter(variable == "Final Energy|Transportation")
      )
      summary(model.transport)
      model.rc <- lm(
        formula = as.formula(reg.model),
        data = income.and.energy.data %>% filter(variable == "Final Energy|Residential and Commercial")
      )
      summary(model.rc)
      model.ind <- lm(
        formula = as.formula(reg.model),
        data = income.and.energy.data %>% filter(variable == "Final Energy|Industry")
      )
      summary(model.ind)
      model.tot <- lm(
        formula = as.formula(reg.model),
        data = income.and.energy.data %>% filter(variable == "Final Energy")
      )
      summary(model.tot)

      # predict missing values
      pred.transport.data <- income.and.energy.data %>% filter(variable == "Final Energy|Transportation")
      pred.transport <- pred.transport.data %>%
        mutate(pred = predict(model.transport, newdata = pred.transport.data))
      pred.rc.data <- income.and.energy.data %>% filter(variable == "Final Energy|Residential and Commercial")
      pred.rc <- pred.rc.data %>%
        mutate(pred = predict(model.rc, newdata = pred.rc.data))
      pred.ind.data <- income.and.energy.data %>% filter(variable == "Final Energy|Industry")
      pred.ind <- pred.ind.data %>%
        mutate(pred = predict(model.ind, newdata = pred.ind.data))
      pred.tot.data <- income.and.energy.data %>% filter(variable == "Final Energy")
      pred.tot <- pred.tot.data %>%
        mutate(pred = predict(model.tot, newdata = pred.tot.data))

      new.ginis.with.pred <- pred.tot %>%
        bind_rows(pred.transport) %>%
        bind_rows(pred.rc) %>%
        bind_rows(pred.ind)


        #define function to extract overall p-value of model
        overall_p <- function(my_model) {
          f <- summary(my_model)$fstatistic
          p <- pf(f[1],f[2],f[3],lower.tail=F)
          attributes(p) <- NULL
          return(p)
        }


        df.predicted.differences <- df.predicted.differences %>%
          bind_rows(
            new.ginis.with.pred %>%
              mutate(diff=energy.gini-pred) %>%
              mutate(regression.model = reg.model) %>%
              mutate(

                # get R^2 values
                r2.transport = summary(model.transport)$adj.r.squared,
                r2.rc = summary(model.rc)$adj.r.squared,
                r2.ind = summary(model.ind)$adj.r.squared,
                r2.tot = summary(model.tot)$adj.r.squared,

                # get p-values
                p.transport = overall_p(model.transport),
                p.rc = overall_p(model.rc),
                p.ind = overall_p(model.ind),
                p.tot = overall_p(model.tot)

              )
          )
      }


      # write it our for further analysis
      df.predicted.differences %>%
        write_delim(file = here("analyses", "tables", "shape_v1_1_0", "energygini_filling_predicted_differences.csv"), delim=",")


    } else if (TEST.GINI.MODELS==FALSE){

      # run_initiatialization_input_data()
      # income.data <- economic_data_helper(fname_gini = "wiid_gini.csv")
      # income.data <- vroom(here("data-raw", "gini_ssp.csv"), show_col_types=FALSE) %>% # could also be changed to WID database if we want to stick more closely to empirical data, instead of the completness that this source offers
      #   filter(country != "World") %>%
      #   filter(year == 2015, scenario == "SSP2") %>%
      #   select(-year, -scenario) %>%
      #   rename(iso = country)
      gini.data <- vroom(
        here("data-raw", "SHAPE-final_gini_v3.csv"), show_col_types=FALSE # file produced by a script in data-raw called `prep_shape_gini_data.R` (last update: SHAPE_Gini_v1p3_annual.csv)
      ) %>% filter(year==2020, scenario=="RC") %>%
        rename(iso=region, gini=value) %>%
        select(iso,gini)
      load_iam_region_mappings()
      income.and.energy.data <- gini.data %>%
        mutate(fakecol = 1) %>%
        full_join(tibble(
          variable = data.based.ginis %>% pull(variable) %>% unique(),
          fakecol = 1
        ), relationship = "many-to-many", by = "fakecol") %>%
        select(-fakecol) %>%
        left_join(data.based.ginis, by = c("iso", "variable")) %>%
        left_join(load_official_country_grouping(grouping.to.load="Income _status_WB"), by = "iso") %>%
        rename(income.status = `Income _status_WB`)

      reg.model <- "energy.gini ~ gini + income.status"

      model.transport <- lm(
        formula = as.formula(reg.model),
        data = income.and.energy.data %>% filter(variable == "Final Energy|Transportation")
      )
      summary(model.transport)
      model.rc <- lm(
        formula = as.formula(reg.model),
        data = income.and.energy.data %>% filter(variable == "Final Energy|Residential and Commercial")
      )
      summary(model.rc)
      model.ind <- lm(
        formula = as.formula(reg.model),
        data = income.and.energy.data %>% filter(variable == "Final Energy|Industry")
      )
      summary(model.ind)
      model.tot <- lm(
        formula = as.formula(reg.model),
        data = income.and.energy.data %>% filter(variable == "Final Energy")
      )
      summary(model.tot)

      # predict missing values
      pred.transport.data <- income.and.energy.data %>% filter(variable == "Final Energy|Transportation")
      pred.transport <- pred.transport.data %>%
        mutate(pred = predict(model.transport, newdata = pred.transport.data))
      pred.rc.data <- income.and.energy.data %>% filter(variable == "Final Energy|Residential and Commercial")
      pred.rc <- pred.rc.data %>%
        mutate(pred = predict(model.rc, newdata = pred.rc.data))
      pred.ind.data <- income.and.energy.data %>% filter(variable == "Final Energy|Industry")
      pred.ind <- pred.ind.data %>%
        mutate(pred = predict(model.ind, newdata = pred.ind.data))
      pred.tot.data <- income.and.energy.data %>% filter(variable == "Final Energy")
      pred.tot <- pred.tot.data %>%
        mutate(pred = predict(model.tot, newdata = pred.tot.data))

      new.ginis.with.pred <- pred.tot %>%
        bind_rows(pred.transport) %>%
        bind_rows(pred.rc) %>%
        bind_rows(pred.ind)



    }


    # select (preferred) energy gini data ------------------------
    new.ginis <- new.ginis.with.pred %>% #filter(regression.model == "energy.gini ~ gini + region.message") %>%
      mutate_cond(is.na(energy.gini), energy.gini = pred) %>%
      select(iso, variable, energy.gini) %>%
      arrange(iso, variable)
  }

}

# new final energy ginis with filling based on prediction
saveRDS(new.ginis,
  file = here(DATA.LOCATION, "FE_gini_iso.RData")
)
