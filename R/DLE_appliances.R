#' Class DLE.dimension.clothing
#'
#' @description subclass for appliances dimension (inheriting DLE.dimension)

DLE.dimension.appliances <-
  setRefClass("DLE.dimension.appliances",
    contains = "DLE.dimension", # inherit the DLE.dimension class

    fields = list(

      # DLS indicator of this dimension
      lifetime.list = "list"
    ),
    methods = list(
      initialize = function(...) {
        print("initialize: appliances")
        callSuper(...)

        lifetime.list <<- list(
          lt.mobile_telephone = 5, lt.television = 10,
          lt.clean_cooking_fuel = 10, lt.refrigerator = 15
        )
      },

      #' @description
      #' Derive DLS thresholds from data if not pre-determined
      DeriveThreshold = function() {
        print("DeriveThreshold: appliances")

        # note:
        # -- make threshold more specific: e.g. mobile phone could be based on population above 10yr, like in GEC paper

        DF.DLS <<- DF.DLS %>% mutate(thres = 1)
      },

      #' @description
      #' Overwrite the same-named function in the superclass to be specific to this dimension
      IdentifyGap = function(data.version="WB20230922_and_GDL20231010") {
        print("IdentifyGap: appliances")

        if(data.version=="WB20230922_and_GDL20231010"){


          base.df.appliances <- message.R11 %>% select(iso) %>%
            crossing(grp=c("clean_cooking_fuel","television","mobile_telephone","refrigerator")) %>%
            left_join(load_official_country_grouping(grouping.to.load = "Income _status_WB")) %>%
            left_join(message.R11 %>% select(iso, R11.region))


          # auxiliary functions - World Bank
          wb_load_csv <- function(path) {

            return(
              read_csv(path, skip = 4, col_names = T) %>%
                wb_parse() %>%
                mutate(value = value/100)
            )

          }

          # auxiliary functions - DHS STATcompiler
          statcompiler_load_and_parse_databaseformat <- function(path,
                                                                 number.of.included.indicators = 6) {

            df <- read_excel(path, sheet = "Indicator Data", skip = 0, col_names = T) %>%
              select(`Country Name`, `Survey Year`, `Indicator`, `Value`) %>%
              slice(1:(n() - (number.of.included.indicators+2))) %>%
              rename(value = Value, indicator = Indicator, year = `Survey Year`) %>%
              mutate(value = value/100)
            df$iso <- countrycode(df$`Country Name`, "country.name", "iso3c")

            return(
                df %>%
                  select(iso,indicator,year,value) %>%
                  arrange(iso,indicator,year) %>%
                  drop_na()
            )

          }

          # auxiliary functions - global_data_lab
          globaldatalab_load_and_parse_databaseformat_national <- function(path,
                                                                  new.indicator.name=NA) {

            df <- read_csv(path, skip = 0, col_names = T) %>%
              filter(Region=="Total") %>% # in case the data also has sub-national data
              select(ISO_Code, where(is.numeric)) %>%
              rename(iso=ISO_Code) %>%
              pivot_longer(names_to = "year", values_to = "value", cols = -c("iso")) %>%
              mutate(year = as.numeric(year),
                     value = value/100,
                     indicator = new.indicator.name)

            return(
              df %>%
                select(iso,indicator,year,value) %>%
                arrange(iso,indicator,year) %>%
                drop_na()
            )

          }

          # world bank data:
          path.cooking <- here("data-raw", "dls_data",
                               "appliances_clean_cooking", "clean_fuels_and_technologies",
                               "API_EG.CFT.ACCS.ZS_DS2_en_csv_v2_5873371.csv")
          data.cooking <- wb_load_csv(path.cooking) %>%
            mutate(grp="clean_cooking_fuel") %>%
            mutate(deprivation.headcount = 1-value) %>%
            select(iso,grp,year,value,deprivation.headcount)

          # global data lab:
          path.television <- here("data-raw", "dls_data",
                                  "appliances_television", "globaldatalab",
                                  "GDL-_-households-with-a-TV-data.csv")
          data.television <- globaldatalab_load_and_parse_databaseformat_national(path.television,
                                                                                  new.indicator.name = "television") %>%
            rename(grp=indicator) %>%
            mutate(deprivation.headcount = 1-value) %>%
            select(iso,grp,year,value,deprivation.headcount)
          path.mobile_telephone <- here("data-raw", "dls_data",
                                        "appliances_mobile_telephone", "globaldatalab",
                                        "GDL-_-households-with-a-cellphone-data.csv")
          data.mobile_telephone <- globaldatalab_load_and_parse_databaseformat_national(path.mobile_telephone,
                                                                                        new.indicator.name = "mobile_telephone") %>%
            rename(grp=indicator) %>%
            mutate(deprivation.headcount = 1-value) %>%
            select(iso,grp,year,value,deprivation.headcount) %>%
            filter(!iso%in%c("MEX"))
          path.refrigerator <- here("data-raw", "dls_data",
                                    "appliances_refrigerator", "globaldatalab",
                                    "GDL-_-households-with-a-refrigerator-data.csv")
          data.refrigerator <- globaldatalab_load_and_parse_databaseformat_national(path.refrigerator,
                                                                                    new.indicator.name = "refrigerator") %>%
            rename(grp=indicator) %>%
            mutate(deprivation.headcount = 1-value) %>%
            select(iso,grp,year,value,deprivation.headcount)



          # dhs STATcompiler data:
          path.STATcompiler.data <- here("data-raw", "dls_data",
                                         "appliances_STATcompiler",
                                         "STATcompilerExport20231010_10548.xlsx")
          data.appliances <- statcompiler_load_and_parse_databaseformat(path.STATcompiler.data) %>%
            # mutate(grp = ifelse(grepl("clean fuel", grp, fixed = TRUE), "clean_cooking_fuel", ifelse(grepl("television", grp, fixed = TRUE), "television", ifelse(grepl("mobile telephone", grp, fixed = TRUE), "mobile_telephone", ifelse(grepl("refrigerator", grp, fixed = TRUE), "refrigerator", NA))))) %>%
            # mutate(grp="clean_cooking_fuel") %>%
            mutate(grp=indicator) %>%
            mutate(deprivation.headcount = 1-value) %>%
            select(iso,grp,year,value,deprivation.headcount)

          # combined data (with world bank country mapping)
          combined.data <- data.appliances %>% mutate(source = "DHS") %>%
            bind_rows(data.cooking %>% mutate(source = "World Bank")) %>%
            bind_rows(data.television %>% mutate(source = "Global Data Lab")) %>%
            bind_rows(data.mobile_telephone %>% mutate(source = "Global Data Lab")) %>%
            bind_rows(data.refrigerator %>% mutate(source = "Global Data Lab")) %>%
            left_join(load_official_country_grouping(grouping.to.load = "Income _status_WB")) %>%
            left_join(message.R11 %>% select(iso, R11.region))


          # # check raw data:
          # p.appliances.headcount.WB <- ggplot(
          #   combined.data,
          #   aes(x = year, y = deprivation.headcount, colour = source)
          # ) +
          #   facet_grid(`Income _status_WB`~grp) +
          #   geom_point() +
          #   geom_smooth()
          # p.appliances.headcount.WB
          #
          # p.appliances.headcount.R11 <- ggplot(
          #   combined.data,
          #   aes(x = year, y = deprivation.headcount, colour = source)
          # ) +
          #   facet_grid(`R11.region`~grp) +
          #   geom_point() +
          #   geom_smooth()
          # p.appliances.headcount.R11
          #
          # save_ggplot(p=p.appliances.headcount, f = here("analyses", "figures", "appliances_headcount_rawdata"))
          #
          # write_delim(x = combined.data, file = here("analyses", "tables", "appliances_headcount_rawdata.csv"), delim = ",")


          # choose and fill in missing data:

          # combined.data %>% distinct(grp,source)
          combined.data.cleaned <- combined.data %>%
            filter(
              (grp == "clean_cooking_fuel" & source == "World Bank") |
                # (grp %in% c("Households possessing a television",
                #             "Households possessing a mobile telephone",
                #             "Households possessing a refrigerator") & source == "DHS") |
                (grp %in% c("television",
                            "mobile_telephone",
                            "refrigerator") & source == "Global Data Lab")
            ) %>%
              mutate_cond(grepl("television", grp, fixed = TRUE), grp="television") %>%
              mutate_cond(grepl("mobile telephone", grp, fixed = TRUE), grp="mobile_telephone") %>%
              mutate_cond(grepl("refrigerator", grp, fixed = TRUE), grp="refrigerator")

          combined.data.year.base <- combined.data.cleaned %>% filter(year==year.base, !is.na(deprivation.headcount))
          # combined.data.year.base %>% group_by(grp) %>% count()
          # combined.data.cleaned %>% distinct(grp,source)
          # combined.data %>% distinct(grp,source)


          combined.data.filler.data <- combined.data.cleaned %>%
            filter(year>=(year.base-3), year<=(year.base+3))
          # p.appliances.headcount.fill.cooking.reg <- ggplot(
          #   combined.data.filler.data %>% left_join(combined.data.year.base %>% filter(grp=="clean_cooking_fuel") %>% rename(deprivation.headcount.cooking=deprivation.headcount) %>% select(iso,deprivation.headcount.cooking)),
          #   aes(x = deprivation.headcount.cooking, y = deprivation.headcount, colour = `year`)
          # ) +
          #   facet_grid(as.factor(year)~grp) +
          #   geom_point() +
          #   geom_smooth(method = "lm") + ylim(c(0,1))
          # p.appliances.headcount.fill.cooking.reg

          # by year and WB income group filling (e.g. for mobile telephone):
          combined.data.filler.data.by.year <- combined.data.filler.data %>%
            group_by(grp, year, `Income _status_WB`) %>%
            mutate(filler.by.year = mean(deprivation.headcount)) %>% ungroup() %>%
            distinct(grp,`Income _status_WB`,year,filler.by.year) %>%
            arrange(grp,`Income _status_WB`,year)
          # p.fill.by.year <- ggplot(combined.data.filler.data.by.year,
          #                          aes(x=year, y = filler.by.year)) +
          #   facet_grid(~grp) +
          #   geom_line(aes(colour=`Income _status_WB`))
          # p.fill.by.year

          # by region filling? (e.g. for fridge and television)
          combined.data.filler.data.by.region <- combined.data.filler.data %>%
            group_by(grp, `R11.region`) %>%
            mutate(filler.by.region = mean(deprivation.headcount)) %>% ungroup() %>%
            distinct(grp,`R11.region`,filler.by.region) %>%
            arrange(grp,`R11.region`)
          # p.fill.by.region <- ggplot(combined.data.filler.data.by.region,
          #                          aes(x=R11.region, y = filler.by.region)) +
          #   facet_grid(~grp) +
          #   geom_col(aes(fill=`R11.region`))
          # p.fill.by.region

          # combined.data.fillmethod.compare <- combined.data.filler.data %>%
          #   left_join(combined.data.filler.data.by.year %>% filter(year==year.base) %>% select(-year)) %>%
          #   left_join(combined.data.filler.data.by.region) %>%
          #   filter(`Income _status_WB`!="High", !is.na(R11.region)) %>%
          #   mutate(filler.combined = (filler.by.year + filler.by.region)/2 ) %>%
          #   mutate(diff.year = deprivation.headcount-filler.by.year,
          #          diff.region = deprivation.headcount-filler.by.region,
          #          diff.combined = (deprivation.headcount - filler.combined) ) %>%
          #   filter(grp!="clean_cooking_fuel",
          #          !(R11.region%in%GN),
          #          `Income _status_WB`!="High",
          #          year>=(year.base-1), year<=(year.base+1))

          # # compared averaging methods
          # combined.data.fillmethod.compare %>% pull(diff.year) %>% abs() %>% sum()
          # combined.data.fillmethod.compare %>% pull(diff.region) %>% abs() %>% sum()
          # combined.data.fillmethod.compare %>% pull(diff.combined) %>% abs() %>% sum()
          # combined.data.fillmethod.compare %>% pull(diff.year) %>% abs() %>% sd()
          # combined.data.fillmethod.compare %>% pull(diff.region) %>% abs() %>% sd()
          # combined.data.fillmethod.compare %>% pull(diff.combined) %>% abs() %>% sd()
          #
          # p.fillmethod.compare.by.region <- ggplot(
          #   combined.data.fillmethod.compare%>%
          #     filter(year>=(year.base-0), year<=(year.base+0))
          # ) +
          #   facet_grid(~grp) +
          #   geom_point(aes(x=deprivation.headcount,y=filler.by.region,colour=year))
          # p.fillmethod.compare.by.year <- ggplot(
          #   combined.data.fillmethod.compare %>%
          #     filter(year>=(year.base-0), year<=(year.base+0))
          # ) +
          #   facet_grid(~grp) +
          #   geom_point(aes(x=deprivation.headcount,y=filler.by.year,colour=year))
          # p.fillmethod.compare.combined <- ggplot(
          #   combined.data.fillmethod.compare %>%
          #     filter(year>=(year.base-0), year<=(year.base+0))
          # ) +
          #   facet_grid(~grp) +
          #   geom_point(aes(x=deprivation.headcount,y=filler.combined,colour=year))
          #
          #
          #
          # # compare with gdp/cap method
          # p.fillmethod.gdp <-
          # ggplot(
          #   combined.data.fillmethod.compare %>%
          #     filter(year>=(year.base-0), year<=(year.base+0)) %>%
          #     left_join(gdp %>% filter(year==2015) %>% select(-year))
          # ) +
          #   facet_grid(~grp) +
          #   geom_point(aes(x=deprivation.headcount,y=log(gdp.pcap),colour=year))
          #
          # p.fillmethod.compare <- p.fillmethod.compare.by.region / p.fillmethod.compare.by.year / p.fillmethod.compare.combined / p.fillmethod.gdp
          # p.fillmethod.compare

          GN <- c("EEU", "FSU", "PAO", "WEU", "NAM")

          combined.data.filler <- base.df.appliances %>%
            left_join(combined.data.filler.data.by.year %>% filter(year==year.base) %>% select(`Income _status_WB`,grp,filler.by.year)) %>%
            left_join(combined.data.filler.data.by.region %>% select(R11.region,grp,filler.by.region)) %>%
            mutate(filler.combined = (filler.by.region+filler.by.year)/2) %>% # default filler: a combination of year, WB income level, and R11 region
            mutate_cond(is.na(filler.combined), filler.combined=filler.by.region) %>%  # if no combination is available, fill based on only R11 region
            mutate(pre.rich.adj = filler.combined) %>%
            mutate_cond(((`Income _status_WB`%in%c("High")) | ((R11.region%in%GN))), filler.combined=0) %>% # rich / global north countries; if no data, fill in no gap.
            mutate(rich.adj.diff = abs(pre.rich.adj - filler.combined))


          appliances_new_updated <- base.df.appliances %>%
            # add source data for 2015
            left_join(combined.data.year.base %>% select(iso,grp,value,deprivation.headcount)) %>%
            # add filler data
            left_join(combined.data.filler %>% select(iso,grp,filler.combined)) %>%
            mutate_cond(is.na(deprivation.headcount),
                        deprivation.headcount = filler.combined,
                        value = 1-filler.combined) %>%
            select(iso, grp, value, deprivation.headcount)

          # below, a comparison with data.version=="ERL2021" is performed (which was loaded in as `appliances_new`)
          # p.update.2023 <- ggplot(
          #   appliances_new_updated %>% mutate(source = "WB20230922_and_GDL20231010") %>%
          #     bind_rows(appliances_new %>% mutate(source = "ERL2021")) %>%
          #     left_join(appliances_new_updated %>% rename(`WB20230922_and_GDL20231010`=deprivation.headcount) %>% select(-value))%>%
          #     left_join(appliances_new %>% rename(`ERL2021`=deprivation.headcount) %>% select(-value)),
          #   aes(x=iso,y=deprivation.headcount,colour=source,group=iso)
          # ) +
          #   facet_grid(~grp) +
          #   geom_segment(aes(y=ERL2021,yend=WB20230922_and_GDL20231010,x=iso,xend=iso),colour="black") +
          #   geom_point() +
          #   coord_flip() +
          #   theme_minimal()
          #
          # save_ggplot(p=p.update.2023, f = here("analyses", "figures", "appliances_headcount_dataupdate_2023_v1"), h = 600, w = 300)

          # appliances_new

          appliances_new <- appliances_new_updated


        } else if (data.version=="ERL2021"){

          fname_appliances <- "/appliances_database_recent_from2011andnewer.xlsx" # DHS recent data, downloaded from https://www.statcompiler.com/en/
          nrows_remove <- 6
          # read in penetration rate (in % of households having access to ...)
          appliances <- read_excel(paste0(data.path, "/Appliances", fname_appliances), sheet = "Indicator Data", skip = 0, col_names = TRUE) %>%
            select(`Country Name`, `Survey Year`, `Indicator`, `Value`) %>%
            slice(1:(n() - nrows_remove)) # %>%
          # rename(iso = 'Country Code')
          appliances$iso <- countrycode(appliances$`Country Name`, "country.name", "iso3c")
          # convert to ratio
          appliances$Value <- appliances$Value / 100

          # take average across available years if required
          appliances <- appliances %>%
            select(iso, `Survey Year`, `Indicator`, `Value`) %>%
            group_by(iso, `Indicator`) %>%
            summarise_at(vars(-`Survey Year`), funs(mean(., na.rm = TRUE)))


          # add R11
          appliances <- appliances %>% spread(Indicator, Value) # spread first
          appliances <- select(message.R11, c(iso, R11.region)) %>% # get all countries from R11
            left_join(appliances, by = "iso")
          appliances <- appliances %>% gather("appliance", "value", 3:6) # bring back to long

          # add zero if in GN, add average if in GS
          GN <- c("EEU", "FSU", "PAO", "WEU", "NAM")
          GS <- c("LAM", "SAS", "AFR", "MEA", "PAS", "CPA")
          appliances[appliances == "NaN"] <- NA
          appliances_new <- appliances %>%
            group_by(R11.region, appliance) %>%
            mutate(fillvalue = ifelse((R11.region %in% GN & is.na(value)), 1,
                                      ifelse((R11.region %in% GS & is.na(value)), mean(na.omit(value)),
                                             value
                                      )
            )) %>%
            mutate(deprivation.headcount = 1 - fillvalue) %>%
            group_by(iso) %>%
            select(iso, appliance, fillvalue, deprivation.headcount) %>%
            rename(value = fillvalue, grp = appliance) %>%
            mutate(grp = ifelse(grepl("clean fuel", grp, fixed = TRUE), "clean_cooking_fuel", ifelse(grepl("television", grp, fixed = TRUE), "television", ifelse(grepl("mobile telephone", grp, fixed = TRUE), "mobile_telephone", ifelse(grepl("refrigerator", grp, fixed = TRUE), "refrigerator", NA)))))

        }


        DF.DLS <<- DF.DLS %>%
          left_join(appliances_new, by = c("iso", "grp")) %>% # Join data: appliances
          mutate(gap = thres - value) %>%
          select(-value) %>%
          mutate(deprivation.headcount = gap)

        DF.DLS[DF.DLS < 0] <<- 0
      },

      #' @description
      #' Overwrite the same-named function in the superclass to be specific to this dimension
      DeriveEnergyIntensity = function() {
        print("DeriveEnergyIntensity: appliances")

        # read in energy intensity values
        fname_appliances_EIs <- "/energyintensities.xlsx"

        # average intensities for OP and CON [GJ/unit] (elec, non.elec)
        EIs <- read_excel(paste0(data.path, "/Appliances", fname_appliances_EIs), sheet = "Data", skip = 0, col_names = TRUE)
        # note: includes appliance AC, which is not used here (but accounted for in cooling_con)

        # convert to standard MJ
        EIs$e.int <- EIs$e.int / mega * giga
        EIs <- EIs %>%
          mutate(unit = ifelse(grepl("GJ/unit", unit, fixed = TRUE), "MJ/unit", unit)) %>%
          mutate(unit = ifelse(grepl("GJ/yr", unit, fixed = TRUE), "MJ/yr", unit)) %>%
          # project EI of CON.new on CON.new.pop and CON.new.dls
          add_ei_for_CONnewDLSPOPsplit()

        # join energy intensities
        DF.tei <<- left_join(group_by(DF.tei, iso), select(EIs, -c(unit, year)), by = c("grp", "type", "elec")) %>%
          mutate(e.int = coalesce(as.numeric(e.int.x), e.int.y)) %>%
          select(-c(e.int.x, e.int.y))
      },

      #' @description
      #' Construct rollout scenario based on identified gap and other assumptions.
      #' This can be coded differently for different scenarios.
      ConstructRolloutScenario = function(scen, yr.tgt = year.target, yr.st.ro=year.base) {
        # grp-specific replacement values:
        r.rep.grp <- TRUE

        r.rep.mobile_telephone <- 1 / lifetime.list$lt.mobile_telephone
        r.rep.television <- 1 / lifetime.list$lt.television
        r.rep.clean_cooking_fuel <- 1 / lifetime.list$lt.clean_cooking_fuel
        r.rep.refrigerator <- 1 / lifetime.list$lt.refrigerator

        DF.rollout <<- DF.rollout %>%
          mutate(r.rep.pergrp = ifelse(type == "CON.rep",
            ifelse(grepl("mobile_telephone", grp), r.rep.mobile_telephone,
              ifelse(grepl("television", grp, fixed = TRUE), r.rep.television,
                ifelse(grepl("clean_cooking_fuel", grp, fixed = TRUE), r.rep.clean_cooking_fuel,
                  ifelse(grepl("refrigerator", grp, fixed = TRUE), r.rep.refrigerator,
                    NA
                  )
                )
              )
            ),
            NA
          ))




        if (scen == "Income") {
          print("ConstructRolloutScenario:  Appliance - Income")

          callSuper(scen, yr.tgt, r.rep.grp = TRUE)
        } else if (scen == "Income.regression") {
          print("ConstructRolloutScenario:  Appliance - Income Regression")

          callSuper(scen, yr.tgt, r.rep.grp = TRUE)
        } else if (scen == "ACCEL") {
          print("ConstructRolloutScenario:  Appliance - ACCEL")

          callSuper(scen, yr.tgt, r.rep.grp = TRUE, year.start.rollout=yr.st.ro)
        } else {
        }
      }
    )
  )
