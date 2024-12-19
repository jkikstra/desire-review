#' Class DLE.dimension.transport
#'
#' @description subclass for transport dimension (inheriting DLE.dimension)

DLE.dimension.transport <-
  setRefClass("DLE.dimension.transport",
    contains = "DLE.dimension", # inherit the DLE.dimension class

    fields = list(

      # Master DF keeping DLS thresholds, gaps, and likely some dimension-specific data
      # In this base definition, per-capita threshold and gap are assume, but each dimension can diverge based on data.
      #' @field iso
      #' @field pkm_data   national mean pkm/cap/yr in base year
      #' @field nu    mean of log(income) distribution (income ~ lognormal)
      #' @field sigma sd of log(income) distribution
      #'
      #' ... how do gini and gdp come in here?! --> mean gdp now of course is mean of linear income...
      #' More dimension-specific fields can be here
      national.average.threshold = "numeric",
      urban.to.rural = "numeric",
      public.occupancy = "numeric",
      modal.share.base = "data.frame",
      pkm.data.gap.base = "data.frame", # added to more easily retrieve DoD data if so desired
      ei_data_all.with.totals = "data.frame", # added to more easily retrieve transport op data if so desired
      ei.data.op = 'data.frame', # added to more easily retrieve transport op data if so desired
      ei.data.con = 'data.frame', # added to more easily retrieve transport op data if so desired
      r.rep.grp = "logical",

      # from final energy intensities LCA [Rao et al. 2019]
      r.rep.car = "numeric",
      r.rep.twothree = "numeric",
      r.rep.bus = "numeric",
      r.rep.rail = "numeric"
    ),
    methods = list(
      initialize = function(...) { # df.input = data.frame(expenditure=0, HDD=0, hh_size=0)) {
        print("initialize: transport")
        callSuper(...)

        # transport dimension requires additional information to derive DLS thresholds by country/region.
        DF.DLS <<- data.frame(DF.DLS) # , df.input)

        national.average.threshold <<- 8527 # 8527 # (sensitivity 3652.5 = 10pkm/cap/day) -- Threshold: in p-km/cap/year corresponding to the average motorized passenger-km/cap within daily life area of Japan
        urban.to.rural <<- 1.24 # based on micro-data analysis of USA data
        public.occupancy <<- 0.5
        # one quick replacement rate to check running:
        # r.rep <- 1/10 # do sensitivities: in DLE assume (12 = car, 10 = motorcycle, bus = 12, rail = 40)

        # grp-specific replacement values:
        r.rep.grp <<- TRUE

        # from final energy intensities LCA [Rao et al. 2019]
        r.rep.car <<- 1 / 12
        r.rep.twothree <<- 1 / 10
        r.rep.bus <<- 1 / 12
        r.rep.rail <<- 1 / 40 # for both rail vehicles and railway construction (assumed maintenance costs)
      },

      #' @description
      #' Derive DLS thresholds from data if not pre-determined
      DeriveThreshold = function(data.version="IMAGE2023") {
        print("DeriveThreshold: transport")


        ## pkm current mode shares
        if(data.version=="IMAGE2023"){
          load_iam_region_mappings()
          fname_pkm <- "image_shape_ssp_20230825_addedtwothree_replacedCHN.xlsx"

          data_pkm_shares <- transport_pkm_helper(fname_pkm = fname_pkm,
                                                  data.version="IMAGE2023",
                                                  totals.or.shares = "shares") %>%
            pivot_wider(names_from = grp, values_from = pkm)  %>%
            rename(ldv = car)

          modal.share.base <<- data_pkm_shares # add to DLE object

          data_pkm_shares_all <- transport_pkm_helper(fname_pkm = fname_pkm,
                                                  data.version="IMAGE2023-all",
                                                  totals.or.shares = "shares") %>%
            pivot_wider(names_from = grp, values_from = pkm)  %>%
            rename(ldv = car)

          modal.share.all <<- data_pkm_shares_all # add to DLE object


        } else if (data.version=="ERL2021"){

          fname_pkm <- "/R11_pkm.xlsx" ## passenger-km shares - ICCT 2010
          data_pkm_shares_R11 <- read_excel(paste0(data.path, "/Transport", fname_pkm), sheet = "Data", col_names = TRUE)
          data_pkm_shares <- message.R11 %>%
            select(iso, R11.region) %>%
            left_join(data_pkm_shares_R11, by = c("R11.region")) %>%
            select(c(iso, ldv, twothree, bus, rail))

          modal.share.base <<- data_pkm_shares # add to DLE object

        }





        # # threshold parameters
        # national.average.threshold <- 8527 #Threshold: in p-km/cap/year corresponding to the average motorized passenger-km/cap within daily life area of Japan
        # urban.to.rural <- 1.24 # based on micro-data analysis of USA data

        # # set threshold to depend on rural/urban, over time, per message region:
        # R11.thresholds <- R11.pop.urbrur %>%
        #   mutate(thres.urb=national.average.threshold/(urban.to.rural*population.rur/population + population.urb/population)) %>%
        #   mutate(thres.rur=thres.urb*urban.to.rural)
        #
        # # select only base-year for regional threshold
        # R11.thresholds.2015 <- R11.thresholds %>% filter(year==2015) %>%
        #   select(R11.region,thres.rur,thres.urb)

        # # propagate 2015 split, but adjust the threshold for future population changes, per country:
        # iso.thres <- pop %>% select(iso, year, population,population.urb,population.rur) %>%
        #   left_join( message.R11 %>% select(iso, R11.region)) %>%
        #   left_join(R11.thresholds.2015) %>%
        #   mutate(thres = thres.urb * (population.urb/population) +
        #                  thres.rur * (population.rur/population) ) %>%
        #   select(iso,year, thres)
        #
        # ggplot(iso.thres, aes(x=year,y=thres)) + geom_line(size=2) + facet_wrap(~iso)

        # # propagate 2015 split, but adjust the threshold for future population changes, per region --> only in the rollout!
        # R11.thres <- R11.pop.urbrur %>% select(R11.region, year, population,population.urb,population.rur) %>%
        #   left_join(R11.thresholds.2015) %>%
        #   mutate(thres = thres.urb * (population.urb/population) +
        #            thres.rur * (population.rur/population) ) %>%
        #   left_join( message.R11 %>% select(iso, R11.region)) %>%
        #   select(iso, year, thres)
        #
        # ggplot(R11.thres, aes(x=year,y=thres)) + geom_line(size=2) + facet_wrap(~iso)




        DF.DLS <<- DF.DLS %>%
          left_join(data_pkm_shares, by = "iso") %>%
          select(-thres) %>%
          mutate(thres = national.average.threshold) %>% # set threshold for total
          mutate(thres = ifelse(grp == "car", # split threshold by mode shares
                                thres * ldv,
                                ifelse(grp == "bus",
                                       thres * bus,
                                       ifelse(grp == "rail",
                                              thres * rail,
                                              ifelse(grp == "twothree",
                                                     thres * twothree,
                                                     NA
                                              )
                                       )
                                )
          )) %>%
          mutate(simple_thres = thres) %>%
          select(-c(ldv, twothree, bus, rail))
      },

      #' @description
      #' Overwrite the same-named function in the superclass to be specific to this dimension
      IdentifyGap = function(transport_cutoff = FALSE,
                             data.version = "IMAGE2023") {
        print("IdentifyGap: transport")
        # read in the necessary data files (energy intensity not done here in IdentifyGap)

        # options for p-km data:
        # ... R11_pkm.xlsx (ICCT & ad-hoc: ERL2021)
        # ... ... p-km (from multiple combined sources, for 11 regions) & p-km shares - ICCT 2010
        # ... ssp2_npi_20230825-2daysago.xlsx (IMAGE2023) -- not used anymore
        # ... ... p-km from IMAGE model, SHAPE project, 2023 August, using SSP2-NPi scenario
        # ... image_shape_ssp_20230825_addedtwothree_replacedCHN.xlsx (IMAGE2023) -- not used anymore
        # ... ... p-km from IMAGE model for the SHAPE project (see under ssp2_npi_20230825-2daysago.xlsx), but adding 2/3 wheeler data based on Aneeque Javaid's input, as well as replacing all of China's pkm data, 2024 January

        # options for Gini data:
        # ... API_SI.POV.GINI_DS2_en_csv_v2_1217501_withIndiaAustraliaJapanUSA.csv (World Bank - prepared 2019).
        # ... ... a bit ad-hoc the nearest (in time) data points for AUS, JAP, USA are added manually in the prepared data (for ERL2021 paper), then interpolation is applied to increase countries. All is income-based gini now.
        # ... wiid_gini.csv (UNU WIDER - downloaded 2023)
        # ... ... taken from World Inequality Database (UNU WIDER) on 3 August 2023.
        # ... ... ... currently causes an error: <error/dplyr:::mutate_error>; Error in `mutate()`:; ℹ In argument: `nu = log(pkm) - (sigma^2)/2`; Caused by error in `sigma^2`: ! non-numeric argument to binary operator

        if(data.version=="IMAGE2023"){
          load_iam_region_mappings()
          fname_pkm <- "image_shape_ssp_20230825_addedtwothree_replacedCHN.xlsx"
        } else if (data.version=="ERL2021"){
          fname_pkm <- "R11_pkm.xlsx"
        }

        fname_gini <- "API_SI.POV.GINI_DS2_en_csv_v2_1217501_withIndiaAustraliaJapanUSA.csv" # gini coefficient on a national level






        # calculated depth-of-deficit
        pkm.data.gap.base <<- transport_pkm_dod_helper(
          thresholds_df = DF.DLS %>% select(iso,grp,thres), # load and process gini data
          pkm_data = transport_pkm_helper(fname_pkm = fname_pkm,
                                          data.version=data.version,
                                          totals.or.shares = "totals"), # load and process pkm by mode data
          gini_data = economic_data_helper(fname_gini = fname_gini),
          transport_cutoff = transport_cutoff
        )
        pkm_data_gap_df <- pkm.data.gap.base %>%
          select(c(iso, pkm, gap, grp, deprivation.headcount))

        # # check that sum of dod by mode is the same as the total dod
        # pkm.data.gap.base <<- dls.dod_transport %>% #pkm.data.gap.base %>%
        #   bind_rows(
        #     dls.dod_transport %>% #pkm.data.gap.base %>%
        #       group_by(iso) %>%
        #       summarise(pkm=sum(pkm), sum.partial.thres=sum(thres), sum.partial.dod=sum(dod), first.partial.deprivation.headcount=first(deprivation.headcount), gini=first(gini), sigma=first(sigma)) %>%
        #       mutate(nu = log(pkm) - (sigma^2) / 2) %>%
        #       mutate(thres=national.average.threshold) %>%
        #       mutate(
        #         dod = sapply(GetDepthofDeficit_lognormal(nu, sigma, thres, "DoD"), "[", 1),
        #         deprivation.headcount = sapply(GetDepthofDeficit_lognormal(nu, sigma, thres, "share"), "[", 1)
        #       ) %>%
        #       mutate(grp = "total")
        #   )



        # Join data and calculate gap
        print("Join data and calculate gap")
        DF.DLS <<- DF.DLS %>% select(-gap) %>%
          left_join(pkm_data_gap_df, by = c("iso", "grp")) %>% # population average gap
          select(-c(pkm)) # note: deprivation.headcount is NOT by mode - is as a total!
        DF.DLS[DF.DLS < 0] <<- 0 # if negative gap, set to zero --> with lognormal, negative gap is unlikely to happen
        print("Gap analysis completed!")
      },


      #' @description
      #' Derive (or import) final energy intensities from data if not pre-determined
      DeriveEnergyIntensity = function(data.version.pkmshares = "IMAGE2023") {
        print("DeriveEnergyIntensity: transport")

        if(data.version.pkmshares=="IMAGE2023"){
          load_iam_region_mappings()
          fname_pkm <- "image_shape_ssp_20230825_addedtwothree_replacedCHN.xlsx"
          data_pkm_shares <- transport_pkm_helper(fname_pkm = fname_pkm,
                                                  data.version="IMAGE2023",
                                                  totals.or.shares = "shares") %>%
            pivot_wider(names_from = grp, values_from = pkm)  %>%
            rename(ldv = car) %>%
            left_join(message.R11 %>% select(iso, R11.region))
        } else if (data.version.pkmshares=="ERL2021"){
          # multiple versions of energy intensity sources is not yet implemented
          fname_pkm_shares <- "/R11_pkm.xlsx" ## passenger-km shares - ICCT 2010
          ## read in and combine files
          # pkm shares
          data_pkm_shares_R11 <- read_excel(paste0(data.path, "/Transport", fname_pkm_shares), sheet = "Data", col_names = TRUE)
          data_pkm_shares <- message.R11 %>%
            select(iso, R11.region) %>%
            left_join(data_pkm_shares_R11, by = c("R11.region")) %>%
            select(c(iso, R11.region, ldv, twothree, bus, rail))
        }

        # define necessary files

        fname_ei_op <- "/R11_ei.xlsx" ## Energy Intensity in MJ per pkm - ICCT 2010 (translate to MJ after reading in)
        fname_ei_con <- "/vehicle_ei.xlsx" ## Final energy intensities LCA Transportation - WS2 Documents of DLE3
        fname_veh_use <- "/usage.xlsx" ## occupancy rates and vehicle_km per region, from Roadmap-distilledforDLE_nr

        # add public transport occupancy values here - for embodied energy where we assume a global value.
        # public.occupancy <- 0.5 # range from 0.1 to 1.





        ## EIs operation energy (total)
        # EI operating energy (in MJ/pkm)
        data_en_int_shares_R11 <- read_excel(paste0(data.path, "/Transport", fname_ei_op), sheet = "Data", col_names = TRUE) %>%
          mutate(ldv = ldv) %>%
          mutate(bus = bus) %>%
          mutate(rail = rail) %>%
          mutate(twothree = twothree)
        data_en_int_shares <- message.R11 %>%
          select(iso, R11.region) %>%
          left_join(data_en_int_shares_R11, by = c("R11.region")) %>%
          select(c(iso, ldv, bus, rail, twothree))
        # combine all this data for operation energy
        ei_data_op <- data_pkm_shares %>%
          rename(ldv_share = ldv, bus_share = bus, rail_share = rail, twothree_share = twothree) %>%
          left_join(data_en_int_shares_R11, by = c("R11.region")) %>%
          rename(ldv_ei_op = ldv, bus_ei_op = bus, rail_ei_op = rail, twothree_ei_op = twothree)
        ## EIs construction energy (Bus and Rail - in MJ/p-km || Car and Twothree - in GJ/unit)
        data_en_veh <- read_excel(paste0(data.path, "/Transport", fname_ei_con), sheet = "Data", col_names = TRUE) # currently treated as equal for each region.
        # translate to MJ
        data_en_veh <- data_en_veh %>%
          pivot_longer(c(value, elec, `non-elec`)) %>% # go to a more useful longer format
          mutate(value = ifelse(grepl("MJ", unit, fixed = TRUE), value * mega / mega, value)) %>%
          mutate(unit = ifelse(grepl("MJ", unit, fixed = TRUE), "MJ/p-km", unit)) %>%
          mutate(value = ifelse(grepl("GJ", unit, fixed = TRUE), value * giga / mega, value)) %>%
          mutate(unit = ifelse(grepl("GJ", unit, fixed = TRUE), "MJ/unit", unit))

        # add extra info to get per p-km energy intensity for ldv and 2/3-wheelers
        data_usage <- read_excel(paste0(data.path, "/Transport", fname_veh_use), sheet = "Data", col_names = TRUE)
        # translate to MJ/pkm from MJ/unit, occupancy, and vehicle-km. (on R11 level?!)
        # ei.con = ei.veh [MJ/unit] * vehicle_gap [unit] / p-km_gap [km/cap] = [MJ/km/cap]
        # vehicle_gap = p-km_gap [p-km/cap]  / (occupancy [cap/unit] * vehicle_km [v-km/unit]) = [(v-km*cap/unit/cap)*unit*unit/cap/v-km] = unit/cap!
        # thus: ei.con = ei_con_unit / (occupancy * vehicle_km)
        ei_con_unit <- message.R11 %>%
          left_join(data_usage, by = "R11.region") %>% # copy vehicle energy construction needs to all countries in a region
          left_join(select(subset(pop, year == year.base), iso, population)) %>%
          select(-country_name) %>% # load population for getting the total gap.
          rename(ldv = occupancy_ldv, two = occupancy_two) %>%
          gather(mode, occupancy, ldv, two) %>% # transform to longer form
          rename(ldv = vehicle_km_ldv, two = vehicle_km_two) %>%
          arrange(mode) %>%
          gather(vehicle, vehicle_km, ldv, two) %>% # transform to longer form
          filter(mode == vehicle) %>%
          select(-mode) %>%
          rename(grp = vehicle) %>% # remove doubles
          mutate_if(is.character, str_replace_all, pattern = "ldv", replacement = "car") %>% # make naming same as group in gap df
          mutate_if(is.character, str_replace_all, pattern = "two", replacement = "twothree") %>% # make naming same as group in gap df
          left_join(select(filter(DF.DLS, grp == "car" | grp == "twothree"), c(iso, grp, gap)), by = c("iso", "grp")) %>% # load the p-km gaps
          # translate vehicle gap to MJ/pkm
          mutate(ei_con_unit = ifelse(grp == "car", as.numeric(select(filter(data_en_veh, vehicle == "Car", name == "value"), value)),
                                      ifelse(grp == "twothree", as.numeric(select(filter(data_en_veh, vehicle == "Twothree", name == "value"), value)),
                                             NA
                                      )
          )) %>% # join energy intensity per unit in MJ/unit
          mutate(ei_con_unit.elec = ifelse(grp == "car", as.numeric(select(filter(data_en_veh, vehicle == "Car", name == "elec"), value)),
                                           ifelse(grp == "twothree", as.numeric(select(filter(data_en_veh, vehicle == "Twothree", name == "elec"), value)),
                                                  NA
                                           )
          )) %>% # join energy intensity per unit in MJ/unit
          mutate(ei_con_unit.nonelec = ifelse(grp == "car", as.numeric(select(filter(data_en_veh, vehicle == "Car", name == "non-elec"), value)),
                                              ifelse(grp == "twothree", as.numeric(select(filter(data_en_veh, vehicle == "Twothree", name == "non-elec"), value)),
                                                     NA
                                              )
          )) %>% # join energy intensity per unit in MJ/unit
          mutate(ei_con_pkm = ei_con_unit / (occupancy * vehicle_km)) %>% # easier and more robust way of calculation
          mutate(ei_con_pkm.elec = ei_con_unit.elec / (occupancy * vehicle_km)) %>% # easier and more robust way of calculation
          mutate(ei_con_pkm.nonelec = ei_con_unit.nonelec / (occupancy * vehicle_km)) # easier and more robust way of calculation

        # create overall ei tibble
        #  - ei_con_unit # construction energy for ldv & 2/3
        #  - data_en_veh # has the info for rail and bus
        #  - ei_data_op # has all operating energy
        ei_data_all <- select(ei_data_op, c(iso, R11.region, ldv_ei_op, twothree_ei_op, rail_ei_op, bus_ei_op)) %>%
          left_join(select(filter(ei_con_unit, grp == "car"), c(iso, ei_con_pkm, ei_con_pkm.elec, ei_con_pkm.nonelec)), by = "iso") %>%
          rename(ldv_ei_con = ei_con_pkm, ldv_ei_con.elec = ei_con_pkm.elec, ldv_ei_con.nonelec = ei_con_pkm.nonelec) %>%
          left_join(select(filter(ei_con_unit, grp == "twothree"), c(iso, ei_con_pkm, ei_con_pkm.elec, ei_con_pkm.nonelec)), by = "iso") %>%
          rename(twothree_ei_con = ei_con_pkm, twothree_ei_con.elec = ei_con_pkm.elec, twothree_ei_con.nonelec = ei_con_pkm.nonelec) %>%
          mutate(
            bus_ei_con = as.numeric(select(filter(data_en_veh, vehicle == "Bus", name == "value"), value)) * (1 / public.occupancy),
            bus_ei_con.elec = as.numeric(select(filter(data_en_veh, vehicle == "Bus", name == "elec"), value)) * (1 / public.occupancy),
            bus_ei_con.nonelec = as.numeric(select(filter(data_en_veh, vehicle == "Bus", name == "non-elec"), value)) * (1 / public.occupancy)
          ) %>%
          mutate(
            rail_ei_con = as.numeric(select(filter(data_en_veh, vehicle == "Rail", name == "value"), value)) * (1 / public.occupancy),
            rail_ei_con.elec = as.numeric(select(filter(data_en_veh, vehicle == "Rail", name == "elec"), value)) * (1 / public.occupancy),
            rail_ei_con.nonelec = as.numeric(select(filter(data_en_veh, vehicle == "Rail", name == "non-elec"), value)) * (1 / public.occupancy)
          ) %>%
          mutate(ldv_ei_op.elec = 0, twothree_ei_op.elec = 0, rail_ei_op.elec = rail_ei_op, bus_ei_op.elec = 0) %>%
          mutate(ldv_ei_op.nonelec = ldv_ei_op, twothree_ei_op.nonelec = twothree_ei_op, rail_ei_op.nonelec = 0, bus_ei_op.nonelec = bus_ei_op) %>%
          mutate(
            ldv_ei_op = ldv_ei_op.elec + ldv_ei_op.nonelec,
            twothree_ei_op = twothree_ei_op.elec + twothree_ei_op.nonelec,
            rail_ei_op = rail_ei_op.elec + rail_ei_op.nonelec,
            bus_ei_op = bus_ei_op.elec + bus_ei_op.nonelec
          )

        # save weighted average of transport modes for transport construction- for con.rep multiply by replacement rate
        ei.data.con <<- ei_data_all %>%
          left_join(data_pkm_shares %>%
                      rename(ldv_share = ldv, bus_share = bus, rail_share = rail, twothree_share = twothree), by = c("iso", "R11.region")) %>%
          mutate(elec = ldv_share * ldv_ei_con.elec + twothree_share * twothree_ei_con.elec + bus_share * bus_ei_con.elec + rail_share * rail_ei_con.elec) %>%
          mutate(elec_rep = ldv_share * ldv_ei_con.elec * r.rep.car + twothree_share * twothree_ei_con.elec * r.rep.twothree + bus_share * bus_ei_con.elec * r.rep.bus + rail_share * rail_ei_con.elec * r.rep.rail) %>%
          mutate(non.elec = ldv_share * ldv_ei_con.nonelec + twothree_share * twothree_ei_con.nonelec + bus_share * bus_ei_con.nonelec + rail_share * rail_ei_con.nonelec) %>%
          mutate(non.elec_rep = ldv_share * ldv_ei_con.nonelec * r.rep.car + twothree_share * twothree_ei_con.nonelec * r.rep.twothree + bus_share * bus_ei_con.nonelec * r.rep.bus + rail_share * rail_ei_con.nonelec * r.rep.rail) %>%
          mutate(total = ldv_share * ldv_ei_con + twothree_share * twothree_ei_con + bus_share * bus_ei_con + rail_share * rail_ei_con) %>%
          mutate(total_rep = ldv_share * ldv_ei_con * r.rep.car + twothree_share * twothree_ei_con * r.rep.twothree + bus_share * bus_ei_con * r.rep.bus + rail_share * rail_ei_con * r.rep.rail) %>%
          select(iso, elec, elec_rep, non.elec, non.elec_rep, total, total_rep)


        # transform to longer format for combining into DF.tei format.


        ei_data_all_long <- ei_data_all %>%
          gather(key, value, ldv_ei_op:bus_ei_op.nonelec) %>% # get all values in one column
          mutate(grp = NA, type = NA, elec = NA) %>%
          mutate(grp = ifelse(grepl("ldv", key), "car",
                              ifelse(grepl("twothree", key, fixed = TRUE), "twothree",
                                     ifelse(grepl("rail", key, fixed = TRUE), "rail",
                                            ifelse(grepl("bus", key, fixed = TRUE), "bus",
                                                   NA
                                            )
                                     )
                              )
          )) %>%
          mutate(type = ifelse(grepl("op", key, fixed = TRUE), "OP",
                               ifelse(grepl("con", key, fixed = TRUE), "CON",
                                      NA
                               )
          )) %>%
          mutate(elec = ifelse(grepl(".elec", key, fixed = TRUE), "elec",
                               ifelse(grepl(".nonelec", key, fixed = TRUE), "non.elec",
                                      # NA))) %>%
                                      "total"
                               )
          )) %>%
          # add here total under elec? as extra set of rows? As sum of elec and non.elec? --> add in wide format, then bring in/bring back here.
          select(-c(key, R11.region))
        con.new <- ei_data_all_long %>%
          filter(type == "CON") %>%
          mutate_if(is.character, str_replace_all, pattern = "CON", replacement = "CON.new")
        con.rep <- ei_data_all_long %>%
          filter(type == "CON") %>%
          mutate_if(is.character, str_replace_all, pattern = "CON", replacement = "CON.rep")
        ei_data_all_long <- ei_data_all_long %>%
          bind_rows(con.new) %>%
          bind_rows(con.rep) %>%
          dplyr::filter(type != "CON") %>%
          # project EI of CON.new on CON.new.pop and CON.new.dls
          add_ei_for_CONnewDLSPOPsplit()

        # add weighted total
        ei_data_all.with.totals <<- ei_data_all %>%
          left_join(data_pkm_shares %>%
                      rename(ldv_share = ldv, bus_share = bus, rail_share = rail, twothree_share = twothree)) %>%
          mutate(total_ei_op = ldv_share * ldv_ei_op +
                   bus_share * bus_ei_op +
                   rail_share * rail_ei_op +
                   twothree_share * twothree_ei_op) %>%
          mutate(total_ei_op.elec = ldv_share * ldv_ei_op.elec +
                   bus_share * bus_ei_op.elec +
                   rail_share * rail_ei_op.elec +
                   twothree_share * twothree_ei_op.elec) %>%
          mutate(total_ei_op.nonelec = ldv_share * ldv_ei_op.nonelec +
                   bus_share * bus_ei_op.nonelec +
                   rail_share * rail_ei_op.nonelec +
                   twothree_share * twothree_ei_op.nonelec) %>%
          left_join(
            ei.data.con %>%
              rename(

                total_ei_con.new = total,
                total_ei_con.new.elec = elec,
                total_ei_con.new.nonelec = non.elec,

                total_ei_con.rep = total_rep,
                total_ei_con.rep.elec = elec_rep,
                total_ei_con.rep.nonelec = non.elec_rep,


              )
          )


        DF.tei <<- DF.tei %>%
          left_join(ei_data_all_long, by = c("iso", "grp", "type", "elec")) %>%
          select(-e.int) %>%
          rename(e.int = value)
      },


      #' @description
      #' This is specifically for editing modal share thresholds to converge to  for DLE result (DLE.tot) aggregation.
      AdaptModalThresholds = function(df.inc, minimal.share.public.year.target = 0.4) {
        # TO-DO:
        # - implement alternative transport pathways:
        #     with share of public transport (bus+rail) in Japan as the minimum.
        #     all countries with lower public transport should converge to it.
        #       option 1: reduce shares of car and two/three-wheelers to keep the ratio, filling it with rail.
        #       option 2: reduce shares of car and two/three-wheelers to keep the ratio, filling it with bus.
        #       option 3: reduce shares of car and two/three-wheelers to keep the ratio, filling it with bus and rail keeping its ratio.
        #       * currently only implemented option 3.
        # e.g. 'if transport { df.rollout, where(public.share<japan.share){ do change in threshold? } }'
        # japan share (Kei Gii et al. ): motorized public / (motorized public + motorized private) = 432/(432+651) = 0.40

        # step 1: calculate new columns (from thresholds): share of public | share of private | ratio of increase public | ratio of decrease public
        new.df.inc <- df.inc %>%
          select(iso, gap, thres, grp, year) %>% # select only the columns we need to work with here.
          pivot_wider(names_from = grp, values_from = c(gap, thres)) %>%
          mutate(thres.share.public = (thres_bus + thres_rail) / (thres_bus + thres_rail + thres_car + thres_twothree)) %>%
          mutate(thres.share.private = (thres_car + thres_twothree) / (thres_bus + thres_rail + thres_car + thres_twothree))
        new.df.inc <- new.df.inc %>%
          mutate(new.public.share = case_when(
            thres.share.public < minimal.share.public.year.target ~ thres.share.public + (minimal.share.public.year.target - thres.share.public) * pmin((year - year.base), (year.target - year.base)) / (year.target - year.base), # old share + total_increase*pmax(dt,1)   func(year,thres,minimal.share.public.year.target,year.target),
            thres.share.public > minimal.share.public.year.target ~ thres.share.public
          )) %>%
          mutate(new.private.share = 1 - new.public.share) %>%
          # find out correct way to do this first, then implement. It needs to be relative to overall threshold.
          mutate(overall_thres = thres_car + thres_bus + thres_rail + thres_twothree) %>%
          # step 2: edit thres and gap starting from the first rollout timestep until and after the target year to increase the public transport share where needed.
          mutate(
            gap_car = pmax(gap_car + thres_car * (new.private.share - thres.share.private) / thres.share.private, 0),
            thres_car = thres_car + thres_car * (new.private.share - thres.share.private) / thres.share.private,
            gap_twothree = pmax(gap_twothree + thres_twothree * (new.private.share - thres.share.private) / thres.share.private, 0),
            thres_twothree = thres_twothree + thres_twothree * (new.private.share - thres.share.private) / thres.share.private,
            gap_rail = pmax(gap_rail + thres_rail * (new.public.share - thres.share.public) / thres.share.public, 0),
            thres_rail = thres_rail + thres_rail * (new.public.share - thres.share.public) / thres.share.public,
            gap_bus = pmax(gap_bus + thres_bus * (new.public.share - thres.share.public) / thres.share.public, 0),
            thres_bus = thres_bus + thres_bus * (new.public.share - thres.share.public) / thres.share.public
          ) %>%
          mutate(overall_thres_new_check = thres_car + thres_bus + thres_rail + thres_twothree)

        # step 3: re-do inc based on the new threshold and thus the new gap in the rollout years (df.inc <- DF.DLS %>% mutate(inc = gap/n.yr.left))
        new.df.inc.thres <- new.df.inc %>%
          pivot_longer(values_to = "thres", names_to = "grp", cols = thres_car:thres_twothree, names_prefix = "thres_") %>%
          select(iso, year, grp, thres)
        new.df.inc.gap <- new.df.inc %>%
          pivot_longer(values_to = "gap", names_to = "grp", cols = gap_car:gap_twothree, names_prefix = "gap_") %>%
          select(iso, year, grp, gap)
        new.df.inc <- left_join(new.df.inc.thres, new.df.inc.gap) %>%
          left_join(df.inc %>% select(-c(thres, gap)), by = c("iso", "year", "grp")) # to add back the unchanged columns in the input
        return(new.df.inc)
      },
      FillRolloutDF = function(df.inc, df.rollout, yr.tgt = year.target, r.rep = 0.05,
                               r.rep.grp = FALSE, only.OP = FALSE, urbrur = FALSE,
                               timely.threshold.transport = FALSE, lct = FALSE, year.start.rollout=year.base) {
        print(paste("FillRolloutDF: ", name_dim, ", timely.threshold.transport =", timely.threshold.transport))

        if (timely.threshold.transport) {
          # threshold parameters
          # national.average.threshold <- 8527 #Threshold: in p-km/cap/year corresponding to the average motorized passenger-km/cap within daily life area of Japan
          # urban.to.rural <- 1.24 # based on micro-data analysis of USA data


          # option 1:
          # - calculate the COUNTRY-SPECIFIC urb and rur thresholds defined by urban.to.rural ratio and threshold
          # - set country pkm threshold to 8527 based on these regional urb/rur thresholds,
          # - let national threshold change for future population changes, using PER COUNTRY urbanization change:

          # set threshold to depend on rural/urban, over time, per message region:
          iso.thresholds <- pop %>%
            mutate(thres.urb = national.average.threshold / (urban.to.rural * population.rur / population + population.urb / population)) %>%
            mutate(thres.rur = thres.urb * urban.to.rural)

          # select only base-year for regional threshold
          iso.thresholds.2015 <- iso.thresholds %>%
            filter(year == 2015) %>%
            select(iso, thres.rur, thres.urb)



          iso.thres <- pop %>%
            select(iso, year, population, population.urb, population.rur) %>%
            # left_join( message.R11 %>% select(iso, R11.region)) %>%
            left_join(iso.thresholds.2015) %>%
            mutate(thres = thres.urb * (population.urb / population) +
                     thres.rur * (population.rur / population)) %>%
            ungroup() %>%
            select(iso, year, thres)
          # ggplot(iso.thres, aes(x=year,y=thres)) + geom_line(size=2) + facet_wrap(~iso)
          iso.thres.change <- iso.thres %>%
            mutate(thres = thres / national.average.threshold) %>%
            rename(thres.change = thres)

          df.inc <- df.inc %>%
            left_join(iso.thres.change) %>%
            mutate(gap = (thres * thres.change) - (thres - gap)) %>% # gap = thres - pkm
            mutate(thres = thres * thres.change) %>%
            select(-thres.change)
          # could decide here to also change inc, if we don't want linear speed.

          # do alternative transport modes pathway
          if (lct) {
            new.df.inc <- AdaptModalThresholds(df.inc)
            n.yr.left <- yr.tgt - year.base
            # # option 1: then take the maximum speed of increase to make sure the biggest gap is closed (generally the one at the target year).
            # #     - this can still lead to a fast increase in private also on the short term, because there is a gap on the short-term, despite there being no gap in the target year
            # df.inc <- new.df.inc %>%
            #   mutate(inc = gap/n.yr.left) %>% # get increase speed determined that would be demanded by the timely (current) gap/threshold if that would be the target
            #   group_by(iso,grp) %>%
            #   mutate(inc = max(inc)) %>%
            #   ungroup()

            # option 2: then take the speed of increase at the target yeat to make sure the gap is closed by then
            #     - this leads to not filling some gaps on the short term that could theoretically be filled
            # --> following this, we should also simply update the thres and gap in the years before year.target
            # get new inc based on new gap
            df.inc.tgt <- new.df.inc %>%
              mutate(inc = gap / n.yr.left) %>% # get increase speed determined that would be demanded by the timely (current) gap/threshold if that would be the target
              filter(year == yr.tgt) %>%
              select(iso, grp, inc, gap, thres)
            # set inc to the correct speed
            df.inc <- new.df.inc %>%
              select(-inc) %>%
              left_join(df.inc.tgt %>% select(iso, grp, inc))
            # and adjust the thres accordingly (what to do with gap?)
            df.inc <- df.inc %>%
              left_join(df.inc.tgt %>% select(iso, grp, gap, thres) %>% rename(newgap = gap, newthres = thres), by = c("iso", "grp")) %>%
              mutate(thres = ifelse(year <= yr.tgt & (grp == "bus" | grp == "rail"), newthres, thres)) %>%
              mutate(gap = ifelse(year <= yr.tgt & (grp == "bus" | grp == "rail"), newgap, gap)) %>%
              select(-c(newgap, newthres))
          }



          # option 2:
          # - calculate the REGIONAL urb and rur thresholds defined by urban.to.rural ratio and threshold
          # - calculate COUNTRY-SPECIFIC pkm threshold based on these regional urb/rur thresholds,
          # - let national threshold change for future population changes, using PER COUNTRY urbanization change:

          # # set threshold to depend on rural/urban, over time, per message region:
          # R11.thresholds <- R11.pop.urbrur %>%
          #   mutate(thres.urb=national.average.threshold/(urban.to.rural*population.rur/population + population.urb/population)) %>%
          #   mutate(thres.rur=thres.urb*urban.to.rural)
          #
          # # select only base-year for regional threshold
          # R11.thresholds.2015 <- R11.thresholds %>% filter(year==2015) %>%
          #   select(R11.region,thres.rur,thres.urb)
          #
          # iso.thres <- pop %>% select(iso, year, population,population.urb,population.rur) %>%
          #   left_join( message.R11 %>% select(iso, R11.region)) %>%
          #   left_join(R11.thresholds.2015) %>%
          #   mutate(thres = thres.urb * (population.urb/population) +
          #                  thres.rur * (population.rur/population) ) %>%
          #   ungroup() %>%
          #   select(iso,year, thres)
          # # ggplot(iso.thres, aes(x=year,y=thres)) + geom_line(size=2) + facet_wrap(~iso)
          # iso.thres.change <- iso.thres %>%
          #   mutate(thres = thres/national.average.threshold) %>%
          #   rename(thres.change=thres)
          #
          # df.inc <- df.inc %>%
          #   left_join(iso.thres.change) %>%
          #   mutate(gap = (thres*thres.change) - (thres-gap)  ) %>% # gap = thres - pkm
          #   mutate(thres = thres*thres.change) %>%
          #   select(-thres.change)
          # # could decide here to also change inc, if we don't want linear speed.



          # option 3:
          # - calculate the REGIONAL urb and rur thresholds defined by urban.to.rural ratio and threshold
          # - let national threshold levels change for future population changes using PER REGION urbanization change:

          # R11.thresholds <- R11.pop.urbrur %>%
          #   mutate(thres.urb=national.average.threshold/(urban.to.rural*population.rur/population + population.urb/population)) %>%
          #   mutate(thres.rur=thres.urb*urban.to.rural)
          #
          # # select only base-year for regional threshold
          # R11.thresholds.2015 <- R11.thresholds %>% filter(year==2015) %>%
          #   select(R11.region,thres.rur,thres.urb)
          #
          # R11.thres <- R11.pop.urbrur %>% select(R11.region, year, population,population.urb,population.rur) %>%
          #   left_join(R11.thresholds.2015) %>%
          #   mutate(thres = thres.urb * (population.urb/population) +
          #            thres.rur * (population.rur/population) ) %>%
          #   left_join( message.R11 %>% select(iso, R11.region)) %>%
          #   ungroup() %>%
          #   select(iso, year, thres)
          # # ggplot(R11.thres, aes(x=year,y=thres)) + geom_line(size=2) + facet_wrap(~iso)
          # R11.thres.change <- R11.thres %>%
          #   mutate(thres = thres/national.average.threshold) %>%
          #   rename(thres.change=thres)
          #
          # df.inc <- df.inc %>%
          #   left_join(R11.thres.change) %>%
          #   mutate(gap = (thres*thres.change) - (thres-gap)  ) %>% # gap = thres - pkm
          #   mutate(thres = thres*thres.change) %>%
          #   select(-thres.change)
          # # could decide here to also change inc, if we don't want linear speed.
        }

        callSuper(df.inc, df.rollout, yr.tgt, r.rep, r.rep.grp, only.OP, urbrur, year.start.rollout)
      },


      #' @description
      #' Construct rollout scenario based on identified gap and other assumptions.
      #' This can be coded differently for different scenarios.
      ConstructRolloutScenario = function(scen, yr.tgt = year.target, timely.threshold.transport = TRUE, lct = FALSE,
                                          yr.st.ro=year.base) {
        # base National Policies scenario:
        # - constant modes? or population density.

        ## total turnover rate: km/vehicle divided by occupancy rate (in usage.xlsx)

        DF.rollout <<- DF.rollout %>%
          mutate(r.rep.pergrp = ifelse(type == "CON.rep",
                                       ifelse(grepl("car", grp), r.rep.car,
                                              ifelse(grepl("twothree", grp, fixed = TRUE), r.rep.twothree,
                                                     ifelse(grepl("rail", grp, fixed = TRUE), r.rep.rail,
                                                            ifelse(grepl("bus", grp, fixed = TRUE), r.rep.bus,
                                                                   NA
                                                            )
                                                     )
                                              )
                                       ),
                                       NA
          ))


        # # not used:
        # r.veh.bus <- 0.59*0.02 # MJ/p-km (final energy internsities LCA - Transportsation - Construction of public transport)
        # r.veh.rail <- 0.612*0.01 # MJ/p-km (final energy internsities LCA - Transportsation - Construction of public transport)
        # r.infra.bus <- 0 # MJ/p-km (final energy internsities LCA - Transportsation - Construction of public transport)
        # r.infra.rail <- 0.612*0.08 # MJ/p-km (final energy internsities LCA - Transportsation - Construction of public transport)

        if (scen == "Income") {
          print("ConstructRolloutScenario:  Transport - Income")

          callSuper(scen, yr.tgt = Inf, r.rep.grp = TRUE, timely.threshold.transport = timely.threshold.transport)
        } else if (scen == "Income.regression") {
          print("ConstructRolloutScenario:  Transport - Income Regression")

          callSuper(scen, r.rep.grp = TRUE, timely.threshold.transport = timely.threshold.transport, lct = T, yr.tgt = 2040)
        } else if (scen == "ACCEL") {
          print("ConstructRolloutScenario:  Transport - ACCEL")

          callSuper(scen, yr.tgt, r.rep.grp = TRUE, timely.threshold.transport = timely.threshold.transport, lct = lct, year.start.rollout=yr.st.ro)
        } else {
          message("Something wrong here....") # replace by throwing an exception
        }
      }
    )
  )
