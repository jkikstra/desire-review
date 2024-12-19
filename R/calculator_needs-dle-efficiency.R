# Script for scaling the DLE threshold over time

# This script does two things:
# 1) read in regional IAM data to calculate end-use technology or service provisioning improvement over time, over final energy
# 2) use that ratio to scale the DLE thresholds



if (!exists("DLE.efficiency.scale")) {
  DLE.efficiency.scale <- TRUE
}




if (DLE.efficiency.scale) {
  log_info("Scenario technology development: derive final energy to service level conversion ratios over time")

  # assume that service data is regional, because it cannot (yet) be downscaled

  if (IAM.OUTPUT.SHORT.FILENAME == "SHAPE-final" & grepl(IAM.SCENARIO.DATA.FILE, pattern="SHAPE-final_v3", fixed = T )) {


    # step 1: load data
    # ... the if statement is here to save running time. I.e., if looping over calculator_run.R multiple times, you can set FIRST.TIME.RUN.PREPARE.REGIONAL.DATA to FALSE after the first run, and then this step will be faster for subsequent runs (using already saved out data, instead of reprocessing the same data)
    # ... saves about 10 seconds on my DELL laptop from ~2022


    # Prepare data from regional raw scenario files ----------------------------
    if (!exists("FIRST.TIME.RUN.PREPARE.REGIONAL.DATA")) {
      FIRST.TIME.RUN.PREPARE.REGIONAL.DATA <- TRUE
    }
    if (FIRST.TIME.RUN.PREPARE.REGIONAL.DATA){
      original.regionallevel.iam.data.path <- here(
        get_data_location_raw(test=FALSE),
        "scenario_data",
        "original_regional"
      )

      original.regionallevel.iam.data.files <- dir(original.regionallevel.iam.data.path, pattern = "*.xlsx") %>%  # get file names
        # remove files that are only Excel backups or temporary files
        str_subset(pattern = "^[^~$]", negate = FALSE)

      # Note: could consider interpolating IMAGE and REMIND 5-yearly data points here


      efficiency.scaler.vars <- c(
        "Final Energy|Residential",
        "Final Energy|Residential and Commercial",
        "Energy Service|Residential|Floor Space",
        "Energy Service|Residential and Commercial|Floor Space",
        "Useful Energy|Residential and Commercial",
        "Useful Energy|Residential",
        "Population", # because useful energy in REMIND is reported per capita, while final energy is reported as a total

        "Final Energy|Industry",
        "Final Energy|Industry|Iron and Steel",
        "Final Energy|Industry|Non-Metallic Minerals",
        "Production|Non-Metallic Minerals|Cement|Volume",
        "Production|Iron and Steel|Volume",

        "Final Energy|Transportation",
        "Energy Service|Transportation|Passenger"


      )

      original.regionallevel.iam.data <- original.regionallevel.iam.data.files %>%
        map(~ (read_excel(file.path(original.regionallevel.iam.data.path,.)) %>%
                 filter(Variable%in%efficiency.scaler.vars) %>%
                 iamc_wide_to_long(upper.to.lower = T)) ) %>%
        reduce(rbind)

      saveRDS(
        original.regionallevel.iam.data,
        file = here(DATA.LOCATION, paste0("original_regionallevel-iam-data", "_",
                                          IAM.OUTPUT.SHORT.FILENAME, ".RData"))
      )

    } else {
      original.regionallevel.iam.data <- readRDS(
        here(DATA.LOCATION, paste0("original_regionallevel-iam-data", "_",
                                   IAM.OUTPUT.SHORT.FILENAME, ".RData"))
      )
    }


    # change if wanting to write out efficiency data.
    save.out.other.regional.variables.to.understand.efficiency <- F
    if (save.out.other.regional.variables.to.understand.efficiency){
      original.regionallevel.iam.data.vars.tohelpunderstand <- original.regionallevel.iam.data.files %>%
        map(~ (read_excel(file.path(original.regionallevel.iam.data.path,.)) %>%
                 filter(Variable%in%c(
                   "Carbon Capture|Industry",
                   "Energy Service|Transportation|Passenger|Road|LDV",
                   "Final Energy|Residential and Commercial|Electricity",
                   "Final Energy|Residential and Commercial|Space Heating",
                   "Useful Energy|Residential and Commercial"
                 )) %>%
                 iamc_wide_to_long(upper.to.lower = T)) ) %>%
        reduce(rbind)

      image.regions.plot <- c("EAF", "INDIA", "CHN", "World")
      remind.regions.plot <- c("SSA", "IND", "CHA", "World")

      write_delim(
        file = here(DATA.LOCATION, paste0("intermed_ir_regions_efficiency_selection", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData")),
        x = original.regionallevel.iam.data %>% bind_rows(original.regionallevel.iam.data.vars.tohelpunderstand) %>%
          filter(region %in% c(image.regions.plot, remind.regions.plot)),
        delim = ","
      )
    }




    # step 2: calculate changes (normalise along the way for ease of reading)

    original.iam.data.pivoted <- original.regionallevel.iam.data %>%
      select(-unit) %>%
      pivot_wider(names_from = variable, values_from = value)


    # Industry -----------------------------------------------------------------
    # step 2.1: calculate global index for industry
    service.efficiency.industry.global <- original.iam.data.pivoted %>%
      filter(region=="World") %>%
      mutate(
        global.service.ratio.steel = `Production|Iron and Steel|Volume`/`Final Energy|Industry|Iron and Steel`,
        global.service.ratio.cement = `Production|Non-Metallic Minerals|Cement|Volume`/`Final Energy|Industry|Non-Metallic Minerals`,
        global.service.ratio.industry = (
          (
            (
              `Production|Non-Metallic Minerals|Cement|Volume`/`Final Energy|Industry|Non-Metallic Minerals`
            ) * ( # in Velez and Pauliuk (2023), non-metallic minerals is about 2.0 t/cap/yr; assuming that is almost all concrete, and assuming that concrete to cement is about 10:1
              0.2 / (0.2+0.7)
              )
            )
          ) + (
            (
              `Production|Iron and Steel|Volume`/`Final Energy|Industry|Iron and Steel`
            ) * ( #  in Velez and Pauliuk (2023), metal ores is about 1.1 t/cap/yr; assuming that is almost all steel, and assuming that the metal or to steel ratio is about 1.6:1
              0.7 / (0.2+0.7)
              )
            )
          ) %>%
      select(model,scenario,year,global.service.ratio.steel,global.service.ratio.cement,global.service.ratio.industry)

    # intermediary save out of the change in ratios of final energy to service
    save.out.industry.global.service.ratios <- F
    if (save.out.industry.global.service.ratios){
      saveRDS(service.efficiency.industry.global,
              file = here(DATA.LOCATION, paste0("intermed_scenario_service_industry_ratios", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData"))
      )

      # intermezzo: check global rates
      p.cement.and.industry.global <- ggplot(
        service.efficiency.industry.global %>%
          pivot_longer(cols = global.service.ratio.steel:global.service.ratio.industry, names_to="variable", values_to = "value") %>%
          filter(
            scenario%in%c(
              "SDP_EI-1p5C",
              "SDP_MC-1p5C",
              "SDP_RC-1p5C"
            ),
            year>=SCENARIO.START.YEAR
          ),
        aes(x=year,y=value, colour=scenario,linetype=model)
      ) +
        facet_grid(~variable) +
        geom_line(linewidth=2) +
        theme_hc() +
        scale_color_bluebrown() +
        ylab("Mt/EJ") + xlab(NULL)

      save_ggplot(
        p = p.cement.and.industry.global,
        filename = here(DATA.LOCATION, paste0("p.intermed_scenario_service_industry_ratios", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData")),
        h =
      )



    }


    # Buildings and Transport --------------------------------------------------
    # step 2.2: calculate regional indices for other regions
    service.efficiency <-
      original.iam.data.pivoted %>%
      # calculate ratios
      mutate(
        ### Residential and Commercial -----------------------------------------
        service.ratio.rescom = ifelse(

          # possible default decision-tree:
          # # take useful energy residential and commercial (preferred for SHAPE)
          # !is.na(`Useful Energy|Residential and Commercial`),
          # `Useful Energy|Residential and Commercial`/`Final Energy|Residential and Commercial`,
          # ifelse(
          #   # otherwise take useful energy residential if available
          #   !is.na(`Useful Energy|Residential`),
          #   `Useful Energy|Residential`/`Final Energy|Residential`,
          #   ifelse(
          #     # otherwise take floor space Residential
          #     !is.na(`Energy Service|Residential|Floor Space`),
          #     `Energy Service|Residential|Floor Space`/`Final Energy|Residential`,
          #     ifelse(
          #       # otherwise take floor space Residential + Commercial
          #       !is.na(`Energy Service|Residential and Commercial|Floor Space`),
          #       `Energy Service|Residential and Commercial|Floor Space`/`Final Energy|Residential and Commercial`,
          #       NA
          #     )
          #   )
          # )

          # SHAPE specific decision tree:
          # take useful energy residential and commercial (preferred for SHAPE, REMIND)
          model=="REMIND-MAgPIE 3.2-4.6",
          `Useful Energy|Residential and Commercial`/(`Final Energy|Residential and Commercial`/`Population`),
          ifelse(
            # take residential floor space (preferred for SHAPE, IMAGE)
            model=="IMAGE 3.3",
            `Energy Service|Residential|Floor Space`/`Final Energy|Residential`,
            NA
          )


        ),
        ### Transport ----------------------------------------------------------
        service.ratio.transport = ifelse(
          # take total passenger transport
          !is.na(`Energy Service|Transportation|Passenger`),
          `Energy Service|Transportation|Passenger`/`Final Energy|Transportation`,
          NA
        ),

        ### Industry (add in global ratios) ------------------------------------
        # - take a weighted sum of (iron and steel) and (cement) production
        #   ... [which together should make up ~50% of DLS flows & >90% of stock]
        # (implement using mutate_cond below, to increase readibility)
        service.ratio.industry = NA
      ) %>%
      left_join(service.efficiency.industry.global,
                by = c("model", "scenario", "year")) %>%
      mutate(service.ratio.industry = global.service.ratio.industry)


    # step 2.3: normalise (and do constant fill where necessary)
    service.efficiency.normalised <- service.efficiency %>%
      select(model,scenario,region,year,service.ratio.rescom,service.ratio.transport,service.ratio.industry) %>%
      pivot_longer(cols = c(service.ratio.rescom,service.ratio.industry,service.ratio.transport),
                   names_to = "variable", values_to = "value") %>%
      mutate(unit = "service/EJ") %>%

      arrange(model,scenario,region,variable,year) %>%

      # normalise
      normalise_iamc_long(starting.year = SCENARIO.START.YEAR) %>%

      # # fill constant
      # constant_year_fill(variable.to.fill = "value") %>%

      # and put in specified format
      mutate_cond(variable=="service.ratio.transport", variable="Final Energy|Transportation") %>%
      mutate_cond(variable=="service.ratio.industry", variable="Final Energy|Industry") %>%
      mutate_cond(variable=="service.ratio.rescom", variable="Final Energy|Residential and Commercial")



    # step 3: map onto countries
    load_iam_region_mappings()

    f2s.data.iso <-
      regions.image %>%
      left_join(service.efficiency.normalised %>% filter(model=="IMAGE 3.3") %>%
                  rename(region.image=region), relationship = "many-to-many",
                by = "region.image") %>%
      select(model, scenario, iso, variable, year, value) %>%
      bind_rows(
        regions.remind %>%
          left_join(service.efficiency.normalised %>% filter(model=="REMIND-MAgPIE 3.2-4.6") %>%
                      rename(region.remind=region), relationship = "many-to-many",
                    by = "region.remind") %>%
          select(model, scenario, iso, variable, year, value)
      ) %>%
      bind_rows(
        regions.message %>%
          left_join(service.efficiency.normalised %>% filter(model=="MESSAGEix-GLOBIOM 1.1-BM-R12") %>%
                      rename(region.message=region), relationship = "many-to-many",
                    by = "region.message") %>%
          select(model, scenario, iso, variable, year, value)
      ) %>%
      mutate(
       fe.to.service.ratio = 1/value
      ) %>%
      drop_na(model)



    # step 4: create a dle scaler (with rho to translate)
    scenario.data.f2s.adj <- f2s.data.iso %>%
      mutate(rho = RHO) %>%
      mutate(
        dle.tech.scaler =

          # # option 1: rho is linear scaling (simple, but has the clear possibility of going negative if rho is >1)
          # 1-((1-(fe.to.service.ratio))*rho)


          # option 2: rho is exponential scaling
          ((fe.to.service.ratio)*rho) # default choice

          # tbd. future options?
          # future visualisation idea: show effect of different rho-scaling options.
      ) %>%
      select(model, scenario, iso, variable, year, dle.tech.scaler)



  } else if (
    IAM.OUTPUT.SHORT.FILENAME == "ENGAGE_for_MESSAGE_DLE"
  ) {
    # step 1: load data
    # ... the if statement is here to save running time. I.e., if looping over calculator_run.R multiple times, you can set FIRST.TIME.RUN.PREPARE.REGIONAL.DATA to FALSE after the first run, and then this step will be faster for subsequent runs (using already saved out data, instead of reprocessing the same data)
    # ... saves about 10 seconds on my DELL laptop from ~2022


    # Prepare data from regional raw scenario files ----------------------------
    if (!exists("FIRST.TIME.RUN.PREPARE.REGIONAL.DATA")) {
      FIRST.TIME.RUN.PREPARE.REGIONAL.DATA <- TRUE
    }
    if (FIRST.TIME.RUN.PREPARE.REGIONAL.DATA){
      original.regionallevel.iam.data.path <- here(
        get_data_location_raw(test=FALSE),
        "scenario_data",
        "original_regional"
      )

      original.regionallevel.iam.data.files <- dir(original.regionallevel.iam.data.path,
                                                   pattern = "*snapshot_all_regions_RAW_ENGAGE_SSP2_v4.1.8.3.1_T4.5_r3.1.csv") %>%  # get file names
        # remove files that are only Excel backups or temporary files
        str_subset(pattern = "^[^~$]", negate = FALSE)

      # Note: could consider interpolating IMAGE and REMIND 5-yearly data points here


      efficiency.scaler.vars <- c(
        "Final Energy|Residential and Commercial",
        "Useful Energy|RC Specific",
        "Useful Energy|RC Thermal",

        "Final Energy|Industry",
        "Useful Energy|Feedstocks",
        "Useful Energy|Industrial Specific",
        "Useful Energy|Industrial Thermal",

        "Final Energy|Transportation",
        "Useful Energy|Transport"

      )

      original.regionallevel.iam.data <- original.regionallevel.iam.data.files %>%
        map(~ (read_csv(file.path(original.regionallevel.iam.data.path,.)) %>%
                 filter(VARIABLE%in%efficiency.scaler.vars) %>%
                 allcaps_to_lower() %>%
                 iamc_wide_to_long(upper.to.lower = F)) ) %>%
        reduce(rbind) %>%
        mutate(
          region = case_when(
            region == "GLB region (R11)" ~ "World",
            grepl("Centrally planned Asia", region) ~ "CPA",
            grepl("Eastern Europe", region) ~ "EEU",
            grepl("Former Soviet Union", region) ~ "FSU",
            grepl("Latin America", region) ~ "LAM",
            grepl("Middle East and Africa", region) ~ "MEA",
            grepl("North America", region) ~ "NAM",
            grepl("Pacific Asia", region) ~ "PAS",
            grepl("Pacific OECD", region) ~ "PAO",
            grepl("South Asia", region) ~ "SAS",
            grepl("Subsaharan Africa", region) ~ "AFR",
            grepl("Western Europe", region) ~ "WEU",
            TRUE ~ region
          )
        )

      saveRDS(
        original.regionallevel.iam.data,
        file = here(DATA.LOCATION, paste0("original_regionallevel-iam-data", "_",
                                          IAM.OUTPUT.SHORT.FILENAME, ".RData"))
      )

    } else {
      original.regionallevel.iam.data <- readRDS(
        here(DATA.LOCATION, paste0("original_regionallevel-iam-data", "_",
                                   IAM.OUTPUT.SHORT.FILENAME, ".RData"))
      )
    }


    # step 2: calculate changes (normalise along the way for ease of reading)

    original.iam.data.pivoted <- original.regionallevel.iam.data %>%
      select(-unit) %>%
      pivot_wider(names_from = variable, values_from = value)


    # Industry -----------------------------------------------------------------

    # step 2.1: calculate global index for industry
    service.efficiency.industry.global <- original.iam.data.pivoted %>%
      filter(region=="World") %>%
      mutate(
        global.service.ratio.industry = (
          (
            `Useful Energy|Feedstocks` + `Useful Energy|Industrial Specific` + `Useful Energy|Industrial Thermal`
          ) / (
            `Final Energy|Industry`
          )
        )
      ) %>%
      select(model,scenario,year,global.service.ratio.industry)


    # Buildings and Transport --------------------------------------------------
    # step 2.2: calculate regional indices for other regions
    service.efficiency <-
      original.iam.data.pivoted %>%
      # calculate ratios
      mutate(
        ### Residential and Commercial -----------------------------------------
        service.ratio.rescom = (
          `Useful Energy|RC Specific` + `Useful Energy|RC Thermal`
        ) / (`Final Energy|Residential and Commercial`),
        ### Transport ----------------------------------------------------------
        service.ratio.transport = `Useful Energy|Transport` / `Final Energy|Transportation`,
        #   ifelse(
        #   # take total passenger transport
        #   !is.na(`Energy Service|Transportation|Passenger`),
        #   `Energy Service|Transportation|Passenger`/`Final Energy|Transportation`,
        #   `Useful Energy|Transport` / `Final Energy|Transportation`
        # ),

        ### Industry (add in global ratios) ------------------------------------
        # - take a weighted sum of (iron and steel) and (cement) production
        #   ... [which together should make up ~50% of DLS flows & >90% of stock]
        # (implement using mutate_cond below, to increase readibility)
        service.ratio.industry = NA
      ) %>%
      left_join(service.efficiency.industry.global,
                by = c("model", "scenario", "year")) %>%
      mutate(service.ratio.industry = global.service.ratio.industry)



    # step 2.3: normalise (and do constant fill where necessary)
    service.efficiency.normalised <- service.efficiency %>%
      select(model,scenario,region,year,service.ratio.rescom,service.ratio.transport,service.ratio.industry) %>%
      pivot_longer(cols = c(service.ratio.rescom,service.ratio.industry,service.ratio.transport),
                   names_to = "variable", values_to = "value") %>%
      mutate(unit = "service/EJ") %>%

      arrange(model,scenario,region,variable,year) %>%

      # normalise
      normalise_iamc_long(starting.year = SCENARIO.START.YEAR) %>%

      # # fill constant
      # constant_year_fill(variable.to.fill = "value") %>%

      # and put in specified format
      mutate_cond(variable=="service.ratio.transport", variable="Final Energy|Transportation") %>%
      mutate_cond(variable=="service.ratio.industry", variable="Final Energy|Industry") %>%
      mutate_cond(variable=="service.ratio.rescom", variable="Final Energy|Residential and Commercial")



    # step 3: map onto countries
    load_iam_region_mappings()

    f2s.data.iso <-
      regions.message %>%
      left_join(service.efficiency.normalised %>%
                  rename(region.message=region), relationship = "many-to-many",
                by = "region.message") %>%
      select(model, scenario, iso, variable, year, value) %>%
      mutate(
        fe.to.service.ratio = 1/value
      ) %>%
      drop_na(model)



    # step 4: create a dle scaler (with rho to translate)
    scenario.data.f2s.adj <- f2s.data.iso %>%
      mutate(rho = RHO) %>%
      mutate(
        dle.tech.scaler =

          # # option 1: rho is linear scaling (simple, but has the clear possibility of going negative if rho is >1)
          # 1-((1-(fe.to.service.ratio))*rho)


          # option 2: rho is exponential scaling
          ((fe.to.service.ratio)*rho) # default choice

        # tbd. future options?
        # future visualisation idea: show effect of different rho-scaling options.
      ) %>%
      select(model, scenario, iso, variable, year, dle.tech.scaler)
  } else if (
    IAM.OUTPUT.SHORT.FILENAME == "JUSTMIP"
  ) {
    # step 1: load data
    # ... the if statement is here to save running time. I.e., if looping over calculator_run.R multiple times, you can set FIRST.TIME.RUN.PREPARE.REGIONAL.DATA to FALSE after the first run, and then this step will be faster for subsequent runs (using already saved out data, instead of reprocessing the same data)
    # ... saves about 10 seconds on my DELL laptop from ~2022


    # Prepare data from regional raw scenario files ----------------------------
    if (!exists("FIRST.TIME.RUN.PREPARE.REGIONAL.DATA")) {
      FIRST.TIME.RUN.PREPARE.REGIONAL.DATA <- TRUE
    }
    if (FIRST.TIME.RUN.PREPARE.REGIONAL.DATA){
      original.regionallevel.iam.data.path <- here(
        get_data_location_raw(test=FALSE),
        "scenario_data",
        "original_regional"
      )

      original.regionallevel.iam.data.files <- dir(original.regionallevel.iam.data.path,
                                                   pattern = ORIGINAL.IAM.DATA.FILE) %>%  # get file name or names [currently, for JUSTMIP, we generally just work with 1 file]
        # remove files that are only Excel backups or temporary files
        str_subset(pattern = "^[^~$]", negate = FALSE)

      # Note: could consider interpolating IMAGE and REMIND 5-yearly data points here


      efficiency.scaler.vars <- c(
        "Final Energy|Residential and Commercial",
        "Useful Energy|RC Specific",
        "Useful Energy|RC Thermal",

        "Final Energy|Industry",
        "Useful Energy|Feedstocks",
        "Useful Energy|Industrial Specific",
        "Useful Energy|Industrial Thermal",

        "Final Energy|Transportation",
        "Useful Energy|Transport"

      )

      original.regionallevel.iam.data <- original.regionallevel.iam.data.files %>%
        map(~ (read_csv(file.path(original.regionallevel.iam.data.path,.)) %>%
                 filter(Variable%in%efficiency.scaler.vars) %>%
                 upper_to_lower() %>%
                 iamc_wide_to_long(upper.to.lower = F)) ) %>%
        reduce(rbind)

      saveRDS(
        original.regionallevel.iam.data,
        file = here(DATA.LOCATION, paste0("original_regionallevel-iam-data", "_",
                                          IAM.OUTPUT.SHORT.FILENAME, ".RData"))
      )

    } else {
      original.regionallevel.iam.data <- readRDS(
        here(DATA.LOCATION, paste0("original_regionallevel-iam-data", "_",
                                   IAM.OUTPUT.SHORT.FILENAME, ".RData"))
      )
    }


    # step 2: calculate changes (normalise along the way for ease of reading)

    original.iam.data.pivoted <- original.regionallevel.iam.data %>%
      select(-unit) %>%
      pivot_wider(names_from = variable, values_from = value)


    # Industry -----------------------------------------------------------------

    # step 2.1: calculate global index for industry
    service.efficiency.industry.global <- original.iam.data.pivoted %>%
      filter(region=="World") %>%
      mutate(
        global.service.ratio.industry = (
          (
            `Useful Energy|Feedstocks` + `Useful Energy|Industrial Specific` + `Useful Energy|Industrial Thermal`
          ) / (
            `Final Energy|Industry`
          )
        )
      ) %>%
      select(model,scenario,year,global.service.ratio.industry)


    # Buildings and Transport --------------------------------------------------
    # step 2.2: calculate regional indices for other regions
    service.efficiency <-
      original.iam.data.pivoted %>%
      # calculate ratios
      mutate(
        ### Residential and Commercial -----------------------------------------
        service.ratio.rescom = (
          `Useful Energy|RC Specific` + `Useful Energy|RC Thermal`
        ) / (`Final Energy|Residential and Commercial`),
        ### Transport ----------------------------------------------------------
        service.ratio.transport = `Useful Energy|Transport` / `Final Energy|Transportation`,
        #   ifelse(
        #   # take total passenger transport
        #   !is.na(`Energy Service|Transportation|Passenger`),
        #   `Energy Service|Transportation|Passenger`/`Final Energy|Transportation`,
        #   `Useful Energy|Transport` / `Final Energy|Transportation`
        # ),

        ### Industry (add in global ratios) ------------------------------------
        # - take a weighted sum of (iron and steel) and (cement) production
        #   ... [which together should make up ~50% of DLS flows & >90% of stock]
        # (implement using mutate_cond below, to increase readibility)
        service.ratio.industry = NA
      ) %>%
      left_join(service.efficiency.industry.global,
                by = c("model", "scenario", "year")) %>%
      mutate(service.ratio.industry = global.service.ratio.industry)



    # step 2.3: normalise (and do constant fill where necessary)
    service.efficiency.tmp <- service.efficiency %>%
      select(model,scenario,region,year,service.ratio.rescom,service.ratio.transport,service.ratio.industry) %>%
      pivot_longer(cols = c(service.ratio.rescom,service.ratio.industry,service.ratio.transport),
                   names_to = "variable", values_to = "value") %>%
      mutate(unit = "service/EJ") %>%
      arrange(model,scenario,region,variable,year)

    service.efficiency.normalised <- service.efficiency.tmp %>%

      # normalise
      normalise_iamc_long(starting.year = SCENARIO.START.YEAR) %>%

      # # fill constant
      # constant_year_fill(variable.to.fill = "value") %>%

      # and put in specified format
      mutate_cond(variable=="service.ratio.transport", variable="Final Energy|Transportation") %>%
      mutate_cond(variable=="service.ratio.industry", variable="Final Energy|Industry") %>%
      mutate_cond(variable=="service.ratio.rescom", variable="Final Energy|Residential and Commercial")

    # save out fe.to.ue factors for JUSTMIP loop
    if (exists("output.name.i")){
      write_delim(
        x = service.efficiency.tmp,
        file = paste0(output.name.i,
                      "_servicetofinal.csv"),
        delim = ","
      )
    }




    # step 3: map onto countries
    load_iam_region_mappings()

    if (MESSAGE.REGION.VERSION=="R12"){
      r.m <- regions.message.r12 %>% rename(region.message=region.message.r12)
    } else if (MESSAGE.REGION.VERSION=="R11"){
      r.m <- regions.message
    }
    f2s.data.iso <-
      r.m %>%
      left_join(service.efficiency.normalised %>%
                  rename(region.message=region), relationship = "many-to-many",
                by = "region.message") %>%
      select(model, scenario, iso, variable, year, value) %>%
      mutate(
        fe.to.service.ratio = 1/value
      ) %>%
      drop_na(model)



    # step 4: create a dle scaler (with rho to translate)
    scenario.data.f2s.adj <- f2s.data.iso %>%
      mutate(rho = RHO) %>%
      mutate(
        dle.tech.scaler =

          # # option 1: rho is linear scaling (simple, but has the clear possibility of going negative if rho is >1)
          # 1-((1-(fe.to.service.ratio))*rho)


          # option 2: rho is exponential scaling
          ((fe.to.service.ratio)*rho) # default choice

        # tbd. future options?
        # future visualisation idea: show effect of different rho-scaling options.
      ) %>%
      select(model, scenario, iso, variable, year, dle.tech.scaler)
  }



  # intermediary save out of the change in ratios of final energy to service
  save.out.intermediary.f2s <- F
  if (save.out.intermediary.f2s){
    saveRDS(scenario.data.f2s.adj,
      file = here(DATA.LOCATION, paste0("intermed_scenario_f2s_change", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData"))
    )
  }



  # step 5: apply to DLE threshold
  log_info("Scenario technology development: scale dle threshold")

  dle.constant.data <- readRDS(here(DATA.LOCATION,"dle-total-and-sectoral.RData"))

  dle.adjusted.data <- scenario.data.f2s.adj %>%
    left_join(
      dle.constant.data %>% rename(dle.threshold.curtech = dle),
      by = c("iso", "variable")
    ) %>%

    mutate(
      dle.threshold.adjusted = dle.threshold.curtech * dle.tech.scaler
    )

  # save out adjusted minimum energy needs
  saveRDS(dle.adjusted.data,
    file = here(DATA.LOCATION, paste0("projected_dle-total-and-sectoral-scaled", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData"))
  )
} else if (DLE.efficiency.scale == FALSE) {
  log_error("Calculator version without DLE efficiency scaling not yet implemented separately, but rather available as separate outputs.")
}
