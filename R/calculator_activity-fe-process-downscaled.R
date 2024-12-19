# Script for mapping and correcting national final energy activity data to be compared to DLE

STILL.TO.COMBINE.FROM.DOWNSCALED.RAW.OUTPUT <- F
if(STILL.TO.COMBINE.FROM.DOWNSCALED.RAW.OUTPUT){
  # for SHAPE:
  source(here("R", "calculator_activity-fe-process-downscaled-rawdata.R"))
}

load_iam_region_mappings()


if (exists("IAM.SCENARIO.DATA.FILE")) {

  # Load + format downscaled data --------------------------------------------
  if(IAM.OUTPUT.SHORT.FILENAME == "SHAPE-final" & grepl(IAM.SCENARIO.DATA.FILE, pattern="SHAPE-final_v3", fixed = T )) {
    processed.population.file <- "scenario_POPULATION_SHAPEv3.RData"
  } else if (IAM.OUTPUT.SHORT.FILENAME == "JUSTMIP"){
    processed.population.file <- "scenario_POPULATION_JUSTMIP.RData"
  }
  ### Load and format total final energy by country (in EJ/year) -------------
  # assume that this is downscaled data, following the output from Fabio, with IAMC-like variable names
  scenario.data.fe <- vroom(
    here(
      "data-raw",
      "scenario_data",
      paste0(IAM.SCENARIO.DATA.FILE, ".csv")
    ), show_col_types=FALSE
  ) %>% format_final_energy_downscaled_shape()

  ### Convert to per capita final energy (in GJ/cap/year) --------------------
  scenario.data.fe.pc <- scenario.data.fe %>%
    left_join(
      readRDS(here(DATA.LOCATION, processed.population.file)) %>% select(-c("variable", "unit")) %>%
        rename(pop_mil=value, iso=region),
      by = c("model", "scenario", "iso", "year")
    ) %>%
    mutate(
      energy.per.capita = ((value * exa) / (pop_mil * mega)) / giga,
      unit = "GJ/cap/yr"
    ) %>%
    select(-value) %>%  # keeping pop_mil, because it could be useful contextual information, though it would not be strictly necessary to keep
    mutate(energy.per.capita.uncorrected = energy.per.capita)



  # Adjust buildings -----------------------------------------------------------

  load_iam_region_mappings() # required for matching IMAGE or MESSAGEix-Buildings regions to countries


  ### process files if they haven't been processed yet
  if (!exists("FIRST.TIME.RUN.PREPARE.REGIONAL.DATA")) {
    FIRST.TIME.RUN.PREPARE.REGIONAL.DATA <- TRUE
  }
  if (FIRST.TIME.RUN.PREPARE.REGIONAL.DATA){
    log_info(paste0("DLE calculator: prepare buildings energy correction data."))
    source(here("R", "calculator_activity-fe-process-IMAGE-rescom-data.R")) # used for SHAPE ["image-SHAPE-mapped"]
  }

  ### split residential and commercial -----------------------------------------
  # correction carried out here:
  # ... rescom_new = rescom_old * (residential_share) +  rescom_old * (1-residential_share) * (commercial.share.keep)

  res.split.method <- RES.TRADBIO.SPLIT.METHOD.CHOICE
  # res.split.method <- "image-SHAPE-mapped" # SHAPE SDP project
  if (
    res.split.method == "image-SHAPE-mapped" | res.split.method == "IMAGE-SHAPE-mapped-for-MESSAGE"
  ){


    ### decide how large of a part of the commercial energy is kept for comparison with DLE data
    commercial.share.keep <- COMMERCIAL.SHARE.TO.KEEP # currently very simple, in lack of useful regional data, we pick the US-based commercial buildings survey split

    ##### Part 1: scenarios that DID NOT model residential energy consumption separately from commercial
    ##### Load splitting data (from the IMAGE model)
    # mapping data
    mapping.data.rescom.split <- vroom(here(
      "data-raw",
      "scenario_data",
      "mapping_rescom_splits",
      IAM.MAPPING.RESCOM.SPLIT.FILENAME
    ), show_col_types=FALSE)

    # load image split data options (directly onto country level)
    rescom.split.data.image <- regions.image %>% rename(Region = region.image) %>%
      left_join(
        vroom(here("data-raw", "scenario_data", "mapping_rescom_splits",
                   "IMAGE_allSHAPE_ResidentialCommercialSplit_v3.csv"), show_col_types=FALSE) %>%
          iamc_wide_to_long() %>%
          pivot_wider(values_from = value, names_from = Variable) %>%
          mutate(
            res.ratio = `Final Energy|Residential` / `Final Energy|Residential and Commercial`
          ),
        relationship = "many-to-many",
        by = "Region"
      ) %>%
      rename(model.for.splitting = Model,
             scenario.for.splitting = Scenario) %>%
      select(
        model.for.splitting, scenario.for.splitting, iso, year, res.ratio
      )


    ##### Scenarios: which need to be split?
    rescom.scenarios.tobesplit <- mapping.data.rescom.split %>% filter(mapping.rescomsplit!="self") %>% ms_unique()
    scenario.data.fe.pc.tobesplit.rescom <- scenario.data.fe.pc %>%
      ms_add() %>%
      filter(`model-scenario` %in% rescom.scenarios.tobesplit) %>%
      select(-`model-scenario`)

    ##### Create scenario-country combinations (no model necessary, because we only have IMAGE, but this will likely need changing if we make this more elaborate)
    # create all combinations of scenarios and countries
    rescom.split.data.iso.prep <- scenario.data.fe.pc.tobesplit.rescom %>%
      left_join(mapping.data.rescom.split,
                by = c("model", "scenario")) %>%
      left_join(rescom.split.data.image %>% select(-model.for.splitting),
                by = c(#"mapping.rescomsplit.model"="model.for.splitting", # noting this down as an idea for later
                  #"mapping.rescomsplit.scenario"="scenario.for.splitting", # noting this down as an idea for later
                  "mapping.rescomsplit"="scenario.for.splitting",
                  "iso", "year")) %>%
      select(-mapping.rescomsplit)

    # Fill NAs where not reported
    rescom.split.data.iso.prep.filledNA <- rescom.split.data.iso.prep %>%
      left_join(regions.message,
                by = c("iso")) %>%
      # if NA, for share adjustement rates, fill in regional average
      # (MESSAGE region-based; where there's [almost] no NAs in the country-mapping)
      left_join(
        rescom.split.data.iso.prep %>%
          left_join(regions.message,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.ratio = mean(res.ratio, na.rm = T),
            .by = c("region.message",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.message)),
        by = c("region.message",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.ratio),
                  res.ratio = regional.mean.res.ratio) %>%
      # remove helper-columns
      select(-region.message,
             -regional.mean.res.ratio) %>%
      # do the same using IMAGE regions (where there's NAs in the country-mapping) to just cover more if needed - at the moment this doesn't change a thing
      left_join(regions.image,
                by = c("iso")) %>%
      left_join(
        rescom.split.data.iso.prep %>%
          left_join(regions.image,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.ratio = mean(res.ratio, na.rm = T),
            .by = c("region.image",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.image)),
        by = c("region.image",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.ratio),
                  res.ratio = regional.mean.res.ratio) %>%
      select(-region.image,
             -regional.mean.res.ratio) %>%
      # do the same using REMIND regions (at the moment this adds Curacao (CUW) and Sint-Maarten (SXM))
      left_join(regions.remind,
                by = c("iso")) %>%
      left_join(
        rescom.split.data.iso.prep %>%
          left_join(regions.remind,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.ratio = mean(res.ratio, na.rm = T),
            .by = c("region.remind",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.remind)),
        by = c("region.remind",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.ratio),
                  res.ratio = regional.mean.res.ratio) %>%
      select(-region.remind,
             -regional.mean.res.ratio)


    # ... do correction calculation
    rescom.split.data.iso <- rescom.split.data.iso.prep.filledNA %>%
      mutate_cond(variable=="Final Energy|Residential and Commercial",
                  energy.per.capita = (energy.per.capita * res.ratio) +
                    (energy.per.capita * (1-res.ratio) * commercial.share.keep))


    ##### Part 2: scenarios that DID model residential energy consumption separately from commercial

    if (
      length(mapping.data.rescom.split %>% filter(mapping.rescomsplit=="self") %>% ms_unique()) > 0
    ) {

      ##### Scenarios: which have already modelled the split?
      rescom.scenarios.alreadysplit <- mapping.data.rescom.split %>% filter(mapping.rescomsplit=="self") %>% ms_unique()
      scenario.data.fe.pc.alreadysplit.rescom <- scenario.data.fe.pc %>%
        ms_add() %>%
        filter(`model-scenario` %in% rescom.scenarios.alreadysplit) %>%
        select(-`model-scenario`)

      # no NAs here, since all is IMAGE only
      # ... do correction calculation
      rescom.original.data.iso <- scenario.data.fe.pc.alreadysplit.rescom %>%
        left_join(scenario.data.fe.pc.alreadysplit.rescom %>% filter(
          variable %in% c(
            "Final Energy|Residential",
            "Final Energy|Commercial"
          )) %>% select(-pop_mil, -energy.per.capita.uncorrected) %>% pivot_wider(values_from = energy.per.capita, names_from = variable),
          by = c("model", "scenario", "iso", "unit", "year")
        ) %>%
        mutate_cond(variable=="Final Energy|Residential and Commercial",
                    energy.per.capita = (`Final Energy|Residential`) +
                      (`Final Energy|Commercial` * commercial.share.keep)) %>%
        mutate(res.ratio = `Final Energy|Residential`/(`Final Energy|Commercial` + `Final Energy|Residential`) ) %>%
        select(-`Final Energy|Residential`, -`Final Energy|Commercial`)


      # Combine data from both scenario sets
      scenario.data.fe.pc.splitresidential <- rescom.split.data.iso %>%
        bind_rows(rescom.original.data.iso)

    } else {

      scenario.data.fe.pc.splitresidential <- rescom.split.data.iso

    }

  } else if (
    res.split.method == "MBuildings-SHAPE-mapped-for-MESSAGE"
  ){

    ### decide how large of a part of the commercial energy is kept for comparison with DLE data
    commercial.share.keep <- COMMERCIAL.SHARE.TO.KEEP # currently very simple, in lack of useful regional data, we pick the US-based commercial buildings survey split

    ##### Part 1: scenarios that DID NOT model residential energy consumption separately from commercial
    ##### Load splitting data (from the IMAGE model)
    # mapping data
    mapping.data.rescom.split <- vroom(here(
      "data-raw",
      "scenario_data",
      "mapping_rescom_splits",
      IAM.MAPPING.RESCOM.SPLIT.FILENAME
    ), show_col_types=FALSE)

    # load image split data options (directly onto country level)
    rescom.split.data.MBuildings <- regions.message.r12 %>% rename(Region = region.message.r12) %>%
      left_join(
        vroom(here("data-raw", "scenario_data", "mapping_rescom_splits",
                   "MESSAGEixBuildings_allSHAPE_ResidentialCommercialSplit_v1.csv"), show_col_types=FALSE) %>%
          mutate(Region = substr(Region, start=5, stop=nchar(Region))) %>%
          iamc_wide_to_long() %>%
          pivot_wider(values_from = value, names_from = Variable) %>%
          mutate(
            res.ratio = `Final Energy|Residential` / `Final Energy|Residential and Commercial`
          ),
        relationship = "many-to-many",
        by = "Region"
      ) %>%
      rename(model.for.splitting = Model,
             scenario.for.splitting = Scenario) %>%
      select(
        model.for.splitting, scenario.for.splitting, iso, year, res.ratio
      )


    ##### Scenarios: which need to be split?
    rescom.scenarios.tobesplit <- mapping.data.rescom.split %>% filter(mapping.rescomsplit!="self") %>% ms_unique()
    scenario.data.fe.pc.tobesplit.rescom <- scenario.data.fe.pc %>%
      ms_add() %>%
      filter(`model-scenario` %in% rescom.scenarios.tobesplit) %>%
      select(-`model-scenario`)

    ##### Create scenario-country combinations (no model necessary, because we only have IMAGE, but this will likely need changing if we make this more elaborate)
    # create all combinations of scenarios and countries
    rescom.split.data.iso.prep <- scenario.data.fe.pc.tobesplit.rescom %>%
      left_join(mapping.data.rescom.split,
                by = c("model", "scenario")) %>%
      left_join(rescom.split.data.MBuildings %>% select(-model.for.splitting),
                by = c(#"mapping.rescomsplit.model"="model.for.splitting", # noting this down as an idea for later
                  #"mapping.rescomsplit.scenario"="scenario.for.splitting", # noting this down as an idea for later
                  "mapping.rescomsplit"="scenario.for.splitting",
                  "iso", "year")) %>%
      select(-mapping.rescomsplit)


    # Fill NAs where not reported
    rescom.split.data.iso.prep.filledNA <- rescom.split.data.iso.prep %>%
      left_join(regions.message,
                by = c("iso")) %>%
      # if NA, for share adjustement rates, fill in regional average
      # (MESSAGE region-based; where there's [almost] no NAs in the country-mapping)
      left_join(
        rescom.split.data.iso.prep %>%
          left_join(regions.message,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.ratio = mean(res.ratio, na.rm = T),
            .by = c("region.message",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.message)),
        by = c("region.message",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.ratio),
                  res.ratio = regional.mean.res.ratio) %>%
      # remove helper-columns
      select(-region.message,
             -regional.mean.res.ratio) %>%
      # do the same using M-Buidlings regions (where there's NAs in the country-mapping) to just cover more if needed - at the moment this doesn't change a thing
      left_join(regions.message.r12,
                by = c("iso")) %>%
      left_join(
        rescom.split.data.iso.prep %>%
          left_join(regions.message.r12,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.ratio = mean(res.ratio, na.rm = T),
            .by = c("region.message.r12",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.message.r12)),
        by = c("region.message.r12",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.ratio),
                  res.ratio = regional.mean.res.ratio) %>%
      select(-region.message.r12,
             -regional.mean.res.ratio) %>%
      # do the same using REMIND regions (at the moment this adds Curacao (CUW) and Sint-Maarten (SXM))
      left_join(regions.remind,
                by = c("iso")) %>%
      left_join(
        rescom.split.data.iso.prep %>%
          left_join(regions.remind,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.ratio = mean(res.ratio, na.rm = T),
            .by = c("region.remind",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.remind)),
        by = c("region.remind",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.ratio),
                  res.ratio = regional.mean.res.ratio) %>%
      select(-region.remind,
             -regional.mean.res.ratio)


    # ... do correction calculation
    rescom.split.data.iso <- rescom.split.data.iso.prep.filledNA %>%
      mutate_cond(variable=="Final Energy|Residential and Commercial",
                  energy.per.capita = (energy.per.capita * res.ratio) +
                    (energy.per.capita * (1-res.ratio) * commercial.share.keep))


    ##### Part 2: scenarios that DID model residential energy consumption separately from commercial

    ##### Scenarios: which have already modelled the split?
    if (
      length(mapping.data.rescom.split %>% filter(mapping.rescomsplit=="self") %>% ms_unique()) > 0
    ) {
      rescom.scenarios.alreadysplit <- mapping.data.rescom.split %>% filter(mapping.rescomsplit=="self") %>% ms_unique()
      scenario.data.fe.pc.alreadysplit.rescom <- scenario.data.fe.pc %>%
        ms_add() %>%
        filter(`model-scenario` %in% rescom.scenarios.alreadysplit) %>%
        select(-`model-scenario`)

      # should not be any NAs here, since it is all the same model
      # ... do correction calculation
      rescom.original.data.iso <- scenario.data.fe.pc.alreadysplit.rescom %>%
        left_join(scenario.data.fe.pc.alreadysplit.rescom %>% filter(
          variable %in% c(
            "Final Energy|Residential",
            "Final Energy|Commercial"
          )) %>% select(-pop_mil, -energy.per.capita.uncorrected) %>% pivot_wider(values_from = energy.per.capita, names_from = variable),
          by = c("model", "scenario", "iso", "unit", "year")
        ) %>%
        mutate_cond(variable=="Final Energy|Residential and Commercial",
                    energy.per.capita = (`Final Energy|Residential`) +
                      (`Final Energy|Commercial` * commercial.share.keep)) %>%
        mutate(res.ratio = `Final Energy|Residential`/(`Final Energy|Commercial` + `Final Energy|Residential`) ) %>%
        select(-`Final Energy|Residential`, -`Final Energy|Commercial`)


      # Combine data from both scenario sets
      scenario.data.fe.pc.splitresidential <- rescom.split.data.iso %>%
        bind_rows(rescom.original.data.iso)
    } else {
      scenario.data.fe.pc.splitresidential <- rescom.split.data.iso
    }




  } else if (
    res.split.method == "MBuildings-SSP-mapped-for-MESSAGE"
  ){

    ### decide how large of a part of the commercial energy is kept for comparison with DLE data
    commercial.share.keep <- COMMERCIAL.SHARE.TO.KEEP # currently very simple, in lack of useful regional data, we pick the US-based commercial buildings survey split

    ##### Part 1: scenarios that DID NOT model residential energy consumption separately from commercial
    ##### Load splitting data (from the IMAGE model)
    # mapping data
    mapping.data.rescom.split <- vroom(here(
      "data-raw",
      "scenario_data",
      "mapping_rescom_splits",
      IAM.MAPPING.RESCOM.SPLIT.FILENAME
    ), show_col_types=FALSE)

    # load image split data options (directly onto country level)
    rescom.split.data.MBuildings <- regions.message.r12 %>% rename(Region = region.message.r12) %>%
      left_join(
        vroom(here("data-raw", "scenario_data", "mapping_rescom_splits",
                   "MESSAGEixBuildings_allSSP_ResidentialCommercialSplit_v1.csv"), show_col_types=FALSE) %>%
          mutate(Region = substr(Region, start=5, stop=nchar(Region))) %>%
          iamc_wide_to_long() %>%
          pivot_wider(values_from = value, names_from = Variable) %>%
          mutate(
            res.ratio = `Final Energy|Residential` / `Final Energy|Residential and Commercial`
          ),
        relationship = "many-to-many",
        by = "Region"
      ) %>%
      rename(model.for.splitting = Model,
             scenario.for.splitting = Scenario) %>%
      select(
        model.for.splitting, scenario.for.splitting, iso, year, res.ratio
      )


    ##### Scenarios: which need to be split?
    rescom.scenarios.tobesplit <- mapping.data.rescom.split %>% filter(mapping.rescomsplit!="self") %>% ms_unique()
    scenario.data.fe.pc.tobesplit.rescom <- scenario.data.fe.pc %>%
      ms_add() %>%
      filter(`model-scenario` %in% rescom.scenarios.tobesplit) %>%
      select(-`model-scenario`)

    ##### Create scenario-country combinations (no model necessary, because we only have IMAGE, but this will likely need changing if we make this more elaborate)
    # create all combinations of scenarios and countries
    rescom.split.data.iso.prep <- scenario.data.fe.pc.tobesplit.rescom %>%
      left_join(mapping.data.rescom.split,
                by = c("model", "scenario")) %>%
      left_join(rescom.split.data.MBuildings %>% select(-model.for.splitting),
                by = c(#"mapping.rescomsplit.model"="model.for.splitting", # noting this down as an idea for later
                  #"mapping.rescomsplit.scenario"="scenario.for.splitting", # noting this down as an idea for later
                  "mapping.rescomsplit"="scenario.for.splitting",
                  "iso", "year")) %>%
      select(-mapping.rescomsplit)


    # Fill NAs where not reported
    rescom.split.data.iso.prep.filledNA <- rescom.split.data.iso.prep %>%
      left_join(regions.message,
                by = c("iso")) %>%
      # if NA, for share adjustement rates, fill in regional average
      # (MESSAGE region-based; where there's [almost] no NAs in the country-mapping)
      left_join(
        rescom.split.data.iso.prep %>%
          left_join(regions.message,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.ratio = mean(res.ratio, na.rm = T),
            .by = c("region.message",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.message)),
        by = c("region.message",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.ratio),
                  res.ratio = regional.mean.res.ratio) %>%
      # remove helper-columns
      select(-region.message,
             -regional.mean.res.ratio) %>%
      # do the same using M-Buidlings regions (where there's NAs in the country-mapping) to just cover more if needed - at the moment this doesn't change a thing
      left_join(regions.message.r12,
                by = c("iso")) %>%
      left_join(
        rescom.split.data.iso.prep %>%
          left_join(regions.message.r12,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.ratio = mean(res.ratio, na.rm = T),
            .by = c("region.message.r12",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.message.r12)),
        by = c("region.message.r12",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.ratio),
                  res.ratio = regional.mean.res.ratio) %>%
      select(-region.message.r12,
             -regional.mean.res.ratio) %>%
      # do the same using REMIND regions (at the moment this adds Curacao (CUW) and Sint-Maarten (SXM))
      left_join(regions.remind,
                by = c("iso")) %>%
      left_join(
        rescom.split.data.iso.prep %>%
          left_join(regions.remind,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.ratio = mean(res.ratio, na.rm = T),
            .by = c("region.remind",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.remind)),
        by = c("region.remind",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.ratio),
                  res.ratio = regional.mean.res.ratio) %>%
      select(-region.remind,
             -regional.mean.res.ratio)


    # ... do correction calculation
    rescom.split.data.iso <- rescom.split.data.iso.prep.filledNA %>%
      mutate_cond(variable=="Final Energy|Residential and Commercial",
                  energy.per.capita = (energy.per.capita * res.ratio) +
                    (energy.per.capita * (1-res.ratio) * commercial.share.keep))


    ##### Part 2: scenarios that DID model residential energy consumption separately from commercial

    ##### Scenarios: which have already modelled the split?
    if (
      length(mapping.data.rescom.split %>% filter(mapping.rescomsplit=="self") %>% ms_unique()) > 0
    ) {
      rescom.scenarios.alreadysplit <- mapping.data.rescom.split %>% filter(mapping.rescomsplit=="self") %>% ms_unique()
      scenario.data.fe.pc.alreadysplit.rescom <- scenario.data.fe.pc %>%
        ms_add() %>%
        filter(`model-scenario` %in% rescom.scenarios.alreadysplit) %>%
        select(-`model-scenario`)

      # should not be any NAs here, since it is all the same model
      # ... do correction calculation
      rescom.original.data.iso <- scenario.data.fe.pc.alreadysplit.rescom %>%
        left_join(scenario.data.fe.pc.alreadysplit.rescom %>% filter(
          variable %in% c(
            "Final Energy|Residential",
            "Final Energy|Commercial"
          )) %>% select(-pop_mil, -energy.per.capita.uncorrected) %>% pivot_wider(values_from = energy.per.capita, names_from = variable),
          by = c("model", "scenario", "iso", "unit", "year")
        ) %>%
        mutate_cond(variable=="Final Energy|Residential and Commercial",
                    energy.per.capita = (`Final Energy|Residential`) +
                      (`Final Energy|Commercial` * commercial.share.keep)) %>%
        mutate(res.ratio = `Final Energy|Residential`/(`Final Energy|Commercial` + `Final Energy|Residential`) ) %>%
        select(-`Final Energy|Residential`, -`Final Energy|Commercial`)


      # Combine data from both scenario sets
      scenario.data.fe.pc.splitresidential <- rescom.split.data.iso %>%
        bind_rows(rescom.original.data.iso)
    } else {
      scenario.data.fe.pc.splitresidential <- rescom.split.data.iso
    }




  }

  ### traditional biomass from residential total -------------------------------
  res.tradbio.split.method <- RES.TRADBIO.SPLIT.METHOD.CHOICE
  # res.tradbio.split.method <- "image-SHAPE-mapped" # SHAPE SDP project
  if (
    res.tradbio.split.method == "image-SHAPE-mapped"
  ){
    # correction carried out here:
    # ... rescom_new_clean = rescom_new - (residential_consumption * (traditional_biomass_share))


    ##### Part 1: scenarios that DID NOT model residential traditional biomass consumption
    ##### Load splitting data (from the IMAGE model)
    # mapping data
    mapping.data.dirtyres.split <- vroom(here(
      "data-raw",
      "scenario_data",
      "mapping_rescom_splits",
      IAM.MAPPING.DIRTYRES.SPLIT.FILENAME
    ), show_col_types=FALSE)

    # load image split data options (directly onto country level)
    dirtyres.split.data.image <- regions.image %>% rename(Region = region.image) %>%
      left_join(
        vroom(here("data-raw", "scenario_data", "mapping_rescom_splits",
                   "IMAGE_allSHAPE_TraditionalBiomass_Residential_v3.csv"), show_col_types=FALSE) %>%
          iamc_wide_to_long() %>%
          pivot_wider(values_from = value, names_from = Variable) %>%
          mutate(
            res.tradbio.ratio = `Final Energy|Residential|Solids|Biomass|Traditional` / `Final Energy|Residential`
          ),
        relationship = "many-to-many",
        by = "Region"
      ) %>%
      rename(model.for.splitting = Model,
             scenario.for.splitting = Scenario) %>%
      select(
        model.for.splitting, scenario.for.splitting, iso, year, res.tradbio.ratio
      )

    ##### Scenarios: which need to be split?
    dirtyres.scenarios.tobesplit <- mapping.data.dirtyres.split %>% filter(mapping.rescomsplit!="self") %>% ms_unique()
    scenario.data.fe.pc.tobesplit.dirtyres <- scenario.data.fe.pc.splitresidential %>%
      ms_add() %>%
      filter(`model-scenario` %in% dirtyres.scenarios.tobesplit) %>%
      select(-`model-scenario`)

    ##### Create scenario-country combinations (no model necessary, because we only have IMAGE, but this will likely need changing if we make this more elaborate)
    # create all combinations of scenarios and countries
    dirtyres.split.data.iso.prep <- scenario.data.fe.pc.tobesplit.dirtyres %>%
      left_join(mapping.data.dirtyres.split,
                by = c("model", "scenario")) %>%
      left_join(dirtyres.split.data.image %>% select(-model.for.splitting),
                by = c(#"mapping.rescomsplit.model"="model.for.splitting", # noting this down as an idea for later
                  #"mapping.rescomsplit.scenario"="scenario.for.splitting", # noting this down as an idea for later
                  "mapping.rescomsplit"="scenario.for.splitting",
                  "iso", "year")) %>%
      select(-mapping.rescomsplit)

    # Fill NAs where not reported
    dirtyres.split.data.iso.prep.filledNA <- dirtyres.split.data.iso.prep %>%
      left_join(regions.message,
                by = c("iso")) %>%
      # if NA, for share adjustement rates, fill in regional average
      # (MESSAGE region-based; where there's [almost] no NAs in the country-mapping)
      left_join(
        dirtyres.split.data.iso.prep %>%
          left_join(regions.message,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.tradbio.ratio = mean(res.tradbio.ratio, na.rm = T),
            .by = c("region.message",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.message)),
        by = c("region.message",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.tradbio.ratio),
                  res.tradbio.ratio = regional.mean.res.tradbio.ratio) %>%
      # remove helper-columns
      select(-region.message,
             -regional.mean.res.tradbio.ratio) %>%
      # do the same using IMAGE regions (where there's NAs in the country-mapping) to just cover more if needed - at the moment this doesn't change a thing
      left_join(regions.image,
                by = c("iso")) %>%
      left_join(
        dirtyres.split.data.iso.prep %>%
          left_join(regions.image,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.tradbio.ratio = mean(res.tradbio.ratio, na.rm = T),
            .by = c("region.image",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.image)),
        by = c("region.image",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.tradbio.ratio),
                  res.tradbio.ratio = regional.mean.res.tradbio.ratio) %>%
      select(-region.image,
             -regional.mean.res.tradbio.ratio) %>%
      # do the same using REMIND regions (at the moment this adds Curacao (CUW) and Sint-Maarten (SXM))
      left_join(regions.remind,
                by = c("iso")) %>%
      left_join(
        dirtyres.split.data.iso.prep %>%
          left_join(regions.remind,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.tradbio.ratio = mean(res.tradbio.ratio, na.rm = T),
            .by = c("region.remind",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.remind)),
        by = c("region.remind",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.tradbio.ratio),
                  res.tradbio.ratio = regional.mean.res.tradbio.ratio) %>%
      select(-region.remind,
             -regional.mean.res.tradbio.ratio)

    # ... do correction calculation
    dirtyres.split.data.iso <- dirtyres.split.data.iso.prep.filledNA %>%
      mutate_cond(variable=="Final Energy|Residential and Commercial",
                  energy.per.capita =  energy.per.capita - ((energy.per.capita.uncorrected * res.ratio) * res.tradbio.ratio) )


    ##### Part 2: scenarios that DID model residential traditional biomass consumption

    ##### Scenarios: which have already modelled the split?
    dirtyres.scenarios.alreadysplit <- mapping.data.dirtyres.split %>% filter(mapping.rescomsplit=="self") %>% ms_unique()
    scenario.data.fe.pc.alreadysplit.dirtyres <- scenario.data.fe.pc.splitresidential %>%
      ms_add() %>%
      filter(`model-scenario` %in% dirtyres.scenarios.alreadysplit) %>%
      select(-`model-scenario`)

    # ... do correction calculation
    # ... ... (note: since we haven't downscaled residential traditional biomass use, we'll also need to use the region-to-country-level mapping here, actually ...; if we ever downscale traditional biomass, the two blocks below can be rewritten following the residencial share split code just above)

    dirtyres.original.data.iso.prep <- scenario.data.fe.pc.alreadysplit.dirtyres %>%
      left_join(mapping.data.dirtyres.split %>% mutate_cond(
        mapping.rescomsplit=="self",
        mapping.rescomsplit=scenario
      ),
      by = c("model", "scenario")
      ) %>%
      # use image-derived data (there's e.g. no MESSAGE data at the moment!), mapped onto country-level
      left_join(dirtyres.split.data.image %>% select(-model.for.splitting),
                by = c(#"mapping.rescomsplit.model"="model.for.splitting", # noting this down as an idea for later
                  #"mapping.rescomsplit.scenario"="scenario.for.splitting", # noting this down as an idea for later
                  "mapping.rescomsplit"="scenario.for.splitting",
                  "iso", "year")) %>%
      select(-mapping.rescomsplit)


    # ... do correction calculation
    dirtyres.original.data.iso <- dirtyres.original.data.iso.prep %>%
      mutate_cond(variable=="Final Energy|Residential and Commercial",
                  energy.per.capita =  energy.per.capita - ((energy.per.capita.uncorrected * res.ratio) * res.tradbio.ratio) )


    # Combine data from both scenario sets
    scenario.data.fe.pc.splitresidential.clean <- dirtyres.split.data.iso %>%
      bind_rows(dirtyres.original.data.iso)



  } else if (
    res.tradbio.split.method == "MBuildings-SHAPE-mapped-for-MESSAGE"
  ){

    # correction carried out here:
    # ... rescom_new_clean = rescom_new - (residential_consumption * (solid_biomass_share))


    ##### Part 1: scenarios that DID NOT model residential solid biomass consumption
    ##### Load splitting data (from the MESSAGEix-Buildings model)
    # mapping data
    mapping.data.dirtyres.split <- vroom(here(
      "data-raw",
      "scenario_data",
      "mapping_rescom_splits",
      IAM.MAPPING.DIRTYRES.SPLIT.FILENAME
    ), show_col_types=FALSE)

    # load image split data options (directly onto country level)
    dirtyres.split.data.MBuildings <- regions.message.r12 %>% rename(Region = region.message.r12) %>%
      left_join(
        vroom(here("data-raw", "scenario_data", "mapping_rescom_splits",
                   "MESSAGEixBuildings_allSHAPE_TraditionalBiomass_Residential_v1.csv"), show_col_types=FALSE) %>%
          mutate(Region = substr(Region, start=5, stop=nchar(Region))) %>%
          iamc_wide_to_long() %>%
          pivot_wider(values_from = value, names_from = Variable) %>%
          mutate(
            res.tradbio.ratio = `Final Energy|Residential|Solids|Biomass` / `Final Energy|Residential`
          ),
        relationship = "many-to-many",
        by = "Region"
      ) %>%
      rename(model.for.splitting = Model,
             scenario.for.splitting = Scenario) %>%
      select(
        model.for.splitting, scenario.for.splitting, iso, year, res.tradbio.ratio
      )

    ##### Scenarios: which need to be split?
    dirtyres.scenarios.tobesplit <- mapping.data.dirtyres.split %>% filter(mapping.rescomsplit!="self") %>% ms_unique()
    scenario.data.fe.pc.tobesplit.dirtyres <- scenario.data.fe.pc.splitresidential %>%
      ms_add() %>%
      filter(`model-scenario` %in% dirtyres.scenarios.tobesplit) %>%
      select(-`model-scenario`)

    ##### Create scenario-country combinations (no model necessary, because we only have IMAGE, but this will likely need changing if we make this more elaborate)
    # create all combinations of scenarios and countries
    dirtyres.split.data.iso.prep <- scenario.data.fe.pc.tobesplit.dirtyres %>%
      left_join(mapping.data.dirtyres.split,
                by = c("model", "scenario")) %>%
      left_join(dirtyres.split.data.MBuildings %>% select(-model.for.splitting),
                by = c(#"mapping.rescomsplit.model"="model.for.splitting", # noting this down as an idea for later
                  #"mapping.rescomsplit.scenario"="scenario.for.splitting", # noting this down as an idea for later
                  "mapping.rescomsplit"="scenario.for.splitting",
                  "iso", "year")) %>%
      select(-mapping.rescomsplit)

    # Fill NAs where not reported
    dirtyres.split.data.iso.prep.filledNA <- dirtyres.split.data.iso.prep %>%
      left_join(regions.message,
                by = c("iso")) %>%
      # if NA, for share adjustement rates, fill in regional average
      # (MESSAGE region-based; where there's [almost] no NAs in the country-mapping)
      left_join(
        dirtyres.split.data.iso.prep %>%
          left_join(regions.message,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.tradbio.ratio = mean(res.tradbio.ratio, na.rm = T),
            .by = c("region.message",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.message)),
        by = c("region.message",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.tradbio.ratio),
                  res.tradbio.ratio = regional.mean.res.tradbio.ratio) %>%
      # remove helper-columns
      select(-region.message,
             -regional.mean.res.tradbio.ratio) %>%
      # do the same using MESSAGE regions (where there's NAs in the country-mapping) to just cover more if needed - at the moment this doesn't change a thing
      left_join(regions.message.r12,
                by = c("iso")) %>%
      left_join(
        dirtyres.split.data.iso.prep %>%
          left_join(regions.message.r12,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.tradbio.ratio = mean(res.tradbio.ratio, na.rm = T),
            .by = c("region.message.r12",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.message.r12)),
        by = c("region.message.r12",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.tradbio.ratio),
                  res.tradbio.ratio = regional.mean.res.tradbio.ratio) %>%
      select(-region.message.r12,
             -regional.mean.res.tradbio.ratio) %>%
      # do the same using REMIND regions (at the moment this adds Curacao (CUW) and Sint-Maarten (SXM))
      left_join(regions.remind,
                by = c("iso")) %>%
      left_join(
        dirtyres.split.data.iso.prep %>%
          left_join(regions.remind,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.tradbio.ratio = mean(res.tradbio.ratio, na.rm = T),
            .by = c("region.remind",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.remind)),
        by = c("region.remind",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.tradbio.ratio),
                  res.tradbio.ratio = regional.mean.res.tradbio.ratio) %>%
      select(-region.remind,
             -regional.mean.res.tradbio.ratio)

    # ... do correction calculation
    dirtyres.split.data.iso <- dirtyres.split.data.iso.prep.filledNA %>%
      mutate_cond(variable=="Final Energy|Residential and Commercial",
                  energy.per.capita =  energy.per.capita - ((energy.per.capita.uncorrected * res.ratio) * res.tradbio.ratio) )


    ##### Part 2: scenarios that DID model residential traditional biomass consumption

    if (
      length(mapping.data.dirtyres.split %>% filter(mapping.rescomsplit=="self") %>% ms_unique()) > 0
    ) {
      ##### Scenarios: which have already modelled the split?
      dirtyres.scenarios.alreadysplit <- mapping.data.dirtyres.split %>% filter(mapping.rescomsplit=="self") %>% ms_unique()
      scenario.data.fe.pc.alreadysplit.dirtyres <- scenario.data.fe.pc.splitresidential %>%
        ms_add() %>%
        filter(`model-scenario` %in% dirtyres.scenarios.alreadysplit) %>%
        select(-`model-scenario`)

      # ... do correction calculation
      # ... ... (note: since we haven't downscaled residential traditional biomass use, we'll also need to use the region-to-country-level mapping here, actually ...; if we ever downscale traditional biomass, the two blocks below can be rewritten following the residencial share split code just above)

      dirtyres.original.data.iso.prep <- scenario.data.fe.pc.alreadysplit.dirtyres %>%
        left_join(mapping.data.dirtyres.split %>% mutate_cond(
          mapping.rescomsplit=="self",
          mapping.rescomsplit=scenario
        ),
        by = c("model", "scenario")
        ) %>%
        # use image-derived data (there's e.g. no MESSAGE data at the moment!), mapped onto country-level
        left_join(dirtyres.split.data.image %>% select(-model.for.splitting),
                  by = c(#"mapping.rescomsplit.model"="model.for.splitting", # noting this down as an idea for later
                    #"mapping.rescomsplit.scenario"="scenario.for.splitting", # noting this down as an idea for later
                    "mapping.rescomsplit"="scenario.for.splitting",
                    "iso", "year")) %>%
        select(-mapping.rescomsplit)


      # ... do correction calculation
      dirtyres.original.data.iso <- dirtyres.original.data.iso.prep %>%
        mutate_cond(variable=="Final Energy|Residential and Commercial",
                    energy.per.capita =  energy.per.capita - ((energy.per.capita.uncorrected * res.ratio) * res.tradbio.ratio) )


      # Combine data from both scenario sets
      scenario.data.fe.pc.splitresidential.clean <- dirtyres.split.data.iso %>%
        bind_rows(dirtyres.original.data.iso)
    } else {
      scenario.data.fe.pc.splitresidential.clean <- dirtyres.split.data.iso
    }



  } else if (
    res.tradbio.split.method == "MBuildings-SSP-mapped-for-MESSAGE"
  ){

    # correction carried out here:
    # ... rescom_new_clean = rescom_new - (residential_consumption * (solid_biomass_share))


    ##### Part 1: scenarios that DID NOT model residential solid biomass consumption
    ##### Load splitting data (from the MESSAGEix-Buildings model)
    # mapping data
    mapping.data.dirtyres.split <- vroom(here(
      "data-raw",
      "scenario_data",
      "mapping_rescom_splits",
      IAM.MAPPING.DIRTYRES.SPLIT.FILENAME
    ), show_col_types=FALSE)

    # load image split data options (directly onto country level)
    dirtyres.split.data.MBuildings <- regions.message.r12 %>% rename(Region = region.message.r12) %>%
      left_join(
        vroom(here("data-raw", "scenario_data", "mapping_rescom_splits",
                   "MESSAGEixBuildings_allSSP_TraditionalBiomass_Residential_v1.csv"), show_col_types=FALSE) %>%
          mutate(Region = substr(Region, start=5, stop=nchar(Region))) %>%
          iamc_wide_to_long() %>%
          pivot_wider(values_from = value, names_from = Variable) %>%
          mutate(
            res.tradbio.ratio = `Final Energy|Residential|Non-clean` / `Final Energy|Residential`
          ),
        relationship = "many-to-many",
        by = "Region"
      ) %>%
      rename(model.for.splitting = Model,
             scenario.for.splitting = Scenario) %>%
      select(
        model.for.splitting, scenario.for.splitting, iso, year, res.tradbio.ratio
      )

    ##### Scenarios: which need to be split?
    dirtyres.scenarios.tobesplit <- mapping.data.dirtyres.split %>% filter(mapping.rescomsplit!="self") %>% ms_unique()
    scenario.data.fe.pc.tobesplit.dirtyres <- scenario.data.fe.pc.splitresidential %>%
      ms_add() %>%
      filter(`model-scenario` %in% dirtyres.scenarios.tobesplit) %>%
      select(-`model-scenario`)

    ##### Create scenario-country combinations (no model necessary, because we only have IMAGE, but this will likely need changing if we make this more elaborate)
    # create all combinations of scenarios and countries
    dirtyres.split.data.iso.prep <- scenario.data.fe.pc.tobesplit.dirtyres %>%
      left_join(mapping.data.dirtyres.split,
                by = c("model", "scenario")) %>%
      left_join(dirtyres.split.data.MBuildings %>% select(-model.for.splitting),
                by = c(#"mapping.rescomsplit.model"="model.for.splitting", # noting this down as an idea for later
                  #"mapping.rescomsplit.scenario"="scenario.for.splitting", # noting this down as an idea for later
                  "mapping.rescomsplit"="scenario.for.splitting",
                  "iso", "year")) %>%
      select(-mapping.rescomsplit)

    # Fill NAs where not reported
    dirtyres.split.data.iso.prep.filledNA <- dirtyres.split.data.iso.prep %>%
      left_join(regions.message,
                by = c("iso")) %>%
      # if NA, for share adjustement rates, fill in regional average
      # (MESSAGE region-based; where there's [almost] no NAs in the country-mapping)
      left_join(
        dirtyres.split.data.iso.prep %>%
          left_join(regions.message,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.tradbio.ratio = mean(res.tradbio.ratio, na.rm = T),
            .by = c("region.message",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.message)),
        by = c("region.message",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.tradbio.ratio),
                  res.tradbio.ratio = regional.mean.res.tradbio.ratio) %>%
      # remove helper-columns
      select(-region.message,
             -regional.mean.res.tradbio.ratio) %>%
      # do the same using MESSAGE regions (where there's NAs in the country-mapping) to just cover more if needed - at the moment this doesn't change a thing
      left_join(regions.message.r12,
                by = c("iso")) %>%
      left_join(
        dirtyres.split.data.iso.prep %>%
          left_join(regions.message.r12,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.tradbio.ratio = mean(res.tradbio.ratio, na.rm = T),
            .by = c("region.message.r12",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.message.r12)),
        by = c("region.message.r12",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.tradbio.ratio),
                  res.tradbio.ratio = regional.mean.res.tradbio.ratio) %>%
      select(-region.message.r12,
             -regional.mean.res.tradbio.ratio) %>%
      # do the same using REMIND regions (at the moment this adds Curacao (CUW) and Sint-Maarten (SXM))
      left_join(regions.remind,
                by = c("iso")) %>%
      left_join(
        dirtyres.split.data.iso.prep %>%
          left_join(regions.remind,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.tradbio.ratio = mean(res.tradbio.ratio, na.rm = T),
            .by = c("region.remind",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.remind)),
        by = c("region.remind",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.tradbio.ratio),
                  res.tradbio.ratio = regional.mean.res.tradbio.ratio) %>%
      select(-region.remind,
             -regional.mean.res.tradbio.ratio)

    # ... do correction calculation
    dirtyres.split.data.iso <- dirtyres.split.data.iso.prep.filledNA %>%
      mutate_cond(variable=="Final Energy|Residential and Commercial",
                  energy.per.capita =  energy.per.capita - ((energy.per.capita.uncorrected * res.ratio) * res.tradbio.ratio) )


    ##### Part 2: scenarios that DID model residential traditional biomass consumption

    if (
      length(mapping.data.dirtyres.split %>% filter(mapping.rescomsplit=="self") %>% ms_unique()) > 0
    ) {
      ##### Scenarios: which have already modelled the split?
      dirtyres.scenarios.alreadysplit <- mapping.data.dirtyres.split %>% filter(mapping.rescomsplit=="self") %>% ms_unique()
      scenario.data.fe.pc.alreadysplit.dirtyres <- scenario.data.fe.pc.splitresidential %>%
        ms_add() %>%
        filter(`model-scenario` %in% dirtyres.scenarios.alreadysplit) %>%
        select(-`model-scenario`)

      # ... do correction calculation
      # ... ... (note: since we haven't downscaled residential traditional biomass use, we'll also need to use the region-to-country-level mapping here, actually ...; if we ever downscale traditional biomass, the two blocks below can be rewritten following the residencial share split code just above)

      dirtyres.original.data.iso.prep <- scenario.data.fe.pc.alreadysplit.dirtyres %>%
        left_join(mapping.data.dirtyres.split %>% mutate_cond(
          mapping.rescomsplit=="self",
          mapping.rescomsplit=scenario
        ),
        by = c("model", "scenario")
        ) %>%
        # use image-derived data (there's e.g. no MESSAGE data at the moment!), mapped onto country-level
        left_join(dirtyres.split.data.image %>% select(-model.for.splitting),
                  by = c(#"mapping.rescomsplit.model"="model.for.splitting", # noting this down as an idea for later
                    #"mapping.rescomsplit.scenario"="scenario.for.splitting", # noting this down as an idea for later
                    "mapping.rescomsplit"="scenario.for.splitting",
                    "iso", "year")) %>%
        select(-mapping.rescomsplit)


      # ... do correction calculation
      dirtyres.original.data.iso <- dirtyres.original.data.iso.prep %>%
        mutate_cond(variable=="Final Energy|Residential and Commercial",
                    energy.per.capita =  energy.per.capita - ((energy.per.capita.uncorrected * res.ratio) * res.tradbio.ratio) )


      # Combine data from both scenario sets
      scenario.data.fe.pc.splitresidential.clean <- dirtyres.split.data.iso %>%
        bind_rows(dirtyres.original.data.iso)
    } else {
      scenario.data.fe.pc.splitresidential.clean <- dirtyres.split.data.iso
    }



  } else if (
    res.tradbio.split.method == "IMAGE-SHAPE-mapped-for-MESSAGE"
  ){
    ##### Part 1: scenarios that DID NOT model residential traditional biomass consumption
    ##### Load splitting data (from the IMAGE model)
    # mapping data
    mapping.data.dirtyres.split <- vroom(here(
      "data-raw",
      "scenario_data",
      "mapping_rescom_splits",
      IAM.MAPPING.DIRTYRES.SPLIT.FILENAME
    ), show_col_types=FALSE)

    # load image split data options (directly onto country level)
    dirtyres.split.data.image <- regions.image %>% rename(Region = region.image) %>%
      left_join(
        vroom(here("data-raw", "scenario_data", "mapping_rescom_splits",
                   "IMAGE_allSHAPE_TraditionalBiomass_Residential_v3.csv"), show_col_types=FALSE) %>%
          iamc_wide_to_long() %>%
          pivot_wider(values_from = value, names_from = Variable) %>%
          mutate(
            res.tradbio.ratio = `Final Energy|Residential|Solids|Biomass|Traditional` / `Final Energy|Residential`
          ),
        relationship = "many-to-many",
        by = "Region"
      ) %>%
      rename(model.for.splitting = Model,
             scenario.for.splitting = Scenario) %>%
      select(
        model.for.splitting, scenario.for.splitting, iso, year, res.tradbio.ratio
      )

    ##### Scenarios: which need to be split?
    dirtyres.scenarios.tobesplit <- mapping.data.dirtyres.split %>% filter(mapping.rescomsplit!="self") %>% ms_unique()
    scenario.data.fe.pc.tobesplit.dirtyres <- scenario.data.fe.pc.splitresidential %>%
      ms_add() %>%
      filter(`model-scenario` %in% dirtyres.scenarios.tobesplit) %>%
      select(-`model-scenario`)

    ##### Create scenario-country combinations (no model necessary, because we only have IMAGE, but this will likely need changing if we make this more elaborate)
    # create all combinations of scenarios and countries
    dirtyres.split.data.iso.prep <- scenario.data.fe.pc.tobesplit.dirtyres %>%
      left_join(mapping.data.dirtyres.split,
                by = c("model", "scenario")) %>%
      left_join(dirtyres.split.data.image %>% select(-model.for.splitting),
                by = c(#"mapping.rescomsplit.model"="model.for.splitting", # noting this down as an idea for later
                  #"mapping.rescomsplit.scenario"="scenario.for.splitting", # noting this down as an idea for later
                  "mapping.rescomsplit"="scenario.for.splitting",
                  "iso", "year")) %>%
      select(-mapping.rescomsplit)

    # Fill NAs where not reported
    dirtyres.split.data.iso.prep.filledNA <- dirtyres.split.data.iso.prep %>%
      left_join(regions.message,
                by = c("iso")) %>%
      # if NA, for share adjustement rates, fill in regional average
      # (MESSAGE region-based; where there's [almost] no NAs in the country-mapping)
      left_join(
        dirtyres.split.data.iso.prep %>%
          left_join(regions.message,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.tradbio.ratio = mean(res.tradbio.ratio, na.rm = T),
            .by = c("region.message",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.message)),
        by = c("region.message",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.tradbio.ratio),
                  res.tradbio.ratio = regional.mean.res.tradbio.ratio) %>%
      # remove helper-columns
      select(-region.message,
             -regional.mean.res.tradbio.ratio) %>%
      # do the same using IMAGE regions (where there's NAs in the country-mapping) to just cover more if needed - at the moment this doesn't change a thing
      left_join(regions.image,
                by = c("iso")) %>%
      left_join(
        dirtyres.split.data.iso.prep %>%
          left_join(regions.image,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.tradbio.ratio = mean(res.tradbio.ratio, na.rm = T),
            .by = c("region.image",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.image)),
        by = c("region.image",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.tradbio.ratio),
                  res.tradbio.ratio = regional.mean.res.tradbio.ratio) %>%
      select(-region.image,
             -regional.mean.res.tradbio.ratio) %>%
      # do the same using REMIND regions (at the moment this adds Curacao (CUW) and Sint-Maarten (SXM))
      left_join(regions.remind,
                by = c("iso")) %>%
      left_join(
        dirtyres.split.data.iso.prep %>%
          left_join(regions.remind,
                    by = c("iso")) %>%
          reframe(
            regional.mean.res.tradbio.ratio = mean(res.tradbio.ratio, na.rm = T),
            .by = c("region.remind",
                    "model","scenario","variable","unit","year")
          ) %>% filter(!is.na(region.remind)),
        by = c("region.remind",
               "model","scenario","variable","unit","year")
      ) %>%
      # replace NAs
      mutate_cond(is.na(res.tradbio.ratio),
                  res.tradbio.ratio = regional.mean.res.tradbio.ratio) %>%
      select(-region.remind,
             -regional.mean.res.tradbio.ratio)

    # ... do correction calculation
    dirtyres.split.data.iso <- dirtyres.split.data.iso.prep.filledNA %>%
      mutate_cond(variable=="Final Energy|Residential and Commercial",
                  energy.per.capita =  energy.per.capita - ((energy.per.capita.uncorrected * res.ratio) * res.tradbio.ratio) )


    ##### Part 2: scenarios that DID model residential traditional biomass consumption
    if (
      length(mapping.data.dirtyres.split %>% filter(mapping.rescomsplit=="self") %>% ms_unique()) > 0
    ) {
      ##### Scenarios: which have already modelled the split?
      dirtyres.scenarios.alreadysplit <- mapping.data.dirtyres.split %>% filter(mapping.rescomsplit=="self") %>% ms_unique()
      scenario.data.fe.pc.alreadysplit.dirtyres <- scenario.data.fe.pc.splitresidential %>%
        ms_add() %>%
        filter(`model-scenario` %in% dirtyres.scenarios.alreadysplit) %>%
        select(-`model-scenario`)

      # ... do correction calculation
      # ... ... (note: since we haven't downscaled residential traditional biomass use, we'll also need to use the region-to-country-level mapping here, actually ...; if we ever downscale traditional biomass, the two blocks below can be rewritten following the residencial share split code just above)

      dirtyres.original.data.iso.prep <- scenario.data.fe.pc.alreadysplit.dirtyres %>%
        left_join(mapping.data.dirtyres.split %>% mutate_cond(
          mapping.rescomsplit=="self",
          mapping.rescomsplit=scenario
        ),
        by = c("model", "scenario")
        ) %>%
        # use image-derived data (there's e.g. no MESSAGE data at the moment!), mapped onto country-level
        left_join(dirtyres.split.data.image %>% select(-model.for.splitting),
                  by = c(#"mapping.rescomsplit.model"="model.for.splitting", # noting this down as an idea for later
                    #"mapping.rescomsplit.scenario"="scenario.for.splitting", # noting this down as an idea for later
                    "mapping.rescomsplit"="scenario.for.splitting",
                    "iso", "year")) %>%
        select(-mapping.rescomsplit)


      # ... do correction calculation
      dirtyres.original.data.iso <- dirtyres.original.data.iso.prep %>%
        mutate_cond(variable=="Final Energy|Residential and Commercial",
                    energy.per.capita =  energy.per.capita - ((energy.per.capita.uncorrected * res.ratio) * res.tradbio.ratio) )


      # Combine data from both scenario sets
      scenario.data.fe.pc.splitresidential.clean <- dirtyres.split.data.iso %>%
        bind_rows(dirtyres.original.data.iso)
    } else {
      scenario.data.fe.pc.splitresidential.clean <- dirtyres.split.data.iso
    }

  }


  # Adjust Transport -----------------------------------------------------------

  if (!exists("FIRST.TIME.RUN.PREPARE.REGIONAL.DATA")) {
    FIRST.TIME.RUN.PREPARE.REGIONAL.DATA <- TRUE
  }
  if (FIRST.TIME.RUN.PREPARE.REGIONAL.DATA){
    log_info(paste0("DLE calculator: prepare aviation energy correction data."))
    source(here("R", "calculator_activity-fe-process-IMAGE-REMIND-transport-aviation-data.R"))
  }


  # split transport into passenger and freight
  # Final Energy|Transportation|Domestic Aviation
  # Energy Service|Transportation|Passenger|Domestic Aviation
  if(
    IAM.OUTPUT.SHORT.FILENAME == "SHAPE-final" & grepl(IAM.SCENARIO.DATA.FILE, pattern="SHAPE-final_v3", fixed = T )
  ){

    regional.transport.adjustment.data <- readRDS(
      file = here("data-raw", "scenario_data", "transport_split",
                  "allSHAPE_Aviation_Transportation_v3.RData")
      ) %>%
      iamc_wide_to_long(upper.to.lower = T)
    regional.transport.adjustment.ratios <- regional.transport.adjustment.data %>%
      select(-unit) %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      mutate(
        aviation.share.energy = `Final Energy|Transportation|Domestic Aviation`/`Final Energy|Transportation`,
        # aviation.share.service = `Energy Service|Transportation|Passenger|Domestic Aviation`/`Energy Service|Transportation|Passenger`
      ) %>% select(model,scenario,region,year,
                   aviation.share.energy
                   # aviation.share.service
                   )

    # to iso
    load_iam_region_mappings()
    iso.transport.adjustment.ratios.remind <- regions.remind %>%
      rename(region = region.remind) %>%
      left_join(regional.transport.adjustment.ratios %>% filter(
        grepl(model,pattern="REMIND",fixed=T)
      ), relationship = "many-to-many",
      by = "region") %>%
      select(-region)
    iso.transport.adjustment.ratios.image <- regions.image %>%
      rename(region = region.image) %>%
      left_join(regional.transport.adjustment.ratios %>% filter(
        grepl(model,pattern="IMAGE",fixed=T)
      ), relationship = "many-to-many",
      by = "region") %>%
      select(-region)

    iso.transport.adjustment.ratios <- iso.transport.adjustment.ratios.remind %>%
      bind_rows(iso.transport.adjustment.ratios.image) %>%
      drop_na(model) # some countries are not assigned to a region, like Antigua (ATA) and Guam (GUM), and Greenland (GRL)

    # perform correction
    scenario.data.fe.pc.splitresidential.clean.aviation.corrected <- scenario.data.fe.pc.splitresidential.clean %>%
      left_join(iso.transport.adjustment.ratios,
                by = c("model","scenario","iso","year")) %>%
      mutate_cond(variable=="Final Energy|Transportation",
                  energy.per.capita = energy.per.capita.uncorrected * (1-aviation.share.energy))
  } else if (
    IAM.OUTPUT.SHORT.FILENAME == "JUSTMIP"
  ){
    # VERY AD-HOC CODE BLOCK [starting here]
    regional.transport.adjustment.data <- readRDS(
      file = here("data-raw", "scenario_data", "transport_split",
                  "allSHAPE_Aviation_Transportation_v3.RData")
    ) %>%
      iamc_wide_to_long(upper.to.lower = T) %>%
      filter(
        model=="IMAGE 3.3",
        scenario=="SSP2-NPi"
      )

    regional.transport.adjustment.ratios <- regional.transport.adjustment.data %>%
      select(-unit) %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      mutate(
        aviation.share.energy = `Final Energy|Transportation|Domestic Aviation`/`Final Energy|Transportation`,
        # aviation.share.service = `Energy Service|Transportation|Passenger|Domestic Aviation`/`Energy Service|Transportation|Passenger`
      ) %>% select(model,scenario,region,year,
                   aviation.share.energy
                   # aviation.share.service
      )

    # to iso
    load_iam_region_mappings()
    iso.transport.adjustment.ratios.image <- regions.image %>%
      rename(region = region.image) %>%
      left_join(regional.transport.adjustment.ratios %>% filter(
        grepl(model,pattern="IMAGE",fixed=T)
      ), relationship = "many-to-many",
      by = "region") %>%
      select(-region)

    original.regionallevel.iam.data.modscen <- read_csv(
      here(
        get_data_location_raw(test=FALSE),
        "scenario_data",
        "original_regional",
        ORIGINAL.IAM.DATA.FILE
      )
    ) %>% upper_to_lower() %>%
      distinct(model,scenario)

    iso.transport.adjustment.ratios <- original.regionallevel.iam.data.modscen %>%
      crossing(
        iso.transport.adjustment.ratios.image %>%
          drop_na(model, scenario) %>%  # drop_na neceassary, because some countries are not assigned to a region, like Antigua (ATG), Guam (GUM), Greenland (GRL), and South Sudan (SSD)
          select(-model,-scenario) %>%
          distinct()
      )

    # perform correction
    scenario.data.fe.pc.splitresidential.clean.aviation.corrected <- scenario.data.fe.pc.splitresidential.clean %>%
      left_join(iso.transport.adjustment.ratios,
                by = c("model","scenario","iso","year")) %>%
      mutate_cond(variable=="Final Energy|Transportation",
                  energy.per.capita = energy.per.capita.uncorrected * (1-aviation.share.energy))

    # VERY AD-HOC CODE BLOCK [ending here]

  }


  # fill in NAs (regional averages)
  scenario.data.fe.pc.splitresidential.clean.aviation.corrected.filledNA <- scenario.data.fe.pc.splitresidential.clean.aviation.corrected %>%
    left_join(regions.message,
              by = c("iso")) %>%
    # if NA, for share adjustment rates, fill in regional average
    # (MESSAGE region-based; where there's [almost] no NAs in the country-mapping)
    left_join(
      scenario.data.fe.pc.splitresidential.clean.aviation.corrected %>%
        left_join(regions.message,
                  by = c("iso")) %>%
        reframe(
          regional.mean.res.ratio = mean(res.ratio, na.rm = T),
          regional.mean.res.tradbio.ratio = mean(res.tradbio.ratio, na.rm = T),
          regional.mean.aviation.share.energy = mean(aviation.share.energy, na.rm = T),

          .by = c("region.message",
                  "model","scenario","variable","unit","year")
        ) %>% filter(!is.na(region.message)),
      by = c("region.message",
             "model","scenario","variable","unit","year")
    ) %>%
    # replace NAs in auxiliary columns
    mutate_cond(is.na(res.ratio),
                res.ratio = regional.mean.res.ratio) %>%
    mutate_cond(is.na(res.tradbio.ratio),
                res.tradbio.ratio = regional.mean.res.tradbio.ratio) %>%
    mutate_cond(is.na(aviation.share.energy),
                aviation.share.energy = regional.mean.aviation.share.energy) %>%
    # remove helper-columns
    select(-region.message,
           -regional.mean.res.ratio, -regional.mean.res.tradbio.ratio, -regional.mean.aviation.share.energy) %>%
    # add Curacao (CUW) and Sint-Maarten (SXM) by doing the same using IMAGE regions
    left_join(regions.image,
              by = c("iso")) %>%
    left_join(
      scenario.data.fe.pc.splitresidential.clean.aviation.corrected %>%
        left_join(regions.image,
                  by = c("iso")) %>%
        reframe(
          regional.mean.res.ratio = mean(res.ratio, na.rm = T),
          regional.mean.res.tradbio.ratio = mean(res.tradbio.ratio, na.rm = T),
          regional.mean.aviation.share.energy = mean(aviation.share.energy, na.rm = T),
          .by = c("region.image",
                  "model","scenario","variable","unit","year")
        ) %>% filter(!is.na(region.image)),
      by = c("region.image",
             "model","scenario","variable","unit","year")
    ) %>%
    # replace NAs in auxiliary columns
    mutate_cond(is.na(res.ratio),
                res.ratio = regional.mean.res.ratio) %>%
    mutate_cond(is.na(res.tradbio.ratio),
                res.tradbio.ratio = regional.mean.res.tradbio.ratio) %>%
    mutate_cond(is.na(aviation.share.energy),
                aviation.share.energy = regional.mean.aviation.share.energy) %>%
    select(-region.image,
           -regional.mean.res.ratio, -regional.mean.res.tradbio.ratio, -regional.mean.aviation.share.energy) %>%

    # now calculate energy.per.capita where this was not done yet
    mutate_cond(
      is.na(energy.per.capita) & variable=="Final Energy|Transportation",
      energy.per.capita = energy.per.capita.uncorrected * (1-aviation.share.energy)
    ) %>%
    mutate_cond(
      is.na(energy.per.capita) & variable=="Final Energy|Residential and Commercial",
      energy.per.capita = energy.per.capita.uncorrected * res.ratio * (1-res.tradbio.ratio)
    )


  # Adjust Total (corrected) ---------------------------------------------------
  scenario.data.fe.pc.splitresidential.clean.aviation.corrected.filledNA <-
    scenario.data.fe.pc.splitresidential.clean.aviation.corrected.filledNA %>%
    group_by(model,scenario,iso,unit,year) %>% # Grouping by relevant columns
    mutate(
      energy_per_capita_sum = sum(energy.per.capita[variable != "Final Energy"], na.rm = TRUE)  # Calculate sum
    ) %>%
    mutate(
      energy.per.capita = if_else(variable == "Final Energy", energy_per_capita_sum, energy.per.capita)  # Replace values
    ) %>%
    ungroup() %>%  # ungroup after operation
    select(-energy_per_capita_sum)  # remove temporary column



  saveRDS(
    scenario.data.fe.pc.splitresidential.clean.aviation.corrected.filledNA,
    file = here(DATA.LOCATION, paste0("scenario_FE", "_", IAM.OUTPUT.SHORT.FILENAME, ".RData"))
  )

}
