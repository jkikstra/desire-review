# Script for mapping DLE data to IAM sectors

#' The logic here is that the production and operation of energy for decent living
#' needs to be mapped from their detailed split onto the less detailed IAM split
#' which has consumption for Transport, ResCom, and Industry.

log_info("Decent Living minimum energy requirement: data mapping")

# load neat dle timeseries data back into memory ====
dle.neat <- readRDS(here(DATA.LOCATION, "dle-threshold-detail.RData")) %>%
  # filter elec total to avoid double counting
  filter(elec != "total") %>%
  # filter urban / rural to avoid double counting
  filter(!grepl(x = variable, pattern = "urban", fixed = T)) %>%
  filter(!grepl(x = variable, pattern = "rural", fixed = T))


# Select DLE-to-IAM mapping version --------------------------------------------
MAPPING.VERSION <- "MRIO-based" # c("SIMPLE", "MRIO-based")


if (MAPPING.VERSION == "MRIO-based"){
  # Mapping of DLE (MRIO-based) --------------------------------------------------
  #' Information about MRIO (EXIO) mapping files:
  #' dle_dim = c("food", "clothing", "health", "education")
  #' sector = c("Ind", "Trp", "Com") and "Tot"
  #' regions (source and destination); message R12 regions

  f.message.mapping <- here("data-raw", DLE.MESSAGE.MAPPING.ASSUMPTIONS)
  mapping.message.categories <- vroom(f.message.mapping, show_col_types=FALSE)
  mapping.message.categories.neat <- mapping.message.categories %>%
    # select(-method) %>%
    pivot_longer(
      cols = rescom:transport,
      names_to = "Mdemand",
      values_to = "mapping.coefficient"
    ) %>%
    pivot_wider(
      names_from = energy.type,
      values_from = mapping.coefficient
    )
  mrio.dle.sectors <- mapping.message.categories.neat %>%
    filter(grepl(x = method, pattern = "MRIO", fixed = T)) %>%
    pull(variable) %>%
    unique()

  # MESSAGE sector names of IAMC variables
  f.message.sector.mapping <- here("data-raw", DLE.SECTORAL.MAPPING)
  message.sector.mapping <- vroom(f.message.sector.mapping, show_col_types=FALSE)

  ### Mapping: LCA sectors -----------------------------------------------------
  # These are the bottom-up, not MRIO-based, dimensions, which are manually mapped.

  dle.mapping.lca.manual <- dle.neat %>%
    # since naming convention changed, need to modify mapping
    full_join(mapping.message.categories.neat, by = c("elec","variable"), relationship = "many-to-many") %>% # multiply by three could be a good unit test here; a flow for each Mdemand
    left_join(message.sector.mapping, by = c("Mdemand")) %>%
    filter(!grepl(x = method, pattern = "MRIO", fixed = T),
           !(is.na(method))) # not MRIO here

  # multiply dle with mapping coefficient
  dle.scen.lcamethods <- dle.mapping.lca.manual %>%
    mutate(
      dle.sector.split = ifelse(
        !is.na(op),
        thres.energy.op * op,
        ifelse(
          !is.na(con),
          thres.energy.conrep * con,
          NA
        )
      )
    ) %>%
    summarise(
      dle.scen.lca = sum(dle.sector.split, na.rm = TRUE),
      .by = c("iso", "sector")
    ) %>%
    rename(variable = sector) # good test; group_by(iso) %>% count() %>% filter(n!=3) == 0 -- all counts should be three



  ### Mapping: MRIO. -----------------------------------------------------------
  # Derived final energy flows from EXIOBASE factors

  # notes on data:
  # - total.share is the share of that sector within each dle_dim
  # src_region: consumption-side? --> check
  # des_region: production-side? --> check


  f.mrio.exio.r12.nonelec <- here(get_data_location_raw(test=FALSE), "DLE_EXIOmapping_non_elec_share_kikstra-Jan2024.csv")
  f.mrio.exio.r12.elec <- here(get_data_location_raw(test=FALSE), "DLE_EXIOmapping_elec_share_kikstra-Jan2024.csv")

  m.r12.mapping <- vroom(here(get_data_location_raw(test=FALSE), "iam_regions", MESSAGE.REGION.MAPPING), show_col_types=FALSE) %>%
    select(iso, region.message.r12)

  ### MRIO: Non-electric data --------------------------------------------------

  mrio.exio.r12.nonelec <- vroom(f.mrio.exio.r12.nonelec, show_col_types=FALSE) %>%
    select(-total.share) %>%
    pivot_longer(cols = LAM:WEU, names_to = "des_reg", values_to = "trade.share") %>%
    filter(
      sector != "Tot"
    ) %>%
    rename(variable = sector) %>%
    mutate_cond(variable == "Ind", variable = "Final Energy|Industry") %>%
    mutate_cond(variable == "Com", variable = "Final Energy|Residential and Commercial") %>%
    mutate_cond(variable == "Trp", variable = "Final Energy|Transportation")


  ### MRIO: Electric data ------------------------------------------------------
  # In EXIOBASE, this is only in transportation (not in industry or commercial).
  mrio.exio.r12.elec <- vroom(f.mrio.exio.r12.elec, show_col_types=FALSE) %>%
    select(-total.share) %>%
    pivot_longer(cols = LAM:WEU, names_to = "des_reg", values_to = "trade.share") %>%
    filter(
      sector != "Tot"
    ) %>%
    rename(variable = sector) %>%
    mutate_cond(variable == "Ind", variable = "Final Energy|Industry") %>%
    mutate_cond(variable == "Com", variable = "Final Energy|Residential and Commercial") %>%
    mutate_cond(variable == "Trp", variable = "Final Energy|Transportation")

  mrio.data.r12 <- bind_rows(
    mrio.exio.r12.elec %>% rename(region.message.r12 = src_reg) %>% mutate(elec = "elec"),
    mrio.exio.r12.nonelec %>% rename(region.message.r12 = src_reg) %>% mutate(elec = "non.elec")
  )


  ### MRIO: domestic allocation --------------------------------------------------
  EXIO.TRADE.SPLIT <- "domestic"
  if (EXIO.TRADE.SPLIT == "domestic") {

    # note: as this includes embodied energy, most energy should go to industry.
    # What may be a bit confusing, is that this is qualified as 'op' in DLE (for exio sectors only).

    # for domestic, basically just go back to total shares in the message region (src_reg) sectoral totals
    mrio.sector.mapping.domestic.r12 <- mrio.data.r12 %>%
      reframe(sector.mapping = sum(trade.share),
              .by = c("dle_dim", "elec", "region.message.r12", "variable")) %>%
      rename(iam.variable = variable)
    # note: here, in `mrio.sector.mapping.domestic.r12`, the sums should add up to 1.
    # i.e. all `mappingsum` in `mrio.sector.mapping.domestic.r12 %>% reframe(mappingsum = sum(sector.mapping), .by = c("dle_dim","elec","region.message.r12"))` should be 1

    # assign DLE from dle_dim to IAMC variable
    dle.scen.mriomethods <- dle.neat %>%
      select(iso,variable,elec,thres.energy,unit.energy) %>%  # all energy (operational + construction replacement)
      filter(variable %in% mrio.dle.sectors) %>% # only keep sectors that are in mrio
      # add mapping of DLE dimensions to the MRIO names of the same dimensions
      mutate(dle_dim = NA) %>%
      mutate_cond(variable %in% c("Nutrition"), dle_dim = "food") %>%
      mutate_cond(variable %in% c("Clothing|clothing",
                                  "Clothing|footwear"), dle_dim = "clothing") %>%
      mutate_cond(variable %in% c("Health care"), dle_dim = "health") %>%
      mutate_cond(variable %in% c("Education|lower_secondary",
                                  "Education|primary"), dle_dim = "education") %>%
      left_join(m.r12.mapping,
                by = c("iso")) %>% # add r12
      left_join(mrio.sector.mapping.domestic.r12, # add sector_mapping, should multiply it by 3 times [3 for non-elec, 3 for elec]
                by = c("elec", "dle_dim", "region.message.r12"),
                relationship = "many-to-many") %>%
      reframe(
        dle.scen.mrio = sum(thres.energy * sector.mapping),
        .by = c("iso", "iam.variable")
      ) %>%
      rename(variable=iam.variable)

  } else if (EXIO.TRADE.SPLIT == "trade-adjusted") {
    # # MRIO. Trade-adjusted.
    # to recover old outdated code; go to commit 18cc4b9 (when the SHAPE cleaning was happening)
  }




  ### LCA + MRIO ---------------------------------------------------------------
  if (nrow(dle.scen.mriomethods %>% drop_na())==nrow(dle.scen.mriomethods %>% drop_na())){
    dle.scen.sector.split <- dle.scen.lcamethods %>%
      left_join(dle.scen.mriomethods,
                by = c("iso", "variable")) %>%
      mutate(
        dle = dle.scen.lca + dle.scen.mrio
      ) %>%
      select(-dle.scen.lca, -dle.scen.mrio)
  } else {
    # throw an error:
    stop(
      "The DLE-to-IAM mapping has gone haywire. Please check in `R/calculator_needs-dle-mapping.R`."
    )
  }

  ### Check: no loss of energy -------------------------------------------------
  if (EXIO.TRADE.SPLIT == "domestic") {
    # for domestic allocation, each country's per capita value should not have changed
    split.error <- dle.scen.sector.split %>%
      reframe(dle = (sum(dle, na.rm = TRUE)),
              .by = "iso") %>%
      left_join(
        dle.neat %>% reframe(thres.energy = (sum(thres.energy, na.rm = TRUE)),
                             .by = "iso"),
        by = "iso"
      ) %>%
      mutate(diff.dle = thres.energy - dle) %>%
      filter(!is.na(diff.dle)) %>%
      pull(diff.dle) %>%
      unique() %>%
      abs() %>%
      max()

    if (split.error > 1e-10) {
      # throw an error:
      stop(
        "Energy is not conserved. In the allocation of domestic EXIO splits, some energy is somehow added or lost when handling the DLE energy flows."
      )
    }
  }



  ### Add back in Final Energy (total) -----------------------------------------
  dle.all <- dle.scen.sector.split %>%
    # add total final energy
    bind_rows(
      dle.scen.sector.split %>%
        summarise(dle = sum(dle),
                  .by = "iso") %>%
        mutate(variable = "Final Energy")
    ) %>%
    arrange(iso, variable, dle)

  # # temporary, to compare (useful for in paper Supplementary information):
  # write_delim(dle.all,
  #             file = here(DATA.LOCATION, "dle-total-and-sectoral_mrio.csv"),
  #             delim = ","
  # )

} else if (MAPPING.VERSION=="SIMPLE"){
  # Mapping of DLE (SIMPLE) ------------------------------------------------------
  #' Manual allocations. Serves more as a placeholder than anything else.


  dle.to.iam.mapping <- read_csv(here(get_data_location_raw(test = TRUE),
                                      "dle_threshold",
                                      "DLE_to_IAM_simplified_withguessedsimpleeemriofactors.csv"), show_col_types = FALSE)

  dle.threshold.detail.with.mapping <- dle.neat %>%
    select(iso,variable,elec,thres.energy.conrep,unit.energy) %>% rename(thres.energy=thres.energy.conrep) %>%
    left_join(dle.to.iam.mapping %>% filter(energy.type=="con")) %>%
    bind_rows(
      dle.neat %>%
        select(iso,variable,elec,thres.energy.op,unit.energy) %>% rename(thres.energy=thres.energy.op) %>%
        left_join(dle.to.iam.mapping %>% filter(energy.type=="op"))
    ) %>% drop_na()

  dle.threshold.mapped <- dle.threshold.detail.with.mapping %>%
    mutate(
      rescom = thres.energy * rescom,
      industry = thres.energy * industry,
      transport = thres.energy * transport
    ) %>%
    reframe(
      thres.energy.rescom = sum(rescom),
      thres.energy.industry = sum(industry),
      thres.energy.transport = sum(transport),
      .by = c("iso")
    )

  dle.all.simple <- dle.threshold.mapped %>%
    rename(
      `Final Energy|Residential and Commercial`=thres.energy.rescom,
      `Final Energy|Industry`=thres.energy.industry,
      `Final Energy|Transportation`=thres.energy.transport
    ) %>%
    pivot_longer(cols = -c(iso), names_to = "variable", values_to = "dle")

  dle.all <- dle.all.simple

  # # temporary, to compare (useful for in paper Supplementary information):
  # write_delim(dle.all.simple,
  #             file = here(DATA.LOCATION, "dle-total-and-sectoral_simple.csv"),
  #             delim = ","
  # )
}



# write out data ====
write_delim(dle.all,
            file = here(DATA.LOCATION, "dle-total-and-sectoral.csv"),
            delim = ","
)
saveRDS(dle.all,
        file = here(DATA.LOCATION, "dle-total-and-sectoral.RData")
)

print("Finished mapping bottom-up DLE per capita (minimum energy requirement) data to each scenario")


# for SHAPE figure B1
SAVE.OUT.DLE.TO.IAM.DLE.THRESHOLD.MAPPING.DATA <- FALSE
if (SAVE.OUT.DLE.TO.IAM.DLE.THRESHOLD.MAPPING.DATA) {
  threshold.data.mapping.dle.to.iam <-
    # LCA data:
    dle.mapping.lca.manual %>%
    mutate(
      dle.sector.split = ifelse(
        !is.na(op),
        thres.energy.op * op,
        ifelse(
          !is.na(con),
          thres.energy.conrep * con,
          NA
        )
      )
    ) %>%
    summarise(
      dle.scen.lca = sum(dle.sector.split, na.rm = TRUE),
      .by = c("iso", "sector", "variable") # line changed to add "variable", compared to above
    ) %>%
    rename(`DLS dimension`=variable,
           `mj_percap`=dle.scen.lca) %>%

    bind_rows(
      # MRIO data:
      dle.neat %>%
        select(iso,variable,elec,thres.energy,unit.energy) %>%  # all energy (operational + construction replacement)
        filter(variable %in% mrio.dle.sectors) %>% # only keep sectors that are in mrio
        # add mapping of DLE dimensions to the MRIO names of the same dimensions
        mutate(dle_dim = NA) %>%
        mutate_cond(variable %in% c("Nutrition"), dle_dim = "food") %>%
        mutate_cond(variable %in% c("Clothing|clothing",
                                    "Clothing|footwear"), dle_dim = "clothing") %>%
        mutate_cond(variable %in% c("Health care"), dle_dim = "health") %>%
        mutate_cond(variable %in% c("Education|lower_secondary",
                                    "Education|primary"), dle_dim = "education") %>%
        left_join(m.r12.mapping,
                  by = c("iso")) %>% # add r12
        left_join(mrio.sector.mapping.domestic.r12, # add sector_mapping, should multiply it by 3 times [3 for non-elec, 3 for elec]
                  by = c("elec", "dle_dim", "region.message.r12"),
                  relationship = "many-to-many") %>%
        reframe(
          dle.scen.mrio = sum(thres.energy * sector.mapping),
          .by = c("iso", "iam.variable", "variable") # line changed to add "variable", compared to above
        ) %>%
        rename(`DLS dimension`=variable,
               sector=iam.variable,
               `mj_percap`=dle.scen.mrio)
    )

  write_delim(threshold.data.mapping.dle.to.iam,
              file = here(DATA.LOCATION, "dle-to-iam-energythreshold.csv"),
              delim = ","
  )

}


