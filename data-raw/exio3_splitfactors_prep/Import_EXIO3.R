IIASA.USER <- "kikstra" # if running with access to IIASA network

# EXIO3_path_old for satellite format
EXIO3_path_old = paste0("C:/Users/",IIASA.USER,"/IIASA/DLE - Documents/WS2 - Documents/Data/IO/EXIOBASE/EXIOBASE3/IOT_bug_w_India/")

# EXIO3_path for A matrix or other base IO matrices
EXIO3_path = paste0("C:/Users/",IIASA.USER,"/IIASA/DLE - Documents/WS2 - Documents/Data/IO/EXIOBASE/EXIOBASE3/IOT_bug_w_TROA/")

# EXIO3_path_fix for energy satellite
EXIO3_path_fix = paste0("C:/Users/",IIASA.USER,"/IIASA/DLE - Documents/WS2 - Documents/Data/IO/EXIOBASE/EXIOBASE3/EnvExt/EnvExt_NEU_1995-2015_20190114/")


#' Notes for HarmonizeEXIO3ExtensionFormat:
#' - in EXIO, energy extension rows have changed from older to newer 2015 version, which has more rows and other structuring. Here, Jihoon wrote code to match it to the old structure.
#' - NTRA / NENE descriptions etc.: was not publicly available on Data repo (Zenodo)
#'    - NTRA == Non-transport
#'    - NENE == Non-energy
#'    - all other: transport
#'      - TAVI: aviation
#'      - TMAR: marine
#'      - TOTH: other
#'      - TRAI: rail
#'      - TROA: roads
#'    - LOSS: conversion losses (specific to transport) -- if added to the sum of the rest, this gives primary energy total (as the rest is in final energy)
HarmonizeEXIO3ExtensionFormat <- function(raw.ext) {

  #' Glossary:
  #' fei = final energy intensity
  #' nei = net energy intensity
  #' raw = raw (new format - before changing to the format Jihoon worked with)
  #' .sub = has all subcomponents [like NTRA, NENE, TAVI, etc.] still (separately)

  fei.NTRA.raw <- data.frame(name=label.S$name[idx.FE.NTRA], mat=raw.ext[idx.FE.NTRA, ]) %>%
    mutate(name=gsub("Energy Carrier Net NTRA ", "", name))



  fei.TAVI.raw <- data.frame(name=label.S$name[idx.FE.TAVI], mat=raw.ext[idx.FE.TAVI, ]) %>%
    mutate(name=gsub("Energy Carrier Net TAVI ", "", name))
  fei.TMAR.raw <- data.frame(name=label.S$name[idx.FE.TMAR], mat=raw.ext[idx.FE.TMAR, ]) %>%
    mutate(name=gsub("Energy Carrier Net TMAR ", "", name))
  fei.TOTH.raw <- data.frame(name=label.S$name[idx.FE.TOTH], mat=raw.ext[idx.FE.TOTH, ]) %>%
    mutate(name=gsub("Energy Carrier Net TOTH ", "", name))
  fei.TRAI.raw <- data.frame(name=label.S$name[idx.FE.TRAI], mat=raw.ext[idx.FE.TRAI, ]) %>%
    mutate(name=gsub("Energy Carrier Net TRAI ", "", name))
  fei.TROA.raw <- data.frame(name=label.S$name[idx.FE.TROA], mat=raw.ext[idx.FE.TROA, ]) %>%
    mutate(name=gsub("Energy Carrier Net TROA ", "", name))
  fei.LOSS.raw <- data.frame(name=label.S$name[idx.FE.LOSS], mat=raw.ext[idx.FE.LOSS, ]) %>% # not relevant to DLE
    mutate(name=gsub("Energy Carrier Net LOSS ", "", name))

  # fei.NENE <- data.frame(name=carriers.Arkz) %>% full_join(fei.NENE.raw) %>% slice(1:69) %>% select(-name)

  fei.NTRA <- data.frame(name=carriers.Arkz) %>% full_join(fei.NTRA.raw) %>% slice(1:69) %>% select(-name)

  fei.TAVI <- data.frame(name=carriers.Arkz) %>% full_join(fei.TAVI.raw) %>% slice(1:69) %>% select(-name)
  fei.TMAR <- data.frame(name=carriers.Arkz) %>% full_join(fei.TMAR.raw) %>% slice(1:69) %>% select(-name)
  fei.TOTH <- data.frame(name=carriers.Arkz) %>% full_join(fei.TOTH.raw) %>% slice(1:69) %>% select(-name)
  fei.TRAI <- data.frame(name=carriers.Arkz) %>% full_join(fei.TRAI.raw) %>% slice(1:69) %>% select(-name)
  fei.TROA <- data.frame(name=carriers.Arkz) %>% full_join(fei.TROA.raw) %>% slice(1:69) %>% select(-name)
  fei.LOSS <- data.frame(name=carriers.Arkz) %>% full_join(fei.LOSS.raw) %>% slice(1:69) %>% select(-name)

  # fei.NENE[is.na(fei.NENE)] <- 0
  fei.NTRA[is.na(fei.NTRA)] <- 0
  fei.TAVI[is.na(fei.TAVI)] <- 0
  fei.TMAR[is.na(fei.TMAR)] <- 0
  fei.TOTH[is.na(fei.TOTH)] <- 0
  fei.TRAI[is.na(fei.TRAI)] <- 0
  fei.TROA[is.na(fei.TROA)] <- 0
  fei.LOSS[is.na(fei.LOSS)] <- 0

  # totals
  fei.exio <- as.matrix(
                          fei.NTRA + fei.TAVI + fei.TMAR + fei.TOTH + fei.TRAI + fei.TROA           ) # w/o fei.NENE: NENE is about manufacturing process. Not about energy use & wellbeing
  nei.exio <- as.matrix(#fei.NENE +
                          fei.NTRA + fei.TAVI + fei.TMAR + fei.TOTH + fei.TRAI + fei.TROA + fei.LOSS)

  # only electricity
  fei.elec <- fei.exio
  fei.elec[-idx.Elec.carrier,] <- 0

  # only non-electricity
  fei.non.elec <- fei.exio
  fei.non.elec[idx.Elec.carrier,] <- 0

  # all subcomponents
  fei.sub <- list(#fei.NENE,
                  fei.NTRA, fei.TAVI, fei.TMAR, fei.TOTH, fei.TRAI, fei.TROA, fei.LOSS)
  names(fei.sub) <- c(#"NENE",
                      "NTRA", "TAVI", "TMAR", "TOTH", "TRAI", "TROA", "LOSS")

  # all files as outputs.
  output <- list(fei.exio, fei.elec, fei.non.elec, fei.sub,
                 #	pei.nature, pei.USE, ei.SUPL,
                 nei.exio)

  return(output)
}



vassign <- function(..., values, envir=parent.frame()) {
  vars <- as.character(substitute(...()))
  values <- rep(values, length.out=length(vars))
  for(i in seq_along(vars)) {
    assign(vars[[i]], values[[i]], envir)
  }
}



library(readxl)
wb <- paste0("C:/Users/", IIASA.USER,"/IIASA/DLE - Documents/WS2 - Documents/Analysis/IO/Bridging/CES-COICOP/ICP_EXIO_Qual_UN_Edited.xlsx")
EX_catnames = names(read_xlsx(wb))[-1] # 200 names of commodities
exio_ctys <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI",
               "FR", "GR",
               "HR", # Added new in EXIO3 (was not in EXIO2).
               "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL",
               "PL", "PT", "RO", "SE", "SI", "SK", "GB", "US", "JP", "CN",
               "CA", "KR", "BR", "IN", "MX", "RU", "AU", "CH", "TR", "TW",
               "NO", "ID", "ZA", "WA", "WL", "WE", "WF", "WM")
num.cty <- length(exio_ctys)
exio.len <- length(exio_ctys)*200 #(200 industrial sectors // commodities) -- {see EXIO3_sect_reg$EXIO_sector a few lines down the code}
exio.fd.len <- length(exio_ctys)*7 #(7 final demand sectors) -- {check on zenodo or on SharePoint where data is loaded for metadata}



#' Earlier parsing of a mat database file happens in MATLAB script, see:
#' "C:\Users\kikstra\IIASA\DLE - Documents\WS2 - Documents\Data\IO\EXIOBASE\EXIOBASE3\EnvExt\convert_mat2csv.m"

raw.S <- read.csv(paste0(EXIO3_path, "S_", IO.year, ".csv"), header = FALSE)    # Stressor (Intensity)
# read in multiple S
raw.st <- read.csv(paste0(EXIO3_path, "st_", IO.year, ".csv"), header = FALSE)  # Total stressor
# read in multiple st
raw.F <- read.csv(paste0(EXIO3_path, "F_", IO.year, ".csv"), header = FALSE)

raw.F_hh <- read.csv(paste0(EXIO3_path, "F_hh_", IO.year, ".csv"), header = FALSE)

label.S <- read_xls(paste0(EXIO3_path, "labs_S_2011.xls"), col_names = FALSE)[,1:2] %>% rename(name = ...1, unit = ...2) # slice(1413:1707)

# Separator for Industrial vs. Commercial (building) for energy accounting
vec.IndComm <- read.csv(here("data-raw", "exio3_splitfactors_prep", "products_IndComm.csv"), header = TRUE)

# Make them sparse
library(Matrix)
m_Ind = as(diag(rep(vec.IndComm$Industry, num.cty)), "sparseMatrix")
m_Comm = as(diag(rep(1-vec.IndComm$Industry, num.cty)), "sparseMatrix")


# idx.FE <- grep("Energy Carrier Net", label.S$name)
# idx.FE.NENE <- grep("NENE", label.S$name)
idx.FE.NTRA <- grep("NTRA", label.S$name)
idx.FE.TAVI <- grep("TAVI", label.S$name)
idx.FE.TMAR <- grep("TMAR", label.S$name)
idx.FE.TOTH <- grep("TOTH", label.S$name)
idx.FE.TRAI <- grep("TRAI", label.S$name)
idx.FE.TROA <- grep("TROA", label.S$name)
idx.FE.LOSS <- grep("LOSS", label.S$name)
idx.PE.nature <- grep("Nature Inputs", label.S$name)
idx.USE <- grep("Energy Carrier Use", label.S$name)
idx.SUPL <- grep("Energy Carrier Supply ", label.S$name)
idx.FE <- c(#idx.FE.NENE,
            idx.FE.NTRA, idx.FE.TAVI, idx.FE.TMAR, idx.FE.TOTH, idx.FE.TRAI, idx.FE.TROA) # indices final energy
idx.NE <- c(idx.FE, idx.FE.LOSS) # indices "net-energy" (so, adding back in losses)



### TROA bug-fix; to align dimensions where Arkaitz had sent them to Jihoon in different formats. We keep the older format.

label.Arkz <- read_excel(paste0(EXIO3_path_fix, "NEU_products_IIASA.xlsx"), col_names = FALSE, sheet="p_neu") %>% rename(carrier=...1, num=...2) # NEU: Net Energy
carriers.Arkz <- gsub("Energy Carrier Net ", "", label.Arkz$carrier)
n_carriers.Arkz <- length(carriers.Arkz)
carriers.RWood <- c(
  # sapply(gsub("Energy Carrier Net NENE ", "", label.S$name[idx.FE.NENE]), function(x) {which(x==carriers.Arkz)}),
  sapply(gsub("Energy Carrier Net NTRA ", "", label.S$name[idx.FE.NTRA]), function(x) {which(x==carriers.Arkz)}),
  sapply(gsub("Energy Carrier Net TAVI ", "", label.S$name[idx.FE.TAVI]), function(x) {which(x==carriers.Arkz)})+n_carriers.Arkz*1,
  sapply(gsub("Energy Carrier Net TMAR ", "", label.S$name[idx.FE.TMAR]), function(x) {which(x==carriers.Arkz)})+n_carriers.Arkz*2,
  sapply(gsub("Energy Carrier Net TOTH ", "", label.S$name[idx.FE.TOTH]), function(x) {which(x==carriers.Arkz)})+n_carriers.Arkz*3,
  sapply(gsub("Energy Carrier Net TRAI ", "", label.S$name[idx.FE.TRAI]), function(x) {which(x==carriers.Arkz)})+n_carriers.Arkz*4,
  sapply(gsub("Energy Carrier Net TROA ", "", label.S$name[idx.FE.TROA]), function(x) {which(x==carriers.Arkz)})+n_carriers.Arkz*5,
  sapply(gsub("Energy Carrier Net LOSS ", "", label.S$name[idx.FE.LOSS]), function(x) {which(x==carriers.Arkz)})+n_carriers.Arkz*6)


idx.Elec.carrier <- grep("Electricity by ", carriers.Arkz) # Among the 69 carriers

# New fixed version replaces the energy part of the extension.
raw.F[idx.NE,] <- read.csv(paste0(EXIO3_path_fix, "F_pxp_", IO.year, ".csv"), header = FALSE)[carriers.RWood,] # .F:
raw.S[idx.NE,] <- read.csv(paste0(EXIO3_path_fix, "S_pxp_", IO.year, ".csv"), header = FALSE)[carriers.RWood,] # .S:
raw.st[idx.NE,] <- read.csv(paste0(EXIO3_path_fix, "st_pxp_", IO.year, ".csv"), header = FALSE)[carriers.RWood,] # .st: Total stressor




# Arrange intensity outputs (with 69 carriers) - fei: MJ/EUR
#' IO matrix operation outputs (done in MATLAB - Jihoon did basic matrix multiplications to get these):
#' S: energy intensity (direct), between each sector
#' st: energy intensity (direct + indirect), including supply-chain indirect effects
#' F: final energy demand?

# get total final energy intensity (direct + indirect)
vassign(tfei.exio, tfei.elec, tfei.non.elec, tfei.sub, tnei.exio, # tpei.nature, tpei.USE, tpei.SUPL,
        values=HarmonizeEXIO3ExtensionFormat(raw.st))
NTRA_sps = as(as.matrix(tfei.sub$NTRA), "sparseMatrix")
tfei.sub$NTRA_I = NTRA_sps %*% m_Ind #ERROR: Cholmod error 'A and B inner dimensions must match' at file ../MatrixOps/cholmod_ssmult.c, line 80
tfei.sub$NTRA_C = NTRA_sps %*% m_Comm #ERROR: Cholmod error 'A and B inner dimensions must match' at file ../MatrixOps/cholmod_ssmult.c, line 80


# get only direct energy intensity (direct)
vassign(dfei.exio, dfei.elec, dfei.non.elec, dfei.sub, dnei.exio, # dpei.nature, dpei.USE, dpei.SUPL,
        values=HarmonizeEXIO3ExtensionFormat(raw.S))

# direct final energy demand (not intensities) - only by industry sectors
vassign(dfe.exio, dfe.elec, dfe.non.elec, dfe.sub, dne.exio, # dpe.nature, dpe.USE, de.SUPL,
        values=HarmonizeEXIO3ExtensionFormat(raw.F))

# direct final energy demand (not intensities) - only by households
vassign(dfe.exio.hh, dfe.elec.hh, dfe.non.elec.hh, dfe.sub.hh, dne.exio.hh,
        values=HarmonizeEXIO3ExtensionFormat(raw.F_hh))



# Save the results for DLE
EXIO3_FD <- read.csv(paste0(EXIO3_path, "Y_", IO.year, ".csv"), header = FALSE) # FD (Y - matrix): final demand // (X: ind prod)
EXIO3_FD.hh <- as.matrix(EXIO3_FD[,seq(1, dim(EXIO3_FD)[2], 7)])
EXIO3_data <- list(tfei.exio, tfei.elec, tfei.non.elec, tfei.sub, tnei.exio, # save all data
                     dfei.exio, dfei.elec, dfei.non.elec, dfei.sub, dnei.exio,
                     dfe.exio, dfe.elec, dfe.non.elec, dfe.sub, dne.exio,
                     dfe.exio.hh, dfe.elec.hh, dfe.non.elec.hh, dfe.sub.hh, dne.exio.hh,
                     EXIO3_FD.hh)
names(EXIO3_data) <- c("tfei.exio", "tfei.elec", "tfei.non.elec", "tfei.sub", "tnei.exio",
                         "dfei.exio", "dfei.elec", "dfei.non.elec", "dfei.sub", "dnei.exio",
                         "dfe.exio", "dfe.elec", "dfe.non.elec", "dfe.sub", "dne.exio",
                         "dfe.exio.hh", "dfe.elec.hh", "dfe.non.elec.hh", "dfe.sub.hh", "dne.exio.hh",
                         "EXIO3_FD.hh")
EXIO3_sect_reg <- list(EXIO_sector = EX_catnames, EXIO_region = exio_ctys)
saveRDS(EXIO3_data,
     file = here("data", paste0("EXIO3_energy_ext_", as.character(IO.year),"_",IIASA.USER,".Rdata")))
saveRDS(EXIO3_sect_reg,
     file = here("data", paste0("EXIO3_sectors_regions","_",IIASA.USER,".Rdata")))



# Read the final demand values from EXIO3
final_demand <- read.csv(paste0(EXIO3_path, paste0("Y_", IO.year, ".csv")), header = FALSE)
