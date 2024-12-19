# Main figures for SHAPE manuscript

# Sampling size ----------------------------------------------------------------
SAMPLING.SIZE <- 10000

# Start code and load packages -------------------------------------------------
# start code
library(here) # for easy and clear relative paths
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
here::i_am("decent.Rproj")
# init
source(here("R", "utils.R") ) # loads several packages used in DESIRE
# install plotting packages
source(here("renv_install_plotting_dependencies.R"))
install_plot_packages()


# Settings ---------------------------------------------------------------------
# data.version <- "shape_v0_1" # original submission
data.version <- "shape_v1_1_0"

### Data location --------------------------------------------------------------

PATH.folder.manuscript <- "C:/Users/kikstra/IIASA/ECE.prog - Documents/Projects/Decent Living/DESIRE/2023 manuscript shape/revisions_v1" # "PATH_TO_WHERE_YOU_UNPACK_kikstra_erl_data_and_figures_revisions_v1.zip"
PATH.data.for.figures <- file.path(PATH.folder.manuscript, "data")
PATH.data.desire <- file.path(
  PATH.data.for.figures,
  "desire",
  "20241114"
)

PATH.out.for.figures <- file.path(PATH.folder.manuscript, "figures")

### Figure versioning ----------------------------------------------------------
# figures.version <- "v3-submission" # original submission
figures.version <- "v4-revisions_v1"

# Check if the folder exists; if not, create it
PATH.replication.figures <- here("analyses", "figures", data.version, figures.version)
if (!dir.exists(PATH.replication.figures)) {
  dir.create(PATH.replication.figures, recursive = T)
  message("Folder created: ", PATH.replication.figures)
} else {
  message("Folder already exists: ", PATH.replication.figures)
}

### Select main scenarios ------------------------------------------------------
shape.sdps <- c("SDP_EI-1p5C","SDP_MC-1p5C","SDP_RC-1p5C")
shape.core.scenarios <-  c(shape.sdps, "SSP2-1p5C", "SSP2-NPi")

### Plotting parameters ------------------------------------------------------------
DLE.END.YEAR.TIMESERIES <- 2040
shape.color.coding <- c("SDP_EI-1p5C" = "midnightblue",
                        "SDP_MC-1p5C" = "aquamarine4",
                        "SDP_RC-1p5C" = "goldenrod3",
                        "SDP-EI" = "midnightblue",
                        "SDP-MC" = "aquamarine4",
                        "SDP-RC" = "goldenrod3",
                        "SSP2-NPi" = "black",
                        "SSP2-1p5C" = "grey",
                        "SSP2-Ref" = "black",
                        "SSP2-1.5C" = "grey")
vis.model <- "REMIND-MAgPIE 3.2-4.6"
vis.scenario <- "SDP_RC-1p5C"

ipcc.r10.color.coding <- c(
  "Asia-Pacific Developed" = "#f8ed62",#fff9ae
  "North America" =  "#e9d700",
  "Eastern Europe and West-Central Asia" =  "#dab600",
  "Europe" =  "#a98600",

  "Latin America and Caribbean" =  "#a4aeae",
  "South-East Asia and developing Pacific" =  "#b4aeae",
  "Africa" =  "#c4aeae",
  "Eastern Asia" =  "#d4aeae",
  "Southern Asia" =  "#e4aeae",
  "Middle East" =  "#f4aeae"
)

# Loading Data -----------------------------------------------------------------

### Default (core) data --------------------------------------------------------
data.folder.default <- file.path(
  PATH.data.desire, "default"
)

df.default <- read_csv(
  file.path(data.folder.default, "shape_submission_results_default_version_1_1_0_rc2_20241114.csv") # revision v1 version
) %>%
  # remove unnecessary columns
  select(-gini.to.gini.elasticities.by.country.up, -gini.to.gini.elasticities.by.country.down)

# only keep countries available for both models (173/171 countries, writing 26.01.2024, version0_1_rc3_20240126)
countries.both.models <- df.default %>% select(model,iso) %>% distinct() %>% mutate(available=T) %>%
  pivot_wider(names_from = model, values_from = available) %>%
  filter((`IMAGE 3.3`==T & `REMIND-MAgPIE 3.2-4.6`==T)) %>%
  select(iso) %>% pull()
df.default <- df.default %>% filter(iso %in% countries.both.models)

df.core <- df.default %>%
  filter(scenario%in%shape.core.scenarios)

### Emissions data (downscaled) ------------------------------------------------
downscaled.default.folder <- "C:/Users/kikstra/Documents/GitHub/decent/analyses/data/default"
downscaled.default.data <- read_excel(
  file.path(
    PATH.data.for.figures,
    "iam-data", "downscaled",
    "REMIND-MAgPIE 3.2-4.6_SHAPE_2023_2024_02_12_2018_harmo_step5e_Scenario_Explorer_upload_FINAL.xlsx"
  )
) %>%
  bind_rows(
    read_excel(
      file.path(
        PATH.data.for.figures,
        "iam-data", "downscaled",
        "IMAGE 3.3_SHAPE_2023_2024_02_12_2018_harmo_step5e_Scenario_Explorer_upload_FINAL.xlsx"
      )
    )
  ) %>%
  mutate(MODEL = substr(MODEL,
                        start = nchar("Downscaling[")+1,
                        stop = nchar(MODEL)-1))
### IAM Regional Model Data ----------------------------------------------------
FILE.IMAGE.IAM.REGIONAL.DATA <- "1702306278226-Consolidated_IMAGE_SHAPE_results.xlsx"
FILE.REMIND.IAM.REGIONAL.DATA <- "1702460359576-SHAPE_alldata_2023-12-13_Final.xlsx"
FILE.IMAGE.IAM.REGIONAL.DATA.native <- "SHAPE_IMAGE_3.3_key-scenarios_native-regions_v1.xlsx"
FILE.REMIND.IAM.REGIONAL.DATA.native <- "SHAPE_REMIND-MAgPIE_3.2-4.6_key-scenarios_native-regions_v1.xlsx"
FILE.IMAGE.IAM.REGIONAL.DATA.explorer <- "SHAPE_IMAGE_3.3_key-scenarios_v1.xlsx"
FILE.REMIND.IAM.REGIONAL.DATA.explorer <- "SHAPE_REMIND-MAgPIE_3.2-4.6_key-scenarios_v1.xlsx"


##### Final Energy data (regional model data) ------------------------------------
fe.iam.vars <- c("Final Energy")
raw.scenario.data.fe.global <- load_excel_iamc(
  here(PATH.data.for.figures,
       "iam-data", "regional", FILE.REMIND.IAM.REGIONAL.DATA.explorer),
  sheet = "data"
) %>% filter(Region=="World", Variable%in%fe.iam.vars) %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  bind_rows(
    load_excel_iamc(
      here(PATH.data.for.figures,
           "iam-data", "regional", FILE.IMAGE.IAM.REGIONAL.DATA.explorer),
      sheet = "data"
    ) %>% filter(Region=="World", Variable%in%fe.iam.vars) %>%
      iamc_wide_to_long(upper.to.lower = T)
  )

# ##### Primary Energy data (regional model data)
#
# pe.iam.vars <- c("Primary Energy")
# iam.pe.global <- load_excel_iamc(
#   here(PATH.data.for.figures,
#        "iam-data", "regional", FILE.REMIND.IAM.REGIONAL.DATA.explorer),
#   sheet = "data"
# ) %>% filter(Region=="World", Variable%in%pe.iam.vars) %>%
#   iamc_wide_to_long(upper.to.lower = T) %>%
#   bind_rows(
#     load_excel_iamc(
#       here(PATH.data.for.figures,
#            "iam-data", "regional", FILE.IMAGE.IAM.REGIONAL.DATA.explorer),
#       sheet = "data"
#     ) %>% filter(Region=="World", Variable%in%pe.iam.vars) %>%
#       iamc_wide_to_long(upper.to.lower = T)
#   )

##### Temperature data (regional model data) -------------------------------------
temp.vars <- c(
  "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile"
)
iam.temperatures <- load_excel_iamc(
  here(PATH.data.for.figures,
       "iam-data", "regional", FILE.REMIND.IAM.REGIONAL.DATA.explorer),
  sheet = "data"
  ) %>% filter(Region=="World", Variable%in%temp.vars) %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  bind_rows(
    load_excel_iamc(
      here(PATH.data.for.figures,
           "iam-data", "regional", FILE.IMAGE.IAM.REGIONAL.DATA.explorer),
      sheet = "data"
    ) %>% filter(Region=="World", Variable%in%temp.vars) %>%
      iamc_wide_to_long(upper.to.lower = T)
  )

### Regional groups ------------------------------------------------------------
ipcc.r10 <- load_official_country_grouping(grouping.to.load = "region_ar6_10_ipcc_fgd", keep.long.names = T,
                                           path.data.countrymapping = file.path(PATH.data.for.figures, "country_mapping")) %>%
  rename(r10=region_ar6_10_ipcc_fgd)
wb.income.grouping <- load_official_country_grouping(grouping.to.load = "Income _status_WB", keep.long.names = T,
                                                     path.data.countrymapping = file.path(PATH.data.for.figures, "country_mapping")) %>%
  rename(wb.r4=`Income _status_WB`)
wb.income.grouping.3 <- load_official_country_grouping(grouping.to.load = "Income _status_WB", keep.long.names = T,
                                                       path.data.countrymapping = file.path(PATH.data.for.figures, "country_mapping")) %>%
  rename(wb.r4=`Income _status_WB`) %>%
  mutate(wb.r3 = case_when(
    wb.r4 %in% c("Low") ~ "Low",
    wb.r4 %in% c("Lower-middle", "Upper middle") ~ "Middle",
    wb.r4 %in% c("High") ~ "High"
  ))


### Population and GDP SHAPE ---------------------------------------------------

##### National population (SHAPE scenarios) ------------------------------------
# - should only be used for only for checks with global - because DESIRE output also has population
population.iso <- read_csv(
  here(get_data_location_raw(test=FALSE),
       "scenario_data",
       "population",
       "SHAPE_pop_sep2023update_fixssp2CYP.csv"), show_col_types=FALSE
) %>%
  iamc_wide_to_long(upper.to.lower = F) %>%
  rename(iso=region, pop_mil=value)
population.iso.2020 <- population.iso %>% filter(year==2020)

##### R10 population projections (SHAPE scenarios) -----------------------------
pop.r10.projection <- population.iso %>% left_join(ipcc.r10 %>% select(-name)) %>%
  reframe(pop_mil = sum(pop_mil),
          .by = c("model","scenario","r10","variable","unit","year")) %>%
  select(-variable) %>%
  filter(model==vis.model)

pop.r10.projection.global <- population.iso %>% left_join(ipcc.r10 %>% select(-name)) %>%
  reframe(pop_mil = sum(pop_mil),
          .by = c("model","scenario","variable","unit","year")) %>%
  select(-variable) %>%
  filter(model==vis.model)

##### DESIRE population coverage -----------------------------------------------
# check what share of the global population is in the assessed data.
iso.assessed.by.desire <- df.core %>% pull(iso) %>% unique()
# ... 2020 population in SDP
population.iso.2020.base <- population.iso.2020 %>% filter(model == vis.model,
                                                           scenario == vis.scenario) %>%
  select(iso,year,pop_mil)
P.df.iso.2020 <- population.iso.2020 %>%
  filter(model==vis.model,scenario==vis.scenario) %>% select(iso,pop_mil)
Piso.2020 <- P.df.iso.2020 %>% pull(iso)
global.pop.2020 <- P.df.iso.2020 %>% pull(pop_mil) %>% sum()

# ... 2050 population in SDP
population.iso.2050 <- population.iso %>% filter(year==2050)
population.iso.2050.base <- population.iso.2050 %>% filter(model == vis.model,
                                                           scenario == vis.scenario) %>%
  select(iso,year,pop_mil)
P.df.iso.2050 <- population.iso.2050 %>%
  filter(model==vis.model,scenario==vis.scenario) %>% select(iso,pop_mil)
Piso.2050 <- P.df.iso.2050 %>% pull(iso)
global.pop.2050 <- P.df.iso.2050 %>% pull(pop_mil) %>% sum()


# Load Data (GDP) --------------------------------------------------------------
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

# DLS Gaps data --------------------------------------------------------------
f.dls.data <- file.path(
  PATH.data.desire, "other", "dle-gaps",
  "DLS_dataproducts_combined.csv"
)


# General functions for this script --------------------------------------------
### Renaming models and scenarios ----------------------------------------------
rename.models <- function(df){
  return(
    df %>%
      mutate(
        model = case_when(
          model == "IMAGE 3.3" ~ "IMAGE",
          model == "REMIND-MAgPIE 3.2-4.6" ~ "REMIND",
          TRUE ~ model
        ))
      )
}
rename.scenarios <- function(df){
  return(
    df %>%
      mutate(
        scenario = case_when(
          scenario == "SDP_RC-1p5C" ~ "SDP-RC",
          scenario == "SDP_MC-1p5C" ~ "SDP-MC",
          scenario == "SDP_EI-1p5C" ~ "SDP-EI",
          scenario == "SSP2-1p5C" ~ "SSP2-1.5C",
          scenario == "SSP2-NPi" ~ "SSP2-Ref",
          TRUE ~ scenario
        ))
  )
}
rename.models.and.scenarios <- function(df){
  return(
    df %>%
      rename.models() %>%
      rename.scenarios()
  )
}

### Sampling functions ---------------------------------------------------------
sample_energy_distributions_sectoral <- function(desire.data,
                                                 N=100, # 100 data points per distribution
                                                 show.progress.bar=FALSE,
                                                 random.seed=FALSE,
                                                 pb=NULL
){

  df <- desire.data %>% select(model,
                               scenario,
                               iso,
                               variable, unit,
                               year,
                               energy.per.capita,
                               energy.gini,
                               pop_mil # not used but useful to keep
  )

  scenario.data.value.combinations <- distinct(df %>%
                                                 select(-energy.per.capita,-energy.gini))

  df.energy.tranformed <- df %>%
    mutate(
      sig = get_sigma_lognormal(gini=energy.gini)
    ) %>%
    mutate(mu = get_nu_lognormal(gini = energy.gini, mean = energy.per.capita))

  if (!random.seed){
    set.seed(19721004) # for reproducibility; date IIASA was founded
  }

  all.sampled.data <- NULL
  for (rn in seq(1,nrow(scenario.data.value.combinations)) ){

    sc <- scenario.data.value.combinations %>% filter(row_number() == rn)
    sd <- df.energy.tranformed %>% filter(row_number() == rn)

    s.mu <- sd %>% pull(mu)
    s.sig <- sd %>% pull(sig)


    # draw from energy distribution function:
    single.dist <- rlnorm(n = N,
                          meanlog = s.mu,
                          sdlog = s.sig)


    sampled.data <- sc %>% crossing(tibble(
      energy.per.capita = single.dist,
      draw = seq(1,N))) %>% arrange(draw)

    all.sampled.data <- all.sampled.data %>% bind_rows(
      sampled.data
    )
    if(show.progress.bar){
      if(is.null(pb)){
        # throw warning if pb is not defined
        warning("Progress bar not defined, so we cannot show a progress bar.")
      } else(
        pb$tick()
      )
    }
  }



  return(all.sampled.data)
}

create_sample_data <- function(
    df.core,
    sample.scenarios,
    sample.years,
    sample.variables,
    population.cutoff=0,
    sample.size,
    adjusted.to.dle=TRUE,
    type.of.output="scaled-to-dle-threshold" # "scaled-to-dle-threshold" / "sample-data-for-lorenz-curve"
){
  ##### Create input data for sampling: across-models mean of each variable ------
  if(adjusted.to.dle){
    df.core.mean.acrossmodels <- df.core %>%
      reframe(
        energy.per.capita.uncorrected = mean(energy.per.capita.uncorrected),
        energy.per.capita = mean(energy.per.capita),
        energy.gini = mean(energy.gini),
        .by = c(#"model",
          "scenario", "iso", "variable", "unit", "year", "pop_mil"),
      ) %>%
      mutate(model="model-average")
  } else if (!adjusted.to.dle){
    df.core.mean.acrossmodels <- df.core %>%
      reframe(
        energy.per.capita.uncorrected = mean(energy.per.capita.uncorrected),
        energy.gini = mean(energy.gini),
        .by = c(#"model",
          "scenario", "iso", "variable", "unit", "year", "pop_mil"),
      ) %>%
      rename(energy.per.capita = energy.per.capita.uncorrected) %>%
      mutate(model="model-average")
  }


  # sampling function is computationally intensive, so take a subset of the data
  df.core.mean.acrossmodels.filtered <- df.core.mean.acrossmodels %>%
    filter(
      scenario %in% sample.scenarios,
      variable %in% sample.variables,
      pop_mil >= population.cutoff,
      year %in% sample.years
    )

  ##### Perform sampling ---------------------------------------------------------
  # create a progress bar
  num_ticks <- nrow(df.core.mean.acrossmodels.filtered)
  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent) elapsed :elapsed eta :eta",
                         total = num_ticks)
  # sample.size <- 10000 # one option would be to do e.g. different sample sizes per country (or groups of countries), depending on population size [e.g. 10000 for china, but 100 for a small island state]; note: to sample e.g. China well [i.e. converge to the same average], one would need about 1 million samples
  dle.aligned.sampled.data <- df.core.mean.acrossmodels.filtered %>%
    sample_energy_distributions_sectoral(N=sample.size,
                                         show.progress.bar=T,
                                         pb=pb
    ) %>%
    mutate(pop_mil = pop_mil / sample.size)



  ##### Scale sample data to align with average energy per capita ----------------
  dle.aligned.sample.scaling.factors <- dle.aligned.sampled.data %>%
    left_join(df.core.mean.acrossmodels.filtered %>% select(scenario,iso,variable,year,
                                                            energy.per.capita) %>%
                rename(energy.per.capita.national.average = energy.per.capita)) %>%
    reframe(
      energy.per.capita.national.average.sampled = weighted.mean(energy.per.capita, pop_mil),
      .by = c("model", "scenario", "iso", "variable", "year", "energy.per.capita.national.average")
    ) %>%
    mutate(scaling.factor = energy.per.capita.national.average / energy.per.capita.national.average.sampled)

  dle.aligned.sampled.data.rescaled <- dle.aligned.sampled.data %>%
    left_join(dle.aligned.sample.scaling.factors) %>%
    left_join(df.core %>% select(model,
                                 scenario, iso, variable, year,
                                 dle.threshold.adjusted) %>%
                reframe(
                  dle.model.average = mean(dle.threshold.adjusted),
                  .by = c("scenario", "iso", "variable", "year")
                )
    ) %>%
    mutate(fe.scaled.to.mean = energy.per.capita * scaling.factor)

  ##### Remove (unscaled) sample data to free up RAM -----------------------------
  rm(dle.aligned.sampled.data)
  gc()

  if (type.of.output=="sample-data-for-lorenz-curve"){

    return(dle.aligned.sampled.data.rescaled)

  } else if (type.of.output=="scaled-to-dle-threshold"){
    ##### Scale sample data to multiples of the DLE threshold ----------------
    dle.aligned.sampled.data.rescaled <- dle.aligned.sampled.data.rescaled %>%
      mutate(fe.scaled.to.dle = fe.scaled.to.mean / dle.model.average) %>%
      select(scenario,iso,variable,year,pop_mil,fe.scaled.to.dle)

    dle.aligned.sampled.data.rescaled <- dle.aligned.sampled.data.rescaled %>% mutate(
      facet.scenario = ifelse(scenario %in% shape.sdps, "SDPs", scenario),
    )

    return(dle.aligned.sampled.data.rescaled)
  }
}



# . ----------------------------------------------------------------------------
# . ----------------------------------------------------------------------------
# . ----------------------------------------------------------------------------
# FIGURES IN MANUSCRIPT --------------------------------------------------------











# . ----------------------------------------------------------------------------
# Figure 1: levers of closing the energy development gap -----------------------

# Figure not produced in this script, but in Adobe Illustrator



# . ----------------------------------------------------------------------------
# Figure 2: scenario drivers ---------------------------------------------------


### Auxiliary functions --------------------------------------------------------
owid_parse <- function(df){

  return(
    df %>% select(-Entity) %>%
      distinct() %>% drop_na() %>%
      rename(iso=Code, year=Year)
  )

}

### Data preparation -----------------------------------------------------------

##### [A] Economics ------------------------------------------------------------
gdp.historical <- vroom(
  here(PATH.data.for.figures, "worldbank",
       "API_NY.GDP.PCAP.PP.KD_DS2_en_csv_v2_5996073",
       "API_NY.GDP.PCAP.PP.KD_DS2_en_csv_v2_5996073.csv"),
  skip = 4
) %>%
  wb_parse()
gdp.growth.historical <- gdp.historical %>%
  filter(year%in%c(2000,2019)) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  mutate(gdp.perc.growth.historical = (`2019`-`2000`)/`2000` ) %>%
  # mutate(gdp.perc.growth.historical = (`2019`-`2000`) ) %>%
  select(iso,gdp.perc.growth.historical)

# our world in data (processed based on World Bank PIP)
# World Bank Poverty and Inequality Platform (2022) – with major processing by Our World in Data. “Gini Coefficient – World Bank” [dataset]. World Bank Poverty and Inequality Platform, “World Bank Poverty and Inequality Platform 20220909_2017_01_02_PROD” [original data]. Retrieved January 27, 2024 from https://ourworldindata.org/grapher/economic-inequality-gini-index
gini.historical <-
  read_csv(
    here(PATH.data.for.figures, "worldbank", "economic-inequality-gini-index.csv")
  ) %>% owid_parse()



# gini.historical %>% count(year) %>% print(n=200)
gini.growth.historical <- gini.historical %>%
  filter(year%in%c(1994,1995,1996,1997,1998,1999,2000,2001,2002,
                   2014,2015,2016,2017,2018,2019,2020,2021,2022)) %>%
  mutate(period = ifelse( year %in%c(1994,1995,1996,1997,1998,1999,2000,2001,2002),
                          "1994-2002",
                          "2014-2022"
  )) %>%
  reframe(gini = mean(`Gini coefficient`, na.rm = T),
          .by = c(iso, period)) %>%
  pivot_wider(names_from = period,
              values_from = `gini`) %>%
  mutate(gini.point.change.historical = `2014-2022` - `1994-2002` ) %>%
  select(iso, gini.point.change.historical) %>% drop_na()


gini.growth <- df.core %>% filter(model==vis.model) %>%
  filter(year%in%c(2020,2040)) %>%
  select(scenario,iso,year,gini) %>%
  distinct() %>%  # 3 times smaller (because we drop energy information; 3 sectors)
  pivot_wider(names_from = year, values_from = gini) %>%
  mutate(gini.point.change = `2040`-`2020`) %>%
  select(scenario,iso,gini.point.change)
gdp.growth <- gdp.iso %>% filter(model==vis.model) %>%
  filter(year%in%c(2020,2040)) %>%
  select(scenario,iso,year,gdp_thousand_percapita) %>%
  pivot_wider(names_from = year, values_from = gdp_thousand_percapita) %>%
  mutate(gdp.perc.growth = (`2040`-`2020`)/`2020` ) %>%
  select(scenario,iso,gdp.perc.growth)

p.economic.futures.data <- gini.growth %>% left_join(gdp.growth) %>%
  left_join(wb.income.grouping.3) %>%
  left_join(population.iso.2020 %>% filter(model==vis.model)) %>%
  left_join(gdp.growth.historical) %>%
  left_join(gini.growth.historical) %>%
  filter(pop_mil>1) # filter out small countries where population/gdp may not be aligned
p.economic.futures.data$wb.r3 = factor(p.economic.futures.data$wb.r3,
                                       levels=c('Low',
                                                'Middle',
                                                'High'))


##### [B] Energy consumption ---------------------------------------------------

energy.gini.growth <- df.core %>%
  filter(year%in%c(2020,2040)) %>%
  select(model,scenario,iso,variable,year,energy.gini) %>%
  pivot_wider(names_from = year, values_from = energy.gini) %>%
  mutate(energy.gini.point.change = `2040`-`2020`) %>%
  select(model,scenario,iso,variable,energy.gini.point.change)
energy.use.growth <- df.core %>%
  filter(year%in%c(2020,2040)) %>%
  select(model,scenario,iso,variable,year,energy.per.capita.uncorrected) %>%
  distinct() %>%  # 3 times smaller (because we drop energy information; 3 sectors)
  pivot_wider(names_from = year, values_from = energy.per.capita.uncorrected) %>%
  mutate(energy.per.capita.change = ((`2040`-`2020`) / `2020`) ) %>%
  select(model,scenario,iso,variable,energy.per.capita.change)


# get avg and uncertainties across models
energy.gini.growth.uncert <- energy.gini.growth %>%
  reframe(
    # note: no uncertainty in the gini change...
    energy.gini.point.change.avg = mean(energy.gini.point.change),
    energy.gini.point.change.min = min(energy.gini.point.change),
    energy.gini.point.change.max = max(energy.gini.point.change),
    .by = c("scenario", "iso", "variable")
  )
energy.use.growth.uncert <- energy.use.growth %>%
  reframe(
    # note: no uncertainty in the gini change...
    energy.per.capita.change.avg = mean(energy.per.capita.change),
    energy.per.capita.change.min = min(energy.per.capita.change),
    energy.per.capita.change.max = max(energy.per.capita.change),
    .by = c("scenario", "iso", "variable")
  )


p.energy.futures.data <- energy.gini.growth.uncert %>% left_join(energy.use.growth.uncert) %>%
  iamc_variable_keep_one_level(level = -1) %>%
  left_join(wb.income.grouping.3) %>%
  left_join(population.iso.2020 %>% select(-variable) %>% filter(model==vis.model))
p.energy.futures.data$wb.r3 = factor(p.energy.futures.data$wb.r3,
                                     levels=c('Low',
                                              'Middle',
                                              'High'))

##### [C] Service provisioning -------------------------------------------------
energy.efficiency.change <- df.core %>%
  filter(year%in%c(2020,2040)) %>%
  select(model,scenario,iso,variable,year,dle.tech.scaler) %>%
  distinct() %>%  # 3 times smaller (because we drop energy information; 3 sectors)
  pivot_wider(names_from = year, values_from = dle.tech.scaler) %>%
  mutate(dle.tech.scaler.change = ((`2040`-`2020`) / `2020`) ) %>%
  select(model,scenario,iso,variable,dle.tech.scaler.change)


p.efficiency.futures.data <- energy.efficiency.change %>%
  iamc_variable_keep_one_level(level = -1) %>%
  pivot_wider(names_from = model,
              values_from = dle.tech.scaler.change) %>%
  left_join(wb.income.grouping.3) %>%
  left_join(population.iso.2020 %>% select(-variable) %>% filter(model==vis.model) %>% select(-model))

p.efficiency.futures.data.longer <- p.efficiency.futures.data %>%
  pivot_longer(cols = c(`IMAGE 3.3`,`REMIND-MAgPIE 3.2-4.6`),
               names_to = "model", values_to = "value")

df.weighted.efficiencies <- p.efficiency.futures.data.longer[rep(seq_along(p.efficiency.futures.data.longer$pop_mil), p.efficiency.futures.data.longer$pop_mil), ]
df.weighted.efficiencies$wb.r3 = factor(df.weighted.efficiencies$wb.r3,
                                        levels=c('Low',
                                                 'Middle',
                                                 'High'))


### Main [A] Economics (SDP_RC) ------------------------------------------------
library(ggside)
p.economic.futures.1sdp <- ggplot(
  data = p.economic.futures.data %>% filter(scenario%in%c("SSP2-1p5C", vis.scenario),
                                            wb.r3%in%c("Low","Middle","High")) %>% rename.models.and.scenarios(),
  mapping = aes(x=gdp.perc.growth, y=gini.point.change)
) +
  facet_grid(wb.r3~.) +

  geom_hline(yintercept = 0, colour="darkgrey", linetype = "dashed") +
  geom_vline(xintercept = 0, colour="darkgrey", linetype = "dashed") +


  geom_point(aes(colour=scenario, size=pop_mil), alpha=0.4) +

  geom_xsidedensity(
    aes(
      y = after_stat(ndensity),
      x = gdp.perc.growth,
      fill = scenario,
      colour = scenario
    ),
    # position = "stack",
    linetype = "solid",
    linewidth = 0.5,
    alpha = 0.4
  ) +
  geom_ysidedensity(
    aes(
      x = after_stat(ndensity),
      y = gini.point.change,
      fill = scenario,
      colour = scenario
    ),
    # position = "stack",
    linetype = "solid",
    linewidth = 0.5,
    alpha = 0.4
  ) +
  geom_xsidedensity(
    data = . %>% select(iso,gdp.perc.growth.historical,wb.r3) %>% distinct() %>% drop_na(),
    aes(
      y = after_stat(ndensity),
      x = gdp.perc.growth.historical
    ),
    linetype = "3131",
    linewidth = 0.5
  ) +
  geom_ysidedensity(
    data = . %>% select(iso,gini.point.change.historical,wb.r3) %>% distinct() %>% drop_na(),
    aes(
      x = after_stat(ndensity),
      y = gini.point.change.historical,
    ),
    linetype = "3131",
    linewidth = 0.5
  ) +


  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=c(shape.color.coding)) +
  scale_linetype_manual("Scenario", values=c("3131","solid","solid"), breaks = c("Historical", "SDP-RC", "SSP2-1.5C")) +
  scale_alpha_manual("Scenario", values=c(0,0.5,0.5), breaks = c("Historical", "SDP-RC", "SSP2-1.5C")) +

  # coord_cartesian(xlim = c(-0.5,5)) +
  theme_minimal() +
  scale_x_continuous(#trans = "log10",
    labels = scales::percent,
    name = "GDP (PPP) per capita growth from 2020 to 2040",
    limits = c(-0.5,4)
  ) +
  scale_y_continuous(name = "Change in gini coefficienct from 2020 to 2040") +
  ggside(x.pos = "bottom",
         y.pos = "left",
         scales = "free") +
  labs(title = "Economic futures",
       caption = "Dashed black (96 countries): historial GDP and Gini in past 20 years.\nDistributions across countries, not population weighted.\nNot showing Burundi and Guinea (+459%, +724% GDP, -0.15, -0.12 Gini).") +
  # scale_ysidex_continuous(guide = guide_axis(angle = 90), minor_breaks = NULL) +
  scale_ysidex_continuous(minor_breaks = NULL, trans = "reverse", breaks = NULL) +
  scale_xsidey_continuous(minor_breaks = NULL, trans = "reverse", breaks = NULL) +
  theme(ggside.panel.scale = .2,
        axis.text.x = element_text(angle = 45, hjust = 1))

p.economic.futures.1sdp



### Main [B] Energy consumption (SDP_RC) ---------------------------------------------------
library(see)
p.energy.futures.1sdp <- ggplot(
  data = p.energy.futures.data %>% filter(scenario%in%c("SSP2-1p5C",vis.scenario)) %>%
    # filter(energy.per.capita.change.avg<10) %>%
    mutate_cond(variable=="Residential and Commercial",variable="Residential and\nCommercial") %>% rename.models.and.scenarios,
  mapping = aes(x=energy.per.capita.change.avg,
                y=energy.gini.point.change.avg)
) +
  facet_grid(wb.r3~variable, scales = "free_x") +
  geom_hline(yintercept = 0, colour="darkgrey", linetype = "dashed") +
  geom_vline(xintercept = 0, colour="darkgrey", linetype = "dashed") +
  geom_vline(xintercept = -1, colour="black", linetype = "solid") +


  geom_pointrange2(aes(colour=scenario, fill=scenario, fatten=log(pop_mil),
                       xmin = energy.per.capita.change.min,
                       xmax = energy.per.capita.change.max),
                   alpha=0.4) +
  geom_pointrange2(aes(colour=scenario, fill=scenario, fatten=log(pop_mil),
                       xmin = energy.gini.point.change.min,
                       xmax = energy.gini.point.change.max),
                   alpha=0.4) +


  geom_xsidedensity(
    aes(
      y = after_stat(ndensity),
      x = energy.per.capita.change.avg,
      fill = scenario,
      colour = scenario
    ),
    # position = "stack",
    linetype = "solid",
    linewidth = 0.5,
    alpha = 0.4
  ) +
  geom_ysidedensity(
    aes(
      x = after_stat(ndensity),
      y = energy.gini.point.change.avg,
      fill = scenario,
      colour = scenario
    ),
    # position = "stack",
    linetype = "solid",
    linewidth = 0.5,
    alpha = 0.4
  ) +


  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +

  theme_minimal() +
  scale_x_continuous(#trans = "log10",
    labels = scales::percent,
    breaks = c(-1,0, 3, 5, 10),
    name = "Energy use per capita growth from 2020 to 2040"
  ) +
  scale_y_continuous(name = "Change in energy use gini coefficienct from 2020 to 2040") +
  ggside(x.pos = "bottom",
         y.pos = "left",
         scales = "free") +
  labs(title = "Energy futures",
       caption = "Size is indicative for population size (not on scale).\nDistributions across countries, not population weighted.") +
  # scale_ysidex_continuous(guide = guide_axis(angle = 90), minor_breaks = NULL) +
  scale_ysidex_continuous(minor_breaks = NULL, trans = "reverse", breaks = NULL) +
  scale_xsidey_continuous(minor_breaks = NULL, trans = "reverse", breaks = NULL) +
  theme(ggside.panel.scale = .3,
        axis.text.x = element_text(angle = 45, hjust = 1))

p.energy.futures.1sdp


### Main [C] Service provisioning (SDP_RC) -------------------------------------

# # example for population weighting, from: https://stackoverflow.com/questions/59831248/weight-ggridges-by-another-variable
# example2 <- example[rep(seq_along(example$weight), example$weight), ]
# ggplot(example2,aes(x=position,y=year))+
#   ggridges::geom_density_ridges()+
#   theme_classic()
library(ggridges)
p.efficiency.futures.pop.weighted.1sdp <- ggplot(
  data = df.weighted.efficiencies %>% filter(scenario%in%c("SSP2-1p5C",vis.scenario)) %>% rename.models.and.scenarios,
  mapping = aes(x=value,
                y=model,
                fill = scenario,
                colour = scenario)
) +
  facet_grid(wb.r3~variable) +
  geom_vline(xintercept = 0, colour="darkgrey", linetype = "dashed") +
  geom_vline(xintercept = -1, colour="black", linetype = "solid") +

  # see: https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
  geom_density_ridges(#rel_min_height = 0.05,
    alpha=0.2,
    aes(height = stat(density)),
    stat = "binline", bins=50, scale = 0.95, draw_baseline = FALSE
    # jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0), point_shape = '|', point_size = 3, point_alpha = 0.1
  ) +

  scale_point_size_continuous(range = c(0.5, 4)) +
  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +

  theme_minimal() +
  scale_x_continuous(
    expand = c(0, 0),
    labels = scales::percent,
    name = "Energy intensity of service provisioning from 2020 to 2040"
  ) +
  scale_y_discrete(
    name = NULL
  ) +
  labs(title = "Service Provisioning Efficiency futures",
       caption = "Population weighted. Industry is modelled as globally uniform.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p.efficiency.futures.pop.weighted.1sdp




### Main [A+B+C] ---------------------------------------------------------------
library(patchwork)
layout.p.drivers <- "
ABBB
ABBB
ABBB
CCCC
"
p.drivers <- ((p.economic.futures.1sdp + theme(legend.position = "none")) +
                (p.energy.futures.1sdp) +
                (p.efficiency.futures.pop.weighted.1sdp) +
                plot_layout(design = layout.p.drivers) +
                plot_annotation(
                  title = "Drivers (2020 to 2040) of development in scenarios",
                  tag_levels = "A"
                ))

# p.drivers

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, paste0("f02") ),
  p = p.drivers,
  w = 400,
  h = 350
)









# . ----------------------------------------------------------------------------
# Figure 3: inequality: global (global gini) and within-country inequality (palma) --------





### Data preparation -----------------------------------------------------------


##### [A] National palma ratios ------------------------------------------------
df.palma.ratio.energy <- df.core %>%
  reframe(
    energy.per.capita.uncorrected = mean(energy.per.capita.uncorrected),
    # energy.per.capita = mean(energy.per.capita),
    energy.gini = mean(energy.gini),
    .by = c(#"model",
      "scenario", "iso", "variable", "unit", "year", "pop_mil"),
  ) %>%
  mutate(model="model-average") %>%
  filter(
    scenario %in% c("SDP_RC-1p5C", "SSP2-1p5C")
  ) %>%
  mutate(
    nu=get_nu_lognormal(mean=energy.per.capita.uncorrected, gini = energy.gini),
    sig=get_sigma_lognormal(gini = energy.gini)
  ) %>%
  mutate(palma.ratio = get_palma_ratio_lognormal(nu=nu,sigma=sig))


##### [B] Lorenz curve: international vs with within-country -------------------

# Sample data
library(progress)
lorenz.sampled.data <- create_sample_data(
  df.core,
  sample.scenarios = c("SDP_RC-1p5C"),
  sample.years = c(2020,2040),
  sample.variables = c("Final Energy|Residential and Commercial", "Final Energy|Industry", "Final Energy|Transportation"),
  population.cutoff = 0,
  sample.size = SAMPLING.SIZE, #10000
  adjusted.to.dle = FALSE,

  type.of.output="sample-data-for-lorenz-curve"
) %>%
  select(model,scenario,iso,variable,unit,year,pop_mil,
         fe.scaled.to.mean) %>%
  rename(energy.per.capita.uncorrected=fe.scaled.to.mean)

nationalaverage.data <- df.core %>%
  reframe(
    energy.per.capita.uncorrected = mean(energy.per.capita.uncorrected),
    energy.gini = mean(energy.gini),
    .by = c(#"model",
      "scenario", "iso", "variable", "unit", "year", "pop_mil"),
  ) %>%
  mutate(model="model-average") %>%
  filter(
    scenario %in% c("SDP_RC-1p5C"),
    # iso == "AUT",
    pop_mil >= 0,
    year %in% c(2020,2040)
  )



global.totals <- nationalaverage.data %>%
  reframe(
    global.pop_mil = sum(pop_mil),
    global.energy = sum(pop_mil * energy.per.capita.uncorrected),
    .by = c("scenario", "variable", "year")
  )

# prepare lorenz curve type data - national means (IAM)
lorenz.prep.national <- nationalaverage.data %>%
  left_join(global.totals) %>%
  mutate(population.share = pop_mil / global.pop_mil) %>%
  mutate(final.energy.share = energy.per.capita.uncorrected * pop_mil / global.energy) %>%
  group_by(variable, year) %>%
  arrange(energy.per.capita.uncorrected) %>%
  mutate(cumu.pop = cumsum(population.share)) %>%
  mutate(cumu.fe = cumsum(final.energy.share)) %>%
  ungroup()

# - add zero-zero point
lorenz.national <- lorenz.prep.national %>%
  bind_rows(
    lorenz.prep.national %>% select(scenario,year,variable) %>% distinct() %>%
      mutate(cumu.pop = 0, cumu.fe = 0)
  ) %>%
  arrange(scenario,year,variable, cumu.pop)

# prepare lorenz curve type data - national within-country samples (DESIRE)
# - start: similar to national averages
lorenz.prep.distributions <- lorenz.sampled.data %>%
  left_join(global.totals) %>%
  mutate(population.share = pop_mil / global.pop_mil) %>%
  mutate(final.energy.share = energy.per.capita.uncorrected * pop_mil / global.energy) %>%
  group_by(variable, year) %>%
  arrange(energy.per.capita.uncorrected) %>%
  mutate(cumu.pop = cumsum(population.share)) %>%
  mutate(cumu.fe = cumsum(final.energy.share)) %>%
  ungroup()
# - since draws do not need to add up to the actual total, we need to re-normalise the final energy shares
#     NOTE: at 1000 draws, one can expect this to be at most 1% off, global, if 6 instances (3 variables, 2 years).
# print out the maximum cumulative final energy share
lorenz.prep.distributions %>%
  group_by(scenario,variable,year) %>%
  summarise(max.cumu.fe = max(cumu.fe)) %>%
  ungroup() %>%
  pull(max.cumu.fe) %>% sort()
# adjust (should not be necessary; and not change anything)
# - [ ] check
lorenz.prep.distributions <- lorenz.prep.distributions %>%
  group_by(scenario,variable,year) %>%
  mutate(cumu.fe = cumu.fe / max(cumu.fe)) %>%
  ungroup()

# - add zero-zero point (global)
lorenz.distributions <- lorenz.prep.distributions %>%
  bind_rows(
    lorenz.prep.distributions %>% select(scenario,year,variable) %>% distinct() %>%
      mutate(cumu.pop = 0, cumu.fe = 0)
  ) %>%
  arrange(scenario,year,variable, cumu.pop)

rm(lorenz.prep.distributions)
gc()

# prepare lorenz curve type data - empirical Oswald data
# load data
gtap9 <- readRDS(
  file.path(
    PATH.data.desire,
    "other","inequality",
    "FE_quantile_iso_sector.RData"
  )
) %>%
  mutate(n.quantile = NA) %>%
  mutate_cond((quantile == "lowest" | quantile == "Q1"), n.quantile = 1) %>%
  mutate_cond((quantile == "low" | quantile == "Q2"), n.quantile = 2) %>%
  mutate_cond((quantile == "middle" | quantile == "Q3"), n.quantile = 3) %>%
  mutate_cond((quantile == "high" | quantile == "Q4"), n.quantile = 4) %>%
  mutate_cond((quantile == "Q5"), n.quantile = 5) %>%
  pivot_longer(cols = c(fe, fe.rescom, fe.transport,fe.industry), names_to = "variable", values_to = "fe") %>%
  mutate_cond(variable == "fe", variable = "Final Energy") %>%
  mutate_cond(variable == "fe.rescom", variable = "Final Energy|Residential and Commercial") %>%
  mutate_cond(variable == "fe.transport", variable = "Final Energy|Transportation") %>%
  mutate_cond(variable == "fe.industry", variable = "Final Energy|Industry") %>%
  select(iso, variable, n.quantile, fe, population) %>%
  arrange(iso, variable, n.quantile) %>%
  filter(population != 0) %>%
  drop_na() # transport for Benin (quantiles 1,2,3), and for Belarus (quantile 1)

# prepare data for lorentz plots
# # gtap
total.pop.global <- gtap9 %>%
  reframe(total.pop = sum(population),
          .by = c("variable"))
total.energy.global <- gtap9 %>%
  reframe(total.fe = sum(fe * population * giga / exa),
          .by = c("variable"))

gtap9.prep <- gtap9 %>%
  left_join(total.pop.global) %>%
  left_join(total.energy.global) %>%
  mutate(population.share = population / total.pop) %>%
  mutate(final.energy.share = (fe * population * giga / exa) / total.fe) %>%
  group_by(variable) %>%
  arrange(fe) %>%
  mutate(cumu.pop = cumsum(population.share)) %>%
  mutate(cumu.fe = cumsum(final.energy.share)) %>%
  ungroup()
lorenz.gtap9.oswald <- gtap9.prep %>%
  # add zero-zero point
  bind_rows(
    gtap9.prep %>% select(variable) %>% distinct() %>%
      mutate(cumu.pop = 0, cumu.fe = 0, fe = 0)
  ) %>%
  arrange(variable, cumu.pop) %>%
  filter(variable != "Final Energy")



##### [C] Global Theil's L index decomposition -------------------------------------------
# - international vs within-country sources of inequality
# ...
# still to be implemented;
# - inspired by Johannes' plot; check there and try to replicate and write out the steps here
# - methods: https://www.oecd-ilibrary.org/docserver/reg_glance-2016-50-en.pdf?expires=1707064610&id=id&accname=guest&checksum=2E60CEF45AE233B2CF0585836EE96A61
#    ... and Giammatteo, M. (2007). The Bidimensional Decomposition of Inequality: A nested Theil Approach. LIS Working papers, Article 466, 1-30
# - implementation: https://search.r-project.org/CRAN/refmans/iIneq/html/iTheilT.html
#    ... and https://cran.r-project.org/web/packages/ineq.2d/ineq.2d.pdf
# ...
# or, other documentation, from wikipedia: https://en.wikipedia.org/wiki/Theil_index#Decomposability
# ...subgroup = country, for us...
# If the population is divided into m {\displaystyle m} subgroups and
#
#     s i {\displaystyle s_{i}} is the income share of group i {\displaystyle i},
#     N {\displaystyle N} is the total population and N i {\displaystyle N_{i}} is the population of group i {\displaystyle i},
#     T i {\displaystyle T_{i}} is the Theil index for that subgroup,
#     x ¯ i {\displaystyle {\overline {x}}_{i}} is the average income in group i {\displaystyle i}, and
#     μ {\displaystyle \mu } is the average income of the population,
#
# then Theil's T index is
#
#     T T = ∑ i = 1 m s i T i + ∑ i = 1 m s i ln ⁡ x ¯ i μ {\displaystyle T_{T}=\sum _{i=1}^{m}s_{i}T_{i}+\sum _{i=1}^{m}s_{i}\ln {\frac {{\overline {x}}_{i}}{\mu }}} for s i = N i N x ¯ i μ {\displaystyle s_{i}={\frac {N_{i}}{N}}{\frac {{\overline {x}}_{i}}{\mu }}}
# for theil index: dineq package: https://cran.r-project.org/web/packages/dineq/dineq.pdf (also does gini decomposition, with an interaction term)
# or more simply: Theil index per country, for a lognormal distribution, is simply given by (sigma^2)/2



# calculate all Theil (T) index decomposition data:
theil.data.prep <- df.core %>%
  reframe(
    energy.per.capita.uncorrected = mean(energy.per.capita.uncorrected),
    # energy.per.capita = mean(energy.per.capita),
    energy.gini = mean(energy.gini),
    .by = c(#"model",
      "scenario", "iso", "variable", "unit", "year", "pop_mil"),
  ) %>%
  mutate(model="model-average") %>%
  # we can drop all the sampling, because we assume a lognormal distrbution, for which the theil index is simply (sigma^2)/2
  distinct(scenario, iso, variable, year, # data points to calculate for
           pop_mil, energy.per.capita.uncorrected, # data to calculate with
           energy.gini # for sigma
  ) %>%
  filter(#iso%in%c("IND", "USA"),
    year>=2020, year<=2100, year!=2055) %>%
  mutate(sig = get_sigma_lognormal(gini=energy.gini))

## get all parameters
# global population
theil.data.prep.N <- theil.data.prep %>%
  reframe(N = sum(pop_mil),
          .by = c("scenario", "variable", "year"))
# subgroup (=country) population
theil.data.prep.N_i <- theil.data.prep %>%
  reframe(N_i = sum(pop_mil),
          .by = c("scenario", "iso", "variable", "year"))
# Theil index for each subgroup (=country) -- N.B. we're now not making use of the sampling, so this could be done much faster - as we're not using it at all because there's a shortcut for a lognormal distribution.
theil.data.prep.T_i <- theil.data.prep %>%
  mutate(T_i = (sig^2)/2) %>%
  distinct(scenario,variable,year, iso,
           T_i)
# average income in subgroup (=country)
theil.data.prep.x_i_mean <- theil.data.prep %>%
  reframe(x_i_mean = weighted.mean(energy.per.capita.uncorrected, pop_mil), # weights should be the same, but just for redundancy
          .by = c("scenario", "iso", "variable", "year"))
# energy income of the global population
theil.data.prep.mu <- theil.data.prep %>%
  reframe(mu = weighted.mean(energy.per.capita.uncorrected, pop_mil),
          .by = c("scenario", "variable", "year"))
# weights s_i (=income share of each subgroup (=country))
theil.data.prep.s_i <- theil.data.prep %>% distinct(scenario,variable,year) %>%
  left_join(theil.data.prep.N_i) %>%
  left_join(theil.data.prep.N) %>%
  left_join(theil.data.prep.x_i_mean) %>%
  left_join(theil.data.prep.mu) %>%
  mutate(s_i = N_i/N * x_i_mean/mu)

theil.data.prep.T_t <- theil.data.prep.s_i %>%
  left_join(theil.data.prep.T_i) %>%
  distinct(scenario,variable,year,
           s_i, T_i, x_i_mean, mu) %>% # remove duplicates; the sum is over countries (sub-national samples only necessary to calculate the country-level T_i)
  reframe(T_t = sum(s_i*T_i) + sum(s_i*log(x_i_mean/mu)),
          T_t_within = sum(s_i*T_i),
          T_t_between = sum(s_i*log(x_i_mean/mu)),
          .by = c("scenario", "variable", "year"))




### Main [A] national Palma ratios ---------------------------------------------
# - from: https://stackoverflow.com/questions/63550588/ggplot2coord-cartesian-on-facets
# install.packages("ggh4x") # for doing coord_cartesian like behaviour in a faceted plot
library(ggh4x)
library(ggthemes)
p.national.palma <- ggplot(df.palma.ratio.energy %>%
                             filter(
                               year >= 2020,
                               year <= 2040
                             ) %>%
                             filter(
                               # remove outliers for visualisation?
                             ) %>%
                             ms_add() %>% mutate(msi = paste0(`model-scenario`,"-",iso)) %>%
                             rename.models.and.scenarios() %>% iamc_variable_keep_one_level(level = -1),
                           aes(x=year,y=palma.ratio,
                               group=msi)) +
  facet_wrap(.~variable, scales = "free_y", nrow = 1) +
  geom_line(aes(colour=scenario), alpha=0.1) +

  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +

  scale_x_continuous(expand = c(0,0)) +
  xlab(NULL) +

  facetted_pos_scales(
    x = list(
      # NULL,
      NULL,
      # NULL,
      NULL,
      # NULL,
      NULL
    ),
    y = list(
      # scale_y_continuous(limits = c(0, 4), expand = c(0,0), name = "Palma ratio"),
      scale_y_continuous(limits = c(0, 4), expand = c(0,0), name = "Palma ratio"),
      # scale_y_continuous(limits = c(0, 4), expand = c(0,0), name = "Palma ratio"),
      scale_y_continuous(limits = c(0, 4), expand = c(0,0), name = "Palma ratio"),
      # scale_y_continuous(limits = c(0, 8), expand = c(0,0), name = "Palma ratio"),
      scale_y_continuous(limits = c(0, 8), expand = c(0,0), name = "Palma ratio")
    )
  ) +

  labs(subtitle = bquote(bold("National palma ratio:")~"\nenergy consumption share of the top 10% of consumers divided by that of the lowest 40%")) +

  theme_classic() +
  theme_hc() +
  theme(#ggside.panel.scale = .3,
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y.right = element_text(angle = 0))

# p.national.palma



### Main [B] Global Lorenz curves ---------------------------------------------------
# plot lorentz
p.lorenz <-
  ggplot() +
  facet_grid(~variable) +

  # oswald data
  geom_point(
    data = lorenz.gtap9.oswald %>% mutate(year="2011 (Oswald et al., 2020; 2021)") %>% iamc_variable_keep_one_level(level = -1),
    aes(y = cumu.fe, x = cumu.pop, colour = year, group = year)
  ) +
  # national data
  geom_line(
    data = lorenz.national %>% mutate(year=paste0(as.character(year), " (IAM, DESIRE): SDP-RC")) %>% iamc_variable_keep_one_level(level = -1),
    linetype = "dotted",
    aes(y = cumu.fe, x = cumu.pop, colour = year, group = year)
  ) +
  # sampled data
  geom_line(
    data = lorenz.distributions %>% mutate(year=paste0(as.character(year), " (IAM, DESIRE): SDP-RC")) %>% iamc_variable_keep_one_level(level = -1),
    aes(y = cumu.fe, x = cumu.pop, colour = year, group = year)
  ) +

  # equality line
  geom_abline(
    intercept = 0, # max(compare.energy.diff$DLE.diff, na.rm=TRUE),
    slope = 1,
    colour = "black",
    linetype = "dashed"
  ) +

  scale_y_continuous(expand = c(0, 0), breaks = NULL, limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), breaks = NULL, limits = c(0, 1)) +

  scale_colour_ptol() +

  xlab("Cumulative population") +
  ylab("Cumulative final energy consumption") +
  labs(
    # title = "Lognormal distributions: vs energy consumption data",
    subtitle = bquote("Global"~bold("Lorenz curves")~"\nfor final energy consumption"),
    # caption = "Solid line: based on sampled (10,000 per country) data with within-country distributions.\nDotted line = using national averages."
  ) +
  guides(colour = guide_legend(NULL), shape = guide_legend(NULL)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

# p.lorenz

### Main [C] Global Theil Index decomposition ----------------------------------
theil.decomposition <- ggplot(
  data = theil.data.prep.T_t %>% filter(year<=2040) %>% mutate(model="model-average") %>%
    rename.models.and.scenarios() %>% iamc_variable_keep_one_level(level = -1),
  aes(x=year,
      fill = scenario,
      colour = scenario)
) +
  facet_grid(variable~scenario) +
  geom_line(aes(y = T_t), alpha = 1) +
  geom_ribbon(aes(ymin = 0, ymax = T_t), alpha = 0.2) +
  geom_ribbon(aes(ymin = 0, ymax = T_t_within), alpha = 1) +

  ylab("Theil Index (T_t) Decomposition") +
  geom_text(label = "between",
            data = . %>% filter(year==2030,
                                scenario=="SDP-EI"),
            mapping=aes(x=2021,y=(T_t+T_t_within)/2+0.1) , hjust=-0.1) +
  geom_text(label = "within",
            colour = "white",
            data = . %>% filter(scenario=="SSP2-Ref"),
            mapping=aes(x=2021,y=0.05) , hjust=-0.1) +

  labs(
    subtitle = bquote("Global"~bold("between- and within-country")~"inequality"),
  ) +

  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +
  # scale_x_continuous(expand = c(0,0)) +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y.right = element_text(angle = 0))

# theil.decomposition



### Main [A+B+C] ---------------------------------------------------------------





p.inequality <-
  (
    p.national.palma +
      p.lorenz +
      theil.decomposition
  ) +
  plot_annotation(
    title = "The importance of within-country inequality",
    tag_levels = "A"
  ) +
  plot_layout(heights = c(1,1.5,1.5))

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, paste0("f03") ),
  p = p.inequality,
  w = 300,
  h = 300
)







# . ----------------------------------------------------------------------------
# Figure 4: DLE threshold and rescom headcounts --------------------------------


### Data preparation -----------------------------------------------------------

##### [A] DLE thresholds -------------------------------------------------------
p.dle.data <- df.core %>%
  reframe(
    dle = sum(dle.threshold.adjusted),
    dle.constant = sum(dle.threshold.curtech),
    .by = c("model", "scenario", "iso", "year")
  ) %>%
  mutate(dle.change = dle / dle.constant ) %>%
  left_join(wb.income.grouping.3) %>%
  left_join(population.iso %>% select(-variable,-unit))
p.dle.data.global <- p.dle.data %>%
  reframe(
    dle = weighted.mean(dle, pop_mil),
    .by = c("model", "scenario", "year")
  )
p.dle.data$wb.r3 = factor(p.dle.data$wb.r3,
                          levels=c('Low',
                                   'Middle',
                                   'High'))


##### [B] ResCom headcounts ----------------------------------------------------
p.dle.headcount.data <- df.core %>%
  left_join(wb.income.grouping.3) %>%
  left_join(population.iso %>% select(-variable,-unit)) %>%
  # from_wbr4_to_wbr3() %>%
  reframe(
    dle.headcount = sum(share.below.projected.adjusted*pop_mil),
    dle.headcount.constant = sum(share.below.projected.curtech*pop_mil),

    dle.headcount.p5 = sum(pmax(share.below.projected.adjusted-0.05,0)*pop_mil),
    dle.headcount.constant.p5 = sum(pmax(share.below.projected.curtech-0.05,0)*pop_mil),

    dle.headcount.p10 = sum(pmax(share.below.projected.adjusted-0.10,0)*pop_mil),
    dle.headcount.constant.p10 = sum(pmax(share.below.projected.curtech-0.10,0)*pop_mil),

    dle.headcount.p20 = sum(pmax(share.below.projected.adjusted-0.20,0)*pop_mil),
    dle.headcount.constant.p20 = sum(pmax(share.below.projected.curtech-0.20,0)*pop_mil),

    # also: energy gap as share of energy threshold
    # dle.energy.gap.share = weighted.mean(x=(depth.below.projected.adjusted*share.below.projected.adjusted)/dle.threshold.adjusted, w=pop_mil),

    .by = c("model", "scenario", "wb.r3", "variable", "year")
    # .by = c("model", "scenario", "wb.r3", "variable", "year")
  ) %>%
  mutate(dle.headcount.change = dle.headcount / dle.headcount.constant)

p.dle.headcount.data$wb.r3 = factor(p.dle.headcount.data$wb.r3,
                                    levels=c("High",
                                             "Middle",
                                             "Low"))

### Main [A] DLE thresholds ----------------------------------------------------

p.dle.threshold.futures <- ggplot(
  data = p.dle.data %>% ms_add() %>% mutate(msi = paste0(`model-scenario`,"-",iso)) %>%
    filter(year>=2020,year<=DLE.END.YEAR.TIMESERIES) %>% rename.models.and.scenarios,
  mapping = aes(x=year)
) +
  facet_grid(model~scenario) +

  geom_line(aes(group=msi,
                y=dle,
                colour = scenario),
            alpha=0.05) +

  geom_textline(
    data = p.dle.data.global %>% ms_add() %>%
      mutate(msi = paste0(`model-scenario`,"-global")) %>%
      filter(year>=2020,year<=DLE.END.YEAR.TIMESERIES,
             scenario=="SSP2-1p5C") %>% rename.models.and.scenarios,
    aes(group=msi,
        y=dle),
    colour = "red",
    label = "Global average"
  ) +
  geom_line(
    data = p.dle.data.global %>% ms_add() %>%
      mutate(msi = paste0(`model-scenario`,"-global")) %>%
      filter(year>=2020,year<=DLE.END.YEAR.TIMESERIES,
             scenario!="SSP2-1p5C") %>% rename.models.and.scenarios,
    aes(group=msi,
        y=dle),
    colour = "red"
  ) +

  geom_ysidedensity(
    data = . %>% filter(year == DLE.END.YEAR.TIMESERIES),
    aes(
      x = after_stat(density),
      y = dle,
      fill = scenario,
      colour = scenario
    ),
    # position = "stack",
    linetype = "solid",
    linewidth = 0.5,
    alpha = 0.4
  ) +
  geom_ysidedensity(
    data = . %>% filter(year == DLE.END.YEAR.TIMESERIES),
    aes(
      x = after_stat(density),
      y = dle.constant
    ),
    linetype = "3131",
    linewidth = 0.7
  ) +

  labs(title = bquote("Minimum energy requirement (DLE) over time:" ~ bold("all sectors combined")),
       caption = "Horizontal black dashed lines are 2020 DLE threshold values.\nEach line is one country (for each model)\nDistributions are kernel densities, and are not population-weighted.") +
  scale_ysidex_continuous(minor_breaks = NULL, breaks = NULL) +
  scale_xsidey_continuous(minor_breaks = NULL, breaks = NULL) +

  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "GJ/cap/yr",
                     limits = c(0,50),
                     breaks = seq(0,40,10),
                     expand = c(0,0)) +

  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +
  theme_classic() +
  theme_hc() +
  theme(ggside.panel.scale = .3,
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

p.dle.threshold.futures

### Main [B] ResCom headcounts -------------------------------------------------

p.dle.headcount.futures <- ggplot(
  data = p.dle.headcount.data %>% ms_add() %>%
    reframe(dle.headcount = sum(dle.headcount),
            .by = c("model", "scenario", "variable", "year")) %>%
    iamc_variable_keep_one_level(level = -1) %>%
    filter(year>=2020,
           year<=DLE.END.YEAR.TIMESERIES,
           variable=="Residential and Commercial") %>%
    rename.models.and.scenarios %>%
    rename(Model=model),
  mapping = aes(x=year)
) +
  # facet_grid(.~scenario) +

  geom_ribbon(
    data = . %>%
      filter(grepl(scenario,pattern="SDP-",fixed=T)) %>%
      reframe(min.dle.headcount = min(dle.headcount), max.dle.headcount = max(dle.headcount), .by=c("variable", "year")),
    fill = "dodgerblue",
    aes(ymin=min.dle.headcount,ymax=max.dle.headcount),
    alpha=0.2
  ) +
  geom_ribbon(
    data = . %>%
      filter(!grepl(scenario,pattern="SDP-",fixed=T)) %>%
      reframe(min.dle.headcount = min(dle.headcount), max.dle.headcount = max(dle.headcount), .by=c("variable", "year")),
    fill = "grey",
    aes(ymin=min.dle.headcount,ymax=max.dle.headcount),
    alpha=0.2
  ) +

  geom_line(aes(y=dle.headcount,
                color=scenario,
                linetype = Model)) +
  labs(subtitle = bquote("Population below minimum energy requirement (DLE) threshold over time:" ~ bold("Residential and Commercial sector"))) +
  scale_ysidex_continuous(minor_breaks = NULL, breaks = NULL) +
  scale_xsidey_continuous(minor_breaks = NULL, breaks = NULL) +

  scale_x_continuous(name = NULL,
                     expand = c(0,0)) +
  scale_y_continuous(name = "Million",
                     expand = c(0,0),
                     limits = c(0,6000)) +

  scale_color_manual("Scenario", values=shape.color.coding) +

  theme_classic() +
  theme_hc() +
  theme(ggside.panel.scale = .3,
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y.right = element_text(angle = 0))

p.dle.headcount.futures


### Main [A+B] -----------------------------------------------------------------
layout.p.dle <- "
A
B
"

p.dle <- p.dle.threshold.futures + p.dle.headcount.futures +
  plot_layout(design = layout.p.dle) +
  plot_annotation(tag_levels = "A")

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, paste0("f04") ),
  p = p.dle,
  w = 250,
  h = 250
)



# . ----------------------------------------------------------------------------
# Figure 5: ResCom distribution plots ------------------------------------------

### Data preparation -----------------------------------------------------------

##### Sample the data ----------------------------------------------------------
SAMPLING.SIZE <- 10000
dle.aligned.sampled.data.rescaled <- create_sample_data(
  df.core,
  sample.scenarios = c("SDP_RC-1p5C", "SDP_EI-1p5C", "SDP_MC-1p5C", "SSP2-1p5C", "SSP2-NPi"),
  sample.years = c(2020,2040),
  sample.variables = c("Final Energy|Residential and Commercial"),
  population.cutoff = 0,
  sample.size = SAMPLING.SIZE,

  type.of.output="scaled-to-dle-threshold"
)


### Plot the results -----------------------------------------------------------
# Ideas:
# - make this plot as a function, to reduce repetition in the code below.


##### Global -------------------------------------------------------------------
p.dle.rescaled.fe.global <- ggplot(
  data = dle.aligned.sampled.data.rescaled %>%
    filter(fe.scaled.to.dle<=10) %>% mutate(model="model-average") %>%
    rename.models.and.scenarios(),
  mapping = aes(x=fe.scaled.to.dle)
) +
  facet_grid(scenario~.) +
  geom_vline(xintercept = 1, linewidth=0.7, linetype = "dashed") +
  geom_hline(yintercept = 0, linewidth=0.1, colour="darkgrey", linetype = "solid") +
  geom_histogram(
    data =. %>% filter(year==2040),
    aes(fill = ifelse(fe.scaled.to.dle < 1, "TRUE", scenario),
        weight = pop_mil),
    # position = "identity",
    alpha=0.8,
    binwidth=0.2,
    boundary=0
  ) +
  geom_histogram(
    data=. %>% filter(year==2020),
    color="black",
    aes(weight = pop_mil,
        y = ifelse(
          after_stat(count) > 0, after_stat(count), NA
        )
    ),
    alpha=0,
    binwidth=0.2,
    boundary=0
  ) +
  geom_text(x=0.5,hjust=0, y=1500, label="2020", colour = "black") +
  geom_text(x=3,hjust=0, y=500, label="2040", aes(colour = scenario)) +
  scale_color_manual(values = c("TRUE" = "red", shape.color.coding)) +
  scale_fill_manual(values = c("TRUE" = "red", shape.color.coding)) +
  scale_x_continuous(limits = c(0,10), expand=c(0,0),
                     breaks = c(0,1,2,4,6,8,10)) +
  scale_y_continuous(name = NULL) +
  labs(
    # title = bquote("Distribution of final energy per capita\nfor Residential and Commercial energy\nrescaled to national DLE thresholds"),
    title = "World",
    x = "Multiples of DLE threshold"
  ) +
  theme_void() +
  theme_hc() +
  theme(
    panel.spacing.y=unit(0.5,"mm"),
    plot.margin=unit(c(2,5,2,5),"mm"),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    # remove lines
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

##### USA ----------------------------------------------------------------------
p.dle.rescaled.fe.USA <- ggplot(
  data = dle.aligned.sampled.data.rescaled %>% filter(variable=="Final Energy|Residential and Commercial",
                                   iso=="USA"
  ) %>%
    filter(fe.scaled.to.dle<=10) %>% mutate(model="model-average") %>%
    rename.models.and.scenarios(),
  mapping = aes(x=fe.scaled.to.dle)
) +
  facet_grid(scenario~.) +
  geom_vline(xintercept = 1, linewidth=0.7, linetype = "dashed") +
  geom_hline(yintercept = 0, linewidth=0.1, colour="darkgrey", linetype = "solid") +
  geom_histogram(
    data =. %>% filter(year==2040),
    aes(fill = ifelse(fe.scaled.to.dle < 1, "TRUE", scenario),
        weight = pop_mil),
    # position = "identity",
    alpha=0.8,
    binwidth=0.2,
    boundary=0
  ) +
  geom_histogram(
    data=. %>% filter(year==2020),
    color="black",
    aes(weight = pop_mil,
        y = ifelse(
          after_stat(count) > 0, after_stat(count), NA
        )
    ),
    alpha=0,
    binwidth=0.2,
    boundary=0
  ) +
  scale_fill_manual(values = c("TRUE" = "red", shape.color.coding)) +
  scale_x_continuous(limits = c(0,10), expand=c(0,0),
                     breaks = c(0,1,2,4,6,8,10)) +
  scale_y_continuous(name = NULL) +
  labs(
    title = "United States of America",
    x = NULL
  ) +
  theme_void() +
  theme_hc() +
  theme(
    panel.spacing.y=unit(0.5,"mm"),
    plot.margin=unit(c(2,5,2,5),"mm"),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    # remove lines
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

##### China --------------------------------------------------------------------
p.dle.rescaled.fe.CHN <- ggplot(
  data = dle.aligned.sampled.data.rescaled %>% filter(variable=="Final Energy|Residential and Commercial",
                                   iso=="CHN"
  ) %>%
    filter(fe.scaled.to.dle<=10) %>% mutate(model="model-average") %>%
    rename.models.and.scenarios(),
  mapping = aes(x=fe.scaled.to.dle)
) +
  facet_grid(scenario~.) +
  geom_vline(xintercept = 1, linewidth=0.7, linetype = "dashed") +
  geom_hline(yintercept = 0, linewidth=0.1, colour="darkgrey", linetype = "solid") +
  geom_histogram(
    data =. %>% filter(year==2040),
    aes(fill = ifelse(fe.scaled.to.dle < 1, "TRUE", scenario),
        weight = pop_mil),
    # position = "identity",
    alpha=0.8,
    binwidth=0.2,
    boundary=0
  ) +
  geom_histogram(
    data=. %>% filter(year==2020),
    color="black",
    aes(weight = pop_mil,
        y = ifelse(
          after_stat(count) > 0, after_stat(count), NA
        )
    ),
    alpha=0,
    binwidth=0.2,
    boundary=0
  ) +
  scale_fill_manual(values = c("TRUE" = "red", shape.color.coding)) +
  scale_x_continuous(limits = c(0,10), expand=c(0,0),
                     breaks = c(0,1,2,4,6,8,10)) +
  scale_y_continuous(name = NULL) +
  labs(
    title = "China",
    x = NULL
  ) +
  theme_void() +
  theme_hc() +
  theme(
    panel.spacing.y=unit(0.5,"mm"),
    plot.margin=unit(c(2,5,2,5),"mm"),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    # remove lines
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

##### India --------------------------------------------------------------------
p.dle.rescaled.fe.IND <- ggplot(
  data = dle.aligned.sampled.data.rescaled %>% filter(variable=="Final Energy|Residential and Commercial",
                                   iso=="IND"
  ) %>%
    filter(fe.scaled.to.dle<=10) %>% mutate(model="model-average") %>%
    rename.models.and.scenarios(),
  mapping = aes(x=fe.scaled.to.dle)
) +
  facet_grid(scenario~.) +
  geom_vline(xintercept = 1, linewidth=0.7, linetype = "dashed") +
  geom_hline(yintercept = 0, linewidth=0.1, colour="darkgrey", linetype = "solid") +
  geom_histogram(
    data =. %>% filter(year==2040),
    aes(fill = ifelse(fe.scaled.to.dle < 1, "TRUE", scenario),
        weight = pop_mil),
    # position = "identity",
    alpha=0.8,
    binwidth=0.2,
    boundary=0
  ) +
  geom_histogram(
    data=. %>% filter(year==2020),
    color="black",
    aes(weight = pop_mil,
        y = ifelse(
          after_stat(count) > 0, after_stat(count), NA
        )
    ),
    alpha=0,
    binwidth=0.2,
    boundary=0
  ) +
  scale_fill_manual(values = c("TRUE" = "red", shape.color.coding)) +
  scale_x_continuous(limits = c(0,10), expand=c(0,0),
                     breaks = c(0,1,2,4,6,8,10)) +
  scale_y_continuous(name = NULL) +
  labs(
    title = "India",
    x = NULL
  ) +
  theme_void() +
  theme_hc() +
  theme(
    panel.spacing.y=unit(0.5,"mm"),
    plot.margin=unit(c(2,5,2,5),"mm"),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    # remove lines
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

##### Nigeria ------------------------------------------------------------------
p.dle.rescaled.fe.NGA <- ggplot(
  data = dle.aligned.sampled.data.rescaled %>% filter(variable=="Final Energy|Residential and Commercial",
                                   iso=="NGA"
  ) %>%
    filter(fe.scaled.to.dle<=10) %>% mutate(model="model-average") %>%
    rename.models.and.scenarios(),
  mapping = aes(x=fe.scaled.to.dle)
) +
  facet_grid(scenario~.) +
  geom_vline(xintercept = 1, linewidth=0.7, linetype = "dashed") +
  geom_hline(yintercept = 0, linewidth=0.1, colour="darkgrey", linetype = "solid") +
  geom_histogram(
    data =. %>% filter(year==2040),
    aes(fill = ifelse(fe.scaled.to.dle < 1, "TRUE", scenario),
        weight = pop_mil),
    # position = "identity",
    alpha=0.8,
    binwidth=0.2,
    boundary=0
  ) +
  geom_histogram(
    data=. %>% filter(year==2020),
    color="black",
    aes(weight = pop_mil,
        y = ifelse(
          after_stat(count) > 0, after_stat(count), NA
        )
        ),
    alpha=0,
    binwidth=0.2,
    boundary=0
  ) +
  scale_fill_manual(values = c("TRUE" = "red", shape.color.coding)) +
  scale_x_continuous(limits = c(0,10), expand=c(0,0),
                     breaks = c(0,1,2,4,6,8,10)) +
  scale_y_continuous(name = NULL) +
  labs(
    title = "Nigeria",
    x = NULL
  ) +
  theme_void() +
  theme_hc() +
  theme(
    panel.spacing.y=unit(0.5,"mm"),
    plot.margin=unit(c(2,5,2,5),"mm"),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    # remove lines
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )




##### Combine plots ------------------------------------------------------------
p.distribution.vs.DLE <-
  (p.dle.rescaled.fe.global +
     p.dle.rescaled.fe.USA +
     p.dle.rescaled.fe.CHN +
     p.dle.rescaled.fe.IND +
     p.dle.rescaled.fe.NGA
   ) +
  plot_layout(
    design = "
    AB
    AC
    AD
    AE
    "
  ) + plot_annotation(title = "Residential and Commercial sector:\nEnergy expressed in multiples of the per-capita energy requirement for providing Decent Living Standards")

##### Remove large ggplot object -----------------------------------------------
rm(p.dle.rescaled.fe.global)
gc()

##### Save combined plot -------------------------------------------------------
save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, paste0("f05") ),
  p = p.distribution.vs.DLE + plot_annotation(tag_levels = "A"),
  w = 300,
  h = 300
)

##### Remove large ggplot object -----------------------------------------------
rm(p.distribution.vs.DLE)
gc()


# . ----------------------------------------------------------------------------
# Figure 6: Energy Needs Gap ---------------------------------------------

### Auxiliary functions --------------------------------------------------------
enterise_long_r10_names <- function(df, n.enters = 1){
  # pop.r10.projection %>% distinct(r10)

  if (n.enters == 1){
    return(
      df %>%
        mutate_cond(r10=="Latin America and Caribbean", r10 = "Latin America\nand Caribbean") %>%
        mutate_cond(r10=="Eastern Europe and West-Central Asia", r10 = "Eastern Europe\nand West-Central\nAsia") %>%
        mutate_cond(r10=="South-East Asia and developing Pacific", r10 = "South-East Asia\nand developing\nPacific") %>%
        mutate_cond(r10=="Asia-Pacific Developed", r10 = "Asia-Pacific\nDeveloped")
    )
  }

}


### Data preparation -----------------------------------------------------------

##### Main [A+B+C]: all indicators, regional averages, ENG ---------------------

# calculate data for each models, scenarios, sectors (R10)
p.EDG.data <- df.core %>%
  left_join(ipcc.r10) %>%
  left_join(population.iso %>% select(-variable,-unit)) %>%
  reframe(

    energy.development.gap = sum(share.below.projected.adjusted*depth.below.projected.adjusted*pop_mil)*giga*mega/exa,
    total.energy.use = sum(energy.per.capita.uncorrected*pop_mil)*giga*mega/exa,
    dle.threshold = sum(dle.threshold.adjusted*pop_mil)*giga*mega/exa,

    .by = c("model", "scenario", "variable", "r10", "year")
  )

# summarise regional data, take across models (R10)
p.EDG.summedsectors.minmaxavg.data <- p.EDG.data %>%

  # sum over sectors
  reframe(
    energy.development.gap = sum(energy.development.gap),
    total.energy.use = sum(total.energy.use),
    dle.threshold = sum(dle.threshold),

    .by = c("model","scenario","r10","year")
  ) %>%
  # min.max.avg across models
  reframe(
    energy.development.gap.min = min(energy.development.gap),
    energy.development.gap.max = max(energy.development.gap),
    energy.development.gap.avg = mean(energy.development.gap),

    total.energy.use.min = min(total.energy.use),
    total.energy.use.max = max(total.energy.use),
    total.energy.use.avg = mean(total.energy.use),

    dle.threshold.min = min(dle.threshold),
    dle.threshold.max = max(dle.threshold),
    dle.threshold.avg = mean(dle.threshold),

    energy.gap.ratio.min = min(energy.development.gap/total.energy.use),
    energy.gap.ratio.max = max(energy.development.gap/total.energy.use),
    energy.gap.ratio.avg = mean(energy.development.gap/total.energy.use),

    dle.energy.min = min(dle.threshold-energy.development.gap),
    dle.energy.max = max(dle.threshold-energy.development.gap),
    dle.energy.avg = mean(dle.threshold-energy.development.gap),

    abovedle.minus.energygap.min = min(total.energy.use - dle.threshold),
    abovedle.minus.energygap.max = max(total.energy.use - dle.threshold),
    abovedle.minus.energygap.avg = mean(total.energy.use - dle.threshold),

    .by = c("scenario","r10","year")
  ) %>%
  ungroup()


### Main [A] closing regional energy development gaps --------------------------
p.EDG.summedsectors.minmaxavg.data$RCB.threshold <- factor(p.EDG.summedsectors.minmaxavg.data$scenario,
                                                           levels = c("SSP2-NPi",
                                                                      "SSP2-1p5C",
                                                                      "SDP_EI-1p5C",
                                                                      "SDP_MC-1p5C",
                                                                      "SDP_RC-1p5C"))
p.EDG.all.relevant.lines <- ggplot(
  data = p.EDG.summedsectors.minmaxavg.data %>%
    # iamc_variable_keep_one_level(level = -1) %>%
    filter(year %in% c(2020,2030,2040),
           scenario %in% c("SDP_RC-1p5C",
                           # "SSP2-1p5C",
                           "SSP2-NPi"),
    ) %>%
    left_join(pop.r10.projection) %>%
    enterise_long_r10_names() %>% rename.models.and.scenarios,
  mapping = aes(x=year)
) +
  facet_grid(scenario~r10, scales = "free_y") +

  geom_line(aes(y=total.energy.use.avg/pop_mil*exa/giga/mega,
                color = scenario,
                linetype = "Total energy use"),
            linewidth = 1.5) +

  geom_line(aes(y=dle.threshold.avg/pop_mil*exa/giga/mega,
                color = scenario,
                linetype = "DLE threshold"),
            # linetype = "dashed",
            linewidth = 1) +

  geom_area(aes(y=(dle.threshold.avg-energy.development.gap.avg)/pop_mil*exa/giga/mega,
                fill = scenario),
            alpha = 0.5) +

  # energy headroom
  # 2020:
  geom_segment(data = . %>% filter(year==2020),
               aes(x=2020+1,xend=2020+1,
                   y = (total.energy.use.avg)/pop_mil*exa/giga/mega,
                   yend = (dle.threshold.avg)/pop_mil*exa/giga/mega
               ),
             colour = "pink") +
  geom_point(data = . %>% filter(year==2020),
             aes(x=2020+1,y=(total.energy.use.avg)/pop_mil*exa/giga/mega ),
             colour = "pink") +
  geom_point(data = . %>% filter(year==2020),
             aes(x=2020+1,y=(dle.threshold.avg)/pop_mil*exa/giga/mega ),
             colour = "pink") +

  # 2040:
  geom_segment(data = . %>% filter(year==2040),
               aes(x=2040-1,xend=2040-1,
                   y = (total.energy.use.avg)/pop_mil*exa/giga/mega,
                   yend = (dle.threshold.avg)/pop_mil*exa/giga/mega
               ),
               colour = "pink") +
  geom_point(data = . %>% filter(year==2040),
             aes(x=2040-1,y=(total.energy.use.avg)/pop_mil*exa/giga/mega ),
             colour = "pink") +
  geom_point(data = . %>% filter(year==2040),
             aes(x=2040-1,y=(dle.threshold.avg)/pop_mil*exa/giga/mega ),
             colour = "pink") +


  # energy gap
  # 2020:
  geom_segment(data = . %>% filter(year==2020),
               aes(x=2020,xend=2020,
                   y = (dle.threshold.avg)/pop_mil*exa/giga/mega,
                   yend = (dle.threshold.avg-energy.development.gap.avg)/pop_mil*exa/giga/mega
               ),
               colour = "red") +
  geom_point(data = . %>% filter(year==2020),
             aes(x=2020,y=(dle.threshold.avg)/pop_mil*exa/giga/mega ),
             colour = "red") +
  geom_point(data = . %>% filter(year==2020),
             aes(x=2020,y=(dle.threshold.avg-energy.development.gap.avg)/pop_mil*exa/giga/mega ),
             colour = "red") +

  # 2040:
  geom_segment(data = . %>% filter(year==2040),
               aes(x=2040,xend=2040,
                   y = (dle.threshold.avg)/pop_mil*exa/giga/mega,
                   yend = (dle.threshold.avg-energy.development.gap.avg)/pop_mil*exa/giga/mega
               ),
               colour = "red") +
  geom_point(data = . %>% filter(year==2040),
             aes(x=2040,y=(dle.threshold.avg)/pop_mil*exa/giga/mega ),
             colour = "red") +
  geom_point(data = . %>% filter(year==2040),
             aes(x=2040,y=(dle.threshold.avg-energy.development.gap.avg)/pop_mil*exa/giga/mega ),
             colour = "red") +



  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +

  scale_linetype_manual("",
                        breaks = c("Total energy use","DLE threshold"),
                        values = c("solid", "dashed")) +

  scale_y_continuous(name = "GJ/cap/year") +
  xlab(NULL) +

  labs(subtitle = bquote("The"~bold("energy needs gap")~"and"~bold("energy headroom")~"per capita, across scenarios"),
       # caption = "Energy needs gap: red vertical lines, in 2020 and 2040\nEnergy above DLS: pink vertical lines, in 2020 and 2040"
       ) +

  theme_classic() +
  theme_hc() +
  theme(#ggside.panel.scale = .3,
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y.right = element_text(angle = 0))

p.EDG.all.relevant.lines



### Main [B] sectoral energy development gaps ----------------------------------

p.EDG.sectors.models <- ggplot(
  data = p.EDG.data %>%
    filter(year %in% c(2020,2030,2040),
           scenario %in% c("SDP_RC-1p5C")
    ) %>%
    # left_join(pop.r10.projection) %>%
    enterise_long_r10_names() %>% rename.models.and.scenarios() %>% iamc_variable_keep_one_level(level = -1),
  mapping = aes(x = year, y = energy.development.gap, fill = variable, group = r10)
) +
  geom_col() +
  facet_grid(model~r10) +


  scale_y_continuous(name = "EJ/year") +
  xlab(NULL) +

  labs(subtitle = bquote("Closing of the total"~bold("energy needs gap")~"across sectors:"~bold("SDP-RC scenario"))) +

  theme_classic() +
  theme_hc() +
  scale_fill_ptol(name = NULL) +
  theme(#ggside.panel.scale = .3,
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y.right = element_text(angle = 0))
p.EDG.sectors.models



### Main [C] energy headroom ---------------------------------------------------

p.above.minus.suff <- ggplot(
  data = p.EDG.summedsectors.minmaxavg.data %>%
    # iamc_variable_keep_one_level(level = -1) %>%
    filter(year %in% c(2020,2025,2030,2035,2040)) %>%
    left_join(pop.r10.projection) %>%
    enterise_long_r10_names() %>% rename.models.and.scenarios,
  mapping = aes(x=year)
) +
  facet_grid(.~r10, scales = "free_y") +

  geom_line(aes(y=abovedle.minus.energygap.avg/pop_mil*exa/giga/mega,
                color = scenario),
            linewidth = 2) +

  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +

  scale_y_continuous(name = "GJ/cap/yr") +
  xlab(NULL) +

  labs(subtitle = bquote(bold("Energy headroom")~"per capita = average energy use per capita - DLE threshold")
       # caption = "Lines are averages between IMAGE and REMIND, ribbons are their range."
       ) +

  theme_classic() +
  theme_hc() +
  theme(#ggside.panel.scale = .3,
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y.right = element_text(angle = 0))

p.above.minus.suff


### Main [A+B+C] -----------------------------------------------------------------

p.EDG.combined <- p.EDG.all.relevant.lines / p.EDG.sectors.models / p.above.minus.suff +
  plot_layout(heights = c(2,1.5,1.5)) +
  plot_annotation(tag_levels = "A")


save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, paste0("f06") ),
  p = p.EDG.combined,
  w = 350,
  h = 350
)








# . ----------------------------------------------------------------------------
# Figure 7: Emissions ----------------------------------------------------------


### Data preparation -----------------------------------------------------------


##### [A+B+C+D] Load downscaled file with emisisons --------------------------------------
downscaled.default.data.emissions <- downscaled.default.data %>%
  filter(VARIABLE %in% c(
    "Emissions|CO2",

    "Emissions|CO2|Energy",
    "Emissions|CO2|Industrial Processes",
    "Emissions|CO2|Other Removal",

    "Emissions|CO2|LULUCF Direct+Indirect",
    "Emissions|CO2|LULUCF Indirect",

    "Emissions|Kyoto Gases (incl. indirect AFOLU)"
  )) %>%
  allcaps_to_lower() %>%
  iamc_wide_to_long() %>%
  filter(scenario %in% shape.core.scenarios) %>%
  mutate(unit = "Mt/yr") %>% pivot_wider(names_from = variable, values_from = value) %>%
  rename(iso=region)


##### [A+B+C+D] DLS emissions through DLE ------------------------------------------------
df.core.total.energy <- df.core %>%
  select(
    model, scenario, iso, variable, unit, year,
    dle.threshold.adjusted,
    share.below.projected.adjusted, depth.below.projected.adjusted,
    energy.per.capita.uncorrected,
    pop_mil
  ) %>%
  reframe(
    dle.threshold.per.capita = sum(dle.threshold.adjusted),
    energy.development.gap.per.capita = sum(share.below.projected.adjusted*depth.below.projected.adjusted),
    final.energy.per.capita = sum(energy.per.capita.uncorrected),
    .by = c("model", "scenario", "iso", "unit", "year", "pop_mil")
  ) %>%
  mutate(
    dle.threshold = dle.threshold.per.capita * pop_mil * giga / exa * 1e6,
    energy.development.gap = energy.development.gap.per.capita * pop_mil * giga / exa * 1e6,
    total.final.energy = final.energy.per.capita * pop_mil * giga / exa * 1e6
  ) %>%
  mutate(unit = "EJ/yr") %>%
  select(-final.energy.per.capita, -pop_mil)

# use total co2 (but exclude all lulucf)
df.core.total.energy.emissions <- df.core.total.energy %>%
  left_join(downscaled.default.data.emissions %>% select(-unit)) %>%
  mutate(
    co2.intensity = (`Emissions|CO2` - `Emissions|CO2|LULUCF Direct+Indirect` ) / `total.final.energy`,
    ghg.intensity = (`Emissions|Kyoto Gases (incl. indirect AFOLU)` - `Emissions|CO2|LULUCF Direct+Indirect` ) / `total.final.energy`,
    unit = "Mt/EJ"
  ) %>%

  mutate(
    dls.co2.emissions.threshold = dle.threshold * co2.intensity * mega / giga,
    dls.ghg.emissions.threshold = dle.threshold * ghg.intensity * mega / giga,
    dls.co2.emissions.gap = energy.development.gap * co2.intensity * mega / giga,
    dls.ghg.emissions.gap = energy.development.gap * ghg.intensity * mega / giga,
    co2.emissions = total.final.energy * co2.intensity * mega / giga, # for checking
    ghg.emissions = total.final.energy * ghg.intensity * mega / giga, # for checking
  )
df.core.total.energy.emissions.meanacrossmodels <- df.core.total.energy.emissions %>%
  reframe(
    dls.co2.emissions.threshold = mean(dls.co2.emissions.threshold),
    dls.ghg.emissions.threshold = mean(dls.ghg.emissions.threshold),
    dls.co2.emissions.gap = mean(dls.co2.emissions.gap),
    dls.ghg.emissions.gap = mean(dls.ghg.emissions.gap),
    co2.emissions = mean(co2.emissions),
    ghg.emissions = mean(ghg.emissions),
    .by = c("scenario", "iso", "year")
  ) %>% mutate(model="model-average")


df.core.total.energy.emissions.meanacrossmodels.percapita <- df.core.total.energy.emissions.meanacrossmodels %>%
  left_join(population.iso %>% filter(model==vis.model) %>% select(scenario,iso,year,pop_mil)) %>%
  mutate(
    dls.co2.emissions.threshold = dls.co2.emissions.threshold/pop_mil/1e6*giga,
    dls.ghg.emissions.threshold = dls.ghg.emissions.threshold/pop_mil/1e6*giga,
    dls.co2.emissions.gap = dls.co2.emissions.gap/pop_mil/1e6*giga,
    dls.ghg.emissions.gap = dls.ghg.emissions.gap/pop_mil/1e6*giga,
    co2.emissions = co2.emissions/pop_mil/1e6*giga,
    ghg.emissions = ghg.emissions/pop_mil/1e6*giga
  )

##### [B+C] Cumulative emissions for DLS ---------------------------------------------

library(pracma)
emissions.data.extract.cumulative <- df.core.total.energy.emissions %>%
  group_by(model, scenario, iso) %>%
  arrange(model,scenario,iso,year) %>%
  mutate(
    cumulative.dls.co2.emissions.threshold = cumtrapz(
      y=dls.co2.emissions.threshold,
      x=year
    ),
    cumulative.dls.co2.emissions.gap = cumtrapz(
      y=dls.co2.emissions.gap,
      x=year
    ),
    cumulative.co2.emissions = cumtrapz(
      y=co2.emissions,
      x=year
    )
  ) %>% ungroup()

emissions.data.extract.2024 <- df.core.total.energy.emissions %>%
  filter(year %in% c(2020, 2025)) %>%
  group_by(model, scenario, iso) %>%
  arrange(model,scenario,iso,year) %>%
  reframe(
    dls.co2.emissions.threshold = dls.co2.emissions.threshold*0.2+lead(dls.co2.emissions.threshold)*0.8,
    dls.co2.emissions.gap = dls.co2.emissions.gap*0.2+lead(dls.co2.emissions.gap)*0.8,
    co2.emissions = co2.emissions*0.2+lead(co2.emissions)*0.8,
  ) %>% drop_na() %>% # drop the empty rows (as we used both 2020 and 2025 to create one number for 2024)
  mutate(year=2024)

emissions.data.extract.cumulative.from.2024 <- df.core.total.energy.emissions %>%
  bind_rows(emissions.data.extract.2024) %>%
  group_by(model, scenario, iso) %>%
  arrange(model,scenario,iso,year) %>%
  mutate(
    cumulative.dls.co2.emissions.threshold = cumtrapz(
      y=dls.co2.emissions.threshold,
      x=year
    ),
    cumulative.dls.co2.emissions.gap = cumtrapz(
      y=dls.co2.emissions.gap,
      x=year
    ),
    cumulative.co2.emissions = cumtrapz(
      y=co2.emissions,
      x=year
    )
  ) %>% ungroup()





##### [B] DLS treemap ----------------------------------------------------------
create_treemap_data <- function(emissions.data.cumulative.from.2024,
                                scen="SDP_RC-1p5C",
                                end.year=2050){


  treemap.dls.data.prep <- emissions.data.extract.cumulative.from.2024 %>%
    filter(
      year==end.year,
      scenario==scen
    ) %>%
    select(model,iso,
           cumulative.dls.co2.emissions.threshold) %>%
    reframe(
      cumulative.dls.co2.emissions.threshold.mean = mean(cumulative.dls.co2.emissions.threshold),
      cumulative.dls.co2.emissions.threshold.min = min(cumulative.dls.co2.emissions.threshold),
      cumulative.dls.co2.emissions.threshold.max = max(cumulative.dls.co2.emissions.threshold),
      .by = c("iso")
    ) %>%
    left_join(load_official_country_grouping(grouping.to.load = "region_ar6_10_ipcc_fgd",
                                             keep.long.names=T)) %>%
    left_join(load_official_country_grouping(grouping.to.load = "Developing_2021_M49_other",
                                             keep.long.names=F)) %>%
    mutate_cond(
      Developing_2021_M49_other=="Developing",
      Developing_2021_M49_other="Global South"
    ) %>%
    mutate_cond(
      Developing_2021_M49_other=="Developed",
      Developing_2021_M49_other="Global North"
    )

  treemap.dls.data.prep.global <- treemap.dls.data.prep %>% reframe(
    global.cumulative.dls.co2.emissions.threshold.mean = sum(cumulative.dls.co2.emissions.threshold.mean),
    global.cumulative.dls.co2.emissions.threshold.min = sum(cumulative.dls.co2.emissions.threshold.min),
    global.cumulative.dls.co2.emissions.threshold.max = sum(cumulative.dls.co2.emissions.threshold.max)
  )
  treemap.dls.data.prep.developed.developing <- treemap.dls.data.prep %>% reframe(
    regional.cumulative.dls.co2.emissions.threshold.mean = sum(cumulative.dls.co2.emissions.threshold.mean),
    regional.cumulative.dls.co2.emissions.threshold.min = sum(cumulative.dls.co2.emissions.threshold.min),
    regional.cumulative.dls.co2.emissions.threshold.max = sum(cumulative.dls.co2.emissions.threshold.max),
    .by = c("Developing_2021_M49_other")
  ) %>% cross_join(
    treemap.dls.data.prep.global
  ) %>%
    mutate(
      developed.developing.share.mean = paste0(Developing_2021_M49_other, " (", round(regional.cumulative.dls.co2.emissions.threshold.mean / global.cumulative.dls.co2.emissions.threshold.mean * 100, 0), "%)"),
      developed.developing.share.min = paste0(Developing_2021_M49_other, " (", round(regional.cumulative.dls.co2.emissions.threshold.min / global.cumulative.dls.co2.emissions.threshold.min * 100, 0), "%)"),
      developed.developing.share.max = paste0(Developing_2021_M49_other, " (", round(regional.cumulative.dls.co2.emissions.threshold.max / global.cumulative.dls.co2.emissions.threshold.max * 100, 0), "%)")
    )
  treemap.dls.data <- treemap.dls.data.prep %>% cross_join(
    treemap.dls.data.prep.global
  ) %>%
    mutate(
      name.perc = ifelse(cumulative.dls.co2.emissions.threshold.mean / global.cumulative.dls.co2.emissions.threshold.mean * 100 > 1,
                         ifelse(cumulative.dls.co2.emissions.threshold.mean / global.cumulative.dls.co2.emissions.threshold.mean * 100 > 10,
                                paste0(name, "\n(", round(cumulative.dls.co2.emissions.threshold.mean / global.cumulative.dls.co2.emissions.threshold.mean * 100, 0), "%)"),
                                paste0(name, "\n(", round(cumulative.dls.co2.emissions.threshold.mean / global.cumulative.dls.co2.emissions.threshold.mean * 100, 1), "%)")),
                         name)
    ) %>%
    left_join(
      treemap.dls.data.prep.developed.developing
    )

  return(treemap.dls.data)
}


treemap.dls.data <- create_treemap_data(
  emissions.data.cumulative.from.2024 = emissions.data.extract.cumulative.from.2024,
  scen="SDP_RC-1p5C",
  end.year=2050)



##### [C] DLS vs RCB -----------------------------

# # RCB
# # from Lamboll et al. 2023, Nat Clim Change
# # 1.5, 66%:
# RCB.from.2023.1p5c.p66 <- 60
# # 1.5, 50%:
# RCB.from.2020.1p5c <- 247 + 121 # recent emissions taken from text, confirmed with Robin over whatsapp
# RCB.from.2023.1p5c <- 247
# # 1.5, 17%:
# RCB.from.2023.1p5c.p10 <- 838
# # 1.5, 10%:
# RCB.from.2023.1p5c.p10 <- 1163
#
# # 2.0, 90%:
# RCB.from.2023.2c.p90 <- 504
# # 2.0, 66%:
# RCB.from.2023.2c.p66 <- 944
# # 2.0, 50%:
# RCB.from.2023.2c <- 1219
#
# RCB.data <- tibble(
#   RCB.threshold = c("1.5C (10%)",
#                     "1.5C (17%)",
#                     "1.5C (50%)",
#                     "1.5C (66%)",
#                     "2C (50%)",
#                     "2C (66%)",
#                     "2C (90%)"),
#   RCB = c(1163,
#           838,
#           247,
#           60,
#           1219,
#           944,
#           504)
# )

# RCB
# from Forster et al. 2024, ESSD: https://essd.copernicus.org/articles/16/2625/2024/#section8&gid=1&pid=1
# 1.5, 67%:
RCB.from.2024.1p5c.p67 <- 150
# 1.5, 50%:
RCB.from.2024.1p5c <- 200
# 1.5, 33%:
RCB.from.2024.1p5c.p33 <- 300
# 1.5, 17%:
RCB.from.2024.1p5c.p17 <- 450

# 2.0, 83%:
RCB.from.2024.2c.p83 <- 750
# 2.0, 67%:
RCB.from.2024.2c.p67 <- 900
# 2.0, 50%:
RCB.from.2024.2c <- 1100

RCB.data <- tibble(
  RCB.threshold = c("1.5C (17%)",
                    "1.5C (33%)",
                    "1.5C (50%)",
                    "1.5C (67%)",
                    "2C (50%)",
                    "2C (67%)",
                    "2C (83%)"),
  RCB = c(450,
          300,
          200,
          150,
          1100,
          900,
          750)
)


cumulative.emissions.until.2050 <- emissions.data.extract.cumulative.from.2024 %>%
  filter(year %in% c(2050)) %>%
  ungroup() %>%
  reframe(
    cumulative.dls.co2.emissions.until2050.all = sum(cumulative.dls.co2.emissions.threshold),
    .by = c("model", "scenario")
  )


RCB.vs.cumulativeDLS.data <- RCB.data %>%
  cross_join(cumulative.emissions.until.2050 %>% reframe(
    cumulative.dls.co2.emissions.until2050.all.mean = mean(cumulative.dls.co2.emissions.until2050.all),
    cumulative.dls.co2.emissions.until2050.all.min = min(cumulative.dls.co2.emissions.until2050.all),
    cumulative.dls.co2.emissions.until2050.all.max = max(cumulative.dls.co2.emissions.until2050.all),
    .by = "scenario"
  ))

RCB.vs.cumulativeDLS.data$RCB.threshold <- factor(RCB.vs.cumulativeDLS.data$RCB.threshold,
                                                  levels = c("2C (50%)",
                                                             "2C (67%)",
                                                             "2C (83%)",
                                                             "1.5C (17%)",
                                                             "1.5C (33%)",
                                                             "1.5C (50%)",
                                                             "1.5C (67%)"))


### Main [A]: emissions for DLS vs total emissions -----------------------------
p.emissions.timseries.1sdp <- ggplot(df.core.total.energy.emissions.meanacrossmodels.percapita %>% left_join(load_long_names_of_iso3c()) %>% ms_add() %>%
                                       filter(scenario %in% c("SDP_RC-1p5C", "SSP2-NPi"),
                                              # iso %in% c("USA", "IND", "CHN", "NGA", "IDN"),
                                              # iso %in% c("USA", "CHN"),
                                              iso %in% c("ETH", "USA"),
                                              year>=2020,
                                              year!=2055, # only reported by 1 model
                                              year<=2050) %>% rename.models.and.scenarios,
                                     aes(group=`model-scenario`)) +
  facet_wrap(.~name, scales = "free_y") +
  geom_hline(aes(yintercept=0),
             color = "darkgrey") +
  geom_line(aes(x=year,
                y=dls.co2.emissions.threshold,
                color = scenario),
            linetype = "dashed",
            linewidth = 1) +
  labs(subtitle = "CO2 emissions (excl. LULUCF) related to DLE threshold (DLS for all)") +
  ylab("tCO2/cap/year") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  scale_colour_manual("Scenario", values=shape.color.coding) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")
p.emissions.timseries.1sdp


### Main [B]: shares of emissions for DLS (threshold) until 2050 ---------------
library(treemapify)
p.treemap.DLS.emissions.until.2050 <- ggplot(treemap.dls.data,
                                             aes(area = cumulative.dls.co2.emissions.threshold.mean,
                                                 fill = region_ar6_10_ipcc_fgd,
                                                 label = name.perc,
                                                 subgroup = developed.developing.share.mean,
                                                 subgroup2 = region_ar6_10_ipcc_fgd)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "black", size = 5) +
  geom_treemap_subgroup_text(place = "bottomright", grow = F, alpha = 0.5,
                             colour = "white", fontface = "italic",
                             min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  # scale_fill_manual(
  #   "Region (IPCC)", values=ipcc.r10.color.coding
  # ) +
  scale_fill_bluebrown() +
  # theme(legend.position = "bottom",
  #       # legend.title = element_blank()
  #       ) +
  theme(legend.position = "none") +
  labs(
    subtitle = "Share of cumulative Decent Living Emissions until 2050",
    caption = "Scenario SDP-RC, 2024-2050, average emissions for DLS for all across models, with population growth following SSP1.\nColouring according to IPCC R10 region (AR6). North/South split following UN developing/developed classification."
  )


p.treemap.DLS.emissions.until.2050


### Main [C]: DLS vs RCB -------------------------------------------------------

p.RCB.comparison <- ggplot(
  RCB.vs.cumulativeDLS.data %>% filter(scenario %in% c("SDP_RC-1p5C", "SSP2-NPi"),
                                       RCB.threshold %nin% c("1.5C (17%)", "2C (50%)", "1.5C (33%)")) %>% mutate(model="model-average") %>% rename.models.and.scenarios,
) +
  facet_grid(scenario~RCB.threshold) +

  geom_rect(aes(xmin=-sqrt(cumulative.dls.co2.emissions.until2050.all.mean),
                xmax=0,
                ymin=0,
                ymax=sqrt(cumulative.dls.co2.emissions.until2050.all.mean),
                fill=scenario),
            alpha = 0.3) +
  geom_text(aes(x=-sqrt(cumulative.dls.co2.emissions.until2050.all.mean)+0.1*sqrt(cumulative.dls.co2.emissions.until2050.all.mean),
                y=sqrt(sqrt(cumulative.dls.co2.emissions.until2050.all.mean)),
                label=paste0(as.character(round(cumulative.dls.co2.emissions.until2050.all.mean/RCB, 2)*100), "%")   ),
            hjust = 0,
            vjust = 1,
            size = 3) +

  geom_rect(aes(xmin=-sqrt(RCB),
                xmax=0,
                ymin=0,
                ymax=sqrt(RCB)),
            color = "black",
            fill = "white",
            linetype = "dotted",
            alpha = 0) +

  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual("Scenario", values=shape.color.coding) +
  scale_colour_manual("Scenario", values=shape.color.coding) +
  labs(subtitle = "Cumulative CO2 Emissions (excl. LULUCF) until 2050 for DLS for all compared to Remaining Carbon Budgets\n")
p.RCB.comparison



### Main [A+B+C] -------------------------------------------------------------
p.emissions <- (
  p.emissions.timseries.1sdp +
    p.treemap.DLS.emissions.until.2050 +
    p.RCB.comparison
) +
  plot_annotation(
    title = "Emissions implied in delivering Decent Living Energy",
    tag_levels = "A"
  ) +
  plot_layout(
    design = "
    AAAA
    BBBB
    BBBB
    CCCC
    CCCC
    "
  )


save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, paste0("f07") ),
  p = p.emissions,
  w = 300,
  h = 350
)



# . ----------------------------------------------------------------------------
# . ----------------------------------------------------------------------------
# . ----------------------------------------------------------------------------
# TABLES IN MANUSCRIPT ---------------------------------------------------------

# . ----------------------------------------------------------------------------
# Table 1: Aggregate Energy Needs Gaps: bottom-up vs top-down ------------------


# top-down analysis, all:
global.gaps.desire <- df.core %>%
  reframe(

    energy.development.gap.global = sum(share.below.projected.adjusted*depth.below.projected.adjusted*pop_mil)*giga*mega/exa,
    total.energy.use.global = sum(energy.per.capita.uncorrected*pop_mil)*giga*mega/exa,
    dle.threshold.global = sum(dle.threshold.adjusted*pop_mil)*giga*mega/exa,

    dle.threshold.percapita.global = weighted.mean(dle.threshold.adjusted, pop_mil),

    .by = c("model", "scenario", "variable", "year")
  ) %>%
  filter(
    year==2020,
    scenario=="SDP_RC-1p5C"
  ) %>% arrange(desc(energy.development.gap.global))
# top-down analysis, totals (rows):
global.gaps.desire.rows <- global.gaps.desire %>%
  reframe(

    energy.development.gap.global = sum(energy.development.gap.global),
    total.energy.use.global = sum(total.energy.use.global),
    dle.threshold.global = sum(dle.threshold.global),

    dle.threshold.percapita.global = sum(dle.threshold.percapita.global),

    .by = c("model", "scenario", "year")
  ) %>%
  rename(
    energy.development.gap = energy.development.gap.global,
    total.energy.use = total.energy.use.global,
    dle.threshold = dle.threshold.global,

    dle.threshold.percapita = dle.threshold.percapita.global
  )
# top-down analysis, totals (columns):
global.gaps.desire.cols <- global.gaps.desire  %>%
  reframe(

    energy.development.gap.global = sum(energy.development.gap.global),
    total.energy.use.global = sum(total.energy.use.global),
    dle.threshold.global = sum(dle.threshold.global),

    dle.threshold.percapita.global = sum(dle.threshold.percapita.global),

    .by = c("model", "scenario", "year")
  ) %>%
  select(model,energy.development.gap.global)




# top-down analysis, by sector:
table.1.globalaverages <- df.core %>%
  reframe(

    energy.development.gap = sum(share.below.projected.adjusted*depth.below.projected.adjusted*pop_mil)*giga*mega/exa,
    total.energy.use = sum(energy.per.capita.uncorrected*pop_mil)*giga*mega/exa,
    dle.threshold = sum(dle.threshold.adjusted*pop_mil)*giga*mega/exa,

    dle.threshold.percapita = weighted.mean(dle.threshold.adjusted, pop_mil),

    .by = c("model", "scenario", "variable", "year")
  ) %>%
  filter(
    year==2020,
    scenario=="SDP_RC-1p5C"
  ) %>% arrange(desc(energy.development.gap)) %>%

  # calculate sectoral shares (and add totals):
  left_join(
    global.gaps.desire.cols
  ) %>% mutate(
    energy.development.gap.sector.share = energy.development.gap/energy.development.gap.global
  ) %>%
  # add totals:
  bind_rows(
    global.gaps.desire.rows %>% mutate(variable="Total")
  ) %>%


  select(model,
         variable,
         energy.development.gap,
         energy.development.gap.sector.share,
         total.energy.use,
         dle.threshold,
         dle.threshold.percapita
  )
# thresholds in starting year are the same per model
table.1.across.countries.threshold <- df.core %>%
  select(model,scenario,iso,variable,year,dle.threshold.adjusted) %>%
  rename(value = dle.threshold.adjusted) %>%
  add_percentile_columns(only.keep.percentiles = T,
                         percentiles = c(0.05,0.50,0.95)) %>%
  filter(
    year==2020,
    scenario=="SDP_RC-1p5C"
  ) %>%
  select(model,
         variable,
         p5,p50,p95
  )
table.1.across.countries.threshold.totals <- df.core %>%
  select(model,scenario,iso,variable,year,dle.threshold.adjusted) %>%
  reframe(dle.threshold.adjusted=sum(dle.threshold.adjusted),
          .by = c(model,scenario,iso,year)) %>%
  rename(value = dle.threshold.adjusted) %>%
  add_percentile_columns(only.keep.percentiles = T,
                         group.cols = c("model", "scenario", "year"),
                         percentiles = c(0.05,0.50,0.95)) %>%
  filter(
    year==2020,
    scenario=="SDP_RC-1p5C"
  ) %>%
  mutate(variable="Total") %>%
  select(model,
         variable,
         p5,p50,p95
  )


table.1 <- table.1.globalaverages %>% left_join(
  bind_rows(
    table.1.across.countries.threshold,
    table.1.across.countries.threshold.totals
  )
)

write_delim(
  table.1,
  file = here("analyses", "figures", data.version, figures.version, paste0("t01-energyneedgaps-desire.csv") ),
  delim = ","
)

## add total global energy threshold ranges too
p.dle.data %>% filter(year%in%c(2020),
                      scenario=="SDP_RC-1p5C") %>%
  filter(
    # pop_mil>5
  ) %>%
  select(-pop_mil) %>%

  rename(value=dle) %>%
  add_percentile_columns(group.cols = c("year", "scenario"),
                         percentiles = c(0.05,0.95)) %>%
  distinct(
    year,
    scenario,
    p5,p95
  )



# . ----------------------------------------------------------------------------
# Table 2: Sectoral Energy Needs Gaps: bottom-up only --------------------------

BOTTOM.UP.DLE.GAP.FILE <- "DLE_gaps.csv"

# bottom-up analysis:
dle_gap_all.global <- read_csv(file.path(PATH.data.desire, "other", "dle-gaps", BOTTOM.UP.DLE.GAP.FILE)) %>%
  left_join(
    population.iso.2020.base %>% rename(pop_mil_2020=pop_mil)
  ) %>%
  mutate_cond(
    variable%in%c("Heating OP|total",
                  "Heating CON|total",
                  "Hot Water OP|total"), variable = "Space and water heating"
  ) %>%
  mutate_cond(
    variable%in%c("Cooling OP|total",
                  "Cooling CON|total"), variable = "Space cooling"
  ) %>%
  mutate_cond(
    variable%in%c("Education|lower_secondary",
                  "Education|primary"), variable = "Education"
  ) %>%

  mutate_cond(
    variable%in%c("Appliance|clean_cooking_fuel",
                  "Appliance|refrigerator",
                  "Appliance|television",
                  "Appliance|mobile_telephone"), variable = "Appliances"
  ) %>%
  reframe(
    gap = sum(gap*mega/exa * pop_mil_2020*1e6),
    .by = c("variable")
  )
dle_gap_all.global %>% pull(gap) %>% sum()
dle_gap_all.global %>% arrange(desc(gap)) %>% print(n=Inf)
dle_gap_all.global %>% cross_join(tibble(global.gap = dle_gap_all.global %>% pull(gap) %>% sum())) %>% mutate(percentage = round(gap/global.gap * 100,4) ) %>% arrange(desc(gap))



table.2 <- dle_gap_all.global %>% cross_join(tibble(global.gap = dle_gap_all.global %>% pull(gap) %>% sum())) %>% mutate(percentage = round(gap/global.gap * 100,4) ) %>% arrange(desc(gap))
write_delim(
  table.2,
  file = here("analyses", "figures", data.version, figures.version,
              paste0("t02-energyneedgaps-sectoral-bottom-up.csv") ),
  delim = ","
)

# . ----------------------------------------------------------------------------
# Table 3: Regional headroom ---------------------------------------------------
df.headroom.table.data <- df.core %>%
  reframe(
    # sum across variables/sectors
    energy.development.gap = sum(share.below.projected.adjusted*depth.below.projected.adjusted),
    total.energy.use = sum(energy.per.capita.uncorrected),
    dle.threshold = sum(dle.threshold.adjusted),

    pop_mil = first(pop_mil),

    .by = c("model", "scenario", "iso", "year")
  ) %>%

  # add r10
  left_join(ipcc.r10) %>%

  reframe(
    # global mean
    energy.development.gap = weighted.mean(energy.development.gap, pop_mil),
    total.energy.use = weighted.mean(total.energy.use, pop_mil),
    dle.threshold = weighted.mean(dle.threshold, pop_mil),

    .by = c("model", "scenario", "r10", "year")
  ) %>%

  reframe(
    # simple mean across models
    energy.development.gap = mean(energy.development.gap),
    total.energy.use = mean(total.energy.use),
    dle.threshold = mean(dle.threshold),

    .by = c("scenario", "r10", "year")
  )
df.headroom.table.data.global <- df.headroom.table.data %>%
  left_join(pop.r10.projection) %>%
  reframe(
    energy.development.gap = weighted.mean(energy.development.gap, pop_mil),
    total.energy.use = weighted.mean(total.energy.use, pop_mil),
    dle.threshold = weighted.mean(dle.threshold, pop_mil),
    .by = c("scenario", "year")
  ) %>%
  mutate(r10="World")
df.headroom.table.data <- df.headroom.table.data %>%
  bind_rows(df.headroom.table.data.global)

table.headroom.2020 <- df.headroom.table.data %>%
  filter(year==2020, scenario==vis.scenario) %>%
  mutate(
    total.energy.use = round(total.energy.use),
    dle.threshold.as.share.of.total = round(dle.threshold/total.energy.use*100),
    gap = round(energy.development.gap),
    gap.as.share.of.total.energy = round(energy.development.gap/total.energy.use*100),
    non.dls.energy = round(total.energy.use - (dle.threshold - energy.development.gap))
  )

table.headroom.2040 <- df.headroom.table.data %>%
  filter(year==2040) %>%
  mutate(
    dle.threshold.as.share.of.total = dle.threshold/total.energy.use,
    gap = energy.development.gap,
    gap.as.share.of.total.energy = energy.development.gap/total.energy.use,
    non.dls.energy = total.energy.use - (dle.threshold - energy.development.gap)
  ) %>%
  # group scenarios
  mutate(scenario=ifelse(scenario%in%shape.sdps,"SDPs",scenario)) %>%
  reframe(
    dle.threshold.as.share.of.total = ifelse(round(min(dle.threshold.as.share.of.total*100))==round(max(dle.threshold.as.share.of.total*100)),
                                             as.character(round(max(dle.threshold.as.share.of.total*100))),
                                             paste0(round(min(dle.threshold.as.share.of.total*100)),
                                                    " to ",
                                                    round(max(dle.threshold.as.share.of.total*100)))),
    gap = ifelse(round(min(gap))==round(max(gap)),
                 as.character(round(max(gap))),
                 paste0(round(min(gap)),
                        " to ",
                        round(max(gap)))),
    gap.as.share.of.total.energy = ifelse(round(min(gap.as.share.of.total.energy*100))==round(max(gap.as.share.of.total.energy*100)),
                                          as.character(round(max(gap.as.share.of.total.energy*100))),
                                          paste0(round(min(gap.as.share.of.total.energy*100)),
                                                 " to ",
                                                 round(max(gap.as.share.of.total.energy*100)))),
    range.2040 = ifelse(round(min(non.dls.energy))==round(max(non.dls.energy)),
                        as.character(round(max(non.dls.energy))),
                        paste0(round(min(non.dls.energy)),
                               " to ",
                               round(max(non.dls.energy)))),
    .by = c("scenario", "r10", "year")
  )

table.regional.dle.characteristics <-
  table.headroom.2020 %>% select(scenario,r10,year,total.energy.use,dle.threshold.as.share.of.total,gap) %>% arrange(r10) %>%
  pivot_wider(names_from = year, values_from = c(total.energy.use,dle.threshold.as.share.of.total,gap)) %>%
  left_join(
    table.headroom.2040 %>% select(scenario,r10,year,dle.threshold.as.share.of.total,gap) %>% arrange(r10) %>%
      pivot_wider(names_from = scenario, values_from = c(dle.threshold.as.share.of.total,gap))
  ) %>%
  select(
    r10,
    total.energy.use_2020,
    dle.threshold.as.share.of.total_2020, gap_2020,
    dle.threshold.as.share.of.total_SDPs, gap_SDPs,
    `dle.threshold.as.share.of.total_SSP2-1p5C`, `gap_SSP2-1p5C`,
    `dle.threshold.as.share.of.total_SSP2-NPi`, `gap_SSP2-NPi`
  )


write_delim(
  table.regional.dle.characteristics,
  file = here("analyses", "figures", data.version, figures.version,
              "t03-regional-dle-characteristics.csv" ),
  delim = ","
)


# . ----------------------------------------------------------------------------
# Table 4: Global Gini coefficients --------------------------------------------

# calculate global gini coefficients
# - write loop, using `get_gini()`
compare.global.gini <- tibble()
for (v in variable_unique(nationalaverage.data)){
  for (y in year_unique(nationalaverage.data)){
    for (s in scenario_unique(nationalaverage.data)){

      # add national
      compare.global.gini <- compare.global.gini %>%
        bind_rows(
          nationalaverage.data %>% mutate(iso="World") %>%
            rename(fe=energy.per.capita.uncorrected,
                   population=pop_mil) %>%
            filter(scenario==s,
                   year==y,
                   variable==v) %>%
            get_gini() %>%
            mutate(
              scenario = s,
              variable = v,
              year = as.character(y),
              type.estimate = "national average"
            )
        )

      # add global
      compare.global.gini <- compare.global.gini %>%
        bind_rows(
          lorenz.sampled.data %>% mutate(iso="World") %>%
            rename(fe=energy.per.capita.uncorrected,
                   population=pop_mil) %>%
            filter(scenario==s,
                   year==y,
                   variable==v) %>%
            get_gini() %>%
            mutate(
              scenario = s,
              variable = v,
              year = as.character(y),
              type.estimate = "national sampled distribution"
            )
        )

    }
  }
  # add oswald
  compare.global.gini <- compare.global.gini %>%
    bind_rows(
      gtap9 %>% mutate(iso="World") %>%
        filter(variable==v) %>%
        # rename(fe=fe,
        #        population=population) %>%
        get_gini() %>%
        mutate(
          scenario = "Historical",
          variable = v,
          year = "2011; Oswald et al. (2020, 2021)",
          type.estimate = "GTAP9 + GCD + IEA"
        )
    )
}

compare.global.gini

write_delim(
  compare.global.gini %>% pivot_wider(names_from = type.estimate, values_from = gini) %>%
    arrange(variable,year),
  file = here("analyses", "figures", data.version, figures.version,
              "t04-global-gini-energy.csv" ),
  delim = ","
)

# total final energy data:
raw.scenario.data.fe.global %>%
  filter(year==2020,
         scenario=="SDP_RC-1p5C")




# . ----------------------------------------------------------------------------
# . ----------------------------------------------------------------------------
# . ----------------------------------------------------------------------------
# NUMBERS IN MANUSCRIPT --------------------------------------------------------

# Abstract ---------------------------------------------------------------------

### number of countries --------------------------------------------------------
iso.assessed.by.desire %>% length()

### Total energy need reduction ------------------------------------------------
# percentage decrease
p.dle.data.global %>% filter(year%in%c(2020,2040)) %>%
  pivot_wider(
    names_from = year,
    values_from = dle
  ) %>% mutate(
    perc.change = (`2020` - `2040`)/ `2020`
  ) %>% arrange(-perc.change) %>%
  mutate(sdp=ifelse(grepl(scenario,pattern="SDP",fixed=T),"Yes","No")) %>%
  reframe(
    range.perc.change = paste0(round(min(`perc.change`*100),0), "-", round(max(`perc.change`*100),0)),
    .by = c("sdp")
  )
### DLE threshold: national ranges, 2020 to 2040 ---------------------------------
p.dle.data %>% filter(year%in%c(2020,2040)) %>%
  select(-pop_mil) %>%
  rename(value=dle) %>%
  mutate(sdp=ifelse(grepl(scenario,pattern="SDP",fixed=T),"Yes","No")) %>%
  add_percentile_columns(group.cols = c("year","scenario","sdp"),
                         percentiles = c(0.05,0.5,0.95)) %>%
  mutate(range = paste0(round(p5),"-",round(p95))) %>%
  distinct(
    year,
    scenario,
    sdp,
    range
  ) %>%
  pivot_wider(names_from = year, values_from = range)

### ResCom deprivation - SDPs vs SSP2-1.5C (global) ----------------------------
# GLOBAL - SDPs:
df.core %>%
  filter(
    variable=="Final Energy|Residential and Commercial",
    grepl(x=scenario, pattern="SDP", fixed=T),
    year%in%c(2020,2040)
  ) %>%
  reframe(dle.headcount = sum(share.below.projected.adjusted*pop_mil),
          .by = c("model", "scenario", "year")) %>%
  reframe(dle.headcount.mean = mean(dle.headcount),
          dle.headcount.max = max(dle.headcount),
          dle.headcount.min = min(dle.headcount),
          .by = c(#"scenario",
            "year")) %>%
  arrange(year,dle.headcount.mean) %>%
  pivot_longer(cols = c(dle.headcount.mean, dle.headcount.min, dle.headcount.max),
               names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = year, values_from = "value") %>%
  mutate(perc.reduction = (`2020`-`2040`)/`2020`*100)


### between 2-4x DLE  --------------------------------------------------------
# global
n.all <- dle.aligned.sampled.data.rescaled %>%
  filter(year==2040, facet.scenario == "SDPs") %>%
  reframe(rows = n(),
          .by = c("scenario"))
n.2ormore <- dle.aligned.sampled.data.rescaled %>%
  filter(year==2040, facet.scenario == "SDPs",
         fe.scaled.to.dle>=2) %>%
  reframe(rows.2ormore = n(),
          .by = c("scenario"))
left_join(n.all,n.2ormore) %>%
  mutate(share = rows.2ormore / rows)

# global south / world bank income groups
n.all.gs <- dle.aligned.sampled.data.rescaled %>%
  left_join(load_official_country_grouping("Income _status_WB")) %>%
  filter(year==2040, facet.scenario == "SDPs") %>%
  reframe(rows = n(),
          .by = c("scenario", "Income _status_WB"))
n.2ormore.gs <- dle.aligned.sampled.data.rescaled %>%
  left_join(load_official_country_grouping("Income _status_WB")) %>%
  filter(year==2040, facet.scenario == "SDPs",
         fe.scaled.to.dle>=2) %>%
  reframe(rows.2ormore = n(),
          .by = c("scenario", "Income _status_WB"))
left_join(n.all.gs,n.2ormore.gs) %>%
  mutate(share = rows.2ormore / rows)



### Beyond-DLS energy as share of total ----------------------------------------
df.headroom.table.data.global %>% filter(year%in%c(2020,2040)) %>%
  mutate(beyond.dls.energy.share = (total.energy.use+energy.development.gap-dle.threshold)/total.energy.use ) %>%
  filter(grepl(scenario, pattern="SDP")) %>%
  reframe(
    global.beyond.dls.energy.share =
      paste0(round(min(beyond.dls.energy.share*100)),
             " to ",
             round(max(beyond.dls.energy.share*100))),
    .by = c("year")
  )



# 1. Introduction --------------------------------------------------------------

# no numbers from this analysis quoted in the introduction

# 2. Data and Methods ----------------------------------------------------------

### Coverage of countries: population share (2020 and 2050; SDP) ---------------

# 2020 population share covered:
P.df.iso.2020 %>% mutate(in.data = ifelse(iso %in% iso.assessed.by.desire, "yes", "no")) %>%
  reframe(population = sum(pop_mil),
          population.share = sum(pop_mil)/global.pop.2020,
          .by = c("in.data"))

# 2050 population share covered:
P.df.iso.2050 %>% mutate(in.data = ifelse(iso %in% iso.assessed.by.desire, "yes", "no")) %>%
  reframe(population = sum(pop_mil),
          population.share = sum(pop_mil)/global.pop.2050,
          .by = c("in.data"))

# check which countries are missing:
missing.isos <- tibble(iso = Piso.2020[(Piso.2020) %nin% iso.assessed.by.desire])
missing.isos %>% left_join(P.df.iso.2020, by="iso") %>% arrange(desc(pop_mil))
# countries:
# Yemen, Venezuela, north korea, taiwan, Syria, Romania, Somalia, Cuba, south sudan,... (76 countries, including small ones like reunion, western sahara, guadaloupe, martinique, Mayotte, French Guyana, Jersey, and Guam)

### 2.5.2 ----------------------------------------------------------------------
# ... see numbers from Table 1

# 3. Results -------------------------------------------------------------------

### 3.1. DESIRE compared to bottom-up  -----------------------------------------
# DESIRE:
table.1 %>% filter(variable=="Total") %>%
  reframe(
    min.ENG = min(energy.development.gap) %>% round(),
    max.ENG = max(energy.development.gap) %>% round()
  )
# bottom-up:
table.2 %>% pull(global.gap) %>% unique() %>% round()

### 3.2. projecting scenario drivers  ------------------------------------------

##### Energy growth rates ResCom low income in SDP-RC --------------------------
energy.use.growth.total <- df.core %>%
  filter(year%in%c(2020,2040)) %>%
  select(model,scenario,iso,variable,year,energy.per.capita.uncorrected) %>%
  distinct() %>%  # 3 times smaller (because we drop energy information; 3 sectors)
  pivot_wider(names_from = year, values_from = energy.per.capita.uncorrected) %>%
  mutate(energy.per.capita.change.total = ((`2040`-`2020`) / `2020`) ) %>%
  select(model,scenario,iso,variable,energy.per.capita.change.total)
energy.use.growth.clean.basic <- df.core %>%
  filter(year%in%c(2020,2040)) %>%
  select(model,scenario,iso,variable,year,energy.per.capita) %>%
  distinct() %>%  # 3 times smaller (because we drop energy information; 3 sectors)
  pivot_wider(names_from = year, values_from = energy.per.capita) %>%
  mutate(energy.per.capita.change.clean.basic = ((`2040`-`2020`) / `2020`) ) %>%
  select(model,scenario,iso,variable,energy.per.capita.change.clean.basic)

energy.use.growth.uncert.total.v.basic.clean <-
  energy.use.growth.total %>%
  reframe(
    energy.per.capita.change.meanacrossmodels.total = mean(energy.per.capita.change.total),
    .by = c("scenario", "iso", "variable")
  ) %>%
  left_join(
    energy.use.growth.clean.basic %>%
      reframe(
        energy.per.capita.change.meanacrossmodels.clean.basic = mean(energy.per.capita.change.clean.basic),
        .by = c("scenario", "iso", "variable")
      )
  )

energy.use.growth.uncert.total.v.basic.clean %>% left_join(wb.income.grouping.3) %>%
  filter(wb.r3%in%c("Low","Middle", "High"),
         scenario%in%shape.sdps,
         variable=="Final Energy|Residential and Commercial") %>%
  reframe(
    total = median(energy.per.capita.change.meanacrossmodels.total),
    clean.basic = median(energy.per.capita.change.meanacrossmodels.clean.basic),
    .by = c("scenario", "wb.r3", "variable")
  ) %>% arrange(desc(wb.r3))

##### Transportation growth ----------------------------------------------------
energy.use.growth.uncert.total.v.basic.clean %>% left_join(wb.income.grouping.3) %>%
  filter(wb.r3%in%c("Low","Middle", "High"),
         scenario%in%shape.sdps,
         variable=="Final Energy|Transportation") %>%
  reframe(
    total = median(energy.per.capita.change.meanacrossmodels.total),
    clean.basic = median(energy.per.capita.change.meanacrossmodels.clean.basic),
    .by = c("scenario", "wb.r3", "variable")
  ) %>% arrange(desc(wb.r3))

##### Industry growth ----------------------------------------------------
energy.use.growth.uncert.total.v.basic.clean %>% left_join(wb.income.grouping.3) %>%
  filter(wb.r3%in%c("Low","Middle", "High"),
         scenario%in%shape.sdps,
         variable=="Final Energy|Industry") %>%
  reframe(
    total = median(energy.per.capita.change.meanacrossmodels.total),
    clean.basic = median(energy.per.capita.change.meanacrossmodels.clean.basic),
    .by = c("scenario", "wb.r3", "variable")
  ) %>% arrange(desc(wb.r3))
# exception
raw.scenario.data.fe.global %>%
  filter(year%in%c(2020,2040)) %>%
  pivot_wider(names_from=year,values_from=value) %>%
  # filter(grepl(scenario,pattern="SDP",fixed=T)) %>%
  mutate(change=`2040`-`2020`) %>%
  arrange(desc(change))



### 3.3 Inequality with higher subnational resolution --------------------------
theil.data.prep.T_t %>%
  filter(year%in%c(2020,2040), scenario==vis.scenario,
         variable%in%c(
           "Final Energy|Transportation",
           "Final Energy|Residential and Commercial",
           "Final Energy|Industry"
         )) %>%
  mutate(model="model-average") %>%
  rename.models.and.scenarios() %>%
  mutate(
    within.country.share = round(T_t_within/T_t*100)
  ) %>%
  distinct(variable,year,within.country.share) %>%
  arrange(year,within.country.share)

### 3.4. DLE threshold ---------------------------------------------------------
##### DLE threshold: global total, 2020 to 2040 --------------------------------

# percentage decrease
p.dle.data.global %>% filter(year%in%c(2020,2040)) %>%
  pivot_wider(
    names_from = year,
    values_from = dle
  ) %>% mutate(
    perc.change = (`2020` - `2040`)/ `2020`
  ) %>% arrange(-perc.change) %>%
  mutate(climate.policies=ifelse(scenario=="SSP2-NPi","No","Yes")) %>%
  reframe(
    range.perc.change = paste0(round(min(`perc.change`*100),0), "-", round(max(`perc.change`*100),0)),
    .by = c("climate.policies")
  )

# absolute; sdps
p.dle.data.global %>% filter(year%in%c(2020,2040)) %>%
  pivot_wider(
    names_from = year,
    values_from = dle
  ) %>% mutate(
    change = `2040` / `2020`
  ) %>% arrange(-change) %>%
  mutate(sdp=ifelse(grepl(scenario,pattern="SDP",fixed=T),"Yes","No")) %>%
  reframe(
    range.2020 = paste0(round(min(`2020`),1), "-", round(max(`2020`),1)),
    range.2040 = paste0(round(min(`2040`),1), "-", round(max(`2040`),1)),
    .by = c("sdp")
  )
# absolute; mitigation
p.dle.data.global %>% filter(year%in%c(2020,2040)) %>%
  pivot_wider(
    names_from = year,
    values_from = dle
  ) %>% mutate(
    change = `2040` / `2020`
  ) %>% arrange(-change) %>%
  mutate(climate.policies=ifelse(scenario=="SSP2-NPi","No","Yes")) %>%
  reframe(
    range.2020 = paste0(round(min(`2020`),1), "-", round(max(`2020`),1)),
    range.2040 = paste0(round(min(`2040`),1), "-", round(max(`2040`),1)),
    .by = c("climate.policies")
  ) %>%
  filter(climate.policies=="Yes")


##### DLE threshold: by income group (from supplementary information table) ----
p.dle.data.supplement <- df.core %>%
  reframe(
    dle = sum(dle.threshold.adjusted),
    dle.constant = sum(dle.threshold.curtech),
    .by = c("model", "scenario", "iso", "year")
  ) %>%
  mutate(dle.change = dle / dle.constant ) %>%
  left_join(wb.income.grouping.3) %>%
  left_join(population.iso %>% select(-variable,-unit))
# 2040v2020
p.dle.data.supplement %>%
  filter(year == DLE.END.YEAR.TIMESERIES) %>%
  filter(grepl(x=scenario, pattern="SDP", fixed=T)|grepl(x=scenario, pattern="SSP2-1p5C", fixed=T)) %>%
  group_by(wb.r4, model, scenario) %>%
  summarise(
    dle = weighted.mean(dle, pop_mil),
    dle.constant = weighted.mean(dle.constant, pop_mil),
    dle.change = dle / dle.constant
  ) %>%
  reframe(
    dle.mean = mean(dle) %>% round(),
    dle.max = max(dle) %>% round(),
    dle.min = min(dle) %>% round(),

    dle.constant = mean(dle.constant) %>% round(),

    change.mean = (mean(1-dle.change)*100) %>% round(),
    change.max = (max(1-dle.change)*100) %>% round(),
    change.min = (min(1-dle.change)*100) %>% round(),
  ) %>%
  distinct(wb.r4, model, dle.constant, dle.min, dle.max)

##### DLE threshold: national ranges, 2020 to 2040 ---------------------------------
p.dle.data %>% filter(year%in%c(2020,2040)) %>%
  filter(
    # pop_mil>5
  ) %>%
  select(-pop_mil) %>%

  rename(value=dle) %>%
  mutate(climate.policies=ifelse(scenario=="SSP2-NPi","No","Yes")) %>%
  add_percentile_columns(group.cols = c("year","climate.policies"),
                         percentiles = c(0.05,0.5,0.95)) %>%
  distinct(
    year,
    climate.policies,
    p5,p95
  )
# absolute; reference
p.dle.data.global %>% filter(year%in%c(2020,2040)) %>%
  pivot_wider(
    names_from = year,
    values_from = dle
  ) %>% mutate(
    change = `2040` / `2020`
  ) %>% arrange(-change) %>%
  mutate(climate.policies=ifelse(scenario=="SSP2-NPi","No","Yes")) %>%
  reframe(
    range.2020 = paste0(round(min(`2020`),1), "-", round(max(`2020`),1)),
    range.2040 = paste0(round(min(`2040`),1), "-", round(max(`2040`),1)),
    .by = c("climate.policies")
  ) %>%
  filter(climate.policies=="No")




### 3.5. ResCom deprivation headcounts -----------------------------------------

##### Global headcounts in SDPs ------------------------------------------------
p.dle.headcount.data %>% ms_add() %>%
  reframe(dle.headcount = sum(dle.headcount),
          .by = c("model", "scenario", "variable", "year")) %>%
  iamc_variable_keep_one_level(level = -1) %>%
  filter(year%in%c(2020,2040),
         variable=="Residential and Commercial") %>%
  pivot_wider(names_from = year, values_from = dle.headcount) %>%
  # range across models and scenarios
  mutate(sdp=ifelse(scenario%in%shape.sdps,"Yes",scenario)) %>%
  reframe(
    range.2020 = paste0(round(min(`2020`)), "-", round(max(`2020`))),
    range.2040 = paste0(round(min(`2040`)), "-", round(max(`2040`))),
    .by = c("sdp")
  )

##### Headcount reductions in SDPs: from >90% to <1% (model average) ----------
model.avg.deprivation.rates <- df.core %>%
  reframe(share.below.projected.adjusted=mean(share.below.projected.adjusted),
          .by = c(#"model",
            "scenario", "iso", "variable", "year")) %>%
  iamc_variable_keep_one_level(level = -1) %>%
  filter(variable=="Residential and Commercial")

countries.with.90plus.rescom <- model.avg.deprivation.rates %>%
  filter(year==2020) %>%
  filter(share.below.projected.adjusted>0.9) %>%
  # distinct(iso)
  filter(scenario==vis.scenario) %>%
  pull(iso) %>% unique()
model.avg.deprivation.rates %>% filter(year==2040) %>%
  filter(iso%in%countries.with.90plus.rescom) %>%
  # filter(scenario=="SDP_EI-1p5C") %>%
  mutate(deprivation.resolved = ifelse(share.below.projected.adjusted<0.01,"Yes","No")) %>%
  group_by(scenario,deprivation.resolved) %>%
  count() %>%
  mutate(n = n/length(countries.with.90plus.rescom))

##### Headcounts persistent in SDPs: >50% in 2040 (model average) --------------
model.avg.deprivation.rates %>% filter(year==2040) %>%
  # filter(iso%in%countries.with.90plus.rescom) %>%
  # filter(scenario=="SDP_EI-1p5C") %>%
  mutate(deprivation.persistent = ifelse(share.below.projected.adjusted>0.50,"Yes","No")) %>%
  filter(deprivation.persistent=="Yes") %>%
  group_by(scenario,deprivation.persistent) %>% count()

# ##### Remaining deprivation headcounts in SDPs (rescom)
# rem.dep.rescom.sdps <- df.core %>%
#   reframe(dle.headcount = share.below.projected.adjusted*pop_mil,
#           share.below.projected.adjusted=share.below.projected.adjusted,
#           .by = c("model", "scenario", "iso", "variable", "year")) %>%
#   iamc_variable_keep_one_level(level = -1) %>%
#   filter(year%in%c(2020,2040),
#          scenario%in%shape.sdps,
#          variable=="Residential and Commercial") %>%
#   pivot_wider(names_from = year,
#               values_from = c(dle.headcount, share.below.projected.adjusted)) %>%
#   # range across models and scenarios
#   reframe(
#     depriv.2040 = mean(`dle.headcount_2040`),
#     share.2040 = mean(`share.below.projected.adjusted_2040`),
#     .by = c("iso")
#   )
#
# rem.dep.rescom.sdps %>%
#   arrange(desc(depriv.2040))

# ##### Remaining deprivation headcounts in SDPs (rescom): only Afganistan over 90%
# rem.dep.rescom.sdps %>%
#   arrange(desc(share.2040)) %>% print(n=30)

##### Global headcounts in SDPs: transport reference ---------------------------
p.dle.headcount.data %>% ms_add() %>%
  reframe(dle.headcount = sum(dle.headcount),
          .by = c("model", "scenario", "variable", "year")) %>%
  iamc_variable_keep_one_level(level = -1) %>%
  filter(year%in%c(2020,2040),
         variable%in%c("Transportation"#,
                       # "Industry"
         )) %>%
  pivot_wider(names_from = year, values_from = dle.headcount) %>%
  # range across models and scenarios
  mutate(sdp=ifelse(scenario%in%shape.sdps,"Yes",scenario)) %>%
  reframe(
    range.2020 = paste0(round(min(`2020`)), "-", round(max(`2020`))),
    range.2040 = paste0(round(min(`2040`)), "-", round(max(`2040`))),
    .by = c("variable","sdp")
  ) %>%
  filter(sdp=="Yes")

##### China (and IND, NGA) remaining deprivation rate -----------------------------------------
df.core %>% filter(year%in%c(2040),
                   variable=="Final Energy|Residential and Commercial",
                   scenario%in%shape.sdps,
                   iso%in%c(
                     "IND", "NGA", "CHN"
                   )) %>%
  select(model,scenario,iso,variable,year,
         dle.threshold.adjusted,
         energy.per.capita,
         dle.tech.scaler,
         energy.gini,
         share.below.projected.adjusted
  ) %>%
  reframe(
    dr = mean(share.below.projected.adjusted),
    .by = c("iso","scenario")
  ) %>%
  reframe(
    dr.min = paste(round(min(dr) * 100), "%"),
    dr.max = paste(round(max(dr) * 100), "%"),
    .by = c("iso")
  ) %>% arrange(desc(iso))

##### Nigeria, India, China progress explained ---------------------------------
country.data.explained <- df.core %>% filter(year%in%c(2020,2040),
                   variable=="Final Energy|Residential and Commercial",
                   scenario%in%shape.sdps,
                   iso%in%c(
                     "IND", "NGA", "CHN", "USA"
                   )) %>%
  select(model,scenario,iso,variable,year,
         dle.threshold.adjusted,
         energy.per.capita,
         dle.tech.scaler,
         energy.gini,
         share.below.projected.adjusted
         ) %>%
  reframe(
    dle.threshold.adjusted = mean(dle.threshold.adjusted),
    energy.per.capita = mean(energy.per.capita),
    dle.tech.scaler = mean(dle.tech.scaler),
    energy.gini = mean(energy.gini),
    share.below.projected.adjusted = mean(share.below.projected.adjusted),
    .by = c("iso",
            "scenario",
            "year")
  ) %>%
  reframe(
    dle.threshold.adjusted = mean(dle.threshold.adjusted),
    energy.per.capita = mean(energy.per.capita),
    dle.tech.scaler = mean(dle.tech.scaler),
    energy.gini = mean(energy.gini),
    share.below.projected.adjusted = paste0(
      round(min(share.below.projected.adjusted),2),
      "-",
      round(max(share.below.projected.adjusted),2)
    ),
    .by = c("iso",
            # "scenario",
            "year")
  )
# change:
country.data.explained %>%
  pivot_wider(names_from = year, values_from = c(
    dle.threshold.adjusted,
    energy.per.capita,
    dle.tech.scaler,
    energy.gini,
    share.below.projected.adjusted)) %>%
  mutate(
    change.energy.per.capita = round(`energy.per.capita_2040` / `energy.per.capita_2020`,digits=1),
    change.dle.threshold = paste(round((1-`dle.tech.scaler_2040`)*100, digits=0), "%"),
    change.energy.gini = round(`energy.gini_2040` - `energy.gini_2020`, digits = 2)
  ) %>%
  select(
    iso,
    change.energy.per.capita,
    change.dle.threshold,
    change.energy.gini
  )


##### between 2-4x DLE  --------------------------------------------------------
# global
n.all <- dle.aligned.sampled.data.rescaled %>%
  filter(year==2040, facet.scenario == "SDPs") %>%
  reframe(rows = n(),
          .by = c("scenario"))
n.2ormore <- dle.aligned.sampled.data.rescaled %>%
  filter(year==2040, facet.scenario == "SDPs",
         fe.scaled.to.dle>=2) %>%
  reframe(rows.2ormore = n(),
          .by = c("scenario"))
left_join(n.all,n.2ormore) %>%
  mutate(share = paste(round(rows.2ormore / rows *100), "%")) %>%
  distinct(scenario,share) %>%
  arrange(share)

# global south / world bank income groups
n.all.gs <- dle.aligned.sampled.data.rescaled %>%
  left_join(load_official_country_grouping("Income _status_WB")) %>%
  filter(year==2040, facet.scenario == "SDPs") %>%
  reframe(rows = n(),
          .by = c("scenario", "Income _status_WB"))
n.2ormore.gs <- dle.aligned.sampled.data.rescaled %>%
  left_join(load_official_country_grouping("Income _status_WB")) %>%
  filter(year==2040, facet.scenario == "SDPs",
         fe.scaled.to.dle>=2) %>%
  reframe(rows.2ormore = n(),
          .by = c("scenario", "Income _status_WB"))
left_join(n.all.gs,n.2ormore.gs) %>%
  mutate(share = paste(round(rows.2ormore / rows *100), "%")) %>%
  distinct(scenario,`Income _status_WB`, share) %>%
  arrange(`Income _status_WB`, share)





### 3.6. Energy needs gaps -----------------------------------------------------
# global
p.EDG.summedsectors.minmaxavg.data %>%
  filter(year %in% c(2020),
         scenario == "SDP_RC-1p5C") %>%
  left_join(pop.r10.projection) %>%
  enterise_long_r10_names() %>%
  mutate(energy.gap=energy.development.gap.avg) %>%
  select(r10,energy.gap) %>%
  summarise(energy.gap=round(sum(energy.gap)))

# regional current (totals and shares)
p.EDG.summedsectors.minmaxavg.data %>%
  filter(year %in% c(2020),
         scenario == "SDP_RC-1p5C") %>%
  left_join(pop.r10.projection) %>%
  enterise_long_r10_names() %>%
  mutate(energy.gap=energy.development.gap.avg) %>%
  select(r10,energy.gap) %>% arrange(desc(energy.gap)) %>%
  cross_join(
    # add total
    p.EDG.summedsectors.minmaxavg.data %>%
      filter(year %in% c(2020),
             scenario == "SDP_RC-1p5C") %>%
      left_join(pop.r10.projection) %>%
      enterise_long_r10_names() %>%
      mutate(energy.gap=energy.development.gap.avg) %>%
      select(r10,energy.gap) %>% arrange(desc(energy.gap)) %>% reframe(global.gap=sum(energy.gap))
  ) %>%
  mutate(
    share = paste0(round(energy.gap/global.gap*100),"%")
  ) %>%
  distinct(r10,energy.gap,share)

# current as multiple of DLE
p.EDG.summedsectors.minmaxavg.data %>%
  filter(year %in% c(2020),
         scenario == "SDP_RC-1p5C") %>%
  left_join(pop.r10.projection) %>%
  enterise_long_r10_names() %>%
  mutate(energy.gap.reltocurrenttotal=energy.development.gap.avg/total.energy.use.avg) %>%
  select(r10,energy.gap.reltocurrenttotal) %>% arrange(desc(energy.gap.reltocurrenttotal)) %>%
  print(n=1)

# as share of total energy - regional
# ... see Table 3
# as share of total energy - global
# ... see Table 3


### 3.7 Energy beyond DLS and convergence in energy headroom -------------------

##### For-DLS energy as share of total ----------------------------------------
# global, across SDPs
df.headroom.table.data.global %>% filter(year%in%c(2020,2040)) %>%
  mutate(for.dls.energy.share = (dle.threshold-energy.development.gap)/total.energy.use ) %>%
  filter(grepl(scenario, pattern="SDP")) %>%
  reframe(
    global.beyond.dls.energy.share =
      paste0(round(min(for.dls.energy.share*100)),
             " to ",
             round(max(for.dls.energy.share*100))),
    .by = c("year")
  )
# global, specific SDPs
df.headroom.table.data %>% filter(year%in%c(2020,2040)) %>%
  mutate(for.dls.energy.share = (dle.threshold-energy.development.gap)/total.energy.use ) %>%
  filter(grepl(scenario, pattern="SDP")) %>%
  reframe(
    regional.for.dls.energy.share =
      paste0(round(min(for.dls.energy.share*100)),
             " to ",
             round(max(for.dls.energy.share*100))),
    .by = c("year", "r10", "scenario")
  ) %>%
  filter(r10=="World",
         year==2040)
# regional, SDP-RC
df.headroom.table.data %>% filter(year%in%c(2020,2040)) %>%
  mutate(for.dls.energy.share = (dle.threshold-energy.development.gap)/total.energy.use ) %>%
  filter(grepl(scenario, pattern="SDP_RC")) %>%
  reframe(
    regional.for.dls.energy.share =
      paste0(round(min(for.dls.energy.share*100)),
             " to ",
             round(max(for.dls.energy.share*100))),
    .by = c("year", "r10", "scenario")
  ) %>%
  filter(r10%in%c("Africa", "North America"))

##### Beyond-DLS energy as share of total ----------------------------------------
df.headroom.table.data.global %>% filter(year%in%c(2020,2040)) %>%
  mutate(beyond.dls.energy.share = (total.energy.use+energy.development.gap-dle.threshold)/total.energy.use ) %>%
  filter(grepl(scenario, pattern="SDP")) %>%
  reframe(
    global.beyond.dls.energy.share =
      paste0(round(min(beyond.dls.energy.share*100)),
             " to ",
             round(max(beyond.dls.energy.share*100))),
    .by = c("year")
  )


##### 2020 energy headroom regional --------------------------------------------
# current
p.EDG.summedsectors.minmaxavg.data %>%
  filter(year %in% c(2020),
         scenario == "SDP_RC-1p5C") %>%
  left_join(pop.r10.projection) %>%
  enterise_long_r10_names() %>%
  mutate(energy.headroom.per.capita=round(abovedle.minus.energygap.avg/pop_mil*exa/giga/mega) ) %>%
  select(r10,energy.headroom.per.capita) %>%
  arrange(desc(energy.headroom.per.capita)) %>%
  print(n=10)

# current, USA, relative to DLE
p.EDG.summedsectors.minmaxavg.data %>%
  filter(year %in% c(2020),
         scenario == "SDP_RC-1p5C") %>%
  left_join(pop.r10.projection) %>%
  enterise_long_r10_names() %>%
  mutate(energy.headroom.per.capita.reltodle=abovedle.minus.energygap.avg/dle.threshold.avg) %>%
  select(r10,energy.headroom.per.capita.reltodle) %>% arrange(desc(energy.headroom.per.capita.reltodle)) %>%
  print(n=10)


##### convergence in energy headroom -----------------------------------------------
# check lowest standard deviation between regions (convergence)
p.EDG.summedsectors.minmaxavg.data %>%
  filter(year %in% c(2020,2040),
         scenario %in% shape.sdps) %>%
  left_join(pop.r10.projection) %>%
  enterise_long_r10_names() %>%
  mutate(energy.headroom.per.capita=abovedle.minus.energygap.avg/pop_mil*exa/giga/mega) %>%
  select(scenario,r10,year,energy.headroom.per.capita) %>% reframe(
    headroom.dev = sd(energy.headroom.per.capita),
    .by = c("scenario","year")
  ) %>% arrange(desc(year))
# check lowest headroom total (contraction)
p.EDG.summedsectors.minmaxavg.data %>%
  filter(year %in% c(2020,2040),
         scenario %in% shape.sdps) %>%
  left_join(pop.r10.projection) %>%
  enterise_long_r10_names() %>%
  mutate(energy.headroom.total=abovedle.minus.energygap.avg) %>%
  select(scenario,r10,year,energy.headroom.total) %>% reframe(
    headroom.sum = sum(energy.headroom.total),
    .by = c("scenario","year")
  ) %>% arrange(desc(year))
# rc (strongest convergence and contraction)
p.EDG.summedsectors.minmaxavg.data %>%
  filter(year %in% c(2040),
         scenario == "SDP_RC-1p5C") %>%
  left_join(pop.r10.projection) %>%
  enterise_long_r10_names() %>%
  mutate(energy.headroom.per.capita=abovedle.minus.energygap.avg/pop_mil*exa/giga/mega) %>%
  select(r10,energy.headroom.per.capita) %>% arrange(desc(energy.headroom.per.capita)) %>%
  print(n=10)

# SSP2-Ref (contrast to reference)
p.EDG.summedsectors.minmaxavg.data %>%
  filter(year %in% c(2040),
         scenario == "SSP2-NPi") %>%
  left_join(pop.r10.projection) %>%
  enterise_long_r10_names() %>%
  mutate(energy.headroom.per.capita=abovedle.minus.energygap.avg/pop_mil*exa/giga/mega) %>%
  select(r10,energy.headroom.per.capita) %>% arrange(desc(energy.headroom.per.capita)) %>%
  print(n=10)




##### Energy headroom as share of total energy use (Africa) --------------------
df.headroom.table.data %>%
  filter(year%in%c(2040),
         r10=="Africa") %>%
  mutate(headroom.share = ((total.energy.use-dle.threshold)/total.energy.use) ) %>%
  filter(grepl(scenario, pattern="SDP")) %>%
  reframe(
    beyond.dle.possible.headroom.share =
      paste0(round(min(headroom.share*100)),
             " to ",
             round(max(headroom.share*100))),
    .by = c("year")
  )

##### Energy headroom as share of total energy use (other regions) --------------------
df.headroom.table.data %>%
  filter(year%in%c(2040),
         r10!="Africa") %>%
  mutate(headroom.share = ((total.energy.use-dle.threshold)/total.energy.use) ) %>%
  filter(grepl(scenario, pattern="SDP")) %>%
  reframe(
    beyond.dle.possible.headroom.share =
      paste0(round(min(headroom.share*100)),
             " to ",
             round(max(headroom.share*100))),
    .by = c("year")
  )
##### Energy headroom as share of total energy use (world) ---------------------
df.headroom.table.data %>%
  filter(year%in%c(2040),
         r10=="World") %>%
  mutate(headroom.share = ((total.energy.use-dle.threshold)/total.energy.use) ) %>%
  filter(grepl(scenario, pattern="SDP")) %>%
  reframe(
    beyond.dle.possible.headroom.share =
      paste0(round(min(headroom.share*100)),
             " to ",
             round(max(headroom.share*100))),
    .by = c("year")
  )




### 3.8 Emissions --------------------------------------------------------------

##### Per capita current, global and national ----------------------------------
# global
df.core.total.energy.emissions.meanacrossmodels %>%
  filter(year==2020) %>% left_join(population.iso.2020.base) %>%
  mutate(dls.co2 = dls.co2.emissions.threshold/pop_mil/1e6*giga) %>%
  reframe(mean.dls.co2 = weighted.mean(dls.co2, w=pop_mil),
          .by=c("scenario")) %>%
  summarise(mean(mean.dls.co2))
# range across countries
df.core.total.energy.emissions.meanacrossmodels %>%
  filter(year==2020) %>% left_join(population.iso.2020.base) %>%
  mutate(value = dls.co2.emissions.threshold/pop_mil/1e6*giga) %>%
  add_percentile_columns(group.cols = c("scenario"),
                         percentiles = c(0.05,0.5,0.95)) %>%
  distinct(scenario,p5,p50,p95)

##### Per capita 2040, global and national ----------------------------------
population.iso.sdp.2040 <- population.iso %>% filter(year==2040) %>%
  filter(model == vis.model, scenario == vis.scenario) %>%
  select(iso,year,pop_mil)
# range across countries
df.core.total.energy.emissions.meanacrossmodels %>%
  filter(grepl(scenario,pattern="SDP",fixed=T)) %>%
  filter(year==2040) %>% left_join(population.iso.sdp.2040) %>%
  mutate(value = dls.co2.emissions.threshold/pop_mil/1e6*giga) %>%
  add_percentile_columns(group.cols = c("scenario"),
                         percentiles = c(0.05,0.5,0.95)) %>%
  distinct(scenario,p5,p50,p95)
# global
df.core.total.energy.emissions.meanacrossmodels %>%
  filter(grepl(scenario,pattern="SDP",fixed=T)) %>%
  filter(year==2040) %>% left_join(population.iso.sdp.2040) %>%
  mutate(dls.co2 = dls.co2.emissions.threshold/pop_mil/1e6*giga) %>%
  reframe(mean.dls.co2 = weighted.mean(dls.co2, w=pop_mil),
          .by=c("scenario")) %>%
  summarise(mean(mean.dls.co2))

##### Per capita: select high and low countries --------------------------------
# low emissions DLS
df.core.total.energy.emissions.meanacrossmodels %>%
  filter(
    year==2020,
    iso=="ETH",
    scenario%in%c(vis.scenario, "SSP2-NPi")
  ) %>%
  left_join(population.iso %>% filter(model==vis.model) %>% select(scenario,iso,year,pop_mil)) %>%
  mutate(value = dls.co2.emissions.threshold/pop_mil/1e6*giga) %>%
  add_percentile_columns(group.cols = c("scenario","year")) %>%
  select(scenario,iso,value)
# high emissions DLS
df.core.total.energy.emissions.meanacrossmodels %>%
  filter(
    year==2020,
    iso=="USA",
    scenario%in%c(vis.scenario, "SSP2-NPi")
  ) %>%
  left_join(population.iso %>% filter(model==vis.model) %>% select(scenario,iso,year,pop_mil)) %>%
  mutate(value = dls.co2.emissions.threshold/pop_mil/1e6*giga) %>%
  add_percentile_columns(group.cols = c("scenario","year")) %>%
  select(scenario,iso,value)

##### DLS cumulative GN / GS shares --------------------------------------------
# ... see Figure 7B

##### DLS cumulative -----------------------------------------------------------
cumulative.emissions.until.2050 %>%
  mutate(
    RCB.share.for.DLS = cumulative.dls.co2.emissions.until2050.all / RCB.from.2024.1p5c
  ) %>%
  # range across models and scenarios
  mutate(sdp=ifelse(scenario%in%shape.sdps,"Yes",scenario)) %>%
  reframe(
    range.cumu = paste0(round(min(`cumulative.dls.co2.emissions.until2050.all`)),
                        "-",
                        round(max(`cumulative.dls.co2.emissions.until2050.all`))),
    range.RCB.share = paste0(round(min(`RCB.share.for.DLS`),2),
                             "-",
                             round(max(`RCB.share.for.DLS`),2)),
    .by = c("sdp")
  )


# 4. Discussion and Conclusion -------------------------------------------------
# all covered in results.




# . ----------------------------------------------------------------------------
# . ----------------------------------------------------------------------------
# . ----------------------------------------------------------------------------
# APPENDIX FIGURES AND NUMBERS -------------------------------------------------

### Figure A1: overview of DESIRE ----------------------------------------------
# no code; figure was made in different software.

### Figure A2: mapping DLE-to-IAM ----------------------------------------------

threshold.data.mapping.dle.to.iam <- read_csv(
  file.path(
    PATH.data.desire,
    "other",
    "dle-to-iam-mapping",
    "dle-to-iam-energythreshold.csv"
  )
)

p.threshold.data.mapping.dle.to.iam.data <- threshold.data.mapping.dle.to.iam %>%
  mutate_cond(
    `DLS dimension`%in%c("Heating OP|total",
                         "Heating CON|total",
                         "Hot Water OP|total"), `DLS dimension` = "Space and water heating"
  ) %>%
  mutate_cond(
    `DLS dimension`%in%c("Cooling OP|total",
                         "Cooling CON|total"), `DLS dimension` = "Space cooling"
  ) %>%
  mutate_cond(
    `DLS dimension`%in%c("Education|lower_secondary",
                         "Education|primary"), `DLS dimension` = "Education"
  ) %>%

  mutate_cond(
    `DLS dimension`%in%c("Clothing|clothing",
                         "Clothing|footwear",
                         "Education",
                         "Health care",
                         "Housing|total",
                         "Sanitation"), `DLS dimension` = "Other"
  ) %>%

  mutate_cond(
    `DLS dimension`%in%c("Appliance|clean_cooking_fuel",
                         "Appliance|refrigerator",
                         "Appliance|television",
                         "Appliance|mobile_telephone"), `DLS dimension` = "Appliances"
  ) %>%
  reframe(
    mj_percap = sum(mj_percap),
    .by = c("iso", "sector", "DLS dimension")
  )

library(ggsci)
p.threshold.data.mapping.dle.to.iam <- ggplot(
  p.threshold.data.mapping.dle.to.iam.data %>%
    filter(sector %in% c("Final Energy|Residential and Commercial",
                         "Final Energy|Industry",
                         "Final Energy|Transportation"),
           iso %in% c(
             # "NOR",
             "CHN",
             "IND",
             "BRA"
           )) %>%
    mutate_cond(sector == "Final Energy|Industry", sector = "Industry") %>%
    mutate_cond(sector == "Final Energy|Transportation", sector = "Transportation") %>%
    mutate_cond(sector == "Final Energy|Residential and Commercial", sector = "Resid. and\nCommercial") %>%
    left_join(load_long_names_of_iso3c()),

) +
  facet_grid(~name) +
  geom_col(aes(y = sector, x = mj_percap/1e3, fill = `DLS dimension`), position = "stack") +
  scale_fill_jco() +
  ylab(NULL) +
  xlab("GJ/cap/yr for DLE") +
  guides(fill = guide_legend("")) +
  theme_classic() +
  theme_hc() +
  theme(panel.grid.major.x = element_line(colour = "lightgrey"),
        panel.grid.major.y = element_blank())



save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, paste0("an-fa2") ),
  p = p.threshold.data.mapping.dle.to.iam,
  w = 300,
  h = 75
)


### Figure A3: current energy inequality ---------------------------------------

# lorenz curve type data - national within-country samples (DESIRE)
lorenz.data.desire.allcountries.draws <- create_sample_data(
  df.core,
  sample.scenarios = c("SDP_RC-1p5C"),
  sample.years = c(2020),
  sample.variables = c("Final Energy|Residential and Commercial",
                       "Final Energy|Transportation",
                       "Final Energy|Industry"),
  population.cutoff = 0,
  sample.size = SAMPLING.SIZE,

  adjusted.to.dle = FALSE,

  type.of.output="sample-data-for-lorenz-curve"
) %>%
  select(model,scenario,iso,variable,unit,year,pop_mil,
         fe.scaled.to.mean) %>%
  rename(energy.per.capita.uncorrected=fe.scaled.to.mean)
# - add zero-zero point
lorenz.data.desire.allcountries <- lorenz.data.desire.allcountries.draws %>%
  mutate(pop_mil = pop_mil/SAMPLING.SIZE) %>%
  bind_rows(
    lorenz.data.desire.allcountries.draws %>% select(model,iso,scenario,year,variable,unit) %>% distinct() %>%
      mutate(pop_mil = 0, energy.per.capita.uncorrected = 0)
  ) %>%
  arrange(scenario,variable,iso,year,pop_mil)

# get national average data
nationalaverage.data.allcountries <- df.core %>%
  reframe(
    energy.per.capita.uncorrected.national = mean(energy.per.capita.uncorrected),
    pop_mil_national = first(pop_mil),
    .by = c(#"model",
      "scenario", "iso", "variable", "unit", "year", "pop_mil"),
  ) %>%
  mutate(model="model-average") %>%
  filter(
    scenario %in% c("SDP_RC-1p5C"),
    pop_mil >= 0,
    year %in% c(2020)
  ) %>%
  select(model,scenario,iso,variable,unit,year,pop_mil_national,
         energy.per.capita.uncorrected.national)

# # prepare lorenz curve type data - national means (IAM)
# lorenz.prep.national <- nationalaverage.data.allcountries %>%
#   mutate(population.share = pop_mil / SAMPLING.SIZE) %>%
#   mutate(final.energy.share = energy.per.capita / SAMPLING.SIZE / energy.per.capita.uncorrected.national) %>%
#   group_by(variable, year) %>%
#   arrange(energy.per.capita.uncorrected) %>%
#   mutate(cumu.pop = cumsum(population.share)) %>%
#   mutate(cumu.fe = cumsum(final.energy.share)) %>%
#   ungroup()

# prepare lorenz curve type data - national within-country samples (DESIRE)
# - start: similar to national averages
lorenz.distributions.allcountries <- lorenz.data.desire.allcountries %>%
  left_join(nationalaverage.data.allcountries) %>%
  mutate(population.share = pop_mil / pop_mil_national * SAMPLING.SIZE) %>%
  mutate(final.energy.share = energy.per.capita.uncorrected / SAMPLING.SIZE / energy.per.capita.uncorrected.national) %>%
  group_by(iso, variable, year) %>%
  arrange(energy.per.capita.uncorrected) %>%
  mutate(cumu.pop = cumsum(population.share)) %>%
  mutate(cumu.fe = cumsum(final.energy.share)) %>%
  ungroup()


# gtap9 oswald data
# load data
gtap9 <- readRDS(
  file.path(
    PATH.data.desire,
    "other","inequality",
    "FE_quantile_iso_sector.RData"
  )
) %>%
  mutate(n.quantile = NA) %>%
  mutate_cond((quantile == "lowest" | quantile == "Q1"), n.quantile = 1) %>%
  mutate_cond((quantile == "low" | quantile == "Q2"), n.quantile = 2) %>%
  mutate_cond((quantile == "middle" | quantile == "Q3"), n.quantile = 3) %>%
  mutate_cond((quantile == "high" | quantile == "Q4"), n.quantile = 4) %>%
  mutate_cond((quantile == "Q5"), n.quantile = 5) %>%
  pivot_longer(cols = c(fe, fe.rescom, fe.transport,fe.industry), names_to = "variable", values_to = "fe") %>%
  mutate_cond(variable == "fe", variable = "Final Energy") %>%
  mutate_cond(variable == "fe.rescom", variable = "Final Energy|Residential and Commercial") %>%
  mutate_cond(variable == "fe.transport", variable = "Final Energy|Transportation") %>%
  mutate_cond(variable == "fe.industry", variable = "Final Energy|Industry") %>%
  select(iso, variable, n.quantile, fe, population) %>%
  arrange(iso, variable, n.quantile) %>%
  filter(population != 0) %>%
  drop_na() # transport for Benin (quantiles 1,2,3), and for Belarus (quantile 1)

# prepare data for lorentz plots
# # gtap
total.pop.iso <- gtap9 %>%
  reframe(total.pop = sum(population),
          .by = c("variable","iso"))
total.energy.iso <- gtap9 %>%
  reframe(total.fe = sum(fe * population * giga / exa),
          .by = c("variable","iso"))

gtap9.prep.iso <- gtap9 %>%
  left_join(total.pop.iso) %>%
  left_join(total.energy.iso)  %>%
  mutate(population.share = population / total.pop) %>%
  mutate(final.energy.share = (fe * population * giga / exa) / total.fe) %>%
  group_by(variable,iso) %>%
  arrange(fe) %>%
  mutate(cumu.pop = cumsum(population.share)) %>%
  mutate(cumu.fe = cumsum(final.energy.share)) %>%
  ungroup()
lorenz.gtap9.oswald.iso <- gtap9.prep.iso %>%
  # add zero-zero point
  bind_rows(
    gtap9.prep %>% select(variable,iso) %>% distinct() %>%
      mutate(cumu.pop = 0, cumu.fe = 0, fe = 0, population = 0)
  ) %>%
  arrange(iso,variable, cumu.pop) %>%
  filter(variable != "Final Energy")
lorenz.gtap9.oswald.iso


# plot lorentz
annex.within.country <-
  ggplot(data = lorenz.distributions.allcountries %>% filter(year==2020) %>%  mutate(year=paste0(as.character(year), " (IAM, DESIRE): SDP-RC")) %>%
           drop_na(iso)) + # %>% rename(code_alpha3=iso)) +
  facet_wrap(iso~.) +
  # facet_geo(~ code_alpha3, grid = "world_countries_grid1", label = "name") +

  # oswald data
  geom_rect(
    data = lorenz.gtap9.oswald.iso %>% mutate(year="2011 (Oswald et al., 2020; 2021)") %>%
      drop_na(iso), # %>% rename(code_alpha3=iso),
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "dodgerblue",
    alpha = 0.01
  ) +

  # oswald data
  geom_line(
    data = lorenz.gtap9.oswald.iso %>% mutate(year="2011 (Oswald et al., 2020; 2021)") %>%
      drop_na(iso), # %>% rename(code_alpha3=iso),
    aes(y = cumu.fe, x = cumu.pop, colour = variable, group = variable),
    linetype = "dotted"
  ) +
  geom_point(
    data = lorenz.gtap9.oswald.iso %>% mutate(year="2011 (Oswald et al., 2020; 2021)") %>%
      drop_na(iso), # %>% rename(code_alpha3=iso),
    aes(y = cumu.fe, x = cumu.pop, colour = variable)
  ) +

  # sampled data
  geom_line(
    data = lorenz.distributions.allcountries %>% filter(year==2020) %>%  mutate(year=paste0(as.character(year), " (IAM, DESIRE): SDP-RC")) %>%
      drop_na(iso), # %>% rename(code_alpha3=iso),
    aes(y = cumu.fe, x = cumu.pop, colour = variable, group = variable)
  ) +

  # equality line
  geom_abline(
    intercept = 0,
    slope = 1,
    colour = "black",
    linetype = "dashed"
  ) +

  scale_y_continuous(expand = c(0, 0), breaks = NULL, limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), breaks = NULL, limits = c(0, 1)) +

  scale_colour_ptol() +

  xlab("Cumulative population") +
  ylab("Cumulative final energy consumption") +
  labs(
    subtitle = bquote("National"~bold("Lorenz curves")~"\nfor final energy consumption"),
    caption = "Solid line: based on sampled (10,000 per country) data with within-country distributions.\nDotted line = data points of energy accounts.\nDashed line = full equality."
  ) +
  guides(colour = guide_legend(NULL), shape = guide_legend(NULL)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

# annex.within.country


save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "an-fa3" ),
  p = annex.within.country,
  w = 300,
  h = 350
)


##### Gini decline in numbers --------------------------------------------------
gini.growth %>% filter(scenario=="SDP_RC-1p5C") %>%
  reframe(
    p5 = quantile(gini.point.change,probs = 0.05),
    p95 = quantile(gini.point.change,probs = 0.95)
  )

##### GDP growth rate numbers (UPDATE!) --------------------------------------------------
gdp.growth %>% filter(scenario%in%shape.core.scenarios) %>%
  left_join(gdp.growth.historical) %>%
  left_join(wb.income.grouping.3) %>%
  mutate(diff.growth.rate = gdp.perc.growth - gdp.perc.growth.historical) %>%
  reframe(

    p05.hist = quantile(gdp.perc.growth.historical,
                        probs = 0.05, na.rm = T),
    p50.hist = quantile(gdp.perc.growth.historical,
                        probs = 0.5, na.rm = T),
    p95.hist = quantile(gdp.perc.growth.historical,
                        probs = 0.95, na.rm = T),

    p05.futu = quantile(gdp.perc.growth,
                        probs = 0.05, na.rm = T),
    p50.futu = quantile(gdp.perc.growth,
                        probs = 0.5, na.rm = T),
    p95.futu = quantile(gdp.perc.growth,
                        probs = 0.95, na.rm = T),

    p05.diff = quantile(gdp.perc.growth - gdp.perc.growth.historical,
                   probs = 0.05, na.rm = T),
    p50.diff = quantile(gdp.perc.growth - gdp.perc.growth.historical,
                      probs = 0.5, na.rm = T),
    p95.diff = quantile(gdp.perc.growth - gdp.perc.growth.historical,
                   probs = 0.95, na.rm = T),

    .by = c("scenario", "wb.r3")
  ) %>%
  mutate(p50.medians.diff = p50.futu/p50.hist) %>%
  filter(wb.r3=="High")



### Figure A4: inequality projection -------------------------------------------
p.inequality.projection.data <- df.core %>%
  filter(year%in%c(2020,2040),
         scenario=="SDP_RC-1p5C") %>%
  select(year,iso,variable,gini,energy.gini,pop_mil) %>%
  distinct() %>%
  rename(`Population (millions)` = pop_mil)

p.inequality.projection <- ggplot(
  data = p.inequality.projection.data,
  mapping = aes(x=gini, y=energy.gini)
) +
  facet_grid(.~variable) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", alpha = 1) +
  geom_segment(
    data = . %>% filter(year==2020),
    aes(x = 0, y = 0, xend = gini, yend = energy.gini),
    linetype = "dotted",
    color = "lightgrey",
    alpha = 0.2
  ) +
  geom_segment(
    data = . %>% filter(year==2020),
    aes(x = 1, y = 1, xend = gini, yend = energy.gini),
    linetype = "dotted",
    color = "lightgrey",
    alpha = 0.2
  ) +
  geom_segment(
    data = . %>% filter(year==2020) %>% mutate(year=2040),
    aes(x = 0, y = 0, xend = gini, yend = energy.gini),
    linetype = "dotted",
    color = "lightgrey",
    alpha = 0.2
  ) +
  geom_segment(
    data = . %>% filter(year==2020) %>% mutate(year=2040),
    aes(x = 1, y = 1, xend = gini, yend = energy.gini),
    linetype = "dotted",
    color = "lightgrey",
    alpha = 0.2
  ) +
  geom_point(data=. %>% filter(year==2020),aes(size = `Population (millions)`,colour=as.character(year)), alpha = 0.7) +
  geom_point(data=. %>% filter(year==2040),aes(size = `Population (millions)`,colour=as.character(year)), alpha = 0.7) +
  scale_color_manual(values = c("grey","black"), breaks = c("2020","2040"), name = "Year") +
  theme_classic() +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.5), expand=c(0,0),
                     name = "Energy Gini coefficient") +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.5), expand=c(0,0),
                     name = "Income Gini coefficient") +
  labs(caption = "Dashed red line: energy gini is equal to income gini") +
  theme(
    panel.spacing.x = unit(7.5, "mm"),
    panel.spacing.y = unit(5, "mm")
  )

p.inequality.projection

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "an-fa4" ),
  p = p.inequality.projection,
  w = 275,
  h = 100
)

##### Range of reduction across countries [5-95th] range, SDP-RC ---------------
df.core %>%
  filter(year%in%c(2020,2040),
         scenario=="SDP_RC-1p5C") %>%
  select(year,iso,gini) %>%
  distinct() %>%
  pivot_wider(values_from = gini, names_from = year) %>%
  summarise(
    p5 = quantile(`2020`-`2040`, probs = 0.05),
    p95 = quantile(`2020`-`2040`, probs = 0.95)
  )



### Figure B1: country-level DLE gaps ------------------------------------------
BOTTOM.UP.DLE.GAP.FILE.validation <- "DLE_gaps.csv"
dle_gap_all.iso <- read_csv(file.path(PATH.data.desire, "other", "dle-gaps", BOTTOM.UP.DLE.GAP.FILE.validation)) %>%
  left_join(
    population.iso.2020.base %>% rename(pop_mil_2020=pop_mil)
  ) %>%
  mutate_cond(
    variable%in%c("Heating OP|total",
                  "Heating CON|total",
                  "Hot Water OP|total"), variable = "Space and water heating"
  ) %>%
  mutate_cond(
    variable%in%c("Cooling OP|total",
                  "Cooling CON|total"), variable = "Space cooling"
  ) %>%
  mutate_cond(
    variable%in%c("Education|lower_secondary",
                  "Education|primary"), variable = "Education"
  ) %>%

  mutate_cond(
    variable%in%c("Appliance|clean_cooking_fuel",
                  "Appliance|refrigerator",
                  "Appliance|television",
                  "Appliance|mobile_telephone"), variable = "Appliances"
  ) %>%
  reframe(
    gap = sum(gap)/1e3, # from MJ/cap to GJ/cap
    .by = c("iso")
  )
dle_gap_all.iso.DESIRE <- df.core %>% filter(year==2020, scenario==vis.scenario,model==vis.model) %>%
  reframe(

    energy.development.gap = sum(share.below.projected.adjusted*depth.below.projected.adjusted),

    .by = c("iso", "pop_mil")
  )

dle_gap_comparison <- dle_gap_all.iso %>% left_join(dle_gap_all.iso.DESIRE)

p.dle_gap_comparison.national <- ggplot(dle_gap_comparison, aes(x=gap, y=energy.development.gap)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  geom_textabline(slope = 1, intercept = -5, label = "- 5GJ", linetype = "dotted", color = "grey") +
  geom_textabline(slope = 1, intercept = 5, label = "+ 5GJ", linetype = "dotted", color = "grey") +
  geom_point(
    aes()
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  labs(
    x = "Bottom up energy needs gap (GJ/cap)",
    y = "DESIRE energy needs gap (GJ/cap)"
  ) +
  theme_classic() +
  labs(caption = "Dashed grey line: 1:1 line\nSolid black line: linear regression") +
  scale_x_continuous(limits = c(0, 18), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 22), expand = c(0,0))
p.dle_gap_comparison.national

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "an-fb1" ),
  p = p.dle_gap_comparison.national,
  w = 150,
  h = 150
)

### Figure B2: DLS gaps vs DLE gaps --------------------------------------------

##### Auxiliary functions ------------------------------------------------------

simplify_dls_dimension_names <- function(dls.data){
  dls.data <- dls.data %>%
    mutate(
      dls = case_when(

        dls == "Appliance|clean_cooking_fuel" ~ "Clean cooking",
        dls == "Appliance|mobile_telephone" ~ "Mobile telephone",
        dls == "Appliance|refrigerator" ~ "Cold storage",
        dls == "Appliance|television" ~ "Television",
        dls == "Clothing|clothing" ~ "Clothing",
        dls == "Clothing|footwear" ~ "Footwear",
        dls == "Cooling CON|rural" ~ "Cooling (rural)",
        dls == "Cooling CON|total" ~ "Cooling",
        dls == "Cooling CON|urban" ~ "Cooling (urban)",
        dls == "Education|lower_secondary" ~ "Education (lower secondary)",
        dls == "Education|primary" ~ "Education (primary)",
        dls == "Health care" ~ "Health care",
        dls == "Heating CON|rural" ~ "Heating (rural)",
        dls == "Heating CON|total" ~ "Heating",
        dls == "Heating CON|urban" ~ "Heating (urban)",
        dls == "Hot Water OP|rural" ~ "Hot water (rural)",
        dls == "Hot Water OP|total" ~ "Hot water",
        dls == "Hot Water OP|urban" ~ "Hot water (urban)",
        dls == "Housing|rural" ~ "Housing (rural)",
        dls == "Housing|total" ~ "Housing",
        dls == "Housing|urban" ~ "Housing (urban)",
        dls == "Nutrition" ~ "Nutrition",
        dls == "Roads" ~ "Roads",
        dls == "Sanitation" ~ "Sanitation",
        dls == "Transport" ~ "Transport",
        dls == "Water" ~ "Water",

        TRUE ~ dls
      )
    )
  return(dls.data)
}

from_dls_data_to_relative_to_threshold <- function(dls.data){

  dls.gap <- dls.data %>%
    filter(
      type == "Service gap"
    )

  dls.thres <- dls.data %>%
    filter(
      type == "Decent Living Standard threshold"
    )

  dls.relative.to.threshold <- dls.gap %>% rename(gap=value) %>% select(iso,variable,unit,gap) %>%
    left_join(dls.thres %>% rename(thres=value) %>% select(iso,variable,unit,thres)) %>%
    mutate(`Service gap in % of the DLS service threshold` = ifelse(
      gap==0, 0, gap/thres
    ))


  return(dls.relative.to.threshold)
}

add_energy_gap_to_desire_output <- function(desire.data){

  dle.gap <- desire.data %>%
    mutate(dle.gap.curtech = share.below.projected.curtech*depth.below.projected.curtech,
           dle.gap.adjusted = share.below.projected.adjusted*depth.below.projected.adjusted)

  return(dle.gap)
}

add_relative_energy_gap_to_desire_output <- function(desire.data){

  dle.relative.gap <- desire.data %>%
    mutate(
      dle.relative.gap.curtech = ifelse(
        (share.below.projected.curtech*depth.below.projected.curtech)==0, 0,
        share.below.projected.curtech*depth.below.projected.curtech / dle.threshold.curtech
      ),
      dle.relative.gap.adjusted = ifelse(
        (share.below.projected.adjusted*depth.below.projected.adjusted)==0, 0,
        share.below.projected.adjusted*depth.below.projected.adjusted / dle.threshold.adjusted
      )
    )

  return(dle.relative.gap)
}

aggregate_region_desire_output <- function(desire.data,
                                           pop.var = "pop_mil",
                                           region.var = NULL,
                                           agg.var = NULL,
                                           group.cols = c("model",
                                                          "scenario",
                                                          # "iso",
                                                          "variable",
                                                          "unit",
                                                          "year" ),
                                           ignore.NA = T){

  population.region <- desire.data %>%
    reframe(
      region.population = sum((!!as.name(pop.var)), na.rm = ignore.NA),
      .by = c(group.cols, region.var)
    )

  agg.data <- desire.data %>% left_join(population.region) %>%
    reframe(
      region.aggregated = weighted.mean(x = (!!as.name(agg.var)),
                                        w = (!!as.name(pop.var))/region.population,
                                        na.rm = ignore.NA),
      .by = c(group.cols, region.var)
    )

  return(agg.data)
}

##### Data preparation: national + regional aggregates  ------------------------

# Mapping DLS and DLE variables
map.headcount.dls.dle <- data.frame(
  dle = c(
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",

    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",

    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",

    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",

    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",
    "Final Energy|Industry",

    "Final Energy|Industry",
    "Final Energy|Industry",
    "Final Energy|Residential and Commercial",

    "Final Energy|Residential and Commercial",
    "Final Energy|Transportation",
    "Final Energy|Residential and Commercial"
  ),
  dls = c(
    "Appliance|clean_cooking_fuel",
    "Appliance|mobile_telephone",
    "Appliance|refrigerator",

    "Appliance|television",
    "Cooling CON|rural",
    "Cooling CON|total",

    "Cooling CON|urban",
    "Education",
    "Heating CON|rural",

    "Heating CON|total",
    "Heating CON|urban",
    "Hot Water OP|rural",

    "Hot Water OP|total",
    "Hot Water OP|urban",
    "Housing|rural",

    "Housing|total",
    "Housing|urban",
    "Nutrition",

    "Sanitation",
    "Transport",
    "Water"
  )
)

map.servicegap.dls.dle <- data.frame(
  dle = c(
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",
    "Final Energy|Industry",

    "Final Energy|Industry",
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",

    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",

    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial",
    "Final Energy|Residential and Commercial", # alternatively: industry
    "Final Energy|Residential and Commercial", # alternatively: industry

    "Final Energy|Residential and Commercial", # alternatively: industry
    "Final Energy|Residential and Commercial",
    "Final Energy|Transportation",
    "Final Energy|Residential and Commercial",
    "Final Energy|Transportation",

    "Final Energy|Residential and Commercial"
  ),
  dls = c(
    "Appliance|clean_cooking_fuel",
    "Appliance|mobile_telephone",
    "Appliance|refrigerator",
    "Appliance|television",
    "Clothing|clothing",

    "Clothing|footwear",
    "Cooling CON|rural",
    "Cooling CON|total",
    "Cooling CON|urban",
    "Education|lower_secondary",

    "Education|primary",
    "Health care",
    "Heating CON|rural",
    "Heating CON|total",
    "Heating CON|urban",

    "Hot Water OP|rural",
    "Hot Water OP|total",
    "Hot Water OP|urban",
    "Housing|rural",
    "Housing|total",

    "Housing|urban",
    "Nutrition",
    "Roads",
    "Sanitation",
    "Transport",

    "Water"
  )
)

##### Calculating DLS and DLE variables ----------------------------------------

dle.base.year <- df.core %>% filter(year==2020)
dle.headcount.base.year.iso <- dle.base.year %>% filter(model==vis.model, scenario==vis.scenario)

dle.headcount.base.year.r10 <- dle.headcount.base.year.iso %>%
  left_join(ipcc.r10) %>%
  aggregate_region_desire_output(agg.var="share.below.projected.curtech",
                                 region.var = "r10") %>%
  rename(`Regional deprivation headcount (%)` = region.aggregated)
dle.gap.base.year.iso <- dle.base.year %>% add_relative_energy_gap_to_desire_output() %>% filter(model==vis.model, scenario==vis.scenario)
dle.gap.base.year.r10 <- dle.gap.base.year.iso %>%
  left_join(ipcc.r10) %>%
  aggregate_region_desire_output(agg.var="dle.relative.gap.curtech",
                                 region.var = "r10") %>%
  rename(`Energy gap in % of the DLE threshold` = region.aggregated)

dls.service.gap.iso <- read_csv(
  f.dls.data
) %>%
  from_dls_data_to_relative_to_threshold()
dls.service.gap.r10 <- dls.service.gap.iso %>% # 26 variables
  left_join(population.iso.2020.base) %>% left_join(ipcc.r10) %>%
  aggregate_region_desire_output(agg.var="Service gap in % of the DLS service threshold",
                                 group.cols = c("variable","unit"),
                                 region.var = "r10",
                                 ignore.NA = T) %>%
  rename(`Service gap in % of the DLS service threshold` = region.aggregated)

dls.headcount.iso <- read_csv(
  f.dls.data
) %>%
  filter(type=="Deprivation headcount")
dls.headcount.r10 <- dls.headcount.iso %>% # 21 variables
  left_join(population.iso.2020.base %>% select(-year)) %>% left_join(ipcc.r10) %>%
  aggregate_region_desire_output(agg.var="value",
                                 group.cols = c("variable","unit"),
                                 region.var = "r10",
                                 ignore.NA = T) %>%
  rename(`DLS deprivation headcount in %` = region.aggregated)

# r10
headcount.data <- dls.headcount.r10 %>% rename(dls=variable) %>%
  left_join(map.headcount.dls.dle %>% rename(variable=dle) %>% iamc_variable_keep_one_level(level = -1)) %>%
  filter(!grepl(dls, pattern="urban", fixed=T),
         !grepl(dls, pattern="rural", fixed=T)) %>%
  # dle
  left_join(
    dle.headcount.base.year.r10  %>%
      iamc_variable_keep_one_level(level = -1) %>%
      select(-year,-unit,-model,-scenario) %>%
      rename(`DLE deprivation headcount in %` = `Regional deprivation headcount (%)`)
  )
gaps.data <- dls.service.gap.r10 %>% rename(dls=variable) %>%
  left_join(map.servicegap.dls.dle %>% rename(variable=dle) %>% iamc_variable_keep_one_level(level = -1)) %>%
  filter(!grepl(dls, pattern="urban", fixed=T),
         !grepl(dls, pattern="rural", fixed=T),
         !grepl(dls, pattern="Clothing", fixed=T)) %>%
  # dle
  left_join(
    dle.gap.base.year.r10  %>%
      iamc_variable_keep_one_level(level = -1) %>%
      select(-year,-unit,-model,-scenario)
  )
headcount.data.iso <- dls.headcount.iso %>% rename(dls=variable) %>% left_join(map.headcount.dls.dle %>% rename(variable=dle) %>% iamc_variable_keep_one_level(level = -1)) %>%
  filter(!grepl(dls, pattern="urban", fixed=T),
         !grepl(dls, pattern="rural", fixed=T)) %>%
  rename(`DLS deprivation headcount in %`=value) %>%
  select(dls,variable,iso,`DLS deprivation headcount in %`) %>%
  # dle
  left_join(
    dle.headcount.base.year.iso  %>%
      iamc_variable_keep_one_level(level = -1) %>%
      rename(`DLE deprivation headcount in %` = share.below.projected.curtech) %>%
      select(variable,iso,`DLE deprivation headcount in %`)
  )
gaps.data.iso <- dls.service.gap.iso %>% rename(dls=variable) %>% left_join(map.servicegap.dls.dle %>% rename(variable=dle) %>% iamc_variable_keep_one_level(level = -1)) %>%
  filter(!grepl(dls, pattern="urban", fixed=T),
         !grepl(dls, pattern="rural", fixed=T),
         !grepl(dls, pattern="Clothing", fixed=T)) %>%
  select(dls,variable,iso,
         `Service gap in % of the DLS service threshold`) %>%
  # dle
  left_join(
    dle.gap.base.year.iso  %>%
      iamc_variable_keep_one_level(level = -1) %>%
      rename(`Energy gap in % of the DLE threshold` = dle.relative.gap.curtech) %>%
      select(variable,iso,
             `Energy gap in % of the DLE threshold`)
  )

# combining
grouped.correlation.plot.data <- gaps.data.iso %>%
  rename(`Service`=`Service gap in % of the DLS service threshold`,
         `Energy` = `Energy gap in % of the DLE threshold`) %>%
  pivot_wider(names_from = variable,
              values_from = c(`Service`,
                              `Energy`),
              names_prefix = "gap_",
              names_sep = "_"
  ) %>%
  left_join(
    headcount.data.iso %>%
      rename(`DLS` = `DLS deprivation headcount in %`,
             `DLE` = `DLE deprivation headcount in %`) %>%
      pivot_wider(names_from = variable,
                  values_from = c(`DLS`,
                                  `DLE`),
                  names_prefix = "headcount_",
                  names_sep = "_"
      )
  )
rescom.min.dls.grouped.correlation.plot.data <- grouped.correlation.plot.data %>%
  select(dls,iso,ends_with("Residential and Commercial")) %>% drop_na() %>%
  reframe(
    dle_gap = first(`Energy_gap_Residential and Commercial`),
    dle_headcount = first(`DLE_headcount_Residential and Commercial`),

    dls_headcount_max = max(`DLS_headcount_Residential and Commercial`),
    dls_headcount_min = min(`DLS_headcount_Residential and Commercial`),
    dls_headcount_avg = mean(`DLS_headcount_Residential and Commercial`),

    dls_gap_max = max(`Service_gap_Residential and Commercial`),
    dls_gap_min = min(`Service_gap_Residential and Commercial`),
    dls_gap_avg = mean(`Service_gap_Residential and Commercial`),
    .by = c("iso")
  )


##### [A]: DLS specific variables ----------------------------------------------
p.ann.b2.a <- ggplot(
  data = gaps.data.iso %>% simplify_dls_dimension_names() %>%
    mutate(across(dls, ~factor(., levels=c(
      "Clean cooking", "Cold storage", "Cooling", "Education (primary)",
      "Education (lower secondary)", "Health care", "Heating", "Hot water",
      "Housing", "Mobile telephone", "Nutrition", "Sanitation",
      "Television", "Water", "Roads", "Transport"
    )))),
  mapping = aes(x = `Energy gap in % of the DLE threshold`,
                y = `Service gap in % of the DLS service threshold`)
) +
  facet_wrap(dls~., nrow = 4) +

  geom_rect(
    data = . %>% group_by(dls) %>% filter(row_number()==1),
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
        fill = variable),
    alpha = 0.1
  ) +

  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  geom_point(alpha=0.1) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "dodgerblue"
  ) +
  theme_bw() +
  geom_text(data = . %>% group_by(dls) %>% filter(row_number()==1),
            x = 0.01, y = 0.95,
            aes(label = variable),
            hjust = 0) +
  scale_fill_ptol() +
  scale_x_continuous(breaks = seq(0, 1, 0.5),
                     name = "Relative energy needs gap",
                     labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 1, 0.5),
                     name = "Relative DLS gap",
                     labels = scales::percent) +
  coord_cartesian(xlim = c(-0, 1), ylim = c(-0, 1)) +
  theme(legend.position = "none") +
  labs(subtitle = bquote("Comparing" ~ bold("DLS gaps") ~ "to the" ~ bold("energy needs gap")))

p.ann.b2.a








##### [B]: Highlight overall high-level correlations (headcounts) ----------
ann.b2.data.headcount <- rescom.min.dls.grouped.correlation.plot.data %>%
  select(
    dle_headcount,

    dls_headcount_max,
    dls_headcount_min,
    dls_headcount_avg,

  ) %>%
  rename(
    `Maximum headcount across DLS dimensions`=dls_headcount_max,
    `Mean headcount across DLS dimensions`=dls_headcount_avg,
    `Minimum headcount across DLS dimensions`=dls_headcount_min
  ) %>%
  pivot_longer(cols = -dle_headcount,
               names_to = "DLS dimensions aggregation",
               values_to = "value")


p.ann.b2.b <- ggplot(ann.b2.data.headcount,
                          mapping = aes(x = 1-dle_headcount,
                                        y = 1-value,
                                        color = `DLS dimensions aggregation`)) +
  geom_point(size = 3, alpha = 0.2) +
  geom_smooth(method = "lm",
              # n = 3,
              se = TRUE) +
  theme_bw() +
  scale_color_bluebrown() +
  scale_x_continuous(breaks = seq(0, 1, 0.5),
                     name = "Share of population achieving DLE",
                     labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 1, 0.5),
                     name = "Share of population achieving aggregate DLS index",
                     labels = scales::percent) +
  labs(subtitle = bquote("Residential and Commercial Energy and DLS" ~ bold("Deprivation rates"))) +
  coord_cartesian(xlim = c(-0, 1), ylim = c(-0, 1)) +
  theme(legend.position = "none")

##### [C]: Highlight overall high-level correlations (gaps) ----------

ann.b2.data.gap <- rescom.min.dls.grouped.correlation.plot.data %>%
  select(
    dle_gap,
    # dle_headcount,

    # dls_headcount_max,
    # dls_headcount_min,
    # dls_headcount_avg,

    dls_gap_max,
    dls_gap_avg,
    dls_gap_min
  ) %>%
  rename(
    `Maximum headcount across DLS dimensions`=dls_gap_max,
    `Mean headcount across DLS dimensions`=dls_gap_avg,
    `Minimum headcount across DLS dimensions`=dls_gap_min
  ) %>%
  pivot_longer(cols = -dle_gap,
               names_to = "DLS dimensions aggregation",
               values_to = "value")
p.ann.b2.c <- ggplot(ann.b2.data.gap,
                    mapping = aes(x = 1-dle_gap,
                                  y = 1-value,
                                  color = `DLS dimensions aggregation`)) +
  geom_point(size = 3, alpha = 0.2) +
  geom_smooth(method = "lm",
              # n = 10,
              se = TRUE) +
  theme_bw() +
  scale_color_bluebrown() +
  scale_x_continuous(breaks = seq(0, 1, 0.5),
                     name = "Relative DLE gap",
                     labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 1, 0.5),
                     name = "Relative DLS gap",
                     labels = scales::percent) +
  labs(subtitle = bquote("Residential and Commercial Energy and DLS" ~ bold("Gaps"))) +
  coord_cartesian(xlim = c(-0, 1), ylim = c(-0, 1))



##### Combine [A+B+C] ----------------------------------------------------------

p.ann.b2.bc <- ((p.ann.b2.b) | (p.ann.b2.c))

p.ann.b2 <- ((p.ann.b2.a) / (p.ann.b2.bc)) +
  plot_layout(
    design = c(
      "
      AA
      AA
      AA
      CC
      CC
      "
    )
  ) +
  plot_annotation(tag_levels = "A")
p.ann.b2

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "an-fb2" ),
  p = p.ann.b2,
  w = 400,
  h = 400
)






### Figure C1: income vs DLE deprivation headcounts ----------------------------

df.income.figure <- df.core %>% filter(year%in%c(2020,2025,2030,2035,2040)) %>%
  left_join(gdp.iso %>% select(-variable,-unit))


p.income.figure <- ggplot(df.income.figure %>% filter(scenario%in%shape.sdps) %>%
                            filter(variable %in%c(
                              "Final Energy|Residential and Commercial",
                              "Final Energy|Transportation"
                            )) %>% rename.models.and.scenarios() %>% iamc_variable_keep_one_level(level=-1),
                          aes(x=gdp_thousand_percapita,
                              y=share.below.projected.adjusted,
                              # y=energy.per.capita.uncorrected,
                              colour=as.character(year)
                              )) +
  facet_grid(variable~scenario) +
  geom_point(alpha=0.1, size = 3) +
  geom_smooth(method="lm",
              aes(fill=as.character(year))) +
  geom_xsidedensity(
    aes(
      y = after_stat(ndensity)
    )
  ) +
  geom_ysidedensity(
    aes(
      x = after_stat(ndensity)
    )
  ) +
  labs(title="GDP (PPP) per capita vs. DLE headcount",
       subtitle = "Each dot represents one country",
       x="Income per capita",
       y="DLE deprivation rate",
       colour="ISO",
       shape="Year") +
  theme_minimal() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_manual(breaks = c(2020,2025,2030,2035,2040) %>% as.character(),
                     values = c("#424242","#767676","#bdbcbc","lightgrey","dodgerblue")) +
  scale_fill_manual(breaks = c(2020,2025,2030,2035,2040) %>% as.character(),
                    values = c("#424242","#767676","#bdbcbc","lightgrey","dodgerblue")) +
  ggside(#x.pos = "bottom",
         #y.pos = "left",
         scales = "free") +
  scale_ysidex_continuous(minor_breaks = NULL, #trans = "reverse",
                          breaks = NULL) +
  scale_xsidey_continuous(minor_breaks = NULL, #trans = "reverse",
                          breaks = NULL) +
  theme(ggside.panel.scale = .2,
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = NULL),
         colour = "none")

p.income.figure

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "an-fc1" ),
  p = p.income.figure,
  w = 200,
  h = 150
)


















# . ----------------------------------------------------------------------------
# . ----------------------------------------------------------------------------
# . ----------------------------------------------------------------------------
# SUPPLEMENTARY INFORMATION ----------------------------------------------------




### ST.1. Two/Three wheeler p-km table -----------------------------------------------
# Not calculated in this script; directly from data and in the manuscript.


### SI.1. P-km and modes update comparison -------------------------------------------

pkm_by_country <- read_csv(file = file.path(PATH.data.for.figures,
                                            "transport_pkm_data",
                                            "20240214_DLS_transport_pkm_collection.csv")) %>%
  left_join(population.iso.2020.base %>% select(-year))

# weight by population
pkm_by_country.weighted.population <- pkm_by_country[rep(seq_along(pkm_by_country$pop_mil), pkm_by_country$pop_mil), ]

p.pkm.bars <- ggplot(pkm_by_country.weighted.population %>% filter(data.version %in% c("IMAGE2023-4modes-add23",
                                                               "ERL2021-4modes")) %>%
                   select(-c("walking","cycling","aviation","sum.of.shares")) %>%
                   pivot_longer(cols=ldv:rail,
                                names_to="transport.mode",
                                values_to="value") %>%
                   mutate_cond(data.version=="IMAGE2023-4modes-add23",
                               data.version="This manuscript") %>%
                   mutate_cond(data.version=="ERL2021-4modes",
                               data.version="Kikstra et al. (2021)"),
                 aes(y=transport.mode,colour=`data.version`, x=value)) +
  facet_wrap(.~variable, scales = "free", nrow = 1) +
  geom_density_ridges(#rel_min_height = 0.05,
    alpha=0.2,
    aes(height = stat(density)),
    stat = "binline", bins=10,
    scale = 1.05, draw_baseline = FALSE
  ) +
  xlab(NULL) +
  ylab("Mode of transport") +
  theme_minimal() +
  theme_hc() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Modal shares and pkm/cap/year (distribution of global population)") +
  theme(legend.title = element_blank())
p.pkm.bars

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-pkm-update-populationweighted"),
  p = p.pkm.bars,
  w = 200,
  h = 100
)

### SI.2. DLS updates ----------------------------------------------------------
dls.erl.2021 <- vroom(file.path(PATH.data.for.figures, "kikstra_erl2021", "DLS_deprivations_website_v1.csv"))

dls.erl.2024 <- vroom(f.dls.data)

dls.headcount.compared <- dls.erl.2021 %>%
  pivot_longer(cols = 3:ncol(dls.erl.2021),
               names_to = "DLS dimension",
               values_to = "Deprivation headcount (% of population below treshold)") %>%
  mutate(`Data version` = "Kikstra et al. (2021)") %>%
  select(-country_name) %>%
  rename(
    `DLS deprivation rate`=`Deprivation headcount (% of population below treshold)`
  ) %>%

  # new
  bind_rows(
    dls.erl.2024 %>%
      filter(type=="Deprivation headcount") %>%
      # from_dls_data_to_relative_to_threshold() %>%
      mutate(
        `Data version` = "This manuscript"
      ) %>%
      rename(`DLS dimension`=variable,
             `DLS deprivation rate`=`value`)
  ) %>%

  # now add a common DLS dimension name for plotting
  mutate(dls_common_name=NA) %>%
  mutate_cond(grepl(`DLS dimension`, pattern="Transport", fixed = T), dls_common_name = "Transport") %>%
  mutate_cond(grepl(`DLS dimension`, pattern="ooking", fixed = T), dls_common_name = "Clean cooking") %>%
  mutate_cond(grepl(`DLS dimension`, pattern="tor", fixed = T), dls_common_name = "Cold storage") %>%
  mutate_cond(grepl(`DLS dimension`, pattern="elevision", fixed = T), dls_common_name = "Television") %>%
  mutate_cond(grepl(`DLS dimension`, pattern="telephone", fixed = T), dls_common_name = "Mobile telephone") %>%
  mutate_cond(grepl(`DLS dimension`, pattern="Water", fixed = T), dls_common_name = "Clean and hot\nwater access") %>%
  mutate_cond(grepl(`DLS dimension`, pattern="Sanitation", fixed = T), dls_common_name = "Sanitation") %>%
  mutate_cond(grepl(`DLS dimension`, pattern="Nutrition", fixed = T), dls_common_name = "Nutrition") %>%
  mutate_cond(grepl(`DLS dimension`, pattern="Education", fixed = T), dls_common_name = "Education") %>%
  mutate_cond(grepl(`DLS dimension`, pattern="Housing", fixed = T), dls_common_name = "Housing") %>%
  mutate_cond(grepl(`DLS dimension`, pattern="Cooling", fixed = T), dls_common_name = "Cooling") %>%
  mutate_cond(grepl(`DLS dimension`, pattern="Heating", fixed = T), dls_common_name = "Heating") %>%

  # align old and new naming
  mutate_cond(`DLS dimension`=="Heating CON|total", `DLS dimension` = "Heating") %>%
  mutate_cond(`DLS dimension`=="Heating CON|urban", `DLS dimension` = "Heating (urban)") %>%
  mutate_cond(`DLS dimension`=="Heating CON|rural", `DLS dimension` = "Heating (rural)") %>%
  mutate_cond(`DLS dimension`=="Cooling CON|total", `DLS dimension` = "Cooling") %>%
  mutate_cond(`DLS dimension`=="Cooling CON|urban", `DLS dimension` = "Cooling (urban)") %>%
  mutate_cond(`DLS dimension`=="Cooling CON|rural", `DLS dimension` = "Cooling (rural)") %>%
  mutate_cond(`DLS dimension`=="Housing|total", `DLS dimension` = "Housing") %>%
  mutate_cond(`DLS dimension`=="Housing|urban", `DLS dimension` = "Housing (urban)") %>%
  mutate_cond(`DLS dimension`=="Housing|rural", `DLS dimension` = "Housing (rural)") %>%
  mutate_cond(`DLS dimension`=="Hot Water OP|total", `DLS dimension` = "Hot Water") %>%
  mutate_cond(`DLS dimension`=="Hot Water OP|urban", `DLS dimension` = "Hot Water (urban)") %>%
  mutate_cond(`DLS dimension`=="Hot Water OP|rural", `DLS dimension` = "Hot Water (rural)") %>%
  mutate_cond(`DLS dimension`=="Appliance|mobile_telephone", `DLS dimension` = "Mobile telephone") %>%
  mutate_cond(`DLS dimension`=="Appliance|refrigerator", `DLS dimension` = "Cold storage") %>%
  mutate_cond(`DLS dimension`=="Appliance|television", `DLS dimension` = "Television") %>%
  mutate_cond(`DLS dimension`=="Appliance|clean_cooking_fuel", `DLS dimension` = "Clean cooking") %>%
  mutate_cond(`DLS dimension`=="Water", `DLS dimension` = "Water access")

dls.headcount.compared <- dls.headcount.compared %>% left_join(population.iso.2020.base %>% select(-year))

# weight by population
dls.headcount.compared.weighted.population <- dls.headcount.compared[rep(seq_along(dls.headcount.compared$pop_mil), dls.headcount.compared$pop_mil), ]


p.headcount.comparison.new.vs.old <- ggplot(
  data = dls.headcount.compared.weighted.population %>% filter(
    !grepl(`DLS dimension`, pattern="rural", fixed = T),
    !grepl(`DLS dimension`, pattern="urban", fixed = T)
  ),
  aes(x=`DLS deprivation rate`)
) +
  facet_wrap(`dls_common_name`~.,scales="free_y") +

  geom_density_ridges(#rel_min_height = 0.05,
    alpha=0.2,
    aes(height = stat(density),
        y = `DLS dimension`,
        colour=`DLS dimension`,
        linetype=`Data version`),
    stat = "binline", bins=10,
    boundary = 0,
    scale = 1.5, draw_baseline = FALSE
    # jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0), point_shape = '|', point_size = 3, point_alpha = 0.1
  ) +

  # geom_density(aes(colour=`DLS dimension`,linetype=`Data version`),
  #              alpha=0.3) +


  scale_linetype_manual(values=c("dashed","solid")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(expand = c(0, 0, 0, 0)) +
  scale_x_continuous(limits = c(0,1))
p.headcount.comparison.new.vs.old



save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-dls-headcount-update"),
  p = p.headcount.comparison.new.vs.old,
  w = 250,
  h = 150
)



### ST.2. LCA mapping matrix ---------------------------------------------------
# Not calculated in this script; directly from data and in the manuscript.


### SI.3. DLE-to-IAM all countries ---------------------------------------------
##### All countries, like Annex A2 -----------------------------------
p.threshold.data.mapping.dle.to.iam.supplement.allcountries <- ggplot(
  p.threshold.data.mapping.dle.to.iam.data %>%
    filter(sector %in% c("Final Energy|Residential and Commercial",
                         "Final Energy|Industry",
                         "Final Energy|Transportation")
    ) %>%
    mutate_cond(sector == "Final Energy|Industry", sector = "Industry") %>%
    mutate_cond(sector == "Final Energy|Transportation", sector = "Transportation") %>%
    mutate_cond(sector == "Final Energy|Residential and Commercial", sector = "Resid. and Commercial") %>%
    left_join(load_long_names_of_iso3c()) %>%
    mutate_cond(grepl(name,pattern = "Bolivia"), name = "Bolivia") %>%
    mutate_cond(grepl(name,pattern = "Tanzania"), name = "Tanzania") %>%
    mutate_cond(grepl(name,pattern = "Venezuela"), name = "Venezuela") %>%
    mutate_cond(grepl(name,pattern = "Vincent"), name = "St. Vincent/Grenadines") %>%
    mutate_cond(grepl(name,pattern = "Korea, Dem. People's Rep."), name = "North Korea") %>%
    mutate_cond(grepl(name,pattern = "Lao People's Democratic Republic"), name = "Laos") %>%
    mutate_cond(grepl(name,pattern = "United Kingdom of Great Britain and Northern Ireland"), name = "United Kingdom") %>%
    mutate_cond(grepl(name,pattern = "Democratic Republic of the Congo"), name = "DR Congo"),

) +
  facet_wrap(~name, ncol = 9) +
  geom_col(aes(y = sector, x = mj_percap/1e3, fill = `DLS dimension`), position = "stack") +
  scale_fill_jco() +
  ylab(NULL) +
  xlab("GJ/cap/yr for DLE") +
  guides(fill = guide_legend("")) +
  theme_classic() +
  theme_hc() +
  theme(panel.grid.major.x = element_line(colour = "lightgrey"),
        panel.grid.major.y = element_blank())



save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, paste0("si-dle-to-iam-mapping-allcountries") ),
  p = p.threshold.data.mapping.dle.to.iam.supplement.allcountries,
  w = 400,
  h = 500
)


### SI.4-9 DLE service provisioning efficiency indicators -----------------------------

rename_variables_SEF <- function(df){
  return(
    df %>%
      mutate_cond(grepl(variable,pattern="Floor Space", fixed=T), variable="Floor space") %>%
      mutate_cond(grepl(variable,pattern="Useful", fixed=T), variable="Useful energy") %>%
      mutate_cond(grepl(variable,pattern="Final", fixed=T), variable="Final energy") %>%
      mutate_cond(grepl(variable,pattern="Passenger", fixed=T), variable="Passenger-kilometre")
  )
}
rename_variables_SEF_industry <- function(df){
  return(
    df %>%
      mutate_cond(grepl(variable,pattern="Production|Non-Metallic Minerals|Cement|Volume", fixed=T), variable="Production (cement)") %>%
      mutate_cond(grepl(variable,pattern="Production|Iron and Steel|Volume", fixed=T), variable="Production (steel)") %>%
      mutate_cond(grepl(variable,pattern="Final Energy|Industry|Non-Metallic Minerals", fixed=T), variable="Energy (cement)") %>%
      mutate_cond(grepl(variable,pattern="Final Energy|Industry|Iron and Steel", fixed=T), variable="Energy (steel)")

  )
}

##### SI.4. RESCOM (IMAGE) -----------------------------
image.data.sef.rescom <- load_excel_iamc(
  file.path(PATH.data.for.figures, "iam-data", "regional", FILE.IMAGE.IAM.REGIONAL.DATA)
) %>% filter(#Region=="World",
             Variable%in%c("Final Energy|Residential",
                                            "Energy Service|Residential|Floor Space",
                                            "Useful Energy|Residential")) %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  normalise_iamc_long(starting.year = 2020)
image.sef.rescom <- image.data.sef.rescom %>% select(-unit) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(value = `Energy Service|Residential|Floor Space`/`Final Energy|Residential`) %>%
  mutate(variable = "Scaling (using floor space)") %>%
  select(model, scenario, variable, year, region, value)
image.sef.rescom.alternative <- image.data.sef.rescom %>% select(-unit) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(value = `Useful Energy|Residential`/`Final Energy|Residential`) %>%
  mutate(variable = "Scaling (using useful energy)") %>%
  select(model, scenario, variable, year, region, value)


p.sef.rescom.image <-
  ggplot(
    data = image.data.sef.rescom %>%
      filter(scenario%in%shape.core.scenarios,
             region=="World",
             year>=2020, year<=2040) %>%
      rename.models.and.scenarios() %>%
      rename_variables_SEF(),
    aes(x = year, y = value,
        label = variable)
  ) +
  facet_grid(region~scenario) +
  geom_textpath(linewidth = 0.9, colour = "black") +
  geom_textpath(data= image.sef.rescom %>% filter(scenario%in%shape.core.scenarios,
                                              region=="World",
                                              year>=2020, year<=2040) %>%
              rename.models.and.scenarios(), linewidth = 1.3, colour = "navyblue") +
  geom_textpath(data= image.sef.rescom.alternative %>% filter(scenario%in%shape.core.scenarios,
                                                          region=="World",
                                                          year>=2020, year<=2040) %>%
              rename.models.and.scenarios(), linewidth = 1.3, colour = "darkgrey") +

  labs(title = "Service provisioning efficiency factor indicators",
       subtitle = "IMAGE, for Residential and Commercial") +
  scale_color_npg() +
  ylab("Value normalised to 2020") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y.right = element_text(angle = 0))

p.sef.rescom.image

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           paste0("si-SEF-rescom-IMAGE-world") ),
  p = p.sef.rescom.image,
  w = 400,
  h = 110
)


##### SI.5. RESCOM (REMIND) -----------------------------
remind.data.sef.rescom.totals <- load_excel_iamc(
  file.path(PATH.data.for.figures, "iam-data", "regional", FILE.REMIND.IAM.REGIONAL.DATA)
) %>% filter(Variable%in%c("Final Energy|Residential and Commercial",
                           "Energy Service|Residential and Commercial|Floor Space")) %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  normalise_iamc_long(starting.year = 2020)

remind.data.sef.rescom.percapita <- load_excel_iamc(
  file.path(PATH.data.for.figures, "iam-data", "regional", FILE.REMIND.IAM.REGIONAL.DATA)
) %>% filter(Variable%in%c("Population",
                           "Useful Energy|Residential and Commercial")) %>%
  iamc_wide_to_long(upper.to.lower = T)
remind.data.sef.rescom.percapita.pop <- remind.data.sef.rescom.percapita %>% filter(variable=="Population") %>% rename(pop=value) %>% select(model,scenario,region,year,pop)
remind.data.sef.rescom.percapita <- remind.data.sef.rescom.percapita %>% filter(variable!="Population") %>%
  left_join(remind.data.sef.rescom.percapita.pop) %>%
  mutate(value=value*pop) %>%
  select(-pop)
remind.data.sef.rescom.percapita <- remind.data.sef.rescom.percapita %>%
  normalise_iamc_long(starting.year = 2020)

remind.data.sef.rescom <- remind.data.sef.rescom.totals %>%
  bind_rows(remind.data.sef.rescom.percapita)

remind.sef.rescom.alternative <- remind.data.sef.rescom %>% select(-unit) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(value = `Energy Service|Residential and Commercial|Floor Space`/`Final Energy|Residential and Commercial`) %>%
  mutate(variable = "Scaling (using floor space)") %>%
  select(model, scenario, variable, year, region, value)
remind.sef.rescom <- remind.data.sef.rescom %>% select(-unit) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(value = `Useful Energy|Residential and Commercial`/`Final Energy|Residential and Commercial`) %>%
  mutate(variable = "Scaling (using useful energy)") %>%
  select(model, scenario, variable, year, region, value)


p.sef.rescom.remind <-
  ggplot(
    data = remind.data.sef.rescom %>%
      filter(scenario%in%shape.core.scenarios,
             region=="World",
             year>=2020, year<=2040) %>%
      rename.models.and.scenarios() %>% rename_variables_SEF(),
    aes(x = year, y = value,
        label = variable)
  ) +
  facet_grid(region~scenario) +
  geom_textpath(linewidth = 0.9, colour = "black") +
  geom_textpath(data= remind.sef.rescom %>% filter(scenario%in%shape.core.scenarios,
                                                  region=="World",
                                                  year>=2020, year<=2040) %>%
                  rename.models.and.scenarios(), linewidth = 1.3, colour = "navyblue") +
  geom_textpath(data= remind.sef.rescom.alternative %>% filter(scenario%in%shape.core.scenarios,
                                                              region=="World",
                                                              year>=2020, year<=2040) %>%
                  rename.models.and.scenarios(), linewidth = 1.3, colour = "darkgrey") +

  labs(title = "Service provisioning efficiency factor indicators",
       subtitle = "REMIND, for Residential and Commercial") +
  scale_color_npg() +
  ylab("Value normalised to 2020") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y.right = element_text(angle = 0))

p.sef.rescom.remind

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           paste0("si-SEF-rescom-REMIND-world") ),
  p = p.sef.rescom.remind,
  w = 400,
  h = 110
)


##### SI.6. TRANSPORT (IMAGE) -----------------------------
image.data.sef.transport <- load_excel_iamc(
  here("data-raw", "scenario_data", "original_regional", FILE.IMAGE.IAM.REGIONAL.DATA)
) %>% filter(#Region=="World",
  Variable%in%c("Final Energy|Transportation",
                "Energy Service|Transportation|Passenger")) %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  normalise_iamc_long(starting.year = 2020)
image.sef.transport <- image.data.sef.transport %>% select(-unit) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(value = `Energy Service|Transportation|Passenger`/`Final Energy|Transportation`) %>%
  mutate(variable = "Scaling (using p-km)") %>%
  select(model, scenario, variable, year, region, value)


p.sef.transport.image <-
  ggplot(
    data = image.data.sef.transport %>%
      filter(scenario%in%shape.core.scenarios,
             region=="World",
             year>=2020, year<=2040) %>%
      rename.models.and.scenarios() %>%
      rename_variables_SEF(),
    aes(x = year, y = value,
        label = variable)
  ) +
  facet_grid(region~scenario) +
  geom_textpath(linewidth = 0.9, colour = "black") +
  geom_textpath(data= image.sef.transport %>% filter(scenario%in%shape.core.scenarios,
                                                  region=="World",
                                                  year>=2020, year<=2040) %>%
                  rename.models.and.scenarios(), linewidth = 1.3, colour = "navyblue") +

  labs(title = "Service provisioning efficiency factor indicators",
       subtitle = "IMAGE, for Transport") +
  scale_color_npg() +
  ylab("Value normalised to 2020") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y.right = element_text(angle = 0))

p.sef.transport.image

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           paste0("si-SEF-transport-IMAGE-world") ),
  p = p.sef.transport.image,
  w = 400,
  h = 110
)

##### SI.7. TRANSPORT (REMIND) -----------------------------
remind.data.sef.transport <- load_excel_iamc(
  file.path(PATH.data.for.figures, "iam-data", "regional", FILE.REMIND.IAM.REGIONAL.DATA)
) %>% filter(#Region=="World",
  Variable%in%c("Final Energy|Transportation",
                "Energy Service|Transportation|Passenger")) %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  normalise_iamc_long(starting.year = 2020)
remind.sef.transport <- remind.data.sef.transport %>% select(-unit) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(value = `Energy Service|Transportation|Passenger`/`Final Energy|Transportation`) %>%
  mutate(variable = "Scaling (using p-km)") %>%
  select(model, scenario, variable, year, region, value)


p.sef.transport.remind <-
  ggplot(
    data = remind.data.sef.transport %>%
      filter(scenario%in%shape.core.scenarios,
             region=="World",
             year>=2020, year<=2040) %>%
      rename.models.and.scenarios() %>%
      rename_variables_SEF(),
    aes(x = year, y = value,
        label = variable)
  ) +
  facet_grid(region~scenario) +
  geom_textpath(linewidth = 0.9, colour = "black") +
  geom_textpath(data= remind.sef.transport %>% filter(scenario%in%shape.core.scenarios,
                                                     region=="World",
                                                     year>=2020, year<=2040) %>%
                  rename.models.and.scenarios(), linewidth = 1.3, colour = "navyblue") +

  labs(title = "Service provisioning efficiency factor indicators",
       subtitle = "REMIND, for Transport") +
  scale_color_npg() +
  ylab("Value normalised to 2020") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y.right = element_text(angle = 0))

p.sef.transport.remind

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           paste0("si-SEF-transport-REMIND-world") ),
  p = p.sef.transport.remind,
  w = 400,
  h = 110
)


##### SI.8. INDUSTRY (IMAGE) -----------------------------
image.data.sef.industry <- load_excel_iamc(
  here("data-raw", "scenario_data", "original_regional", FILE.IMAGE.IAM.REGIONAL.DATA)
) %>% filter(#Region=="World",
  Variable%in%c("Production|Non-Metallic Minerals|Cement|Volume",
                "Production|Iron and Steel|Volume",
                "Final Energy|Industry|Non-Metallic Minerals",
                "Final Energy|Industry|Iron and Steel")) %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  normalise_iamc_long(starting.year = 2020)
image.sef.industry <- image.data.sef.industry %>% select(-unit) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(value = (`Production|Non-Metallic Minerals|Cement|Volume`/`Final Energy|Industry|Non-Metallic Minerals`)*0.22+
           (`Production|Iron and Steel|Volume`/`Final Energy|Industry|Iron and Steel`)*0.78
  ) %>%
  mutate(variable = "Scaling (22 / 78)") %>%
  select(model, scenario, variable, year, region, value)
image.sef.industry.alternative <- image.data.sef.industry %>% select(-unit) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(value = (`Production|Non-Metallic Minerals|Cement|Volume`/`Final Energy|Industry|Non-Metallic Minerals`)*0.40+
           (`Production|Iron and Steel|Volume`/`Final Energy|Industry|Iron and Steel`)*0.60
  ) %>%
  mutate(variable = "Scaling (40 / 60)") %>%
  select(model, scenario, variable, year, region, value)


p.sef.industry.image <-
  ggplot(
    data = image.data.sef.industry %>%
      filter(scenario%in%shape.core.scenarios,
             region=="World",
             year>=2020, year<=2040) %>%
      rename.models.and.scenarios() %>%
      rename_variables_SEF_industry(),
    aes(x = year, y = value,
        label = variable)
  ) +
  facet_grid(region~scenario) +
  geom_textpath(linewidth = 0.9, colour = "black") +
  geom_textpath(data= image.sef.industry %>% filter(scenario%in%shape.core.scenarios,
                                                     region=="World",
                                                     year>=2020, year<=2040) %>%
                  rename.models.and.scenarios(), linewidth = 1.3, colour = "navyblue") +
  geom_textpath(data= image.sef.industry.alternative %>% filter(scenario%in%shape.core.scenarios,
                                                                 region=="World",
                                                                 year>=2020, year<=2040) %>%
                  rename.models.and.scenarios(), linewidth = 1.3, colour = "darkgrey") +

  labs(title = "Service provisioning efficiency factor indicators",
       subtitle = "IMAGE, for Industry") +
  scale_color_npg() +
  ylab("Value normalised to 2020") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y.right = element_text(angle = 0))

p.sef.industry.image

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           paste0("si-SEF-industry-IMAGE-world") ),
  p = p.sef.industry.image,
  w = 400,
  h = 110
)

##### SI.9. INDUSTRY (REMIND) -----------------------------
remind.data.sef.industry <- load_excel_iamc(
  file.path(PATH.data.for.figures, "iam-data", "regional", FILE.REMIND.IAM.REGIONAL.DATA)
) %>% filter(#Region=="World",
  Variable%in%c("Production|Non-Metallic Minerals|Cement|Volume",
                "Production|Iron and Steel|Volume",
                "Final Energy|Industry|Non-Metallic Minerals",
                "Final Energy|Industry|Iron and Steel")) %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  normalise_iamc_long(starting.year = 2020)
remind.sef.industry <- remind.data.sef.industry %>% select(-unit) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(value = (`Production|Non-Metallic Minerals|Cement|Volume`/`Final Energy|Industry|Non-Metallic Minerals`)*0.22+
           (`Production|Iron and Steel|Volume`/`Final Energy|Industry|Iron and Steel`)*0.78
         ) %>%
  mutate(variable = "Scaling (22 / 78)") %>%
  select(model, scenario, variable, year, region, value)
remind.sef.industry.alternative <- remind.data.sef.industry %>% select(-unit) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(value = (`Production|Non-Metallic Minerals|Cement|Volume`/`Final Energy|Industry|Non-Metallic Minerals`)*0.40+
           (`Production|Iron and Steel|Volume`/`Final Energy|Industry|Iron and Steel`)*0.60
  ) %>%
  mutate(variable = "Scaling (40 / 60)") %>%
  select(model, scenario, variable, year, region, value)


p.sef.industry.remind <-
  ggplot(
    data = remind.data.sef.industry %>%
      filter(scenario%in%shape.core.scenarios,
             region=="World",
             year>=2020, year<=2040) %>%
      rename.models.and.scenarios() %>%
      rename_variables_SEF_industry(),
    aes(x = year, y = value,
        label = variable)
  ) +
  facet_grid(region~scenario) +
  geom_textpath(linewidth = 0.9, colour = "black") +
  geom_textpath(data= remind.sef.industry %>% filter(scenario%in%shape.core.scenarios,
                                                      region=="World",
                                                      year>=2020, year<=2040) %>%
                  rename.models.and.scenarios(), linewidth = 1.3, colour = "navyblue") +
  geom_textpath(data= remind.sef.industry.alternative %>% filter(scenario%in%shape.core.scenarios,
                                                               region=="World",
                                                               year>=2020, year<=2040) %>%
                  rename.models.and.scenarios(), linewidth = 1.3, colour = "darkgrey") +

  labs(title = "Service provisioning efficiency factor indicators",
       subtitle = "REMIND, for Industry") +
  scale_color_npg() +
  ylab("Value normalised to 2020") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y.right = element_text(angle = 0))

p.sef.industry.remind

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           paste0("si-SEF-industry-REMIND-world") ),
  p = p.sef.industry.remind,
  w = 400,
  h = 110
)













### ST.3. and SI.10-11 DLE thresholds (grouped pathways) -----------------------
##### ST.3. DLE threshold WB income groups (total) -----------------------------------------
dle.threshold.stats.wb.r4.2040v2020 <- p.dle.data.supplement %>%
  filter(year == DLE.END.YEAR.TIMESERIES) %>%
  filter(grepl(x=scenario, pattern="SDP", fixed=T)|grepl(x=scenario, pattern="SSP2-1p5C", fixed=T)) %>%
  group_by(wb.r4, scenario) %>%
  summarise(
    dle = weighted.mean(dle, pop_mil),
    dle.constant = weighted.mean(dle.constant, pop_mil),
    dle.change = dle / dle.constant
  ) %>%
  reframe(
    dle.mean = mean(dle),
    dle.max = max(dle),
    dle.min = min(dle),

    dle.constant = mean(dle.constant),

    change.mean = mean(1-dle.change),
    change.max = max(1-dle.change),
    change.min = min(1-dle.change),
  ) %>%
  mutate(
    abs.2020 = dle.constant,
    abs.2040 = paste0(
      round(dle.mean, 0), " [", round(dle.min, 0), "-", round(dle.max, 0), "]"
    ),
    rel.2040 = paste0(
      round(change.mean*100, 0), " [", round(change.min*100, 0), "-", round(change.max*100, 0), "]"
    )
  ) %>%
  select(wb.r4, abs.2020, abs.2040, rel.2040)

write_delim(
  dle.threshold.stats.wb.r4.2040v2020,
  file = here("analyses", "figures", data.version, figures.version,
              paste0("si-dle-threshold-stats-WB-R4.csv") ),
  delim = ","
)

##### SI.10. DLE threshold WB income groups (total) -----------------------------------------
p.dle.data.supplement <- df.core %>%
  reframe(
    dle = sum(dle.threshold.adjusted),
    dle.constant = sum(dle.threshold.curtech),
    .by = c("model", "scenario", "iso", "year")
  ) %>%
  mutate(dle.change = dle / dle.constant ) %>%
  left_join(wb.income.grouping.3) %>%
  left_join(population.iso %>% select(-variable,-unit))
p.dle.data.r4 <- p.dle.data.supplement %>%
  reframe(
    dle = weighted.mean(dle, pop_mil),
    .by = c("model", "scenario", "wb.r4", "year")
  )
p.dle.data.supplement$wb.r4 = factor(p.dle.data.supplement$wb.r4,
                             levels=c('Low',
                                      'Lower-middle',
                                      "Upper middle",
                                      'High'))
p.dle.threshold.futures.r4 <- ggplot(
  data = p.dle.data.supplement %>% ms_add() %>% mutate(msi = paste0(`model-scenario`,"-",iso)) %>%
    filter(year>=2020,year<=DLE.END.YEAR.TIMESERIES) %>% rename.models.and.scenarios,
  mapping = aes(x=year, linetype=model)
) +
  facet_grid(wb.r4~scenario) +

  geom_hline(yintercept = 0, linetype="solid") +

  geom_line(aes(group=msi,
                y=dle,
                colour = wb.r4),
            alpha=0.5) +

  geom_ysidedensity(
    data = . %>% filter(year == DLE.END.YEAR.TIMESERIES),
    aes(
      x = after_stat(density),
      y = dle,
      fill = wb.r4,
      colour = wb.r4
    ),
    # position = "stack",
    linetype = "solid",
    linewidth = 0.5,
    alpha = 0.4
  ) +
  geom_ysidehline(yintercept = 0) +

labs(title = bquote("Minimum energy requirement (DLE) over time:" ~ bold("all sectors combined")),
     caption = "Each line is one country (for each model)\nDistributions are kernel densities, and are not population-weighted.") +
  scale_ysidex_continuous(minor_breaks = NULL, breaks = NULL) +
  scale_xsidey_continuous(minor_breaks = NULL, breaks = NULL) +

  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "GJ/cap/yr",
                     limits = c(0,50),
                     breaks = seq(0,40,10),
                     expand = c(0,0)) +

  scale_color_npg() +
  scale_fill_npg() +

  theme_classic() +
  theme_hc() +
  theme(ggside.panel.scale = .3,
        axis.text.x = element_text(angle = 45, hjust = 1)
        # legend.position = "none"
  )

p.dle.threshold.futures.r4

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, "si-dle_thresold_WorldBank4"),
  p = p.dle.threshold.futures.r4,
  w = 350,
  h = 350
)



##### SI.11. Sector & World Bank income groups ----------------------------------------
p.dle.data.supplement.bysector <- df.core %>%
  reframe(
    dle = sum(dle.threshold.adjusted),
    dle.constant = sum(dle.threshold.curtech),
    .by = c("model", "scenario", "variable", "iso", "year")
  ) %>%
  mutate(dle.change = dle / dle.constant ) %>%
  left_join(wb.income.grouping.3) %>%
  left_join(population.iso %>% select(-variable,-unit))
p.dle.data.r4.bysector <- p.dle.data.supplement.bysector %>%
  reframe(
    dle = weighted.mean(dle, pop_mil),
    .by = c("model", "scenario", "wb.r4", "year")
  )
p.dle.data.supplement.bysector$wb.r4 = factor(p.dle.data.supplement.bysector$wb.r4,
                                     levels=c('Low',
                                              'Lower-middle',
                                              "Upper middle",
                                              'High'))
p.dle.threshold.futures.r4.bysector <- ggplot(
  data = p.dle.data.supplement.bysector %>% ms_add() %>% mutate(msiv = paste0(`model-scenario`,"-",iso, "-", variable)) %>%
    filter(year>=2020,year<=DLE.END.YEAR.TIMESERIES) %>% rename.models.and.scenarios() %>% iamc_variable_keep_one_level(level=-1) %>%
    left_join(ipcc.r10) %>% enterise_long_r10_names(),
  mapping = aes(x=year, linetype=model)
) +
  facet_grid(r10~scenario) +

  geom_hline(yintercept = 0, linetype="solid") +

  geom_line(aes(group=msiv,
                y=dle,
                colour = variable),
            alpha=0.5) +

  geom_ysidedensity(
    data = . %>% filter(year == DLE.END.YEAR.TIMESERIES),
    aes(
      x = after_stat(density),
      y = dle,
      fill = variable,
      colour = variable
    ),
    # position = "stack",
    linetype = "solid",
    linewidth = 0.5,
    alpha = 0.4
  ) +
  geom_ysidehline(yintercept = 0) +

  labs(title = bquote("Minimum energy requirement (DLE) over time:" ~ bold("for each sector")),
       caption = "Each line is one country (for each model)\nDistributions are kernel densities, and are not population-weighted.") +
  scale_ysidex_continuous(minor_breaks = NULL, breaks = NULL) +
  scale_xsidey_continuous(minor_breaks = NULL, breaks = NULL) +

  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "GJ/cap/yr",
                     limits = c(0,35),
                     breaks = seq(0,30,10),
                     expand = c(0,0)) +

  scale_color_ptol() +
  scale_fill_ptol() +

  theme_classic() +
  theme_hc() +
  theme(ggside.panel.scale = .3,
        axis.text.x = element_text(angle = 45, hjust = 1)
        # legend.position = "none"
  )

p.dle.threshold.futures.r4.bysector

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, "si-dle_thresold_R10_bysector"),
  p = p.dle.threshold.futures.r4.bysector,
  w = 350,
  h = 450
)


### SI.12. IMAGE residential vs commercial -------------------------------------
p.residential.v.commercial <- read_csv(
  file.path(PATH.data.desire, "other",
            "residential-and-commercial-splits",
            "IMAGE_allSHAPE_ResidentialCommercialSplit_v3.csv")
) %>% iamc_wide_to_long(upper.to.lower = TRUE) %>%
  filter(year <= 2050,
         year >= 2020,
         region!="World",
         scenario %in% shape.core.scenarios) %>%
  rename.models.and.scenarios() %>%
  pivot_wider(values_from = value, names_from = variable) %>%
  mutate(
    res.ratio = `Final Energy|Residential` / `Final Energy|Residential and Commercial`
  ) %>%
  ggplot(
    data = .,
    aes(x = year, y = res.ratio)
  ) +
  facet_wrap(~region, nrow = 2) +
  geom_line(aes(colour = scenario), linewidth = 1.2) +
  labs(title = "Share of residential energy in buildings energy consumtpion",
       caption = "Calculated as `Final Energy|Residential`/`Final Energy|Residential and Commercial`") +
  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +
  ylab("Share residential energy in total buildings energy consumption") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y.right = element_text(angle = 0))

p.residential.v.commercial

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-residential-v-commercial" ),
  p = p.residential.v.commercial,
  w = 300,
  h = 170
)

### SI.13. IMAGE traditional biomass -------------------------------------------
p.traditional.biomass <- df.core %>% filter(model=="IMAGE 3.3",
                                            year <= 2050,
                                            year >= 2020) %>%
  left_join(regions.image) %>%
  rename.models.and.scenarios() %>%
  distinct(model,scenario,region.image,year,res.tradbio.ratio) %>%
  ggplot(
    data = .,
    aes(x = year, y = res.tradbio.ratio)
  ) +
  facet_wrap(~region.image, nrow = 2) +
  geom_line(aes(colour = scenario), linewidth = 1.2) +
  labs(title = "Traditional biomass as share of Residential energy consumption in IMAGE") +
  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +
  ylab("Share of traditional biomass in residential energy consumption") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y.right = element_text(angle = 0))

p.traditional.biomass

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-traditional-biomass" ),
  p = p.traditional.biomass,
  w = 300,
  h = 170
)

### SI.14. IMAGE & REMIND domestic aviation ------------------------------------
p.aviation.adjustment <- readRDS(
  file.path(PATH.data.desire, "other",
            "aviation-split",
            "allSHAPE_Aviation_Transportation_v3.RData")
) %>% iamc_wide_to_long(upper.to.lower = TRUE) %>%
  filter(year <= 2050,
         year >= 2020,
         # region!="World",
         scenario %in% shape.core.scenarios) %>%
  rename.models.and.scenarios() %>%
  pivot_wider(values_from = value, names_from = variable) %>%
  mutate(
    aviation.ratio = `Final Energy|Transportation|Domestic Aviation` / `Final Energy|Transportation`
  ) %>%
  ggplot(
    data = .,
    aes(x = year, y = aviation.ratio)
  ) +
  facet_wrap(model~region, nrow = 4) +
  geom_line(aes(colour = scenario), linewidth = 1.2) +
  labs(title = "Share of domestic aviation in transport energy consumtpion",
       caption = "Calculated as `Final Energy|Transportation|Domestic Aviation`/`Final Energy|Transportation`") +
  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +
  ylab("Share of domestic aviation in transport energy consumption") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y.right = element_text(angle = 0))

p.aviation.adjustment

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-aviation" ),
  p = p.aviation.adjustment,
  w = 300,
  h = 300
)

### SI.15. Correlations income gini and energy gini ---------------------------
df.predicted.differences <- read_csv(
  file.path(PATH.data.desire, "other",
            "inequality",
            "energygini_filling_predicted_differences.csv")
) %>%
  mutate(energy.gini = ifelse(iso %in% (gtap9 %>% pull(iso) %>% unique()), energy.gini, NA),
         diff = ifelse(iso %in% (gtap9 %>% pull(iso) %>% unique()), diff, NA))
df.predicted.differences$income.status = factor(df.predicted.differences$income.status,
                                                levels=c('Low',
                                                         'Lower-middle',
                                                         'Upper middle',
                                                         'High'))
##### numbers
# only gini
df.predicted.differences %>% filter(
  variable!="Final Energy",
  regression.model%in%c("energy.gini ~ gini")
) %>% iamc_variable_keep_one_level(level=-1) %>%
  distinct(
    r2.rc,r2.transport,r2.ind
  )
# gini and income status
df.predicted.differences %>% filter(
  variable!="Final Energy",
  regression.model%in%c("energy.gini ~ gini + income.status")
) %>% iamc_variable_keep_one_level(level=-1) %>%
  distinct(
    r2.rc,r2.transport,r2.ind
  )

##### visualise
p.prediction.energygini.visualised.linearregression.onlygini <-
  ggplot(
    data = df.predicted.differences %>% filter(
      variable!="Final Energy",
      regression.model%in%c("energy.gini ~ gini")
    ) %>% iamc_variable_keep_one_level(level=-1),
    mapping = aes(x = gini/100, y = energy.gini, colour = variable)
  ) +
  facet_grid(variable~., scales = "free_y") +
  geom_point() +
  geom_smooth(data = . %>% filter(regression.model=="energy.gini ~ gini"),
              se = FALSE,
              method = "lm") +
  geom_smooth(data = . %>% filter(regression.model=="energy.gini ~ gini + income.status"),
              se = FALSE,
              method = "lm") +
  labs(
    title = "Relationship:\n'energy.gini ~ gini'"
  ) +
  scale_y_continuous("Energy gini") +
  scale_x_continuous(expand = c(0,0),
                     name = "Income gini") +
  scale_color_ptol() +
  theme_minimal() +
  theme(legend.position="none",
        strip.text.y.right = element_blank())
p.prediction.energygini.visualised.linearregression.onlygini

p.prediction.energygini.visualised.linearregression.preferred <-
  ggplot(
    data = df.predicted.differences %>% filter(
      variable!="Final Energy",
      regression.model%in%c("energy.gini ~ gini + income.status")
    ) %>% iamc_variable_keep_one_level(level=-1),
    mapping = aes(x = gini/100, y = energy.gini, colour = variable)
  ) +
  facet_grid(variable~income.status, scales = "free_y") +
  geom_point() +
  geom_smooth(data = . %>% filter(regression.model=="energy.gini ~ gini"),
              se = FALSE,
              method = "lm") +
  geom_smooth(data = . %>% filter(regression.model=="energy.gini ~ gini + income.status"),
              se = FALSE,
              method = "lm") +
  labs(
    title = "Relationship:\n'energy.gini ~ gini + income.status'"
  ) +
  scale_y_continuous("Energy gini") +
  scale_x_continuous(expand = c(0,0),
                     name = "Income gini") +
  scale_color_ptol() +
  theme_minimal() +
  theme(legend.position="none",
        strip.text.y.right = element_text(angle = 0))

p.prediction.energygini.visualised.linearregression.preferred

p.prediction.energygini.visualised.linearregression <-
  (p.prediction.energygini.visualised.linearregression.onlygini +
     p.prediction.energygini.visualised.linearregression.preferred) +
  plot_layout(
    design = "
    ABBB
    ABBB
    ABBB
    "
  ) +
  plot_annotation(
    tag_levels = "A"
  )


save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-gini-to-gini-regression" ),
  p = p.prediction.energygini.visualised.linearregression,
  w = 300,
  h = 250
)


### SI.16. Differences for predicted vs observed energy gini, per country ------
rename_reg_model_name <- function(df){
  return(
    df %>%
      mutate_cond(
        regression.model == "energy.gini ~ gini + income.status",
        regression.model = "energy.gini ~\n   gini + income.status"
      ) %>%
      mutate_cond(
        regression.model == "energy.gini ~ gini",
        regression.model = "energy.gini ~\n   gini"
      )
  )
}
# remove levels of income.status (in SI.15, for visualisation, as an ordered factor)
df.predicted.differences <- df.predicted.differences %>% droplevels()
df.predicted.differences.quantiles <- df.predicted.differences %>%
  reframe(
    p01 = quantile(diff, 0.01, na.rm = TRUE),
    p05 = quantile(diff, 0.05, na.rm = TRUE),
    p25 = quantile(diff, 0.25, na.rm = TRUE),
    p50 = quantile(diff, 0.50, na.rm = TRUE),
    p75 = quantile(diff, 0.75, na.rm = TRUE),
    p95 = quantile(diff, 0.95, na.rm = TRUE),
    p99 = quantile(diff, 0.99, na.rm = TRUE),
    .by = c("regression.model", "variable")
  ) %>%
  pivot_longer(cols = starts_with("p"), names_to = "q", values_to = "value")


p.prediction.energygini.visualised.histogramdiffs.rescom <- ggplot(
  data = df.predicted.differences %>% filter(
    regression.model%in%c("energy.gini ~ gini",
                          "energy.gini ~ gini + income.status"),
    variable=="Final Energy|Residential and Commercial"
  ) %>% rename_reg_model_name() %>% iamc_variable_keep_one_level(level=-1),
  mapping = aes(y=diff,fill=variable)) +
  facet_grid(regression.model~variable) +
  geom_hline(yintercept = 0)+
  geom_histogram(bins=20) +

  geom_hline(data = df.predicted.differences.quantiles %>% filter(
    regression.model%in%c("energy.gini ~ gini",
                          "energy.gini ~ gini + income.status"),
    variable=="Final Energy|Residential and Commercial"
  ) %>% rename_reg_model_name() %>% iamc_variable_keep_one_level(level=-1),
  aes(yintercept = value), linetype = "dashed") +
  geom_label(data = df.predicted.differences.quantiles %>% filter(
    regression.model%in%c("energy.gini ~ gini",
                          "energy.gini ~ gini + income.status"),
    variable=="Final Energy|Residential and Commercial"
  ) %>% rename_reg_model_name() %>% iamc_variable_keep_one_level(level=-1),
  alpha=0.5,
  x = 10,
  aes(
    y = value,
    label = paste(c("1%", "5%", "25%", "50%", "75%", "95%", "99%"), as.character(round(value,2)), sep = '\n')
  )
  ) +

  coord_flip() +
  labs(
    title = "Difference gini in Oswald data vs predicted gini"
  ) +
  scale_y_continuous("Data-based energy gini minus predicted energy gini") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_ptol() +
  theme_bw() + theme(legend.position = "none")
p.prediction.energygini.visualised.histogramdiffs.rescom

p.prediction.energygini.visualised.histogramdiffs.transport <- ggplot(
  data = df.predicted.differences %>% filter(
    regression.model%in%c("energy.gini ~ gini",
                          "energy.gini ~ gini + income.status"),
    variable=="Final Energy|Transportation"
  ) %>% rename_reg_model_name() %>% iamc_variable_keep_one_level(level=-1),
  mapping = aes(y=diff,fill=variable)) +
  facet_grid(regression.model~variable) +
  geom_hline(yintercept = 0)+
  geom_histogram(bins=20) +

  geom_hline(data = df.predicted.differences.quantiles %>% filter(
    regression.model%in%c("energy.gini ~ gini",
                          "energy.gini ~ gini + income.status"),
    variable=="Final Energy|Transportation"
  ) %>% rename_reg_model_name() %>% iamc_variable_keep_one_level(level=-1),
  aes(yintercept = value), linetype = "dashed") +
  geom_label(data = df.predicted.differences.quantiles %>% filter(
    regression.model%in%c("energy.gini ~ gini",
                          "energy.gini ~ gini + income.status"),
    variable=="Final Energy|Transportation"
  ) %>% rename_reg_model_name() %>% iamc_variable_keep_one_level(level=-1),
  alpha=0.5,
  x = 10,
  aes(
    y = value,
    label = paste(c("1%", "5%", "25%", "50%", "75%", "95%", "99%"), as.character(round(value,2)), sep = '\n')
  )
  ) +

  coord_flip() +
  labs(
    # title = "Difference gini in Oswald data vs predicted gini"
  ) +
  scale_y_continuous("Data-based energy gini minus predicted energy gini") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_ptol() +
  theme_bw() + theme(legend.position = "none")
p.prediction.energygini.visualised.histogramdiffs.transport

p.prediction.energygini.visualised.histogramdiffs.ind <- ggplot(
  data = df.predicted.differences %>% filter(
    regression.model%in%c("energy.gini ~ gini",
                          "energy.gini ~ gini + income.status"),
    variable=="Final Energy|Industry"
  ) %>% rename_reg_model_name() %>% iamc_variable_keep_one_level(level=-1),
  mapping = aes(y=diff,fill=variable)) +
  facet_grid(regression.model~variable) +
  geom_hline(yintercept = 0)+
  geom_histogram(bins=20) +

  geom_hline(data = df.predicted.differences.quantiles %>% filter(
    regression.model%in%c("energy.gini ~ gini",
                          "energy.gini ~ gini + income.status"),
    variable=="Final Energy|Industry"
  ) %>% rename_reg_model_name() %>% iamc_variable_keep_one_level(level=-1),
  aes(yintercept = value), linetype = "dashed") +
  geom_label(data = df.predicted.differences.quantiles %>% filter(
    regression.model%in%c("energy.gini ~ gini",
                          "energy.gini ~ gini + income.status"),
    variable=="Final Energy|Industry"
  ) %>% rename_reg_model_name() %>% iamc_variable_keep_one_level(level=-1),
  alpha=0.5,
  x = 10,
  aes(
    y = value,
    label = paste(c("1%", "5%", "25%", "50%", "75%", "95%", "99%"), as.character(round(value,2)), sep = '\n')
  )
  ) +

  coord_flip() +
  labs(
    # title = "Difference gini in Oswald data vs predicted gini"
  ) +
  scale_y_continuous("Data-based energy gini minus predicted energy gini") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_ptol() +
  theme_bw() + theme(legend.position = "none")
p.prediction.energygini.visualised.histogramdiffs.ind

p.prediction.energygini.visualised.histogramdiffs <- (
  p.prediction.energygini.visualised.histogramdiffs.rescom +
    p.prediction.energygini.visualised.histogramdiffs.transport +
    p.prediction.energygini.visualised.histogramdiffs.ind
) + plot_layout(
  design = "
  A
  B
  C
  "
) + plot_annotation(
  tag_levels = "A"
)

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-diffs-predicted-observed" ),
  p = p.prediction.energygini.visualised.histogramdiffs,
  w = 300,
  h = 350
)


### SI.17. Infilled vs Observed energy gini (histograms), all countries --------
data.predicted.energygini.histograms <- df.predicted.differences %>%
  filter(regression.model%in%c("energy.gini ~ gini + income.status",
                               "energy.gini ~ gini"),
         variable!="Final Energy") %>% iamc_variable_keep_one_level(level=-1) %>% rename_reg_model_name()
p.predicted.energygini.histograms <-
  ggplot(
    data = data.predicted.energygini.histograms,
  ) +
  facet_grid(regression.model~variable) +
  geom_histogram(data = data.predicted.energygini.histograms %>% filter(is.na(energy.gini)),
                 mapping = aes(x=pred),
                 bins = 30,
                 color="grey", linewidth = 0.5,
                 fill = "grey") +
  geom_histogram(data = . %>% filter(!is.na(energy.gini)),
                 mapping = aes(x=energy.gini),
                 bins = 30,
                 color="red",
                 alpha = 0) +
  theme_bw() +
  theme(legend.position="none") +
  labs(x="Energy gini coefficient",
       y="Count",
       title="Infilled (grey) vs data-based (red) Gini coefficient distributions")
p.predicted.energygini.histograms

# Numbers:
df.predicted.differences %>%
  filter(regression.model%in%c("energy.gini ~ gini + income.status"),
         variable!="Final Energy") %>%
  iamc_variable_keep_one_level(level=-1) %>% rename_reg_model_name() %>%
  # only infilled, where no diff is available
  filter(is.na(diff)) %>%
  reframe(
    min.energy.gini.infilled = min(pred),
    max.energy.gini.infilled = max(pred),
    .by = c("variable")
  )

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-histograms-new-prediction-vs-data" ),
  p = p.predicted.energygini.histograms,
  w = 200,
  h = 125
)


### SI.18. Q-Q plot for national surveus underlying MESSAGE-Access -------------
MICRODATA.PREPROCESSED <- T
# countries with data
list.of.countries <- c(
  "AGO", # one super high out of sample observation,
  # "BLR", # no cons variables
  "BRA", # looks ok; only has "other_cons" and "elec_cons"
  "CHL", # looks ok
  "CHN", # quite a bit of zero consumption
  # "DZA", # everything is 1?
  "ETH", # looks ok
  "FRA", # has only elec_cons and gas_cons; lot of NA for gas_cons, and some household observations entirely NA -- I replace with zero
  "GHA", # looks ok
  "GTM", # looks ok
  "IDN", # looks ok
  "IND", # looks ok - reading in 4 files
  "IRQ", # looks ok
  "ITA", # looks ok
  "KOR", # looks ok
  "MAR", # looks ok
  "MEX", # looks ok - lot of NAs for firewood and gas consumption -- I replace with zero now.
  # "MNG", # nothing in this folder
  "POL", # looks ok (a lot of biomass)
  "RUS", # looks ok
  # "SRB", # all data is the same
  # "THA", # all data is the same
  # "UKR", # all data is the same
  "USA", # looks ok
  # "UZB", # all data is the same
  "ZAF" # looks ok
)

# Data pre-processing (only possible if you have a data-connection to an IIASA server)
if (MICRODATA.PREPROCESSED==FALSE){
  mircodata.pdrive <- file.path("P:", "ene.general", "DecentLivingEnergy", "Surveys", "BUILDINGS PROCESSING" )

  get_prepped_data_set_from_pdrive <- function(iso, keep.only.total=T, add.iso.col=T, only.clean.sources = F){

    # data *cons options:
    # - frwd_cons = firewood
    # - chrcl_cons = charcoal
    # - kero_cons = kerosene
    # - gas_cons = natural gas and LPG
    # - elec_cons = electricity
    # - biomass_cons = biomass (used in POL)
    #
    # - other_cons = other consumption (used in BRA, ...)



    # summarize as suggested by Setu

    if (iso=="IND"){

      df <- vroom(file.path(mircodata.pdrive, "IND", "IND_data_1.txt")) %>% select(-urban) %>%
        bind_rows(
          vroom(file.path(mircodata.pdrive, "IND", "IND_data_2.txt")) %>% select(-urban)
        ) %>%
        bind_rows(
          vroom(file.path(mircodata.pdrive, "IND", "IND_data_3.txt")) %>% select(-urban)
        ) %>%
        bind_rows(
          vroom(file.path(mircodata.pdrive, "IND", "IND_data_elec.txt")) %>% select(-urban)
        )

    } else {
      df <- vroom(
        file.path(mircodata.pdrive, iso, paste0(iso,"_data.txt"))
      )
    }


    if (only.clean.sources){
      df <- df %>%
        # select only LPG/natural gas (gas_cons) and electricity (elec_cons) "*cons" columns
        select(any_of(c("gas_cons", "elec_cons")), hh_size, hh_weight) %>%
        # replace NA with 0
        replace(is.na(.), 0) %>%
        # add total column, per capita
        mutate(total = rowSums(across(c(where(is.numeric), -hh_weight, -hh_size))) / hh_size ) %>%
        # adjust household weight to per capita weight by multiplying with household size
        mutate(weight = hh_weight * hh_size)
    } else {
      df <- df %>%
        # select only "*cons" columns
        select(ends_with("_cons"), hh_size, hh_weight) %>%

        # to-do: in case non-clean sources would be used, then Ethiopia mihgt need a filter for frwd_cons (not going below zero)

        # replace NA with 0
        replace(is.na(.), 0) %>%
        # add total column, per capita
        mutate(total = rowSums(across(c(where(is.numeric), -hh_weight, -hh_size))) / hh_size ) %>%
        # adjust household weight to per capita weight by multiplying with household size
        mutate(weight = hh_weight * hh_size)
    }


    # ad_hoc fix [e.g. to filter unrealistic frwd_cons in AGO of 99748]
    df <- df %>% filter(total<10000)




    if (keep.only.total){
      df <- df %>% select(total, weight)
    }

    if (add.iso.col){
      df <- df %>% mutate(iso=iso)
    }

    return(df)
  }

  # clean sources only
  d.clean <- list.of.countries %>%
    map(~ get_prepped_data_set_from_pdrive(iso=., only.clean.sources = T)) %>%
    reduce(rbind)
  # drop na
  d.clean <- d.clean %>%
    filter(!is.na(total))

  # check number of observations remaining
  d.clean %>% group_by(iso) %>% count() %>% arrange(n)


  # write the data to the drive for faster loading
  write_delim(
    file = file.path(PATH.data.for.figures,
                     "household_microdata",
                     "hh_cons_percapita_clean.csv"),
    x = d.clean,
    delim = ","
  )

  # should rename above
  d.hh.clean <- d.clean


} else {
  # load pre-processed data

  d.hh.clean <- read_csv(
    file.path(
      PATH.data.for.figures,
      "household_microdata",
      "hh_cons_percapita_clean.csv"
    )
  )
}

##### Sample data (for a global distribution) ----------------------------------
sample_data <- function(c, fdf, n=2000){

  dfd <- fdf %>% filter(iso==c) %>% select(total,weight)


  df <- tibble( total = sample(x=dfd$total,
                               size=n,
                               prob=dfd$weight,
                               replace = TRUE), # replace = TRUE, to allow for sample size bigger than the data
                iso = c)

  return(df)
}

set.seed(19721004) # for reproducibility; date IIASA was founded
SAMPLE.SIZE <<- 10000
d.clean.sample <- list.of.countries %>%
  map(~ sample_data(c=., fdf=d.hh.clean, n=SAMPLE.SIZE)) %>%
  reduce(rbind)
d.clean.sample.world <- c("World") %>%
  map(~ sample_data(c=., fdf=d.hh.clean %>% mutate(iso="World"), n=SAMPLE.SIZE*10)) %>%
  reduce(rbind)

##### Plot Q-Q plot (based on sampled data) ------------------------------------
library(qqplotr)
p.qq.pc.weighted.clean <-
  ggplot(d.clean.sample %>%
           # bind_rows(d.clean.sample.world) %>%
           filter(iso!="MEX", iso!="FRA") %>%
           mutate(total = log(total)),
         aes(sample = total)) +
  labs(title = "Weighted quantile-quantile plot of the natural logarithm of energy consumption",
       subtitle = "Only gas and electricity.",
       x="Theoretical quantiles",
       y="ln(residential clean energy consumption [GJ/cap/yr])",
       caption="Since the y-axis is a logaritmic, it does not show observations with 0 GJ/cap/yr.") +
  facet_wrap(~iso) +
  stat_qq_point(colour="grey", alpha=0.5, size=0.2) +
  stat_qq_line(colour="blue") +
  geom_hline(yintercept = 0, colour="lightblue", linetype = "dashed") +
  theme_bw()
p.qq.pc.weighted.clean

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-qqplot-microdata" ),
  p = p.qq.pc.weighted.clean,
  w = 200,
  h = 200
)

### SI.19. comparing microdata gini with oswald gini ----------------------------
##### Auxiliary functions
get_gini_sampled_access_data <- function(df, n=2000, ret="gini") {

  # weighted, so population has equal weight
  dg <- df %>% mutate(pop=1)
  # get totals
  dg.sums <- dg %>% group_by(iso) %>% summarise(tot.sum=sum(total),pop.sum=sum(pop))
  # get shares
  dg <- dg %>% left_join(dg.sums) %>% mutate(total=total/tot.sum, pop=pop/pop.sum)
  # arrange data
  dg <- dg %>% select(total,pop,iso) %>% arrange(iso,total)
  # get cumulative shares
  dg <- dg %>%
    reframe(
      c.ene.share = cumsum(total),
      c.pop.share = cumsum(pop),
      .by = c("iso")
    )
  if (ret == "lorenz.curve.data"){
    return(dg)
  } else if (ret == "gini"){
    ginis <- dg %>%
      mutate(
        area.points.rec = lag(c.ene.share, default = 0) * c.pop.share,
        area.points.tri = (c.ene.share - lag(c.ene.share, default = 0)) * c.pop.share * 0.5
      ) %>%
      mutate(area.points = area.points.rec + area.points.tri) %>%
      reframe(
        area = sum(area.points),
        .by = c("iso")
      ) %>%
      mutate(area=area/n) %>%
      mutate(
        gini = (0.5 - area) / 0.5
      ) %>%
      select(-area)

    return(ginis)

  } else (
    stop("return must be either 'lorenz.curve.data' or 'gini'")
  )
}


##### Plot (Gini coefficient comparison with mircodata) ------------------------
g.comp <-
  get_gini_sampled_access_data(d.clean.sample, n=SAMPLE.SIZE,
                               ret = "gini") %>%
  rename(access.gini=gini) %>%
  left_join(
    df.core %>% filter(year==2020, variable=="Final Energy|Residential and Commercial") %>%
      select(iso, energy.gini) %>% distinct() %>% rename(gtap.oswald.gini=energy.gini)
  )

library(ggrepel)
p.g.comp.clean <- ggplot(g.comp %>% filter(iso!="FRA", iso!="MEX"),
                         aes(x=access.gini,
                             y=gtap.oswald.gini)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(fill=iso),colour="black", shape=21, size=2) +
  geom_text_repel(aes(colour=iso, label=iso)) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = "none",
        plot.margin=unit(c(2,5,2,2),"mm")) +
  labs(title = "Comparing Gini coefficients derived from microdata and Oswald et al.",
       y = "DESIRE (all) energy Gini coefficient (from Oswald et al.)",
       x = "Mircodata (clean) energy Gini coefficient (from Poblete-Cazenave et al.)")

p.g.comp.clean
save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-gini-microdata-vs-oswald-clean" ),
  p = p.g.comp.clean,
  w = 200,
  h = 200
)

### SI.20. Lorenz curves for microdata gini -------------------------------------

microdata.lorenz.data <- get_gini_sampled_access_data(d.clean.sample, n = SAMPLE.SIZE,
                                                      ret = "lorenz.curve.data")

label.countries <- c("RUS", "USA", "AGO", "CHN")
p.lorenz.access.data <-
  ggplot(microdata.lorenz.data %>% filter(iso!="FRA", iso!="MEX")) +
  geom_abline(slope = 1, intercept = 0) +
  geom_line(data = . %>% filter(iso%nin%label.countries),
            colour = "grey",
            aes(x=c.pop.share,y=c.ene.share,group=iso)) +
  geom_textpath(data = . %>% filter(iso%in%label.countries),
                aes(x=c.pop.share,y=c.ene.share,colour=iso, label=iso)) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(0,1)) +
  labs(
    title = "Residential energy consumption per capita lorenz curve (microdata)",
    y = "Cumulative share of energy consumption",
    x = "Cumulative share of population"
  )
p.lorenz.access.data
save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-lorenz-microdata" ),
  p = p.lorenz.access.data,
  w = 200,
  h = 200
)


### SI.21. Comparing fits of various distributions ------------------------------
test_distribution_function_at_resolution_pvalue <- function(n,
                                                            df.microdata,
                                                            form.dist.test = "lognormal",
                                                            remove.lows.and.notindlerange = T,
                                                            remove.lows.and.notindlerange.lower = 0.05,
                                                            remove.lows.and.notindlerange.upper = 40,
                                                            seednum=19721004){
  print(form.dist.test)
  list.of.countries <- df.microdata$iso %>% unique

  set.seed(seednum)


  if(!require(goft)) {install.packages("goft"); require(goft)}


  df.pval.test.samples <- NULL # hold pvalues of the test samples
  for (s in n){

    print(s)

    df.microdata.sampled <- list.of.countries %>%
      map(~ sample_data(c=., fdf=df.microdata, n=s)) %>%
      reduce(rbind)

    if (remove.lows.and.notindlerange){
      df.microdata.sampled <- df.microdata.sampled %>% filter(
        total>remove.lows.and.notindlerange.lower,
        total<remove.lows.and.notindlerange.upper
      )
    }


    if (form.dist.test == "lognormal"){
      p <- NULL
      for (i in list.of.countries){
        p <- p %>%
          bind_rows(
            tibble(
              iso = i,
              pvalue = tryCatch({(lnorm_test( df.microdata.sampled %>% filter(iso==i) %>% pull(total) ))$p.value}, error = function(e){print(e);return(NA)})
            ) %>%
              mutate(lognormal = ifelse(pvalue<0.05, "no", "yes"))
          )
      }
      p

    } else if (form.dist.test == "weibull") {
      p <- NULL
      for (i in list.of.countries){
        p <- p %>%
          bind_rows(
            tibble(
              iso = i,
              pvalue = tryCatch({(weibull_test( df.microdata.sampled %>% filter(iso==i) %>% pull(total), N=500  ))$p.value # N lower than default 1000, to speed up the sampling
              }, error = function(e){print(e);return(NA)})
            ) %>%
              mutate(weibull = ifelse(pvalue<0.05, "no", "yes"))
          )
      }
      p
    } else if (form.dist.test == "generalized.pareto"){
      stop("generalized.pareto implementation does not seem to work currently")

      p <- NULL
      for (i in list.of.countries){
        p <- p %>%
          bind_rows(
            tibble(
              iso = i,
              pvalue = tryCatch({(gp_test( df.microdata.sampled %>% filter(iso==i) %>% pull(total), B=500  ))$p.value # B lower than default 999, to speed up the sampling
              }, error = function(e){print(e);return(NA)})
            ) %>%
              mutate(pareto = ifelse(pvalue<0.05, "no", "yes"))
          )
      }
      p
    } else if (form.dist.test == "gamma"){
      p <- NULL
      for (i in list.of.countries){
        p <- p %>%
          bind_rows(
            tibble(
              iso = i,
              pvalue = tryCatch({(gamma_test( df.microdata.sampled %>% filter(iso==i) %>% pull(total)))$p.value
              }, error = function(e){print(e);return(NA)})
            ) %>%
              mutate(gamma = ifelse(pvalue<0.05, "no", "yes"))
          )
      }
      p
    }  else if (form.dist.test %in% c("power","exponential")){
      p <- NULL
      for (i in list.of.countries){
        p <- p %>%
          bind_rows(
            tibble(
              iso = i,
              pvalue = tryCatch({(exp_test( df.microdata.sampled %>% filter(iso==i) %>% pull(total), N=500))$p.value # N lower than default 1000, to speed up the sampling
              }, error = function(e){print(e);return(NA)})
            ) %>%
              mutate(exp = ifelse(pvalue<0.05, "no", "yes"))
          )
      }
      p
    }

    df.pval.test.samples <- df.pval.test.samples %>%
      bind_rows(
        p %>% mutate(sample.size = s)
      )
  }




  return(df.pval.test.samples)
}

##### Create data
set.seed(19721004) # for reproducibility; date IIASA was founded
ln.testdata <- rlnorm(100, log(10), log(2.5))
wei.testdata <- rweibull(100, 0.75)
gam.testdata <- rgamma(100, 0.75)
exp.testdata <- rexp(100, 0.75)

library(goft)
(lnorm_test( ln.testdata ))$p.value # gives pvalue much larger than 0
(lnorm_test( wei.testdata ))$p.value # gives pvalue of close to 0

(weibull_test( wei.testdata ))$p.value # gives pvalue much larger than 0
(weibull_test( ln.testdata ))$p.value # gives pvalue of close to 0

(gamma_test( gam.testdata ))$p.value # gives pvalue much larger than 0
(gamma_test( ln.testdata ))$p.value # gives pvalue of close to 0

(exp_test( exp.testdata ))$p.value # gives pvalue much larger than 0
(exp_test( ln.testdata ))$p.value # gives pvalue of close to 0


##### All, raw data: run and plot the p-values ---------------------------------

sample.sizes <- c(
  15,
  20,
  30, 40,
  50,
  60, 70, 80, 90,
  100,
  125,
  150,
  200, 300, 400,
  500
)
NUMBER.OF.SAMPLE.ITERATIONS <- 30 # takes about 70 seconds per iteration (weibull test takes the longest)
sample.sizes.seeds <- c(
  19721004*seq(1,NUMBER.OF.SAMPLE.ITERATIONS)
)

pvals.lognormal.different.seeds <- NULL
for (seed in sample.sizes.seeds){
  print(paste0("Seed number: ", as.character(seed/19721004)))
  pvals.lognormal.different.seeds <- pvals.lognormal.different.seeds %>%
    # lognormal
    bind_rows(
      test_distribution_function_at_resolution_pvalue(
        n = sample.sizes,
        df.microdata = d.hh.clean %>% filter(iso!="MEX", iso!="FRA"), # directly from the microdata survey
        form.dist.test = "lognormal",
        seednum = seed,
        remove.lows.and.notindlerange = F
      ) %>%
        mutate(seednum=seed)
    ) %>%
    # weibull
    bind_rows(
      test_distribution_function_at_resolution_pvalue(
        n = sample.sizes,
        df.microdata = d.hh.clean %>% filter(iso!="MEX", iso!="FRA"), # directly from the microdata survey
        form.dist.test = "weibull",
        seednum = seed,
        remove.lows.and.notindlerange = F
      ) %>%
        mutate(seednum=seed)
    ) %>%
    # gamma
    bind_rows(
      test_distribution_function_at_resolution_pvalue(
        n = sample.sizes,
        df.microdata = d.hh.clean %>% filter(iso!="MEX", iso!="FRA"), # directly from the microdata survey
        form.dist.test = "gamma",
        seednum = seed,
        remove.lows.and.notindlerange = F
      ) %>%
        mutate(seednum=seed)
    ) %>%
    # exponential/power law
    bind_rows(
      test_distribution_function_at_resolution_pvalue(
        n = sample.sizes,
        df.microdata = d.hh.clean %>% filter(iso!="MEX", iso!="FRA"), # directly from the microdata survey
        form.dist.test = "exponential",
        seednum = seed,
        remove.lows.and.notindlerange = F
      ) %>%
        mutate(seednum=seed)
    )
}



p.pvals.lognormal.different.seeds.countries <- ggplot(
  pvals.lognormal.different.seeds %>%
    # clean up awkward dataframe format
    mutate(`Distribution fit` = lognormal,
           `Distribution type` = ifelse(!is.na(lognormal), "lognormal",as.character(NA))) %>%
    mutate_cond(
      (is.na(`Distribution fit`) & !is.na(weibull)), `Distribution fit` = weibull
    ) %>%
    mutate_cond(
      (is.na(`Distribution type`) & !is.na(weibull)), `Distribution type` = "weibull"
    ) %>%
    mutate_cond(
      (is.na(`Distribution fit`) & !is.na(gamma)), `Distribution fit` = gamma
    ) %>%
    mutate_cond(
      (is.na(`Distribution type`) & !is.na(gamma)), `Distribution type` = "gamma"
    ) %>%
    mutate_cond(
      (is.na(`Distribution fit`) & !is.na(exp)), `Distribution fit` = exp
    ) %>%
    mutate_cond(
      (is.na(`Distribution type`) & !is.na(exp)), `Distribution type` = "exp"
    ) %>%
    select(-lognormal, -weibull, -gamma, -exp) %>%

    drop_na() %>% # filter out small subset for Angola which is NA due to drawing too many zeroes in the small sample set

    # summarise
    mutate(fit=ifelse(`Distribution fit`=="yes", 1,0)) %>%
    reframe(`Average p-value` = mean(pvalue, na.rm=T),
            `Average fit possible` = mean(fit,na.rm=T),
            .by = c("sample.size", "Distribution type")
    ),
  aes(x=sample.size)
) +

  geom_line(aes(y=`Average p-value`, colour=`Distribution type`), linetype="dashed") +
  geom_textpath(aes(y=`Average fit possible`, colour=`Distribution type`,
                    label=`Distribution type`), linetype="solid") +

  scale_x_continuous(limits = c(sample.sizes[1], sample.sizes[length(sample.sizes)]),
                     expand = c(0,0),
                     breaks = sample.sizes, trans = "log") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0), labels = scales::percent) +
  scale_color_ptol() +
  labs(title = "Test distributions at various sample sizes",
       subtitle = "For full microdata sample range across 17 countries.",
       y = "Average fit cannot be rajected (solid line)\nAverage p-value cannot be rajected (dashed line)"
       # caption = ""
       # caption = "Note: actual sample size is smaller than x-axis, after filtering." # for DLE range
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")

p.pvals.lognormal.different.seeds.countries






##### DLE-range, raw data: run and plot the p-values ---------------------------


# see above for definition of these vectors
# sample.sizes <- c(
#   15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 125, 150, 200, 300, 400, 500
# )
# NUMBER.OF.SAMPLE.ITERATIONS <- 30 # takes about 70 seconds per iteration (weibull test takes the longest)
# sample.sizes.seeds <- c(
#   19721004*seq(1,NUMBER.OF.SAMPLE.ITERATIONS)
# )

pvals.lognormal.different.seeds.dlerange <- NULL
for (seed in sample.sizes.seeds){
  print(paste0("Seed number: ", as.character(seed/19721004)))
  pvals.lognormal.different.seeds.dlerange <- pvals.lognormal.different.seeds.dlerange %>%
    # lognormal
    bind_rows(
      test_distribution_function_at_resolution_pvalue(
        n = sample.sizes,
        df.microdata = d.hh.clean %>% filter(iso!="MEX", iso!="FRA"), # directly from the microdata survey
        form.dist.test = "lognormal",
        seednum = seed,
        remove.lows.and.notindlerange = T
      ) %>%
        mutate(seednum=seed)
    ) %>%
    # weibull
    bind_rows(
      test_distribution_function_at_resolution_pvalue(
        n = sample.sizes,
        df.microdata = d.hh.clean %>% filter(iso!="MEX", iso!="FRA"), # directly from the microdata survey
        form.dist.test = "weibull",
        seednum = seed,
        remove.lows.and.notindlerange = T
      ) %>%
        mutate(seednum=seed)
    ) %>%
    # gamma
    bind_rows(
      test_distribution_function_at_resolution_pvalue(
        n = sample.sizes,
        df.microdata = d.hh.clean %>% filter(iso!="MEX", iso!="FRA"), # directly from the microdata survey
        form.dist.test = "gamma",
        seednum = seed,
        remove.lows.and.notindlerange = T
      ) %>%
        mutate(seednum=seed)
    ) %>%
    # exponential/power law
    bind_rows(
      test_distribution_function_at_resolution_pvalue(
        n = sample.sizes,
        df.microdata = d.hh.clean %>% filter(iso!="MEX", iso!="FRA"), # directly from the microdata survey
        form.dist.test = "exponential",
        seednum = seed,
        remove.lows.and.notindlerange = T
      ) %>%
        mutate(seednum=seed)
    )
}


p.pvals.lognormal.different.seeds.countries.notails <- ggplot(
  pvals.lognormal.different.seeds.dlerange %>%
    # clean up awkward dataframe format
    mutate(`Distribution fit` = lognormal,
           `Distribution type` = ifelse(!is.na(lognormal), "lognormal",as.character(NA))) %>%
    mutate_cond(
      (is.na(`Distribution fit`) & !is.na(weibull)), `Distribution fit` = weibull
    ) %>%
    mutate_cond(
      (is.na(`Distribution type`) & !is.na(weibull)), `Distribution type` = "weibull"
    ) %>%
    mutate_cond(
      (is.na(`Distribution fit`) & !is.na(gamma)), `Distribution fit` = gamma
    ) %>%
    mutate_cond(
      (is.na(`Distribution type`) & !is.na(gamma)), `Distribution type` = "gamma"
    ) %>%
    mutate_cond(
      (is.na(`Distribution fit`) & !is.na(exp)), `Distribution fit` = exp
    ) %>%
    mutate_cond(
      (is.na(`Distribution type`) & !is.na(exp)), `Distribution type` = "exp"
    ) %>%
    select(-lognormal, -weibull, -gamma, -exp) %>%

    drop_na() %>% # filter out small subset for Angola which is NA due to drawing too many zeroes in the small sample set

    # summarise
    mutate(fit=ifelse(`Distribution fit`=="yes", 1,0)) %>%
    reframe(`Average p-value` = mean(pvalue, na.rm=T),
            `Average fit possible` = mean(fit,na.rm=T),
            .by = c("sample.size", "Distribution type")
    ),
  aes(x=sample.size)
) +

  geom_line(aes(y=`Average p-value`, colour=`Distribution type`), linetype="dashed") +
  geom_textpath(aes(y=`Average fit possible`, colour=`Distribution type`,
                    label=`Distribution type`), linetype="solid") +

  scale_x_continuous(limits = c(sample.sizes[1], sample.sizes[length(sample.sizes)]),
                     expand = c(0,0),
                     breaks = sample.sizes, trans = "log") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0), labels = scales::percent) +
  scale_color_ptol() +
  labs(title = "Test distributions at various sample sizes",
       subtitle = "For 0.05-40 GJ/cap/yr energy consumption range, across 17 countries.",
       y = "Average fit cannot be rajected (solid line)\nAverage p-value cannot be rajected (dashed line)"
       # caption = ""
       # caption = "Note: actual sample size is smaller than x-axis, after filtering." # for DLE range
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")

p.pvals.lognormal.different.seeds.countries.notails


##### Save combined plots of p-values ------------------------------------------

p.pvals <- (p.pvals.lognormal.different.seeds.countries + p.pvals.lognormal.different.seeds.countries.notails) +
  plot_layout(design = "
              A
              B
              ") +
  plot_annotation(tag_levels = "A")

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-pvalues-microdata" ),
  p = p.pvals,
  w = 200,
  h = 250
)

# also save the combined data
df.pvalues.bothfacets <- pvals.lognormal.different.seeds.dlerange %>%
  # clean up awkward dataframe format
  mutate(`Distribution fit` = lognormal,
         `Distribution type` = ifelse(!is.na(lognormal), "lognormal",as.character(NA))) %>%
  mutate_cond(
    (is.na(`Distribution fit`) & !is.na(weibull)), `Distribution fit` = weibull
  ) %>%
  mutate_cond(
    (is.na(`Distribution type`) & !is.na(weibull)), `Distribution type` = "weibull"
  ) %>%
  mutate_cond(
    (is.na(`Distribution fit`) & !is.na(gamma)), `Distribution fit` = gamma
  ) %>%
  mutate_cond(
    (is.na(`Distribution type`) & !is.na(gamma)), `Distribution type` = "gamma"
  ) %>%
  mutate_cond(
    (is.na(`Distribution fit`) & !is.na(exp)), `Distribution fit` = exp
  ) %>%
  mutate_cond(
    (is.na(`Distribution type`) & !is.na(exp)), `Distribution type` = "exp"
  ) %>%
  select(-lognormal, -weibull, -gamma, -exp) %>%

  drop_na() %>% # filter out small subset for Angola which is NA due to drawing too many zeroes in the small sample set

  # summarise
  mutate(fit=ifelse(`Distribution fit`=="yes", 1,0)) %>%
  reframe(`Average p-value` = mean(pvalue, na.rm=T),
          `Average fit possible` = mean(fit,na.rm=T),
          .by = c("sample.size", "Distribution type")) %>%
  mutate(facet = "Ranges (0.05-40 GJ/cap/yr)") %>%

  bind_rows(
    pvals.lognormal.different.seeds %>%
      # clean up awkward dataframe format
      mutate(`Distribution fit` = lognormal,
             `Distribution type` = ifelse(!is.na(lognormal), "lognormal",as.character(NA))) %>%
      mutate_cond(
        (is.na(`Distribution fit`) & !is.na(weibull)), `Distribution fit` = weibull
      ) %>%
      mutate_cond(
        (is.na(`Distribution type`) & !is.na(weibull)), `Distribution type` = "weibull"
      ) %>%
      mutate_cond(
        (is.na(`Distribution fit`) & !is.na(gamma)), `Distribution fit` = gamma
      ) %>%
      mutate_cond(
        (is.na(`Distribution type`) & !is.na(gamma)), `Distribution type` = "gamma"
      ) %>%
      mutate_cond(
        (is.na(`Distribution fit`) & !is.na(exp)), `Distribution fit` = exp
      ) %>%
      mutate_cond(
        (is.na(`Distribution type`) & !is.na(exp)), `Distribution type` = "exp"
      ) %>%
      select(-lognormal, -weibull, -gamma, -exp) %>%

      drop_na() %>% # filter out small subset for Angola which is NA due to drawing too many zeroes in the small sample set

      # summarise
      mutate(fit=ifelse(`Distribution fit`=="yes", 1,0)) %>%
      reframe(`Average p-value` = mean(pvalue, na.rm=T),
              `Average fit possible` = mean(fit,na.rm=T),
              .by = c("sample.size", "Distribution type")
      ) %>%
      mutate(facet = "Full range (all data)")
  )


write_delim(
  x = df.pvalues.bothfacets,
  file = here("analyses", "figures", data.version, figures.version,
              "si-microdata-pvalues_data.csv" ),
  delim = ","
)




### SI.22-27: Downscaling sensitivity analysis ---------------------------------
##### [Supplementary data] Sensitivity data ------------------------------------
data.folder.sensitivity <- file.path(
  PATH.data.desire, "sensitivity"
)
data.folder.sensitivity.downscaling <- file.path(
  data.folder.sensitivity, "downscaling_sensitivity_at_rho1"
)


##### [Supplementary data] Load downscaling sensitivity data [lot of data] -----

# data
data.files.downscaling <- dir(data.folder.sensitivity.downscaling, pattern = "*.RData") %>%
  substr(x = ., start = 0, stop = nchar(.)-nchar(".RData"))

config.all <- NULL
for (i in data.files.downscaling){
  # for (i in data.files){
  config.all <- config.all %>%
    bind_rows(
      vroom(file.path(data.folder.sensitivity.downscaling, paste0(i,"_inputoptions.csv")),show_col_types=FALSE) %>% mutate(file=i)
    )
}
# select just 1 / or all
df.all.downscaled <- NULL
for (i in data.files.downscaling){ # load one selected setting
  df.all.downscaled <- df.all.downscaled %>%
    bind_rows(
      readRDS(file.path(data.folder.sensitivity.downscaling, paste0(i,".RData"))) %>%
        cross_join(vroom(file.path(data.folder.sensitivity.downscaling, paste0(i,"_inputoptions.csv")),show_col_types=FALSE))
    )
}

df.all.downscaled <- df.all.downscaled %>%
  filter(
    iso %in% countries.both.models,
    scenario %in% shape.core.scenarios
  )

# calculate statistics for plotting
df.all.downscaled.stats <- df.all.downscaled %>%
  reframe(
    min.energy = min(energy.per.capita.uncorrected),
    median.energy = median(energy.per.capita.uncorrected),
    max.energy = max(energy.per.capita.uncorrected),
    .by = c("model","scenario", "iso", "variable", "unit", "year")
  )
df.all.downscaled.stats.dlealigned.energy <- df.all.downscaled %>%
  reframe(
    min.energy = min(energy.per.capita),
    median.energy = median(energy.per.capita),
    max.energy = max(energy.per.capita),
    .by = c("model","scenario", "iso", "variable", "unit", "year")
  )

##### SI.22. Plot Residential and Commercial -----------------------------------
p.downscaling.uncertainty.rescom <- ggplot(
  data = df.all.downscaled.stats %>% filter(
    variable=="Final Energy|Residential and Commercial",
    year>=2020, year<=2050,
    scenario=="SDP_MC-1p5C",
    iso %in% countries.both.models
  ) %>% rename.models.and.scenarios(),
  mapping = aes(x=year,
                colour=model)
) +
  facet_wrap(~iso, scales = "free_y") +
  geom_ribbon(aes(ymin=min.energy,
                  ymax=max.energy,
                  fill=model), alpha=0.2) +
  geom_line(aes(y=median.energy,
                  colour=model), linewidth=1.1,
              linetype = "dotted") +
  geom_line(
    data = df.core %>%
      filter(variable=="Final Energy|Residential and Commercial",
             year>=2020, year<=2050,
             scenario=="SDP_MC-1p5C") %>%
      reframe(
        energy.per.capita.uncorrected = mean(energy.per.capita.uncorrected),
        .by = c(#"model",
          "scenario", "iso", "variable", "unit", "year"),
      ) %>%
      mutate(model="Default setting: Model average"),
    aes(y=energy.per.capita.uncorrected),
    linewidth = 1.2, linetype = "dashed", colour = "darkred"
  ) +
  scale_colour_jco() +
  scale_fill_jco() +
  ylab("GJ/cap/yr") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  labs(title="Final energy: Residential and Commercial", subtitle = "Ranges indicate the minimum and maximum across 27 sensitivity methods for the downscaling.\nThe dotted lines are the default setting per model.\nThe dark red dashed line indicates the mean of the default setting of both models.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, "si-downscaling-sensitivity-total-rescom"),
  p = p.downscaling.uncertainty.rescom,
  w = 350,
  h = 350
)
##### SI.23. Plot Transportation -----------------------------------------------
p.downscaling.uncertainty.transport <- ggplot(
  data = df.all.downscaled.stats %>% filter(
    variable=="Final Energy|Transportation",
    year>=2020, year<=2050,
    scenario=="SDP_MC-1p5C",
    iso %in% countries.both.models
  ) %>% rename.models.and.scenarios(),
  mapping = aes(x=year,
                colour=model)
) +
  facet_wrap(~iso, scales = "free_y") +
  geom_ribbon(aes(ymin=min.energy,
                  ymax=max.energy,
                  fill=model), alpha=0.2) +
  geom_line(aes(y=median.energy,
                colour=model), linewidth=1.1,
            linetype = "dotted") +
  geom_line(
    data = df.core %>%
      filter(variable=="Final Energy|Transportation",
             year>=2020, year<=2050,
             scenario=="SDP_MC-1p5C") %>%
      reframe(
        energy.per.capita.uncorrected = mean(energy.per.capita.uncorrected),
        .by = c(#"model",
          "scenario", "iso", "variable", "unit", "year"),
      ) %>%
      mutate(model="Default setting: Model average"),
    aes(y=energy.per.capita.uncorrected),
    linewidth = 1.2, linetype = "dashed", colour = "darkred"
  ) +
  scale_colour_jco() +
  scale_fill_jco() +
  ylab("GJ/cap/yr") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  labs(title="Final energy: Transportation", subtitle = "Ranges indicate the minimum and maximum across 27 sensitivity methods for the downscaling.\nThe dotted lines are the default setting per model.\nThe dark red dashed line indicates the mean of the default setting of both models.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, "si-downscaling-sensitivity-total-transport"),
  p = p.downscaling.uncertainty.transport,
  w = 350,
  h = 350
)
##### SI.24. Plot Industry -----------------------------------------------------
p.downscaling.uncertainty.industry <- ggplot(
  data = df.all.downscaled.stats %>% filter(
    variable=="Final Energy|Industry",
    year>=2020, year<=2050,
    scenario=="SDP_MC-1p5C",
    iso %in% countries.both.models
  ) %>% rename.models.and.scenarios(),
  mapping = aes(x=year,
                colour=model)
) +
  facet_wrap(~iso, scales = "free_y") +
  geom_ribbon(aes(ymin=min.energy,
                  ymax=max.energy,
                  fill=model), alpha=0.2) +
  geom_line(aes(y=median.energy,
                colour=model), linewidth=1.1,
            linetype = "dotted") +
  geom_line(
    data = df.core %>%
      filter(variable=="Final Energy|Industry",
             year>=2020, year<=2050,
             scenario=="SDP_MC-1p5C") %>%
      reframe(
        energy.per.capita.uncorrected = mean(energy.per.capita.uncorrected),
        .by = c(#"model",
          "scenario", "iso", "variable", "unit", "year"),
      ) %>%
      mutate(model="Default setting: Model average"),
    aes(y=energy.per.capita.uncorrected),
    linewidth = 1.2, linetype = "dashed", colour = "darkred"
  ) +
  scale_colour_jco() +
  scale_fill_jco() +
  ylab("GJ/cap/yr") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  labs(title="Final energy: Industry", subtitle = "Ranges indicate the minimum and maximum across 27 sensitivity methods for the downscaling.\nThe dotted lines are the default setting per model.\nThe dark red dashed line indicates the mean of the default setting of both models.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, "si-downscaling-sensitivity-total-industry"),
  p = p.downscaling.uncertainty.industry,
  w = 350,
  h = 350
)








##### SI.25. Plot Residential and Commercial (DLE-aligned) ---------------------
p.downscaling.uncertainty.rescom.dlealigned.energy <- ggplot(
  data = df.all.downscaled.stats.dlealigned.energy %>% filter(
    variable=="Final Energy|Residential and Commercial",
    year>=2020, year<=2050,
    scenario=="SDP_MC-1p5C",
    iso %in% countries.both.models
  ) %>% rename.models.and.scenarios(),
  mapping = aes(x=year,
                colour=model)
) +
  facet_wrap(~iso, scales = "free_y") +
  geom_ribbon(aes(ymin=min.energy,
                  ymax=max.energy,
                  fill=model), alpha=0.2) +
  geom_line(aes(y=median.energy,
                colour=model), linewidth=1.1,
            linetype = "dotted") +
  geom_line(
    data = df.core %>%
      filter(variable=="Final Energy|Residential and Commercial",
             year>=2020, year<=2050,
             scenario=="SDP_MC-1p5C") %>%
      reframe(
        energy.per.capita = mean(energy.per.capita),
        .by = c(#"model",
          "scenario", "iso", "variable", "unit", "year"),
      ) %>%
      mutate(model="Default setting: Model average"),
    aes(y=energy.per.capita),
    linewidth = 1.2, linetype = "dashed", colour = "darkred"
  ) +
  scale_colour_jco() +
  scale_fill_jco() +
  ylab("GJ/cap/yr") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  labs(title="Final energy: Residential and Commercial (DLE-aligned)", subtitle = "Ranges indicate the minimum and maximum across 27 sensitivity methods for the downscaling.\nThe dotted lines are the default setting per model.\nThe dark red dashed line indicates the mean of the default setting of both models.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, "si-downscaling-sensitivity-dlealigned-rescom"),
  p = p.downscaling.uncertainty.rescom.dlealigned.energy,
  w = 350,
  h = 350
)
##### SI.26. Plot Transportation (DLE-aligned) ---------------------------------
p.downscaling.uncertainty.transport.dlealigned.energy <- ggplot(
  data = df.all.downscaled.stats.dlealigned.energy %>% filter(
    variable=="Final Energy|Transportation",
    year>=2020, year<=2050,
    scenario=="SDP_MC-1p5C",
    iso %in% countries.both.models
  ) %>% rename.models.and.scenarios(),
  mapping = aes(x=year,
                colour=model)
) +
  facet_wrap(~iso, scales = "free_y") +
  geom_ribbon(aes(ymin=min.energy,
                  ymax=max.energy,
                  fill=model), alpha=0.2) +
  geom_line(aes(y=median.energy,
                colour=model), linewidth=1.1,
            linetype = "dotted") +
  geom_line(
    data = df.core %>%
      filter(variable=="Final Energy|Transportation",
             year>=2020, year<=2050,
             scenario=="SDP_MC-1p5C") %>%
      reframe(
        energy.per.capita = mean(energy.per.capita),
        .by = c(#"model",
          "scenario", "iso", "variable", "unit", "year"),
      ) %>%
      mutate(model="Default setting: Model average"),
    aes(y=energy.per.capita),
    linewidth = 1.2, linetype = "dashed", colour = "darkred"
  ) +
  scale_colour_jco() +
  scale_fill_jco() +
  ylab("GJ/cap/yr") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  labs(title="Final energy: Transportation (DLE-aligned)", subtitle = "Ranges indicate the minimum and maximum across 27 sensitivity methods for the downscaling.\nThe dotted lines are the default setting per model.\nThe dark red dashed line indicates the mean of the default setting of both models.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, "si-downscaling-sensitivity-dlealigned-transport"),
  p = p.downscaling.uncertainty.transport.dlealigned.energy,
  w = 350,
  h = 350
)
##### SI.27. Plot Industry (DLE-aligned) ---------------------------------------
p.downscaling.uncertainty.industry.dlealigned.energy <- ggplot(
  data = df.all.downscaled.stats.dlealigned.energy %>% filter(
    variable=="Final Energy|Industry",
    year>=2020, year<=2050,
    scenario=="SDP_MC-1p5C",
    iso %in% countries.both.models
  ) %>% rename.models.and.scenarios(),
  mapping = aes(x=year,
                colour=model)
) +
  facet_wrap(~iso, scales = "free_y") +
  geom_ribbon(aes(ymin=min.energy,
                  ymax=max.energy,
                  fill=model), alpha=0.2) +
  geom_line(aes(y=median.energy,
                colour=model), linewidth=1.1,
            linetype = "dotted") +
  geom_line(
    data = df.core %>%
      filter(variable=="Final Energy|Industry",
             year>=2020, year<=2050,
             scenario=="SDP_MC-1p5C") %>%
      reframe(
        energy.per.capita = mean(energy.per.capita),
        .by = c(#"model",
          "scenario", "iso", "variable", "unit", "year"),
      ) %>%
      mutate(model="Default setting: Model average"),
    aes(y=energy.per.capita),
    linewidth = 1.2, linetype = "dashed", colour = "darkred"
  ) +
  scale_colour_jco() +
  scale_fill_jco() +
  ylab("GJ/cap/yr") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  labs(title="Final energy: Industry (DLE-aligned)", subtitle = "Ranges indicate the minimum and maximum across 27 sensitivity methods for the downscaling.\nThe dotted lines are the default setting per model.\nThe dark red dashed line indicates the mean of the default setting of both models.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, "si-downscaling-sensitivity-dlealigned-industry"),
  p = p.downscaling.uncertainty.industry.dlealigned.energy,
  w = 350,
  h = 350
)




### SI.28-31 Additional versions of Main text Figure 2 -------------------------

##### SI.28. Economics (all SDPs) ----------------------------------------------
p.economic.futures <- ggplot(
  data = p.economic.futures.data %>% rename.models.and.scenarios,# %>% filter(scenario!="SSP2-NPi"),
  mapping = aes(x=gdp.perc.growth, y=gini.point.change)
) +
  facet_grid(wb.r3~.) +

  geom_hline(yintercept = 0, colour="darkgrey", linetype = "dashed") +
  geom_vline(xintercept = 0, colour="darkgrey", linetype = "dashed") +


  geom_point(aes(colour=scenario, size=pop_mil), alpha=0.4) +


  geom_xsidedensity(
    aes(
      # y = after_stat(group + ndensity),
      y = after_stat(ndensity),
      x = gdp.perc.growth,
      # fill = scenario,
      colour = scenario
    ),
    # position = "stack",
    linetype = "solid",
    linewidth = 0.5,
    alpha = 0.4
  ) +
  geom_ysidedensity(
    aes(
      # x = after_stat(group + ndensity),
      x = after_stat(ndensity),
      y = gini.point.change,
      # fill = scenario,
      colour = scenario
    ),
    # position = "stack",
    linetype = "solid",
    linewidth = 0.5,
    alpha = 0.4
  ) +
  geom_xsidedensity(
    data = . %>% select(iso,gdp.perc.growth.historical,wb.r3) %>% distinct() %>% drop_na(),
    aes(
      y = after_stat(ndensity),
      x = gdp.perc.growth.historical
    ),
    linetype = "3131",
    linewidth = 0.5
  ) +
  geom_ysidedensity(
    data = . %>% select(iso,gini.point.change.historical,wb.r3) %>% distinct() %>% drop_na(),
    aes(
      x = after_stat(ndensity),
      y = gini.point.change.historical
    ),
    linetype = "3131",
    linewidth = 0.5
  ) +


  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +
  # coord_cartesian(xlim = c(-0.5,5)) +
  theme_minimal() +
  scale_x_continuous(#trans = "log10",
    labels = scales::percent,
    name = "GDP (PPP) per capita growth from 2020 to 2040",
    limits = c(-0.5,5)
  ) +
  scale_y_continuous(name = "Change in gini coefficienct from 2020 to 2040") +
  ggside(x.pos = "bottom",
         y.pos = "left",
         scales = "free") +
  labs(title = "Economic futures",
       caption = "Dashed black marginals are historical trends:\nGDP for 2000 to 2019;\nGini for 1999-2001 to 2017-2019.\nNot showing 4 data points (Burundi and Guinea) at 613-1000% growth.\nDistributions across countries, not population weighted.\nSSP2-1.5C and SSP2-Ref are the same and thus overlap.") +
  # scale_ysidex_continuous(guide = guide_axis(angle = 90), minor_breaks = NULL) +
  scale_ysidex_continuous(minor_breaks = NULL, trans = "reverse", breaks = NULL) +
  scale_xsidey_continuous(minor_breaks = NULL, trans = "reverse", breaks = NULL) +
  theme(#ggside.panel.scale = .4,
    ggside.panel.scale = .2,
    axis.text.x = element_text(angle = 45, hjust = 1))

p.economic.futures


save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-econ-allcorescenarios"),
  p = p.economic.futures,
  w = 200,
  h = 250
)


##### SI.29. Energy (all SDPs) -------------------------------------------------

p.energy.futures <- ggplot(
  data = p.energy.futures.data %>% #filter(scenario!="SSP2-NPi") %>%
    filter(energy.per.capita.change.avg<15) %>%
    mutate_cond(variable=="Residential and Commercial",variable="Residential and\nCommercial") %>% rename.models.and.scenarios,
  mapping = aes(x=energy.per.capita.change.avg,
                y=energy.gini.point.change.avg)
) +
  facet_grid(wb.r3~variable, scales = "free_x") +
  geom_hline(yintercept = 0, colour="darkgrey", linetype = "dashed") +
  geom_vline(xintercept = 0, colour="darkgrey", linetype = "dashed") +
  geom_vline(xintercept = -1, colour="black", linetype = "solid") +

  geom_pointrange2(aes(colour=scenario, fill=scenario, fatten=log(pop_mil),
                       xmin = energy.per.capita.change.min,
                       xmax = energy.per.capita.change.max),
                   alpha=0.4) +


  geom_xsidedensity(
    aes(
      y = after_stat(ndensity),
      x = energy.per.capita.change.avg,
      # fill = scenario,
      colour = scenario
    ),
    # position = "stack",
    linetype = "solid",
    linewidth = 0.5,
    alpha = 0.4
  ) +
  geom_ysidedensity(
    aes(
      x = after_stat(ndensity),
      y = energy.gini.point.change.avg,
      # fill = scenario,
      colour = scenario
    ),
    # position = "stack",
    linetype = "solid",
    linewidth = 0.5,
    alpha = 0.4
  ) +


  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +
  # scale_shape_manual("Model", values=c(21,24)) +

  # coord_cartesian(xlim = c(-0.5,5)) +
  theme_minimal() +
  scale_x_continuous(#trans = "log10",
    labels = scales::percent,
    breaks = c(-1,0, 3, 5, 10,15),
    name = "Energy use per capita growth from 2020 to 2040"
  ) +
  scale_y_continuous(name = "Change in energy use gini coefficienct from 2020 to 2040") +
  ggside(x.pos = "bottom",
         y.pos = "left",
         scales = "free") +
  labs(title = "Energy futures",
       caption = "Size is indicative for population size (not on scale).\nDistributions across countries, not population weighted.") +
  # scale_ysidex_continuous(guide = guide_axis(angle = 90), minor_breaks = NULL) +
  scale_ysidex_continuous(minor_breaks = NULL, trans = "reverse", breaks = NULL) +
  scale_xsidey_continuous(minor_breaks = NULL, trans = "reverse", breaks = NULL) +
  theme(ggside.panel.scale = .3,
        axis.text.x = element_text(angle = 45, hjust = 1))

p.energy.futures

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-energy-allcorescenarios"),
  p = p.energy.futures,
  w = 300,
  h = 250
)

##### SI.30. Energy clean (all SDPs) -------------------------------------------------
# energy.gini.growth
energy.use.growth.clean <- df.core %>%
  filter(year%in%c(2020,2040)) %>%
  select(model,scenario,iso,variable,year,energy.per.capita) %>%
  distinct() %>%  # 3 times smaller (because we drop energy information; 3 sectors)
  pivot_wider(names_from = year, values_from = energy.per.capita) %>%
  mutate(energy.per.capita.change = ((`2040`-`2020`) / `2020`) ) %>%
  select(model,scenario,iso,variable,energy.per.capita.change)


# get avg and uncertainties across models
#energy.gini.growth.uncert
energy.use.growth.uncert.clean <- energy.use.growth.clean %>%
  reframe(
    # note: no uncertainty in the gini change...
    energy.per.capita.change.avg = mean(energy.per.capita.change),
    energy.per.capita.change.min = min(energy.per.capita.change),
    energy.per.capita.change.max = max(energy.per.capita.change),
    .by = c("scenario", "iso", "variable")
  )


p.energy.futures.data.clean <- energy.gini.growth.uncert %>% left_join(energy.use.growth.uncert.clean) %>%
  iamc_variable_keep_one_level(level = -1) %>%
  left_join(wb.income.grouping.3) %>%
  left_join(population.iso.2020 %>% select(-variable) %>% filter(model==vis.model) %>% select(scenario,iso,pop_mil))
p.energy.futures.data.clean$wb.r3 = factor(p.energy.futures.data.clean$wb.r3,
                                     levels=c('Low',
                                              'Middle',
                                              'High'))

p.energy.futures.clean <- ggplot(
  data = p.energy.futures.data.clean %>% #filter(scenario!="SSP2-NPi") %>%
    filter(energy.per.capita.change.avg<15) %>%
    mutate_cond(variable=="Residential and Commercial",variable="Residential and\nCommercial") %>% mutate(model="Model-average") %>% rename.models.and.scenarios,
  mapping = aes(x=energy.per.capita.change.avg,
                y=energy.gini.point.change.avg)
) +
  facet_grid(wb.r3~variable, scales = "free_x") +
  geom_hline(yintercept = 0, colour="darkgrey", linetype = "dashed") +
  geom_vline(xintercept = 0, colour="darkgrey", linetype = "dashed") +
  geom_vline(xintercept = -1, colour="black", linetype = "solid") +


  geom_pointrange2(aes(colour=scenario, fill=scenario, fatten=log(pop_mil),
                       xmin = energy.per.capita.change.min,
                       xmax = energy.per.capita.change.max),
                   alpha=0.4) +


  geom_xsidedensity(
    aes(
      y = after_stat(ndensity),
      x = energy.per.capita.change.avg,
      # fill = scenario,
      colour = scenario
    ),
    # position = "stack",
    linetype = "solid",
    linewidth = 0.5,
    alpha = 0.4
  ) +
  geom_ysidedensity(
    aes(
      x = after_stat(ndensity),
      y = energy.gini.point.change.avg,
      # fill = scenario,
      colour = scenario
    ),
    # position = "stack",
    linetype = "solid",
    linewidth = 0.5,
    alpha = 0.4
  ) +


  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +
  # scale_shape_manual("Model", values=c(21,24)) +

  # coord_cartesian(xlim = c(-0.5,5)) +
  theme_minimal() +
  scale_x_continuous(#trans = "log10",
    labels = scales::percent,
    breaks = c(-1,0, 3, 5, 10,15),
    name = "Energy use per capita growth from 2020 to 2040"
  ) +
  scale_y_continuous(name = "Change in energy use gini coefficienct from 2020 to 2040") +
  ggside(x.pos = "bottom",
         y.pos = "left",
         scales = "free") +
  labs(title = "Energy futures",
       caption = "Size is indicative for population size (not on scale).\nNot showing 5 data points (Benin, Namibia) at >15x growth for Res. and Comm.\nDistributions across countries, not population weighted.") +
  # scale_ysidex_continuous(guide = guide_axis(angle = 90), minor_breaks = NULL) +
  scale_ysidex_continuous(minor_breaks = NULL, trans = "reverse", breaks = NULL) +
  scale_xsidey_continuous(minor_breaks = NULL, trans = "reverse", breaks = NULL) +
  theme(ggside.panel.scale = .3,
        axis.text.x = element_text(angle = 45, hjust = 1))

p.energy.futures.clean


save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-energy-clean-allcorescenarios"),
  p = p.energy.futures.clean,
  w = 300,
  h = 250
)



##### SI.31. Service provisioning (all SDPs) ---------------------------

p.efficiency.futures.pop.weighted <- ggplot(
  data = df.weighted.efficiencies %>% rename.models.and.scenarios,# %>% filter(scenario!="SSP2-NPi"),
  mapping = aes(x=value,
                y=model,
                fill = scenario,
                colour = scenario)
) +
  facet_grid(wb.r3~variable) +
  geom_vline(xintercept = 0, colour="darkgrey", linetype = "dashed") +
  geom_vline(xintercept = -1, colour="black", linetype = "solid") +

  # see: https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
  geom_density_ridges(#rel_min_height = 0.05,
    alpha=0.2,
    aes(height = stat(density)),
    stat = "binline", bins=50, scale = 0.95, draw_baseline = FALSE
    # jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0), point_shape = '|', point_size = 3, point_alpha = 0.1
  ) +

  scale_point_size_continuous(range = c(0.5, 4)) +
  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +

  theme_minimal() +
  scale_x_continuous(
    expand = c(0, 0),
    labels = scales::percent,
    name = "Energy intensity of service provisioning from 2020 to 2040"
  ) +
  scale_y_discrete(
    name = NULL
  ) +
  labs(title = "Service Provisioning Efficiency futures",
       caption = "Population weighted. Industry is modelled as globally uniform.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p.efficiency.futures.pop.weighted
save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-efficiency-allcorescenarios"),
  p = p.efficiency.futures.pop.weighted,
  w = 300,
  h = 150
)



### SI.32-32 Additional versions of Main text Figure 3 -------------------------

##### SI.32. Global Theil Index decomposition (until 2100) ---------------------
p.theil.decomposition.until2100 <- ggplot(
  data = theil.data.prep.T_t %>% filter(year!=2055,year<=2100) %>% mutate(model="model-average") %>%
    rename.models.and.scenarios() %>% iamc_variable_keep_one_level(level = -1),
  aes(x=year,
      fill = scenario,
      colour = scenario)
) +
  facet_grid(variable~scenario) +
  geom_line(aes(y = T_t), alpha = 1) +
  geom_ribbon(aes(ymin = 0, ymax = T_t), alpha = 0.2) +
  geom_ribbon(aes(ymin = 0, ymax = T_t_within), alpha = 1) +

  ylab("Theil Index (T_t) Decomposition") +
  geom_text(label = "between",
            data = . %>% filter(year==2030,
                                scenario=="SSP2-Ref"),
            mapping=aes(x=2021,y=(T_t+T_t_within)/2) , hjust=-0.1) +
  geom_text(label = "within",
            colour = "white",
            data = . %>% filter(scenario=="SSP2-Ref"),
            mapping=aes(x=2021,y=0.05) , hjust=-0.1) +

  labs(
    subtitle = bquote("Global"~bold("between- and within-country")~"inequality"),
  ) +

  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +
  # scale_x_continuous(expand = c(0,0)) +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y.right = element_text(angle = 0))

p.theil.decomposition.until2100

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-theildecomposition-until2100"),
  p = p.theil.decomposition.until2100,
  w = 300,
  h = 300
)

### SI.33-36 Additional versions of Main text Figure 3 -------------------------

# Data for SI. 33 and 34
rem.dep.rescom.sdps.for.vis <- df.core %>%
  reframe(dle.headcount = share.below.projected.adjusted*pop_mil,
          share.below.projected.adjusted=share.below.projected.adjusted,
          .by = c("model", "scenario", "iso", "variable", "year")) %>%
  iamc_variable_keep_one_level(level = -1) %>%
  filter(year%in%c(2020,2040),
         scenario%in%shape.core.scenarios,
         variable=="Residential and Commercial") %>%
  pivot_wider(names_from = year,
              values_from = c(dle.headcount, share.below.projected.adjusted)) %>%
  # range across models and scenarios
  reframe(
    depriv.2020 = mean(`dle.headcount_2020`),
    share.2020 = mean(`share.below.projected.adjusted_2020`),
    depriv.2040 = mean(`dle.headcount_2040`),
    share.2040 = mean(`share.below.projected.adjusted_2040`),
    country.count = 1,
    .by = c("scenario","iso")
  ) %>% mutate(country.count=1)

rem.dep.rescom.sdps.for.vis.2040 <- rem.dep.rescom.sdps.for.vis %>%
  left_join(population.iso %>% filter(year==2040,model==vis.model) %>% select(scenario,iso,pop_mil),
            by=c("scenario","iso")) %>%
  arrange(-share.2040) %>% reframe(countries.cumu = cumsum(country.count),
                                   population.cumu = cumsum(depriv.2040),
                                   share.2040=lag(share.2040, default = 1),
                                   iso=iso,
                                   .by = c("scenario"))
rem.dep.rescom.sdps.for.vis.2020 <- rem.dep.rescom.sdps.for.vis %>%
  left_join(population.iso %>% filter(year==2020,model==vis.model) %>% select(scenario,iso,pop_mil),
            by=c("scenario","iso")) %>%
  arrange(-share.2020) %>% reframe(countries.cumu = cumsum(country.count),
                                   population.cumu = cumsum(depriv.2020),
                                   share.2020=lag(share.2020, default = 1),
                                   .by = c("scenario"))

##### SI.33. Cumulative deprivation headcounts (by country) --------------------

p.rem.depriv.countrycount <- ggplot() +
  geom_line(
    data=rem.dep.rescom.sdps.for.vis.2020,
    aes(colour=scenario,
        y = share.2020, x = countries.cumu),
    linetype = "dotted",
    linewidth = 1.2
  ) +
  geom_line(
    data=rem.dep.rescom.sdps.for.vis.2040,
    aes(colour=scenario,
        y = share.2040, x = countries.cumu),
    linewidth = 1.4
  ) +
  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +
  scale_x_continuous(expand = c(0,0)) +
  xlab("Number of countries") +
  ylab("Share of country below Res. and Comm. DLE threshold") +
  theme_classic() +
  theme_hc()

p.rem.depriv.countrycount

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-headcounts-rescom-cumulative-countries"),
  p = p.rem.depriv.countrycount,
  w = 200,
  h = 200
)


##### SI.34. Cumulative deprivation headcounts (by population) -----------------
p.rem.depriv.population <- ggplot() +
  geom_line(
    data=rem.dep.rescom.sdps.for.vis.2020,
    aes(colour=scenario,
        y = share.2020, x = population.cumu),
    linetype = "dotted",
    linewidth = 1.2
  ) +
  geom_line(
    data=rem.dep.rescom.sdps.for.vis.2040,
    aes(colour=scenario,
        y = share.2040, x = population.cumu),
    linewidth = 1.4
  ) +
  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +
  scale_x_continuous(expand = c(0,0)) +
  xlab("Deprivation headcount [Million]") +
  ylab("Share of country below Res. and Comm. DLE threshold") +
  theme_classic() +
  theme_hc()

p.rem.depriv.population

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-headcounts-rescom-cumulative-population"),
  p = p.rem.depriv.population,
  w = 200,
  h = 200
)



##### SI.35. Transport headcount timeseries ------------------------------------
p.dle.headcount.futures.transport <- ggplot(
  data = p.dle.headcount.data %>% ms_add() %>%
    reframe(dle.headcount = sum(dle.headcount),
            .by = c("model", "scenario", "variable", "year")) %>%
    iamc_variable_keep_one_level(level = -1) %>%
    filter(year>=2020,
           year<=DLE.END.YEAR.TIMESERIES,
           variable=="Transportation") %>%
    rename.models.and.scenarios %>%
    rename(Model=model),
  mapping = aes(x=year)
) +
  # facet_grid(.~scenario) +

  geom_ribbon(
    data = . %>%
      filter(grepl(scenario,pattern="SDP-",fixed=T)) %>%
      reframe(min.dle.headcount = min(dle.headcount), max.dle.headcount = max(dle.headcount), .by=c("variable", "year")),
    fill = "dodgerblue",
    aes(ymin=min.dle.headcount,ymax=max.dle.headcount),
    alpha=0.2
  ) +
  geom_ribbon(
    data = . %>%
      filter(!grepl(scenario,pattern="SDP-",fixed=T)) %>%
      reframe(min.dle.headcount = min(dle.headcount), max.dle.headcount = max(dle.headcount), .by=c("variable", "year")),
    fill = "grey",
    aes(ymin=min.dle.headcount,ymax=max.dle.headcount),
    alpha=0.2
  ) +

  geom_line(aes(y=dle.headcount,
                color=scenario,
                linetype = Model)) +
  labs(subtitle = bquote("Population below minimum energy requirement (DLE) threshold over time:" ~ bold("Transportation")),
       caption = "Blue range: SDPs. Grey range: SSP2.") +
  scale_ysidex_continuous(minor_breaks = NULL, breaks = NULL) +
  scale_xsidey_continuous(minor_breaks = NULL, breaks = NULL) +

  scale_x_continuous(name = NULL,
                     expand = c(0,0)) +
  scale_y_continuous(name = "Million",
                     expand = c(0,0),
                     limits = c(0,8000)) +

  scale_color_manual("Scenario", values=shape.color.coding,) +

  theme_classic() +
  theme_hc() +
  theme(ggside.panel.scale = .3,
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y.right = element_text(angle = 0))

p.dle.headcount.futures.transport


save_ggplot(
  # f = "C:/Users/kikstra/OneDrive - IIASA/_PhD/Thesis/Figures/viva/scenarios/si-headcounts-transport-timeseries",
  f = here("analyses", "figures", data.version, figures.version,
           "si-headcounts-transport-timeseries"),
  p = p.dle.headcount.futures.transport,
  w = 300,
  h = 200
)


##### SI.36. ResCom headcount timeseries (income classifications) --------------
p.dle.headcount.futures.rescom.incomegroups <- ggplot(
  data = p.dle.headcount.data %>% ms_add() %>%
    reframe(dle.headcount = sum(dle.headcount),
            .by = c("model", "wb.r3", "scenario", "variable", "year")) %>%
    iamc_variable_keep_one_level(level = -1) %>%
    filter(year>=2020,
           year<=DLE.END.YEAR.TIMESERIES,
           variable=="Residential and Commercial") %>%
    rename.models.and.scenarios %>%
    rename(Model=model),
  mapping = aes(x=year)
) +
  facet_grid(.~wb.r3) +

  geom_ribbon(
    data = . %>%
      filter(grepl(scenario,pattern="SDP-",fixed=T)) %>%
      reframe(min.dle.headcount = min(dle.headcount), max.dle.headcount = max(dle.headcount), .by=c("variable", "wb.r3", "year")),
    fill = "dodgerblue",
    aes(ymin=min.dle.headcount,ymax=max.dle.headcount),
    alpha=0.2
  ) +
  geom_ribbon(
    data = . %>%
      filter(!grepl(scenario,pattern="SDP-",fixed=T)) %>%
      reframe(min.dle.headcount = min(dle.headcount), max.dle.headcount = max(dle.headcount), .by=c("variable", "wb.r3", "year")),
    fill = "grey",
    aes(ymin=min.dle.headcount,ymax=max.dle.headcount),
    alpha=0.2
  ) +

  geom_line(aes(y=dle.headcount,
                color=scenario,
                linetype = Model)) +
  labs(subtitle = bquote("Population below minimum energy requirement (DLE) threshold over time:" ~ bold("Residential and Commercial"))) +
  scale_ysidex_continuous(minor_breaks = NULL, breaks = NULL) +
  scale_xsidey_continuous(minor_breaks = NULL, breaks = NULL) +

  scale_x_continuous(name = NULL,
                     expand = c(0,0)) +
  scale_y_continuous(name = "Million",
                     expand = c(0,0),
                     limits = c(0,5000)) +

  scale_color_manual("Scenario", values=shape.color.coding) +

  theme_classic() +
  theme_hc() +
  theme(ggside.panel.scale = .3,
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(2, "lines"),
        strip.text.y.right = element_text(angle = 0))

p.dle.headcount.futures.rescom.incomegroups


save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-headcounts-rescom-timeseries-incomegroups"),
  p = p.dle.headcount.futures.rescom.incomegroups,
  w = 250,
  h = 200
)


### SI.37-38 Additional versions of Main text Figure 5 -------------------------

##### SI.37. Regional breakdown of global ResCom distribution for 2020 ---------

p.energydist.zoomin.regional.2020 <-  dle.aligned.sampled.data.rescaled %>% filter(
    variable=="Final Energy|Residential and Commercial",
    scenario==vis.scenario,
    year==2020,
    fe.scaled.to.dle<3
  ) %>% left_join(ipcc.r10) %>%
  ggplot(
    aes(x=fe.scaled.to.dle, fill=r10, weight=pop_mil)
  ) +
  geom_vline(xintercept = 1, linewidth=0.7, linetype = "dashed", colour = "grey") +
  geom_histogram(binwidth=0.05, boundary=0) +
  theme_minimal() +
  scale_fill_viridis_d() +
  # theme(legend.position = "none") +
  labs(title="Regional composition of the global deprivation distribution (2020), SDP-RC",
       subtitle = "Residential and Commercial energy per capita, rescaled to DLE",
       x="Multiples of DLE threshold",
       y="Population (millions)")

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-energydist-deprivationcompostion-2020"),
  p = p.energydist.zoomin.regional.2020,
  w = 250,
  h = 150
)

##### SI.38. Regional breakdown of global ResCom distribution for 2040 ---------

p.energydist.zoomin.regional.2040 <-  dle.aligned.sampled.data.rescaled %>% filter(
  variable=="Final Energy|Residential and Commercial",
  scenario==vis.scenario,
  year==2040,
  fe.scaled.to.dle<3
) %>% left_join(ipcc.r10) %>%
  ggplot(
    aes(x=fe.scaled.to.dle, fill=r10, weight=pop_mil)
  ) +
  geom_vline(xintercept = 1, linewidth=0.7, linetype = "dashed", colour = "grey") +
  geom_histogram(binwidth=0.05, boundary=0) +
  theme_minimal() +
  scale_fill_viridis_d() +
  # theme(legend.position = "none") +
  labs(title="Regional composition of the global deprivation distribution (2040), SDP-RC",
       subtitle = "Residential and Commercial energy per capita, rescaled to DLE",
       x="Multiples of DLE threshold",
       y="Population (millions)")

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-energydist-deprivationcompostion-2040"),
  p = p.energydist.zoomin.regional.2040,
  w = 250,
  h = 150
)

### SI.39-41 Additional versions of Main text Figure 6 -------------------------

##### SI.39. Overview per capita, all regions, all scenarios -------------------

p.EDG.all.relevant.lines.SI <- ggplot(
  data = p.EDG.summedsectors.minmaxavg.data %>%
    # iamc_variable_keep_one_level(level = -1) %>%
    filter(year %in% c(2020,2030,2040),
           scenario %in% shape.core.scenarios,
    ) %>%
    left_join(pop.r10.projection) %>%
    enterise_long_r10_names() %>% rename.models.and.scenarios,
  mapping = aes(x=year)
) +
  facet_grid(scenario~r10, scales = "free_y") +

  geom_line(aes(y=total.energy.use.avg/pop_mil*exa/giga/mega,
                color = scenario,
                linetype = "Total energy use"),
            linewidth = 1.5) +

  geom_line(aes(y=dle.threshold.avg/pop_mil*exa/giga/mega,
                color = scenario,
                linetype = "DLE threshold"),
            # linetype = "dashed",
            linewidth = 1) +

  geom_area(aes(y=(dle.threshold.avg-energy.development.gap.avg)/pop_mil*exa/giga/mega,
                fill = scenario),
            alpha = 0.5) +

  geom_segment(data = . %>% filter(year==2020),
             aes(x=2020+1,xend=2020+1,
                 y = (total.energy.use.avg)/pop_mil*exa/giga/mega,
                 yend = (dle.threshold.avg)/pop_mil*exa/giga/mega
             ),
             colour = "pink") +
  geom_point(data = . %>% filter(year==2020),
             aes(x=2020+1,y=(total.energy.use.avg)/pop_mil*exa/giga/mega ),
             colour = "pink") +
  geom_point(data = . %>% filter(year==2020),
             aes(x=2020+1,y=(dle.threshold.avg)/pop_mil*exa/giga/mega ),
             colour = "pink") +

  # 2040:
  geom_segment(data = . %>% filter(year==2040),
               aes(x=2040-1,xend=2040-1,
                   y = (total.energy.use.avg)/pop_mil*exa/giga/mega,
                   yend = (dle.threshold.avg)/pop_mil*exa/giga/mega
               ),
               colour = "pink") +
  geom_point(data = . %>% filter(year==2040),
             aes(x=2040-1,y=(total.energy.use.avg)/pop_mil*exa/giga/mega ),
             colour = "pink") +
  geom_point(data = . %>% filter(year==2040),
             aes(x=2040-1,y=(dle.threshold.avg)/pop_mil*exa/giga/mega ),
             colour = "pink") +


  # energy gap
  # 2020:
  geom_segment(data = . %>% filter(year==2020),
               aes(x=2020,xend=2020,
                   y = (dle.threshold.avg)/pop_mil*exa/giga/mega,
                   yend = (dle.threshold.avg-energy.development.gap.avg)/pop_mil*exa/giga/mega
               ),
               colour = "red") +
  geom_point(data = . %>% filter(year==2020),
             aes(x=2020,y=(dle.threshold.avg)/pop_mil*exa/giga/mega ),
             colour = "red") +
  geom_point(data = . %>% filter(year==2020),
             aes(x=2020,y=(dle.threshold.avg-energy.development.gap.avg)/pop_mil*exa/giga/mega ),
             colour = "red") +

  # 2040:
  geom_segment(data = . %>% filter(year==2040),
               aes(x=2040,xend=2040,
                   y = (dle.threshold.avg)/pop_mil*exa/giga/mega,
                   yend = (dle.threshold.avg-energy.development.gap.avg)/pop_mil*exa/giga/mega
               ),
               colour = "red") +
  geom_point(data = . %>% filter(year==2040),
             aes(x=2040,y=(dle.threshold.avg)/pop_mil*exa/giga/mega ),
             colour = "red") +
  geom_point(data = . %>% filter(year==2040),
             aes(x=2040,y=(dle.threshold.avg-energy.development.gap.avg)/pop_mil*exa/giga/mega ),
             colour = "red") +



  scale_color_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +

  scale_linetype_manual("",
                        breaks = c("Total energy use","DLE threshold"),
                        values = c("solid", "dashed")) +

  scale_y_continuous(name = "GJ/cap/year") +
  xlab(NULL) +

  labs(subtitle = bquote("The"~bold("energy needs gap")~"and"~bold("energy headroom")~"per capita, across scenarios"),
       # caption = "Energy needs gap: red vertical lines, in 2020 and 2040\nEnergy above DLS: pink vertical lines, in 2020 and 2040"
  ) +

  theme_classic() +
  theme_hc() +
  theme(#ggside.panel.scale = .3,
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y.right = element_text(angle = 0))

p.EDG.all.relevant.lines.SI


save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-alllines-allscenarios"),
  p = p.EDG.all.relevant.lines.SI,
  w = 350,
  h = 350
)



##### SI.40. Energy needs gap, all regions, all scenarios (IMAGE) --------------
p.EDG.sectors.models.SI.IMAGE <- ggplot(
  data = p.EDG.data %>%
    filter(year %in% c(2020,2030,2040),
           scenario %in% shape.core.scenarios,
           model == "IMAGE 3.3"
    ) %>%
    # left_join(pop.r10.projection) %>%
    enterise_long_r10_names() %>% rename.models.and.scenarios() %>% iamc_variable_keep_one_level(level = -1),
  mapping = aes(x = year, y = energy.development.gap, fill = variable, group = r10)
) +
  geom_col() +
  facet_grid(scenario~r10) +


  scale_y_continuous(name = "EJ/year") +
  xlab(NULL) +

  labs(title = bquote("Closing of the total"~bold("energy needs gap")~"across sectors"),
       subtitle = "IMAGE") +

  theme_classic() +
  theme_hc() +
  scale_fill_ptol(name = NULL) +
  theme(#ggside.panel.scale = .3,
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y.right = element_text(angle = 0))
p.EDG.sectors.models.SI.IMAGE

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-energygap-allscenarios-IMAGE"),
  p = p.EDG.sectors.models.SI.IMAGE,
  w = 350,
  h = 350
)

##### SI.41. Energy needs gap, all regions, all scenarios (IMAGE) --------------
p.EDG.sectors.models.SI.REMIND <- ggplot(
  data = p.EDG.data %>%
    filter(year %in% c(2020,2030,2040),
           scenario %in% shape.core.scenarios,
           model == "REMIND-MAgPIE 3.2-4.6"
    ) %>%
    # left_join(pop.r10.projection) %>%
    enterise_long_r10_names() %>% rename.models.and.scenarios() %>% iamc_variable_keep_one_level(level = -1),
  mapping = aes(x = year, y = energy.development.gap, fill = variable, group = r10)
) +
  geom_col() +
  facet_grid(scenario~r10) +


  scale_y_continuous(name = "EJ/year") +
  xlab(NULL) +

  labs(title = bquote("Closing of the total"~bold("energy needs gap")~"across sectors"),
       subtitle = "REMIND") +

  theme_classic() +
  theme_hc() +
  scale_fill_ptol(name = NULL) +
  theme(#ggside.panel.scale = .3,
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y.right = element_text(angle = 0))
p.EDG.sectors.models.SI.REMIND

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-energygap-allscenarios-REMIND"),
  p = p.EDG.sectors.models.SI.REMIND,
  w = 350,
  h = 350
)






### SI.42-48 Additional versions of Main text Figure 7 -------------------------

##### SI.42. Overview per capita, all regions, all scenarios (emissions) -------
p.emissions.timseries.SI <- ggplot(df.core.total.energy.emissions.meanacrossmodels.percapita %>% left_join(load_long_names_of_iso3c()) %>% ms_add() %>%
                                            filter(scenario %in% shape.core.scenarios,
                                                   # iso %in% c("USA", "IND", "CHN", "NGA", "IDN"),
                                                   # iso %in% c("USA", "CHN"),
                                                   iso %in% c("ETH", "USA"),
                                                   year>=2020,
                                                   year!=2055, # only reported by 1 model
                                                   year<=2050) %>% rename.models.and.scenarios,
                                          aes(group=`model-scenario`)) +
  facet_grid(name~scenario, scales = "free_y") +
  geom_hline(aes(yintercept=0),
             color = "black") +
  geom_ribbon(aes(x=year,
                  ymin=0,
                  ymax=dls.co2.emissions.threshold-dls.co2.emissions.gap,
                  fill = scenario),
              alpha = 0.3) +
  geom_line(aes(x=year,
                y=dls.co2.emissions.threshold,
                color = scenario),
            linetype = "dashed",
            linewidth = 1) +
  geom_line(aes(x=year,
                y=co2.emissions,
                color = scenario),
            linetype = "solid",
            linewidth = 1.5) +
  labs(subtitle = "CO2 emissions (excl. LULUCF)",
       caption = "Solid line: total emissions\nDashed line: emissions related to DLE threshold (DLS for all)\nArea: emissions related to provided DLS") +
  ylab("tCO2/cap/year") +
  xlab(NULL) +
  theme_classic() +
  theme_hc() +
  scale_colour_manual("Scenario", values=shape.color.coding) +
  scale_fill_manual("Scenario", values=shape.color.coding) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")
p.emissions.timseries.SI

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-emissions-timeseries-all-lines"),
  p = p.emissions.timseries.SI,
  w = 250,
  h = 250
)

##### SI.43. Treemap - until 2050, SDP-EI -------

p.treemap.DLS.emissions.until.2050.SI.EI <- ggplot(
    create_treemap_data(
    emissions.data.cumulative.from.2024 = emissions.data.extract.cumulative.from.2024,
    scen="SDP_EI-1p5C",
    end.year=2050),
                                             aes(area = cumulative.dls.co2.emissions.threshold.mean,
                                                 fill = region_ar6_10_ipcc_fgd,
                                                 label = name.perc,
                                                 subgroup = developed.developing.share.mean,
                                                 subgroup2 = region_ar6_10_ipcc_fgd)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "black", size = 5) +
  geom_treemap_subgroup_text(place = "bottomright", grow = F, alpha = 0.5,
                             colour = "white", fontface = "italic",
                             min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  # scale_fill_manual(
  #   "Region (IPCC)", values=ipcc.r10.color.coding
  # ) +
  scale_fill_bluebrown() +
  # theme(legend.position = "bottom",
  #       # legend.title = element_blank()
  #       ) +
  theme(legend.position = "none") +
  labs(
    subtitle = "Share of cumulative Decent Living Emissions until 2050",
    caption = "Scenario SDP-EI, 2024-2050, average emissions for DLS for all across models, with population growth following SSP1.\nColouring according to IPCC R10 region (AR6). North/South split following UN developing/developed classification."
  )


p.treemap.DLS.emissions.until.2050.SI.EI


save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-treemap-2050-EI"),
  p = p.treemap.DLS.emissions.until.2050.SI.EI,
  w = 250,
  h = 200
)


##### SI.44. Treemap - until 2050, SDP-MC -------

p.treemap.DLS.emissions.until.2050.SI.MC <- ggplot(
  create_treemap_data(
    emissions.data.cumulative.from.2024 = emissions.data.extract.cumulative.from.2024,
    scen="SDP_MC-1p5C",
    end.year=2050),
  aes(area = cumulative.dls.co2.emissions.threshold.mean,
      fill = region_ar6_10_ipcc_fgd,
      label = name.perc,
      subgroup = developed.developing.share.mean,
      subgroup2 = region_ar6_10_ipcc_fgd)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "black", size = 5) +
  geom_treemap_subgroup_text(place = "bottomright", grow = F, alpha = 0.5,
                             colour = "white", fontface = "italic",
                             min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  # scale_fill_manual(
  #   "Region (IPCC)", values=ipcc.r10.color.coding
  # ) +
  scale_fill_bluebrown() +
  # theme(legend.position = "bottom",
  #       # legend.title = element_blank()
  #       ) +
  theme(legend.position = "none") +
  labs(
    subtitle = "Share of cumulative Decent Living Emissions until 2050",
    caption = "Scenario SDP-MC, 2024-2050, average emissions for DLS for all across models, with population growth following SSP1.\nColouring according to IPCC R10 region (AR6). North/South split following UN developing/developed classification."
  )


p.treemap.DLS.emissions.until.2050.SI.MC


save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-treemap-2050-MC"),
  p = p.treemap.DLS.emissions.until.2050.SI.MC,
  w = 250,
  h = 200
)

##### SI.45. Treemap - until 2050, SDP-MC -------

p.treemap.DLS.emissions.until.2050.SI.ssp215 <- ggplot(
  create_treemap_data(
    emissions.data.cumulative.from.2024 = emissions.data.extract.cumulative.from.2024,
    scen="SSP2-1p5C",
    end.year=2050),
  aes(area = cumulative.dls.co2.emissions.threshold.mean,
      fill = region_ar6_10_ipcc_fgd,
      label = name.perc,
      subgroup = developed.developing.share.mean,
      subgroup2 = region_ar6_10_ipcc_fgd)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "black", size = 5) +
  geom_treemap_subgroup_text(place = "bottomright", grow = F, alpha = 0.5,
                             colour = "white", fontface = "italic",
                             min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  # scale_fill_manual(
  #   "Region (IPCC)", values=ipcc.r10.color.coding
  # ) +
  scale_fill_bluebrown() +
  # theme(legend.position = "bottom",
  #       # legend.title = element_blank()
  #       ) +
  theme(legend.position = "none") +
  labs(
    subtitle = "Share of cumulative Decent Living Emissions until 2050",
    caption = "Scenario SSP2-1.5C, 2024-2050, average emissions for DLS for all across models, with population growth following SSP1.\nColouring according to IPCC R10 region (AR6). North/South split following UN developing/developed classification."
  )


p.treemap.DLS.emissions.until.2050.SI.ssp215


save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-treemap-2050-ssp215"),
  p = p.treemap.DLS.emissions.until.2050.SI.ssp215,
  w = 250,
  h = 200
)

##### SI.46. Treemap - until 2035, SDP-RC -------

p.treemap.DLS.emissions.until.2035.SI.RC <- ggplot(
  create_treemap_data(
    emissions.data.cumulative.from.2024 = emissions.data.extract.cumulative.from.2024,
    scen="SDP_RC-1p5C",
    end.year=2035),
  aes(area = cumulative.dls.co2.emissions.threshold.mean,
      fill = region_ar6_10_ipcc_fgd,
      label = name.perc,
      subgroup = developed.developing.share.mean,
      subgroup2 = region_ar6_10_ipcc_fgd)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "black", size = 5) +
  geom_treemap_subgroup_text(place = "bottomright", grow = F, alpha = 0.5,
                             colour = "white", fontface = "italic",
                             min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  # scale_fill_manual(
  #   "Region (IPCC)", values=ipcc.r10.color.coding
  # ) +
  scale_fill_bluebrown() +
  # theme(legend.position = "bottom",
  #       # legend.title = element_blank()
  #       ) +
  theme(legend.position = "none") +
  labs(
    subtitle = "Share of cumulative Decent Living Emissions until 2035",
    caption = "Scenario SDP-RC, 2024-2035, average emissions for DLS for all across models, with population growth following SSP1.\nColouring according to IPCC R10 region (AR6). North/South split following UN developing/developed classification."
  )


p.treemap.DLS.emissions.until.2035.SI.RC


save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-treemap-2035-RC"),
  p = p.treemap.DLS.emissions.until.2035.SI.RC,
  w = 250,
  h = 200
)

##### SI.47. Treemap - until 2070, SDP-RC -------

p.treemap.DLS.emissions.until.2070.SI.RC <- ggplot(
  create_treemap_data(
    emissions.data.cumulative.from.2024 = emissions.data.extract.cumulative.from.2024,
    scen="SDP_RC-1p5C",
    end.year=2070),
  aes(area = cumulative.dls.co2.emissions.threshold.mean,
      fill = region_ar6_10_ipcc_fgd,
      label = name.perc,
      subgroup = developed.developing.share.mean,
      subgroup2 = region_ar6_10_ipcc_fgd)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "black", size = 5) +
  geom_treemap_subgroup_text(place = "bottomright", grow = F, alpha = 0.5,
                             colour = "white", fontface = "italic",
                             min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  # scale_fill_manual(
  #   "Region (IPCC)", values=ipcc.r10.color.coding
  # ) +
  scale_fill_bluebrown() +
  # theme(legend.position = "bottom",
  #       # legend.title = element_blank()
  #       ) +
  theme(legend.position = "none") +
  labs(
    subtitle = "Share of cumulative Decent Living Emissions until 2070",
    caption = "Scenario SDP-RC, 2024-2070, average emissions for DLS for all across models, with population growth following SSP1.\nColouring according to IPCC R10 region (AR6). North/South split following UN developing/developed classification."
  )


p.treemap.DLS.emissions.until.2070.SI.RC


save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-treemap-2070-RC"),
  p = p.treemap.DLS.emissions.until.2070.SI.RC,
  w = 250,
  h = 200
)


##### SI.48. Carbon budgets - all scenarios ------------------------------------
p.RCB.comparison.SI <- ggplot(
  RCB.vs.cumulativeDLS.data %>% filter(scenario %in% shape.core.scenarios,
                                       RCB.threshold %nin% c("1.5C (10%)",
                                                             "2C (50%)",
                                                             "1.5C (17%)")) %>%
    mutate(model="model-average") %>% rename.models.and.scenarios,
) +
  facet_grid(scenario~RCB.threshold) +

  geom_rect(aes(xmin=-sqrt(cumulative.dls.co2.emissions.until2050.all.mean),
                xmax=0,
                ymin=0,
                ymax=sqrt(cumulative.dls.co2.emissions.until2050.all.mean),
                fill=scenario),
            alpha = 0.3) +
  geom_text(aes(x=-sqrt(cumulative.dls.co2.emissions.until2050.all.mean)+0.1*sqrt(cumulative.dls.co2.emissions.until2050.all.mean),
                y=sqrt(sqrt(cumulative.dls.co2.emissions.until2050.all.mean)),
                label=paste0(as.character(round(cumulative.dls.co2.emissions.until2050.all.mean/RCB, 2)*100), "%")   ),
            hjust = 0,
            vjust = 1,
            size = 3) +

  geom_rect(aes(xmin=-sqrt(RCB),
                xmax=0,
                ymin=0,
                ymax=sqrt(RCB)),
            color = "black",
            fill = "white",
            linetype = "dotted",
            alpha = 0) +

  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual("Scenario", values=shape.color.coding) +
  scale_colour_manual("Scenario", values=shape.color.coding) +
  labs(subtitle = "Cumulative CO2 Emissions (excl. LULUCF) until 2050 for DLS for all compared to Remaining Carbon Budgets\n")
p.RCB.comparison.SI

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-RCB-allscenarios"),
  p = p.RCB.comparison.SI,
  w = 250,
  h = 250
)


### ST.5-8 Country groupings ---------------------------------------------------
##### ST.5. World Bank ---------------------------------------------------------
wb.country.groupings <- data.frame(
  low = wb.income.grouping.3 %>% filter(wb.r4=="Low") %>% pull(iso) %>% unique() %>% sort() %>% paste(collapse = ", "),
  lmic = wb.income.grouping.3 %>% filter(wb.r4=="Lower-middle") %>% pull(iso) %>% unique() %>% sort() %>% paste(collapse = ", "),
  umic = wb.income.grouping.3 %>% filter(wb.r4=="Upper middle") %>% pull(iso) %>% unique() %>% sort() %>% paste(collapse = ", "),
  high = wb.income.grouping.3 %>% filter(wb.r4=="High") %>% pull(iso) %>% unique() %>% sort() %>% paste(collapse = ", ")
)
write_delim(
  x = wb.country.groupings,
  file = here("analyses", "figures", data.version, figures.version,
              "st-country-groupings-worldbank.csv"),
  delim = ","
)

##### ST.6. IPCC R10 -----------------------------------------------------------
ipcc.r10.groupings <- NULL
for (r in ipcc.r10$r10 %>% unique()){
  ipcc.r10.groupings <- ipcc.r10.groupings %>%
    bind_rows(
      data.frame(
      region = r,
      countries = ipcc.r10 %>% filter(r10==r) %>% pull(iso) %>% unique() %>% sort() %>% paste(collapse = ", ")
    ))
}
ipcc.r10.groupings

write_delim(
  x = ipcc.r10.groupings,
  file = here("analyses", "figures", data.version, figures.version,
              "st-country-groupings-IPCCr10.csv"),
  delim = ","
)

##### ST.7. IMAGE --------------------------------------------------------------
image.groupings <- NULL
for (r in regions.image$region.image %>% unique()){
  image.groupings <- image.groupings %>%
    bind_rows(
      data.frame(
        region = r,
        countries = regions.image %>% filter(region.image==r) %>% pull(iso) %>% unique() %>% sort() %>% paste(collapse = ", ")
      ))
}
image.groupings

write_delim(
  x = image.groupings %>% arrange(region),
  file = here("analyses", "figures", data.version, figures.version,
              "st-country-groupings-IMAGE.csv"),
  delim = ","
)

##### ST.8. REMIND -------------------------------------------------------------
remind.groupings <- NULL
for (r in regions.remind$region.remind %>% unique()){
  remind.groupings <- remind.groupings %>%
    bind_rows(
      data.frame(
        region = r,
        countries = regions.remind %>% filter(region.remind==r) %>% pull(iso) %>% unique() %>% sort() %>% paste(collapse = ", ")
      ))
}
remind.groupings

write_delim(
  x = remind.groupings %>% arrange(region),
  file = here("analyses", "figures", data.version, figures.version,
              "st-country-groupings-REMIND.csv"),
  delim = ","
)



### ST.9. SDP narratives -------------------------------------------------------

# Table not produced in this script.


### SI.49. Temperature ---------------------------------------------------------
# library(geomtextpath)

p.sdps.temp <- ggplot(
  data = iam.temperatures %>% filter(scenario%in%shape.core.scenarios,
                                     # model==vis.model
                                     ) %>%
    rename.models.and.scenarios() %>% filter(
    year>=2015
  ),
  aes(x = year, color = scenario, y = value)
) +
  facet_grid(~model) +
  # geom_texthline(yintercept = 1.5, label = "1.5\u00B0C", linetype = 2, color = "black", vjust = -0.4, hjust = 0.92) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = 2020, linetype = "dotted") +
  geom_vline(xintercept = 2040, linetype = "dotted") +
  theme_classic() +
  theme_hc() +
  theme(legend.position = "right", legend.title = element_blank()) +
  ylab("Temperature above 1850-1900 [\u00B0C]") +
  xlab(NULL) +
  scale_x_continuous(breaks = seq(2020,2100,10),labels = seq(2020,2100,10),
                     limits = c(2015,2100),
                     expand = c(0,0)) +
  labs(#caption = "Using only REMIND for visualisation.",
       title = "Global Surface Temperature change",
       subtitle = "Using MAGICCv7.5.3 in IPCC AR6 setup, 50.0th percentile") +
  scale_color_manual(values = shape.color.coding) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p.sdps.temp


save_ggplot(
  f = here("analyses", "figures", data.version, figures.version,
           "si-temperature"),
  p = p.sdps.temp,
  w = 200,
  h = 150
)


### ST.10. Emissions for DLS by country (2020) ---------------------------------
# N.B. depends on processing for Figure 7
dls.emissions.thresholds.current.2020 <- df.core.total.energy.emissions %>%
  filter(scenario%in%shape.sdps) %>%
  left_join(population.iso %>% filter(model==vis.model) %>% select(scenario,iso,year,pop_mil)) %>%
  mutate(
    dls.co2.emissions.threshold = dls.co2.emissions.threshold/pop_mil/1e6*giga,
    dls.ghg.emissions.threshold = dls.ghg.emissions.threshold/pop_mil/1e6*giga
  ) %>%
  reframe(
    # summarise across model-scenario uncertainty
    # - CO2
    dls.co2.emissions.threshold.mean = mean(dls.co2.emissions.threshold),
    dls.co2.emissions.threshold.min = min(dls.co2.emissions.threshold),
    dls.co2.emissions.threshold.max = max(dls.co2.emissions.threshold),
    # - GHGs
    dls.ghg.emissions.threshold.mean = mean(dls.ghg.emissions.threshold),
    dls.ghg.emissions.threshold.min = min(dls.ghg.emissions.threshold),
    dls.ghg.emissions.threshold.max = max(dls.ghg.emissions.threshold),
    .by = c("iso", "year")
  ) %>%
  filter(year%in%c(2020)) %>%
  select(iso,year,
         dls.co2.emissions.threshold.mean,
         dls.co2.emissions.threshold.min,
         dls.co2.emissions.threshold.max,
         dls.ghg.emissions.threshold.mean,
         dls.ghg.emissions.threshold.min,
         dls.ghg.emissions.threshold.max) %>%
  mutate(unit = "tCO2/cap/yr and tCO2eq/cap/yr") %>%
  mutate(note.ranges = "Ranges span the uncertainty in the starting point data [i.e., 2020] across two models (REMIND and IMAGE) and three scenarios (three 1.5C-compatible sustainable development pathways)") %>%
  mutate(note.co2 = "Calculated based on (`Emissions|CO2` - `Emissions|CO2|LULUCF Direct+Indirect`)") %>%
  mutate(note.ghg = "Calculated based on `Emissions|Kyoto Gases (incl. indirect AFOLU)`")

dls.emissions.thresholds.current.table.2020 <- dls.emissions.thresholds.current.2020 %>%
  mutate(`CO2 (tCO2/cap/yr)` = paste0(
    as.character(round(dls.co2.emissions.threshold.mean, digits = 1)), " (",
    as.character(round(dls.co2.emissions.threshold.min, digits = 1)), "-",
    as.character(round(dls.co2.emissions.threshold.max, digits = 1)), ")"
  )) %>%
  mutate(`GHGs (tCO2eq/cap/yr)` = paste0(
    as.character(round(dls.ghg.emissions.threshold.mean, digits = 1)), " (",
    as.character(round(dls.ghg.emissions.threshold.min, digits = 1)), "-",
    as.character(round(dls.ghg.emissions.threshold.max, digits = 1)), ")"
  )) %>%
  select(iso,year,`CO2 (tCO2/cap/yr)`,`GHGs (tCO2eq/cap/yr)`)

dls.emissions.thresholds.current.table.2020

write_delim(
  dls.emissions.thresholds.current.table.2020,
  file = here("analyses", "figures", data.version, figures.version,
              paste0("st-DLS-emissions-thresholds-desire-percapita_2020.csv") ),
  delim = ","
)


### ST.11. Emissions for DLS by country (2050) ---------------------------------
# N.B. depends on processing for Figure 7
dls.emissions.thresholds.current.2050 <- df.core.total.energy.emissions %>%
  filter(scenario%in%shape.sdps) %>%
  left_join(population.iso %>% filter(model==vis.model) %>% select(scenario,iso,year,pop_mil)) %>%
  mutate(
    dls.co2.emissions.threshold = dls.co2.emissions.threshold/pop_mil/1e6*giga,
    dls.ghg.emissions.threshold = dls.ghg.emissions.threshold/pop_mil/1e6*giga
  ) %>%
  reframe(
    # summarise across model-scenario uncertainty
    # - CO2
    dls.co2.emissions.threshold.mean = mean(dls.co2.emissions.threshold),
    dls.co2.emissions.threshold.min = min(dls.co2.emissions.threshold),
    dls.co2.emissions.threshold.max = max(dls.co2.emissions.threshold),
    # - GHGs
    dls.ghg.emissions.threshold.mean = mean(dls.ghg.emissions.threshold),
    dls.ghg.emissions.threshold.min = min(dls.ghg.emissions.threshold),
    dls.ghg.emissions.threshold.max = max(dls.ghg.emissions.threshold),
    .by = c("iso", "year")
  ) %>%
  filter(year%in%c(2050)) %>%
  select(iso,year,
         dls.co2.emissions.threshold.mean,
         dls.co2.emissions.threshold.min,
         dls.co2.emissions.threshold.max,
         dls.ghg.emissions.threshold.mean,
         dls.ghg.emissions.threshold.min,
         dls.ghg.emissions.threshold.max) %>%
  mutate(unit = "tCO2/cap/yr and tCO2eq/cap/yr") %>%
  mutate(note.ranges = "Ranges span the uncertainty in 2050 across two models (REMIND and IMAGE) and three scenarios (three 1.5C-compatible sustainable development pathways)") %>%
  mutate(note.co2 = "Calculated based on (`Emissions|CO2` - `Emissions|CO2|LULUCF Direct+Indirect`)") %>%
  mutate(note.ghg = "Calculated based on `Emissions|Kyoto Gases (incl. indirect AFOLU)`")

dls.emissions.thresholds.current.table.2050 <- dls.emissions.thresholds.current.2050 %>%
  mutate(`CO2 (tCO2/cap/yr)` = paste0(
    as.character(round(dls.co2.emissions.threshold.mean, digits = 1)), " (",
    as.character(round(dls.co2.emissions.threshold.min, digits = 1)), "-",
    as.character(round(dls.co2.emissions.threshold.max, digits = 1)), ")"
  )) %>%
  mutate(`GHGs (tCO2eq/cap/yr)` = paste0(
    as.character(round(dls.ghg.emissions.threshold.mean, digits = 1)), " (",
    as.character(round(dls.ghg.emissions.threshold.min, digits = 1)), "-",
    as.character(round(dls.ghg.emissions.threshold.max, digits = 1)), ")"
  )) %>%
  select(iso,year,`CO2 (tCO2/cap/yr)`,`GHGs (tCO2eq/cap/yr)`)

dls.emissions.thresholds.current.table.2050

write_delim(
  dls.emissions.thresholds.current.table.2050,
  file = here("analyses", "figures", data.version, figures.version,
              paste0("st-DLS-emissions-thresholds-desire-percapita_2050.csv") ),
  delim = ","
)

### SI.50-55 SSP1 figures ------------------------------------------------------
shape.color.coding.with.ssp1 <- c("SDP_EI-1p5C" = "midnightblue",
                        "SDP_MC-1p5C" = "aquamarine4",
                        "SDP_RC-1p5C" = "goldenrod3",
                        "SDP-EI" = "midnightblue",
                        "SDP-MC" = "aquamarine4",
                        "SDP-RC" = "goldenrod3",
                        "SSP2-NPi" = "black",
                        "SSP2-1p5C" = "grey",
                        "SSP2-Ref" = "black",
                        "SSP2-1.5C" = "grey",
                        "SSP1-NPi" = "purple",
                        "SSP1-1p5C" = "pink",
                        "SSP1-Ref" = "purple",
                        "SSP1-1.5C" = "pink"
                        )
##### SI.50. ResCom headcounts SSP1 ----------------------------------------------
df.core.with.ssp1 <- df.default %>%
  filter(scenario%in%
           c(shape.core.scenarios, "SSP1-1p5C", "SSP1-NPi")
           )

p.dle.headcount.data.with.ssp1 <- df.core.with.ssp1 %>%
  left_join(wb.income.grouping.3) %>%
  left_join(population.iso %>% select(-variable,-unit)) %>%
  # from_wbr4_to_wbr3() %>%
  reframe(
    dle.headcount = sum(share.below.projected.adjusted*pop_mil),
    dle.headcount.constant = sum(share.below.projected.curtech*pop_mil),

    dle.headcount.p5 = sum(pmax(share.below.projected.adjusted-0.05,0)*pop_mil),
    dle.headcount.constant.p5 = sum(pmax(share.below.projected.curtech-0.05,0)*pop_mil),

    dle.headcount.p10 = sum(pmax(share.below.projected.adjusted-0.10,0)*pop_mil),
    dle.headcount.constant.p10 = sum(pmax(share.below.projected.curtech-0.10,0)*pop_mil),

    dle.headcount.p20 = sum(pmax(share.below.projected.adjusted-0.20,0)*pop_mil),
    dle.headcount.constant.p20 = sum(pmax(share.below.projected.curtech-0.20,0)*pop_mil),

    # also: energy gap as share of energy threshold
    # dle.energy.gap.share = weighted.mean(x=(depth.below.projected.adjusted*share.below.projected.adjusted)/dle.threshold.adjusted, w=pop_mil),

    .by = c("model", "scenario", "wb.r3", "variable", "year")
    # .by = c("model", "scenario", "wb.r3", "variable", "year")
  ) %>%
  mutate(dle.headcount.change = dle.headcount / dle.headcount.constant)

p.dle.headcount.futures.with.ssp1 <- ggplot(
  data = p.dle.headcount.data.with.ssp1 %>% ms_add() %>%
    reframe(dle.headcount = sum(dle.headcount),
            .by = c("model", "scenario", "variable", "year")) %>%
    iamc_variable_keep_one_level(level = -1) %>%
    filter(year>=2020,
           year<=DLE.END.YEAR.TIMESERIES,
           variable=="Residential and Commercial") %>%
    rename.models.and.scenarios %>%
    rename(Model=model),
  mapping = aes(x=year)
) +
  # facet_grid(.~scenario) +

  geom_ribbon(
    data = . %>%
      filter(grepl(scenario,pattern="SDP-",fixed=T)) %>%
      reframe(min.dle.headcount = min(dle.headcount), max.dle.headcount = max(dle.headcount), .by=c("variable", "year")),
    fill = "dodgerblue",
    aes(ymin=min.dle.headcount,ymax=max.dle.headcount),
    alpha=0.2
  ) +
  geom_ribbon(
    data = . %>%
      filter(grepl(scenario,pattern="SSP2",fixed=T)) %>%
      reframe(min.dle.headcount = min(dle.headcount), max.dle.headcount = max(dle.headcount), .by=c("variable", "year")),
    fill = "grey",
    aes(ymin=min.dle.headcount,ymax=max.dle.headcount),
    alpha=0.2
  ) +
  geom_ribbon(
    data = . %>%
      filter(grepl(scenario,pattern="SSP1",fixed=T)) %>%
      reframe(min.dle.headcount = min(dle.headcount), max.dle.headcount = max(dle.headcount), .by=c("variable", "year")),
    fill = "pink",
    aes(ymin=min.dle.headcount,ymax=max.dle.headcount),
    alpha=0.2
  ) +

  geom_line(aes(y=dle.headcount,
                color=scenario,
                linetype = Model)) +
  labs(subtitle = bquote("Population below minimum energy requirement (DLE) threshold over time:" ~ bold("Residential and Commercial sector")),
       caption = "Grey range: SSP2\nPink range: SSP1\nBlue range: SDPs") +
  scale_ysidex_continuous(minor_breaks = NULL, breaks = NULL) +
  scale_xsidey_continuous(minor_breaks = NULL, breaks = NULL) +

  scale_x_continuous(name = NULL,
                     expand = c(0,0)) +
  scale_y_continuous(name = "Million",
                     expand = c(0,0),
                     limits = c(0,6000)) +

  scale_color_manual("Scenario", values=shape.color.coding.with.ssp1) +

  theme_classic() +
  theme_hc() +
  theme(ggside.panel.scale = .3,
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y.right = element_text(angle = 0))

p.dle.headcount.futures.with.ssp1

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, "si-ssp1-rescom-headcount"),
  p = p.dle.headcount.futures.with.ssp1,
  w = 250,
  h = 200
)

##### SI.51. Energy Gaps SSP1 vs SSP2 VS SDPs: ResCom ----------------------------

###### data ----------------------------------------------------------------------
p.EDG.data.with.ssp1 <- df.core.with.ssp1 %>%
  left_join(ipcc.r10) %>%
  left_join(population.iso %>% select(-variable,-unit)) %>%
  reframe(

    energy.development.gap = sum(share.below.projected.adjusted*depth.below.projected.adjusted*pop_mil)*giga*mega/exa,
    total.energy.use = sum(energy.per.capita.uncorrected*pop_mil)*giga*mega/exa,
    dle.threshold = sum(dle.threshold.adjusted*pop_mil)*giga*mega/exa,

    .by = c("model", "scenario", "variable", "r10", "year")
  )

# summarise regional data, take across models (R10)
p.EDG.summedsectors.minmaxavg.data.with.ssp1 <- p.EDG.data.with.ssp1 %>%

  # sum over sectors
  reframe(
    energy.development.gap = sum(energy.development.gap),
    total.energy.use = sum(total.energy.use),
    dle.threshold = sum(dle.threshold),

    .by = c("model","scenario","r10","year")
  ) %>%
  # min.max.avg across models
  reframe(
    energy.development.gap.min = min(energy.development.gap),
    energy.development.gap.max = max(energy.development.gap),
    energy.development.gap.avg = mean(energy.development.gap),

    total.energy.use.min = min(total.energy.use),
    total.energy.use.max = max(total.energy.use),
    total.energy.use.avg = mean(total.energy.use),

    dle.threshold.min = min(dle.threshold),
    dle.threshold.max = max(dle.threshold),
    dle.threshold.avg = mean(dle.threshold),

    energy.gap.ratio.min = min(energy.development.gap/total.energy.use),
    energy.gap.ratio.max = max(energy.development.gap/total.energy.use),
    energy.gap.ratio.avg = mean(energy.development.gap/total.energy.use),

    dle.energy.min = min(dle.threshold-energy.development.gap),
    dle.energy.max = max(dle.threshold-energy.development.gap),
    dle.energy.avg = mean(dle.threshold-energy.development.gap),

    abovedle.minus.energygap.min = min(total.energy.use - dle.threshold),
    abovedle.minus.energygap.max = max(total.energy.use - dle.threshold),
    abovedle.minus.energygap.avg = mean(total.energy.use - dle.threshold),

    .by = c("scenario","r10","year")
  ) %>%
  ungroup()

###### plot ----------------------------------------------------------------------
plot_EDG_sector_bothmodels <- function(df, var = "Residential and Commercial"){
  return(
    ggplot(
      data = df %>%
        filter(year %in% c(2020,2030,2040),
               variable==paste0("Final Energy|",var)
               # scenario %in% c("SDP_RC-1p5C")
        ) %>%
        enterise_long_r10_names() %>% rename.models.and.scenarios() %>%
        mutate(`Scenario set` = ifelse(grepl(scenario, pattern="SDP", fixed=T), "SDPs",
                                       ifelse(grepl(scenario, pattern="SSP1", fixed=T), "SSP1",
                                              ifelse(grepl(scenario, pattern="SSP2", fixed=T), "SSP2",
                                                     NA)))),
      mapping = aes(x = year, y = energy.development.gap, colour = scenario)
    ) +
      geom_hline(yintercept = 0) +
      geom_point(size = 8, shape = 95, alpha=0.7) +
      geom_point(size = 3, aes(shape = `Scenario set`), alpha=0.7) +

      facet_grid(model~r10) +

      scale_y_continuous(name = "EJ/year") +
      scale_x_continuous(name = NULL, breaks = c(2020,2030,2040)) +
      # xlab(NULL) +

      labs(subtitle = bquote("Closing of the total"~bold("energy needs gap")~"for"~bold(.(var)))) +

      theme_classic() +
      theme_hc() +

      scale_color_manual("Scenario", values=shape.color.coding.with.ssp1) +

      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text.y.right = element_text(angle = 0),
            panel.spacing.x = unit(1, "lines"),
            panel.spacing.y = unit(1, "lines"))
  )
}

p.EDG.sectors.models.with.SSP1.rescom <- plot_EDG_sector_bothmodels(p.EDG.data.with.ssp1)
p.EDG.sectors.models.with.SSP1.rescom

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, "si-ssp1-rescom-ENG"),
  p = p.EDG.sectors.models.with.SSP1.rescom,
  w = 350,
  h = 150
)


##### SI.52. Energy Gaps SSP1 vs SSP2 VS SDPs: Transport -------------------------
p.EDG.sectors.models.with.SSP1.transport <- plot_EDG_sector_bothmodels(p.EDG.data.with.ssp1,
                                                                    var = "Transportation")
p.EDG.sectors.models.with.SSP1.transport

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, "si-ssp1-transport-ENG"),
  p = p.EDG.sectors.models.with.SSP1.transport,
  w = 350,
  h = 150
)

##### SI.53. Energy Gaps SSP1 vs SSP2 VS SDPs: Transport -------------------------
p.EDG.sectors.models.with.SSP1.industry <- plot_EDG_sector_bothmodels(p.EDG.data.with.ssp1,
                                                                       var = "Industry")
p.EDG.sectors.models.with.SSP1.industry

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, "si-ssp1-industry-ENG"),
  p = p.EDG.sectors.models.with.SSP1.industry,
  w = 350,
  h = 150
)

##### SI.54. Energy headroom SSP1 vs SSP2 VS SDPs: total -------------------------
p.above.minus.suff.with.ssp1 <- ggplot(
  data = p.EDG.summedsectors.minmaxavg.data.with.ssp1 %>%
    # iamc_variable_keep_one_level(level = -1) %>%
    filter(year %in% c(2020,2025,2030,2035,2040)) %>%
    # filter(year!=2055) %>%
    left_join(pop.r10.projection) %>%
    enterise_long_r10_names() %>% rename.models.and.scenarios %>%
    mutate(`Scenario set` = ifelse(grepl(scenario, pattern="SDP", fixed=T), "SDPs",
                                   ifelse(grepl(scenario, pattern="SSP1", fixed=T), "SSP1",
                                          ifelse(grepl(scenario, pattern="SSP2", fixed=T), "SSP2",
                                                 NA)))),
  mapping = aes(x=year)
) +
  facet_grid(.~r10, scales = "free_y") +

  geom_line(aes(y=abovedle.minus.energygap.avg/pop_mil*exa/giga/mega,
                color = scenario,
                linetype = `Scenario set`),
            linewidth = 2) +

  scale_color_manual("Scenario", values=shape.color.coding.with.ssp1) +
  scale_fill_manual("Scenario", values=shape.color.coding.with.ssp1) +

  scale_y_continuous(name = "GJ/cap/yr") +
  xlab(NULL) +

  labs(subtitle = bquote(bold("Energy headroom")~"per capita = average energy use per capita - DLE threshold")
       # caption = "Lines are averages between IMAGE and REMIND, ribbons are their range."
  ) +

  theme_classic() +
  theme_hc() +
  theme(#ggside.panel.scale = .3,
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y.right = element_text(angle = 0))

p.above.minus.suff.with.ssp1

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, "si-ssp1-headroom"),
  p = p.above.minus.suff.with.ssp1,
  w = 350,
  h = 150
)


##### SI.55. Income gini values SSP1 vs SSP2 VS SDPs: 2020 to 2040 ---------------
gini.growth <- df.core.with.ssp1 %>% filter(model==vis.model) %>%
  filter(year%in%c(2020,2040)) %>%
  select(scenario,iso,year,gini) %>%
  distinct() %>%  # 3 times smaller (because we drop energy information; 3 sectors)
  pivot_wider(names_from = year, values_from = gini) %>%
  mutate(gini.point.change = `2040`-`2020`) %>%
  select(scenario,iso,gini.point.change)

p.gini.change.2040.with.ssp1 <- ggplot(gini.growth %>% rename.scenarios() %>%
                                         bind_rows(
                                           gini.growth.historical %>%
                                             mutate(scenario = "History\n(1994-2002 to 2014-2022)") %>%
                                             rename(gini.point.change = gini.point.change.historical)
                                         ),
                                       aes(x=gini.point.change, y=scenario)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_density_ridges(aes(fill=scenario, height = ..density..), stat = "density", trim = TRUE) +
  scale_fill_manual("Scenario", values=shape.color.coding.with.ssp1) +
  scale_colour_manual("Scenario", values=shape.color.coding.with.ssp1) +
  scale_y_discrete(name = NULL) +
  xlab(NULL) +

  labs(subtitle = bquote(bold("Income Gini coefficient")~"change from 2020 to 2040"),
       caption = "Distributions across countries, not population weighted."
  ) +

  theme_classic() +
  theme_hc()

p.gini.change.2040.with.ssp1

save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, "si-ssp1-gini"),
  p = p.gini.change.2040.with.ssp1,
  w = 175,
  h = 150
)

### ST.12. Values of this study in table for comparing to 2021 study -----------

# this study
ei <- vroom(
  file.path(
    PATH.data.desire, "other", "energy-intensities",
    "DLE_threshold_input_opcon_elecnonelec_dimensions.csv"
  )
)

# construction energy
con.ei <- ei %>%
  filter(elec=="total") %>%
  reframe(con.min = min(thres.energy.conrep),
          con.max = max(thres.energy.conrep),
          .by = c("variable","unit.energy"))
# operation energy
op.ei <- ei %>%
  filter(elec=="total") %>%
  reframe(op.min = min(thres.energy.op),
          op.max = max(thres.energy.op),
          .by = c("variable","unit.energy"))

write_delim(
  left_join(con.ei,op.ei),
  file = here("analyses", "figures", data.version, figures.version,
              paste0("st-EI-op-con.csv") ),
  delim = ","
)

# 2021 study
ei.2021 <- vroom(
  file.path(
    PATH.data.for.figures, "kikstra_erl2021",
    "SSP2_2040_lctTRUE_R11.csv"
  )
) %>% filter(elec=="total", year==2050) %>%
  select(region,dimension,grp,type,DLE.pcap,unit.DLE.pcap) %>%
  mutate(DLE.pcap = DLE.pcap * 1e3,unit.DLE.pcap="MJ/cap/year") %>%
  pivot_wider(names_from = type, values_from = DLE.pcap)
ei.2021 <- ei.2021 %>% filter(dimension!="transport") %>%
  bind_rows(
    ei.2021 %>% filter(dimension=="transport") %>% mutate(grp="total") %>%
      reframe(
        CON.new = sum(CON.new),
        CON.rep = sum(CON.rep),
        OP = sum(OP),
        .by = c("region", "dimension", "grp", "unit.DLE.pcap")
      )
  )

# construction energy
con.ei.2021 <- ei.2021 %>%
  reframe(con.min = min(CON.new + CON.rep),
          con.max = max(CON.new + CON.rep),
          .by = c("dimension","grp","unit.DLE.pcap"))
# operation energy
op.ei.2021 <- ei.2021 %>%
  reframe(op.min = min(OP),
          op.max = max(OP),
          .by = c("dimension","grp","unit.DLE.pcap"))

write_delim(
  left_join(con.ei.2021,op.ei.2021),
  file = here("analyses", "figures", data.version, figures.version,
              paste0("st-EI-op-con-2021.csv") ),
  delim = ","
)

### SI.56. Contributions of levers -----------------------------------------------
##### Auxiliary functions ---------------------------------------------------------
calculate_deprivation_headcount_rate <- function(df){
  df <- df %>%
    mutate(
      share.below = GetDepthofDeficit_lognormal(nu=log(energy.per.capita) - (GetInverseCDF_lognormal(energy.gini)^2) / 2,
                                                sigma=GetInverseCDF_lognormal(energy.gini),
                                                thres=dle.threshold.adjusted,
                                                "share")
    )
  return(df)
}

##### Data preparation -----------------------------------------------------------
df.whatworks <- df.core %>% filter(
  year %in% c(2020, 2040),
  variable %in% c("Final Energy|Residential and Commercial"),
  scenario %in% shape.core.scenarios
) %>%
  select(model, scenario, year, iso, variable, unit, pop_mil,
         share.below.projected.adjusted,
         energy.per.capita, # clean residential energy
         dle.threshold.adjusted, # dle threshold changes
         energy.gini # inequality
  )

df.whatworks.2020 <- df.whatworks %>% filter(year==2020) %>% select(-year)
df.whatworks.2040.all.levers <- df.whatworks %>% filter(year==2040) %>% select(-year)

df.whatworks.2040.no.withincountryequalitypush <- df.whatworks.2040.all.levers %>% select(-share.below.projected.adjusted) %>%
  select(-energy.gini) %>%
  left_join(df.whatworks.2020 %>% select(model, scenario, iso, variable,
                                         energy.gini),
            by = join_by(model, scenario, iso, variable)) %>%
  calculate_deprivation_headcount_rate()
df.whatworks.2040.no.energylevelchanges <- df.whatworks.2040.all.levers %>% select(-share.below.projected.adjusted) %>%
  select(-energy.per.capita) %>%
  left_join(df.whatworks.2020 %>% select(model, scenario, iso, variable,
                                         energy.per.capita),
            by = join_by(model, scenario, iso, variable)) %>%
  calculate_deprivation_headcount_rate()
df.whatworks.2040.no.efficiencychanges <- df.whatworks.2040.all.levers %>% select(-share.below.projected.adjusted) %>%
  select(-dle.threshold.adjusted) %>%
  left_join(df.whatworks.2020 %>% select(model, scenario, iso, variable,
                                         dle.threshold.adjusted),
            by = join_by(model, scenario, iso, variable)) %>%
  calculate_deprivation_headcount_rate()

# combined.data
df.levers.combined <-
  # sdps
  df.whatworks.2040.all.levers %>% mutate(Version = "All levers in 2040\n") %>%
  rename(share.below=share.below.projected.adjusted) %>%
  # variants
  bind_rows(df.whatworks.2040.no.withincountryequalitypush %>% mutate(Version = "2020 inequality\n(2040 efficiency and energy growth)")) %>%
  bind_rows(df.whatworks.2040.no.energylevelchanges %>% mutate(Version = "2020 energy levels\n(2040 efficiency and energy inequality)")) %>%
  bind_rows(df.whatworks.2040.no.efficiencychanges %>% mutate(Version = "2020 service provisioning systems\n(2040 energy inequality and energy growth)")) %>%
  # 2020
  bind_rows(
    df.whatworks.2020 %>% mutate(Version = "The world in 2020\n") %>%
      rename(share.below=share.below.projected.adjusted)
  ) %>%
  summarise(
    # calculate global headcount rates
    `Energy deprivation headcount` = sum(pop_mil * share.below),
    Population = sum(pop_mil),
    .by = c("model", "scenario", "variable", "Version")
  ) %>%
  mutate(`Share of population not meeting energy needs (%)` = `Energy deprivation headcount` / Population)



##### Visualise bars -------------------------------------------------------------
visualise_deprivation_bars <- function(df,vis.model,version){
  ggplot(
    data = df %>%
      filter(
        scenario%in%shape.core.scenarios[shape.core.scenarios!="SSP2-NPi"],
        model==vis.model,
        Version==version,
      ) %>%
      rename.models.and.scenarios(),
    aes(x = "0", y = 1-`Share of population not meeting energy needs (%)`)
  ) +
    geom_col(
      position = position_dodge(),
      mapping = aes(fill = scenario)
    ) +

    # add 2020
    geom_col(
      data = df %>%
        filter(
          scenario%in%shape.core.scenarios[shape.core.scenarios!="SSP2-NPi"],
          model==vis.model,
          Version=="The world in 2020\n",
        ) %>%
        rename.models.and.scenarios(),
      colour = "white", linetype = "dashed", alpha = 0,
      position = position_dodge()
    ) +

    # add all levers 2040 (if not showing only 2020)
    geom_col(
      data = df %>%
        filter(
          scenario%in%shape.core.scenarios[shape.core.scenarios!="SSP2-NPi"],
          model==vis.model,
          Version=="All levers in 2040\n",
        ) %>%
        rename.models.and.scenarios(),
      mapping = aes(colour = scenario),
      linetype = "solid", alpha = 0,
      position = position_dodge()
    ) +

    xlab(NULL) +
    ylab("Share of population\nmeeting energy needs (%)") +
    theme_classic() +
    theme_hc() +
    # labs(caption = "Using only REMIND for visualisation.\nAll scenarios return back below 1.5\u00B0C with limited overshoot",
    #      title = "Population meeting residential energy needs (global)",
    #      subtitle = version) +
    labs(subtitle = version) +
    scale_fill_manual(values = shape.color.coding) +
    scale_colour_manual(values = shape.color.coding) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0,1),
                       expand = c(0,0)) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.title=element_blank()
    ) %>%
    return()
}


p.levers.REMIND <- (
  visualise_deprivation_bars(df = df.levers.combined,
                             vis.model = "REMIND-MAgPIE 3.2-4.6",
                             v="The world in 2020\n") + theme(legend.position = "none")
) + (
  visualise_deprivation_bars(df = df.levers.combined,
                             vis.model = "REMIND-MAgPIE 3.2-4.6",
                             v="All levers in 2040\n") + theme(legend.position = "none")
) + (
  visualise_deprivation_bars(df = df.levers.combined,
                             vis.model = "REMIND-MAgPIE 3.2-4.6",
                             v="2020 energy levels\n(2040 efficiency and energy inequality)") + theme(legend.position = "none")
) + (
  visualise_deprivation_bars(df = df.levers.combined,
                             vis.model = "REMIND-MAgPIE 3.2-4.6",
                             v="2020 inequality\n(2040 efficiency and energy growth)") + theme(legend.position = "none")
) + (
  visualise_deprivation_bars(df = df.levers.combined,
                             vis.model = "REMIND-MAgPIE 3.2-4.6",
                             v="2020 service provisioning systems\n(2040 energy inequality and energy growth)") + theme(legend.position = "none")
# ) + plot_annotation(subtitle = "REMIND-MAgPIE 3.2-4.6")
) + plot_layout(design =
  "AAABBB
  CCDDEE"
) + plot_annotation(tag_levels = "A",
                    title = "Population meeting residential energy needs (global), with hypothetical variants where one lever is not pulled.",
                    subtitle = "REMIND-MAgPIE 3.2-4.6"
                    )
# p.levers.REMIND
p.levers.IMAGE <- (
  visualise_deprivation_bars(df = df.levers.combined,
                             vis.model = "IMAGE 3.3",
                             v="The world in 2020\n") + theme(legend.position = "none")
) + (
  visualise_deprivation_bars(df = df.levers.combined,
                             vis.model = "IMAGE 3.3",
                             v="All levers in 2040\n") + theme(legend.position = "none")
) + (
  visualise_deprivation_bars(df = df.levers.combined,
                             vis.model = "IMAGE 3.3",
                             v="2020 energy levels\n(2040 efficiency and energy inequality)") + theme(legend.position = "none")
) + (
  visualise_deprivation_bars(df = df.levers.combined,
                             vis.model = "IMAGE 3.3",
                             v="2020 inequality\n(2040 efficiency and energy growth)")
) + (
  visualise_deprivation_bars(df = df.levers.combined,
                             vis.model = "IMAGE 3.3",
                             v="2020 service provisioning systems\n(2040 energy inequality and energy growth)") + theme(legend.position = "none")
# ) + plot_annotation(subtitle = "IMAGE 3.3")
) + plot_layout(design =
                  "AAABBB
  CCDDEE"
) + plot_annotation(tag_levels = "A",
                    caption = "All scenarios return back below 1.5\u00B0C with limited overshoot",
                    subtitle = "IMAGE.3.3"
)
# p.levers.IMAGE

p.levers <- ((p.levers.REMIND) / (p.levers.IMAGE) ) +
  plot_layout(design =
  "A
  B"
  ) +
  plot_annotation(tag_levels = "A",
                  caption = "All scenarios return back below 1.5\u00B0C with limited overshoot",
                  title = "Population meeting residential and commercial energy needs (global), with hypothetical variants where one lever is not pulled.")
# p.levers


save_ggplot(
  f = here("analyses", "figures", data.version, figures.version, "si-levers_rescom_global"),
  p = p.levers,
  w = 300,
  h = 400
)


# NUMBERS in SUPPLMENTARY INFORMATION ------------------------------------------

## Section 1.3.2. --------------------------------------------------------------
data.folder.sens.90p <- file.path(
  PATH.data.desire, "sensitivity", "90p_gini_sensitivity"
)

df.sens.90p <- read_csv(
  file.path(data.folder.sens.90p, "90p_shape_submission_results_default_version_1_1_0_rc2_20241114.csv")
) %>%
  # remove unnecessary columns
  select(-gini.to.gini.elasticities.by.country.up, -gini.to.gini.elasticities.by.country.down)

# only keep countries available for both models (173/171 countries, writing 26.01.2024, version0_1_rc3_20240126)
countries.both.models <- df.sens.90p %>% select(model,iso) %>% distinct() %>% mutate(available=T) %>%
  pivot_wider(names_from = model, values_from = available) %>%
  filter((`IMAGE 3.3`==T & `REMIND-MAgPIE 3.2-4.6`==T)) %>%
  select(iso) %>% pull()
df.sens.90p <- df.sens.90p %>% filter(iso %in% countries.both.models)

df.core.90p <- df.sens.90p %>%
  filter(scenario%in%shape.core.scenarios)

### ResCom headcount -----------------------------------------------------------
#### Main text - default setting -----------------------------------------------
# ... see section 3.5; "Global headcounts in SDPs"

#### Sensitivity - 90% effect ---------------------------------------------------

p.dle.headcount.data.90p <- df.core.90p %>%
  left_join(wb.income.grouping.3) %>%
  left_join(population.iso %>% select(-variable,-unit)) %>%
  # from_wbr4_to_wbr3() %>%
  reframe(
    dle.headcount = sum(share.below.projected.adjusted*pop_mil),
    dle.headcount.constant = sum(share.below.projected.curtech*pop_mil),

    dle.headcount.p5 = sum(pmax(share.below.projected.adjusted-0.05,0)*pop_mil),
    dle.headcount.constant.p5 = sum(pmax(share.below.projected.curtech-0.05,0)*pop_mil),

    dle.headcount.p10 = sum(pmax(share.below.projected.adjusted-0.10,0)*pop_mil),
    dle.headcount.constant.p10 = sum(pmax(share.below.projected.curtech-0.10,0)*pop_mil),

    dle.headcount.p20 = sum(pmax(share.below.projected.adjusted-0.20,0)*pop_mil),
    dle.headcount.constant.p20 = sum(pmax(share.below.projected.curtech-0.20,0)*pop_mil),

    # also: energy gap as share of energy threshold
    # dle.energy.gap.share = weighted.mean(x=(depth.below.projected.adjusted*share.below.projected.adjusted)/dle.threshold.adjusted, w=pop_mil),

    .by = c("model", "scenario", "wb.r3", "variable", "year")
    # .by = c("model", "scenario", "wb.r3", "variable", "year")
  ) %>%
  mutate(dle.headcount.change = dle.headcount / dle.headcount.constant)

p.dle.headcount.data.90p %>% ms_add() %>%
  reframe(dle.headcount = sum(dle.headcount),
          .by = c("model", "scenario", "variable", "year")) %>%
  iamc_variable_keep_one_level(level = -1) %>%
  filter(year%in%c(2020,2040),
         variable=="Residential and Commercial") %>%
  pivot_wider(names_from = year, values_from = dle.headcount) %>%
  # range across models and scenarios
  mutate(sdp=ifelse(scenario%in%shape.sdps,"Yes",scenario)) %>%
  reframe(
    range.2020 = paste0(round(min(`2020`)), "-", round(max(`2020`))),
    range.2040 = paste0(round(min(`2040`)), "-", round(max(`2040`))),
    .by = c("sdp")
  )


### Energy gap -----------------------------------------------------------------
#### Main text - default setting -----------------------------------------------
# ... see Table 3; `table.regional.dle.characteristics`

#### Sensitivity - 90% effect --------------------------------------------------
df.headroom.table.data.90p <- df.core.90p %>%
  reframe(
    # sum across variables/sectors
    energy.development.gap = sum(share.below.projected.adjusted*depth.below.projected.adjusted),
    total.energy.use = sum(energy.per.capita.uncorrected),
    dle.threshold = sum(dle.threshold.adjusted),

    pop_mil = first(pop_mil),

    .by = c("model", "scenario", "iso", "year")
  ) %>%

  # add r10
  left_join(ipcc.r10) %>%

  reframe(
    # global mean
    energy.development.gap = weighted.mean(energy.development.gap, pop_mil),
    total.energy.use = weighted.mean(total.energy.use, pop_mil),
    dle.threshold = weighted.mean(dle.threshold, pop_mil),

    .by = c("model", "scenario", "r10", "year")
  ) %>%

  reframe(
    # simple mean across models
    energy.development.gap = mean(energy.development.gap),
    total.energy.use = mean(total.energy.use),
    dle.threshold = mean(dle.threshold),

    .by = c("scenario", "r10", "year")
  )
df.headroom.table.data.global.90p <- df.headroom.table.data.90p %>%
  left_join(pop.r10.projection) %>%
  reframe(
    energy.development.gap = weighted.mean(energy.development.gap, pop_mil),
    total.energy.use = weighted.mean(total.energy.use, pop_mil),
    dle.threshold = weighted.mean(dle.threshold, pop_mil),
    .by = c("scenario", "year")
  ) %>%
  mutate(r10="World")
df.headroom.table.data.90p <- df.headroom.table.data.90p %>%
  bind_rows(df.headroom.table.data.global.90p)

table.headroom.2020.90p <- df.headroom.table.data.90p %>%
  filter(year==2020, scenario==vis.scenario) %>%
  mutate(
    total.energy.use = round(total.energy.use),
    dle.threshold.as.share.of.total = round(dle.threshold/total.energy.use*100),
    gap = round(energy.development.gap),
    gap.as.share.of.total.energy = round(energy.development.gap/total.energy.use*100),
    non.dls.energy = round(total.energy.use - (dle.threshold - energy.development.gap))
  )

table.headroom.2040.90p <- df.headroom.table.data.90p %>%
  filter(year==2040) %>%
  mutate(
    dle.threshold.as.share.of.total = dle.threshold/total.energy.use,
    gap = energy.development.gap,
    gap.as.share.of.total.energy = energy.development.gap/total.energy.use,
    non.dls.energy = total.energy.use - (dle.threshold - energy.development.gap)
  ) %>%
  # group scenarios
  mutate(scenario=ifelse(scenario%in%shape.sdps,"SDPs",scenario)) %>%
  reframe(
    dle.threshold.as.share.of.total = ifelse(round(min(dle.threshold.as.share.of.total*100))==round(max(dle.threshold.as.share.of.total*100)),
                                             as.character(round(max(dle.threshold.as.share.of.total*100))),
                                             paste0(round(min(dle.threshold.as.share.of.total*100)),
                                                    " to ",
                                                    round(max(dle.threshold.as.share.of.total*100)))),
    gap = ifelse(round(min(gap))==round(max(gap)),
                 as.character(round(max(gap))),
                 paste0(round(min(gap)),
                        " to ",
                        round(max(gap)))),
    gap.as.share.of.total.energy = ifelse(round(min(gap.as.share.of.total.energy*100))==round(max(gap.as.share.of.total.energy*100)),
                                          as.character(round(max(gap.as.share.of.total.energy*100))),
                                          paste0(round(min(gap.as.share.of.total.energy*100)),
                                                 " to ",
                                                 round(max(gap.as.share.of.total.energy*100)))),
    range.2040 = ifelse(round(min(non.dls.energy))==round(max(non.dls.energy)),
                        as.character(round(max(non.dls.energy))),
                        paste0(round(min(non.dls.energy)),
                               " to ",
                               round(max(non.dls.energy)))),
    .by = c("scenario", "r10", "year")
  )

table.regional.dle.characteristics.90p <-
  table.headroom.2020.90p %>% select(scenario,r10,year,total.energy.use,dle.threshold.as.share.of.total,gap) %>% arrange(r10) %>%
  pivot_wider(names_from = year, values_from = c(total.energy.use,dle.threshold.as.share.of.total,gap)) %>%
  left_join(
    table.headroom.2040.90p %>% select(scenario,r10,year,dle.threshold.as.share.of.total,gap) %>% arrange(r10) %>%
      pivot_wider(names_from = scenario, values_from = c(dle.threshold.as.share.of.total,gap))
  ) %>%
  select(
    r10,
    total.energy.use_2020,
    dle.threshold.as.share.of.total_2020, gap_2020,
    dle.threshold.as.share.of.total_SDPs, gap_SDPs,
    `dle.threshold.as.share.of.total_SSP2-1p5C`, `gap_SSP2-1p5C`,
    `dle.threshold.as.share.of.total_SSP2-NPi`, `gap_SSP2-NPi`
  )

table.regional.dle.characteristics.90p







## Section 1.4.3. --------------------------------------------------------------

### Commercial energy included in DLE ------------------------------------------
# - health care, by country informed by EXIOBASE (for 1995-2015)

# IMAGE:
image2010to2015.downscaled.rescom <- load_excel_iamc(
  file.path(PATH.data.for.figures, "iam-data", "downscaled",
            "IMAGE 3.3_SHAPE_2023_2024_02_12_2018_harmo_step5e_Scenario_Explorer_upload_FINAL.xlsx")
) %>% filter(VARIABLE %in% c("Final Energy|Residential and Commercial",
                             "Final Energy|Residential",
                             "Final Energy|Residential|Electricity",
                             "Final Energy|Residential|Gases",
                             "Final Energy|Residential|Heat",
                             "Final Energy|Residential|Liquids",
                             "Final Energy|Residential|Solids"
),
SCENARIO=="SSP2-NPi") %>% rename(
  model = MODEL, scenario = SCENARIO, region = REGION, variable = VARIABLE, unit = UNIT
) %>%
  iamc_wide_to_long(upper.to.lower = F) %>% filter(year<=2015) %>%
  rename(iso=region)
image2010to2015.downscaled.rescom <- image2010to2015.downscaled.rescom %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  mutate(image.energy.commercial=ifelse(is.na(`Final Energy|Residential`),
                                        `Final Energy|Residential and Commercial`-(
                                          `Final Energy|Residential|Electricity` +
                                            `Final Energy|Residential|Gases` +
                                            `Final Energy|Residential|Heat` +
                                            `Final Energy|Residential|Liquids` +
                                            `Final Energy|Residential|Solids`
                                        ),
                                        `Final Energy|Residential and Commercial`-`Final Energy|Residential`))

image2010to2015.downscaled.total <- load_excel_iamc(
  file.path(PATH.data.for.figures, "iam-data", "downscaled",
            "IMAGE 3.3_SHAPE_2023_2024_02_12_2018_harmo_step5e_Scenario_Explorer_upload_FINAL.xlsx")
) %>% filter(VARIABLE %in% c("Final Energy"),
             SCENARIO=="SSP2-NPi") %>% rename(
               model = MODEL, scenario = SCENARIO, region = REGION, variable = VARIABLE, unit = UNIT
             ) %>%
  iamc_wide_to_long(upper.to.lower = F) %>% filter(year<=2015) %>%
  rename(iso=region)

# Andrieu2023
health.exio2005to2015 <- read_excel(file.path(PATH.data.for.figures, "commercial_split", "Andrieu2023_healthcare_energyfootprint_EXIOBASEv3_8_2.xlsx"),
                                    sheet = "satellite-manually-restructured") %>%
  filter(Satellite == "Energy Carrier Use: Total") %>%
  # "Energy Carrier Use: Total","Energy Carrier Net NTRA" "Energy Carrier Net Total"
  pivot_longer(cols = c(`Austria`:`South Africa`), names_to = "country", values_to = "exio.energy.health") %>%
  # add share of direct energy
  left_join(
    read_excel(file.path(PATH.data.for.figures, "commercial_split", "Andrieu2023_healthcare_energyfootprint_EXIOBASEv3_8_2.xlsx"),
               sheet = "direct-energy-share-table-s11")
  ) %>%
  mutate_cond(country=="Czech Republic", country="Czechia") %>%
  mutate_cond(country=="Great Britain", country="United Kingdom of Great Britain and Northern Ireland") %>%
  mutate_cond(country=="South Korea", country="Korea, Rep.") %>%
  mutate_cond(country=="Russia", country="Russian Federation") %>%
  mutate_cond(country=="Taiwan", country="Taiwan, China") %>%
  mutate_cond(country=="Taiwan", country="Taiwan, China") %>%
  mutate(iso = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
  #note: not mapping RoW Asia and Pacific, RoW Europe, RoW Africa, RoW America
  left_join(ipcc.r10, by = c("iso")) %>%
  mutate(exio.energy.health = exio.energy.health * share.direct *tera/exa,
         unit = "EJ/yr")

# - Education; Lang & Kennedy, Table 1, for WIOD and EXIOBASE, for EE = Europe; OE = OECD less Europe; XX = Rest of World, Education and research and development (R&D)-driven impacts as intensities per student and percentages of total regional environmental impacts from https://onlinelibrary.wiley.com/doi/full/10.1111/jiec.12396?casa_token=B_f4yHioznAAAAAA%3AlFbPWmehYR9nfdR-qxWZeKsgAHa71b8VLsl8BjVTNTpR4XC5fP963EYIblbNyszmxU2b7njy6LdUPdd_Rw
#   * table 1 data: 1-14 GJ/cap (0.9-2.0%) for total EXIOBASE, 6-25 GJ/cap (1.5-2.7%) for WIOD.
#           * Figure 3: energy intensity by student, indexed to world average
#           * Figure 6: the share of education in energy is slowly rising, from 2% roughly to 2.2% over 15 years
#   * note: this includes higher education, and would thus be generous (could be halved)
education.exio2005to2015 <- load_official_country_grouping(grouping.to.load = "OECD") %>% mutate(OECD=ifelse(is.na(OECD),"Non-OECD",OECD)) %>% left_join(ipcc.r10) %>%
  # following exiobase for now, direct only, from Table 1
  mutate(education.energy.share.of.total=0.0026) %>% # other
  mutate_cond(OECD=="OECD", education.energy.share.of.total=0.0056) %>% # OECD
  mutate_cond(r10=="Europe", education.energy.share.of.total=0.0058) %>% # Europe
  select(iso,education.energy.share.of.total)

## Shares
# share health
calculated.shares.health.image <- image2010to2015.downscaled.rescom %>%
  left_join(health.exio2005to2015) %>% #drop_na() %>%
  mutate(dle.share.of.commercial.health=exio.energy.health/image.energy.commercial)
calculated.shares.health.image

# share education
calculated.shares.education.image <- image2010to2015.downscaled.total %>% rename(total.energy=value) %>%
  left_join(image2010to2015.downscaled.rescom %>% select(model,scenario,iso,unit,year,image.energy.commercial)) %>%
  left_join(education.exio2005to2015) %>%
  mutate(
    dle.share.of.commercial.education=(education.energy.share.of.total*total.energy)/image.energy.commercial
  )

## Shares to keep
keep.commercial.calculated.shares.image <-
  calculated.shares.health.image %>% select(iso,year,dle.share.of.commercial.health) %>%
  left_join(
    calculated.shares.education.image %>% select(iso,year,dle.share.of.commercial.education)
  ) %>% mutate(dle.share.to.keep =
                 dle.share.of.commercial.health +
                 dle.share.of.commercial.education) %>%
  drop_na(dle.share.to.keep) %>%
  pivot_longer(cols=c(dle.share.to.keep,dle.share.of.commercial.health,dle.share.of.commercial.education),
               names_to = "variable",
               values_to = "value")
# detail of health and education contributions
keep.commercial.calculated.shares.image %>%
  summarise(p05=quantile(value, probs = 0.05),
            p25=quantile(value, probs = 0.25),
            p50=quantile(value, probs = 0.50),
            p75=quantile(value, probs = 0.75),
            p95=quantile(value, probs = 0.95),
            .by = c("year","variable"))
# max-min results for 2015 across regions
keep.commercial.calculated.shares.image %>%
  filter(variable!="dle.share.to.keep", year==2015) %>%
  group_by(variable) %>%
  filter(value == min(value) | value == max(value)) %>%
  ungroup()
# US (dimension-specific) in this calculation method:
us.share.IObased.dims <- keep.commercial.calculated.shares.image %>%
  filter(iso=="USA", variable!="dle.share.to.keep", year==2015) %>%
  distinct(variable,value)
us.share.IObased.dims
# USA (total) in this calculation method:
us.share.IObased <- keep.commercial.calculated.shares.image %>%
  filter(iso=="USA", variable=="dle.share.to.keep", year==2015) %>%
  pull(value)
us.share.IObased
# key regions compared to USA
keep.commercial.calculated.shares.image %>%
  filter(variable=="dle.share.to.keep", year==2015) %>%
  mutate(relative.to.USA=value/us.share.IObased) %>%
  filter(iso%in%c(
    "CHN",
    "ZAF",
    "IND",
    "BRA",
    "GBR",
    "DEU",
    "FRA"
  ))
