# desire-review

**This version of this repository is meant to support peer-review. It will not be maintained and is not meant for duplication or further use.**

`desire-review` hosts a set of decent living energy tools to assess progress towards Decent Living Energy (DLE) requirement in integrated assessment model pathways.
It is the main repository hosting the DESIRE model. 

The final version of the full SHAPE Sustainable Development Pathway (SDP) data is to be downloaded from: Soergel, B., Rauner, S., Daioglou, V., Weindl, I., Mastrucci, A., Carrer, F., Kikstra, J., Ambrósio, G., 2024a. Scenario database of the SHAPE project. https://doi.org/10.5281/zenodo.13752116
With an interactive Scenario Explorer available at: https://shape.apps.ece.iiasa.ac.at/

Below, in this README, you find a brief description of this repository. 

# Installation

This repository uses Git LFS.

To install the necessary packages, refer to the `renv_install_*.R` scripts.


# Overview of folders and R scripts in repository

## Manuscript replication files for Kikstra et al. 2024 (in review)

Model runscript: `analyses\SHAPE_calculator_run_all-options.R`

Figures of manuscript: `analyses\SHAPE_main_figures_manuscript_v2.R`

Data and figures: `kikstra_erl_data_and_figures_revisions_v1.zip`.

## Root folder
### Folders

`.github`: information for running test workflows online (using Github Actions)
`R`: the core code. Holds many R scripts, including all modules of the model.
`analyses`: contains both scripts and output files. Scripts are meant to be applications using the modules of the code in `R`, or are otherwise relevant analyses, with output files for instance being figures and tables.
`data-raw`: input files.
`data`: empty folder to store processed data during runs.
`docs`: documentation.
`renv`: manages dependencies on other R packages.
`tests`: testing code that tests code, especially that in the `R` folder.

### Files
All other files are either for package, dependency, or version control management.

## Most important R scripts of DESIRE

- `R`
  - `collect-dle-threshold-input.R`: creates Decent Living Energy (DLE) thresholds, being the annual need to continue to support DLS (i.e., operation + infrastructure replaces; NOT including construction of new infrastructure). 
  - `collect-dle-gaps.R`: calculates the annual energy needs gap (also in terms of operation + infrastructure replaces; NOT including construction of new infrastructure).
  - `collect-dle-current-subtractinggapsfromthreshold.R`: calculates a hypothetical estimate of how much energy currently is related to the provisioning of DLS (the DLE threholds minus the DLE gap).
  - `collect-dls-gaps.R`: creates static, base year files, for DLS deprivation headcount, DLS thresholds, DLS depth-of-deficits, and DLS gaps.
  - `collect-ei.R`: creates static, base year files, for the energy intensity of delivering DLS services (in dimension-specific units).
  - `collect-con-gap.R`: creates static, base year files, for total construction energy required for achieving full DLS (in MJ, and MJ/cap).
  - `dle_run.R`: script for running DLE bottom up calculations, making forward-looking bottom-up energy needs pathways (based on Kikstra et al. 2021, ERL publication).
  - `DLE_integration_data_structure.R` defines data structure of `DLE.dimension` and `DLE.scenario` objects.4r
  - `calculator_run.R`: script that runs all modular scripts required to perform a calculator run (like in Kikstra et al. in review).
  - `utils.R`: loads many essential functions for this software package.

## R scripts used for particular projects and publications

- DESIRE introduction and SHAPE application: Kikstra et al. (paper in review) 
  - `analyses\SHAPE_calculator_run_all-options.R`: perform all DESIRE runs
  - `analyses\SHAPE_main_figures_manuscript_v2.R`: all visualisations and calculations for the paper
- Kikstra et al. (2021), Environmental Research Letters
  - Very similar to the code called by `dle_run.R`



## All scripts in the `R` folder

- DESIRE
  - `calculator_run.R`: script that runs all files required to run DESIRE.
  
  - `calculator_input-options.R`: configuration file for a DESIRE run.
  
  - "activity" module: calculating (total) energy consumption projections
    - `calculator_activity-fe-process-downscaled.R`: process downscaled IAM data
    
    - `calculator_activity-fe-process-downscaled-rawdata.R`: process raw version of downscaled IAM data [SHAPE version]
    
    - `calculator_activity-fe-process-IMAGE-REMIND-transport-aviation-data.R`: process transport/aviation split data [SHAPE version for IMAGE and REMIND]
    
    - `calculator_activity-fe-process-IMAGE-rescom-data.R`: process traditional/residential/commercial split data [SHAPE version for IMAGE]
    
  - "needs" module: calculating energy needs projections
    - `calculator_needs-dle-mapping.R`: map bottom-up energy needs on IAM-style final energy sectors
    
    - [placeholder] `calculator_needs-climate-scale.R`: scale energy needs with climate change
    
    - `calculator_needs-dle-efficiency.R`: calculate service provisioning efficiency projections and their effect on projections of energy needs
    
    - `calculator_needs-dle-input.R`: process input (current) DLE threshold data (coming from `collect-dle-threshold-input.R`)
    
  - "inequality" module: calculating inequality projections
    - `calculator_inequality-fe-process-starting.R`: process energy consumption inequality data and calculate (current) energy gini coefficients
    
    - `calculator_inequality-process-income-driver.R`: process income inequality data projections used to drive energy inequality projections
    
    - `calculator_inequality-project-gini-to-gini.R`: project energy inequality (gini) based on income inequality projections (gini)
    
    - `calculator_inequality-simple-filling-fe-process-starting.R`: impute missing energy inequality gini values
    
  - "population" module: formatting population projections
    - `calculator_population.R`: formatting population projections
    
  - "output" module: calculating results
    - `calculator_output_lognormal-calculation.R`: calculate DESIRE output
    
- Decent Living Standards and Decent Living Energy (`dle_*.R` is new code, `DLE_*.R` is code that is based on the `iiasa/DLE-scaleup` repository which reproduces Kikstra et al. 2021, ERL):
  - "Current" DLS and DLE modules:
    - DLS
      - `collect-dls-gaps.R`: calculate DLS data products (Decent Living Standard threshold, Deprivation headcount, Service gap, Depth-of-deficit)
      
    - DLE
      - `collect-ei.R`: collect all assumed energy intensities
      
      - `collect-dle-threshold-input.R`: collect all DLE thresholds
      
      - `collect-con-gap.R`: calculate total energy required to build all DLS-supporting infrastructure (assuming current population, no population growth)
      
      - `collect-dle-gaps.R`: calculate energy needs gaps
      
      - `collect-dle-current-subtractinggapsfromthreshold.R`: calculate energy currently going to supporting DLS-supporting activities 
      
  - DLE-only "scenario" module: 
    - `dle_run.R`: script that runs all files required to create a new DLE scenario.
  
    - `dle_core.R`: core code for creating a new DLE scenario.
  
    - `DLE_integration_data_structure.R`: essential code that sets up the objects for DLE scale-up functions and allows for the integration between all DLE dimension specific files.
  
    - `DLE_appliances.R`: decent living gaps and energy needs calculation for household appliances.
  
    - `DLE_clothing.R`: decent living gaps and energy needs calculation for clothing and footwear.
  
    - `DLE_cooling_con.R`: decent living gaps and energy needs calculation for the construction of residential cooling systems.
  
    - `DLE_cooling_op.R`: decent living gaps and energy needs calculation for the operation of residential cooling systems.
  
    - `DLE_education.R`: decent living gaps and energy needs calculation for the provisioning of basic education.
  
    - `DLE_health.R`: decent living gaps and energy needs calculation for the provisioning of basic health care.
  
    - `DLE_heating_con.R`: decent living gaps and energy needs calculation for the construction of residential heating systems.
  
    - `DLE_heating_op.R`: decent living gaps and energy needs calculation for the operation of residential heating systems.
  
    - `DLE_hotwater_op.R`: decent living gaps and energy needs calculation for heating water.
  
    - `DLE_housing.R`: decent living gaps and energy needs calculation for permanent solid housing structures.
  
    - `DLE_nutrition.R`: decent living gaps and energy needs calculation for food (but excluding preparation (cooking) and storage (refrigeration)).
  
    - `DLE_roads.R`: decent living gaps and energy needs calculation for the construction of new roads.
  
    - `DLE_sanitation.R`: decent living gaps and energy needs calculation for safely managed sanitation.
  
    - `DLE_transport.R`: decent living gaps and energy needs calculation for physical (motorized) mobility.
  
    - `DLE_water.R`: decent living gaps and energy needs calculation for safe water provisioning.




# How to use


Below, we introduce how to use:
- DESIRE: assessing the energy availability for providing human needs in IAM scenarios
- DLE Scenario: creating a bottom-up scenario of how much energy is provided for supporting Decent Living Standards
- DLS current: creating static, base year files, for DLS deprivation headcount, DLS thresholds, and DLS gaps
- DLE current: creating static, base year files, for energy intensities, DLE per capita, DLE total, and new construction energy for full DLS (at current population) 

## DESIRE

### Default run (worked example)

To run the current version of DESIRE, open and run the `R\calculator_run.R` file in Rstudio.
Output in the form of figures and tables can be found in the folder `analyses`.

### Data notes:

Currently, none.

Data handling, especially of big files, may be changed in the future to better separate input data from the model structure.

### Changing input parameters

Input parameters can currently be found and edited in the  `calculator_input-options.R` scripts.

For one example, see `analyses/SHAPE_calculator_run-all-options.R`


## DLE scenario

### Default run (worked example)

To run the current version of the Decent Living Energy machinery (using the code from the `iiasa/DLE-scaleup` repository), open and run the `R\dle_run.R` file in Rstudio.
Output in the form of a scenario data file can be found in the folder `data`.

### Changing input parameters

Input parameters can currently be found and edited in the file `R\input-options.R`.
DLS thresholds can be flexibly specified in `dls_threshold_config.R`.


## DLS current

### Default scripts

#### Decent Living Standards
To extract static, base year files, for DLS deprivation headcount, DLS thresholds, DLS depth-of-deficits, and DLS gaps, run `collect-dls-gaps.R`. 
Output files are multiple, and can be found in the folder `data`.

#### Energy for DLS: energy intensities
To extract static, base year files, for the energy intensity of delivering DLS services, run `collect-ei.R`. 
Output files are multiple, and can be found in the folder `data`.

#### Energy for DLS: Decent Living Energy (DLE) threholds and gaps
To extract current DLE threshold estimates, run `collect-dle-threshold-input.R`.
To extract current energy development gap (DLE gap) estimates, run `collect-dle-gaps.R` (op + con.rep).
To extract an estimate of the energy required for new construction, run `collect-con-gap.R`.
Output files are multiple, and can be found in the folder `data`.

### Data notes:

All necessary input data should currently be available in the `data-raw` folder.

### Changing input parameters

DLS thresholds can be flexibly specified in `dls_threshold_config.R`.

For a worked example where multiple versions are created based on different threshold sets, see the `analyses/create_multiple_DLE_threshold_versions.R`.



# Information for developers (OUT OF DATE)

## Package dependencies

We use `renv` for handling and recording used dependencies.

### Installing dependencies

This is currently done in the `R/utils.R` script. The variable ENVIRONMENT is used to determine what set of packages is loaded, depending on your use-case, with "default" being the minimum to ensure functionality or the DLE and calculator modules. More packages (including for testing, plotting, and other development tools) can be loaded if necessary, with ENVIRONMENT<<-"dev" being the recommended setting for developers.

### Updating dependencies

When dependencies change, the lockfiles used by `renv` need to be updated, too. This can be done regularly, to keep up-to-date with packages. This must be done immediately if a new dependency is created, and is highly recommended to do immediately if a dependency is removed. 
For this, 
1. Add the new package to `R/dev/renv_dependencies.R`(choose if the package is part of the default, development, testing or plotting packages)
2. Run the script `R/dev/renv_create_lock.R`. 


## Code style

Before committing new commits, we would appreciate it if you run the function `clean_code_style()`

## Testing

To ensure that no aspects of the code break, we run the tests using `tests/testthat.R`.

Each code added should ideally come with new tests. 

### Running code tests locally

Either all in one go using `tests/testthat.R`, or one by one in the `tests/testthat/*.R` files. Running `tests/testthat.R` will result in output as shown below with some expected failing tests.

### Running code tests on GitHub Actions

Automated testing using a combination of GitHub Actions (to automatically test any PR to the main branch) and a self-hosted (IIASA) runner is planend.
It is possible that in the automated version, not all local tests can be run.
Therefore, running local tests remains advised.


### Updating test-data (expected output of integration tests)

Currently the following files need to be updated if something changes: 
- `tests/testthat/testdata/integration/expected/RESULTS-DLE-emulator-SHAPE-final.RData` 
  - how to update: run `tests/testthat/test-calculator_output-lognormal-calculation.R` and replace the file above (see variable `expected.output`) with the new outcome (see variable `received.output`)
- `tests/testthat/testdata/expected_outputs/DLS_dataproducts_combined.csv`
  - how to update: run `collect-dls-gaps.R` (using "default" config in `dls_threshold_config.R`) and replace the file above.

### Current expected test results
The code is still under development, and at times tests may not all be passing.
This holds especially for running the tests on GitHub Actions, which hasn't been made to fully work yet.

#### Online: GitHub Actions
The GitHub Actions CI/CD workflow is currently not running successfully.

#### Local: run tests/testthat.R manually

The tests are expected to pass locally.

Expected output of running `tests/testthat.R` is something like: 

```
══ Results ═══════════════════════════════════════════════════════════════════════════
Duration: 569.8 s

── Skipped tests (3) ─────────────────────────────────────────────────────────────────
• empty test (3): test-collect-ei.R:28:1, test-running-calculator_run.R:8:1,
  test-running-dle_run.R:10:1

[ FAIL 0 | WARN 278 | SKIP 3 | PASS 772 ]
```


# Credits

List of contributors: [Jarmo Kikstra](https://orcid.org/0000-0001-9405-1228), [Jihoon Min](https://orcid.org/0000-0002-0020-1174), [Alessio Mastrucci](https://orcid.org/0000-0002-5611-7780), [Jonas van Laere](https://github.com/jonas-v-l), [Hanbit Lee](https://github.com/hanbitlee), and [Tommaso Zaini](https://iiasa.ac.at/staff/tommaso-zaini)

Lead maintainers: [Jarmo Kikstra](https://orcid.org/0000-0001-9405-1228) and [Tommaso Zaini](https://iiasa.ac.at/staff/tommaso-zaini)

License: all rights reserved. While we plan to (re)publish this repository with an open source license (likely copyleft), the current version is meant to serve the review of a manuscript. Please do not rehost or reuse this data. Please do feel free to ask questions and report any potential issues to kikstra@iiasa.ac.at.
