# Mapping data (DLE-to-IAM) for Decent Living Energy for SHAPE

Done by running `data-raw/exio3_splitfactors_prep/DLE-MESSAGE_allocation.R` in R (which relies on the script `data-raw/exio3_splitfactors_prep/Import_EXIO3.R`).

## MRIO-based mapping using EXIOBASE3
There are two CSV files for mapping DLE (indirect) demands in four dimensions (Clothing/Nutrition/Education/Health) to MESSAGEix (direct) demand categories (Industry(`Ind`)/Transportation(`Trp`)/Commercial(`Com`)). 
DLE values already have the electric/non-electric breakdown, each of which a corresponding CSV file applies to. This mapping is derived for MESSAGE R12 regions from EXIOBASE3 2015.

- `data-raw/DLE_EXIOmapping_elec_share_kikstra-Jan2024.csv`
- `data-raw/DLE_EXIOmapping_non_elec_share_kikstra-Jan2024.csv`


Each file has three index columns for DLE dimensions `dle_dim`, MESSAGE demand categories `sector`, and the DLE service demand regions `src_reg`. 

In addition, the file has 12 columns, representing MESSAGE demand regions. 

The DLE consumption was only accounted to the final users of the energy services, which can then be tracked back through the MRIO setup to the regions where the actual energy demand occurs.

The cell values in each file mean what shares of total dimensional DLE of the region that are coming from the `sector` and the region (in columns). The column `tot.share` shows the aggregate share among the three demand `sector`s.


## Other mapping using Life Cycle Analysis studies
See `data-raw/DLE_to_IAM_simplified.csv`.

