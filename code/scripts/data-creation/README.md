## Streamflow data

#### `create-database-commands.R` 
Takes in station names and version numbers and outputs database commands needed to pull data for each station/version from archive 35 (HYKVALP-ICECORR) amd archive 05 (HYDAG). See [this guide](/data/how-to/hvordan_henter_jeg_data_med_lescon_var.md) for how to use the command files to pull data from NVE databases.

#### `clean-and-process-rawdata-from-database.R`
Basic data cleaning for NVE database data: validate all files, combine stations into a single data.table, remove negative values, and convert dates to the correct format. The resulting data object is used as input for `quality-control-streamflow-data.R`.

#### `quality-control-streamflow-data.R`
Procedures for quality control of fine/variable spaced HYKVALP-ICECORR data for flood frequency analysis based on annual maxima. See [this guide]() for details. 


## Catchment covariate data
