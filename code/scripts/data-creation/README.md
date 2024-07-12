## Streamflow data

#### `create-database-commands.R` 
Input:
 - `table_A2_report_2016-85.csv` - station names
 - `alternate_version_numbers.csv` - station version numbers
   
Output:

 - `lescon_var_commands_archive_X.txt` - database commands needed to pull data for each station/version from archive 35 (HYKVALP-ICECORR) amd archive 05 (HYDAG). See [this guide](/data/how-to/hvordan_henter_jeg_data_med_lescon_var.md) for how to use the command files to pull data from NVE databases.

#### `clean-and-process-rawdata-from-database.R`
This script cleans data retrieved from the NVE database. Stations are compiled into a single data.table, removing negative values and converting dates to the correct format. The cleaned data is then used as input for `quality-control-streamflow-data.R`.

#### `quality-control-streamflow-data.R`
This script contains the procedures we use to quality control the HYKVALP-ICECORR data. See [this]() for details. 

## Catchment covariate data
