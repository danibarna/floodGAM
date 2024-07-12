Raw datafiles.

Catchment covariate datasets created from GIS. 

Note the raw streamflow data is not stored here (too large for github). Instead we store the commands needed to talk to the NVE database and pull streamflow data. 

Supporting files `alternate_version_numbers.csv`, `table_A2_report_2016-85.csv` are used to create the database commands. `utelatt.csv` is used to quality control the streamflow data. If needed, see [/code/scripts/data-creation/](/code/scripts/data-creation/)

 ## gamfelt catchments

 ### covariates

 ### streamflow data
 - `utelatt.csv` - list of stations / years with problematic streamflow data. Manually identified (Kolbjørn and Danielle)
 - `alternate_version_numbers.csv` - alternate version numbers for some gauging stations. Manually identified (Kolbjørn)
 - `table_A2_report_2016-85.csv` - Table A2 from NVE report 2016:85. Identifies stations / years suitable for flood frequency analysis 
 - `lescon_var_commands_archive_05.txt` - lescon_var commands to pull data from archive 05 (HYDAG)
 - `lescon_var_commands_archive_35.txt` - leacon_var_commands to pull data from archive 35 (HYKVALP-ICECORR)

See [this](/data/how-to/hvordan_henter_jeg_data_med_lescon_var.md) guide for details how to run the lescon_var commands to get streamflow data.

## zfelt catchments
 - `zfelt_catchment_covariates.csv` - catchment covariates from GIS. Created by Kolbjørn 05.07.2024
