Raw datafiles used to create the Rdata objects used in the analysis. These files are used in the scripts located at [/code/scripts/data-creation/](/code/scripts/data-creation/).

Note the raw streamflow data is not stored here (too large for github). Instead we store the commands needed to talk to the NVE database and pull streamflow data. See [this](/data/how-to/hvordan_henter_jeg_data_med_lescon_var.md) guide for how to pull data from the databases using lescon_var (internal system).

Supporting files `alternate_version_numbers.csv`, `table_A2_report_2016-85.csv` are used to create the database commands. `utelatt.csv` is used to quality control the streamflow data. 

 ## gamfelt catchments

 ### covariates
 - `gamfelt_catchment_covariates.csv` - catchment covariates from GIS. Created by Kolbjørn in 2019(?)
 - `README-gamfelt-covariates.txt` - description of gamfelt catchment covariates

 ### streamflow data
 - `utelatt.csv` - list of stations / years with problematic streamflow data. Manually identified (Kolbjørn and Danielle)
 - `alternate_version_numbers.csv` - alternate version numbers for some gauging stations. Manually identified (Kolbjørn)
 - `table_A2_report_2016-85.csv` - Table A2 from NVE report 2016:85. Identifies stations / years suitable for flood frequency analysis 
 - `lescon_var_commands_archive_05.txt` - lescon_var commands to pull data from archive 05 (HYDAG)
 - `lescon_var_commands_archive_35.txt` - leacon_var_commands to pull data from archive 35 (HYKVALP-ICECORR)

## zfelt catchments
 - `zfelt_catchment_covariates.csv` - catchment covariates from GIS. Created by Kolbjørn 05.07.2024
