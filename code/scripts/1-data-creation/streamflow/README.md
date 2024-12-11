## Scripts to create the floodGAM streamflow datasets

Scripts in this folder are used to create the data objects in the [processed-data](/data/processed-data/) and 
[cleaned-data](/data/cleaned-data/) folders, as well as the [lescon_var commands](/data/raw-data/) used to 
pull data from the Hydra II database.

-   `1-pull-and-clean-data-from-hyfin-complete.R` - combines and cross-checks the tables from [the NIFS report](https://publikasjoner.nve.no/rapport/2015/rapport2015_13.pdf) and [report
2016-85](https://publikasjoner.nve.no/rapport/2016/rapport2016_85.pdf) to create a [set of lescon_var commands](/data/raw-data/lesconvar_commands_NIFS-A2_archive_39-hyfincomplete.txt).
Data is then uploaded back into R and cleaned. Saves hyfin_complete streamflow data for a set of stations that have 
at least 20 years of fine data, where every year has at least 200 days of data (the
`gamfelt` dataset).

-   `1.5-pull-and-clean-data-hydag-supplement.R` - if using a dataset supplemented with daily data, this script 
creates a [set of lescon_var commands](/data/raw-data/lesconvar_commands_min-findata_archive_37-hydag.txt)
to pull data from hydag. Data is then uploaded back into R and cleaned. Saves hydag streamflow data for a set of stations that have 
at least 20 years total of data, where every year has at least 200 days of data and at least ten of those years are fine data years.

-   `2-process-to-durations-and-get-ann-max.R` - generates the `gamfelt` sets of 
annual maxima. Also generates annual maxima for supplementary datasets (gamfelt.hydagsupplement and minfin.hydagsupplement)


