## Scripts to create the floodGAM streamflow datasets

Scripts in this folder are used to create the data objects in the [processed-data](/data/processed-data/) and 
[cleaned-data](/data/cleaned-data/) folders, as well as the [lescon_var commands](/data/raw-data/) used to 
pull data from the Hydra II database.

-   `1-pull-and-clean-data-from-hyfin-complete.R` - combines and cross-checks the tables from [the NIFS report](https://publikasjoner.nve.no/rapport/2015/rapport2015_13.pdf) and [report
2016-85](https://publikasjoner.nve.no/rapport/2016/rapport2016_85.pdf) to create a [set of lescon_var commands](/data/raw-data/lesconvar_commands_NIFS-A2_archive_39-hyfincomplete.txt).
Data is then uploaded back into R and cleaned. Saves a set of stations that have at least 20 years of fine data, where every year has at least 200 days of data.

-   `tabell_A3_rapport_2016-85.txt` - Table of stations not suitable for flood frequency analysis
    because of poor data quality. Table copy-pasted from report
    (appendix, pg 58-70).



