## Scripts to create the floodGAM streamflow datasets

Scripts in this folder are used to create the data objects in the [processed-data](/data/processed-data/) and 
[cleaned-data](/data/cleaned-data/) folders, as well as the [lescon_var commands](/data/raw-data/) used to 
pull data from the Hydra II database.

-   `1-pull-and-clean-data-from-hyfin-complete.R` - combines and cross-checks the tables from [the NIFS report](https://publikasjoner.nve.no/rapport/2015/rapport2015_13.pdf) and [report
2016-85](https://publikasjoner.nve.no/rapport/2016/rapport2016_85.pdf) to create 
a [set of lescon_var commands](/data/raw-data/lesconvar_commands_NIFS-A2_archive_39-hyfincomplete.txt) 
for HYFIN_COMPLETE. Data is pulled using lescon_var and then uploaded back into R 
and cleaned. The cleaning process involves (1) removing years marked as 'utelatt' in Table A2, report 2016-85; (2) removing 
years where more than a third of the year is missing; and (3) cross-checking with the HYDAG archive. Step (3) 
is necessary since the historic data in HYFIN_COMPLETE relies on the virtually ice reduced data, which is not gap-filled. 
We therefore (i) check the minimum number of days per year in HYFIN_COMPLETE and HYDAG and discard years where both HYFIN_COMPLETE 
and HYDAG have < 300 days of data, and (ii) perform an annual maximum check using the HYDAG data: 
we check if HYFIN_COMPLETE contains an observation within +/- 24 hours of the date the annual maximum from HYDAG was observed. 
If there is no HYFIN_COMPLETE observation within +/- 24 hours of the needed point, we discard the year.


File output is fine/variable spaced streamflow data for a set of stations that have 
at least 20 years of fine data, where every year has at least 244 days of data and passes 
the archive cross-check. This is the `gamfelt` dataset. Additionally, the script saves a 
separate file containing supplemental HYDAG (daily) data for the gamfelt stations.

-   `2-process-to-durations-and-get-ann-max.R` - generates the `gamfelt` sets of 
annual maxima. Also generates annual maxima for supplementary datasets (gamfelt.hydagsupplement)


### Supplementing the record length with daily data

For certain catchments we wish the supplement the findata record with the much longer series of daily data.  
This is feasible, f. eks., if the catchment has a 'slow' response time with a culmination factor (ratio between
 1-hour and 24-hour median flood) close to one. See script `2-process-to-durations-and-get-ann-max.R` and comments within.  

We reccomend running the analysis both with and without the daily data supplement 
to assess impact on results. 

