
## Raw data files for use in floodGAM analysis.

Data in this folder originates from published reports and is not edited by hand.
Instead it is loaded and processed/cleaned using the scripts located at
[/code/scripts/data-creation/](/code/scripts/1-data-creation/).

The exception is the file `tabell_A2_rapport_2016-85_daba_edited.txt`
(see notes below).

Note the raw streamflow data is not stored here (too large for github).
Instead we store the commands needed to talk to the Hydra II database
and pull streamflow data. See
[this](/data/how-to/hvordan_henter_jeg_data_med_lescon_var.md) guide for
how to pull data from the databases using lescon_var (internal system).

### Station selection

The gamfelt stations are selected from the stations listed in two NVE
reports: **Nasjonalt formelverk for flomberegning i små nedbørfelt**
[(report
13-2015](https://publikasjoner.nve.no/rapport/2015/rapport2015_13.pdf))
and **Utvalg og kvalitetssikring av flomdata for flomfrekvensanalyser**
([report
2016-85](https://publikasjoner.nve.no/rapport/2016/rapport2016_85.pdf)).
There is much overlap between the two reports, but not every station
listed in report 13-2015 can be found in report 2016-85. Additionally,
about 30% of the stations listed in report 13-2015 were subsequently
identified in report 2016-85 as too low-quality for flood frequency
analysis.

-   `tabell_5_NIFS_rapport_2015-13.txt` - Table of stations used in
    NIFS report
    13-2015.
    Table copy-pasted from report text (vedlegg 1, table 5, pg 39-46).

-   `tabell_A3_rapport_2016-85.txt` - Table of stations not suitable for flood frequency analysis
    because of poor data quality. Table copy-pasted from report
    (appendix, pg 58-70).

-   `tabell_A2_rapport_2016-85_daba_edited.txt` - Table of stations
    identified by report
    2016-85
    as suitable for flood frequency analysis. Table copy-pasted from
    report (appendix, pg 32-57).

    For 15 stations, the version number in the published table A2 does not
    produce a valid data file. In these cases, when possible, we substitute with 
    the version number used in the lescon_var commands in the Flomkart project. If the station does not exist
    in the Flomkart file, we select the valid version number from Hysopp.
    
    The valid version numbers are inserted by hand into the table. In addition,
    more years for the station Jaren (12.286) are added by hand to the 'utelatt' column 
    after reccomendation from the data section at NVE. 

These three tables are used to make the lescon_var commands for the gamfelt dataset:

-   `lesconvar_commands_NIFS-A2_archive_39-hyfincomplete.txt` -
    lescon_var commands to pull data from Hydra II, archive 39
    (hyfin_complete). These commands are created by
    [this](/code/scripts/1-data-creation/streamflow/1-pull-and-clean-data-from-hyfincomplete.R)
    script.
    
### Supplementary (daily) data

For certain stations we are interested in supplementing the findata record with the 
(much longer) series of daily data. See [this file]() for a discussion of this approach.

-   `lesconvar_commands_min-findata_archive_37-hydag.txt` - lescon_var commands to pull data from hydag (archive 37) for a 
certain set of stations (the 'min-findata' or 'minfin' stations). All stations in this file have at least 10 years of fine data in hyfin_complete with at least 200 days 
of data per year.


    
### Catchment descriptors

-   `catchment_properties_zenodo_5382146.csv` - catchment covariates from GIS. Created by Kolbjørn (?). [Link](https://zenodo.org/records/5382146) 
to published dataset.

-   `description_catchment_properties_zenodo_5382146.txt` - description of the catchment covariates.

## zfelt catchments

-   `zfelt_catchment_covariates.csv` - catchment covariates from GIS.
    Created by Kolbjørn 05.07.2024
    
## Other

-   `map_files` -- large raster data items not uploaded to GitHub.
