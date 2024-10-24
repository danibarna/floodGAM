
<h1 align="center">
floodGAM datasets
</h1>

## Overview

As part of the floodGAM analysis, we developed a flood dataset focused
on sub-daily (“fine”) sampling frequency.

NVE report 2016:85 [Flomdata: utvalg og kvalitetssikring av flomdata for
flomfrekvensanalyser](https://asp.bibliotekservice.no/nve/title.aspx?tkey=23147)
identifies 529 stations suitable for flood frequency analysis. Starting
with these 529 stations, we independently evaluate each year of data at
each station for ability to capture annual maxima at sub-daily sampling
frequency, discarding years that are inadequate. The result is a set of
248 stations (the `gamfelt` dataset), each with at least 20 years of
total data and at least 10 years of sub-daily data.

We provide both (i) the `gamfelt` dataset and (ii) all scripts and
resources needed to recreate the dataset from the raw data stored in
HYDRA II.

Feedback on the dataset is very welcome.

## How to get the data

The annual maxima, catchment descriptors, descriptive table and summary
of the `gamfelt` dataset are stored in
[`/data/processed-data/gamfelt/`](/data/processed-data/gamfelt/)

## File structure

In addition to the `gamfelt` dataset, this data folder contains many
data products relevant to the larger floodGAM analysis (e.g. model
fitting and evaluation).

- `processed-data` – Any data loaded/manipulated/changed/saved with code
  from the `code` folders.

- `raw-data` – HYDRA II database commands and other raw data.

- `how-to-guides` – how to use the database commands in `raw-data`.

## Data pipeline

The data pipeline is the process of building the `gamfelt` dataset from
the raw streamflow data provided by NVE. There are multiple steps in the
data pipeline. The “findata quality control” step is described in more
detail in the following section.

### Dependencies

Recreating the dataset with the scripts in this repository requires:

- the NVE database **HYDRA II**
- access to the internal NVE system **lescon_var**
- the programming language **R**.

Some of the intermediate data files are large (part of the quality
control requires downloading and cross-checking the HYKVALP-ICECORR
database with HYDAG). Any data file over 50 Mb is stored on the shared
NVE drive under /Brukere/daba/.

| Action                  | Description                                                                                                             | Requires                                                                                                                                                           | Output saved? |                 Where?                  |
|-------------------------|-------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-------------:|:---------------------------------------:|
| Get streamflow data     | Download data from HYDRA II                                                                                             | lescon_var (internal system), [`lescon_var_commands.txt`](/data/raw-data/), [lescon_var user guide](/data/how-to-guides/hvordan_henter_jeg_data_med_lescon_var.md) |      \-       |                   \-                    |
| Change formatting       | Change downloaded data to .rds format                                                                                   | [`clean-and-process-rawdata-from-database.R`](/code/scripts/data-creation/)                                                                                        |      yes      |  [daba](\nve.no\fil\h\HM\Brukere\daba)  |
| Findata quality control | Handle missing data, check time spacing at annual maxima, enforce minimum record length, choose excluded years/stations | [`quality-control-streamflow-data.R`](/code/scripts/data-creation/)                                                                                                |      yes      |  [daba](\nve.no\fil\h\HM\Brukere\daba)  |
| Process data            | Select annual maxima                                                                                                    | [`get-annual-maxima.R`](/code/scripts/data-creation/)                                                                                                              |      yes      | [github](/data/processed-data/gamfelt/) |

## Findata quality control

We start with the 529 stations from report
[2016:85](https://asp.bibliotekservice.no/nve/title.aspx?tkey=23147).
The findata quality control process evaluates each year of data at these
stations, discarding those with too much missing data or improper
spacing around annual maxima (**archive cross-check criteria**), too few
years of data (**minimum record length criteria**), or are otherwise
unsuited for the sub-daily dataset (**manual quality control**).

### Filtering and quality control by the numbers

Out of 529 stations, 329 have some fine data, but only 273 have at least
20 years of total data and 10 years of fine data (where a “year of fine
data” is defined as a year having at least 200 days where the median
spacing between observations was less than 24 hours).

We quality control these 273 stations year by year, removing those that
fail the ‘archive cross-check’ criteria, resulting in 255 stations. We
then manually remove 7 stations and several individual years from the
remaining stations. The final dataset consists of 248 stations, each
with at least 20 years of total data and 10 years of fine data.

##### Distribution of data removed vs final dataset

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

### Archive cross-check criteria

NVE has no “perfect” archive for either fine data or daily data. Some
archives are ice-reduced, while others are not. The same applies to
completeness.

We use data from the HYKVALP-ICECORR archive (archive 35), which has
primarily controlled data with fine/variable time resolution and is
virtually ice-reduced. HYKVALP-ICECORR is not currently secondarily
controlled or complete[^1].

Because the HYKVAL data is incomplete, we must make decisions on how to
handle years with missing data. We choose to cross-check the
HYKVALP-ICECORR data with data from another archive, HYDAG (archive 05),
which contains controlled, complete daily data that is ice-reduced and
re-checked.

The cross-checking has two components:

#### 1. Check the minimum number of days per year in HYKVALP-ICECORR and HYDAG

First we count the number of days per year for every year and every
station. Then we can remove years that have less than 200 days of
observations in HYKVALP-ICECORR or less than 300 days in both
HYKVALP-ICECORR and HYDAG. We can look at some of the years that we
remove:

##### Example of years removed due to \< 200 days in HYKVALP-ICECORR:

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

##### Example of years removed due to \< 300 days in both archives:

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

#### 2. Check minimum time spacing using HYDAG annual maxima

The criteria for cross-checking annual maxima with HYDAG are a bit more
complicated. HYDAG and HYKVALP-ICECORR do not perfectly match. There are
some stations and years that are in HYKVALP-ICECORR but not in HYDAG,
and vice versa.

``` r
# find unique station-year combinations in both hydag and hykvalp-icecorr:
hykval.sy <- data35[,unique(.SD),.SDcols = c("ID","yk")]
hydag.sy <- data05[,unique(.SD),.SDcols = c("ID","yk")]

setkey(hykval.sy,ID,yk); setkey(hydag.sy,ID,yk)
```

``` r
# station-years in hykvalp-icecorr *not* in hydag:
hykval.sy[, in.hydag := FALSE][hydag.sy, in.hydag := TRUE]
print(hykval.sy[in.hydag==FALSE], nrows = 5)
```

    ## Key: <ID, yk>
    ##           ID    yk in.hydag
    ##       <char> <num>   <lgcl>
    ##  1:  1200013  2023    FALSE
    ##  2: 12300029  1998    FALSE
    ##  3: 12300029  1999    FALSE
    ##  4: 12300029  2000    FALSE
    ##  5: 12300029  2001    FALSE
    ## ---                        
    ## 66:  7500022  2018    FALSE
    ## 67:  7500028  2023    FALSE
    ## 68:   800008  2003    FALSE
    ## 69:   800008  2004    FALSE
    ## 70:   800008  2005    FALSE

``` r
# station-years in hydag *not* in hykvalp-icecorr:
hydag.sy[, in.hykval := FALSE][hykval.sy, in.hykval := TRUE]
print(hydag.sy[in.hykval==FALSE], nrows = 5)
```

    ## Key: <ID, yk>
    ##           ID    yk in.hykval
    ##       <char> <num>    <lgcl>
    ##   1: 1100004  1975     FALSE
    ##   2: 1100004  1976     FALSE
    ##   3: 1500053  1969     FALSE
    ##   4: 1500053  1973     FALSE
    ##   5: 1500053  1974     FALSE
    ##  ---                        
    ## 279: 8300006  1970     FALSE
    ## 280: 8300006  1971     FALSE
    ## 281: 9800004  1979     FALSE
    ## 282: 9800004  1980     FALSE
    ## 283: 9800004  1981     FALSE

There are 70 unique station - year combinations where we have data in
HYKVALP-ICECORR but not in HYDAG. For the remaining 14,156 unique
station - year combinations where we have data in both HYKVALP-ICECORR
and HYDAG, we perform an annual maximum check:

Calculate the annual maxima using HYDAG. Check if HYKVALP-ICECORR
contains an observation within +/- 24 hours of the date the annual
maximum from HYDAG was observed. If there is no HYKVALP-ICECORR
observation within +/- 24 hours of the needed point, discard the year.

We can look at some of the years we discard:

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Manually exclude years / stations

The archive cross-check criteria systematically removes some problematic
years, but will not catch all issues. External knowledge about a
station’s reliability can help in manually validating and removing
problematic data if necessary.

Since manual removal can be subjective, it is always good to get a
second opinion on any stations and years removed manually. Feedback here
is always welcome.

The files containing the manually removed stations and years are:

- [`utelatt_stations.csv`](/data/raw-data/) - stations that should be
  excluded
- [`utelatt_years.csv`](/data/raw-data/) - individual years at remaining
  stations that should be excluded
- [`utelatt_notes.xlsx`](/data/raw-data/) - some notes on manually
  removed years and stations

[^1]: Det kommer snart (slutten av 2024/tidlig 2025) en oppdatering i
    databasen. HYKVAL-data skal bli sekundærkontrollert etter en bestemt
    dato. Data før denne datoen skal ikke endres.
