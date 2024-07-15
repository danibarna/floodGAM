
## Overview

As part of the floodGAM analysis, we developed a flood dataset focused
on sub-daily sampling frequency for flood frequency analysis using
annual maximum data. It includes 259 stations, each with at least 20
years of total data and at least 11 years of sub-daily data.

The stations are based on NVE-rapport 2016:85 [Flomdata: utvalg og
kvalitetssikring av flomdata for
flomfrekvensanalyser](https://asp.bibliotekservice.no/nve/title.aspx?tkey=23147),
with added requirements for sub-daily sampling frequency around annual
maxima and data up to 2023.

## Dataprodukter

#### Flomdata

download the set of annual maxima \[l/s/km2\] here.

the full streamflow time series for each station is too large to upload
here, but can be found on the NVE server at (link). The database
commands used to pull the data from HYDRA II are saved in this
repository at (link) and a database how-to guide can be found at (link).

#### Felt egenskaper

download catchment covariates for each of the 259 stations here.

#### Andre dataprodukter

We also store other datasets relevant to the floodGAM analysis in this
repository. The raw-data folder contains the list of station names,
version numbers, and manually controlled utelatt years and stations used
to make the database commands. The how-to folder contains guides
describing how to get raw streamflow data. The Rdata folder saves many
intermediate data objects used in the analysis.

All code used to clean and construct the dataset can be found here.

## Valg av flomdata

NVE har ingen «perfekt» arkiv for verken findata eller døgndata. Noen
arkiver er isredusert, mens andre er ikke. Det samme gjelder
kompletthet.

Vi bruker data fra arkiv HYKALP-ICECORR (arkiv 35), som har
primærtkontrollerte data med fin/variabel tidsoppløsning og er virtuelt
isreduserte. HYKVALP-ICECORR er ikke sekundærkontrollert eller
kompletterte.

Fordi HYKVAL-dataene ikke er komplette, må vi derfor ta beslutninger om
hvordan vi skal håndtere år med manglende data. Vi velger å kryssjekke
HYKVALP-ICECORR dataene med data fra et annet arkiv, HYDAG (arkiv 05),
som inneholder kontrollerte, kompletterte døgndata som er isredusert og
etterkontrollert.

Denne delen av rapporten beskriver prosedyrene vi bruker for å
kvalitetskontrollere HYKVALP-ICECORR dataene.
