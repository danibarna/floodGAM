## Processed data objects for floodGAM analysis

These folders contain annual maxima and catchment descriptors for use in regression analysis.

The different folders contain slightly different data sets.


### gamfelt
The `gamfelt` folder contains the core data set: 1-hour annual maximum floods from 251 (253?) stations 
together with catchment descriptors. Each station has at least 20 years of fine data. 
The only archive used to create this dataset is hyfin_complete.


### gamfelt-durations
This folder contains several different datasets. 

 - `gamfelt_durations_annual_maxima.rds` - same set of stations and annual maxima that sits in 
  the `gamfelt` folder, but contains durations up to 72 hours in addition to 1-hour data.
 
 - `gamfelt_hydagsupplement_durations_annual_maxima.rds` - same set of stations, but different records lengths. In certain
  cases the station record (from hyfin_complete) has been supplemented with daily data from the hydag archive. Durations from 1 hour to 72 hours.
 



