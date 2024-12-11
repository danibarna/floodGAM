##
##
##
##
##
##
##
## Get annual maxima from the cleaned and filtered HYKVALP-ICECORR data
## -----------------------------------------------------------------------------

library(data.table)
library(lubridate)

gamfelt.hyfinc <- readRDS(paste0("~/floodGAM/data/cleaned-data/",
                                 "gamfelt-NIFS-A2-hyfincomplete.rds"))


# Process the data into different durations -------------------------------

source(paste0("~/floodGAM/code/functions/","fn_durations_streamflow.R"))

dvec = c(1, 6, 12, 18, 24, 36, 48, 72) #durations (hours)

am.d <- createdurations(gamfelt.hyfinc,dvec)
setnames(am.d,"sQm3_s","Qm3_s") # fix naming convention

saveRDS(am.d,file = paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                         "hyfin_durations_gamfelt_annual_maxima.rds"))

