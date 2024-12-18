##
##
##
##
##
##
##
## Get annual maxima at each duration for gamfelt dataset
## 
## and supplementary dataset: gamfelt.hydagsupplement (20 yrs findata + hydag)
## -----------------------------------------------------------------------------

library(data.table)
library(lubridate)

source(paste0("~/floodGAM/code/functions/","fn_durations_streamflow.R"))

dvec = c(1, 6, 12, 18, 24, 36, 48) #durations (hours)



# gamfelt -----------------------------------------------------------------

## ---- load the gamfelt data
gamfelt.hyfinc <- readRDS(paste0("~/floodGAM/data/cleaned-data/",
                                 "gamfelt-NIFS-A2-hyfincomplete.rds"))

gamfelt.hyfinc[,uniqueN(ID)] # number of stations


## this function takes a little while to run
amd.gamfelt.hyfinc <- createdurations(gamfelt.hyfinc,dvec)

setnames(amd.gamfelt.hyfinc,"sQm3_s","Qm3_s") # fix naming convention

## ---------- save the annual maxima at each duration for the gamfelt data set 
saveRDS(amd.gamfelt.hyfinc,
        file = paste0("~/floodGAM/data/processed-data/",
                                          "gamfelt-durations/",
                         "gamfelt_durations_annual_maxima.rds"))

## ---------- save the 1 hour gamfelt annual maxima
## create a data object that is just the one hour duration
## and save it to its own folder.
## most people will probably just want to 1-hour duration, not the entire
## fleet of durations.

gamfelt.1hr <- amd.gamfelt.hyfinc[d==1]

saveRDS(gamfelt.1hr,
        file = paste0("~/floodGAM/data/processed-data/",
                                          "gamfelt/",
                      "gamfelt_1hour_annual_maxima.rds"))

fwrite(gamfelt.1hr,
       file = paste0("~/floodGAM/data/processed-data/",
                                                "gamfelt/",
                      "gamfelt_1hour_annual_maxima.csv"))





## -----------------------------------------------------------------------------
## all code below here is used to
## create and check supplementary datasets
## -----------------------------------------------------------------------------


# gamfelt.hydagsupplement -------------------------------------------------
## 20 years of fine data, record length extended with hydag records:

## ---- load in the hydag supplement
hydag.supp <- readRDS(paste0("~/floodGAM/data/cleaned-data/",
                                 "gamfelt-NIFS-A2-hydag.rds"))

## make sure only the gamfelt stations are included:
gamfelt.hydag <- hydag.supp[ID%in%unique(amd.gamfelt.hyfinc$ID)]

## keep only years that do not already exist in gamfelt.hyfinc
setkey(gamfelt.1hr,ID,year_key); setkey(gamfelt.hydag,ID,year_key)

gamfelt.hydag <- gamfelt.hydag[!.(gamfelt.1hr)]

## find annual maxima at each duration. Note that this is creating
## 1, 6, 12, and 18 hour data from 24 hour data.
amd.gamfelt.hydag <- createdurations(gamfelt.hydag,dvec)
setnames(amd.gamfelt.hydag,"sQm3_s","Qm3_s") # fix naming convention

## stitch together amd.gamfelt.hyfinc and amd.gamfelt.hydag. Mark which
## archive each year comes from.

amd.gamfelt.hyfinc[,tag:="hyfinc"]
amd.gamfelt.hydag[,tag:="hydag"]

amd.gamfelt.hydagsupp <- rbind(amd.gamfelt.hydag,amd.gamfelt.hyfinc)


# Now check impact of adding daily data to gamfelt records --------------------

mt <- merge(amd.gamfelt.hydagsupp[tag=="hyfinc",
                              .(hyfinc.median=median(Qm3_s)),by=c("d","ID")],
            amd.gamfelt.hydagsupp[,
                              .(blended.median=median(Qm3_s)),by=c("d","ID")],
            by=c("d","ID"))

## look at the percent difference relative to the blended.median (this metric
## is sensitive to underestimation of hyfinc.median, which is what we want to 
## check; if the blended median is much much lower than the hyfinc median,
## we may not want to supplement the record with daily data at that station)

mt[,ape:=abs(hyfinc.median-blended.median)/blended.median*100]

setkey(mt,ape)

mt
## so there are a few stations / durations where there is a larger percent
## difference between pure hyfinc and blended data. 

## are there any instances where blended.median > hyfinc.median?
mt[blended.median>hyfinc.median,] ## yes

## and if we cut out all the ones that have >5% ape?
mtkey <- mt[ape>=5,c("d","ID")]
mtkey[,tag:="hydag"]

setkey(mtkey,ID,d,tag)
setkey(amd.gamfelt.hydagsupp,ID,d,tag)

amd.gamfelt.hydagsupp <- amd.gamfelt.hydagsupp[!.(mtkey)]

## ---------- save the annual maxima at each duration for the 
## gamfelt.hydagsupplement data set 
saveRDS(
  amd.gamfelt.hydagsupp,
  file = paste0("~/floodGAM/data/processed-data/",
                "gamfelt-durations/",
                "gamfelt_hydagsupplement_durations_annual_maxima.rds"))

