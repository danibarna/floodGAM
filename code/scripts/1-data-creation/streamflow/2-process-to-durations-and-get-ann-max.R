##
##
##
##
##
##
##
## Get annual maxima at each duration for gamfelt dataset
## 
## and two supplementary datasets: gamfelt.hydagsupplement (20 yrs findata + hydag)
## and gamfelt.10.hydagsupplement (10 years findata + hydag, 20 years minimum total)
## -----------------------------------------------------------------------------

library(data.table)
library(lubridate)

source(paste0("~/floodGAM/code/functions/","fn_durations_streamflow.R"))

dvec = c(1, 6, 12, 18, 24, 36, 48, 72) #durations (hours)



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
minfin.hydag <- readRDS(paste0("~/floodGAM/data/cleaned-data/",
                                 "cleaned-minfin-hydag.rds"))

## make sure only the gamfelt stations are included:
gamfelt.hydag <- minfin.hydag[ID%in%unique(gamfelt.1hr$ID)]

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





# minfin.hydagsupplement ----------------------------------------------

## this dataset is every station that has at least 10 years findata in
## hyfin_complete and at least 20 years total when supplemented with hydag data.

## this adds 23 stations. (worth it...?)
minfin.hydag[,uniqueN(ID)] - amd.gamfelt.hydagsupplement[,uniqueN(ID)]

## ---- load the cleaned hyfin_complete data
cleaned.hyfinc <- readRDS(paste0("~/floodGAM/data/cleaned-data/",
                                 "cleaned-NIFS-A2-hyfincomplete.rds"))

## ---- load the record length data table
recordlen <- readRDS(paste0("~/floodGAM/data/cleaned-data/",
                            "record_length_data_table.rds"))

## fix some naming conventions
cleaned.hyfinc <- cleaned.hyfinc[,c("date","cumecs","ID","yk")]
setnames(cleaned.hyfinc,c("cumecs","yk"),c("Qm3_s","year_key"))

## select stations with at least 10 years findata (minfin set)
cleaned.hyfinc <- cleaned.hyfinc[ID %in% recordlen[N.hfc>=10,get("ID")]]

## find annual max for the stations and years you haven't already found it for
keyTab <- amd.gamfelt.hydagsupplement[d==1]

setkey(keyTab,ID,year_key); setkey(cleaned.hyfinc,ID,year_key)

cleaned.hyfinc <- cleaned.hyfinc[!.(keyTab)]

## find d - annual max for those 23 stations, hyfinc data:
amd.supp.hyfinc <- createdurations(cleaned.hyfinc,dvec)
setnames(amd.supp.hyfinc,"sQm3_s","Qm3_s")

## cut minfin.hydag down to those 23 stations and find the ann max for those too. 
minfin.hydag <- minfin.hydag[ID %in% unique(amd.supp.hyfinc$ID)]
## but not the years already in hyfinc:
setkey(amd.supp.hyfinc,ID,year_key)
setkey(minfin.hydag,ID,year_key)

minfin.hydag <- minfin.hydag[!.(amd.supp.hyfinc)]

## find d - annual max for those minfin hydag stations:
amd.supp.hydag <- createdurations(minfin.hydag,dvec)
setnames(amd.supp.hydag,"sQm3_s","Qm3_s")

## stitch together gamfelt.supplement, amd.supp.hydag and amd.supp.hyfinc:
amd.supp.hyfinc[,tag:="hyfinc"]
amd.supp.hydag[,tag:="hydag"]

amd.minfin.supp <- rbind(amd.supp.hydag,amd.supp.hyfinc)

minfin.hydagsupplement <- rbind(amd.gamfelt.hydagsupplement,amd.minfin.supp)

## ---------- save the annual maxima at each duration for the 
## minfin.hydagsupplement data set 
saveRDS(
  minfin.hydagsupplement,
  file = paste0("~/floodGAM/data/processed-data/",
                "gamfelt-durations/",
                "minfin_hydagsupplement_durations_annual_maxima.rds"))










