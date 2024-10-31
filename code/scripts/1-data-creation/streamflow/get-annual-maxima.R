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

load(paste0("~/floodGAM/data/cleaned-data/","cleaned_archive39.rda"))

# find the index of the annual maxima
idx <- data35[, .I[which.max(Qm3_s)], by=c("ID","year_key")]$V1

# select the annual maxima 
am <- data35[idx,]

saveRDS(am,file = paste0("~/floodGAM/data/processed-data/gamfelt/",
                         "gamfelt_annual_maxima.rds"))

fwrite(am,file = paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_annual_maxima.csv"))



# Process the data into different durations -------------------------------

source(paste0("~/floodGAM/code/functions/","fn_durations_streamflow.R"))

dvec = c(1, 6, 12, 18, 24, 36, 48, 72, 168, 336, 720) #durations (hours)

am.d <- createdurations(data39,dvec)
setnames(am.d,"sQm3_s","Qm3_s") # fix naming convention

# bind raw data (no interpolaation, no smoothing, d=0):
am[,d:=0]
am.d <- rbind(am[,c("year_key","decimaldate","Qm3_s","d","ID")],am.d)

saveRDS(am.d,file = paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                         "hyfin_durations_gamfelt_annual_maxima.rds"))


# Mark hykval - hyfin annual maxima ---------------------------------------

## requires both hyfin_durations_gamfelt_annual_maxima.rds and 
## hykval_durations_gamfelt_annual_maxima.rds to exist already

hyfin <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                        "hyfin_durations_gamfelt_annual_maxima.rds"))

hykval <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                        "hykval_durations_gamfelt_annual_maxima.rds"))
setkey(hyfin,ID,d,year_key)
setkey(hykval,ID,d,year_key)

## outer join hyfin and hykval
gf <- rbind(hyfin[hykval[,.(ID,d,year_key,Qm3_s,decimaldate)],
                  on=.(ID,d,year_key),nomatch = NA],
            hyfin[!hykval],fill=T)

## when hyfin is missing an annual max, use the hykval ann max.
## indicate which archive the ann max comes from.
## this is probably a better data table way to do this. 
gf[, rowpos := .I]
gf[, c("archive","Qm3_s","decimaldate") := list(ifelse(is.na(Qm3_s)==T,37,39),
                                                ifelse(is.na(Qm3_s)==T,
                                                       i.Qm3_s,
                                                       Qm3_s),
                                                ifelse(is.na(decimaldate)==T,
                                                       i.decimaldate,
                                                       decimaldate)),
   by=rowpos]


gf <- gf[,.(ID,year_key,Qm3_s,decimaldate,d,archive)]

saveRDS(gf, paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                   "durations_gamfelt_annual_maxima.rds"))
