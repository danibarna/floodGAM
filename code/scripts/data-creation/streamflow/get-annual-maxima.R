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

## ---define paths
dataPath <- paste0("~/ClimDesign_PhD/XGBoost-GAM index flood model",
                   "/src/","dataset_construction/","publishable/") 
# ^this is not saved in github because it's too big

load(paste0(dataPath,"cleaned_archive35.rda"))

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

am.d <- createdurations(data35,dvec)
setnames(am.d,"sQm3_s","Qm3_s") # fix naming convention

# bind raw data (no interpolaation, no smoothing, d=0):
am[,d:=0]
am.d <- rbind(am[,c("year_key","decimaldate","Qm3_s","d","ID")],am.d)

saveRDS(am.d,file = paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                         "durations_gamfelt_annual_maxima.rds"))


