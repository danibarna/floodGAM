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

dvec = c(1, 6, 12, 18, 24, 36, 48) #durations (hours)

am.d <- createdurations(data35,dvec)

# a few lines of messy code to make into true long format:
tt <- am.d[d==1][,d:=0][,sQm3_s:=NULL]
am.d[,Qm3_s:=NULL]; setnames(am.d,"sQm3_s","Qm3_s")
am.d <- rbind(tt,am.d)

saveRDS(am.d,file = paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                         "durations_gamfelt_annual_maxima.rds"))


