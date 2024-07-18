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

load(paste0(dataPath,"cleaned_archive35.rda"))

# find the index of the annual maxima
idx <- data35[, .I[which.max(Qm3_s)], by=c("ID","year_key")]$V1

# select the annual maxima 
am <- data35[idx,]

saveRDS(am,file = paste0("~/floodGAM/data/processed-data/gamfelt/",
                         "gamfelt_annual_maxima.rds"))

fwrite(am,file = paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_annual_maxima.csv"))





