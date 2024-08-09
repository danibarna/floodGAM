##
##
##
## -----------------------------------------------------------------------------

library(data.table)
library(lubridate)

## ---define paths
dataPath <- paste0("~/ClimDesign_PhD/XGBoost-GAM index flood model",
                   "/src/","dataset_construction/","publishable/") # zenodo download

load(paste0(dataPath,"cleaned_archive35.rda"))
load(paste0("~/floodGAM/data/processed-data/gamfelt/README_files/",
            "lescon_var_writing_variables.rda"))


## find the start and end of the daily and findata periods for each station

# mark the findata years:
numdayfin <- data35[,median(gapmin),by=c("year_key","month_key","day_key","ID")]
numdayfin <- numdayfin[,sum(V1<1440),by=c("ID","year_key")]
setnames(numdayfin,"V1","nfin.yk")

numdayfin[,isfin:=ifelse(nfin.yk>=200,1,0)]



tt <- numdayfin[,min(year_key),by=c("ID","isfin")]
tt <- reshape(tt, idvar = "ID", timevar = "isfin", direction = "wide")
setnames(tt,c("V1.0","V1.1"),c("daily_data_start","fine_data_start"))


tv <- numdayfin[,max(year_key),by=c("ID","isfin")]
tv <- reshape(tv, idvar = "ID", timevar = "isfin", direction = "wide")
setnames(tv,c("V1.0","V1.1"),c("daily_data_end","fine_data_end"))

td <- merge(tt,tv,by="ID")


# num of year of both daily and fine data
numyrsfin <- merge(numdayfin[nfin.yk>199, .N, by="ID"][,c("ID","N")],
                   numdayfin[, .N, by="ID"][,"ID"],
                   all.y=T)
setnafill(numyrsfin,cols="N",fill=0)
setnames(numyrsfin,"N","N_fine_data")

numyrsday <- merge(numdayfin[nfin.yk<=199, .N, by="ID"][,c("ID","N")],
                   numdayfin[, .N, by="ID"][,"ID"],
                   all.y=T)
setnafill(numyrsday,cols="N",fill=0)
setnames(numyrsday,"N","N_daily_data")

td <- merge(td,numyrsfin,by="ID")
td <- merge(td,numyrsday,by="ID")

# total years of data
tg <- numdayfin[,.N,by="ID"]

td <- merge(td,tg,by="ID")


## add utelatt years as character string
utelatt <- readRDS(paste0("~/floodGAM/data/raw-data/","utelatt.rds"))

utelatt <- utelatt[, lapply(.SD, paste0, collapse=", "), by = "ID"]
utelatt <- utelatt[,c("ID","yk")]
setnames(utelatt,"yk","utelatt_yrs")

td <- merge(td,utelatt,by="ID",all.x = T)

# add version number, RN, HN

myNR <- unique(Nrlescon[,c("ID","RN","HN","version")])

td <- merge(td,myNR,by="ID")


# order columns
td <- td[,c("RN","HN","version",
      "daily_data_start","daily_data_end","N_daily_data",
      "fine_data_start","fine_data_end","N_fine_data",
      "N","utelatt_yrs")]


fwrite(td,file = paste0("~/floodGAM/data/processed-data/gamfelt/README_files/",
                        "gamfelt_descriptive_table.csv"))






