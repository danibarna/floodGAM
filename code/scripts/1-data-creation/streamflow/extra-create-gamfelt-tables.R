##
##
##
##
##
##
## create tables for gamfelt dataset:
##   stations + years
##   stations
## -----------------------------------------------------------------------------

library(data.table)
library(lubridate)

gamfelt <- readRDS(paste0("~/floodGAM/data/processed-data/",
                   "gamfelt/",
                   "gamfelt_1hour_annual_maxima.rds"))

gamfelt.tab <- gamfelt[,c("year_key","ID")]

setnames(gamfelt.tab,"year_key","year")



gamfelt.rl <- gamfelt.tab[,.N,by="ID"]

setnames(gamfelt.rl,"N","record_length_years")



fwrite(gamfelt.tab,file=paste0("~/floodGAM/data/processed-data/",
                               "gamfelt/",
                               "gamfelt_year-station_table.csv"))

fwrite(gamfelt.rl,file=paste0("~/floodGAM/data/processed-data/",
                               "gamfelt/",
                               "gamfelt_record-length_table.csv"))



