##
##
##
##
## Create lescon_var script for calling NIFS stations
## uses Table 5 in report 13-2015 
## 'Nasjonalt formelverk for flomberegning i små nedbørfelt'
## 
## Compare to table A2 from report 2016:85
## -----------------------------------------------------------------------------

library(data.table)
library(stringr)

## read in the table
nifs.tab <- fread("~/floodGAM/data/raw-data/NIFS_stations_table_5.txt",
                  fill=T, colClasses = list(character=1:20))

## first column is station numbers. We want the station numbers to create
## the lescon_var executable:
nifs.tab[, c("RN", "HN") := tstrsplit(V1, ".", fixed=TRUE)]


## cross-check with Table A2. How many of these stations do we already have?
tabellA2 <- data.table::fread(paste0("~/floodGAM/data/raw-data/",
                                     "table_A2_report_2016-85.csv"),
                              colClasses = list(character=1))
tabellA2[, c("RN", "HN") := tstrsplit(Nr, ".", fixed=TRUE)]


iA2 <- merge(tabellA2[,c("RN","HN")],nifs.tab[,c("RN","HN")],by=c("RN","HN"))
dim(iA2)
# so we already have 95 of the nifs stations in the original lescon_var call.

setkey(iA2,RN,HN)
setkey(nifs.tab,RN,HN)

# these are the stations we do not already call:
these <- nifs.tab[!iA2]









