##
##
##
##
##
## make table comparing stations from:
## - små vassdrags rapport https://publikasjoner.nve.no/rapport/2013/rapport2013_66.pdf
## - NIFS https://publikasjoner.nve.no/rapport/2015/rapport2015_13.pdf
## - RFFA https://publikasjoner.nve.no/rapport/2016/rapport2016_85.pdf
##
## -----------------------------------------------------------------------------

library(data.table)
library(lubridate)


nifs.tab <- fread("~/floodGAM/data/raw-data/NIFS_stations_table_5.txt",
                  fill=T, colClasses = list(character=1))
nifs.tab[, c("RN", "HN") := tstrsplit(V1, ".", fixed=TRUE)]
setnames(nifs.tab,"V2","navn")
nifs.tab <- nifs.tab[,c("navn","RN","HN")]
nifs.tab[,NIFS:="nifs"]

A2.tab <- fread("~/floodGAM/data/raw-data/tabell_for_rapport2016_forenkelt_v.txt",
                fill=T, colClasses = list(character=1))
A2.tab[, c("RN", "HN") := tstrsplit(Nr, ".", fixed=TRUE)]
A2.tab[,A2:="A2"]


A3.tab <- fread("~/floodGAM/data/raw-data/rapport2016_85_tabell_A3_list_over_utelatt_fra_flomdatasett.txt",
                fill=T, colClasses = list(character=1:2))
A3.tab[, key_ := do.call(paste, c(.SD, sep = " ")), .SDcols = names(A3.tab)]
A3.tab <- A3.tab[,c("V1","key_")]
A3.tab[, c("RN", "HN") := tstrsplit(V1, ".", fixed=TRUE)]
A3.tab[,A3:="A3"]
A3.tab <- A3.tab[,c("key_","RN","HN","A3")]



sma.tab <- fread("~/floodGAM/data/raw-data/rapport2013_66_tabell_1.txt",
                 fill=T, colClasses = list(character=1))
sma.tab[,c("RN","HN") := tstrsplit( 
  sma.tab[, tstrsplit(V1, " ", fixed=TRUE)]$V1, ".", fixed=TRUE)]
sma.tab <- sma.tab[,c("RN","HN")]
sma.tab[,SMA:="sma"]

# load in table from Anja
d1 <- fread("~/floodGAM/data/raw-data/d1.csv")
setnames(d1,c("regine_area","main_no"),c("RN","HN"))
d1[,RN:=as.character(RN)]; d1[,HN:=as.character(HN)]
d1[,d1:="d1"]

d1 <- unique(d1,by=c("RN","HN"))
setkey(d1,RN,HN)

setkey(nifs.tab,RN,HN); setkey(A2.tab,RN,HN)
setkey(A3.tab,RN,HN); setkey(sma.tab,RN,HN)


# in which report? --------------------------------------------------------

tab <- merge(nifs.tab[,c("RN","HN","navn","NIFS")],
             A2.tab[,c("RN","HN","A2")],all=T)

tab <- merge(tab,sma.tab[,c("RN","HN","SMA")],all=T)


tab <- merge(tab,A3.tab[,c("RN","HN","A3")],all=T)

## what tab is in d1?

tab <- merge(tab,d1[,c("RN","HN","d1")],all.x=T)



# create lescon_var commands ----------------------------------------------


