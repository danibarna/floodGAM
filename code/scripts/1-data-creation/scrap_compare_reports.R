##
##
##
##
##
##


library(data.table)


nifs.tab <- fread("~/floodGAM/data/raw-data/NIFS_stations_table_5.txt",
                  fill=T, colClasses = list(character=1))
nifs.tab[, c("RN", "HN") := tstrsplit(V1, ".", fixed=TRUE)]
setnames(nifs.tab,"V2","navn")
nifs.tab <- nifs.tab[,c("navn","RN","HN")]

A2.tab <- fread("~/floodGAM/data/raw-data/rapport2016_85_tabell_A2.txt",
                  fill=T, colClasses = list(character=1))
A2.tab[, c("RN", "HN") := tstrsplit(V1, ".", fixed=TRUE)]
setnames(A2.tab,"V3","navn")
A2.tab <- A2.tab[,c("navn","RN","HN")]


A3.tab <- fread("~/floodGAM/data/raw-data/rapport2016_85_tabell_A3_list_over_utelatt_fra_flomdatasett.txt",
                  fill=T, colClasses = list(character=1))
A3.tab[, c("RN", "HN") := tstrsplit(V1, ".", fixed=TRUE)]
setnames(A3.tab,"V2","navn")
A3.tab <- A3.tab[,c("navn","RN","HN")]


sma.tab <- fread("~/floodGAM/data/raw-data/rapport2013_66_tabell_1.txt",
                          fill=T, colClasses = list(character=1))
sma.tab[,c("RN","HN") := tstrsplit( sma.tab[, tstrsplit(V1, " ", fixed=TRUE)]$V1, ".", fixed=TRUE)]
sma.tab <- sma.tab[,c("RN","HN")]

setkey(nifs.tab,RN,HN); setkey(A2.tab,RN,HN); setkey(A3.tab,RN,HN); setkey(sma.tab,RN,HN)





## 100 stations are in both nifs and A2:
merge(nifs.tab,A2.tab)

## Table A3 says to exclude 48 of the nifs stations:
merge(nifs.tab,A3.tab)

##...11 stations are in nifs but not A2 or A3
nifs.tab[!rbind(merge(nifs.tab,A2.tab),merge(nifs.tab,A3.tab))]


## all nifs stations are in sma.tab (good)
merge(sma.tab,nifs.tab)


## 102 of the stations in sma.tab are recommended excluded by A3
merge(sma.tab,A3.tab)





















