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


setkey(nifs.tab,RN,HN); setkey(A2.tab,RN,HN)
setkey(A3.tab,RN,HN); setkey(sma.tab,RN,HN)


# in which report? --------------------------------------------------------

tab <- merge(nifs.tab[,c("RN","HN","navn","NIFS")],
             A2.tab[,c("RN","HN","A2","A2_version","Findata_N")],all=T)

tab <- merge(tab,sma.tab[,c("RN","HN","SMA")],all=T)


tab <- merge(tab,A3.tab[,c("RN","HN","A3")],all=T)



## any stations that shouldn't be in table A2?

tab[!is.na(A2) & !is.na(A3)] ## Talgøyfoss. should be in A3

tab[!is.na(A2) & !is.na(A3), A2:=NA]

## what NIFS stations ended up in table A3?
tab[!is.na(NIFS) & !is.na(A3)]

## so 48 stations (30% of the NIFS data) that were used to fit NIFS ended up  
## being excluded from the 2016 analysis.
48/dim(tab[!is.na(NIFS)])[1]

## what NIFS stations are *not* in table A2 or A3?
tab[!is.na(NIFS) & is.na(A2) & is.na(A3)]
## so there's 11 stations we can potentially add.

## why weren't they included in 2016?



# create lescon_var commands ----------------------------------------------

## set a working directory. R will write the text file to this directory.
setwd("~/floodGAM/data/raw-data/")


## create commands for stations in NIFS and Table A2. Use version number from
## Table A2. 

lescon <- tab[(!is.na(NIFS) | !is.na(A2)) & is.na(A3)]

## if version num doesn't exist, use version 1
lescon[,version:=A2_version]
lescon[is.na(A2_version),version:=1]

## archive 39 (hyfin completed)
## name the executable lescon_var file and open the connection
f <- file("nifs-a2-39.txt", open="wb")

## write text to file using cat command
cat("# !/bin/bash",file=f,append=F,sep="\n")
for(i in 1:dim(lescon)[1]){
  cat(paste0("lescon_var -b 0 -f timevalue 39 ",
             lescon$RN[i], " ", lescon$HN[i],
             " 0 1001 ",lescon$version[i]," > ",
             lescon$RN[i], "-", lescon$HN[i],".txt"),
      file=f,append=T,sep="\n")
}

## close the connection
close(f)


## for the files that fail to pull data: it could be the version number, or it could be that no data exists

## after pull data:
##   - right now, hyfin_complete pulls mostly from hykvalp-icecorr. This means there's 
##     still gaps in data, etc. 
##
##   - so: check for years w < 200 days data
##         and exclude utelatt years from 2016 report
##
##   - then: count number of 'FFA-valid' years
##   


# What stations failed the command? ---------------------------------------

## list the files in the directory you downloaded the data files into
alltxtfiles <- list.files(path="C:/data-tab/NIFS-A2/", pattern = ".txt") 

## check for empty files:
idx <- which(file.info(path=paste0("C:/data-tab/NIFS-A2/",alltxtfiles))$size == 0)

## 162 failed the command.
## these stations either (i) do not exist in hyfin_complete 
## or (ii) need another version num.
emptyID <- substr(alltxtfiles[idx],1,nchar(alltxtfiles[idx])-4)

## are they NIFS or A2 stations?
lescon[,ID:=paste0(RN,"-",HN)]

lescon[ID%in%emptyID&!is.na(NIFS)]
# 1 NIFS station, 3 NIFS+A2 stations, 158 A2 stations

# are any listed as findata stations in A2?
A2.tab[,ID:=paste0(RN,"-",HN)]
A2.tab[ID%in%emptyID & !is.na(Findata_N)]
## - yes. 17 stations. 

## - SUMMARY: 162 stations failed command. 161 come from A2, 1 from only NIFS
##            Of the 161 from A2, 17 were marked as findata stations
##            No info for the 1 NIFS station.
##
##   TO DO: look into why these 17 findata stations from table A2 failed command. 


# Load in the stations ----------------------------------------------------

## remove command file
fileloop <- alltxtfiles[-c(idx,which(alltxtfiles=="nifs-a2-39.txt"))]

## initialize data table to store data
data <- data.table(date=POSIXct(),cumecs=numeric(),ID=character())

for(fl in floop){
  
  station.data <- fread(paste0("C:/data-tab/NIFS-A2/",fl))
  setnames(station.data,c("V1","V2"),c("date","cumecs"))
  ## --- remove negative Data values ---
  station.data <- station.data[cumecs>=0]
  ## --- convert to date object with lubridate ----
  # use UTC because CET or "Europe/Oslo" in R has 
  # the wrong daylight savings times for Norway so
  # four or five dates fail to parse
  tryCatch(station.data <- station.data[,
                                        ("date"):=lapply(.SD,ymd_hm,tz="UTC"),
                                        .SDcols = "date"],
           warning = function(w) {
             ## some of the limnigraph dates have hour-minute-second format:
             station.data <- station.data[,
                                          ("date"):=lapply(.SD,ymd_hms,tz="UTC"),
                                          .SDcols = "date"]
           })
  station.data[,ID:=substr(fl,1,nchar(fl)-4)]
  data <- rbind(data,station.data)
} # end file loop


data[,yk:=year(date)]
setkey(data,ID,yk)

saveRDS(data,file="~/floodGAM/data/raw-data/NIFS-A2-raw-hyfinc.rds")

# Remove years identified as utelatt, A2 ----------------------------------

expandutelatt <- function(x){
  out <- lapply(strsplit(x, ", "), 
                function(x) do.call(c, 
                           lapply(strsplit(x, ":"), 
                           function(y) Reduce(`:`, as.numeric(y)))))
  out <- list(as.numeric(unlist(out)))
  return(out)
}

utelatt <- A2.tab[,
                   lapply(.SD,expandutelatt),
                   by=ID,
                   .SDcols="Findata_Utelatt"]

utelatt <- utelatt[, .(yk = unlist(Findata_Utelatt)), by = ID]

setkey(utelatt,ID,yk)

# define column to filter out discarded values
data[,discard:=FALSE]

# discard all utelatt data:
# subset all rows of data using key columns from utelatt where first key 
# matches ID and second key matches year. Return the 'discard' column
# and set discard value = T for utelatt subset. Then, use chaining 
# to select only rows that have discard values = F. 
data <- data[.(utelatt), discard := TRUE][discard == FALSE]


# Remove years with < 200 days --------------------------------------------

# create month column, day column and search for
# unique tuples:
keyGenerator <- function(x){list(month(x),day(x))}

# add month and day
data[,c("mk","dk"):=sapply(.SD,keyGenerator),.SDcols = "date"]

# count number of days in a year (unique tuples of month key and
# day key grouped by year)
data[,numdays:=uniqueN(.SD),by=list(ID,yk),.SDcols=c("mk","dk")]

# find the ID-yk (station-year) tuples that have less than 200 days of data
discard.200 <- data[numdays<200,unique(.SD),.SDcols=c("ID","yk")]

setkey(discard.200,ID,yk)

data <- data[.(discard.200), discard := TRUE][discard == FALSE]


# How many years of data does each station have now? ----------------------

# what is the record length?
recordlen <- data[,.(N.hfc=uniqueN(.SD)),by=ID,.SDcols = "yk"]

# what is the first and last year recorded?
recordlen <- merge(recordlen,data[,.(start.hfc=min(.SD)),by=ID,.SDcols = "yk"],
                   by="ID")
recordlen <- merge(recordlen,data[,.(slutt.hfc=max(.SD)),by=ID,.SDcols = "yk"],
                   by="ID")

ffastations <- recordlen[N.hfc>=20]

# what report are these stations from? 
ffaDT <- merge(ffastations,
               lescon[,c("ID","navn","NIFS","A2","Findata_N","SMA")],by="ID")

ffaDT <- merge(ffaDT,A2.tab[,c("ID","Findata_Start","Findata_Slutt")],by="ID",all.x=T)

ffaDT[!is.na(NIFS)]
## 81 stations from NIFS & A2, 203 just in A2, 1 just in NIFS (46.7, Brakhaug)

## are there any ffa stations that were not previously identified as findata
## stations in table A2?
# That is, stations that survived the 200 days check, have data in hyfin_complete, 
# but were *not* in findata A2:

ffaDT[is.na(Findata_N)]
## yep, 48 of them. 

## how many are listed as 'nedlagt-regulert' in A2?

ffaDT[,Ned:="A"]

nedlagt <- c("R","R","R","R","R",
  "R","R","R","R","R",
  "R","R","R","R","R",
  "R","R","R","R","R",
  "R","R","C","A-N","R",
  "R","R","A-S","R","R",
  "R","C","R","R","R",
  "R","R","R","nifs","R",
  "R","R","R","R","R",
  "R","R","R")

## nearly all are listed as nedlagt-regulert

ffaDT[is.na(Findata_N),Ned:=nedlagt]

## how many with over 20 years data and NOT regulated:
ffaDT[Ned!="R"] #242

ffaDT <- ffaDT[Ned!="R"]

## how many were in the små vassdrags report?
ffaDT[!is.na(SMA)] # 89


# use ffaDT to select years and stations ----------------------------------

hyfinc <- data[ID%in%ffaDT$ID]

saveRDS(hyfinc,file="~/floodGAM/data/cleaned-data/NIFS-A2-hyfincomplete.rds")



