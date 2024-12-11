##
##
##
##
## Station selection is based off of report 2016-85 'Utvalg og kvalitetssikring 
## av flomdata for flomfrekvensanalyser' and report 62-2014 'Nasjonalt formelverk 
## for flomberegning i små nedbørfelt'
## 
## 
## INPUT FILES: 
##   - tabell_A2_rapport_2016-85_daba_edited.txt
##   - tabell_5_NIFS_rapport_2015-13.txt (copy-pasted version of tabell 5, vedlegg 1)
##   - tabell_A3_rapport_2016-85.txt
##       (copy-pasted from 2016-85 report)
## 
## -----------------------------------------------------------------------------

library(data.table)
library(lubridate)


# load in the data --------------------------------------------------------

nifs.tab <- fread(paste0("~/floodGAM/data/raw-data/",
                         "tabell_5_NIFS_rapport_2015-13.txt"),
                  fill=T, colClasses = list(character=1))

A2.tab <- fread(paste0("~/floodGAM/data/raw-data/",
                       "tabell_A2_rapport_2016-85_daba_edited.txt"),
                fill=T, colClasses = list(character=1))


A3.tab <- fread(paste0("~/floodGAM/data/raw-data/",
                       "tabell_A3_rapport_2016-85.txt"),
                fill=T, colClasses = list(character=1:2))

# Preliminary data cleaning -----------------------------------------------

## fix column names and set indicator columns; set keys

nifs.tab[, c("RN", "HN") := tstrsplit(V1, ".", fixed=TRUE)]
setnames(nifs.tab,"V2","navn")
nifs.tab <- nifs.tab[,c("navn","RN","HN")]
nifs.tab[,NIFS:="nifs"]

A2.tab <- A2.tab[-1,]
A2.tab[, c("RN", "HN") := tstrsplit(Nr, ".", fixed=TRUE)]
A2.tab[,A2:="A2"]

A3.tab[, key_ := do.call(paste, c(.SD, sep = " ")), .SDcols = names(A3.tab)]
A3.tab <- A3.tab[,c("V1","key_")]
A3.tab[, c("RN", "HN") := tstrsplit(V1, ".", fixed=TRUE)]
A3.tab[,A3:="A3"]
A3.tab <- A3.tab[,c("key_","RN","HN","A3")]

setkey(nifs.tab,RN,HN); setkey(A2.tab,RN,HN); setkey(A3.tab,RN,HN)

# Merge the data tables ---------------------------------------------------

tab <- merge(nifs.tab[,c("RN","HN","NIFS")],
             A2.tab[,c("RN","HN","A2","Navn","version",
                       "Findata_N","Nedlagtregulert")],all=T)

tab <- merge(tab,A3.tab[,c("RN","HN","A3")],all=T)


# Sanity checks -----------------------------------------------------------

## no station should be in both table A2 and table A3:

tab[!is.na(A2) & !is.na(A3)] ## but there is one: 2.481 (Jora v/Nysetra). 
## this is a typo in 2016-85 I think.

## fix this (put station in only A3)
tab[!is.na(A2) & !is.na(A3), A2:=NA]



# Compare NIFS to A2 and A3 -----------------------------------------------

## what NIFS stations ended up in table A3?
tab[!is.na(NIFS) & !is.na(A3)]

## so 48 stations (30% of the NIFS data) that were used to fit NIFS ended up  
## being excluded from the 2016 analysis because the data quality was poor.
48/dim(tab[!is.na(NIFS)])[1]

## what NIFS stations are *not* in table A2 or A3?
tab[!is.na(NIFS) & is.na(A2) & is.na(A3)]

## so there's 11 stations that were used to fit NIFS but not mentioned 
## at all in 2016-85.



# create lescon_var commands ----------------------------------------------

## set a working directory. R will write the text file to this directory.
setwd("~/floodGAM/data/raw-data/")

## create commands for stations in NIFS and Table A2. Use version number from
## Table A2 - daba edited. See notes in raw-data/README. Do not use stations
## marked as nedlagt-regulert

lescon <- tab[(!is.na(NIFS) | !is.na(A2)) & is.na(A3)]
lescon[,ID:=paste0(RN,"-",HN)]

lescon <- lescon[!ID%in%lescon[Nedlagtregulert=="R",get("ID")]]

## for the 11 stations in NIFS but not A2 the version number is chosen from Hysopp
lescon[,version:=ifelse(is.na(version),1,version)]

## station 160.6 needs version 2 (according to Hysopp)
lescon[,version:=ifelse(ID=="160-6",2,version)]

## pull from archive 39 (hyfin completed)
## name the executable lescon_var file and open the connection
f <- file("lesconvar_commands_NIFS-A2_archive_39-hyfincomplete.txt", open="wb")

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


## -----------------------------------------------------------------------------
## *****************************************************************************
## go to smarTTY and use the commands in txt file 
## "lesconvar_commands_NIFS-A2_archive_39-hyfincomplete.txt" to pull data.
## see data/how-to-guides/ for instructions
## download data to a local folder on your machine. 
## *****************************************************************************
## -----------------------------------------------------------------------------


# What stations failed the command? ---------------------------------------

## list the files in the directory you downloaded the data files into
alltxtfiles <- list.files(path="C:/data-tab/NIFS-A2/", pattern = ".txt") 

## check for empty files:
idx <- which(file.info(path=paste0("C:/data-tab/NIFS-A2/",alltxtfiles))$size == 0)

## 143 failed the command.
## these stations either (i) do not exist in hyfin_complete 
## or (ii) need another version num.
emptyID <- substr(alltxtfiles[idx],1,nchar(alltxtfiles[idx])-4)

## are they NIFS or A2 stations?
lescon[ID%in%emptyID&is.na(NIFS)&is.na(Findata_N)]
# the stations that are failing the command to hyfin_complete are all A2 stations
# listed as having daily data (this is good, means things working as they should,
# daily data not in hyfin)

# are any listed as findata stations in A2?
A2.tab[,ID:=paste0(RN,"-",HN)]
A2.tab[ID%in%emptyID & !is.na(Findata_N)]$ID ## no


# Load in the stations ----------------------------------------------------

## remove command file
fileloop <- alltxtfiles[-c(idx,
                           which(alltxtfiles==paste0("lesconvar_commands_",
                                                     "NIFS-A2_archive_39-hyfincomplete.txt")))]

## initialize data table to store data
data <- data.table(date=POSIXct(),cumecs=numeric(),ID=character())

for(fl in fileloop){
  
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

saveRDS(data,file="~/floodGAM/data/raw-data/raw-NIFS-A2-hyfincomplete.rds")



# Remove years identified as utelatt, table A2 -------------------------------

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
# unique combinations:
keyGenerator <- function(x){list(month(x),day(x))}

# add month and day
data[,c("mk","dk"):=sapply(.SD,keyGenerator),.SDcols = "date"]

# count number of days in a year (unique combinations of month key and
# day key grouped by year)
data[,numdays:=uniqueN(.SD),by=list(ID,yk),.SDcols=c("mk","dk")]

# find the ID-yk (station-year) combinations that have less than 200 days of data
discard.200 <- data[numdays<200,unique(.SD),.SDcols=c("ID","yk")]

setkey(discard.200,ID,yk)

data <- data[.(discard.200), discard := TRUE][discard == FALSE]

saveRDS(data,file="~/floodGAM/data/cleaned-data/cleaned-NIFS-A2-hyfincomplete.rds")

# How many years of data does each station have now? ----------------------

# what is the record length?
recordlen <- data[,.(N.hfc=uniqueN(.SD)),by=ID,.SDcols = "yk"]
# 'N.hfc' = number of years in hyfin complete, not utelatt, with 200+ days of data per year 

# what is the first and last year recorded?
recordlen <- merge(recordlen,data[,.(start.hfc=min(.SD)),by=ID,.SDcols = "yk"],
                   by="ID")
recordlen <- merge(recordlen,data[,.(slutt.hfc=max(.SD)),by=ID,.SDcols = "yk"],
                   by="ID")

# what stations have 20 years or more finedata?
fin.stations <- recordlen[N.hfc>=20]

# what report are these stations from? 
fin.stations <- merge(fin.stations,
               lescon[,c("ID","Navn","NIFS","A2","Findata_N")],by="ID")

fin.stations[!is.na(NIFS)]
## 1 station just in NIFS (46.7, Brakhaug)


# Select years and stations ----------------------------------

# select years that are (i) not marked as utelatt in Table A2, with 
# (ii) 20 years of data that have at least 200 days of data per year

gamfelt.hyfinc <- data[ID%in%fin.stations$ID]

gamfelt.hyfinc <- gamfelt.hyfinc[,c("date","cumecs","ID","yk")]
setnames(gamfelt.hyfinc,c("cumecs","yk"),c("Qm3_s","year_key"))

saveRDS(gamfelt.hyfinc,
        file="~/floodGAM/data/cleaned-data/gamfelt-NIFS-A2-hyfincomplete.rds")

## also save recordlen

