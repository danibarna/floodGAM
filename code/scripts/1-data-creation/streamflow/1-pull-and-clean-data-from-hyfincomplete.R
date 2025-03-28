## daba@nve.no
##
## Purpose of script:
## 1. create database commands for lescon_var (Hydra II database)
## 2. clean the streamflow data obtained with the database commands
##
## Station selection is based off of report 2016-85 'Utvalg og kvalitetssikring 
## av flomdata for flomfrekvensanalyser' and report 13-2015 'Nasjonalt formelverk 
## for flomberegning i små nedbørfelt'
##
## -----
## Notes:
## This script is written to be run step-by-step with user input.
##
## It automatically writes lescon_var command files based off of tables in 
## report 2016-85 and 13-2015.
##
## After the command file is written, the user must leave this R script, use
## the command file in an external application (lescon_var) to download
## the data to their local machine, and then return to this R script to run
## the rest of the data cleaning script with the newly downloaded data. 
##
## -----
##
## INPUT FILES: 
##   - tabell_A2_rapport_2016-85_daba_edited.txt
##   - tabell_5_NIFS_rapport_2015-13.txt (copy-pasted version of tab 5, ved. 1)
##   - tabell_A3_rapport_2016-85.txt
##       (copy-pasted from 2016-85 report)
## 
## OUTPUT FILES:
##   - lesconvar_commands_NIFS-A2_archive_39-hyfincomplete.txt
##   - lesconvar_commands_gamfelt_archive_37-hydag.txt
##
##   several intermediate data objects are saved /data/cleaned-data:
##   - raw-NIFS-A2-hyfincomplete.rds
##   - cleaned-NIFS-A2-hyfincomplete.rds
##   - gamfelt-NIFS-A2-hyfincomplete.rds (this is input to script 2-process-to-durations-and-get-ann-max.R)
##
##   we save also a table with record lengths:
##   - record_length_data_table.rds
## 
## -----------------------------------------------------------------------------

library(data.table)
library(lubridate)
library(ggplot2)


# Define filepaths --------------------------------------------------------

## filepath to the folder you want the lescon_var command files
## to be written to:
cmdpath <- "~/floodGAM/data/raw-data/"

## filepath to the folder with raw hyfin-complete streamflow data:
hyfincpath <- "C:/data-tab/NIFS-A2/"

## filepath to the folder with raw hydag streamflow data:
hydagpath <- "C:/data-tab/NIFS-A2-hydag/"

# Custom functions --------------------------------------------------------

expandutelatt <- function(x){
  ## expands year ranges
  ## for use in Table A2
  ## ---
  ## outputs a list for use as data.table column
  ## --------
  ## x = character vector of years. Can take both comma 
  ## separated (for example: "2012, 2013, 2014")
  ## or colon separated (for example "2012:2014")
  ## ---------------------------------------------
  out <- lapply(strsplit(x, ", "), 
                function(x) do.call(c, 
                                    lapply(strsplit(x, ":"), 
                            function(y) Reduce(`:`, as.numeric(y)))))
  out <- list(as.numeric(unlist(out)))
  return(out)
}


# load in the tables --------------------------------------------------------

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

A2.tab[, c("RN", "HN") := tstrsplit(Nr, ".", fixed=TRUE)]
A2.tab[,A2:="A2"]
A2.tab[,ID:=paste0(RN,"-",HN)]

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


## -----------------------------------------------------------------------------
## *****************************
## Create lescon_Var commands for hyfin_complete
## *****************************
## -----------------------------------------------------------------------------

## set a working directory. R will write the text file to this directory.
setwd(cmdpath)

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


# HYFIN_C: What stations failed the command? -------------------------------

## list the files in the directory you downloaded the data files into
alltxtfiles <- list.files(path=hyfincpath, pattern = ".txt") 

## check for empty files:
idx <- which(file.info(path=paste0(hyfincpath,alltxtfiles))$size == 0)

## 143 failed the command.
## these stations either (i) do not exist in hyfin_complete 
## or (ii) need another version num.
emptyID <- substr(alltxtfiles[idx],1,nchar(alltxtfiles[idx])-4)

## are they NIFS or A2 stations?
lescon[ID%in%emptyID&is.na(NIFS)&is.na(Findata_N)]
# the stations that are failing the command to hyfin_complete are all A2 stations
# listed as having daily data (this is good, means things working as they should,
# daily data not in hyfin_complete)

# are any listed as findata stations in A2?
A2.tab[,ID:=paste0(RN,"-",HN)]
A2.tab[ID%in%emptyID & !is.na(Findata_N)]$ID ## no


# HYFIN_C: Load in the stations ------------------------------------------

## remove command file
fileloop <- alltxtfiles[-c(idx,
                           which(alltxtfiles==paste0("lesconvar_commands_",
                                                     "NIFS-A2_archive_39-",
                                                     "hyfincomplete.txt")))]

## initialize data table to store data
data <- data.table(date=POSIXct(),cumecs=numeric(),ID=character())

for(fl in fileloop){
  
  station.data <- fread(paste0(hyfincpath,fl))
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


# HYFIN_C: Remove years identified as utelatt, table A2 -------------------

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


# HYFIN_C: Remove years with < 244 days ------------------------------------


# create month column, day column and search for
# unique combinations:
keyGenerator <- function(x){list(month(x),day(x))}

# add month and day
data[,c("mk","dk"):=sapply(.SD,keyGenerator),.SDcols = "date"]

# count number of days in a year (unique combinations of month key and
# day key grouped by year)
data[,numdays:=uniqueN(.SD),by=list(ID,yk),.SDcols=c("mk","dk")]

# find the ID-yk (station-year) combinations that have less than 244 days of data
discard.244 <- data[numdays<244,unique(.SD),.SDcols=c("ID","yk")]
setkey(discard.244,ID,yk)

data <- data[.(discard.244), discard := TRUE][discard == FALSE]

saveRDS(data,file="~/floodGAM/data/cleaned-data/cleaned-NIFS-A2-hyfincomplete.rds")



# How many years of data do we have from hyfin_complete? ----------------------

# create record length data table--used to index into larger 'data' object
recordlen <- data[,.(N.hfc=uniqueN(.SD)),by=ID,.SDcols = "yk"]
# 'N.hfc' = number of years in hyfin complete, not utelatt, with 200+ days of 
## data per year 

# add in supplementary info (which report, station name, num findata yrs from A2)
recordlen <- merge(recordlen,
                   lescon[,c("ID","Navn","NIFS","A2","Findata_N","version")],
                   by="ID")


# what stations have 20 years or more finedata?
fin.stations <- recordlen[N.hfc>=20]

data <- data[ID%in%fin.stations$ID]


## these stations, with >20 year findata, are the stations we will
## pull hydag data for and cross-check.


## -----------------------------------------------------------------------------
## *****************************
## Pull data from hydag
## *****************************
## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
## *** Now start archive cross-check criteria. For every station with >20 years
##     findata, also pull data from hydag (archive 37). Discard years with too 
##     much missing data or improper spacing around annual maxima (archive 
##     cross-check criteria) ***
## -----------------------------------------------------------------------------


fin.stations[, c("RN", "HN") := tstrsplit(ID, "-", fixed=TRUE)]


## set a working directory. R will write the text file to this directory.
setwd(cmdpath)

## create commands to pull from hydag for stations in min.findata

## pull from archive 37 (hydag)
## name the executable lescon_var file and open the connection
f <- file("lesconvar_commands_gamfelt_archive_37-hydag.txt", open="wb")

## write text to file using cat command
cat("# !/bin/bash",file=f,append=F,sep="\n")
for(i in 1:dim(fin.stations)[1]){
  cat(paste0("lescon_var -b 0 -f timevalue 37 ",
             fin.stations$RN[i], " ", fin.stations$HN[i],
             " 0 1001 ",fin.stations$version[i]," > ",
             fin.stations$RN[i], "-", fin.stations$HN[i],".txt"),
      file=f,append=T,sep="\n")
}

## close the connection
close(f)

## -----------------------------------------------------------------------------
## *****************************************************************************
## go to smarTTY and use the commands in txt file 
## "lesconvar_commands_min-findata_archive_37-hydag.txt" to pull data.
## see data/how-to-guides/ for instructions
## download data to a local folder on your machine. 
## *****************************************************************************
## -----------------------------------------------------------------------------

# HYDAG: Sanity check: what stations failed the command? ------------------

## list the files in the directory you downloaded the data files into
alltxtfiles <- list.files(path=hydagpath, 
                          pattern = ".txt") 

## check for empty files:
idx <- which(file.info(path=paste0(hydagpath,
                                   alltxtfiles))$size == 0)

## no empty files. 



# HYDAG: Load in the stations ----------------------------------------------

## remove command file
fileloop <- alltxtfiles[-c(idx,
                           which(alltxtfiles==paste0("lesconvar_commands_gamfelt_archive_37-hydag.txt")))]

## initialize data table to store data
hydag <- data.table(date=POSIXct(),cumecs=numeric(),ID=character())

for(fl in fileloop){
  
  station.data <- fread(paste0(hydagpath,fl))
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
  hydag <- rbind(hydag,station.data)
} # end file loop


hydag[,yk:=year(date)]
setkey(hydag,ID,yk)

saveRDS(hydag,file="~/floodGAM/data/raw-data/raw-gamfelt-hydag.rds")


# Sanity check: what years and stations are in 37 vs 39? ------------------

hyfinc.sy <- data[,unique(.SD),.SDcols = c("ID","yk")]
hydag.sy <- hydag[,unique(.SD),.SDcols = c("ID","yk")]
setkey(hyfinc.sy,ID,yk); setkey(hydag.sy,ID,yk)

# station-years in hyfinc *not* in hydag:
hyfinc.sy[, in.hydag := FALSE][hydag.sy, in.hydag := TRUE]
hyfinc.sy[in.hydag==FALSE]

## so all hyfin_c station-years are in hydag. good. 


# Remove hydag years identified as utelatt, table A2 ---------------------------

## dependencies: expandutelatt function

utelatt.day <- A2.tab[,
                  lapply(.SD,expandutelatt),
                  by=ID,
                  .SDcols="Dailydata_Utlatt"]

utelatt.day <- utelatt.day[, .(yk = unlist(Dailydata_Utlatt)), by = ID]

setkey(utelatt.day,ID,yk)

# for the stations we care about: are any utelatt.day years in the data?
merge(hyfinc.sy,utelatt.day,by=c("ID","yk"))
## no. good.


# define column to filter out discarded values
setkey(hydag,ID,yk)
hydag[,discard:=FALSE]

hydag <- hydag[.(utelatt.day), discard := TRUE][discard == FALSE]


# HYDAG: Remove years with < 244 days -------------------------------------

# create month column, day column and search for
# unique combinations:
keyGenerator <- function(x){list(month(x),day(x))}

# add month and day
hydag[,c("mk","dk"):=sapply(.SD,keyGenerator),.SDcols = "date"]

# count number of days in a year (unique combinations of month key and
# day key grouped by year)
hydag[,numdays:=uniqueN(.SD),by=list(ID,yk),.SDcols=c("mk","dk")]

# find the ID-yk (station-year) combinations that have less than 200 days of data
discard.244 <- hydag[numdays<244,unique(.SD),.SDcols=c("ID","yk")]

setkey(discard.244,ID,yk)

hydag <- hydag[.(discard.244), discard := TRUE][discard == FALSE]

saveRDS(hydag,file="~/floodGAM/data/cleaned-data/cleaned-gamfelt-hydag.rds")




# Cross-check hyfin_complete with hydag ----------------------------------

## --- 1. filter out station-years that have less than 300 days 
## --- of data recorded both in hyfin_complete and hydag

# find the ID-yk tuples that have < 300 days in *both* databases:
discard.both300 <- merge(data[numdays<300,unique(.SD),.SDcols=c("ID","yk")],
                         hydag[numdays<300,unique(.SD),.SDcols=c("ID","yk")])

setkey(discard.both300,ID,yk)

data <- data[.(discard.both300), discard := TRUE]

## take a quick look at the years flagged for discard.both300:
for(station in unique(discard.both300$ID)){
  
  gdat <- data[discard==T&ID==station]
  setkey(gdat,ID,yk)
  hdat <- hydag[ID==station&yk%in%unique(gdat$yk)]
  
  gg <- ggplot(gdat) +
    geom_point(aes(date,cumecs),size=0.9) +
    geom_point(data=hdat,aes(date,cumecs),color="red") +
    labs(title=station) +
    facet_wrap(vars(yk),scales="free_x") +
    theme_bw()
  
  print(gg)
  
  gc()
  
}


data <- data[discard == FALSE]
hydag <- hydag[.(discard.both300), discard := TRUE][discard == FALSE]


## --- 2. look at, and discard, years in hyfin_c
## --- that are missing the hydag ann max.


# switch to working with decimal dates instead of POSIXt format:
data[, dd:=lapply(.SD,decimal_date), .SDcols="date"]
hydag[, dd:=lapply(.SD,decimal_date), .SDcols="date"]

twentyfour <- 0.00273224 # 24 hours in decimal date

# find annual maxima from hydag (data05)
amhd <- hydag[hydag[, .I[which.max(cumecs)], by=c("ID","yk")]$V1]
# only keep a few relevant columns:
amhd <- amhd[,c("ID","yk","cumecs","dd")]

# take the intersection of station-years in hydag and hyfin_c:
data <- merge(data, amhd, all.x = T)

# compute distance between points in hyfin_c and decimal date
# of annual maxima from hydag
data[,dd.dist:=abs(dd.x-dd.y)]

# find minimum distance by station-year. If minimum distance is > 24 hrs
# (if there is no observation in hyfin_c within +/- 1 day of the needed
# point), then set discard to TRUE. Then select only rows
# with discard = FALSE
data[,discard:=ifelse(min(dd.dist)>twentyfour,TRUE,FALSE),by=c("ID","yk")]

discard.annmax <- data[discard==T]
discard.annmax <- discard.annmax[,unique(yk),by="ID"]
setnames(discard.annmax,"V1","yk")

## take a quick look at the years flagged for discard:

for(station in unique(discard.annmax$ID)[1:7]){
  
  gdat <- data[discard==T&ID==station]
  setkey(gdat,ID,yk)
  hdat <- hydag[ID==station&yk%in%unique(gdat$yk)]
  
  gg <- ggplot(gdat) +
    geom_line(aes(dd.x,cumecs.x),linewidth=0.9) +
    geom_line(data=hdat,aes(dd,cumecs),color="red") +
    geom_vline(aes(xintercept = dd.y),color="blue",
               linetype=2,linewidth=1) +
    labs(title=station) +
    facet_wrap(vars(yk),scales="free_x") +
    theme_bw()
  
  print(gg)
  
  gc()
  
}


## discard all the years flagged in discard.annmax:
data <- data[discard == FALSE]



# How many years of data does each station have now? ----------------------

# create record length data table--used to index into larger 'data' object
recordlen <- data[,.(N.hfc=uniqueN(.SD)),by=ID,.SDcols = "yk"]
# 'N.hfc' = number of years in hyfin complete, not utelatt, with 244+ days of 
## data per year 

# what is the first and last year recorded?
recordlen <- merge(recordlen,data[,.(start.hfc=min(.SD)),by=ID,.SDcols = "yk"],
                   by="ID")
recordlen <- merge(recordlen,data[,.(slutt.hfc=max(.SD)),by=ID,.SDcols = "yk"],
                   by="ID")

# add in supplementary info (which report, station name, num findata yrs from A2)
recordlen <- merge(recordlen,
                      lescon[,c("ID","Navn","NIFS","A2","Findata_N","version")],
                   by="ID")


# what stations have 20 years or more finedata?
fin.stations <- recordlen[N.hfc>=20]

# what report are these stations from?
fin.stations[!is.na(A2)&!is.na(NIFS)]

fin.stations[!is.na(NIFS)]
## 1 station just in NIFS (46.7, Brakhaug)


# Save the record length data table -----------------------

saveRDS(recordlen, 
        file="~/floodGAM/data/cleaned-data/record_length_data_table.rds")


# Select and save years and stations for gamfelt -------------------------------

# select years that are (i) not marked as utelatt in Table A2, with 
# (ii) 20 years of data that have at least 244 days of data per year

gamfelt.hyfinc <- data[ID%in%fin.stations$ID]

gamfelt.hyfinc <- gamfelt.hyfinc[,c("date","cumecs.x","ID","yk")]
setnames(gamfelt.hyfinc,c("cumecs.x","yk"),c("Qm3_s","year_key"))

saveRDS(gamfelt.hyfinc,
        file="~/floodGAM/data/cleaned-data/gamfelt-NIFS-A2-hyfincomplete.rds")



## save the supplementary hydag dataset:

hydag <- hydag[ID%in%fin.stations$ID]

hydag <- hydag[,c("date","cumecs","ID","yk")]
setnames(hydag,c("cumecs","yk"),c("Qm3_s","year_key"))

saveRDS(hydag,
        file="~/floodGAM/data/cleaned-data/gamfelt-NIFS-A2-hydag.rds")






