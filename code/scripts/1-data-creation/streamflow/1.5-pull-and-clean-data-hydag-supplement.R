##
##
##
##
## For certain stations we are interested in supplementing the findata record
## length with longer series of daily data
##
## This script creates the lescon_var commands to pull from hydag
## 
## -----------------------------------------------------------------------------

library(data.table)
library(lubridate)


# load in the data --------------------------------------------------------

recordlen <- readRDS(paste0("~/floodGAM/data/cleaned-data/",
                            "record_length_data_table.rds"))

A2.tab <- fread(paste0("~/floodGAM/data/raw-data/",
                       "tabell_A2_rapport_2016-85_daba_edited.txt"),
                fill=T, colClasses = list(character=1))


# Preliminary data cleaning -----------------------------------------------

A2.tab[, c("RN", "HN") := tstrsplit(Nr, ".", fixed=TRUE)]
A2.tab[,A2:="A2"]
A2.tab[,ID:=paste0(RN,"-",HN)]


# Enforce minimum length of findata 10 years ------------------------------

min.findata <- recordlen[N.hfc>=10]

## we want to pull hydag data for these stations (minfin stations)

## create Hn Rn columns
min.findata[, c("RN", "HN") := tstrsplit(ID, "-", fixed=TRUE)]

# create lescon_var commands ----------------------------------------------

## set a working directory. R will write the text file to this directory.
setwd("~/floodGAM/data/raw-data/")

## create commands to pull from hydag for stations in min.findata

## pull from archive 37 (hydag)
## name the executable lescon_var file and open the connection
f <- file("lesconvar_commands_min-findata_archive_37-hydag.txt", open="wb")

## write text to file using cat command
cat("# !/bin/bash",file=f,append=F,sep="\n")
for(i in 1:dim(min.findata)[1]){
  cat(paste0("lescon_var -b 0 -f timevalue 37 ",
             min.findata$RN[i], " ", min.findata$HN[i],
             " 0 1001 ",min.findata$version[i]," > ",
             min.findata$RN[i], "-", min.findata$HN[i],".txt"),
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


# What stations failed the command? ---------------------------------------

## list the files in the directory you downloaded the data files into
alltxtfiles <- list.files(path="C:/data-tab/NIFS-A2-minfin-hydag/", 
                          pattern = ".txt") 

## check for empty files:
idx <- which(file.info(path=paste0("C:/data-tab/NIFS-A2-minfin-hydag/",
                                   alltxtfiles))$size == 0)

## no empty files. this is good (all stations that have fine data also have daily data)


# Load in the stations ----------------------------------------------------

## remove command file
fileloop <- alltxtfiles[-c(idx,
    which(alltxtfiles==paste0("lesconvar_commands_min-findata_archive_37-hydag.txt")))]

## initialize data table to store data
data <- data.table(date=POSIXct(),cumecs=numeric(),ID=character())

for(fl in fileloop){
  
  station.data <- fread(paste0("C:/data-tab/NIFS-A2-minfin-hydag/",fl))
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

saveRDS(data,file="~/floodGAM/data/raw-data/raw-minfin-hydag.rds")


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
                  .SDcols="Dailydata_Utlatt"]

utelatt <- utelatt[, .(yk = unlist(Dailydata_Utlatt)), by = ID]

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


# Save minfin-hydag data --------------------------------------------------

data <- data[,c("date","cumecs","ID","yk")]
setnames(data,c("cumecs","yk"),c("Qm3_s","year_key"))

saveRDS(data,file="~/floodGAM/data/cleaned-data/cleaned-minfin-hydag.rds")
















