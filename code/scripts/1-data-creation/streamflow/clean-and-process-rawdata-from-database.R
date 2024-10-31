##
##
##
##
## Steg 1: les data i R ----
## 
## Skriptet leser inn dataene vi hentet ved hjelp av lescon_var. 
## Hver enkelt .txt-fil inneholder data for en individuell stasjon. 
## Vi looper over alle filene i mappen for å samle alle dataene i en enkelt 
## data.table. Negative verdier fjernes og datoer konverteres til riktig format. 
## Utdataene er en enkelt .rds-fil som inneholder alle dataene.
# --------------------------------------------------------------------------

library(data.table)
library(lubridate)

## set the path to the directory you downloaded the data files into
path <- "C:/new_lescon_data"
setwd(path) 

## list all .txt files in the directory. Each individual .txt file 
## contains data for an individual station. We then loop over these files 
## to get all data into a single data.table
alltxtfiles <- list.files(pattern = ".txt") 

## check for any empty files:
## (if file is empty, check lescon_var command..
## they should all work as of 18.10.2024)
idx <- which(file.info(list.files(".", 
                                  all.files = TRUE, 
                                  recursive = TRUE))$size == 0)
alltxtfiles[idx]

## differentiate between files from arkiv 5 and arkiv 35:
fl37 <- list.files(pattern = "37_")
fl39 <- alltxtfiles[-which(alltxtfiles %in% fl37)]
fl39 <- fl39[-which(grepl("lescon", fl39))]

flList <- list(fl37,fl39)

for(i in 1:2){ # loop over the archives
  
  fl <- flList[[i]]
  
  ## initialize data table to store data
  data <- data.table(date=POSIXct(),cumecs=numeric(),ID=character())
  
  for(thisfile in fl){ # loop over the files within each archive
    ## --- set inside a tryCatch so we can identify problematic files,
    ## --- if they exist. 
    tryCatch(station.data <- fread(thisfile),
             warning = function(w) {
               print(w)
               print(thisfile)
             })
    setnames(station.data,c("V1","V2"),c("date","cumecs"))
    ## --- remove negative Data values ---
    station.data <- station.data[cumecs >= 0]
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
    n  <- ifelse(i == 1, 4, 1)
    station.data[,ID:=substr(thisfile,n,nchar(thisfile)-4)]
    data <- rbind(data,station.data)
  } # end file loop
  
  ## save the data file somewhere (define "dataPath")
  if(i == 1){
    saveRDS(data, paste0("~/floodGAM/data/raw-data/","arkiv37","data.rds"))
  }else{
    saveRDS(data, paste0("~/floodGAM/data/raw-data/","arkiv39","data.rds"))
  }
} # end archive loop
