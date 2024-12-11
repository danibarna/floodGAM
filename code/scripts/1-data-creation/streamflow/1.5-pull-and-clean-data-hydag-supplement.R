##
##
##
##
## Create commands to pull hydag data for relevant stations.
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

## we want to pull hydag data for these stations

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









