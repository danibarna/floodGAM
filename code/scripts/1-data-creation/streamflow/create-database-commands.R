##
##
##
## create executable .txt file to run lescon var
##
## based on table A2 from NVE report 2016:85 (data for flood freq analysis)
##
## takes the station names from table A2.
## a few version numbers are manually controlled
##
## ---------------------------------------------

library(data.table)
library(stringr)

## set a working directory. R will write the text file to this directory.
setwd("~/floodGAM/data/raw-data/")

## read in table A2 from report 2016:85
tabellA2 <- data.table::fread(paste0("~/floodGAM/data/raw-data/",
                                     "tabell_for_rapport2016_forenkelt_v.txt"),
                              colClasses = list(character=1))

## first column is station numbers. We want the station numbers to create
## the lescon_var executable:
tabellA2[, c("RN", "HN") := tstrsplit(Nr, ".", fixed=TRUE)]

## there are 530 stations in table A2. Some of these stations have just daily 
## data, some have both daily data and findata.
dim(tabellA2)[1]

## we exclude the stations with only daily data. 
## This leaves 327 stations with at least some findata:
findata <- tabellA2[is.na(Findata_N) != T] 
dim(findata)[1]

## create ID column (this format matches an old version of a covariate file;
## now it just functions as a key column)
findata[,HNpad:=lapply(.SD,str_pad,width=5,pad="0"),.SDcols="HN"]
findata[,ID:=paste0(RN,HNpad)]

Nrlescon <- findata[,c("RN", "HN", "version","ID")]

## these stations need different version numbers than the ones
## listed in Table A2 2016:85
changevnum <- data.table(ID=c("200102","1200113","7100001",
                              "7200005","10300003",
                              "11100001","12700006",
                              "1100004"),
                         version=c(0,2,2,2,2,3,2,2))
setkey(changevnum,ID)
setkey(Nrlescon,ID)

Nrlescon[.(changevnum),version := changevnum$version]

## make file name column:
Nrlescon[,flnm:= paste0(ID,".txt")]


## ------- Skriv den kjørebare lescon_var filen:

## good idea to double check the working directory
## before opening the connection to bash

## archive 39 (hyfin completed)
## name the executable lescon_var file and open the connection
f <- file("lescon_var_commands_archive_39.txt", open="wb")

## write text to file using cat command
cat("# !/bin/bash",file=f,append=F,sep="\n")
for(i in 1:dim(Nrlescon)[1]){
  cat(paste0("lescon_var -b 0 -f timevalue 39 ",
             Nrlescon$RN[i], " ", Nrlescon$HN[i],
             " 0 1001 ",Nrlescon$version[i]," > ",
             Nrlescon$flnm[i]),
      file=f,append=T,sep="\n")
}

## close the connection
close(f)


## archive 37 (hydag)
f <- file("lescon_var_commands_archive_37.txt", open="wb")

## write text to file using cat command
cat("# !/bin/bash",file=f,append=F,sep="\n")
for(i in 1:dim(Nrlescon)[1]){
  cat(paste0("lescon_var -b 0 -f timevalue 37 ",
             Nrlescon$RN[i], " ", Nrlescon$HN[i],
             " 0 1001 ",Nrlescon$version[i]," > ",
             "37_",Nrlescon$flnm[i]),
      file=f,append=T,sep="\n")
}

## close the connection
close(f)


