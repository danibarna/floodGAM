##
##
##
## create executable .txt file to run lescon var
##
## based on .txt file created from excel sheet found at
## "\\nve.no\fil\h\HM\Eksterne Prosjekter\ClimDesign\PhD"
## and the lescon_var scripts provided by Kolbjørn
##
## takes the station names from tabel A2 (excel sheet)
## and version numbers from a mix of Kolbjørn's script
## and manual control (i.e. checking which version actually exists)
##
## ---------------------------------------------

library(data.table)
library(stringr)

## set a working directory. R will write the text file to this directory.
setwd(myfilePath)

## ------- Les inn data: 
## read in the manually controlled version numbers
altversions <- readRDS(paste0(myfilePath,
                              "alternate_version_numbers_from_ks_script.rds"))
## read in table A2 from report 2016:85
tabellA2 <- data.table::fread(paste0(myfilePath,
                                     "tabell_for_rapport2016_forenkelt_v.txt"))



## there are 530 stations in table A2. Some of these stations have just daily 
## data, some have both daily data and findata.
dim(tabellA2)[1]

## we exclude the stations with only daily data. 
## This leaves 329 stations with at least some findata:
findata <- tabellA2[is.na(Findata_N) != T] 
dim(findata)[1]

## ------- Stasjon nummer fra Tabell A2 samsvarer ikke med navnekonvensjonen 
## som brukes i lescon_var. Vi fikser dette:

zeroInd <- findata[, .I[which(onezero==T)]] # append 1 to fix trailing zeros
findata[zeroInd, Nr:=(Nr+1*10^(-(numdig+1)))]
findata[, Nr:=as.character(Nr)]
findata[, c("RN", "HN") := tstrsplit(Nr, ".", fixed=TRUE)]
findata[zeroInd, HN:=substr(HN,1,(nchar(HN)-1))] # get rid of the 1
findata[,HNpad:=lapply(.SD,str_pad,width=5,pad="0"),.SDcols="HN"]
findata[,ID:=paste0(RN,HNpad)]

Nrlescon <- findata[,c("RN", "HN", "version","ID")]

# stitch together rn and hn and pad with zeros to make
# names to call files from server
Nrlescon[,flnm:= paste0(ID,".txt")]


## ------- Legg til manuel-kontrollert versjonsnumre:
  
# now add in alternate version numbers from Kolbjørn provided script
altversions[,ID:=lapply(.SD,function(x) substr(x,1,nchar(x)-4)),
            .SDcols = "ID"]

Nrlescon <- merge(Nrlescon,altversions,all.x=T)

# in situations where kvnum doesn't exist fill with "version" (the manually 
# corrected version)
idx <- Nrlescon[,.(idx = .I[is.na(kvnum)])]$idx
Nrlescon$kvnum[idx] <- Nrlescon$version[idx]


## ------- Skriv den kjørebare lescon_var filen:
  
## good idea to double check the working directory
## before opening the connection to bash

## name the executable lescon_var file (here we name it 
## "my_lescon_var_commands.txt") and open the connection
f <- file("my_lescon_var_commands.txt", open="wb")

## write text to file using cat command
cat("# !/bin/bash",file=f,append=F,sep="\n")
for(i in 1:dim(Nrlescon)[1]){
  cat(paste0("lescon_var -b 0 -f timevalue 35 ",
             Nrlescon$RN[i], " ", Nrlescon$HN[i],
             " 0 1001 ",Nrlescon$kvnum[i]," > ",
             Nrlescon$flnm[i]),
      file=f,append=T,sep="\n")
}

## close the connection
close(f)

## the my_lescon_var_commands.txt file should now be found in the 
## working directory


