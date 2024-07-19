##
##
##
## Vi bruker data fra arkiv HYKALP-ICECORR (arkiv 35), som har 
## primærtkontrollerte data med fin/variabel tidsoppløsning og er virtuelt 
## isreduserte. HYKVALP-ICECORR er ikke sekundærkontrollert eller kompletterte. 
##
## Fordi HYKVAL-dataene ikke er komplette, må vi derfor ta beslutninger om 
## hvordan vi skal håndtere år med manglende data. Vi velger å kryssjekke 
## HYKVALP-ICECORR dataene med data fra et annet arkiv, HYDAG (arkiv 05), som 
## inneholder kontrollerte, kompletterte døgndata som er 
## isredusert og etterkontrollert. 
##
## Denne skripten kvalitetskontrollere HYKVALP-ICECORR dataene.
## -----------------------------------------------------------------------------

library(data.table)
library(lubridate)

## ---define a local path to wherever you have stored the 
## .rds files (see /code/scripts/data-creation/clean-and-process-rawdata-from-database.R) 
dataPath

## ---read in data
data35 <- readRDS(paste0(dataPath,"arkiv35","data.rds")) # not stored on github
data05 <- readRDS(paste0(dataPath,"arkiv05","data.rds")) # not stored on github

# load in utelatt (all stations and years)
utelatt <- readRDS(paste0("~/floodGAM/data/raw-data/","utelatt.rds"))

## hykvalp-icecorr dataset is a few gigabytes:
print(object.size(data35), units = "Gb")

## to speed up operations we set up the 
## key columns for binary searches:
# make year column
data05[,yk:=year(date)]; data35[,yk:=year(date)]
# set the key
setkey(data05,ID,yk); setkey(data35,ID,yk)


# Steg 2: manuell fjerning ------------------------------------------------
##
## NVE rapporten 2016:85 identifiserer visse år med data som bør utelates 
## for flomfrekvenseanalyse. I tillegg, har Kolbjørn og jeg identifiserte 
## noen år og stasjoner som bør utelates om vi trenger 1-times oppløsning. 
## Filen som identifiserer de manuelle kontrollerte stasjonene og årene 
## finnes på data/raw-data og noen notater om manuelle fjernet år finnes
## i Excel-filen på /assets

setkey(utelatt,ID,yk)

# define column to filter out discarded values
data05[,discard:=FALSE]; data35[,discard:=FALSE]

# discard all utelatt data:
# subset all rows of data using key columns from utelatt where first key 
# matches ID and second key matches year. Return the 'discard' column
# and set discard value = T for utelatt subset. Then, use chaining 
# to select only rows that have discard values = F. 
data05 <- data05[.(utelatt), discard := TRUE][discard == FALSE]
data35 <- data35[.(utelatt), discard := TRUE][discard == FALSE]

# Steg 3: Fjern year med manglende data -------------------------------------

# This many unique ID-yk (station-year) tuples before filtering
data35[,uniqueN(.SD),.SDcols = c("ID","yk")]

# create month column, day column and search for
# unique tuples:

keyGenerator <- function(x){list(month(x),day(x))}

# add month and day
data35[,c("mk","dk"):=sapply(.SD,keyGenerator),.SDcols = "date"]
data05[,c("mk","dk"):=sapply(.SD,keyGenerator),.SDcols = "date"]

# count number of days in a year (unique tuples of month key and
# day key grouped by year)
data35[,numdays:=uniqueN(.SD),by=list(ID,yk),.SDcols=c("mk","dk")]
data05[,numdays:=uniqueN(.SD),by=list(ID,yk),.SDcols=c("mk","dk")]


## --- filter out station-years that have less than 200 days 
## --- of data recorded in hykvalp-icecorr (database 35)

# find the ID-yk (station-year) tuples that have less than 200 days of data
# in hykvalp-icecorr (database 35)
discard.hykval200 <- data35[numdays<200,unique(.SD),.SDcols=c("ID","yk")]

# set key for cache-efficient indexing
setkey(discard.hykval200,"ID","yk")

data05 <- data05[.(discard.hykval200), discard := TRUE][discard == FALSE]
data35 <- data35[.(discard.hykval200), discard := TRUE][discard == FALSE]

# after filtering on the hykval-200 days criteria, we have
# this many unique ID-yk tuples:
data35[,uniqueN(.SD),.SDcols = c("ID","yk")]


## --- filter out station-years that have less than 300 days 
## --- of data recorded both in hykvalp-icecorr (database 35)
## --- and hydag (database 05)

# find the ID-yk tuples that have < 300 days in *both* databases:
discard.both300 <- merge(data35[numdays<300,unique(.SD),.SDcols=c("ID","yk")],
                         data05[numdays<300,unique(.SD),.SDcols=c("ID","yk")])

data05 <- data05[.(discard.both300), discard := TRUE][discard == FALSE]
data35 <- data35[.(discard.both300), discard := TRUE][discard == FALSE]

# after filtering on the both-300 days criteria, we have
# this many unique ID-yk tuples:
data35[,uniqueN(.SD),.SDcols = c("ID","yk")]


## --- filter out station-years based on annual maxima - Hydag criteria

# hydag and hykvalp-icecorr are not a perfect match. There are some
# stations and years that are in hykval but not hydag and vice versa:
hykval.sy <- data35[,unique(.SD),.SDcols = c("ID","yk")]
hydag.sy <- data05[,unique(.SD),.SDcols = c("ID","yk")]
setkey(hykval.sy,ID,yk); setkey(hydag.sy,ID,yk)

# station-years in hykvalp-icecorr *not* in hydag:
hykval.sy[, in.hydag := FALSE][hydag.sy, in.hydag := TRUE]
hykval.sy[in.hydag==FALSE]

# station-years in hydag *not* in hykvalp-icecorr:
hydag.sy[, in.hykval := FALSE][hykval.sy, in.hykval := TRUE]
hydag.sy[in.hykval==FALSE]


## there are 70 station-years where we have data in hykval
## but no data in hydag. These station-years are not checked
## with the ann max criteria.
## for the remaining 14156 station-years where we *have* data in hydag,
## run the annual maxima check against hykval:

# switch to working with decimal dates instead of POSIXt format:
data05[, dd:=lapply(.SD,decimal_date), .SDcols="date"]
data35[, dd:=lapply(.SD,decimal_date), .SDcols="date"]

twentyfour <- 0.00273224 # 24 hours in decimal date

# find annual maxima from hydag (data05)
amhd <- data05[data05[, .I[which.max(cumecs)], by=c("ID","yk")]$V1]
# only keep a few relevant columns:
amhd <- amhd[,c("ID","yk","cumecs","dd")]

# take the intersection of station-years in hydag and hykval:
data35 <- merge(data35, amhd, all.x = T)

# compute distance between points in hykval and decimal date
# of annual maxima from hydag
data35[,dd.dist:=abs(dd.x-dd.y)]

# find minimum distance by station-year. If minimum distance is > 48 hrs
# (if there is no observation in hykval within +/- 2 days of the needed
# point), then set discard to TRUE. Then select only rows
# with discard = FALSE
data35[,discard:=ifelse(min(dd.dist)>twentyfour,TRUE,FALSE),by=c("ID","yk")]

data35 <- data35[discard == FALSE]

# after filtering on the hydag-annmax criteria, we have
# 14030 unique ID-yk tuples:
data35[,uniqueN(.SD),.SDcols = c("ID","yk")]



# Steg 4: Fjern stasjoner < 20 years -------------------------------------------

data35 <- data35[ID %in% data35[,uniqueN(.SD),by=ID,.SDcols = "yk"][V1>=20]$ID,]

# after filtering on the hydag-annmax criteria, we have
# this many unique ID-yk tuples:
data35[,uniqueN(.SD),.SDcols = c("ID","yk")]

# and this many stations:
data35[,uniqueN(.SD),.SDcols = c("ID")]

## distribution of the number of years per station:
hykval.sy <- data35[,uniqueN(.SD),.SDcols = c("yk"),by="ID"]
hist(hykval.sy$V1)


# Steg 5: Fjern stasjoner < 10 år findata ---------------------------------

# compute the time between observations
difftimeFn <- function(x){
  a=x[-1]
  b=x[-length(x)]
  dtvec <- as.numeric(difftime(a,b,units="mins"))
  return(c(0,dtvec))
}

data35[, gapmin := lapply(.SD,difftimeFn), .SDcols = "date", by = c("ID","yk")]

# what station-years have at least 200 days of findata?
setkey(data35,ID,yk,mk,dk)

numdayfin <- data35[,median(gapmin),by=c("yk","mk","dk","ID")]
numdayfin <- numdayfin[,sum(V1<1440),by=c("ID","yk")]
setnames(numdayfin,"V1","nfin.yk")

# how many years of fine data does each station have?
numyrsfin <- merge(numdayfin[nfin.yk>199, .N, by="ID"][,c("ID","N")],
                   numdayfin[, .N, by="ID"][,"ID"],
                   all.y=T)
setnafill(numyrsfin,cols="N",fill=0)

setkey(numyrsfin,ID)

# remove the nine stations with less than 10 years of findata
discard.fin <- numyrsfin[N<10]

data35 <- data35[.(discard.fin), discard := TRUE][discard == FALSE]

# now we have this many stations:
data35[,uniqueN(.SD),.SDcols = c("ID")]


# Save data ---------------------------------------------------------------

save(data35,file=paste0("cleaned_archive35.rda"))



