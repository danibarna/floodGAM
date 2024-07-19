##
##
##
##
## what is removed when: numbers for summarizing findata quality control
##
## need to fix: still relies on old repository
## -----------------------------------------------------------------------------

library(data.table)
library(lubridate)

## ---define paths
dataPath <- paste0("~/ClimDesign_PhD/XGBoost-GAM index flood model",
                   "/src/","dataset_construction/","publishable/")

## ---read in data
data35 <- readRDS(paste0(dataPath,"arkiv35","data.rds"))
data05 <- readRDS(paste0(dataPath,"arkiv05","data.rds"))

## hykvalp-icecorr dataset is a few gigabytes:
print(object.size(data35), units = "Gb")

## to speed up operations we set up the 
## key columns for binary searches:
# make year column
data05[,yk:=year(date)]; data35[,yk:=year(date)]
# set the key
setkey(data05,ID,yk); setkey(data35,ID,yk)

# add month and day
keyGenerator <- function(x){list(month(x),day(x))}
data35[,c("mk","dk"):=sapply(.SD,keyGenerator),.SDcols = "date"]
data05[,c("mk","dk"):=sapply(.SD,keyGenerator),.SDcols = "date"]

# define column to filter out discarded values
data05[,discard:=FALSE]; data35[,discard:=FALSE]

# compute the time between observations
difftimeFn <- function(x){
  a=x[-1]
  b=x[-length(x)]
  dtvec <- as.numeric(difftime(a,b,units="mins"))
  return(c(0,dtvec))
}

## we start with 329 stations that have at least some findata:
data35[,uniqueN(.SD),.SDcols = c("ID")]

# 1: total years ? subdaily years ? ---------------------------------------

# total years > 20:
data35 <- data35[ID %in% data35[,uniqueN(.SD),by=ID,.SDcols = "yk"][V1>=20]$ID,]

data35[,uniqueN(.SD),.SDcols = c("ID")]

# findata years > 10:
data35[, gapmin := lapply(.SD,difftimeFn), .SDcols = "date", by = c("ID","yk")]
setkey(data35,ID,yk,mk,dk)
numdayfin <- data35[,median(gapmin),by=c("yk","mk","dk","ID")]
numdayfin <- numdayfin[,sum(V1<1440),by=c("ID","yk")]
setnames(numdayfin,"V1","nfin.yk")
# how many years of fine data does each station have?
numyrsfin <- numdayfin[nfin.yk>199, .N, by="ID"]
discard.fin <- numyrsfin[N<10]
setkey(discard.fin,ID)

data35 <- data35[.(discard.fin), discard := TRUE][discard == FALSE]

data35[,uniqueN(.SD),.SDcols = c("ID")] # 274 stations


# run cross-check before utelatt ------------------------------------------

data35[,uniqueN(.SD),.SDcols = c("ID","yk")]

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

# after filtering on the hykval-200 days criteria
data35[,uniqueN(.SD),.SDcols = c("ID","yk")]


## --- filter out station-years that have less than 300 days 
## --- of data recorded both in hykvalp-icecorr (database 35)
## --- and hydag (database 05)

# find the ID-yk tuples that have < 300 days in *both* databases:
discard.both300 <- merge(data35[numdays<300,unique(.SD),.SDcols=c("ID","yk")],
                         data05[numdays<300,unique(.SD),.SDcols=c("ID","yk")])

data05 <- data05[.(discard.both300), discard := TRUE][discard == FALSE]
data35 <- data35[.(discard.both300), discard := TRUE][discard == FALSE]

# after filtering on the both-300 days criteria, we have:
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

discard.annmax <- data35[discard==T]
discard.annmax <- discard.annmax[,unique(yk),by="ID"]
setnames(discard.annmax,"V1","yk")

data35 <- data35[discard == FALSE]

# after filtering on the hydag-annmax criteria, 
data35[,uniqueN(.SD),.SDcols = c("ID","yk")]


## ---------------- now apply 20 yrs- 10 yrs criteria:
# total years > 20:
data35 <- data35[ID %in% data35[,uniqueN(.SD),by=ID,.SDcols = "yk"][V1>=20]$ID,]

data35[,uniqueN(.SD),.SDcols = c("ID")]

# findata years > 10:
data35[, gapmin := lapply(.SD,difftimeFn), .SDcols = "date", by = c("ID","yk")]
setkey(data35,ID,yk,mk,dk)
numdayfin <- data35[,median(gapmin),by=c("yk","mk","dk","ID")]
numdayfin <- numdayfin[,sum(V1<1440),by=c("ID","yk")]
setnames(numdayfin,"V1","nfin.yk")
# how many years of fine data does each station have?
numyrsfin <- numdayfin[nfin.yk>199, .N, by="ID"]
discard.fin <- numyrsfin[N<10]
setkey(discard.fin,ID)

data35 <- data35[.(discard.fin), discard := TRUE][discard == FALSE]

data35[,uniqueN(.SD),.SDcols = c("ID")] # 256 stations


## ------------------ now: how many utelatt years are still valid?

utelatt <- fread(paste0(dataPath,"utelatt.csv"),
                 colClasses = c("character","numeric"))


# merge utelatt and ID, yk from data35.

tt <- data35[,unique(yk),by="ID"]
setnames(tt,"V1","yk")

# 636 station-years in utelatt are still valid
utelatt <- merge(tt,utelatt,by=c("ID","yk"))

# see if the number of utelatt years = N years per station
utelatt[,isutelatt:=1]
tv <- merge(tt,utelatt,by=c("ID","yk"), all.x = T)
setnafill(tv,cols="isutelatt", fill = 0)

tv[,uteyrs:=sum(isutelatt),by="ID"]
tv[,Nyrs:= .N, by ="ID"]

utelatt.stations <- tv[uteyrs==Nyrs,unique(ID)] # so we remove 8 stations 

# and 376 years from other stations
tv[!ID%in%utelatt.stations,sum(isutelatt)]

utelatt.years <- utelatt[!ID%in%utelatt.stations,]
utelatt.years[,isutelatt:=NULL]

# split into utelatt_stations and utelatt_years. Save the new utelatt files
# to that messy folder in ClimDesign_PhD

save(utelatt.stations,utelatt.years,file=paste0(dataPath,"utelatt.rda"))
saveRDS(utelatt,file=paste0("~/floodGAM/data/raw-data/","utelatt.rds"))

write.csv(utelatt.stations,paste0("~/floodGAM/data/raw-data/","utelatt_stations.csv"),row.names=F)
write.csv(utelatt.years,paste0("~/floodGAM/data/raw-data/","utelatt_years.csv"),row.names = F)


discard.utelatt <- utelatt
discard.utelatt[,isutelatt:=NULL]


# rbind all the discard types ---------------------------------------------

discard.annmax[,type:="Missing observations around HYDAG ann max"]
discard.both300[,type:="HYKVALP-ICECORR and HYDAG < 300 days"]
discard.hykval200[,type:="HYKVALP-ICECORR < 200 days"]
discard.utelatt[,type:="Manual removal"]

discard.all <- rbind(discard.annmax,discard.both300,
                     discard.hykval200,discard.utelatt)

notdiscard <- data35[,unique(.SD),.SDcols = c("ID","yk")]
notdiscard[,type:="Data used in analysis"]

discard.all <- rbind(discard.all,notdiscard)

saveRDS(discard.all,file=paste0("~/floodGAM/data/","README_files/",
                                "discard_histogram_data.rds"))











