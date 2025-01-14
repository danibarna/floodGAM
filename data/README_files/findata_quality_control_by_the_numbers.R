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

library(ggplot2)


discard.tab <- data.table(ID=character(),yk=numeric(),type=character)

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



## -----------------------------------------------------------------------------
## *****************************
## Pull data from hyfin_complete
## *****************************
## -----------------------------------------------------------------------------

# HYFIN_C: create lescon_var commands ------------------------------------

lescon <- tab[(!is.na(NIFS) | !is.na(A2)) & is.na(A3)]
lescon[,ID:=paste0(RN,"-",HN)]

lescon <- lescon[!ID%in%lescon[Nedlagtregulert=="R",get("ID")]]

## for the 11 stations in NIFS but not A2 the version number is chosen from Hysopp
lescon[,version:=ifelse(is.na(version),1,version)]

## station 160.6 needs version 2 (according to Hysopp)
lescon[,version:=ifelse(ID=="160-6",2,version)]



# HYFIN_C: What stations failed the command? -------------------------------

## list the files in the directory you downloaded the data files into
alltxtfiles <- list.files(path="C:/data-tab/NIFS-A2/", pattern = ".txt") 

## check for empty files:
idx <- which(file.info(path=paste0("C:/data-tab/NIFS-A2/",alltxtfiles))$size == 0)

length(alltxtfiles) - length(idx)

# HYFIN_C: Load in the stations ------------------------------------------

data <- readRDS("~/floodGAM/data/raw-data/raw-NIFS-A2-hyfincomplete.rds")


## select only stations that have at least 20 years data
tt <- data[,uniqueN(.SD),by="ID",.SDcols="yk"]
setkey(tt,ID)

tt <- tt[V1>=20]

data <- data[ID%in%tt$ID]

# HYFIN_C: Remove years identified as utelatt, table A2 -------------------

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
data <- data[.(utelatt), discard := TRUE]


ute.tab <- data[discard==TRUE,unique(yk),by="ID"]
setnames(ute.tab,"V1","yk")
ute.tab[,type:="Markert som utelatt, tabell A2 85-2016"]

discard.tab <- rbind(discard.tab,ute.tab)

data <- data[discard == FALSE]


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

## select only stations that have at least 20 years data
tt <- data[,uniqueN(.SD),by="ID",.SDcols="yk"]
setkey(tt,ID)

tt <- tt[V1>=20]

data <- data[ID%in%tt$ID]


discard.244 <- data[numdays<244,unique(.SD),.SDcols=c("ID","yk")]
setkey(discard.244,ID,yk)



data <- data[.(discard.244), discard := TRUE][discard == FALSE]



discard.244[,type:="HYFIN_COMPLETE < 244 days"]

discard.tab <- rbind(discard.tab,discard.244)

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



hydag <- readRDS("~/floodGAM/data/raw-data/raw-gamfelt-hydag.rds")


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


# Cross-check hyfin_complete with hydag ----------------------------------

## --- 1. filter out station-years that have less than 300 days 
## --- of data recorded both in hyfin_complete and hydag

# find the ID-yk tuples that have < 300 days in *both* databases:
discard.both300 <- merge(data[numdays<300,unique(.SD),.SDcols=c("ID","yk")],
                         hydag[numdays<300,unique(.SD),.SDcols=c("ID","yk")])

setkey(discard.both300,ID,yk)

discard.both300[,type:="HYFIN_COMPLETE and HYDAG < 300 days"]

discard.tab <- rbind(discard.tab,discard.both300)


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

discard.annmax[,type:="Missing observations around HYDAG ann max"]

discard.tab <- rbind(discard.tab, discard.annmax)

## take a quick look at the years flagged for discard:

for(station in unique(discard.annmax$ID)[16:30]){
  
  
  gdat <- data[discard==T&ID==station]
  setkey(gdat,ID,yk)
  gdat[,type:="hyfin_c"]
  hdat <- hydag[ID==station&yk%in%unique(gdat$yk)]
  hdat[,type:="hydag"]
  setnames(hdat,c("cumecs","dd"),c("cumecs.x","dd.x"))
  
  g2dat <- rbind(gdat[,c("dd.x","cumecs.x","yk","type")],
                 hdat[,c("dd.x","cumecs.x","yk","type")])
  
  gg <- ggplot(g2dat) +
    geom_line(aes(dd.x,cumecs.x,color=type,linewidth=type)) +
    #geom_line(data=hdat,aes(dd,cumecs),color="red") +
    geom_vline(data=gdat,aes(xintercept = dd.y),color="blue",
               linetype=2,linewidth=1) +
    scale_color_manual(values=c("red","black")) +
    scale_linewidth_manual(values=c(0.6,1))+
    labs(title=station,
         y = "m3/s", x= "date (as decimal)") +
    facet_wrap(vars(yk),scales="free_x") +
    theme_bw() +
    theme(legend.title=element_blank())
  
  print(gg)
  
  gc()
  
}


## discard all the years flagged in discard.annmax:
data <- data[discard == FALSE]



# save discard tab ----------------------


keep <- data[,unique(yk),by="ID"]

keep[,type:="Data used in analysis"]
setnames(keep,"V1","yk")

discard.tab <- rbind(discard.tab,keep)


save(discard.tab,file="~/floodGAM/data/README_files/histogram_findata_control.rda")

discard.tab[,type:=as.character(type)]

discard.tab[,type:=ifelse(type=="utelatt tabell A2",
                          "Markert som utelatt, tabell A2 85-2016",type )]

library(ggplot2)
library(scico)

ggplot(discard.tab) +
  geom_histogram(aes(yk,fill=type,alpha=type),binwidth=1) +
  labs(y = "Number of stations",
       x = " ") +
  scale_alpha_manual(values=c(0.4,0.8,0.8,0.8,0.8),guide=F) +
  scale_fill_manual(name = " ",values=c("grey",scico(4,palette = "batlow"))) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 16)) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE))
