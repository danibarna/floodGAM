##
##
##
##
##
##
## -----------------------------------------------------------------------------

library(data.table)
library(lubridate)


## ---read in data
data05 <- readRDS(paste0("~/floodGAM/data/raw-data/","arkiv05","data.rds")) # not stored on github
data39 <- readRDS(paste0("~/floodGAM/data/raw-data/","arkiv39","data.rds")) # not stored on github
data37 <- readRDS(paste0("~/floodGAM/data/raw-data/","arkiv37","data.rds")) # not stored on github
data35 <- readRDS(paste0("~/floodGAM/data/raw-data/","arkiv35","data.rds")) # not stored on github
data35.okt15 <- readRDS(paste0("~/floodGAM/data/raw-data/","arkiv35_etter_okt15","data.rds")) # not stored on github

# look at date ranges and number of years in hydag (37) vs hyfin_completed (39)
# make year column
data37[,yk:=year(date)]; data39[,yk:=year(date)]
data35[,yk:=year(date)]; data35.okt15[,yk:=year(date)]
data05[,yk:=year(date)]
# set the key
setkey(data37,ID,yk); setkey(data39,ID,yk)
setkey(data35,ID,yk); setkey(data35.okt15,ID,yk)
setkey(data05,ID,yk)

yrtab <- merge(data37[,length(unique(yk)),by=c("ID")],
               data39[,length(unique(yk)),by=c("ID")],by="ID")

yrtab <- merge(yrtab,data35[,length(unique(yk)),by=c("ID")],by="ID")

setnames(yrtab,c("V1.x","V1.y","V1"),c("37.N","39.N","35.N"))

yrtab <- merge(yrtab,data35.okt15[,length(unique(yk)),by=c("ID")],by="ID")

setnames(yrtab,"V1","35.okt.N")

yrtab <- merge(yrtab,data05[,length(unique(yk)),by=c("ID")],by="ID")

setnames(yrtab,"V1","05.N")

yrtab[`35.N` != `35.okt.N`]


plot(0,0,type="n",xlim=c(0,150),ylim=c(0,90),xlab="",ylab="")
hist(yrtab$`37.N`,breaks=seq(0,150,by=5),add=T)
hist(yrtab$`39.N`,
     col=rgb(0,0,1,1/4),breaks=seq(0,150,by=5),add=T)
hist(yrtab$`35.okt.N`,
     col=rgb(1,0,0,1/4),breaks=seq(0,150,by=5),add=T)
hist(yrtab$`35.N`,col=rgb(0,1,0,1/4), breaks=seq(0,150,by=5),add=T)
hist(yrtab$`05.N`,col=rgb(0,0,1,1/4), breaks=seq(0,150,by=5),add=T)


## make a few example plots.

data37[,type:="37"]
data35[,type:="35"]
data39[,type:="39"]
data35.okt15[,type:="35.etterokt15"]
data05[,type:="05"]

library(ggplot2)

this.station <- "2200016"

this.station <- "24400002"

this.station <- "10400023"


## look at the median spacing per day
## compute the time between observations:
difftimeFn <- function(x){
  a=x[-1]
  b=x[-length(x)]
  dtvec <- as.numeric(difftime(a,b,units="mins"))
  return(c(0,dtvec))
}

keyGenerator <- function(x){list(year(x),month(x),day(x))}

# which stations ahve the biggest gaps ebtween daily and fin data N?
these <- yrtab[which(yrtab[,abs(`37.N`-`39.N`)]>30),get("ID")]



for(this.station in these){
  ds <- rbind(data37[ID==this.station,],
              data35[ID==this.station,],
              data39[ID==this.station,],
              data05[ID==this.station,],
              data35.okt15[ID==this.station,])
  
  
  
  ds[, gapmin := lapply(.SD,difftimeFn), .SDcols = "date", by="type"]
  
  
  ds[, # create key column
     c("yk","mk","dk"):=sapply(.SD,keyGenerator),
     .SDcols = "date"]
  ds[,meangapmin:=mean(gapmin),by=c("yk","mk","dk","type")]
  
  tersk <- quantile(ds$cumecs,0.7)
  
  ds[,bf:=ifelse(cumecs > tersk,1,0)]
  
  gdat <- ds[,.SD[sample(.N, min(10000,.N))],by = type]
  
  gdat <- rbind(gdat,ds[bf==1])
  
  # ggplot(gdat) +
  #   geom_line(aes(date,cumecs,color=type)) +
  #   facet_wrap(vars(type),nrow = 5)
  
  gs <- ggplot(gdat) +
    geom_line(aes(date,meangapmin,color=type)) +
    labs(title = this.station) +
    lims(y=c(0,1440*2)) +
    facet_wrap(vars(type),nrow = 5)
  
  print(gs)
}










