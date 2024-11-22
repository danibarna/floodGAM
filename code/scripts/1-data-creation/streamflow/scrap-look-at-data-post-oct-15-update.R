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
data39 <- readRDS(paste0("~/floodGAM/data/raw-data/","arkiv39","data.rds")) # not stored on github
data37 <- readRDS(paste0("~/floodGAM/data/raw-data/","arkiv37","data.rds")) # not stored on github
data35 <- readRDS(paste0("~/floodGAM/data/raw-data/","arkiv35","data.rds")) # not stored on github
data35.okt15 <- readRDS(paste0("~/floodGAM/data/raw-data/","arkiv35_etter_okt15","data.rds")) # not stored on github

# look at date ranges and number of years in hydag (37) vs hyfin_completed (39)
# make year column
data37[,yk:=year(date)]; data39[,yk:=year(date)]
data35[,yk:=year(date)]; data35.okt15[,yk:=year(date)]
# set the key
setkey(data37,ID,yk); setkey(data39,ID,yk)
setkey(data35,ID,yk); setkey(data35.okt15,ID,yk)

yrtab <- merge(data37[,length(unique(yk)),by=c("ID")],
               data39[,length(unique(yk)),by=c("ID")],by="ID")

yrtab <- merge(yrtab,data35[,length(unique(yk)),by=c("ID")],by="ID")

setnames(yrtab,c("V1.x","V1.y","V1"),c("37.N","39.N","35.N"))

yrtab <- merge(yrtab,data35.okt15[,length(unique(yk)),by=c("ID")],by="ID")

setnames(yrtab,"V1","35.okt.N")

yrtab[`35.N` != `35.okt.N`]


plot(0,0,type="n",xlim=c(0,150),ylim=c(0,90),xlab="",ylab="")
hist(yrtab$`37.N`,breaks=seq(0,150,by=5),add=T)
hist(yrtab$`39.N`,
     col=rgb(0,0,1,1/4),breaks=seq(0,150,by=5),add=T)
hist(yrtab$`35.okt.N`,
     col=rgb(1,0,0,1/4),breaks=seq(0,150,by=5),add=T)
