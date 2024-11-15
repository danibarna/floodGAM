##
##
##
##
##
##
##
## create the histograms for the data section: 
## - record length histogram
## - percent of record that is subdaily data histogram
## -----------------------------------------------------------------------------

library(data.table)

gf <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                     "durations_gamfelt_annual_maxima.rds"))


percentfin <- merge(gf[d==1 & archive == 39,.N,by="ID"],
                    gf[d==1,.N,by="ID"],
                    by="ID")


par(mfrow=c(1,2),mar = c(5,5,1,1))

hist(gf[d==1,.N,by="ID"]$N,breaks=20,
     xlab="Record length (years)",ylab="Stations",main="",
     col="white")
H2 <- hist(percentfin[,perct:=ceiling(N.x/N.y*100)]$perct,xaxt="n",
     xlab="Percent of record that is subdaily data",ylab="Stations",
     main="",
     col="white")
axis(side=1, at=H2$breaks, labels=paste0(H2$breaks, "%"))

# saved manually as data_details_histograms.pdf, landscape, 11.5 x 4 in


