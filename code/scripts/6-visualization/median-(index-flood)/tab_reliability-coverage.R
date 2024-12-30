##
##
##
##
##
## Create coverage table for section 4.2: reliability
## 
## -----------------------------------------------------------------------------

library(data.table)


# Load data ---------------------------------------------------------------

## ----- load in the predictions and error metrics:
oos.pred <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                           "gamfelt_hydagsupp_median_flood_predictive_accuracy.rds"))


# Create table 5 ----------------------------------------------------------

N <- length(unique(oos.pred$ID)) # number of stations

pit <- oos.pred[,pnorm(log(eta.obs),mu.gam,sigma.gam),by=c("model","d")]

## there's a more elegant way to do this...
coverage <- pit[, .(round(sum(V1 > 0.25 & V1 < 0.75) / N * 100, 2),
                    round(sum(V1 > 0.10 & V1 < 0.90) / N * 100, 2),
                    round(sum(V1 > 0.05 & V1 < 0.95) / N * 100, 2)), 
                by = c("d","model")]
setnames(coverage,c("V1","V2","V3"),c("cov.50","cov.80","cov.90"))

width <- oos.pred[, .(round(mean(qlnorm(0.75, mu.gam, sigma.gam) - qlnorm(0.25, mu.gam, sigma.gam)),0),
                      round(mean(qlnorm(0.90, mu.gam, sigma.gam) - qlnorm(0.10, mu.gam, sigma.gam)),0),
                      round(mean(qlnorm(0.95, mu.gam, sigma.gam) - qlnorm(0.05, mu.gam, sigma.gam)),0)),
                  by=c("d","model")]
setnames(width,c("V1","V2","V3"),c("wid.50","wid.80","wid.90"))

covwid <- merge(coverage,width,by=c("d","model"))

## print the table
covwid[model%in%c("RFFA2018","floodGAM") & d%in%c(1,24),
       c("d","model",
                          "cov.50","wid.50",
                          "cov.80","wid.80","cov.90","wid.90")]



















