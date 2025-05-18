##
##
##
##
##
## check internal model consistency for RFFA2018 and floodGAM
##
## Consistency for floodGAM and RFFA2018 means oos predicted parameters fulfill 
## support requirements for the GEV
##
## The parameters are not jointly estimated, so this step is necessary
## -----------------------------------------------------------------------------

library(data.table)
library(quantreg)

load("~/floodGAM/results/output/all-quantiles/qrt-prt-oos.rda")
load("~/floodGAM/results/output/all-quantiles/rffa2018-oos.rda")

## load in the min/max data values for each station to check the gev support:
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))
gfam <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                       "gamfelt_hydagsupplement_durations_annual_maxima.rds"))
# convert to specific discharge
gfam <- merge(gfam,gfcov[,c("ID","A")],by="ID")
gfam[,specQ:=Qm3_s/A*1000]

## floodGAM ------------------------------------------

## go from long to wide format
prt <- dcast(prt.predictions, ID + d ~ param, value.var = "val")

## convert to location-scale parameterization
# note these are the sigma and mu for the GEV, not the
# sigma and mu in data.table prt.predictions--those sigma and mu
# come from the GAM predictions
prt[,sigma:=eta*exp(beta)]
prt[,mu:=eta - sigma*(log(2)^(-xi)-1)/xi]

# find the min and max data point for each station
dp <- gfam[, list(min(specQ), max(specQ)), by=c("ID","d")]
setnames(dp,c("V1","V2"),c("miny","maxy"))

# add the data points to the data table:
prt <- merge(prt,dp,by=c("ID","d"))


## test the support conditions:
which(prt[,(1 + xi*(maxy-mu)/sigma)]<=0)

which(prt[,(1 + xi*(miny-mu)/sigma)]<=0)

## none are outside the support. good. 



## RFFA2018 ------------------------------------------

## go from long to wide format
rffa <- dcast(rffa.predictions, ID + d ~ param, value.var = "val")

## convert to location-scale parameterization
# note these are the sigma and mu for the GEV, not the
# sigma and mu in data.table rffa.predictions--those sigma and mu
# come from the GAM predictions
rffa[,sigma:=eta*exp(beta)]
rffa[,mu:=eta - sigma*(log(2)^(-xi)-1)/xi]

# find the min and max data point for each station
dp <- gfam[, list(min(specQ), max(specQ)), by=c("ID","d")]
setnames(dp,c("V1","V2"),c("miny","maxy"))

# add the data points to the data table:
rffa <- merge(rffa,dp,by=c("ID","d"))


## test the support conditions:
which(rffa[,(1 + xi*(maxy-mu)/sigma)]<=0)

which(rffa[,(1 + xi*(miny-mu)/sigma)]<=0)

## none are outside the support. good. 