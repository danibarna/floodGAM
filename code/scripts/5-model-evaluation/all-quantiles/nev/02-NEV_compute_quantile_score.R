##
##
##
##
##
##
## We use a 10-fold cross-validation scheme such
## that the dataset is divided into 10 mutually exclusive subsets of equal size.
##
## Two things: 
##   - when we have data, compare estimates to observed data via quantile score
##   - beyond the range of the observed data, directly compare locally estimated
##     return levels with predicted return levels
## -----------------------------------------------------------------------------

library(data.table)
library(quantreg)

load("~/floodGAM/results/output/all-quantiles/qrt-prt-oos.rda")
load("~/floodGAM/results/output/all-quantiles/rffa2018-oos.rda")


## load in the local Stan fits:
gevp <- readRDS("~/floodGAM/results/output/all-quantiles/gamfeltstanresult.rds")

## load in the data for each station and cov to convert to specific discharge:
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))
gfam <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                       "gamfelt_hydagsupplement_durations_annual_maxima.rds"))
# convert to specific discharge
gfam <- merge(gfam,gfcov[,c("ID","A")],by="ID")
gfam[,specQ:=Qm3_s/A*1000]



# Quantile score (within the range of observed data) ----------------------

# evaluate at these return periods:
evalrp <- c(10,20,50)

## ---- RFFA2018

## go from long to wide format
rffa <- dcast(rffa.predictions, ID + d ~ param, value.var = "val")

## convert to location-scale parameterization
# note these are the GEV location-scale parameterization sigma and mu,
# not the sigma mu from the predictive distribution (GAM)
rffa[,sigma:=eta*exp(beta)]
rffa[,mu:=eta - sigma*(log(2)^(-xi)-1)/xi]

# compute the return periods from the GEV parameters
rffa.am <- rffa[,list(mu+sigma/xi * ((-log(1-1/evalrp))^(-xi)-1),evalrp),
              by=c("d","ID")]
# merge in the observed data points
rffa.am <- merge(gfam,rffa.am,by=c("ID","d"),allow.cartesian = T)
# change return level column name
setnames(rffa.am,c("V1","evalrp"),c("rl","rp"))

## calculate the quantile score, mean over data points per station:
rffa.am <- rffa.am[,mean( (specQ-rl)*((1-1/rp)-ifelse(specQ<=rl,1,0)) ),
                 by = c("ID","d","rp")]
## now take the mean over stations:
setnames(rffa.am,"V1","rffa.qs")

rffa.qs <- rffa.am[,.(rffa.qs=mean(rffa.qs)),by=c("d","rp")]



## ---- floodGAM

## go from long to wide format
prt <- dcast(prt.predictions, ID + d ~ param, value.var = "val")

## convert to location-scale parameterization
# note these are the GEV location-scale parameterization sigma and mu,
# not the sigma mu from the predictive distribution (GAM)
prt[,sigma:=eta*exp(beta)]
prt[,mu:=eta - sigma*(log(2)^(-xi)-1)/xi]

# compute the return periods from the GEV parameters
prt.am <- prt[,list(mu+sigma/xi * ((-log(1-1/evalrp))^(-xi)-1),evalrp),
              by=c("d","ID")]
# merge in the observed data points
prt.am <- merge(gfam,prt.am,by=c("ID","d"),allow.cartesian = T)
# change return level column name
setnames(prt.am,c("V1","evalrp"),c("rl","rp"))

## calculate the quantile score, mean over data points per station:
prt.am <- prt.am[,mean( (specQ-rl)*((1-1/rp)-ifelse(specQ<=rl,1,0)) ),
                 by = c("ID","d","rp")]
## now take the mean over stations:
setnames(prt.am,"V1","prt.qs")

prt.qs <- prt.am[,.(prt.qs=mean(prt.qs)),by=c("d","rp")]


## ---- Local fit 

# go from long to wide format, selecting only the posterior mean:
gevp <- dcast(gevp, ID + d ~ param, value.var = "mean")

# compute the return periods from the GEV parameters
gevp.am <- gevp[,list(mu+sigma/xi * ((-log(1-1/evalrp))^(-xi)-1),evalrp),
                by=c("d","ID")]
# merge in the observed data points
gevp.am <- merge(gfam,gevp.am,by=c("ID","d"),allow.cartesian = T)

# change return level column name
setnames(gevp.am,c("V1","evalrp"),c("rl","rp"))

## calculate the quantile score, mean over data points per station:
gevp.am <- gevp.am[,mean( (specQ-rl)*((1-1/rp)-ifelse(specQ<=rl,1,0)) ),
                   by = c("ID","d","rp")]
## now take the mean over stations:
setnames(gevp.am,"V1","gev.qs")

gev.qs <- gevp.am[,.(gev.qs=mean(gev.qs)),by=c("d","rp")]



##### table, all quantile scores:

qsDT <- merge(rffa.qs,prt.qs,by=c("d","rp"))
qsDT <- merge(qsDT,gev.qs,by=c("d","rp"))

coltab = c("prt.qs","rffa.qs","gev.qs")
tt <- cbind(qsDT[,c("d","rp")],qsDT[,lapply(.SD,round,1),.SDcols=coltab])

t(tt)


# Save objects for plotting -----------------------------------------------

rffa.am <- merge(rffa.am,gfcov[,c("ID","A","QD_fgp")],by="ID")
prt.am <- merge(prt.am,gfcov[,c("ID","A","QD_fgp")],by="ID")
gevp.am <- merge(gevp.am,gfcov[,c("ID","A","QD_fgp")],by="ID")

save(rffa.am,prt.am,gevp.am,qsDT,file=
       "~/floodGAM/results/output/all-quantiles/nev/rffa-objects-qs-and-rl-dotplots.rda")





