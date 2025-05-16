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

## load in the GAM-predicted return levels
load("~/floodGAM/results/output/all-quantiles/qrt-prt-oos.rda")

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


## ---- QRT

# make sure the quantiles are consistent (monotonically increasing)
adjust.fn <- function(x,y){c(NA,quantreg::rearrange(stepfun(x[-1],y))(x[-1]))}

qrt.predictions[,adj.val := lapply(.SD,function(col) adjust.fn(x=rp,y=val))$rp,
                .SDcols = c("rp","val"),
                by = c("d","ID")]

qrt <- qrt.predictions[rp %in% evalrp]
qrt.am <- merge(gfam,qrt,by=c("ID","d"),allow.cartesian = T)

## calculate the quantile score, mean over data points per station:
qrt.am <- qrt.am[,mean( (specQ-adj.val)*((1-1/rp)-ifelse(specQ<=adj.val,1,0)) ),
                 by = c("ID","d","rp")]
## now take the mean over stations:
setnames(qrt.am,"V1","qrt.qs")

qrt.qs <- qrt.am[,.(qrt.qs=mean(qrt.qs)),by=c("d","rp")]


## ---- PRT 

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

qsDT <- merge(qrt.qs,prt.qs,by=c("d","rp"))
qsDT <- merge(qsDT,gev.qs,by=c("d","rp"))

coltab = c("prt.qs","qrt.qs","gev.qs")
tt <- cbind(qsDT[,c("d","rp")],qsDT[,lapply(.SD,round,1),.SDcols=coltab])

t(tt)


# Save objects for plotting -----------------------------------------------

qrt.am <- merge(qrt.am,gfcov[,c("ID","A","QD_fgp")],by="ID")
prt.am <- merge(prt.am,gfcov[,c("ID","A","QD_fgp")],by="ID")
gevp.am <- merge(gevp.am,gfcov[,c("ID","A","QD_fgp")],by="ID")

save(qrt.am,prt.am,gevp.am,qsDT,file=
       "~/floodGAM/results/output/all-quantiles/objects-qs-and-rl-dotplots.rda")





