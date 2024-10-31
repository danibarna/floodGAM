##
##
##
##
##
##
## Tune hyperparameters, XGBoost for prediction
## -----------------------------------------------------------------------------

library(data.table)
library(xgboost)

## ----- source xgtune function:
source("~/floodGAM/code/functions/fn_xgtune.R")

# Data preparation --------------------------------------------------------

## ----- load in the gamfelt dataset
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))

gfam <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                       "durations_gamfelt_annual_maxima.rds"))

# convert to specific discharge
gfam <- merge(gfam,gfcov[,c("ID","A")],by="ID")
gfam[,specQ:=Qm3_s/A*1000]

# remove the id and lat/long columns
# XGBoost gets access to the full covariate set
gfcov <- gfcov[,-c("RN","HN","Y_lat","X_long","Y_utm","X_utm")]

# standardize cov values by centering and dividing by 2 standard deviations
coltab = names(gfcov)[-which(names(gfcov)=="ID")]
gfcov[, 
      (coltab) := lapply(.SD, function(Xw) (Xw - mean(Xw)) / (sd(Xw) * 2)), 
      .SDcols = coltab]

# the response variable here is the median of the annual max
gfam <- gfam[,.(qind = median(specQ)),by=c("ID","d")]

# make the gamdat data object
gamdat <- merge(gfcov,gfam,by="ID")



# Tune the hyperparameters ------------------------------------------------

dvec <- unique(gamdat$d)

## evaluation metric: mean absolute percent error (MAPE) #gives better predictive results than MAE
## objective function: default (reg:squarederror)

nfolds = 10 # cross-validation internal to xgb.cv in xgtune
nbr = 999 
esr = 25 
mcw = c(3,5,7) 
eta = seq(0.01,0.1,length.out=3)
treedepth = 1:7
subsamp = seq(0.1,0.75,length.out=3)

for(di in dvec){
  gamdat.d <- gamdat[d==di]
  ## --- prep data for XGBoost ---
  yraw <- gamdat.d[,get("qind")]
  y <- log(yraw)
  hplist <- xgtune.mape(y,
                        gamdat.d[,-c("ID","d","qind")],
                        nbr,esr,treedepth,mcw,subsamp,eta,nfolds)
  saveRDS(hplist,file=paste0("~/floodGAM/results/output/median-(index-flood)/",
                            "xgboost-hyperparameters/",
                            "xgbpurehp_",di,".rds"))
}

