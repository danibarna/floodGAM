##
##
##
##
##
##
## Master script for covariate pre-selection XGBoost-GAM index flood model
##
## Option to use either a GAM or XGBoost as a base learner within the IIS
## algorithm. 
##
## If GAM is used as base learner, CRPS is used as error metric
## If XGBoost used as base learner, MAE is used as error metric
##
## Note that this is a rather naive application of GAM and we do not assess
## model fit beyond the single chosen evaluation metric. GAMs are not generally
## meant for mass application and we could be missing fitting problems. Always 
## check the chosen GAM with the script at
## /code/scripts/4-model-fitting/median-(index-flood)/checking_auto_data_driven_gams.R
## -----------------------------------------------------------------------------

library(data.table)
library(xgboost)
library(caret)
library(mgcv)

## ----- source custom functions:
source("~/floodGAM/code/functions/fn_xgtune.R")
source("~/floodGAM/code/functions/fn_IIS_gam.R") ## Check! which is loaded

# Data preparation --------------------------------------------------------

## ----- load in the gamfelt dataset
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))

gfam <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                       "gamfelt_hydagsupplement_durations_annual_maxima.rds"))

# convert to specific discharge
gfam <- merge(gfam,gfcov[,c("ID","A")],by="ID")
gfam[,specQ:=Qm3_s/A*1000]

# remove the id and lat/long columns
# XGBoost gets access to the full covariate set
gfcov <- gfcov[,-c("RN","HN","Y_lat","X_long","Y_utm","X_utm",
                   "Y_G_Lat","X_G_Long","Y_G_UTM","X_G_UTM",
                   "QD_fgp","R_L_sqrt","R_G_1085","R_G","Q_N_cuberoot",
                   "T_Feb_sqrd","T_Mar_cubed","W_Mai_sqrt")]

# # standardize cov values by centering and dividing by 2 standard deviations
# coltab = names(gfcov)[-which(names(gfcov)=="ID")]
# gfcov[, 
#       (coltab) := lapply(.SD, function(Xw) (Xw - mean(Xw)) / (sd(Xw) * 2)), 
#       .SDcols = coltab]

# the response variable here is the median of the annual max
gfam <- gfam[,.(qind = median(specQ)),by=c("ID","d")]

# make the gamdat data object
gamdat <- merge(gfcov,gfam,by="ID")

# Choose which durations to run the analysis on ---------------------------

mydurations <- c(1,6,12,18,24,36,48)


# Define the data folds ---------------------------------------------------
set.seed(8)
k = 10
# use the 24 hour duration
fidx <- createFolds(gamdat[d==24,get("qind")],k) 




# step 1: tune XGBoost hyperparameters, tree depth 1 ----------------------

## Uses xgb.cv built-in function with custom wrapper 'xgtune'

## evaluation metric: mean absolute error (MAE)
## objective function: default (reg:squarederror)

nfolds = 10 # 10 fold cross-validation (internal to xgb.cv)
nbr = 999 # number of boosting rounds
esr = 25 # number of rounds w/out improvement we stop after

## parameter ranges in grid search
mcw = c(1,2,3,5,7) # minimum child weight for each node
eta = seq(0.01,0.1,length.out=5) # learning rate
subsamp = seq(0.5,1,length.out=5) # subsampling rate 

treedepth = 1 # no interactions in XGBoost for predictor pre-selection

## tune a set of hyperparameters for each duration of interest:
for(di in mydurations){
  gamdat.d <- gamdat[d==di]
  ## --- prep data for XGBoost ---
  yraw <- gamdat.d[,get("qind")]
  y <- log(yraw)
  hplist <- xgtune.mape(y,
                        gamdat.d[,-c("ID","d","qind")],
                        nbr,esr,treedepth,mcw,subsamp,eta,nfolds)
  saveRDS(hplist,file=paste0("~/floodGAM/results/output/median-(index-flood)/",
                             "xgboost-hyperparameters/",
                             "xgb1hp_",di,".rds"))
}


# step 2: run IIS with GAM --------------------------------------------

p = 10 # we evaluate the top p features at each step (in order of avg gain)
eps = 1e-4 # stop algorithm when improvement drops below this level


## run with different seeds.....
for( thisseed in c(30,32,85)){
  
  set.seed(thisseed)
  k = 10
  # use the 24 hour duration
  fidx <- createFolds(gamdat[d==24,get("qind")],k) 
  
  ## data table to store output
  iisDE <- data.table(Feature=character(), ordFeat=numeric(), maeFeat = numeric(), 
                      fold = numeric(), model = character(), d = numeric())
  
  for(di in mydurations){
    
    gamdat.d <- gamdat[d==di]
    
    # load XGBoost hyperparameters for this duration:
    hpXGB = readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                           "xgboost-hyperparameters/",
                           "xgb1hp_",di,".rds"))
    
    
    for(i in 1:k){
      
      train.gamdat.d <- gamdat.d[-fidx[[i]]]
      
      train.X <- train.gamdat.d[,!c("ID","d","qind")]
      train.y <- train.gamdat.d[,get("qind")]
      
      gamfeatIIS <- IIS(train.y,train.X,10,p,eps, # 10 is internal IIS cv folds
                        hpXGB[[3]],hpXGB[[1]])
      
      iisDE <- rbind(iisDE,data.table(Feature = gamfeatIIS$Feature,
                                      ordFeat = gamfeatIIS$ord,
                                      maeFeat = gamfeatIIS$errormetric,
                                      fold = rep(i,
                                                 length(gamfeatIIS$Feature)),
                                      model = rep("gam",
                                                  length(gamfeatIIS$Feature)),
                                      d = rep(di,length(gamfeatIIS$Feature))))
      print(paste0("Duration ", di, " fold ", i,
                   " model gam", " for DE"))
    }
    
    
  }
  
  saveRDS(iisDE,file=paste0("~/floodGAM/results/output/median-(index-flood)/",
                            "gamfelt_hydagsupp_featuresFromIIS_gam_",thisseed,".rds"))
  
}







# Step 3: run IIS with XGBoost --------------------------------------------

p = 10 # we evaluate the top p features at each step (in order of avg gain)
eps = 1e-3 # stop algorithm when improvement drops below this level


## data table to store output
iisDE <- data.table(Feature=character(), ordFeat=numeric(), maeFeat = numeric(), 
                    fold = numeric(), model = character(), d = numeric())


## XGBoost stuff
for(di in mydurations){
  
  gamdat.d <- gamdat[d==di]
  
  # load XGBoost hyperparameters for this duration:
  hpXGB = readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                         "xgboost-hyperparameters/",
                         "xgb1hp_",di,".rds"))

  for(i in 1:k){
    
    train.gamdat.d <- gamdat.d[-fidx[[i]]]
    
    train.X <- train.gamdat.d[,!c("ID","d","qind")]
    train.y <- log(train.gamdat.d[,get("qind")])
    
    XGB1featIIS <- IIS(train.y,train.X,10,p,eps, # 10 is internal IIS cv folds
                       hpXGB[[3]],hpXGB[[1]],
                       hpXGB[[3]],hpXGB[[1]])
    
    iisDE <- rbind(iisDE,data.table(Feature = XGB1featIIS$Feature,
                                    ordFeat = XGB1featIIS$ord,
                                    maeFeat = XGB1featIIS$mae,
                                    fold = rep(i,length(XGB1featIIS$Feature)),
                                    model = rep("XGB1",
                                                length(XGB1featIIS$Feature)),
                                    d = rep(di,length(XGB1featIIS$Feature))))
    print(paste0("Duration ", di, " Fold ",i,
                 " model XGB1", " for DE"))
  }
}

saveRDS(iisDE,file=paste0("~/floodGAM/results/output/median-(index-flood)/",
                          "gamfelt_hydagsupp_featuresFromIIS_xgboost.rds"))
