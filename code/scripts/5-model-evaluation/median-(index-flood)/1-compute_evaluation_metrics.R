##
##
##
##
##
## Evaluate fitted models on:
## - predictive accuracy at oos locations (5 metrics)
## - duration consistency
## - reliability (PIT)
## -----------------------------------------------------------------------------

library(data.table)
library(scoringRules)

## ----- source function to calculate optimal predictor for APE and RE:
source("~/floodGAM/code/functions/fn_calc_optimal_predictor.R")

for( thisseed in c(30,32,85)){

## ----- load in the predictions from the models:
oos.pred <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                           "gamfelt_hydagsupp_median_flood_oos_pred_",thisseed,".rds"))

# Predictive accuracy -----------------------------------------------------

oos.pred[,type:=ifelse(model=="xgboost",
                       "xgboost","stat")]

## Squared Error (SE)
oos.pred[type!="xgboost", se := (eta.obs-eta)^2 ]
## CRPS -- requires package scoringRules
oos.pred[type!="xgboost",
         crps:=scoringRules::crps_lnorm(eta.obs, mu.gam, sigma.gam)]
## Absolute Error (AE)
oos.pred[,ae:=abs( (eta.obs-eta) )]

## Proportional error metrics: RE and APE
## These take a while to get the approx to the optimal predictor...
oos.pred[, rowpos:= .I]
## Relative error
oos.pred[type!="xgboost",
         eta.re:=optimal.predictor.re(sigma.gam,mu.gam,eta),
         by=rowpos]
oos.pred[,re:=abs( (eta.obs-eta.re)/eta.re )]
## Absolute percent error
oos.pred[type!="xgboost",
         eta.ape:=optimal.predictor.ape(sigma.gam,mu.gam,eta),
         by=rowpos]
oos.pred[,ape:=abs( (eta.obs-eta.ape)/eta.obs )]

oos.pred[,type:=NULL]

saveRDS(oos.pred,
        file = paste0("~/floodGAM/results/output/median-(index-flood)/",
                      "gamfelt_hydagsupp_median_flood_predictive_accuracy_",thisseed,".rds"))

}
