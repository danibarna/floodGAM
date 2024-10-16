##
##
##
##
##
## Evaluate fitted models on both:
## - predictive accuracy at oos locations (5 metrics)
## - reliability (PIT)
## -----------------------------------------------------------------------------

library(data.table)


## ----- source function to calculate optimal predictor for APE and RE:
source("~/floodGAM/code/functions/fn_posterior_simulation_GAM.R")

## ----- load in the predictions from the models:
oos.pred <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                      "median-index-flood-oos-predictions.rds"))





