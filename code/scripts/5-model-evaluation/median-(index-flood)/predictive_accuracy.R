##
##
##
##
##
## Evaluate fitted models on:
## - predictive accuracy at oos locations (5 metrics)
## -----------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(ggpubr)
library(scico)

## source function permutation test:
source("~/floodGAM/code/functions/fn_calc_optimal_predictor.R")
## source functions to quickly plot and check results:
source("~/floodGAM/code/functions/fn_check_results_with_plots.R")

## ----- load in the predictions and error metrics:
oos.pred <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                           "median-index-flood-predictive-accuracy.rds"))

## ----- for plotting of evaluation metrics:
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))


# Predictive accuracy -----------------------------------------------------

scre <- oos.pred[,
                 lapply(.SD,mean),.SDcols = c("crps","ae","re","ape"),
                 by=c("model","d")]

scre <- merge(scre,
              oos.pred[,sqrt(mean(se)),by=c("model","d")],
              by=c("model","d"))

setnames(scre,"V1","rmse")

scre[,re:=re*100]
scre[,ape:=ape*100]

scre[,c("rmse","crps","ae","re","ape") := lapply(.SD,round,1),
     .SDcols= c("rmse","crps","ae","re","ape")]

setkey(scre,d,re)

## TABLE 4 - error metrics
scre[,c("model","d","rmse","crps","ae","re","ape")]

## statistical significance of different error metrics:
for(m in c("se","crps","ae","re","ape")){
  print(paste0("********",m))
  for(di in unique(oos.pred$d)){
    print(paste0(di," - ",permutationTest(oos.pred,
                                          "floodGAM","datadrive",
                                          1000,
                                          m,di)) )
  }
}



## ------- Dotplots - predictive accuracy check 

oos.pred <- merge(oos.pred,gfcov[,c("ID","A","QD_fgp")],by="ID")

glist <- list()
i = 1
for(di in unique(oos.pred$d)){
  
  ggdat <- dcast(oos.pred[d==di], ID + A + QD_fgp ~ model, value.var = "re")
  
  glist[[i]] <- dotplotRFFA2018floodGAM(ggdat,0,6,paste0("RE, d = ",di))
  
  i = i+1
}

figure <- ggarrange(glist[[1]],glist[[2]],glist[[3]],glist[[4]],
                    glist[[5]],glist[[6]],glist[[7]],glist[[8]],
                    nrow=2,ncol = 4,
                    common.legend = T, legend = "bottom")

figure





oos.pred[oos.pred[,.I[which.max(ape)],by=c("d","model")]$V1]


oos.pred[ID=="23400018"&model=="RFFA2018"]
