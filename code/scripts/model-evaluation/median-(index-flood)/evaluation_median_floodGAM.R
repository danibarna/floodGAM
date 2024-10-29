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

setkey(scre,d)

## TABLE 4 - error metrics
scre[,c("model","d","rmse","crps","ae","re","ape")]

## statistical significance of different error metrics:
for(di in unique(oos.pred$d)){
  print(paste0(di," - ",permutationTest(oos.pred,
                                        "floodGAM","RFFA2018",
                                        1000,
                                        "ape",di)) )
}


## ------- Dotplots - predictive accuracy

oos.pred <- merge(oos.pred,gfcov[,c("ID","A","QD_fgp")],by="ID")

glist <- list()
i = 1
for(di in unique(oos.pred$d)){
 
  ggdat <- dcast(oos.pred[d==di], ID + A + QD_fgp ~ model, value.var = "crps")
  
  glist[[i]] <- dotplotRFFA2018floodGAM(ggdat,0,1000,paste0("CRPS, d = ",di))
  
  i = i+1
}

figure <- ggarrange(glist[[1]],glist[[2]],glist[[3]],glist[[4]],
                    glist[[5]],glist[[6]],glist[[7]],glist[[8]],
                    nrow=2,ncol = 4,
                    common.legend = T, legend = "bottom")

figure

glist[[5]]



# Reliability -------------------------------------------------------------

# use the model mu and model sigma to PIT the eta.obs:

pit <- oos.pred[,pnorm(log(eta.obs),mu.gam,sigma.gam),by=c("model","d")]

PITplot(pit,"floodGAM",1,24)

PITplot(pit,"RFFA2018",1,24)


## TABLE 5 - coverage and width

N <- length(unique(oos.pred$ID)) # number of stations

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
covwid[,c("d","model","cov.50","wid.50","cov.80","wid.80","cov.90","wid.90")]




# Duration consistency ----------------------------------------------------

# order the columns properly
setkey(oos.pred,ID,model,d)

ics <- which(oos.pred[,.(difc = c(diff(eta),0), d=d),by=c("ID","model")]$difc>0)

oos.ics <- oos.pred[ics,] # so 801 out of 5904 are inconsistent. how many are repeats?

oos.ics[,.N,by=c("ID","model")]
oos.ics[,max(d),by=c("ID","model")]

oos.ics[,.N,by="model"] # 463 = RFFA2018, 338 = floodGAM

tt <- oos.ics[,.N,by=c("d","model")]
setkey(tt,d,model)
tt

## (can also check the predictive uncertainty for the inconsistent stations)
## how many are *significantly* inconsistent? 

pred.draws <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                             "median-index-flood-posterior-draws.rds"))

pred.draws <- pred.draws[ID%in%unique(oos.ics$ID)]

tt <- pred.draws[,list(quantile(eta.draws,0.05),
                               quantile(eta.draws,0.95)),by=c("ID","model","d")]

tv <- merge(tt,
            oos.pred[ID%in%unique(oos.ics$ID),c("ID","eta","eta.obs","model","d")],
                  by=c("ID","model","d"))


## where are the stations (if any) where there is no overlap in CI between durations?

ggplot(pred.draws[ID=="9900017"]) +
  geom_density(aes(eta.draws,group=d,fill=d),alpha=0.2)+
  facet_wrap(vars(model))

ggplot(pred.draws[ID=="10100001"]) +
  geom_density(aes(eta.draws,group=d,fill=d),alpha=0.2)+
  facet_wrap(vars(model))

ggplot(pred.draws[ID=="8900001"]) +
  geom_density(aes(eta.draws,group=d,fill=as.factor(d)),alpha=0.2)+
  facet_wrap(vars(model))

ggplot(pred.draws[ID=="15700003"]) +
  geom_density(aes(eta.draws,group=d,fill=as.factor(d)),alpha=0.2)+
  facet_wrap(vars(model))






# scrap -------------------------------------------------------------------



oos.pred <- merge(oos.pred,gfcov[,c("ID","A","QD_fgp")],by="ID")

library(scico)

# go long to wide:
ggcrps <- dcast(oos.pred[d==0], ID + A + QD_fgp ~ model, value.var = "re")









